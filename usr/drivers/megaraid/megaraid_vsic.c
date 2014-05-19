/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <string.h>
#ifdef BARRELFISH
#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <barrelfish/waitset.h>
#include <pci/pci.h>
#include <lwip/inet.h>
#else
#include <arpa/inet.h>
#include <pci/devids.h>
#include <errors/errno.h>
#include <sys/mman.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "linux_defs.h"
#endif
#include <storage/vsic.h>
#include <storage/vsa.h>

#include "megaraid.h"

struct megaraid_vsic {
  struct megaraid_ctrl *ctrl;
};

static uint64_t htonll(uint64_t value)
{
    // The answer is 42
    static const int num = 42;

    // Check the endianness
    if (*(const char *)&num == num)
    {
      const uint32_t high_part = htonl(value >> 32);
      const uint32_t low_part = htonl(value & 0xFFFFFFFFLL);

      return ((uint64_t)low_part << 32) | high_part;
    } else {
      return value;
    }
}

static uint64_t cmd_cnt = 0;

static uint8_t *pmem_start = NULL;
static uintptr_t paddr_start = 0;

#define BUF_SIZE	(1 * 1024 * 1024)

lpaddr_t v2p(void *ptr, size_t len)
{
  lpaddr_t paddr;

  assert((uint8_t *)ptr >= pmem_start);
  assert((uint8_t *)ptr < pmem_start + BUF_SIZE);
  paddr = (uintptr_t)((uint8_t *)ptr - pmem_start);
  paddr += paddr_start;
  return paddr;
}

static void *alloc_map_frame(vregion_flags_t attr, size_t size,
                             struct capref *retcap)
{
    struct capref frame;
    errval_t r;

    r = frame_alloc(&frame, size, NULL);
    assert(err_is_ok(r));
    void *va;
    r = vspace_map_one_frame_attr(&va, size, frame, attr,
                                  NULL, NULL);
    if (err_is_fail(r)) {
        DEBUG_ERR(r, "vspace_map_one_frame failed");
        return NULL;
    }

    if (retcap != NULL) {
        *retcap = frame;
    }

    return va;
}

void *user_alloc(size_t size, uintptr_t *paddr);
void *user_alloc(size_t size, uintptr_t *paddr)
{
  struct capref cap;
  void * va = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE,
			      size, &cap);
  assert(va != NULL);
  struct frame_identity id;
  errval_t err = invoke_frame_identify(cap, &id);
  assert(err_is_ok(err));
  *paddr = id.base;
  return va;
}

static errval_t vsic_write(struct storage_vsic *vsic, struct storage_vsa *vsa,
                           off_t offset, size_t size, void *buffer)
{
    assert(vsic != NULL);
    assert(vsa != NULL);
    assert(buffer != NULL);
    /* struct megaraid_vsic *mydata = vsic->data; */

    struct write16 {
      uint8_t	opcode;
      uint8_t	flags;
      uint64_t	lba;
      uint32_t	length;
      uint8_t	group;
      uint8_t	control;
    } __attribute__ ((packed));

    assert(offset % BLOCK_SIZE == 0);

    memcpy(pmem_start, buffer, size);
    buffer = pmem_start;

    size = STORAGE_VSIC_ROUND(vsic, size);
    /* assert(size % BLOCK_SIZE == 0); */

    struct write16 write16_cmd = {
      .opcode = 0x8a,
      .flags = 0,
      .lba = htonll(offset / BLOCK_SIZE),
      .length = htonl(size / BLOCK_SIZE),
      .group = 0,
      .control = 0,
    };

    struct mrsas_mpt_cmd *cmd;
    MRSAS_REQUEST_DESCRIPTOR_UNION *req_desc;

    cmd = mrsas_get_mpt_cmd();
    assert(cmd != NULL);

    // Need virtual data addresses
    cmd->flags = MRSAS_DIR_OUT;
    cmd->length = size;
    cmd->data = buffer;
    cmd->ccb_ptr = (void *)cmd_cnt;	// Data pointer we can use

    cmd_cnt++;

    req_desc = mrsas_get_request_desc((cmd->index)-1);
    assert(req_desc != NULL);
    memset(req_desc, 0, sizeof(MRSAS_REQUEST_DESCRIPTOR_UNION));
    cmd->request_desc = req_desc;

    // command data block
    memcpy(cmd->io_request->CDB.CDB32, &write16_cmd, 16);

    // Build LDIO command
    MRSAS_RAID_SCSI_IO_REQUEST *io_request = cmd->io_request;
    io_request->RaidContext.VirtualDiskTgtId = TARGET_DEVICE_ID;
    io_request->RaidContext.status = 0;
    io_request->RaidContext.exStatus = 0;
    io_request->IoFlags = 16;	// CDB length in bytes
    io_request->RaidContext.regLockFlags = 0;
    io_request->RaidContext.timeoutValue = 0;
    cmd->request_desc->SCSIIO.RequestFlags =
      (MRSAS_REQ_DESCRIPT_FLAGS_LD_IO << MRSAS_REQ_DESCRIPT_FLAGS_TYPE_SHIFT);
    if (io_request->RaidContext.regLockFlags == REGION_TYPE_UNUSED)
      cmd->request_desc->SCSIIO.RequestFlags = (MRSAS_REQ_DESCRIPT_FLAGS_NO_LOCK << MRSAS_REQ_DESCRIPT_FLAGS_TYPE_SHIFT);
    io_request->RaidContext.Type = MPI2_TYPE_CUDA;
    io_request->RaidContext.regLockFlags |= (MR_RL_FLAGS_GRANT_DESTINATION_CPU0 | MR_RL_FLAGS_SEQ_NUM_ENABLE);
    io_request->RaidContext.nseg = 0x1;
    io_request->Function = MRSAS_MPI2_FUNCTION_LD_IO_REQUEST;
    io_request->DevHandle = TARGET_DEVICE_ID;
    io_request->DataLength = cmd->length;

    pMpi25IeeeSgeChain64_t sgl_ptr;
    sgl_ptr = (pMpi25IeeeSgeChain64_t)&io_request->SGL;
    pMpi25IeeeSgeChain64_t sgl_ptr_end = sgl_ptr;
    sgl_ptr_end += sc->max_sge_in_main_msg - 1;
    sgl_ptr_end->Flags = 0;
    sgl_ptr->Address = v2p(buffer, size);
    sgl_ptr->Length = size;
    sgl_ptr->Flags = IEEE_SGE_FLAGS_END_OF_LIST;
    sgl_ptr++;
    cmd->sge_count = 1;
    io_request->RaidContext.numSGE = cmd->sge_count;

    cmd->io_request->Control |= MPI2_SCSIIO_CONTROL_READ;
    cmd->io_request->SGLFlags = MPI2_SGE_FLAGS_64_BIT_ADDRESSING;
    cmd->io_request->SGLOffset0 = offsetof(MRSAS_RAID_SCSI_IO_REQUEST, SGL)/4;
    cmd->io_request->SenseBufferLowAddress = cmd->sense_phys_addr;
    cmd->io_request->SenseBufferLength = MRSAS_SCSI_SENSE_BUFFERSIZE;

    req_desc->SCSIIO.SMID = cmd->index;

    if (cmd->io_request->ChainOffset != 0 &&
	cmd->io_request->ChainOffset != 0xF) {
      DEBUG("megasas: The chain offset value is not "
	     "correct : %x\n", cmd->io_request->ChainOffset);
    }

    DEBUG("Firing write cmd (outstanding %u)...\n", sc->fw_outstanding.val);

    sc->fw_outstanding.val++;
    mrsas_fire_cmd(req_desc->addr.u.low, req_desc->addr.u.high);

    return SYS_ERR_OK;
}

static errval_t vsic_read(struct storage_vsic *vsic, struct storage_vsa *vsa,
                          off_t offset, size_t size, void *buffer)
{
    assert(vsic != NULL);
    assert(vsa != NULL);
    assert(buffer != NULL);
    /* struct megaraid_vsic *mydata = vsic->data; */

    struct read16 {
      uint8_t	opcode;
      uint8_t	flags;
      uint64_t	lba;
      uint32_t	length;
      uint8_t	group;
      uint8_t	control;
    } __attribute__ ((packed));

    buffer = pmem_start;
    assert(offset % BLOCK_SIZE == 0);
    size = STORAGE_VSIC_ROUND(vsic, size);
    /* assert(size % BLOCK_SIZE == 0); */

    struct read16 read16_cmd = {
      .opcode = 0x88,
      .flags = 0,
      .lba = htonll(offset / BLOCK_SIZE),
      .length = htonl(size / BLOCK_SIZE),
      .group = 0,
      .control = 0,
    };

    struct mrsas_mpt_cmd *cmd;
    MRSAS_REQUEST_DESCRIPTOR_UNION *req_desc;

    cmd = mrsas_get_mpt_cmd();
    assert(cmd != NULL);
    cmd->flags = MRSAS_DIR_IN;

    // Need virtual data addresses
    cmd->length = size;
    cmd->data = buffer;
    cmd->ccb_ptr = NULL;	// Data pointer we can use

    req_desc = mrsas_get_request_desc((cmd->index)-1);
    assert(req_desc != NULL);
    memset(req_desc, 0, sizeof(MRSAS_REQUEST_DESCRIPTOR_UNION));
    cmd->request_desc = req_desc;

    // command data block
    memcpy(cmd->io_request->CDB.CDB32, &read16_cmd, 16);

    // Build LDIO command
    MRSAS_RAID_SCSI_IO_REQUEST *io_request = cmd->io_request;
    io_request->RaidContext.VirtualDiskTgtId = TARGET_DEVICE_ID;
    io_request->RaidContext.status = 0;
    io_request->RaidContext.exStatus = 0;
    io_request->IoFlags = 16;	// CDB length in bytes
    io_request->RaidContext.regLockFlags = 0;
    io_request->RaidContext.timeoutValue = 0;
    cmd->request_desc->SCSIIO.RequestFlags =
      (MRSAS_REQ_DESCRIPT_FLAGS_LD_IO << MRSAS_REQ_DESCRIPT_FLAGS_TYPE_SHIFT);
    if (io_request->RaidContext.regLockFlags == REGION_TYPE_UNUSED)
      cmd->request_desc->SCSIIO.RequestFlags = (MRSAS_REQ_DESCRIPT_FLAGS_NO_LOCK << MRSAS_REQ_DESCRIPT_FLAGS_TYPE_SHIFT);
    io_request->RaidContext.Type = MPI2_TYPE_CUDA;
    io_request->RaidContext.regLockFlags |= (MR_RL_FLAGS_GRANT_DESTINATION_CPU0 | MR_RL_FLAGS_SEQ_NUM_ENABLE);
    io_request->RaidContext.nseg = 0x1;
    io_request->Function = MRSAS_MPI2_FUNCTION_LD_IO_REQUEST;
    io_request->DevHandle = TARGET_DEVICE_ID;
    io_request->DataLength = cmd->length;

    pMpi25IeeeSgeChain64_t sgl_ptr;
    sgl_ptr = (pMpi25IeeeSgeChain64_t)&io_request->SGL;
    pMpi25IeeeSgeChain64_t sgl_ptr_end = sgl_ptr;
    sgl_ptr_end += sc->max_sge_in_main_msg - 1;
    sgl_ptr_end->Flags = 0;
    sgl_ptr->Address = v2p(buffer, size);
    sgl_ptr->Length = size;
    sgl_ptr->Flags = IEEE_SGE_FLAGS_END_OF_LIST;
    sgl_ptr++;
    cmd->sge_count = 1;
    io_request->RaidContext.numSGE = cmd->sge_count;

    cmd->io_request->Control |= MPI2_SCSIIO_CONTROL_READ;
    cmd->io_request->SGLFlags = MPI2_SGE_FLAGS_64_BIT_ADDRESSING;
    cmd->io_request->SGLOffset0 = offsetof(MRSAS_RAID_SCSI_IO_REQUEST, SGL)/4;
    cmd->io_request->SenseBufferLowAddress = cmd->sense_phys_addr;
    cmd->io_request->SenseBufferLength = MRSAS_SCSI_SENSE_BUFFERSIZE;

    req_desc->SCSIIO.SMID = cmd->index;

    if (cmd->io_request->ChainOffset != 0 &&
	cmd->io_request->ChainOffset != 0xF) {
      DEBUG("megasas: The chain offset value is not "
	     "correct : %x\n", cmd->io_request->ChainOffset);
    }

    DEBUG("Firing read cmd...\n");

    sc->fw_outstanding.val++;
    mrsas_fire_cmd(req_desc->addr.u.low, req_desc->addr.u.high);

    return SYS_ERR_OK;
}

static errval_t vsic_flush(struct storage_vsic *vsic, struct storage_vsa *vsa)
{
    assert(vsic != NULL);
    assert(vsa != NULL);
    /* struct megaraid_vsic *mydata = vsic->data; */

    return SYS_ERR_OK;
}

static errval_t vsic_flush2(struct storage_vsic *vsic, struct storage_vsa *vsa,
                            void *handle)
{
    assert(vsic != NULL);
    assert(vsa != NULL);
    /* struct megaraid_vsic *mydata = vsic->data; */

    return SYS_ERR_OK;
}

static errval_t vsic_wait(struct storage_vsic *vsic)
{
    assert(vsic != NULL);
    /* struct megaraid_vsic *mydata = vsic->data; */

    poll_mode = false;

    while(sc->fw_outstanding.val > 0) {
      /* DEBUG("Outstanding cmds = %u\n", sc->fw_outstanding.val); */
      mrsas_complete_cmd();
    }

    return SYS_ERR_OK;
}

static errval_t vsic_poll(struct storage_vsic *vsic, void **handle)
{
    assert(vsic != NULL);
    /* struct megaraid_vsic *mydata = vsic->data; */

    if(sc->fw_outstanding.val > 0) {
        DEBUG("polling: Outstanding cmds = %u\n", sc->fw_outstanding.val);
        poll_mode = true;
        mrsas_complete_cmd();
        *handle = (void *)1234;
    } else {
        return FLOUNDER_ERR_TX_BUSY;
    }

    return SYS_ERR_OK;
}

static struct storage_vsic_ops megaraid_ops = {
    .write = vsic_write,
    .read = vsic_read,
    .flush = vsic_flush,
    .wait = vsic_wait,
    .flush2 = vsic_flush2,
    .poll = vsic_poll,
};

errval_t storage_vsic_driver_init(int argc, const char **argv,
				  struct storage_vsic *vsic)
{
    assert(vsic != NULL);
    struct megaraid_vsic *mydata = malloc(sizeof(struct megaraid_vsic));
    assert(mydata != NULL);
    memset(mydata, 0, sizeof(struct megaraid_vsic));

    // Init VSIC data structures
    vsic->ops = megaraid_ops;
    vsic->data = mydata;
    vsic->blocksize = BLOCK_SIZE;	// XXX: Determine from drive?

    megaraid_driver_init(argc, argv);

    pmem_start = user_alloc(BUF_SIZE, &paddr_start);
    assert(pmem_start != NULL);

    return SYS_ERR_OK;
}

errval_t storage_vsa_acquire(struct storage_vsa *vsa, const char *name,
			     size_t size)
{
    // XXX: Always return empty VSA of fixed size
    return SYS_ERR_OK;
}

errval_t storage_vsa_resize(struct storage_vsa *vsa, size_t size)
{
    assert("NYI");
    return SYS_ERR_OK;
}
