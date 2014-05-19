/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <stdint.h>
#include <string.h>
#ifdef BARRELFISH
#include <barrelfish/barrelfish.h>
#include <barrelfish/inthandler.h>
#include <barrelfish/sys_debug.h>
#include <barrelfish/deferred.h>
#include <barrelfish/waitset.h>
#include <barrelfish/core_state.h>
#include <pci/pci.h>
#include <skb/skb.h>
#include <acpi_client/acpi_client.h>
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

#include "megaraid.h"

/* PCI device address passed on command line */
#ifdef BARRELFISH
static uint32_t pci_bus = PCI_DONT_CARE;
static uint32_t pci_device = PCI_DONT_CARE;
static uint32_t pci_function = 0;
static bool use_vtd = false;
static int vtd_coherency = 1;
#endif
static uint32_t pci_deviceid = MRSAS_INVADER;

#ifndef BARRELFISH
#	define MEM_SIZE	(4 * 1024 * 1024)
#endif

#ifdef BARRELFISH
#	define DELAY(x) barrelfish_usleep(x)
#else
#	define DELAY(x) usleep((x))
#endif

#define ECONNREFUSED	61

#define MIN(a,b)	((a) < (b) ? (a) : (b))

struct megaraid_ctrl *sc = NULL;
#ifndef BARRELFISH
static uint8_t *pmem_base = NULL, *pmem_start = NULL;
static uintptr_t paddr_start = 0, paddr_base = 0, paddr_end = 0;
#endif

struct megaraid_vsic {
  struct megaraid_ctrl *ctrl;
};

#ifdef BARRELFISH
static void interrupt_handler(void *arg)
{
  assert(!"NYI");
}
#endif

/* allocate a single frame, mapping it into our vspace with given attributes */
static void *alloc_map_frame(vregion_flags_t attr, size_t size,
                             struct capref *retcap)
{
#ifdef BARRELFISH
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
#else
    /* void *va = mmap(NULL, size, */
    /* 		    PROT_READ | PROT_WRITE, */
    /* 		    MAP_PRIVATE | MAP_ANONYMOUS | MAP_POPULATE, 0, 0); */
    /* assert(va != NULL); */
    /* assert((uintptr_t)va % getpagesize() == 0); */
    size += getpagesize() - (size % getpagesize());

    assert(pmem_base != NULL);
    void *va = pmem_base;
    pmem_base += size;

    /* printf("va = %p, mapped size = %zu\n", va, size); */

    /* int fd = open("/proc/self/pagemap", O_RDONLY); */
    /* assert(fd > 0); */
    /* uint64_t pagemap; */
    /* int ret = pread(fd, &pagemap, 8, ((uintptr_t)va / getpagesize()) * 8 + 8); */
    /* assert(ret == 8); */
    /* assert(pagemap & (1ULL << 63)); // is present */
    /* assert(!(pagemap & (1ULL << 62))); // not swapped */
    /* unsigned int shift = ((pagemap >> 55) & ((1UL << 6) - 1)); */
    /* retcap->paddr = (pagemap & ((1UL << 55) - 1)) << shift; */
    retcap->paddr = paddr_base;
    paddr_base += size;
    /* printf("paddr = %p\n", (void *)retcap->paddr); */

    assert(paddr_base <= paddr_end);
    /* close(fd); */

    /* uint8_t buf[256]; */
    /* fd = open("/dev/mem", O_RDONLY); */
    /* ret = pread(fd, buf, 64, retcap->paddr); */
    /* assert(ret == 64); */
    /* for(int i = 0; i < 64; i++) { */
    /*   printf("%x\n", buf[i]); */
    /* } */
    /* close(fd); */

    /* abort(); */

    memset(va, 0, size);

    return va;
#endif
}

#ifndef BARRELFISH
lpaddr_t v2p(void *ptr, size_t len)
{
  lpaddr_t paddr;

#ifdef BARRELFISH
  if (!use_vtd) {
    // Check if it's in morecore's region
    struct morecore_state *mc_state = get_morecore_state();
    struct vspace_mmu_aware *mmu_state = &mc_state->mmu_state;
    genvaddr_t base = vregion_get_base_addr(&mmu_state->vregion);
    struct memobj_frame_list *i;

    // Walk frame list
    for(i = mmu_state->memobj.frame_list; i != NULL; i = i->next) {
      // If address is completely within frame, we can resolve
      // XXX: Everything else would be easier with an IOMMU
      /* printf("Heap: Comparing [%p:%p] against [%p:%p]\n", */
      /*        ptr, ptr + q->len, */
      /*        (void *)(base + i->offset), */
      /*        (void *)(base + i->offset + i->size)); */
      if(base + i->offset <= (genvaddr_t)ptr &&
	 ((genvaddr_t)ptr) + len < base + i->offset + i->size) {
	assert(i->pa != 0);

	/* buf->pa = id.base + ((genvaddr_t)ptr - base - i->offset); */
	paddr = i->pa + ((genvaddr_t)ptr - base - i->offset);
	return paddr;
      }
    }

    // Check if it's in text/data region
    int entry;
    for(entry = 0; entry < mc_state->v2p_entries; entry++) {
      struct v2pmap *pmap = &mc_state->v2p_mappings[entry];

      // If address is completely within frame, we can resolve
      // XXX: Everything else would be easier with an IOMMU
      /* printf("BSS: Comparing [%p:%p] against [%p:%p]\n", */
      /*        ptr, ptr + len, */
      /*        (void *)(pmap->va), */
      /*        (void *)(pmap->va + pmap->size)); */
      if(pmap->va <= (genvaddr_t)ptr &&
	 ((genvaddr_t)ptr) + len < pmap->va + pmap->size) {
	paddr = pmap->pa + ((genvaddr_t)ptr - pmap->va);
	return paddr;
      }
    }

    // Not found...
    printf("Called from %p %p %p\n",
	   __builtin_return_address(0),
	   __builtin_return_address(1),
	   __builtin_return_address(2));

    USER_PANIC("Invalid buffer! ptr = %p\n", ptr);
  } else {
    return (lpaddr_t)ptr;
  }
#else
  assert((uint8_t *)ptr >= pmem_start);
  assert((uint8_t *)ptr < pmem_start + MEM_SIZE);
  paddr = (uintptr_t)((uint8_t *)ptr - pmem_start);
  paddr += paddr_start;
  return paddr;
#endif
}
#endif

#ifndef BARRELFISH
errval_t invoke_frame_identify(struct capref cap, struct frame_identity *id)
{
  id->base = cap.paddr;
  return SYS_ERR_OK;
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
#endif

/** 
 * Interrupt Disable/Enable/Clear Functions 
 *
 */
static void mrsas_disable_intr(void)
{
    u_int32_t mask = 0xFFFFFFFF;
    megaraid_outbound_intr_mask_wr(&sc->d, mask);
    megaraid_outbound_intr_mask_rd(&sc->d);
}

/**
 * mrsas_fire_cmd:     Sends command to FW
 * input:              Adapter soft state
 *                     request descriptor address low
 *                     request descriptor address high
 *
 * This functions fires the command to Firmware by writing to the 
 * inbound_low_queue_port and inbound_high_queue_port.
 */
void mrsas_fire_cmd(u_int32_t req_desc_lo, u_int32_t req_desc_hi)
{ 
    /* mtx_lock(&sc->pci_lock); */
    megaraid_inbound_low_queue_port_wr(&sc->d, req_desc_lo);
    megaraid_inbound_high_queue_port_wr(&sc->d, req_desc_hi);
    /* mtx_unlock(&sc->pci_lock); */
}

/**
 * mrsas_alloc_frame -   Allocates MFI Frames
 * input:                Adapter soft state
 *
 * Create bus DMA memory tag and dmamap and load memory for MFI frames. 
 * Returns virtual memory pointer to allocated region. 
 */
static void *mrsas_alloc_frame(struct mrsas_mfi_cmd *cmd)
{
    u_int32_t frame_size = MRSAS_MFI_FRAME_SIZE;
    struct capref cap;

    cmd->frame_mem = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE,
				     frame_size, &cap);
    assert(cmd->frame_mem != NULL);
    struct frame_identity id;
    errval_t err = invoke_frame_identify(cap, &id);
    assert(err_is_ok(err));
    cmd->frame_phys_addr = id.base;
  
    return(cmd->frame_mem);
}

/**
 * mrsas_alloc_mfi_cmds:  Allocates the command packets
 * input:                 Adapter instance soft state
 *
 * Each IOCTL or passthru command that is issued to the FW are wrapped in a
 * local data structure called mrsas_mfi_cmd.  The frame embedded in this 
 * mrsas_mfi is issued to FW. The array is used only to look up the 
 * mrsas_mfi_cmd given the context. The free commands are maintained in a
 * linked list.
 */
static int mrsas_alloc_mfi_cmds(void)
{
    int i;
    u_int32_t max_cmd;
    struct mrsas_mfi_cmd *cmd;

    max_cmd = MRSAS_MAX_MFI_CMDS;

    /*
     * sc->mfi_cmd_list is an array of struct mrsas_mfi_cmd pointers. Allocate the
     * dynamic array first and then allocate individual commands.
     */
    sc->mfi_cmd_list = malloc(sizeof(struct mrsas_mfi_cmd*)*max_cmd);
    assert(sc->mfi_cmd_list != NULL);
    memset(sc->mfi_cmd_list, 0, sizeof(struct mrsas_mfi_cmd *)*max_cmd);
    for (i = 0; i < max_cmd; i++) {
        sc->mfi_cmd_list[i] = malloc(sizeof(struct mrsas_mfi_cmd));
	assert(sc->mfi_cmd_list[i] != NULL);
    }

    for (i = 0; i < max_cmd; i++) {
        cmd = sc->mfi_cmd_list[i];
        memset(cmd, 0, sizeof(struct mrsas_mfi_cmd));
        cmd->index = i;
        cmd->ccb_ptr = NULL;
        cmd->sc = sc; 
        TAILQ_INSERT_TAIL(&(sc->mrsas_mfi_cmd_list_head), cmd, next);
    }

    for (i = 0; i < MRSAS_MAX_MFI_CMDS; i++) {
        cmd = sc->mfi_cmd_list[i];
        cmd->frame = mrsas_alloc_frame(cmd);
	assert(cmd->frame != NULL);
        memset(cmd->frame, 0, MRSAS_MFI_FRAME_SIZE); 
        cmd->frame->io.context = cmd->index;
        cmd->frame->io.pad_0 = 0;
    }

    return(0);
}

/**
 * mrsas_alloc_mpt_cmds:  Allocates the command packets
 * input:                 Adapter instance soft state
 *
 * This function allocates the internal commands for IOs. Each command that is
 * issued to FW is wrapped in a local data structure called mrsas_mpt_cmd.
 * An array is allocated with mrsas_mpt_cmd context.  The free commands are 
 * maintained in a linked list (cmd pool). SMID value range is from 1 to
 * max_fw_cmds.
 */
static int mrsas_alloc_mpt_cmds(void)
{
    int i;
    u_int32_t max_cmd;
    struct mrsas_mpt_cmd *cmd;
    pMpi2ReplyDescriptorsUnion_t reply_desc;
    u_int32_t offset, chain_offset, sense_offset;
    lpaddr_t io_req_base_phys, chain_frame_base_phys, sense_base_phys;
    u_int8_t *io_req_base, *chain_frame_base, *sense_base;

    max_cmd = sc->max_fw_cmds;

    sc->req_desc = malloc(sc->request_alloc_sz);
    assert(sc->req_desc != NULL);
    memset(sc->req_desc, 0, sc->request_alloc_sz);

    /*
     * sc->mpt_cmd_list is an array of struct mrsas_mpt_cmd pointers. Allocate the
     * dynamic array first and then allocate individual commands.
     */
    sc->mpt_cmd_list = malloc(sizeof(struct mrsas_mpt_cmd*)*max_cmd);
    assert(sc->mpt_cmd_list != NULL);
    memset(sc->mpt_cmd_list, 0, sizeof(struct mrsas_mpt_cmd *)*max_cmd);
    for (i = 0; i < max_cmd; i++) {
        sc->mpt_cmd_list[i] = malloc(sizeof(struct mrsas_mpt_cmd));
        assert(sc->mpt_cmd_list[i] != NULL);
    }

    io_req_base = (u_int8_t*)sc->io_request_mem + MRSAS_MPI2_RAID_DEFAULT_IO_FRAME_SIZE;
    io_req_base_phys = (lpaddr_t)sc->io_request_phys_addr + MRSAS_MPI2_RAID_DEFAULT_IO_FRAME_SIZE; 
    chain_frame_base = (u_int8_t*)sc->chain_frame_mem;
    chain_frame_base_phys = (lpaddr_t)sc->chain_frame_phys_addr;
    sense_base = (u_int8_t*)sc->sense_mem;
    sense_base_phys = (lpaddr_t)sc->sense_phys_addr;
    for (i = 0; i < max_cmd; i++) {
        cmd = sc->mpt_cmd_list[i];
        offset = MRSAS_MPI2_RAID_DEFAULT_IO_FRAME_SIZE * i;
	chain_offset = 1024 * i;
        sense_offset = MRSAS_SENSE_LEN * i;
        memset(cmd, 0, sizeof(struct mrsas_mpt_cmd));
        cmd->index = i + 1;
        cmd->ccb_ptr = NULL;
        /* callout_init(&cmd->cm_callout, 0); */
        cmd->sync_cmd_idx = (u_int32_t)MRSAS_ULONG_MAX;
        cmd->sc = sc;
        cmd->io_request = (MRSAS_RAID_SCSI_IO_REQUEST *) (io_req_base + offset);
        memset(cmd->io_request, 0, sizeof(MRSAS_RAID_SCSI_IO_REQUEST));
        cmd->io_request_phys_addr = io_req_base_phys + offset;
	cmd->chain_frame = (MPI2_SGE_IO_UNION *) (chain_frame_base + chain_offset);
	cmd->chain_frame_phys_addr = chain_frame_base_phys + chain_offset;
        cmd->sense = sense_base + sense_offset;
        cmd->sense_phys_addr = sense_base_phys + sense_offset;
        /* if (bus_dmamap_create(sc->data_tag, 0, &cmd->data_dmamap)) { */
        /*     return(FAIL); */
        /* } */
        TAILQ_INSERT_TAIL(&(sc->mrsas_mpt_cmd_list_head), cmd, next);
    }
    
    /* Initialize reply descriptor array to 0xFFFFFFFF */
    reply_desc = sc->reply_desc_mem;
    for (i = 0; i < sc->reply_q_depth; i++, reply_desc++) {
        reply_desc->Words = MRSAS_ULONG_MAX;
    }

    return 0;
}

/**
 * mrsas_get_mfi_cmd:      Get a cmd from free command pool
 * input:                  Adapter soft state
 *
 * This function removes an MFI command from the command list.
 */
static struct mrsas_mfi_cmd* mrsas_get_mfi_cmd(void)
{
    struct mrsas_mfi_cmd *cmd = NULL;

    /* mtx_lock(&sc->mfi_cmd_pool_lock); */
    if (!TAILQ_EMPTY(&sc->mrsas_mfi_cmd_list_head)){
        cmd = TAILQ_FIRST(&sc->mrsas_mfi_cmd_list_head);
        TAILQ_REMOVE(&sc->mrsas_mfi_cmd_list_head, cmd, next);
    }
    /* mtx_unlock(&sc->mfi_cmd_pool_lock); */

    return cmd;
}

/**
 * mrsas_get_mpt_cmd:            Get a cmd from free command pool  
 * input:                        Adapter instance soft state 
 *
 * This function removes an MPT command from the command free list and 
 * initializes it.
 */
struct mrsas_mpt_cmd* mrsas_get_mpt_cmd(void)
{
    struct mrsas_mpt_cmd *cmd = NULL;

    /* mtx_lock(&sc->mpt_cmd_pool_lock); */
    if (!TAILQ_EMPTY(&sc->mrsas_mpt_cmd_list_head)){
        cmd = TAILQ_FIRST(&sc->mrsas_mpt_cmd_list_head);
        TAILQ_REMOVE(&sc->mrsas_mpt_cmd_list_head, cmd, next);
    }
    memset((uint8_t *)cmd->io_request, 0, MRSAS_MPI2_RAID_DEFAULT_IO_FRAME_SIZE);
    cmd->data = NULL;
    cmd->length = 0;
    cmd->flags = 0;
    cmd->error_code = 0;
    cmd->load_balance = 0;
    cmd->ccb_ptr = NULL;
    /* mtx_unlock(&sc->mpt_cmd_pool_lock); */

    return cmd;
}

/**
 * mrsas_build_mptmfi_passthru - Builds a MPT MFI Passthru command 
 * input:                        Adapter soft state
 *                               mfi cmd pointer 
 *
 * The MPT command and the io_request are setup as a passthru command. 
 * The SGE chain address is set to frame_phys_addr of the MFI command. 
 */
static u_int8_t
mrsas_build_mptmfi_passthru(struct mrsas_mfi_cmd *mfi_cmd)
{
    MPI25_IEEE_SGE_CHAIN64 *mpi25_ieee_chain;
    PTR_MRSAS_RAID_SCSI_IO_REQUEST io_req;
    struct mrsas_mpt_cmd *mpt_cmd;
    struct mrsas_header *frame_hdr = &mfi_cmd->frame->hdr;

    mpt_cmd = mrsas_get_mpt_cmd();
    if (!mpt_cmd)
        return(1);

    /* Save the smid. To be used for returning the cmd */
    mfi_cmd->cmd_id.context.smid = mpt_cmd->index;

    mpt_cmd->sync_cmd_idx = mfi_cmd->index;

    /* DEBUG("Building sync cmd #%u, from %p, %p, %p, %p\n", mpt_cmd->sync_cmd_idx, */
    /* 	  __builtin_return_address(0), */
    /* 	  __builtin_return_address(1), */
    /* 	  __builtin_return_address(2), */
    /* 	  __builtin_return_address(3)); */

    /*
     * For cmds where the flag is set, store the flag and check
     * on completion. For cmds with this flag, don't call
     * mrsas_complete_cmd.
     */

    if (frame_hdr->flags & MFI_FRAME_DONT_POST_IN_REPLY_QUEUE)
        mpt_cmd->flags = MFI_FRAME_DONT_POST_IN_REPLY_QUEUE;

    io_req = mpt_cmd->io_request;

    if ((sc->device_id == MRSAS_INVADER) || (sc->device_id == MRSAS_FURY)) {
		pMpi25IeeeSgeChain64_t sgl_ptr_end = (pMpi25IeeeSgeChain64_t) &io_req->SGL;
                sgl_ptr_end += sc->max_sge_in_main_msg - 1;
                sgl_ptr_end->Flags = 0;
    }

    mpi25_ieee_chain = (MPI25_IEEE_SGE_CHAIN64 *)&io_req->SGL.IeeeChain;

    io_req->Function    = MRSAS_MPI2_FUNCTION_PASSTHRU_IO_REQUEST;
    io_req->SGLOffset0  = offsetof(MRSAS_RAID_SCSI_IO_REQUEST, SGL) / 4;
    io_req->ChainOffset = sc->chain_offset_mfi_pthru;

    mpi25_ieee_chain->Address = mfi_cmd->frame_phys_addr;

    mpi25_ieee_chain->Flags= IEEE_SGE_FLAGS_CHAIN_ELEMENT |
              MPI2_IEEE_SGE_FLAGS_IOCPLBNTA_ADDR;

    mpi25_ieee_chain->Length = MRSAS_MAX_SZ_CHAIN_FRAME;

    return(0);
}

/**
 * mrsas_get_request_desc:     Get request descriptor from array  
 * input:                      Adapter instance soft state
 *                             SMID index 
 *
 * This function returns a pointer to the request descriptor.
 */
MRSAS_REQUEST_DESCRIPTOR_UNION *mrsas_get_request_desc(u_int16_t idx)
{
    u_int8_t *p;

    if (idx >= sc->max_fw_cmds) {
        DEBUG("Invalid SMID (0x%x)request for desc\n", idx);
        return NULL;
    }
    p = sc->req_desc + sizeof(MRSAS_REQUEST_DESCRIPTOR_UNION) * idx;

    return (MRSAS_REQUEST_DESCRIPTOR_UNION *)p;
}

/**
 * mrsas_build_mpt_cmd - Calls helper function to build Passthru cmd
 * input:                Adapter soft state
 *                       mfi cmd to build
 *
 * This function is called by mrsas_issue_cmd() to build the MPT-MFI
 * passthru command and prepares the MPT command to send to Firmware.
 */
static MRSAS_REQUEST_DESCRIPTOR_UNION *
mrsas_build_mpt_cmd(struct mrsas_mfi_cmd *cmd)
{
    MRSAS_REQUEST_DESCRIPTOR_UNION *req_desc;
    u_int16_t idx;

    if (mrsas_build_mptmfi_passthru(cmd)) {
        DEBUG("Cannot build MPT-MFI passthru cmd.\n");
        return NULL;
    }

    idx = cmd->cmd_id.context.smid;

    req_desc = mrsas_get_request_desc(idx-1);
    if(!req_desc)
        return NULL;

    req_desc->addr.Words = 0;
    req_desc->SCSIIO.RequestFlags = (MPI2_REQ_DESCRIPT_FLAGS_SCSI_IO << MRSAS_REQ_DESCRIPT_FLAGS_TYPE_SHIFT);

    req_desc->SCSIIO.SMID = idx;

    return(req_desc);
}

/**
 * mrsas_issue_dcmd -     Issues a MFI Pass thru cmd
 * input:                 Adapter soft state
 *                        mfi cmd pointer
 *
 * This function is called by mrsas_issued_blocked_cmd() and
 * mrsas_issued_polled(), to build the MPT command and then fire the 
 * command to Firmware. 
 */
static int
mrsas_issue_dcmd(struct mrsas_mfi_cmd *cmd)
{
    MRSAS_REQUEST_DESCRIPTOR_UNION *req_desc;

    req_desc = mrsas_build_mpt_cmd(cmd);
    if (!req_desc) {
        DEBUG("Cannot build MPT cmd.\n");
        return(1);
    }

    mrsas_fire_cmd(req_desc->addr.u.low, req_desc->addr.u.high);

    return(0);
}

/**
 * mrsas_issue_polled:        Issues a polling command
 * inputs:                    Adapter soft state
 *                            Command packet to be issued
 *
 * This function is for posting of internal commands to Firmware.  MFI 
 * requires the cmd_status to be set to 0xFF before posting.  The maximun
 * wait time of the poll response timer is 180 seconds.
 */
static int mrsas_issue_polled(struct mrsas_mfi_cmd *cmd)
{
    struct mrsas_header *frame_hdr = &cmd->frame->hdr;
    u_int8_t max_wait = MRSAS_INTERNAL_CMD_WAIT_TIME;
    int i, retcode = 0;

    frame_hdr->cmd_status = 0xFF;
    frame_hdr->flags |= MFI_FRAME_DONT_POST_IN_REPLY_QUEUE;

    /* Issue the frame using inbound queue port */
    if (mrsas_issue_dcmd(cmd)) {
        DEBUG("Cannot issue DCMD internal command.\n");
        return(1);
    }

    DEBUG("Waiting for return of polled command...\n");

    /* 
     * Poll response timer to wait for Firmware response.  While this   
     * timer with the DELAY call could block CPU, the time interval for 
     * this is only 1 millisecond. 
     */
    if (frame_hdr->cmd_status == 0xFF) {
        for (i=0; i < (max_wait * 1000); i++){
            if (frame_hdr->cmd_status == 0xFF)
                DELAY(1000);
            else
                break;
        } 
    }

    DEBUG("Polled command returned.\n");

    if (frame_hdr->cmd_status != 0)
    {
        if (frame_hdr->cmd_status == 0xFF)
            DEBUG("DCMD timed out after %d seconds.\n", max_wait); 
        else
            DEBUG("DCMD failed, status = 0x%x\n", frame_hdr->cmd_status);
        retcode = 1;
    }

    return(retcode);
}

/**
 * mrsas_release_mfi_cmd: Return a cmd to free command pool
 * input:                 Command packet for return to free cmd pool 
 *
 * This function returns the MFI command to the command list.
 */
static void mrsas_release_mfi_cmd(struct mrsas_mfi_cmd *cmd)
{
    struct megaraid_ctrl *s = cmd->sc;

    /* mtx_lock(&sc->mfi_cmd_pool_lock); */
    cmd->ccb_ptr = NULL;
    cmd->cmd_id.frame_count = 0;
    TAILQ_INSERT_TAIL(&(s->mrsas_mfi_cmd_list_head), cmd, next);
    /* mtx_unlock(&sc->mfi_cmd_pool_lock); */

    return;
}

/**
 * MR_ValidateMapInfo:        Validate RAID map
 * input:                     Adapter instance soft state
 *
 * This function checks and validates the loaded RAID map. It returns 0 if 
 * successful, and 1 otherwise.
 */
static u_int8_t MR_ValidateMapInfo(void)
{
    uint32_t total_map_sz;
    MR_FW_RAID_MAP_ALL *map = sc->raidmap_mem[(sc->map_id & 1)];
    MR_FW_RAID_MAP *pFwRaidMap = &map->raidMap;
    /* PLD_SPAN_INFO ldSpanInfo = (PLD_SPAN_INFO) &sc->log_to_span; */

    total_map_sz = (sizeof(MR_FW_RAID_MAP) - sizeof(MR_LD_SPAN_MAP) +
                     (sizeof(MR_LD_SPAN_MAP) * pFwRaidMap->ldCount));

    if (pFwRaidMap->totalSize != total_map_sz) {
        DEBUG("map size %x not matching ld count\n", total_map_sz);
        DEBUG("span map= %x\n", (unsigned int)sizeof(MR_LD_SPAN_MAP));
        DEBUG("pFwRaidMap->totalSize=%x\n", pFwRaidMap->totalSize);
        return 1;
    }

    printf("Max logical drives = %u\n", pFwRaidMap->raid_desc.validationInfo.maxLd);
    printf("Num Logical drives = %u\n", pFwRaidMap->ldCount);

    /* if (sc->UnevenSpanSupport) { */
    /*     mr_update_span_set(map, ldSpanInfo); */
    /* } */

    /* mrsas_update_load_balance_params(map, sc->load_balance_info); */

    return 0;
}

/*
 * Various RAID map access functions.  These functions access the various
 * parts of the RAID map and returns the appropriate parameters. 
 */

static MR_LD_RAID *MR_LdRaidGet(u_int32_t ld, MR_FW_RAID_MAP_ALL *map)
{
    return (&map->raidMap.ldSpanMap[ld].ldRaid);
}

static u_int16_t MR_GetLDTgtId(u_int32_t ld, MR_FW_RAID_MAP_ALL *map)
{
    return (map->raidMap.ldSpanMap[ld].ldRaid.targetId);
}

/**
 * mrsas_sync_map_info:        Get FW's ld_map structure
 * input:                      Adapter instance soft state
 *
 * Issues an internal command (DCMD) to get the FW's controller PD
 * list structure.  
 */
static int mrsas_sync_map_info(void)
{
    int retcode = 0, i;
    struct mrsas_mfi_cmd *cmd;
    struct mrsas_dcmd_frame *dcmd;
    uint32_t num_lds;
    MR_LD_TARGET_SYNC *target_map = NULL;
    MR_FW_RAID_MAP_ALL *map;
    MR_LD_RAID  *raid;
    MR_LD_TARGET_SYNC *ld_sync;
    lpaddr_t map_phys_addr = 0;

    cmd = mrsas_get_mfi_cmd();
    assert(cmd != NULL);

    map = sc->raidmap_mem[sc->map_id & 1];
    num_lds = map->raidMap.ldCount;
    
    dcmd = &cmd->frame->dcmd;
    memset(dcmd->mbox.b, 0, MFI_MBOX_SIZE);

    target_map = (MR_LD_TARGET_SYNC *)sc->raidmap_mem[(sc->map_id - 1) & 1];
    memset(target_map, 0, sizeof(MR_FW_RAID_MAP_ALL));

    map_phys_addr = sc->raidmap_phys_addr[(sc->map_id - 1) & 1];

    ld_sync = (MR_LD_TARGET_SYNC *)target_map;

    for (i = 0; i < num_lds; i++, ld_sync++) {
        raid = MR_LdRaidGet(i, map);
        ld_sync->targetId = MR_GetLDTgtId(i, map);
        ld_sync->seqNum = raid->seqNum;
    }

    dcmd->cmd = MFI_CMD_DCMD;
    dcmd->cmd_status = 0xFF;
    dcmd->sge_count = 1;
    dcmd->flags = MFI_FRAME_DIR_WRITE;
    dcmd->timeout = 0;
    dcmd->pad_0 = 0;
    dcmd->data_xfer_len = sc->map_sz;
    dcmd->mbox.b[0] = num_lds;
    dcmd->mbox.b[1] = MRSAS_DCMD_MBOX_PEND_FLAG;
    dcmd->opcode = MR_DCMD_LD_MAP_GET_INFO;
    dcmd->sgl.sge32[0].phys_addr = map_phys_addr;
    dcmd->sgl.sge32[0].length = sc->map_sz;

    sc->map_update_cmd = cmd;
    if (mrsas_issue_dcmd(cmd)) {
        DEBUG("Fail to send sync map info command.\n");
        return(1);
    }
    return(retcode);
}

/**
 * mrsas_alloc_tmp_dcmd:       Allocates memory for temporary command
 * input:                      Adapter soft state
 *                             Temp command
 *                             Size of alloction
 *
 * Allocates DMAable memory for a temporary internal command. The allocated
 * memory is initialized to all zeros upon successful loading of the dma 
 * mapped memory.
 */
static int mrsas_alloc_tmp_dcmd(struct mrsas_tmp_dcmd *tcmd, int size)
{
    struct capref cap;
    tcmd->tmp_dcmd_mem =
      alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE, size, &cap);
    if(use_vtd) {
        tcmd->tmp_dcmd_phys_addr = (lpaddr_t)tcmd->tmp_dcmd_mem;
    } else {
        struct frame_identity id;
        errval_t err = invoke_frame_identify(cap, &id);
        assert(err_is_ok(err));
        tcmd->tmp_dcmd_phys_addr = id.base;
    }

    memset(tcmd->tmp_dcmd_mem, 0, size);
    return (0);
}

/**
 * mrsas_alloc_ctlr_info_cmd:  Allocates memory for controller info command
 * input:                      Adapter soft state
 *
 * Allocates DMAable memory for the controller info internal command.
 */
static int mrsas_alloc_ctlr_info_cmd(void)
{
    int ctlr_info_size;

    /* Allocate get controller info command */
    ctlr_info_size = sizeof(struct mrsas_ctrl_info);
    struct capref cap;
    sc->ctlr_info_mem =
      alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE, ctlr_info_size, &cap);
    if(use_vtd) {
        sc->ctlr_info_phys_addr = (lpaddr_t)sc->ctlr_info_mem;
    } else {
        struct frame_identity id;
        errval_t err = invoke_frame_identify(cap, &id);
        assert(err_is_ok(err));
        sc->ctlr_info_phys_addr = id.base;
    }

    memset(sc->ctlr_info_mem, 0, ctlr_info_size);
    return (0);
}

/**
 * mrsas_get_controller_info -        Returns FW's controller structure
 * input:                             Adapter soft state
 *                                    Controller information structure
 *
 * Issues an internal command (DCMD) to get the FW's controller structure.
 * This information is mainly used to find out the maximum IO transfer per
 * command supported by the FW.
 */
static int mrsas_get_ctrl_info(struct mrsas_ctrl_info *ctrl_info)
{
    int retcode = 0;
    struct mrsas_mfi_cmd *cmd;
    struct mrsas_dcmd_frame *dcmd;

    cmd = mrsas_get_mfi_cmd();
    assert(cmd != NULL);

    dcmd = &cmd->frame->dcmd;

    if (mrsas_alloc_ctlr_info_cmd() != SUCCESS) {
        DEBUG("Cannot allocate get ctlr info cmd\n");
        mrsas_release_mfi_cmd(cmd);
	abort();
    }
    memset(dcmd->mbox.b, 0, MFI_MBOX_SIZE);

    dcmd->cmd = MFI_CMD_DCMD;
    dcmd->cmd_status = 0xFF;
    dcmd->sge_count = 1;
    dcmd->flags = MFI_FRAME_DIR_READ;
    dcmd->timeout = 0;
    dcmd->pad_0 = 0;
    dcmd->data_xfer_len = sizeof(struct mrsas_ctrl_info);
    dcmd->opcode = MR_DCMD_CTRL_GET_INFO;
    dcmd->sgl.sge32[0].phys_addr = sc->ctlr_info_phys_addr;
    dcmd->sgl.sge32[0].length = sizeof(struct mrsas_ctrl_info);

    if (!mrsas_issue_polled(cmd)) 
        memcpy(ctrl_info, sc->ctlr_info_mem, sizeof(struct mrsas_ctrl_info));
    else 
        retcode = 1;

    /* mrsas_free_ctlr_info_cmd(sc); */
    mrsas_release_mfi_cmd(cmd);
    return(retcode);
}

/**
 * mrsas_get_ld_list:           Returns FW's LD list structure
 * input:                       Adapter soft state
 *
 * Issues an internal command (DCMD) to get the FW's controller PD
 * list structure.  This information is mainly used to find out about
 * supported by the FW.
 */
static int mrsas_get_ld_list(void)
{
    int ld_list_size, retcode = 0, ld_index = 0, ids = 0;
    struct mrsas_mfi_cmd *cmd;
    struct mrsas_dcmd_frame *dcmd;
    struct MR_LD_LIST *ld_list_mem;
    lpaddr_t ld_list_phys_addr = 0;
    struct mrsas_tmp_dcmd *tcmd;

    cmd = mrsas_get_mfi_cmd();
    assert(cmd != NULL);

    dcmd = &cmd->frame->dcmd;

    tcmd = malloc(sizeof(struct mrsas_tmp_dcmd));
    ld_list_size = sizeof(struct MR_LD_LIST);
    if (mrsas_alloc_tmp_dcmd(tcmd, ld_list_size) != SUCCESS) {
        DEBUG("Cannot alloc dmamap for get LD list cmd\n");
        mrsas_release_mfi_cmd(cmd);
	abort();
    }
    else {
        ld_list_mem = tcmd->tmp_dcmd_mem;
        ld_list_phys_addr = tcmd->tmp_dcmd_phys_addr;
    }
    memset(dcmd->mbox.b, 0, MFI_MBOX_SIZE);

    dcmd->cmd = MFI_CMD_DCMD;
    dcmd->cmd_status = 0xFF;
    dcmd->sge_count = 1;
    dcmd->flags = MFI_FRAME_DIR_READ;
    dcmd->timeout = 0;
    dcmd->data_xfer_len = sizeof(struct MR_LD_LIST);
    dcmd->opcode = MR_DCMD_LD_GET_LIST;
    dcmd->sgl.sge32[0].phys_addr = ld_list_phys_addr;
    dcmd->sgl.sge32[0].length = sizeof(struct MR_LD_LIST);
    dcmd->pad_0  = 0;

    if (!mrsas_issue_polled(cmd)) 
        retcode = 0;
    else 
        retcode = 1;

    DEBUG("Logical drive list: retcode = %d, ldcount = %d\n",
	  retcode, ld_list_mem->ldCount);

     /* Get the instance LD list */ 
     if ((retcode == 0) && (ld_list_mem->ldCount <= (MAX_LOGICAL_DRIVES))){
        sc->CurLdCount = ld_list_mem->ldCount;
        memset(sc->ld_ids, 0xff, MRSAS_MAX_LD);
        for (ld_index = 0; ld_index < ld_list_mem->ldCount; ld_index++) {
            if (ld_list_mem->ldList[ld_index].state != 0) {
                ids = ld_list_mem->ldList[ld_index].ref.ld_context.targetId;
                sc->ld_ids[ids] = ld_list_mem->ldList[ld_index].ref.ld_context.targetId;
		DEBUG("Logical drive %d, ID = %d, state = %u, size = %" PRIu64 "\n",
		      ld_index, ids,
		      ld_list_mem->ldList[ld_index].state,
		      ld_list_mem->ldList[ld_index].size);
            }
        }
    } 

    /* mrsas_free_tmp_dcmd(tcmd); */
    mrsas_release_mfi_cmd(cmd);
    free(tcmd);
    return(retcode);
}

static void mrsas_enable_intr(void)
{
    u_int32_t mask = MFI_FUSION_ENABLE_INTERRUPT_MASK;

    megaraid_outbound_intr_status_wr(&sc->d, ~0);
    /* mrsas_write_reg(sc, offsetof(mrsas_reg_set, outbound_intr_status), ~0); */
    megaraid_outbound_intr_status_rd(&sc->d);
    /* status = mrsas_read_reg(sc, offsetof(mrsas_reg_set, outbound_intr_status)); */

    megaraid_outbound_intr_mask_wr(&sc->d, ~mask);
    /* mrsas_write_reg(sc, offsetof(mrsas_reg_set, outbound_intr_mask), ~mask); */
    megaraid_outbound_intr_mask_rd(&sc->d);
    /* status = mrsas_read_reg(sc, offsetof(mrsas_reg_set, outbound_intr_mask)); */
}

#ifdef BARRELFISH
static void pci_init_card(struct device_mem *bar_info, int bar_count)
#else
static void pci_init_card(int bar_count)
#endif
{
    errval_t err;

    sc = malloc(sizeof(struct megaraid_ctrl));
    assert(sc != NULL);
    sc->device_id = pci_deviceid;

    /* Intialize mutexes */
    /* mtx_init(&sc->sim_lock,  "mrsas_sim_lock", NULL, MTX_DEF); */
    /* mtx_init(&sc->pci_lock,  "mrsas_pci_lock", NULL, MTX_DEF); */
    /* mtx_init(&sc->io_lock,  "mrsas_io_lock", NULL, MTX_DEF); */
    /* mtx_init(&sc->aen_lock,  "mrsas_aen_lock", NULL, MTX_DEF); */
    /* mtx_init(&sc->ioctl_lock,  "mrsas_ioctl_lock", NULL, MTX_SPIN); */
    /* mtx_init(&sc->mpt_cmd_pool_lock, "mrsas_mpt_cmd_pool_lock", NULL, MTX_DEF); */
    /* mtx_init(&sc->mfi_cmd_pool_lock, "mrsas_mfi_cmd_pool_lock", NULL, MTX_DEF); */
    /* mtx_init(&sc->raidmap_lock, "mrsas_raidmap_lock", NULL, MTX_DEF); */

    /* Intialize linked list */
    TAILQ_INIT(&sc->mrsas_mpt_cmd_list_head);
    TAILQ_INIT(&sc->mrsas_mfi_cmd_list_head);

    sc->fw_outstanding.val = 0;
    /* atomic_set(&sc->fw_outstanding,0); */

    // Map first memory BAR for memory mapped register access
    assert(bar_count >= 2);
#ifdef BARRELFISH
    map_device(&bar_info[0]);
    DEBUG("BAR[0] mapped (v=%llx p=%llx l=%llx)\n",
            (unsigned long long) bar_info[0].vaddr,
            (unsigned long long) bar_info[0].paddr,
            (unsigned long long) bar_info[0].bytes);

    // Initialize Mackerel binding
    megaraid_initialize(&sc->d, (void *)bar_info[0].vaddr);
#else
    sc->uiofd = open("/dev/uio0", O_RDWR);
    assert(sc->uiofd > 0);
    sc->configfd = open("/sys/class/uio/uio0/device/config", O_RDWR | O_SYNC);
    assert(sc->configfd > 0);
    int resfd = open("/sys/bus/pci/devices/0000:09:00.0/resource1", O_RDWR | O_SYNC);
    assert(resfd > 0);

    /* uint16_t *cfgptr = mmap(NULL, 256, PROT_READ | PROT_WRITE, MAP_SHARED, sc->configfd, 0); */
    /* assert(cfgptr != MAP_FAILED); */

    /* uint16_t cfgptr[256]; */
    /* int ret = pread(sc->configfd, cfgptr, 256, 0); */
    /* assert(ret == 256); */

    /* printf("vendor = %x\n", cfgptr[0]); */
    /* printf("device = %x\n", cfgptr[1]); */
    /* printf("cmd = %x\n", cfgptr[2]); */

    void *vaddr = mmap(NULL, 65536, PROT_READ | PROT_WRITE, MAP_SHARED, resfd, 0);
    if(vaddr == (void *)-1) {
      perror("mmap");
    }
    assert(vaddr != (void *)-1);
    megaraid_initialize(&sc->d, (void *)vaddr);
#endif

    // TODO: Transition device to ready state
    // See mrsas.c:mrsas_transition_to_ready()
#ifdef MEGARAID_DEBUG
    uint32_t status = megaraid_status_rd(&sc->d);
#endif
    megaraid_status_t state = megaraid_status_state_rdf(&sc->d);
    DEBUG("Status register = 0x%x, state = 0x%x\n", status, state);
    switch(state) {
    case megaraid_state_ready:
      break;

    case megaraid_state_operational:
      /* DEBUG("Transitioning\n"); */
      mrsas_disable_intr();
      megaraid_doorbell_wr(&sc->d, MFI_RESET_FLAGS);
      for (int i=0; i < MRSAS_RESET_WAIT_TIME * 1000; i++) {
	if (megaraid_doorbell_rd(&sc->d) & 1)
	  DELAY(1000);
	else
	  break;
      }
      break;

    default:
      assert(state == megaraid_state_ready);
      break;
    }

    // -1 is needed (cf. mrsas.c:mrsas_init_adapter())
    sc->max_fw_cmds = megaraid_status_max_cmds_rdf(&sc->d) - 1;
    DEBUG("Max commands = %u\n", sc->max_fw_cmds);

    /* Determine allocation size of command frames */
    sc->reply_q_depth = ((sc->max_fw_cmds *2 +1 +15)/16*16);
    sc->request_alloc_sz = sizeof(MRSAS_REQUEST_DESCRIPTOR_UNION) * sc->max_fw_cmds;
    sc->reply_alloc_sz = sizeof(MPI2_REPLY_DESCRIPTORS_UNION) * (sc->reply_q_depth);
    sc->io_frames_alloc_sz = MRSAS_MPI2_RAID_DEFAULT_IO_FRAME_SIZE + (MRSAS_MPI2_RAID_DEFAULT_IO_FRAME_SIZE * (sc->max_fw_cmds + 1));
    sc->chain_frames_alloc_sz = 1024 * sc->max_fw_cmds;
    sc->max_sge_in_main_msg = (MRSAS_MPI2_RAID_DEFAULT_IO_FRAME_SIZE - 
        offsetof(MRSAS_RAID_SCSI_IO_REQUEST, SGL))/16;

    sc->max_sge_in_chain = MRSAS_MAX_SZ_CHAIN_FRAME / sizeof(MPI2_SGE_IO_UNION);
    sc->max_num_sge = sc->max_sge_in_main_msg + sc->max_sge_in_chain - 2;

    /* Used for pass thru MFI frame (DCMD) */
    sc->chain_offset_mfi_pthru = offsetof(MRSAS_RAID_SCSI_IO_REQUEST, SGL)/16;

    sc->chain_offset_io_request = (MRSAS_MPI2_RAID_DEFAULT_IO_FRAME_SIZE - 
        sizeof(MPI2_SGE_IO_UNION))/16;

    sc->last_reply_idx = 0;

    u_int32_t verbuf_size, io_req_size, reply_desc_size, sense_size,
              chain_frame_size, evt_detail_size;
    /*
     * Allocate for version buffer
     */
    struct capref cap;
    struct frame_identity id;
    verbuf_size = MRSAS_MAX_NAME_LENGTH*(sizeof(lpaddr_t));
    sc->verbuf_mem = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE, verbuf_size, &cap);
    if(use_vtd) {
        sc->verbuf_phys_addr = (lpaddr_t)sc->verbuf_mem;
    } else {
        err = invoke_frame_identify(cap, &id);
        assert(err_is_ok(err));
        sc->verbuf_phys_addr = id.base;
    }

    /*
     * Allocate IO Request Frames
     */ 
    io_req_size = sc->io_frames_alloc_sz; 
    sc->io_request_mem = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE, io_req_size, &cap);
    if(use_vtd) {
        sc->io_request_phys_addr = (lpaddr_t)sc->io_request_mem;
    } else {
        err = invoke_frame_identify(cap, &id);
        assert(err_is_ok(err));
        sc->io_request_phys_addr = id.base;
    }

    /*
     * Allocate Chain Frames
     */
    chain_frame_size = sc->chain_frames_alloc_sz;
    sc->chain_frame_mem = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE, chain_frame_size, &cap);
    if(use_vtd) {
        sc->chain_frame_phys_addr = (lpaddr_t)sc->chain_frame_mem;
    } else {
        err = invoke_frame_identify(cap, &id);
        assert(err_is_ok(err));
        sc->chain_frame_phys_addr = id.base;
    }

    /*
     * Allocate Reply Descriptor Array
     */ 
    reply_desc_size = sc->reply_alloc_sz; 
    sc->reply_desc_mem = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE, reply_desc_size, &cap);
    if(use_vtd) {
        sc->reply_desc_phys_addr = (lpaddr_t)sc->reply_desc_mem;
    } else {
        err = invoke_frame_identify(cap, &id);
        assert(err_is_ok(err));
        sc->reply_desc_phys_addr = id.base;
    }

    /*
     * Allocate Sense Buffer Array.  Keep in lower 4GB 
     */
    sense_size = sc->max_fw_cmds * MRSAS_SENSE_LEN;
    sc->sense_mem = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE, sense_size, &cap);
    if(use_vtd) {
        sc->sense_phys_addr = (lpaddr_t)sc->sense_mem;
    } else {
        err = invoke_frame_identify(cap, &id);
        assert(err_is_ok(err));
        sc->sense_phys_addr = id.base;
    }

    /*
     * Allocate for Event detail structure
     */ 
    evt_detail_size = sizeof(struct mrsas_evt_detail); 
    sc->evt_detail_mem = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE, evt_detail_size, &cap);
    if(use_vtd) {
        sc->evt_detail_phys_addr = (lpaddr_t)sc->evt_detail_mem;
    } else {
        err = invoke_frame_identify(cap, &id);
        assert(err_is_ok(err));
        sc->evt_detail_phys_addr = id.base;
    }

    mrsas_alloc_mpt_cmds();

    /* Allocate memory for the IOC INIT command */
    int ioc_init_size;
    ioc_init_size = 1024 + sizeof(MPI2_IOC_INIT_REQUEST); 
    sc->ioc_init_mem = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE, ioc_init_size, &cap);
    memset(sc->ioc_init_mem, 0, ioc_init_size);
    if(use_vtd) {
        sc->ioc_init_phys_mem = (lpaddr_t)sc->ioc_init_mem;
    } else {
        err = invoke_frame_identify(cap, &id);
        assert(err_is_ok(err));
        sc->ioc_init_phys_mem = id.base;
    }

    DEBUG("IOC Init frame at 0x%" PRIxLPADDR " in phys mem\n", sc->ioc_init_phys_mem);

    // Issue IOC init command to firmware
    pMpi2IOCInitRequest_t   IOCInitMsg;
    IOCInitMsg = (pMpi2IOCInitRequest_t)(((char *)sc->ioc_init_mem) +1024);
    IOCInitMsg->Function = MPI2_FUNCTION_IOC_INIT;
    IOCInitMsg->WhoInit = MPI2_WHOINIT_HOST_DRIVER;
    IOCInitMsg->MsgVersion = MPI2_VERSION;
    IOCInitMsg->HeaderVersion = MPI2_HEADER_VERSION;
    IOCInitMsg->SystemRequestFrameSize = MRSAS_MPI2_RAID_DEFAULT_IO_FRAME_SIZE / 4;
    IOCInitMsg->ReplyDescriptorPostQueueDepth = sc->reply_q_depth;
    IOCInitMsg->ReplyDescriptorPostQueueAddress = sc->reply_desc_phys_addr;
    IOCInitMsg->SystemRequestFrameBaseAddress = sc->io_request_phys_addr;

    volatile struct mrsas_init_frame *init_frame;
    init_frame = (struct mrsas_init_frame *)sc->ioc_init_mem;
    init_frame->cmd = MFI_CMD_INIT;
    init_frame->cmd_status = 0xFF;
    init_frame->flags |= MFI_FRAME_DONT_POST_IN_REPLY_QUEUE;

    if (sc->verbuf_mem != NULL) {
        snprintf((char *)sc->verbuf_mem, strlen(MRSAS_VERSION)+2,"%s\n",
                MRSAS_VERSION);
        init_frame->driver_ver_lo = (lpaddr_t)sc->verbuf_phys_addr;
        init_frame->driver_ver_hi = 0;
    }

    lpaddr_t phys_addr;
    phys_addr = (lpaddr_t)sc->ioc_init_phys_mem + 1024;
    init_frame->queue_info_new_phys_addr_lo = phys_addr;
    init_frame->data_xfer_len = sizeof(Mpi2IOCInitRequest_t);

    MRSAS_REQUEST_DESCRIPTOR_UNION req_desc;
    req_desc.addr.Words = (lpaddr_t)sc->ioc_init_phys_mem;
    req_desc.MFAIo.RequestFlags = 
        (MRSAS_REQ_DESCRIPT_FLAGS_MFA << MRSAS_REQ_DESCRIPT_FLAGS_TYPE_SHIFT);

    mrsas_disable_intr();
    DEBUG("Issuing IOC INIT command to FW. 0x%x low, 0x%x high\n",
          req_desc.addr.u.low, req_desc.addr.u.high);
    assert(init_frame->cmd_status == 0xFF);

    sys_debug_flush_cache();

    mrsas_fire_cmd(req_desc.addr.u.low, req_desc.addr.u.high);

    DEBUG("Waiting for status\n");

    /*
     * Poll response timer to wait for Firmware response.  While this
     * timer with the DELAY call could block CPU, the time interval for
     * this is only 1 millisecond.
     */
    /* while((volatile u_int8_t)init_frame->cmd_status == 0xFF); */
    u_int8_t max_wait = MRSAS_IOC_INIT_WAIT_TIME;
    if (init_frame->cmd_status == 0xFF) {
        for (int i=0; i < (max_wait * 1000); i++){
	  if ((volatile u_int8_t)init_frame->cmd_status == 0xFF)
                DELAY(1000);
            else
                break;
        } 
    }

    if (init_frame->cmd_status == 0)
        DEBUG("IOC INIT response received from FW.\n");
    else {
        DEBUG("IOC Init failed, status = 0x%x\n", init_frame->cmd_status);
	abort();
    }

    /* mrsas_free_ioc_cmd(sc); */

    mrsas_alloc_mfi_cmds();

    // Allocate DMA memory for RAID maps
    sc->map_sz = sizeof(MR_FW_RAID_MAP) +
                (sizeof(MR_LD_SPAN_MAP) * (MAX_LOGICAL_DRIVES - 1));
    for(int i = 0; i < 2; i++) {
        sc->raidmap_mem[i] = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE,
                                             sc->map_sz, &cap);
        if(use_vtd) {
            sc->raidmap_phys_addr[i] = (lpaddr_t)sc->raidmap_mem[i];
        } else {
            err = invoke_frame_identify(cap, &id);
            assert(err_is_ok(err));
            sc->raidmap_phys_addr[i] = id.base;
        }
    }

    /* DEBUG("Getting RAID map\n"); */

    /* if (!mrsas_get_map_info()) */
    /*     mrsas_sync_map_info(); */

    /* DEBUG("Getting physical drive list\n"); */

    /* memset(sc->pd_list, 0, MRSAS_MAX_PD * sizeof(struct mrsas_pd_list)); */
    /* mrsas_get_pd_list(); */

    memset(sc->ld_ids, 0xff, MRSAS_MAX_LD);
    mrsas_get_ld_list();

    struct mrsas_ctrl_info *ctrl_info;
    ctrl_info = malloc(sizeof(struct mrsas_ctrl_info));

    /*
     * Compute the max allowed sectors per IO: The controller info has two
     * limits on max sectors. Driver should use the minimum of these two.
     *
     * 1 << stripe_sz_ops.min = max sectors per strip
     *
     * Note that older firmwares ( < FW ver 30) didn't report information
     * to calculate max_sectors_1. So the number ended up as zero always.
     */
    u_int32_t max_sectors_1;
    u_int32_t max_sectors_2;
    u_int32_t tmp_sectors;
    tmp_sectors = 0;
    if (ctrl_info && !mrsas_get_ctrl_info(ctrl_info)) {
      DEBUG("Ctrl name '%s'\n",
	    ctrl_info->image_component[0].name);
      DEBUG("Product name '%s'\n",
	    ctrl_info->product_name);
      DEBUG("NVRAM size %u MB\n",
	    ctrl_info->nvram_size);
      DEBUG("Memory size %u MB\n",
	    ctrl_info->memory_size);
      DEBUG("Flash size %u MB\n",
	    ctrl_info->flash_size);
      /* DEBUG("port count = %u\n", ctrl_info->host_interface.port_count); */
      /* DEBUG("port count = %u\n", ctrl_info->device_interface.port_count); */
      /* for(int i = 0; i < 8; i++) { */
      /* 	DEBUG("image name %d = '%s'\n", i, ctrl_info->image_component[i].name); */
      /* } */
      /* DEBUG("UART present = %u\n", ctrl_info->hw_present.uart); */
      /* DEBUG("PD present = %u\n", ctrl_info->pd_present_count); */
      /* DEBUG("Package version = '%s'\n", ctrl_info->package_version); */

        max_sectors_1 = (1 << ctrl_info->stripe_sz_ops.min) *
                    ctrl_info->max_strips_per_io;
        max_sectors_2 = ctrl_info->max_request_size;
        tmp_sectors = MIN(max_sectors_1 , max_sectors_2);
        sc->disableOnlineCtrlReset = 
            ctrl_info->properties.OnOffProperties.disableOnlineCtrlReset;
        sc->UnevenSpanSupport = 
            ctrl_info->adapterOperations2.supportUnevenSpans;
        if(sc->UnevenSpanSupport) {
	  DEBUG("FW supports: UnevenSpanSupport=%x\n",
                sc->UnevenSpanSupport);
            if (MR_ValidateMapInfo())
           	    sc->fast_path_io = 1;
            else
                sc->fast_path_io = 0;

        }
    }
    sc->max_sectors_per_req = sc->max_num_sge * MRSAS_PAGE_SIZE / 512;

    if (tmp_sectors && (sc->max_sectors_per_req > tmp_sectors))
        sc->max_sectors_per_req = tmp_sectors;

    if (ctrl_info)
        free(ctrl_info);

    mrsas_enable_intr();

    mrsas_complete_cmd();

    /* mrsas_start_aen(); */
}

int megaraid_driver_init(int argc, const char **argv)
{
#ifdef BARRELFISH
    errval_t r;

    r = pci_client_connect();
    assert(err_is_ok(r));
    DEBUG("connected to pci\n");

    r = pci_register_driver_irq(pci_init_card, PCI_CLASS_MASS_STORAGE,
                                PCI_SUB_RAID, PCI_DONT_CARE,
                                PCI_VENDOR_LSI, pci_deviceid,
                                pci_bus, pci_device, pci_function,
                                interrupt_handler, NULL);
    assert(err_is_ok(r));

    while(sc == NULL) {
        event_dispatch(get_default_waitset());
    }

    errval_t err = skb_client_connect();
    assert(err_is_ok(err));

    err = skb_execute_query("vtd_enabled(0,C), write(vtd_coherency(C)).");
    if (err_is_ok(err)) {
        use_vtd = true;
        /* for(int i = 0; i < *argc; i++) {  */
	/*     if(!strncmp((*argv)[i], "use_vtd=", strlen("use_vtd=") - 1)) { */
	/*       use_vtd = !!atol((*argv)[i] + strlen("use_vtd=")); */
        /*         break; */
        /*     } */
        /* } */
	err = skb_read_output("vtd_coherency(%d)", &vtd_coherency);
	assert(err_is_ok(err));
    }

    if (use_vtd) {
        err = connect_to_acpi();
	assert(err_is_ok(err));
	err = vtd_create_domain(cap_vroot);
	assert(err_is_ok(err));
        printf("megaraid: Using VT-d on bus 9\n");
	err = vtd_domain_add_device(0, 9, 0, 0, cap_vroot);
	assert(err_is_ok(err));
    }
#else
    if(argc < 2) {
      printf("Usage: %s PADDR\n", argv[0]);
      exit(1);
    }

    int fd = open("/dev/mem", O_RDWR | O_SYNC);
    assert(fd > 0);

    uintptr_t paddr = strtoul(argv[1], NULL, 0);

    uint32_t *ptr = mmap(NULL, MEM_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, fd, paddr);
    if(ptr == MAP_FAILED) {
      perror("mmap");
      exit(1);
    }
    assert(ptr != MAP_FAILED);
    /* printf("ptr = %x\n", *ptr); */
    pmem_start = pmem_base = (uint8_t *)ptr;
    paddr_start = paddr_base = paddr;
    paddr_end = paddr + MEM_SIZE;

    pci_init_card(3);
#endif

    return 0;
}

/**
 * mrsas_release_mpt_cmd:      Return a cmd to free command pool  
 * input:                      Command packet for return to free command pool 
 *
 * This function returns an MPT command to the free command list.
 */
static void mrsas_release_mpt_cmd(struct mrsas_mpt_cmd *cmd) 
{
    /* struct mrsas_softc *sc = cmd->sc; */

    /* mtx_lock(&sc->mpt_cmd_pool_lock); */
    cmd->sync_cmd_idx = (u_int32_t)MRSAS_ULONG_MAX;
    TAILQ_INSERT_TAIL(&(sc->mrsas_mpt_cmd_list_head), cmd, next);
    /* mtx_unlock(&sc->mpt_cmd_pool_lock); */

    return; 
}

/**
 * mrsas_complete_abort:      Completes aborting a command
 * input:                     Adapter soft state
 *                            Cmd that was issued to abort another cmd
 *
 * The mrsas_issue_blocked_abort_cmd() function waits for the command status
 * to change after sending the command.  This function is called from 
 * mrsas_complete_mptmfi_passthru() to wake up the sleep thread associated.
 */
static void mrsas_complete_abort(struct mrsas_mfi_cmd *cmd)
{
    if (cmd->sync_cmd) {
        cmd->sync_cmd = 0;
        cmd->cmd_status = 0;
        sc->chan = (void*)&cmd;
        /* wakeup_one((void *)&sc->chan); */
	assert(!"NYI");
    }
    return;
}

/**
 * mrsas_complete_aen:        	Completes AEN command
 * input:                     	Adapter soft state
 *                            	Cmd that was issued to abort another cmd
 *
 * 								This function will be called from ISR and will continue 
 * 								event processing from thread context by enqueuing task
 * 								in ev_tq (callback function "mrsas_aen_handler").
 */
static void mrsas_complete_aen(struct mrsas_mfi_cmd *cmd)
{
	/*
	* Don't signal app if it is just an aborted previously registered aen
	*/
	if ((!cmd->abort_aen) && (sc->remove_in_progress == 0)) {
		/* TO DO (?) */
	}
	else
		cmd->abort_aen = 0;

	sc->aen_cmd = NULL;
	mrsas_release_mfi_cmd(cmd);

	/* if (!sc->remove_in_progress) */
	/* 	taskqueue_enqueue(sc->ev_tq, &sc->ev_task); */

	return;
}

/**
 * mrsas_wakeup -         Completes an internal command
 * input:                 Adapter soft state
 *                        Command to be completed
 *
 * In mrsas_issue_blocked_cmd(), after a command is issued to Firmware, 
 * a wait timer is started.  This function is called from  
 * mrsas_complete_mptmfi_passthru() as it completes the command,
 * to wake up from the command wait.
 */
static void mrsas_wakeup(struct mrsas_mfi_cmd *cmd)
{
    cmd->cmd_status = cmd->frame->io.cmd_status;

    if (cmd->cmd_status == ECONNREFUSED) 
        cmd->cmd_status = 0;

    /* For debug only ... */ 
    //device_printf(sc->mrsas_dev,"DCMD rec'd for wakeup, sc->chan=%p\n", sc->chan);

    sc->chan = (void*)&cmd;
    /* wakeup_one((void *)&sc->chan); */
    return;
}

/**
 * mrsas_complete_mptmfi_passthru - Completes a command
 * input:                           sc: Adapter soft state
 *                                  cmd: Command to be completed
 *                                  status: cmd completion status 
 *
 * This function is called from mrsas_complete_cmd() after an interrupt 
 * is received from Firmware, and io_request->Function is 
 * MRSAS_MPI2_FUNCTION_PASSTHRU_IO_REQUEST.
 */
static void
mrsas_complete_mptmfi_passthru(struct mrsas_mfi_cmd *cmd,
			       u_int8_t status)
{
    struct mrsas_header *hdr = &cmd->frame->hdr;
    u_int8_t cmd_status = cmd->frame->hdr.cmd_status;

    /* Reset the retry counter for future re-tries */ 
    cmd->retry_for_fw_reset = 0;

    if (cmd->ccb_ptr)
        cmd->ccb_ptr = NULL;

    switch (hdr->cmd) {
        case MFI_CMD_INVALID:
	  DEBUG("MFI_CMD_INVALID command.\n");
            break;
        case MFI_CMD_PD_SCSI_IO:
        case MFI_CMD_LD_SCSI_IO:
            /*
             * MFI_CMD_PD_SCSI_IO and MFI_CMD_LD_SCSI_IO could have been
             * issued either through an IO path or an IOCTL path. If it
             * was via IOCTL, we will send it to internal completion.
             */
            if (cmd->sync_cmd) {
                cmd->sync_cmd = 0;
                /* mrsas_wakeup(sc, cmd); */
		assert(!"NYI");
                break;
            }
        case MFI_CMD_SMP:
        case MFI_CMD_STP:
        case MFI_CMD_DCMD:
            /* Check for LD map update */
            if ((cmd->frame->dcmd.opcode == MR_DCMD_LD_MAP_GET_INFO) && 
                (cmd->frame->dcmd.mbox.b[1] == 1)) {
                sc->fast_path_io = 0;
		/* mtx_lock(&sc->raidmap_lock); */
                if (cmd_status != 0) {
                    if (cmd_status != MFI_STAT_NOT_FOUND)
		      DEBUG("map sync failed, status=%x\n",cmd_status);
                    else {
                        mrsas_release_mfi_cmd(cmd);
		        /* mtx_unlock(&sc->raidmap_lock); */
                        break;
                    }
                } 
                else 
                    sc->map_id++;
                mrsas_release_mfi_cmd(cmd);
                if (MR_ValidateMapInfo())
                    sc->fast_path_io = 0;
                else
                    sc->fast_path_io = 1;
                mrsas_sync_map_info();
                /* mtx_unlock(&sc->raidmap_lock); */
                break;
            }
            /* See if got an event notification */
            if (cmd->frame->dcmd.opcode == MR_DCMD_CTRL_EVENT_WAIT)
                mrsas_complete_aen(cmd);
            else
                mrsas_wakeup(cmd);
            break;
        case MFI_CMD_ABORT:
            /* Command issued to abort another cmd return */
            mrsas_complete_abort(cmd);
            break;
        default:
	  DEBUG("Unknown command completed! [0x%X]\n", hdr->cmd);
            break;
    }
}

bool poll_mode = false;

/*
 * mrsas_complete_cmd:        Process reply request  
 * input:                     Adapter instance soft state
 *
 * This function is called from mrsas_isr() to process reply request and 
 * clear response interrupt. Processing of the reply request entails
 * walking through the reply descriptor array for the command request  
 * pended from Firmware.  We look at the Function field to determine
 * the command type and perform the appropriate action.  Before we
 * return, we clear the response interrupt.
 */
int mrsas_complete_cmd(void)
{
    Mpi2ReplyDescriptorsUnion_t *desc;
    MPI2_SCSI_IO_SUCCESS_REPLY_DESCRIPTOR *reply_desc;
    MRSAS_RAID_SCSI_IO_REQUEST  *scsi_io_req;
    struct mrsas_mpt_cmd *cmd_mpt;
    struct mrsas_mfi_cmd *cmd_mfi;
    u_int8_t arm, reply_descript_type;
    u_int16_t smid, num_completed;
    u_int8_t status, extStatus;
    union desc_value desc_val;
    PLD_LOAD_BALANCE_INFO lbinfo;
    u_int32_t device_id;
    int threshold_reply_count = 0;

    desc = sc->reply_desc_mem;
    desc += sc->last_reply_idx;

    reply_desc = (MPI2_SCSI_IO_SUCCESS_REPLY_DESCRIPTOR *)desc;

    desc_val.word = desc->Words;
    num_completed = 0;

    reply_descript_type = reply_desc->ReplyFlags & MPI2_RPY_DESCRIPT_FLAGS_TYPE_MASK;

    /* Find our reply descriptor for the command and process */
    while((desc_val.u.low != 0xFFFFFFFF) && (desc_val.u.high != 0xFFFFFFFF)) 
    {
        smid = reply_desc->SMID;
        cmd_mpt = sc->mpt_cmd_list[smid -1];
        scsi_io_req = (MRSAS_RAID_SCSI_IO_REQUEST *)cmd_mpt->io_request;

        status = scsi_io_req->RaidContext.status;
        extStatus = scsi_io_req->RaidContext.exStatus;
       
        switch (scsi_io_req->Function)
        {
            case MPI2_FUNCTION_SCSI_IO_REQUEST :  /*Fast Path IO.*/
	      device_id = TARGET_DEVICE_ID;
                lbinfo = &sc->load_balance_info[device_id];
                if (cmd_mpt->load_balance == MRSAS_LOAD_BALANCE_FLAG) {
                    arm = lbinfo->raid1DevHandle[0] == scsi_io_req->DevHandle ? 0 : 1;
                    /* atomic_dec(&lbinfo->scsi_pending_cmds[arm]); */
		    lbinfo->scsi_pending_cmds[arm].val--;
                    cmd_mpt->load_balance &= ~MRSAS_LOAD_BALANCE_FLAG;
                }
                //Fall thru and complete IO
            case MRSAS_MPI2_FUNCTION_LD_IO_REQUEST:
                /* mrsas_map_mpt_cmd_status(cmd_mpt, status, extStatus); */
	      if(status != MFI_STAT_OK) {
		DEBUG("Command SMID %u failed with status 0x%x, extStatus 0x%x\n",
		      smid, status, extStatus);
	      }
	      assert(status == MFI_STAT_OK);
	      cmd_mpt->ccb_ptr = NULL;
	      mrsas_release_mpt_cmd(cmd_mpt);
                /* mrsas_cmd_done(sc, cmd_mpt); */
                scsi_io_req->RaidContext.status = 0;
                scsi_io_req->RaidContext.exStatus = 0;
                /* atomic_dec(&sc->fw_outstanding); */
		sc->fw_outstanding.val--;
                break;
            case MRSAS_MPI2_FUNCTION_PASSTHRU_IO_REQUEST: /*MFI command */
	      DEBUG("sync_cmd_idx = %x\n", cmd_mpt->sync_cmd_idx);
                cmd_mfi = sc->mfi_cmd_list[cmd_mpt->sync_cmd_idx];
                mrsas_complete_mptmfi_passthru(cmd_mfi, status);
                cmd_mpt->flags = 0;
                mrsas_release_mpt_cmd(cmd_mpt);
                break;
        }

        sc->last_reply_idx++;
        if (sc->last_reply_idx >= sc->reply_q_depth)
            sc->last_reply_idx = 0;

        desc->Words = ~((uint64_t)0x00); /* set it back to all 0xFFFFFFFFs */
        num_completed++;
        threshold_reply_count++;

        /* Get the next reply descriptor */
        if (!sc->last_reply_idx)
            desc = sc->reply_desc_mem;
	else
            desc++;

        reply_desc = (MPI2_SCSI_IO_SUCCESS_REPLY_DESCRIPTOR *)desc;
        desc_val.word = desc->Words;

        reply_descript_type = reply_desc->ReplyFlags & MPI2_RPY_DESCRIPT_FLAGS_TYPE_MASK;

        if(reply_descript_type == MPI2_RPY_DESCRIPT_FLAGS_UNUSED)
            break;

        /* 
         * Write to reply post index after completing threshold reply count 
         * and still there are more replies in reply queue pending to be 
         * completed.
         */
        if (threshold_reply_count >= THRESHOLD_REPLY_COUNT) {
	  megaraid_reply_post_host_index_wr(&sc->d, sc->last_reply_idx);
	  threshold_reply_count = 0;
        }

        if(poll_mode) {
            break;
        }
    }

    /* No match, just return */
    if (num_completed == 0)
        return (DONE);

    /* Clear response interrupt */
    megaraid_reply_post_host_index_wr(&sc->d, sc->last_reply_idx);

    return(0);
}
