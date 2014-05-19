/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <alloca.h>
#include <stdint.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <string.h>
#include <errors/errno.h>
#include <tenaciousd/log.h>
#include <storage/storage.h>

#define LOG_IDENTIFIER	"TenaciousD_Log_structure_rev01"

#define LOG_FIRST_ENTRY_OFFSET(log) \
  STORAGE_VSIC_ROUND(log->vsic, sizeof(struct log_header))

#define LOG_MIN_ENTRY_SIZE(log) \
  STORAGE_VSIC_ROUND(log->vsic, sizeof(struct tenaciousd_log_entry))

#define LOG_ENTRY_END_MARKER	0xff

struct log_header {
  char		identifier[32];
  uint8_t	version;
  uint64_t	entries;
  uint32_t	blocksize;
  uint64_t	end;
} __attribute__ ((packed));

static struct tenaciousd_log_entry *read_entry(struct tenaciousd_log *log,
					       off_t offset)
{
    // Allocate minimum sized log entry
    struct tenaciousd_log_entry *entry =
        storage_malloc(log->vsic, sizeof(struct tenaciousd_log_entry));
    assert(entry != NULL);

    // Read minimum sized entry
    errval_t err = log->vsic->ops.read(log->vsic, log->vsa, offset,
				       LOG_MIN_ENTRY_SIZE(log), entry);
    assert(err_is_ok(err));
    err = log->vsic->ops.wait(log->vsic);
    assert(err_is_ok(err));

    if(err_no(err) == VFS_ERR_EOF) {
        // Invalid read
        storage_free(log, entry);
        return NULL;
    }

    // If this was less or equal to the min size, we're done
    if(entry->size + sizeof(struct tenaciousd_log_entry)
       <= LOG_MIN_ENTRY_SIZE(log)) {
      return entry;
    }

    // Otherwise, we have to read the rest
    entry = storage_realloc(log->vsic, entry,
                            entry->size + sizeof(struct tenaciousd_log_entry));
    assert(entry != NULL);
    err = log->vsic->ops.read(log->vsic, log->vsa,
                              offset + LOG_MIN_ENTRY_SIZE(log),
                              entry->size - LOG_MIN_ENTRY_SIZE(log) + sizeof(struct tenaciousd_log_entry),
       ((uint8_t *)entry) + LOG_MIN_ENTRY_SIZE(log));
    assert(err_is_ok(err));
    err = log->vsic->ops.wait(log->vsic);
    assert(err_is_ok(err));

    // Is the end marker there?
    if(entry->data[entry->size] != LOG_ENTRY_END_MARKER) {
        // This entry is invalid
        storage_free(log, entry);
        return NULL;
    }

    return entry;
}

struct tenaciousd_log *tenaciousd_log_new(struct storage_vsa *vsa,
					  struct storage_vsic *vsic)
{
  assert(vsa != NULL);
  assert(vsic != NULL);

  struct tenaciousd_log *log = malloc(sizeof(struct tenaciousd_log));
  assert(log != NULL);
  memset(log, 0, sizeof(log));

  log->vsa = vsa;
  log->vsic = vsic;

  // Check if VSA already has a log
  struct log_header *header = storage_alloca(vsic, sizeof(struct log_header));
  assert(header != NULL);
  errval_t err = vsic->ops.read(vsic, vsa, 0,
				sizeof(struct log_header), header);
  assert(err_is_ok(err));
  err = vsic->ops.wait(vsic);
  assert(err_is_ok(err));

  if(!strncmp(header->identifier, LOG_IDENTIFIER, sizeof(header->identifier))) {
    // Log already exists -- initialize from storage
    assert(header->blocksize == vsic->blocksize);
    log->entries = header->entries;
    log->end = header->end;

    // Check whether header is up-to-date
    for(;;) {
        struct tenaciousd_log_entry *logentry = read_entry(log, log->end);

        // Either we couldn't read (cause EOF) or there's no entry
        if(logentry == NULL || logentry->size == 0) {
            // Header up-to-date -- we're done
            return log;
        }

        // More entries -- log header not up-to-date
        log->entries++;
        log->end += STORAGE_VSIC_ROUND(log->vsic, logentry->size);
        tenaciousd_log_entry_delete(log, logentry);
    }
  } else {
    // Reset header
    memset(header, 0, sizeof(struct log_header));
    memcpy(header->identifier, LOG_IDENTIFIER, 32);
    header->version = 0;
    header->blocksize = vsic->blocksize;
    header->end = STORAGE_VSIC_ROUND(vsic, sizeof(struct log_header));
  }

  // Write new header
  err = vsic->ops.write(vsic, vsa, 0, sizeof(struct log_header), header);
  assert(err_is_ok(err));
  err = vsic->ops.flush(vsic, vsa);
  assert(err_is_ok(err));
  err = vsic->ops.wait(vsic);
  assert(err_is_ok(err));

  log->entries = header->entries;
  log->end = header->end;

  return log;
}

errval_t tenaciousd_log_delete(struct tenaciousd_log *log)
{
  // Flush out log
  errval_t err = log->vsic->ops.flush(log->vsic, log->vsa);
  assert(err_is_ok(err));

  // Update header and flush again and wait for it to finish
  struct log_header *header =
      storage_alloca(log->vsic, sizeof(struct log_header));
  assert(header != NULL);

  memset(header, 0, sizeof(struct log_header));
  memcpy(header->identifier, LOG_IDENTIFIER, 32);
  header->version = 0;
  header->entries = log->entries;
  header->blocksize = log->vsic->blocksize;
  header->end = log->end;

  err = log->vsic->ops.write(log->vsic, log->vsa, 0, sizeof(struct log_header), header);
  assert(err_is_ok(err));
  err = log->vsic->ops.flush(log->vsic, log->vsa);
  assert(err_is_ok(err));
  err = log->vsic->ops.wait(log->vsic);
  assert(err_is_ok(err));

  // Free memory and return
  free(log);
  return SYS_ERR_OK;
}

errval_t tenaciousd_log_append(struct tenaciousd_log *log,
                               struct tenaciousd_log_entry *entry)
{
  struct storage_vsic *vsic = log->vsic;
  off_t end = log->end;

  log->end = entry->next = end +
    STORAGE_VSIC_ROUND(vsic, entry->size + sizeof(struct tenaciousd_log_entry));
  log->entries++;
  entry->data[entry->size] = LOG_ENTRY_END_MARKER;

  return log->vsic->ops.
    write(vsic, log->vsa, end,
	  entry->size + sizeof(struct tenaciousd_log_entry), entry);
}

errval_t tenaciousd_log_trim(struct tenaciousd_log *log, int nentries)
{
    assert(!"NYI");
}

struct tenaciousd_log_iter tenaciousd_log_begin(struct tenaciousd_log *log)
{
  struct tenaciousd_log_entry *entry =
      read_entry(log, LOG_FIRST_ENTRY_OFFSET(log));

  if(entry == NULL) {
      log->entries = 0;
  }

  // TODO: Could do some prefetching here...

  return (struct tenaciousd_log_iter) {
      .entry = entry,
      .next = NULL,
      .offset = LOG_FIRST_ENTRY_OFFSET(log),
  };
}

struct tenaciousd_log_iter tenaciousd_log_next(struct tenaciousd_log *log,
					       struct tenaciousd_log_iter iter)
{
  if(iter.next != NULL) {
      // Cached
      iter = *iter.next;
  } else {
      // Not cached
      iter.offset += STORAGE_VSIC_ROUND(log->vsic, iter.entry->size);
      iter.entry = read_entry(log, iter.offset);
  }

  // TODO: Prefetch another one

  return iter;
}

struct tenaciousd_log_entry *
tenaciousd_log_entry_new(struct tenaciousd_log *log, size_t *size)
{
    assert(*size > 0);
    *size += sizeof(struct tenaciousd_log_entry);
    struct tenaciousd_log_entry *entry = storage_malloc(log->vsic, *size);
    assert(entry != NULL);

    *size = STORAGE_VSIC_ROUND(log->vsic, *size)
        - sizeof(struct tenaciousd_log_entry);
    entry->size = *size;
    return entry;
}

struct tenaciousd_log_entry *
tenaciousd_log_entry_resize(struct tenaciousd_log *log,
                            struct tenaciousd_log_entry *entry,
                            size_t *newsize)
{
    assert(*newsize > 0); // XXX: free on zero size?
    *newsize += sizeof(struct tenaciousd_log_entry);
    entry = storage_realloc(log->vsic, entry, *newsize);
    assert(entry != NULL);

    *newsize = STORAGE_VSIC_ROUND(log->vsic, *newsize)
        - sizeof(struct tenaciousd_log_entry);
    entry->size = *newsize;
    return entry;
}

void tenaciousd_log_entry_delete(struct tenaciousd_log *log,
				 struct tenaciousd_log_entry *entry)
{
  storage_free(log->vsic, entry);
}
