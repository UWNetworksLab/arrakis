/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TENACIOUSD_LOG_H
#define TENACIOUSD_LOG_H

struct storage_vsa;
struct storage_vsic;

#define TENACIOUSD_LOG_MIN_ENTRY_SIZE(log)      \
    (STORAGE_VSIC_ROUND(log->vsic, sizeof(struct tenaciousd_log_entry)) - sizeof(struct tenaciousd_log_entry))

// sizeof(struct tenaciousd_log_entry) = on-disk size of header + footer
struct tenaciousd_log_entry {
  uint64_t	size;
  uint64_t	next;		// Only valid on disk
  uint8_t	data[0];
  uint8_t	marker;		// Never valid
} __attribute__ ((packed));

struct tenaciousd_log_iter {
  struct tenaciousd_log_entry	*entry;
  struct tenaciousd_log_iter	*next;
  uint64_t			offset;
};

struct tenaciousd_log {
    struct storage_vsa *vsa;
    struct storage_vsic *vsic;
    uint64_t entries;
    uint64_t end;
};

struct tenaciousd_log *tenaciousd_log_new(struct storage_vsa *vsa,
					  struct storage_vsic *vsic);

errval_t tenaciousd_log_delete(struct tenaciousd_log *log);

errval_t tenaciousd_log_append(struct tenaciousd_log *log,
                               struct tenaciousd_log_entry *entry);

errval_t tenaciousd_log_trim(struct tenaciousd_log *log, int nentries);

struct tenaciousd_log_iter tenaciousd_log_begin(struct tenaciousd_log *log);

struct tenaciousd_log_iter tenaciousd_log_next(struct tenaciousd_log *log,
					       struct tenaciousd_log_iter iter);

struct tenaciousd_log_entry *
tenaciousd_log_entry_new(struct tenaciousd_log *log,
                         size_t *size);

struct tenaciousd_log_entry *
tenaciousd_log_entry_resize(struct tenaciousd_log *log,
                            struct tenaciousd_log_entry *entry,
                            size_t *newsize);

void tenaciousd_log_entry_delete(struct tenaciousd_log *log,
				 struct tenaciousd_log_entry *entry);

static inline bool tenaciousd_log_end(struct tenaciousd_log_iter iter)
{
  return iter.entry == NULL ? true : false;
}

static inline void *tenaciousd_log_iter_data(struct tenaciousd_log_iter iter)
{
    return (void *)iter.entry->data;
}

#endif
