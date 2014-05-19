/*
 * Copyright (c) 2007, 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __CPIOBIN_H__
#define __CPIOBIN_H__

#include <stdint.h>
#include <sys/cdefs.h>

__BEGIN_DECLS

typedef enum
{
    CPIO_MODE_FILE_TYPE_MASK = 0170000,
    CPIO_MODE_SOCKET         = 0140000,
    CPIO_MODE_SYMLINK        = 0120000,
    CPIO_MODE_FILE           = 0100000,
    CPIO_MODE_BLOCK_DEVICE   = 0060000,
    CPIO_MODE_DIRECTORY      = 0040000,
    CPIO_MODE_CHAR_DEVICE    = 0020000,
    CPIO_MODE_FIFO           = 0010000,
    CPIO_MODE_SUID           = 0004000,
    CPIO_MODE_SGID           = 0002000,
    CPIO_MODE_STICKY         = 0001000,
    CPIO_MODE_UMASK          = 0000777
} cpio_mode_bits_t;

typedef struct
{
    cpio_mode_bits_t mode;
    const char*      name;
    const uint8_t*   data;
    size_t           datasize;
    uint32_t         checksum;
} cpio_generic_header_t;

/**
 * CPIO visitor function.
 *
 * This is invoked by cpio_visit.
 *
 * returns zero to continue visit after current invocation, non-zero to stop.
 */
typedef int (*cpio_visitor_t)(int                          ordinal,
                              const cpio_generic_header_t* header,
                              void*                        arg);

/**
 * Find file with specified in CPIO binary image.
 *
 * @param cpio_base     base of CPIO memory image.
 * @param cpio_bytes    size of CPIO memory image in bytes.
 * @param name          file name to locate.
 * @param file_base     pointer to receive file start address.
 * @param file_bytes    pointer to receive file size in bytes.
 *
 * @return non-zero if file is found.
 */
int cpio_get_file_by_name(
    const uint8_t*  cpio_base,
    size_t          cpio_bytes,
    const char*     name,
    const uint8_t** file_base,
    size_t*         file_bytes
);

/**
 * Find file with specified in CPIO binary image.
 *
 * @param cpio_base     base of CPIO memory image.
 * @param cpio_bytes    size of CPIO memory image in bytes.
 * @param ordinal       ordinal'th file to find.
 * @param file_name     pointer to receive file name.
 * @param file_base     pointer to receive file start address.
 * @param file_bytes    pointer to receive file size in bytes.
 *
 * @return non-zero if file is found.
 */
int cpio_get_file_by_ordinal(
    const uint8_t*  cpio_base,
    size_t          cpio_bytes,
    uint32_t        ordinal,
    const char**    file_name,
    const uint8_t** file_base,
    size_t*         file_bytes
);

/**
 * Determine size of CPIO bin format image.
 *
 * @param cpio_base     base of CPIO memory image.
 * @param cpio_bytes    size of CPIO memory image in bytes.
 *
 * @return number of bytes in image, zero if image is invalid.
 */
size_t cpio_archive_bytes(
    const uint8_t*  cpio_base,
    size_t          cpio_bytes
);

/**
 * Check validity of CPIO bin format image.
 *
 * @param cpio_base     base of CPIO memory image.
 * @param cpio_bytes    size of CPIO memory image in bytes.
 *
 * @return non-zero if image is valid.
 */
int
cpio_archive_valid(
    const uint8_t*  cpio_base,
    size_t          cpio_bytes
);

/**
 * Apply visitor function to each header in CPIO memory image.
 *
 * @param cpio_base     base of CPIO memory image.
 * @param cpio_bytes    size of CPIO memory image in bytes.
 * @param cpio_visit_fn visitor function.
 * @param arg           user supplied argument to visitor function.
 *
 * @return              number of CPIO headers visited.
 */
int
cpio_visit(
    const uint8_t*         cpio_base,
    size_t                 cpio_bytes,
    cpio_visitor_t         cpio_visit_fn,
    cpio_generic_header_t* g,
    void*                  arg
);

__END_DECLS

#endif // __CPIOBIN_H__
