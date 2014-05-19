/**
 * \file
 * \brief CPIO archive reader routines for newc/crc/bin variants.
 */

/*
 * Copyright (c) 2007, 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * Thi2s file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <barrelfish/static_assert.h>
#include "cpiobin.h"

#ifndef MIN
#define MIN(a,b) ((a) < (b)) ? (a) : (b)
#endif // MIN

typedef struct
{
    uint8_t magic[2];
    uint8_t dev[2];
    uint8_t inode[2];
    uint8_t mode[2];
    uint8_t uid[2];
    uint8_t gid[2];
    uint8_t nlink[2];
    uint8_t rdev[2];
    uint8_t mtime[4];
    uint8_t namesize[2];
    uint8_t filesize[4];
} __attribute__((packed)) cpio_bin_header_t;

STATIC_ASSERT_SIZEOF(cpio_bin_header_t, 26);

typedef struct
{
    char magic[6];
    char inode[8];
    char mode[8];
    char uid[8];
    char gid[8];
    char nlink[8];
    char mtime[8];
    char filesize[8];
    char devmajor[8];
    char devminor[8];
    char rdevmajor[8];
    char rdevminor[8];
    char namesize[8];
    char check[8];
} __attribute__((packed)) cpio_newc_header_t;

STATIC_ASSERT_SIZEOF(cpio_newc_header_t, 110);

static const char CPIO_LAST[] = "TRAILER!!!";
const size_t CPIO_BIN_LAST_SIZE = sizeof(cpio_bin_header_t) + sizeof(CPIO_LAST);
const size_t CPIO_NEWC_LAST_SIZE = sizeof(cpio_newc_header_t) + sizeof(CPIO_LAST);

// ----------------------------------------------------------------------------
// Identification

static inline int
cpio_is_bin_be(const uint8_t* buffer)
{
    const cpio_bin_header_t* h = (const cpio_bin_header_t*)buffer;
    return (h->magic[0] * 0x100 + h->magic[1]) == 070707;
}

static inline int
cpio_is_bin_le(const uint8_t* buffer)
{
    const cpio_bin_header_t* h = (const cpio_bin_header_t*)buffer;
    return (h->magic[0] + h->magic[1] * 0x100) == 070707;
}

static inline int
cpio_is_newc(const uint8_t* buffer)
{
    const cpio_newc_header_t* h = (const cpio_newc_header_t*)buffer;
    return strncmp(h->magic, "070701", 6) == 0;
}

static inline int
cpio_is_crc(const uint8_t* buffer)
{
    const cpio_newc_header_t* h = (const cpio_newc_header_t*)buffer;
    return strncmp(h->magic, "070702", 6) == 0;
}

static inline int
cpio_mode_is_file(cpio_mode_bits_t m)
{
    return CPIO_MODE_FILE == (m & CPIO_MODE_FILE_TYPE_MASK);
}

// ----------------------------------------------------------------------------
// Decoding

static inline
int toupper(int c)
{
    if (c >= 'a' && c <= 'z')
    {
        c = c - 'a' + 'A';
    }
    return c;
}

static inline
int isxdigit(int c)
{
    return ((c >= '0' && c <= '9') |
            (c >= 'A' && c <= 'F') |
            (c >= 'a' && c <= 'f'));
}

static inline uint16_t
c2u16(const cpio_bin_header_t* h, const uint8_t* p)
{
    if (cpio_is_bin_be((const uint8_t*)h))
    {
        return (p[0] * 0x100) + p[1];
    }
    else
    {
        return (p[1] * 0x100) + p[0];
    }
}

static inline uint32_t
c4u32(const cpio_bin_header_t* h, const uint8_t* p)
{
    if (cpio_is_bin_be((const uint8_t*)h))
    {
        return (p[0] * 0x1000000) + (p[1] * 0x10000) + (p[2] * 0x100) + p[3];
    }
    else
    {
        return (p[3] * 0x100 + p[2]) + (p[1] * 0x100 + p[0]) * 0x10000;
    }
}

static inline uint32_t
a1u8(char c)
{
    if (c == 0)
    {
        return 0;
    }
    else {
        c = toupper(c);
        if (c >= 'A' && c <= 'F')
        {
            return ((uint32_t)(c + 10 - 'A')) & 0xffu;
        }
        else if (c >= '0' || c <= '9')
        {
            return ((uint32_t)(c - '0')) & 0xffu;
        }
        else
        {
            return ~0;
        }
    }
}

static inline uint32_t
a6u32(const char* c)
{
    return ((a1u8(c[0]) << 20) | (a1u8(c[1]) << 16) | (a1u8(c[2]) << 12) |
            (a1u8(c[3]) <<  8) | (a1u8(c[4]) <<  4) | (a1u8(c[5])));
}

static inline uint32_t
a8u32(const char* c)
{
    return ((a1u8(c[0]) << 28) | (a1u8(c[1]) << 24) | (a1u8(c[2]) << 20) |
            (a1u8(c[3]) << 16) | (a1u8(c[4]) << 12) | (a1u8(c[5]) <<  8) |
            (a1u8(c[6]) <<  4) | (a1u8(c[7])));
}

// ----------------------------------------------------------------------------
// Legacy Format (bin) accessors

static inline uintptr_t
cpio_bin_align(uintptr_t s)
{
    return s + (s & 1);
}

static inline uint16_t
cpio_bin_name_bytes(const cpio_bin_header_t* h)
{
    return c2u16(h, h->namesize);
}

static inline const uint8_t*
cpio_bin_data_start(const cpio_bin_header_t* h)
{
    return cpio_bin_align(cpio_bin_name_bytes(h)) + ((const uint8_t*)(h + 1));
}

static inline uint32_t
cpio_bin_data_bytes(const cpio_bin_header_t* h)
{
    return c4u32(h, h->filesize);
}

static inline cpio_mode_bits_t
cpio_bin_mode(const cpio_bin_header_t* h)
{
    return (cpio_mode_bits_t)c2u16(h, h->mode);
}

static inline const char*
cpio_bin_name(const cpio_bin_header_t* h)
{
    return (const char*)(h + 1);
}

static inline int
cpio_valid_bin_header(const uint8_t* h)
{
    return cpio_is_bin_be(h) || cpio_is_bin_le(h);
}

// ----------------------------------------------------------------------------
// newc / crc accessors

static inline uintptr_t
cpio_newc_align(uintptr_t s)
{
    return (s + 3) & ~3;
}

static inline uint32_t
cpio_newc_name_bytes(const cpio_newc_header_t* h)
{
    return a8u32(h->namesize);
}

static inline const uint8_t*
cpio_newc_data_start(const cpio_newc_header_t* h)
{
    uintptr_t h_end = (uintptr_t)(h + 1);
    return (const uint8_t*)cpio_newc_align(cpio_newc_name_bytes(h) + h_end);
}

static inline uint32_t
cpio_newc_data_bytes(const cpio_newc_header_t* h)
{
    return a8u32(h->filesize);
}

static inline cpio_mode_bits_t
cpio_newc_mode(const cpio_newc_header_t* h)
{
    return (cpio_mode_bits_t)a8u32(h->mode);
}

static inline const char*
cpio_newc_name(const cpio_newc_header_t* h)
{
    return (const char*)(h + 1);
}

static inline uint32_t
cpio_newc_checksum(const cpio_newc_header_t* h)
{
    return a8u32(h->check);
}

static int
is_newc_string(const uint8_t* field, size_t bytes)
{
    const char* s = (const char*)field;
    size_t i = 0;

    while (i < bytes)
    {
        if (!isxdigit(s[i]))
        {
            return 0;
        }
        i++;
    }
    return 1;
}

static inline int
cpio_valid_newc_header(const uint8_t* h)
{
    return ((cpio_is_newc(h) || cpio_is_crc(h)) &&
            is_newc_string(h+ 6, 8) && is_newc_string(h+14, 8) &&
            is_newc_string(h+22, 8) && is_newc_string(h+30, 8) &&
            is_newc_string(h+38, 8) && is_newc_string(h+46, 8) &&
            is_newc_string(h+54, 8) && is_newc_string(h+62, 8) &&
            is_newc_string(h+70, 8) && is_newc_string(h+78, 8) &&
            is_newc_string(h+86, 8) && is_newc_string(h+94, 8) &&
            (is_newc_string(h+102, 8)));
}

static int
cpio_bin_visit(
    const uint8_t*         cpio_base,
    size_t                 cpio_bytes,
    cpio_visitor_t         cv,
    cpio_generic_header_t* g,
    void*                  arg
    )
{
    const uint8_t* cpio_limit = cpio_base + cpio_bytes;
    int visited = 0;

    while (cpio_limit - cpio_base >= CPIO_BIN_LAST_SIZE &&
           cpio_valid_bin_header(cpio_base))
    {
        const cpio_bin_header_t* h = (const cpio_bin_header_t*)cpio_base;

        g->mode     = cpio_bin_mode(h);
        g->name     = cpio_bin_name(h);
        g->data     = cpio_bin_data_start(h);
        g->datasize = cpio_bin_data_bytes(h);
        g->checksum = 0;

        if (((g->data + g->datasize) > cpio_limit) ||
            (cv(visited, g, arg) && (g->mode != 0)))
        {
            break;
        }
        cpio_base = (cpio_bin_data_start(h) + cpio_bin_align(cpio_bin_data_bytes(h)));
        visited++;
    }
    return visited;
}

static int
cpio_newc_visit(
    const uint8_t*         cpio_base,
    size_t                 cpio_bytes,
    cpio_visitor_t         cv,
    cpio_generic_header_t* g,
    void*                  arg
    )
{
    const uint8_t* cpio_limit = cpio_base + cpio_bytes;
    int visited = 0;

    while (cpio_limit - cpio_base >= CPIO_NEWC_LAST_SIZE &&
           cpio_valid_newc_header(cpio_base))
    {
        const cpio_newc_header_t* h = (const cpio_newc_header_t*)cpio_base;

        g->mode     = cpio_newc_mode(h);
        g->name     = cpio_newc_name(h);
        g->data     = cpio_newc_data_start(h);
        g->datasize = cpio_newc_data_bytes(h);
        g->checksum = cpio_newc_checksum(h);

        if (((g->data + g->datasize) > cpio_limit) ||
            (cv(visited, g, arg) && (g->mode != 0)))
        {
            break;
        }
        cpio_base = (cpio_newc_data_start(h) + cpio_newc_align(cpio_newc_data_bytes(h)));
        visited++;
    }
    return visited;
}

int
cpio_visit(
    const uint8_t*         cpio_base,
    size_t                 cpio_bytes,
    cpio_visitor_t         cpio_visit_fn,
    cpio_generic_header_t* g,
    void*                  arg
)
{
    int visited = 0;
    if (cpio_bytes >= CPIO_BIN_LAST_SIZE &&
        cpio_valid_bin_header(cpio_base))
    {
        visited = cpio_bin_visit(cpio_base, cpio_bytes, cpio_visit_fn, g, arg);
    }
    else if (cpio_bytes >= CPIO_NEWC_LAST_SIZE &&
             cpio_valid_newc_header(cpio_base))
    {
        visited = cpio_newc_visit(cpio_base, cpio_bytes, cpio_visit_fn, g, arg);
    }
    return visited;
}

struct cpio_find_info
{
    const char* search_name;
    int ordinal;

    const cpio_generic_header_t* header; // result
};

static int
cpio_match_name(int ordinal, const cpio_generic_header_t* header, void* arg)
{
    struct cpio_find_info* cfi = (struct cpio_find_info*)arg;
    int match = !strcmp(cfi->search_name, header->name);
    if (match)
    {
        cfi->ordinal = ordinal;
        cfi->header  = header;
    }
    return match;
}

int
cpio_get_file_by_name(
    const uint8_t*  cpio_base,
    size_t          cpio_bytes,
    const char*     name,
    const uint8_t** file_base,
    size_t*         file_bytes
    )
{
    cpio_generic_header_t h;

    struct cpio_find_info cfi = { name, 0, NULL };
    cpio_visit(cpio_base, cpio_bytes, cpio_match_name, &h, &cfi);

    int match = (cfi.header == &h) && cpio_mode_is_file(h.mode);
    if (match)
    {
        if (file_base != NULL)
        {
            *file_base = h.data;
        }
        if (file_bytes != NULL)
        {
            *file_bytes = h.datasize;
        }
    }
    return match;
}

static int
cpio_match_file_ordinal(
    int                          ordinal,
    const cpio_generic_header_t* header,
    void*                        arg
    )
{
    int match = 0;
    if (cpio_mode_is_file(header->mode))
    {
        struct cpio_find_info* cfi = (struct cpio_find_info*)arg;

        if (cfi->ordinal == 0)
        {
            cfi->header = header;
            match = 1;
        }
        else
        {
            cfi->ordinal--;
        }
    }
    return match;
}

int
cpio_get_file_by_ordinal(
    const uint8_t*  cpio_base,
    size_t          cpio_bytes,
    uint32_t        ordinal,
    const char**    file_name,
    const uint8_t** file_base,
    size_t*         file_bytes
    )
{
    cpio_generic_header_t h;
    struct cpio_find_info cfi = { NULL, ordinal, NULL };

    cpio_visit(cpio_base, cpio_bytes, cpio_match_file_ordinal, &h, &cfi);
    int match = (cfi.header == &h);
    if (match)
    {
        if (NULL != file_name)
        {
            *file_name = cfi.header->name;
        }
        if (NULL != file_base)
        {
            *file_base = cfi.header->data;
        }
        if (NULL != file_bytes)
        {
            *file_bytes = cfi.header->datasize;
        }
    }
    return match;
}

size_t
cpio_archive_bytes(
    const uint8_t* cpio_base,
    size_t         cpio_bytes
    )
{
    // Search for trailer
    cpio_generic_header_t g;
    struct cpio_find_info cfi = { CPIO_LAST, 0, NULL };

    cpio_visit(cpio_base, cpio_bytes, cpio_match_name, &g, &cfi);
    if (cfi.header != NULL) {
        const uint8_t* cpio_limit = cfi.header->data - 1;
        // Limit may be too far because of CPIO alignment rounding up
        // of the data start region so...
        return MIN((size_t)(cpio_limit - cpio_base), cpio_bytes);
    }
    else
    {
        return 0;
    }
}

static int
cpio_crc_visitor(int ordinal, const cpio_generic_header_t* header, void *arg)
{
    int* found_trailer = (int*)arg;

    if (header->datasize > 0)
    {
        // "CRC" is the unsigned 32-bit sum of file data.
        uint32_t computed = 0;
        for (size_t i = 0; i < header->datasize; i++)
        {
            computed += header->data[i];
        }
        // Stop visiting if mismatch.
        return computed != header->checksum;
    }
    else if (header->name)
    {
        // Success is reaching the trailer
        *found_trailer = !strcmp(header->name, CPIO_LAST);
    }
    return 0;
}

int
cpio_archive_valid(const uint8_t* cpio_base, size_t cpio_bytes)
{
    if ((cpio_bytes > sizeof(cpio_newc_header_t) &&
         cpio_is_crc(cpio_base)))
    {
        int found_trailer = 0;  // Success is reaching the trailer
        cpio_generic_header_t g;
        cpio_newc_visit(cpio_base, cpio_bytes, cpio_crc_visitor,
                        &g, &found_trailer);
        return found_trailer;
    }

    return cpio_archive_bytes(cpio_base, cpio_bytes) > 0;
}

#ifdef TEST_CPIO

#include <malloc.h>
#include <sys/stat.h>
#include <fcntl.h>

static void cpio_newc_test(uint8_t* data, size_t data_bytes)
{
    if (data_bytes >= sizeof(cpio_newc_header_t*))
    {
        const cpio_newc_header_t* ph = (const cpio_newc_header_t*)data;
        printf("newc magic %08x inode %08x\n",
               a6u32(ph->magic), a8u32(ph->inode));
    }
}

static int
cpio_dump_info(int ordinal, const cpio_generic_header_t* header, void* arg)
{
    const uint8_t* file_data  = header->data;
    uint32_t       file_bytes = header->datasize;

    printf("%3d %-16s mode %7o %p...%p [%d bytes]\n",
           ordinal, header->name, header->mode,
           file_data, file_data + file_bytes, file_bytes);

    return 0;
}

int main(int argc, const char* argv[])
{
    if (argc == 2)
    {
        size_t read_bytes;
        struct stat st;
        int fd = open(argv[1], O_RDONLY);

        if (0 > fd)
        {
            fprintf(stderr, "Could not open %s\n", argv[1]);
            return -1;
        }

        fstat(fd, &st);
        size_t max_bytes = st.st_size;

        uint8_t* read_data = (uint8_t*)malloc(max_bytes);
        read_bytes = read(fd, read_data, max_bytes);

        printf("Read %lu bytes\n", read_bytes);
        printf("Image Valid %d Image bytes %lu\n",
               cpio_archive_valid(read_data, read_bytes),
               cpio_archive_bytes(read_data, read_bytes));

        cpio_generic_header_t g;
        cpio_visit(read_data, read_bytes, cpio_dump_info, &g, NULL);
        {
            const char* files[] = { "bfish/tip/build/hake/Path.o", "sbin/.marker", "fred" };
            int i;
            for (i = 0; i < sizeof(files) / sizeof(files[0]); i++)
            {
                const uint8_t *my_base;
                size_t my_bytes;
                if (cpio_get_file_by_name(read_data, read_bytes, files[i],
                                          &my_base, &my_bytes))
                {
                    printf("%s => %p...%p [%lu bytes]\n",
                           files[i], my_base, my_base + my_bytes, my_bytes);
                }
                else
                {
                    printf("%s => FILE NOT FOUND\n", files[i]);
                }
            }
        }

        free(read_data);
        close(fd);
    }
    return 0;
}

#endif // TEST_CPIO
