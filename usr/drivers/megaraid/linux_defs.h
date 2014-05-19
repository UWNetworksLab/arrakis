#ifndef LINUX_DEFS_H
#define LINUX_DEFS_H

/* Local physical address. */
typedef uintptr_t lpaddr_t;
#define PRIuLPADDR PRIuPTR
#define PRIxLPADDR PRIxPTR

#define __packed        __attribute__((__packed__))
#define offsetof(type, member) __builtin_offsetof(type, member)

#define VREGION_FLAGS_READ_WRITE_NOCACHE	0

struct capref {
  lpaddr_t paddr;
};

typedef int vregion_flags_t;

struct frame_identity {
  lpaddr_t base;
};

#endif
