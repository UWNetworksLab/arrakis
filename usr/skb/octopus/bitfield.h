#ifndef BITFIELD_H_
#define BITFIELD_H_

#include <stddef.h>
#include <inttypes.h>

#define BITS_PER_CHAR 8
#define BITFIELD_CHARS 2048
#define BITFIELD_MAX (BITS_PER_CHAR*BITFIELD_CHARS)

struct bitfield {
	uint8_t field[BITFIELD_CHARS];
};

void bitfield_on(struct bitfield*, size_t);
void bitfield_off(struct bitfield*, size_t);
bool bitfield_get(struct bitfield* b, size_t bit_number);
int32_t bitfield_union(struct bitfield** fields, size_t field_count, int32_t last);
errval_t bitfield_create(struct bitfield** b);

void print_bitfield(struct bitfield*, size_t, size_t);

#endif /* BITFIELD_H_ */
