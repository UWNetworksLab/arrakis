#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <assert.h>
#include <errors/errno.h>

#include "bitfield.h"

static inline uint8_t* get_offset(struct bitfield* b, size_t bit_number) 
{
	assert(bit_number < BITFIELD_MAX);
	uint8_t char_offset = bit_number / BITS_PER_CHAR;
	
	return b->field + char_offset;
}

/**
 * \brief Turns on a bit in the bit field.
 *
 * \param bitfield Pointer to the bit field
 * \param bit_number Which bit to set
 */
void bitfield_on(struct bitfield* b, size_t bit_number) 
{
	uint8_t* bitmask = get_offset(b, bit_number);
	uint8_t bit_offset = bit_number % BITS_PER_CHAR;
	*bitmask |= 1 << bit_offset;
}


/**
 * \brief Turns off a bit in the bit field.
 *
 * \param bitfield Pointer to bitfield
 * \param bit_number Which bit to unset
 */
void bitfield_off(struct bitfield* b, size_t bit_number) 
{
	uint8_t* bitmask = get_offset(b, bit_number);
	uint8_t bit_offset = bit_number % BITS_PER_CHAR;
	*bitmask &= ~(1 << bit_offset);
}

/**
 * \brief Gets the high/low of a specific bit in a bit field.
 *
 * \return value of the bit
 */
bool bitfield_get(struct bitfield* b, size_t bit_number) 
{
	uint8_t* bitmask = get_offset(b, bit_number);
	uint8_t bit_offset = bit_number % BITS_PER_CHAR;
	return (*bitmask & (1 << bit_offset)) > 0;
}

static uint8_t compute_union(size_t index, struct bitfield** fields, size_t field_count)
{
	//printf("compute union at index: %lu\n", index);
	uint8_t u = 0;
	for (size_t i=0; i < field_count; i++) {
		u |= fields[i]->field[index];
	}

	return u;
}

/**
 * \brief Computes union for a given number of bitfields.
 * 
 * TODO: This functions could be improved a lot by using
 * word size instead of uint8.
 *
 * \param fields Array of pointers to bitfields
 * \param field_count Number of bitfields
 * \param last -1 to indicate a new run, last returned result otherwise.
 */
int32_t bitfield_union(struct bitfield** fields, size_t field_count, int32_t last)
{
	static uint8_t field_union;
	static size_t current = 0;
	if (last == -1) {
		field_union = 0;
		current = 0;
	}

	// Scan over whole bitfield
	while(current < BITFIELD_MAX) {

		// Compute union for next character
		if (current % BITS_PER_CHAR == 0) {
			field_union = compute_union(current/BITS_PER_CHAR, fields, field_count);
		}
	
		// Walk through character, output all the bits we find
		for(size_t idx = (current % BITS_PER_CHAR); idx < BITS_PER_CHAR; idx++) {
			//printf("bit: %lu is %d\n", idx, (field_union & (1 << idx)));
			if ( (field_union & (1 << idx)) > 0) {
				return current++;
			}
			else {
				current++;
			}
		}
	}

	return -1;
}

/**
 * \brief Prints the current bitfield up to
 * a given number of bits.
 *
 * \param first where to start printing in the bitfield
 * \param number_of_bits #bits printed beginning from first
 */
void print_bitfield(struct bitfield* b, size_t first, size_t number_of_bits) {
	printf("Printing Bitfield from %zu to %zu:\n", first, first+number_of_bits-1);

	for(size_t i=0; i<number_of_bits; i++) {
		printf("%d", bitfield_get(b, first+i));
		if((i+1) % 10 == 0)
			printf("\n");
	}
}


errval_t bitfield_create(struct bitfield** b) 
{
	*b = calloc(1, sizeof(struct bitfield));
	if (*b == NULL) {
		return LIB_ERR_MALLOC_FAIL;
	}
	
	return SYS_ERR_OK;
}


#ifdef TEST_BITFIELD
int main(int argc, char** argv)
{
	struct bitfield* b = malloc(sizeof(struct bitfield));
	memset(b, 0, sizeof(struct bitfield));

	*(uint64_t*)b->field = 0xffff;
	struct bitfield* b2 = malloc(sizeof(struct bitfield));
	memset(b2, 0, sizeof(struct bitfield));
	*(uint64_t*)b2->field = 0xFFffffff0000;

	struct bitfield* fields[2] = {b, b2};
	size_t next = -1;
	while( (next = bitfield_union(fields, 2, next)) != -1) {
		printf("union found next: %lu\n", next);
	}

	//printf("before print\n");
	//print_bitfield(b, 0, 1256);
	//printf("\ndone.\n");
	
	return 0;
}
#endif
