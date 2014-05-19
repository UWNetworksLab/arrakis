#ifndef __HASH_H_
#define __HASH_H_

/*
 * Simple hash table implementation
 * --kkourt@cslab.ece.ntua.gr
 */

/* hash entry */
typedef struct hash_entry_st {
	/* key, value for hash entry */
	unsigned long key;
	unsigned long val;
	/* pointer to next entry, if NULL this is the last one */
	struct hash_entry_st *next;
	//char padding[40];
} hash_entry_t;

typedef struct hash_st {
	 /* hash table: each entry is is a pointer to
	  * the head of a linked list of hash entries */
	hash_entry_t **table;
	unsigned int size;	/* number of slots */
} hash_t;

/**
 * hash_init: initialize a hash table
 *  size: size of the table
 */
hash_t *hash_init(unsigned int size);

#define HASH_ENTRY_NOTFOUND (~0UL)
/**
 * hash_lookup: find the value of the given key
 *  If entry exists return val, else return HASH_ENTRY_NOTFOUND
 */
unsigned long hash_lookup(hash_t *hash, unsigned long key);

/**
 * hash_insert: insert a new value for the given key
 * If an entry for the key exists, just replace it
 */
void hash_insert(hash_t *hash, unsigned long key, unsigned long val);

/**
 * hash_delete: delete the entry of the given value
 * If entry exists return old value, else return HASH_ENTRY_NOTFOUND
 */
unsigned long hash_delete(hash_t *hash, unsigned long key);

/**
 * hash_destroy: destroy the hash
 */
void hash_destroy(hash_t *hash);

/**
 * hash_swap: exchange the values of two keys.
 *  if an entry for either keys does not exist, return -1
 *  else return 0
 */
int hash_swap(hash_t *hash, unsigned long key1, unsigned long key2);

static inline unsigned long hash_fn(hash_t *hash, unsigned long key)
{
	return (key % hash->size);
}


void hash_print(hash_t *hash);
#endif
