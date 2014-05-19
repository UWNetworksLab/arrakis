/* simple hash table
 * --kkourt@cslab.ece.ntua.gr */
#include <stdio.h>
#include <stdlib.h>

#include "hash.h"

hash_t *hash_init(unsigned int size)
{
	hash_t *hash;

	hash = malloc(sizeof(hash_t));
	hash->table = calloc(size, sizeof(hash_entry_t *));
	if (!hash || !hash->table){
		fprintf(stderr, "malloc failed\n");
		exit(1);
	}
	hash->size = size;

	return hash;
}

void hash_print(hash_t *hash)
{
	unsigned int i;
	hash_entry_t *curr;

	printf("hash:%p\n", hash);
	printf("==============================\n");
	for (i = 0; i < hash->size; i++) {
		printf("Bucket %d:", i);
		for (curr = hash->table[i]; curr != NULL; curr = curr->next)
			printf("[%lu] => %lu", curr->key, curr->val);
		printf("\n");
	}
	printf("==============================\n");
}

void hash_destroy(hash_t *hash)
{
	unsigned int i;
	hash_entry_t *curr;
	for (i = 0; i < hash->size; i++) {
		for (curr = hash->table[i]; curr != NULL;  ) {
			curr = curr->next;
			free(curr);
		}
	}
	free(hash->table);
	free(hash);
}

void hash_insert(hash_t *hash, unsigned long key, unsigned long val)
{
	hash_entry_t *new_entry, *curr;
	unsigned int bucket;

	bucket = hash_fn(hash, key);
	//Lookup
	curr = hash->table[bucket];
	while (curr) {
		/* found key */
		if (curr->key == key){
			curr->val = val;
			return;
		}
		curr = curr->next;
	}

	/* key does not exist, allocate a new entry  */
	new_entry = malloc(sizeof(hash_entry_t));
	new_entry->key = key;
	new_entry->val = val;

	/* put new entry, at the beggining of the bucket */
	new_entry->next = hash->table[bucket];
	hash->table[bucket] = new_entry;
}

unsigned long hash_lookup(hash_t *hash, unsigned long key)
{
	unsigned int bucket;
	hash_entry_t *curr;

	bucket = hash_fn(hash,key);
	curr = hash->table[bucket];
	for (;;){
		/* entry not found */
		if (curr == NULL)
			return HASH_ENTRY_NOTFOUND;

		/* entry found */
		if (curr->key == key)
			break;

		curr = curr->next;
	}
	return curr->val;

}

unsigned long hash_delete(hash_t *hash, unsigned long key)
{
	unsigned int bucket;
	hash_entry_t *curr, *prev;
	unsigned long ret;

	bucket = hash_fn(hash,key);
	curr = hash->table[bucket];
	prev = curr;
	for (;;){
		/* entry does not exist */
		if (curr == NULL)
			return HASH_ENTRY_NOTFOUND;

		/* found entry */
		if (curr->key == key)
			break;

		prev = curr;
		curr = curr->next;
	}

	/* delete entry */
	ret = curr->val;
	if (curr == hash->table[bucket])
		hash->table[bucket] = curr->next;
	else
		prev->next = curr->next;
	free(curr);
	return ret;
}

int hash_swap(hash_t *hash, unsigned long key1, unsigned long key2)
{
	int bucket;
	unsigned long tmp;
	hash_entry_t *entry1, *entry2;

	/* find first value */
	bucket = hash_fn(hash,key1);
	entry1 = hash->table[bucket];
	for (;;){
		/* first entry not found */
		if (entry1 == NULL)
			return -1;

		/* first entry found */
		if (entry1->key == key1)
			break;

		entry1 = entry1->next;
	}

	/* find second value */
	bucket = hash_fn(hash,key2);
	entry2 = hash->table[bucket];
	for (;;){
		/* first entry not found */
		if (entry2 == NULL)
			return -1;

		/* first entry found */
		if (entry2->key == key2)
			break;

		entry2 = entry2->next;
	}

	/* do the swap */
	tmp = entry1->val;
	entry1->val = entry2->val;
	entry2->val = tmp;

	return 0;
}
