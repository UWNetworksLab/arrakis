#include <mdb/mdb.h>

extern struct cte *old_start;
void old_mdb_insert(struct cte *cte);
void old_mdb_remove(struct cte *cte);
struct cte* old_mdb_predecessor(struct cte *cte);
struct cte* old_mdb_successor(struct cte *cte);
bool old_mdb_has_copies(struct cte *cte);
bool old_mdb_has_ancestors(struct cte *cte);
bool old_mdb_has_descendants(struct cte *cte);
