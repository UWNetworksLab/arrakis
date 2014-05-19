#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#ifndef __linux__
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/bulk_transfer.h>
#include <if/replay_defs.h>
#include <errno.h>
#else
static const char vfs_cache_str[] = "linux (TCP/IP)";
#include <unistd.h>
#include <stdbool.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <poll.h>
#include <netdb.h>
#include <errno.h>
#include <sched.h>
#include <inttypes.h>
#endif

#include "defs.h"
#include "hash.h"

#define MAX_LINE        1024
#define MAX_SLAVES      64
#define MAX_DEPS        60
#define BULK_BLOCK_SIZE (4096*256) // 1MiB
#define BULK_BLOCKS_NR  1
#define BULK_TOTAL_SIZE (BULK_BLOCK_SIZE*BULK_BLOCKS_NR)

/* TRACE LIST */

struct trace_list {
    struct trace_entry *head, *tail;
};

static inline void trace_add_tail(struct trace_list *tl, struct trace_entry *te)
{
    te->next = NULL;

    if (tl->head == NULL) {
        tl->head = te;
    } else {
        tl->tail->next = te;
    }

    tl->tail = te;
}

/* PID ENTRIES (TASKS) */

struct slave; /* forward reference */
struct pid_entry {
    int pid;
    struct trace_list trace_l;
    size_t tentries_nr;
    /* list of children */
    struct pid_entry *children[MAX_DEPS];
    size_t children_nr;
    /* list of parents */
    struct pid_entry *parents[MAX_DEPS];
    size_t parents_nr;

    /* buffer for trace entries */
    replay_eventrec_t *tes;
    size_t tes_size; /* size of tes buffer in bytes */

    size_t parents_completed;
    struct slave *sl;
    unsigned completed:1;
};

/* SLAVES */

struct slave {
    //int pid[MAX_PIDS];
    struct pid_entry *pe;
    //struct trace_entry *current_te;
    //struct qelem *queue, *qend;
#ifndef __linux__
    struct replay_binding *b;
    /* bulk transfer data */
    struct capref        frame;
    struct bulk_transfer bt;
    uintptr_t            current_bid;
#else
    int socket;
    ssize_t sentbytes;
#endif
};

static struct {
    int num_slaves;
    int num_finished;
    struct slave slaves[MAX_SLAVES];
} SlState;

//struct qelem {
//    replay_eventrec_t er;
//    struct qelem *next, *prev;
//};
//
//struct writer {
//    int fnum, pid;
//    struct slave *slave;
//    struct writer *prev, *next;
//};

/*
 * Dependencies:
 *  W writes to a file that R reads
 *  R needs to wait for W to finish
 *  W is parent, R is child
 *  nodes that are ready to execute have no parents
 */
struct task_graph {
    struct pid_entry *pes;      /* array of (all) pid entries */
    int pes_nr, pes_size;       /* number of pid entries, size of array */
    struct pid_entry **stack;   /* current stack */
    int stack_nr, stack_size;   /* stack entries/size */
    int pes_completed;          /* completed entries */
    hash_t *pids_h;             /* pid -> pid_entry hash */
};
static struct task_graph TG;

static struct pid_entry *
tg_stack_peek(struct task_graph *tg, int i)
{
    assert(i < tg->stack_nr);
    return tg->stack[tg->stack_size - i];
}

static struct pid_entry *
tg_pop(struct task_graph *tg)
{
    struct pid_entry *ret;
    if (tg->stack_nr == 0)
        return NULL;
    ret = tg->stack[tg->stack_size +1 - tg->stack_nr];
    tg->stack_nr--;
    assert(ret != NULL);
    return ret;
}

static void
tg_push(struct task_graph *tg, struct pid_entry *pe)
{
    assert(tg->stack_nr < tg->stack_size);
    tg->stack[tg->stack_size - tg->stack_nr] = pe;
    tg->stack_nr++;
}

static void
__build_taskgraph_stack(struct task_graph *tg)
{
    tg->stack_nr = 0;
    for (int i=0; i<tg->pes_nr; i++) {
        struct pid_entry *pe = tg->pes + i;
        if (pe->parents_nr == 0)
            tg_push(tg, pe);
    }
}

static void
build_taskgraph_stack(struct task_graph *tg)
{
    // build stack, and fill it with ready nodes (i.e., no parent nodes)
    tg->stack_size = TOTAL_PIDS;
    tg->stack = calloc(tg->stack_size, sizeof(struct pid_entry *));
    assert(tg->stack);
    __build_taskgraph_stack(tg);
}

static void __attribute__((unused))
tg_reset(struct task_graph *tg)
{
    for (int i=0; i<tg->pes_nr; i++) {
        tg->pes[i].completed = 0;
        tg->pes[i].parents_completed = 0;
    }
    tg->pes_completed = 0;
    __build_taskgraph_stack(tg);
}

static void
tg_complete(struct task_graph *tg, struct pid_entry *pe)
{
    assert(pe->completed == 0);
    pe->completed = 1;
    tg->pes_completed++;
    dmsg("\tpe: %p (idx:%ld,pid:%d) completed (%d/%d)\n", pe, (pe - tg->pes), pe->pid, tg->pes_completed, tg->pes_nr);
    for (int i=0; i < pe->children_nr; i++) {
        struct pid_entry *pe_child = pe->children[i];
        if (++pe_child->parents_completed == pe_child->parents_nr) {
            tg_push(tg, pe_child);
        }
    }
}


//static struct writer *writers = NULL;
#ifndef __linux__
static bool bound; /* XXX: make this volatile? No, handler runs on the same context */
static bool init_ok;
static bool print_stats_ok;
#endif

#ifdef __linux__
#endif

#ifndef __linux__
//static void event_done(struct replay_binding *b, uint32_t fnum)
//{
//    /* if(te->op == TOP_Close) { */
//        // See if it was a writer and remove
//
//    printf("writer done for %u\n", fnum);
//
//        for(struct writer *w = writers; w != NULL; w = w->next) {
//            if(w->fnum == fnum) {
//                assert(w != NULL);
//                if(w != writers) {
//                    assert(w != NULL);
//                    assert(w->prev != NULL);
//                    w->prev->next = w->next;
//                } else {
//                    writers = w->next;
//                }
//                free(w);
//                break;
//            }
//        }
//    /* } */
//}

static void
init_reply_handler(struct replay_binding *b)
{
    assert(!init_ok);
    init_ok = true;
}

static void
print_stats_reply_handler(struct replay_binding *b)
{
    assert(!print_stats_ok);
    print_stats_ok = true;
}
static void
task_completed_handler(struct replay_binding *b, uint16_t pid, uint64_t bulk_id)
{
    struct task_graph *tg = b->st;
    struct slave *sl;
    struct pid_entry *pe;

    pe = (void *)hash_lookup(tg->pids_h, pid);
    assert(pe != (void *)HASH_ENTRY_NOTFOUND);
    sl = pe->sl;
    tg_complete(tg, pe);
    bulk_free(&sl->bt, bulk_id);
    sl->pe = NULL;
}

static void
finish_reply_handler(struct replay_binding *b)
{
    dmsg("ENTER\n");
    SlState.num_finished++;
}

static struct replay_rx_vtbl replay_vtbl = {
    .task_completed           = task_completed_handler,
    .slave_finish_reply      = finish_reply_handler,
    .slave_init_reply        = init_reply_handler,
    .slave_print_stats_reply = print_stats_reply_handler
};

static void replay_bind_cont(void *st, errval_t err, struct replay_binding *b)
{
    static int slavenum = 0;
    struct slave *sl = &SlState.slaves[slavenum];
    slavenum++;

    dmsg("ENTER\n");
    //printf("%s:%s MY TASKGRAPH IS %p\n", __FILE__, __FUNCTION__, &TG);

    assert(err_is_ok(err));
    sl->b = b;
    b->rx_vtbl = replay_vtbl;
    b->st = &TG;
    bound = true;
    /* printf("assigned binding to %p\n", sl); */
}
#else
static inline uint64_t rdtsc(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
}
#endif

//static bool printall = false;


static inline void
pentry_add_dependency(struct pid_entry *parent, struct pid_entry *child)
{
    int idx;
    for (int i=0; i<parent->children_nr; i++)
        if (parent->children[i]->pid == child->pid)
            return;

    //printf("%d -> %d;\n", parent->pid, child->pid);

    idx = parent->children_nr;
    assert(idx < MAX_DEPS);
    parent->children[idx] = child;
    parent->children_nr++;

    idx = child->parents_nr;
    assert(idx < MAX_DEPS);
    child->parents[idx] = parent;
    child->parents_nr++;
}

//static struct pid_entry *allpids[nOTAL_PIDS];

static void
parse_tracefile_line(char *line, int linen, struct trace_entry *te)
{
        size_t fnum, size;
        char flags[1024];
        int fd;
        unsigned int pid;

        if(sscanf(line, "open %zu %s %d %u", &fnum, flags, &fd, &pid) >= 4) {
            te->op = TOP_Open;
            te->fd = fd;
            te->u.fnum = fnum;
        } else if(sscanf(line, "close %d %u", &fd, &pid) >= 2) {
            te->op = TOP_Close;
            te->fd = fd;
        } else if(sscanf(line, "read %d %zu %u", &fd, &size, &pid) >= 3) {
            te->op = TOP_Read;
            te->fd = fd;
            te->u.size = size;
        } else if(sscanf(line, "write %d %zu %u", &fd, &size, &pid) >= 3) {
            te->op = TOP_Write;
            te->fd = fd;
            te->u.size = size;
        } else if(sscanf(line, "seek %d %zu %u", &fd, &size, &pid) >= 3) {
            te->op = TOP_Seek;
            te->fd = fd;
            te->u.size = size;
        } else if(sscanf(line, "creat %zu %s %d %u", &fnum, flags, &fd, &pid) >= 4) {
            te->op = TOP_Create;
            te->fd = fd;
            te->u.fnum = fnum;
        } else if(sscanf(line, "unlink %zu %s %u", &fnum, flags, &pid) >= 3) {
            te->op = TOP_Unlink;
            te->u.fnum = fnum;
        } else if(sscanf(line, "exit %u", &pid) >= 1) {
            te->op = TOP_Exit;
        } else {
            printf("Invalid line %d: %s\n", linen, line);
            exit(EXIT_FAILURE);
        }

        // There's always a PID
        te->pid = pid;
        assert(pid != 0);
        te->fline = linen;

        // If we have flags, set them now
        if(te->op == TOP_Open || te->op == TOP_Create) {
            if(!strcmp(flags, "rdonly")) {
                te->mode = FLAGS_RdOnly;
            } else if(!strcmp(flags, "wronly")) {
                te->mode = FLAGS_WrOnly;
            } else if(!strcmp(flags, "rdwr")) {
                te->mode = FLAGS_RdWr;
            } else {
                printf("Invalid open flags: %s\n", flags);
                exit(EXIT_FAILURE);
            }
        }
}

static void
parse_tracefile(const char *tracefile, struct trace_list *tlist)
{
    printf("reading tracefile...\n");

    FILE *f = fopen(tracefile, "r");
    assert(f != NULL);
    int linen = 0;

    while(!feof(f)) {
        char line[MAX_LINE];

        if (fgets(line, MAX_LINE, f) == NULL) {
            break;
        }

        linen++;
        if (linen % 1000 == 0) {
            printf("---- %s:%s() parsing line = %d\n", __FILE__, __FUNCTION__, linen);
        }

        struct trace_entry *tentry = malloc(sizeof(struct trace_entry));
        assert(tentry != NULL);

        // parse current line to tentry
        parse_tracefile_line(line, linen, tentry);

        // Link it in with the rest of the list (forward order)
        trace_add_tail(tlist, tentry);
    }
    fclose(f);
    printf("tracefile read [number of lines:%d]\n", linen);
}

static void
build_taskgraph(struct trace_list *tl, struct task_graph *tg)
{
    assert(tl->head != NULL && tl->tail != NULL);
    struct trace_entry *te, *te_next = NULL;
    hash_t *writers_h = hash_init(TOTAL_PIDS); /* fnum -> pid_entry */
    // alloc a sequential array for all pid entries
    tg->pids_h = hash_init(TOTAL_PIDS); /* pid  -> pid_entry */
    tg->pes_size = TOTAL_PIDS;
    tg->pes = calloc(tg->pes_size, sizeof(struct pid_entry)); // we can realloc it later
    tg->pes_nr = 0;

    for (te = tl->head; te != NULL; te = te_next) {
        // store next pointer (trace_add_tail will change it)
        te_next = te->next;

        // if this is a new pid, create a new pid_entry
        struct pid_entry *pe = (void *)hash_lookup(tg->pids_h, te->pid);
        if (pe == (void *)HASH_ENTRY_NOTFOUND) {
            // if a pid's first operation is not Open/Create ignore it
            if (te->op != TOP_Open && te->op != TOP_Create) {
                printf("%s() :: IGNORING operation %d for pid %d\n", __FUNCTION__, te->op, te->pid);
                continue;
            }
            assert(tg->pes_nr < tg->pes_size);
            pe = tg->pes + tg->pes_nr++;
            pe->pid = te->pid;
            hash_insert(tg->pids_h, pe->pid, (unsigned long)pe);
            //printf("%d;\n", pe->pid);
        }

        // add trace entry into pid list
        trace_add_tail(&pe->trace_l, te);
        pe->tentries_nr++;

        /* track dependencies:
         *  - look at open/create operations
         *  - put writers in a hash table, based on the file they write
         *  - for readers, check if a writer exists for the file they read
         *  - avoid cyclic dependencies of RW open/create
         *  Note: For multiple writers we just track the latest open/create */
        if (te->op == TOP_Open && te->op == TOP_Create) {
            size_t fnum = te->u.fnum;
            struct pid_entry *pe_writer = (void *)hash_lookup(writers_h, fnum);

            if (te->mode != FLAGS_RdOnly) { // writer
                if (pe_writer != (void *)HASH_ENTRY_NOTFOUND && pe_writer->pid != pe->pid) {
                     //assert(0 && "multiple different writers");
                }
                hash_insert(writers_h, fnum, (unsigned long)pe);
                pe_writer = pe;
            }

            if ((te->mode != FLAGS_WrOnly) &&                 // this is a reader
                (pe_writer != (void *)HASH_ENTRY_NOTFOUND) && // a writer exists
                (pe_writer->pid != pe->pid)) {                // and is not the reader
                pentry_add_dependency(pe_writer, pe);
            }
        }
    }
    build_taskgraph_stack(tg);
    //cleanup and return
    tl->head = tl->tail = NULL;
    hash_destroy(writers_h);
}

static void
__print_taskgraph(struct pid_entry *root, int level)
{
    if (root == NULL)
        return;
    for (int i=0; i<level; i++)
        printf("\t");
    printf("%d (completed:%d)\n", root->pid, root->completed);

    for (int i=0; i<root->children_nr; i++) {
        __print_taskgraph(root->children[i], level+1);
    }
}

static void __attribute__((unused))
print_taskgraph(struct task_graph *tg)
{
    for (int i=0; i<tg->stack_nr; i++) {
        struct pid_entry *pe = tg_stack_peek(tg, i);
        __print_taskgraph(pe, 0);
    }
}

static void
print_pid_entry(struct pid_entry *pe, int print_ops)
{
    struct trace_entry *te;
    printf("pid entry (%p) pid:%d children:%zu parents:%zu completed:%d tentries:%zd\n", pe, pe->pid, pe->children_nr, pe->parents_nr, pe->completed, pe->tentries_nr);
    te = pe->trace_l.head;

    if (!print_ops) {
        return;
    }

    do {
        printf("\t op:%d pid:%d\n", te->op, te->pid);
    } while ((te = te->next) != NULL);
    printf("\tEND\n");
}

static void __attribute__((unused))
print_task(struct task_graph *tg, int pid)
{
    for (int i=0; i<tg->pes_nr; i++) {
        struct pid_entry *pe = tg->pes + i;
        if (pe->pid == pid) {
            print_pid_entry(pe, 0);
        }
    }
}

static void
mk_replay_event_req(struct trace_entry *te, replay_eventrec_t *req)
{
    req->op    = te->op;
    req->fd    = te->fd;
    req->mode  = te->mode;
    //req->fline = te->fline;
    req->pid   = te->pid;

    switch(te->op) {
    case TOP_Open:
    case TOP_Create:
    case TOP_Unlink:
        req->fnumsize = te->u.fnum;
        break;

    case TOP_Read:
    case TOP_Write:
    case TOP_Seek:
        req->fnumsize = te->u.size;
        break;

    case TOP_Close:
    case TOP_Exit:
        break;

    default:
        assert(0);
        break;
    }

    assert(req->pid != 0);
}


static void
trace_bufs_init(struct task_graph *tg)
{
    for (int i=0; i < tg->pes_nr; i++) {
        struct pid_entry *pe = tg->pes + i;

        size_t size = pe->tes_size = sizeof(replay_eventrec_t)*pe->tentries_nr;
        if (size >= BULK_TOTAL_SIZE) {
            msg("size for pid:%d [%zd] larger than %d\n", pe->pid, size, BULK_TOTAL_SIZE);
            assert(0);
        }

        assert(size <= BULK_TOTAL_SIZE);
        pe->tes = malloc(size);
        assert(pe->tes);

        struct trace_entry *te = pe->trace_l.head;
        for (int ti=0; ti < pe->tentries_nr; ti++) {
            mk_replay_event_req(te, pe->tes + ti);
            te = te->next;
        }
        assert(te == NULL); // make sure the count is right!
    }
}

static void __attribute__((unused))
print_all_tasks(struct task_graph *tg)
{
    for (int i=0; i<tg->pes_nr; i++) {
        struct pid_entry *pe = tg->pes + i;
        print_pid_entry(pe, 0);
    }
}

/* functions to be implemented seperately by bfish/linux */
static void slaves_connect(struct task_graph *tg);
static void slave_push_work(struct slave *);
static void slaves_finalize(void);
static void slaves_print_stats(void);
static void master_process_reqs(void);
uint64_t tscperms;

int main(int argc, char *argv[])
{
#ifndef __linux__
    if(argc < 5) {
        printf("Usage: %s tracefile nslaves mountdir mount-URL\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    assert(err_is_ok(sys_debug_get_tsc_per_ms(&tscperms)));
    errval_t err = vfs_mkdir(argv[3]);
    assert(err_is_ok(err));

    printf("----------------------------------- VFS MOUNT\n");
    err = vfs_mount(argv[3], argv[4]);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "vfs_mount");
    }
    printf("----------------------------------- VFS MOUNT DONE\n");
    assert(err_is_ok(err));
#else
    if(argc < 3) {
        printf("Usage: %s tracefile nslaves\n", argv[0]);
        exit(EXIT_FAILURE);
    }
#endif

    memset(&SlState, 0, sizeof(SlState));
    //SlState.waitset = get_default_waitset();
    //struct waitset ws;
    //waitset_init(&ws);
    //SlState.waitset = &ws;

    char *tracefile = argv[1];
    SlState.num_slaves = atoi(argv[2]);
    printf("tracefile=%s\n", tracefile);

    printf("reading dependency graph...\n");

    // Parse trace file into memory records
    struct trace_list tlist = {.head = NULL, .tail = NULL};
    parse_tracefile(tracefile, &tlist);

    // Build task graph. roots are nodes without dependencies
    #ifndef __linux__
    msg("[MASTER] My cpu is: %d\n", disp_get_core_id());
    #endif
    memset(&TG, 0, sizeof(TG));
    build_taskgraph(&tlist, &TG);
    //print_all_tasks(&TG);
    //print_taskgraph(&TG);
    //printf("TG entries:%d completed:%d stack_size:%d\n", TG.pes_nr, TG.pes_completed, TG.stack_nr);

    msg("[MASTER] INITIALIZING BUFFERS...\n");
    trace_bufs_init(&TG);

    msg("[MASTER] CONNECTING TO SLAVES...\n");
    slaves_connect(&TG);

    msg("[MASTER] STARTING WORK...\n");
    uint64_t start_ticks = rdtsc();
    for (;;) {
        /* enqueue work to the slaves */
        for (int sid=0; sid < SlState.num_slaves; sid++) {
            struct slave *sl = SlState.slaves + sid;
            // try to assign a pid entry to a slave, if it doesn't hove one
            if (sl->pe == NULL) {
                sl->pe = tg_pop(&TG);
                if (sl->pe == NULL) {
                    continue; /* no more tasks in the stack */
                }
                dmsg("[MASTER] assigned pid:%d to sl:%d (stack_nr:%d completed:%d total:%d bytes:%zd)\n",
                      sl->pe->pid, sid, TG.stack_nr, TG.pes_completed, TG.pes_nr, sl->pe->tes_size);
                sl->pe->sl = sl;
                slave_push_work(sl);
            }
            master_process_reqs();
        }

        if (TG.pes_completed == TG.pes_nr)
            break;
    }

    uint64_t work_ticks = rdtsc() - start_ticks;
    slaves_finalize();
    uint64_t total_ticks = rdtsc() - start_ticks;
    printf("[MASTER] replay done> cache:%s slaves:%d ticks:%" PRIu64
           " (%lf ms) [total: %lfms]\n",
            vfs_cache_str, SlState.num_slaves, work_ticks,
            (double)work_ticks /(double)tscperms,
            (double)total_ticks/(double)tscperms);
    slaves_print_stats();
    return 0;
}

#ifndef __linux__
static void
master_process_reqs(void)
{
    /* process slave requests */
    for (;;){
        struct event_closure closure;
        errval_t ret;
        ret = check_for_event(get_default_waitset(), &closure);
        if (ret == LIB_ERR_NO_EVENT)
            break;
        assert(err_is_ok(ret));
        assert(closure.handler != NULL);
        //printf(RED("-------- GOT A MASTER EVENT handler=%p arg=%p"), closure.handler, closure.arg);
        closure.handler(closure.arg);
    }
}

static void
slaves_finalize(void)
{
    int err;

    /* notify slaves */
    for (int sid=0; sid < SlState.num_slaves; sid++) {
        struct slave *sl = SlState.slaves + sid;
        do {
            err = sl->b->tx_vtbl.slave_finish(sl->b, NOP_CONT);
        } while (err_no(err) == FLOUNDER_ERR_TX_BUSY);
        assert(err_is_ok(err));
    }

    /* wait for their replies */
    do {
        err = event_dispatch(get_default_waitset());
        assert(err_is_ok(err));
    } while (SlState.num_finished < SlState.num_slaves);
}

static void
slaves_print_stats(void)
{
    int err;

    /* have slaves print stats synchronously */
    for (int sid=0; sid < SlState.num_slaves; sid++) {
        struct slave *sl = SlState.slaves + sid;
        print_stats_ok = false;
        err = sl->b->tx_vtbl.slave_print_stats(sl->b, NOP_CONT);
        assert(err_is_ok(err));
        while (!print_stats_ok) {
            err = event_dispatch(get_default_waitset());
            assert(err_is_ok(err));
        }
    }
}

static void
slave_push_work(struct slave *sl)
{
    int err;
    struct bulk_buf *bb;
    uint64_t bulk_id;

    //dmsg("pushing work for slave: %ld (%p) pid:%d (completed:%d)\n", sl-slaves, sl, sl->pe->pid, sl->pe->completed);
    bb = bulk_alloc(&sl->bt);
    if (bb == NULL) {
        return;
    }
    bulk_buf_copy(bb, sl->pe->tes, sl->pe->tes_size);
    bulk_id = bulk_prepare_send(bb);
    err = sl->b->tx_vtbl.new_task(sl->b, NOP_CONT, bulk_id, sl->pe->tes_size);
    if (err == FLOUNDER_ERR_TX_BUSY) {
        bulk_free(&sl->bt, bulk_id);
        return;
    }
    assert(err_is_ok(err));
}

static void
slaves_connect(struct task_graph *tg)
{
    char name[128];
    iref_t iref;
    int err;

    for (int sid=0; sid < SlState.num_slaves; sid++) {
        int r = snprintf(name, 128, "replay_slave.%u", sid + 1);
        struct slave *sl = SlState.slaves + sid;
        assert(r != -1);

        err = nameservice_blocking_lookup(name, &iref);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "could not lookup IREF for replay slave");
            abort();
        }

        /* bound to slave  */
        bound = false;
        err = replay_bind(iref, replay_bind_cont, NULL,
                          get_default_waitset(),
                          IDC_BIND_FLAGS_DEFAULT);
        if(err_is_fail(err)) {
            DEBUG_ERR(err, "replay_bind");
        }
        while(!bound) {
            err = event_dispatch(get_default_waitset());
            assert(err_is_ok(err));
        }
        msg("Bound to slave %d\n", sid);

        /* initialize bulk transfer for slave */
        init_ok = false;
        err = bulk_create(BULK_TOTAL_SIZE, BULK_BLOCK_SIZE, &sl->frame, &sl->bt, false);
        assert(err_is_ok(err));
        err = sl->b->tx_vtbl.slave_init(sl->b, NOP_CONT, sl->frame, BULK_TOTAL_SIZE);
        assert(err_is_ok(err));
        while (!init_ok) {
            err = event_dispatch(get_default_waitset());
            assert(err_is_ok(err));
        }

        msg("Slave %d initialized\n", sid);
    }
}
#endif

#ifdef __linux__
static void
slave_push_work(struct slave *sl)
{
    ssize_t ret;

    assert(sl->pe != NULL);
    assert(sl->pe->tes_size > sl->sentbytes);

    dmsg("TRYING TO SEND: %zd bytes for pid=%u\n", sl->pe->tes_size - sl->sentbytes, sl->pe->pid);
    ret = send(sl->socket,
               (char *)sl->pe->tes + sl->sentbytes,
               sl->pe->tes_size - sl->sentbytes,
               0); /* setting MSG_DONTWAIT seems to cause problems */
    if (ret <= 0 && errno != EAGAIN) {
        perror("send");
        exit(1);
    }
    dmsg("SENT: %zd bytes for pid=%u\n", ret, sl->pe->pid);
    sl->sentbytes += ret;
}

static void
slaves_finalize(void)
{
    for (int i=0; i<SlState.num_slaves; i++) {
        struct slave *sl = SlState.slaves + i;
        close(sl->socket);
    }
}

static void
master_process_reqs(void)
{
    uint16_t rpid;
    // read from available fds
    for (int i=0; i<SlState.num_slaves; i++) {
        struct slave *sl = SlState.slaves + i;

        if (sl->pe == NULL) {
            continue;
        }

        /* first check if we need to send more data to the slave
         * (main loop will only call slave_push_work() once) */
         if (sl->pe->tes_size > sl->sentbytes) {
            slave_push_work(sl);
         }

        int r = recv(sl->socket, &rpid, sizeof(rpid), MSG_DONTWAIT);
        if (r == -1) {
            assert(errno == EWOULDBLOCK || errno == EAGAIN);
            continue; /* no data here, move on */
        }
        /* slave is done with task */
        assert(r == sizeof(rpid));
        assert(rpid == sl->pe->pid);
        tg_complete(&TG, sl->pe);
        sl->pe = NULL;
        sl->sentbytes = 0;
    }
}

static void slaves_print_stats(void)
{
}

/* connection info */

static void
slaves_connect(struct task_graph *tg)
{
    msg("connecting to slaves...\n");
    for(int i = 0; i < SlState.num_slaves; i++) {
        int ret;
        struct slave *sl = &SlState.slaves[i];

        sl->socket = socket(AF_INET, SOCK_STREAM, 0);
        if (sl->socket == -1) {
            perror("socket");
            exit(1);
        }

        struct sockaddr_in a = {
            .sin_family = PF_INET,
            .sin_port = htons(0),
            .sin_addr = {
                .s_addr = htonl(INADDR_ANY)
            }
        };

        ret = bind(sl->socket, (struct sockaddr *)&a, sizeof(a));
        if (ret != 0) {
            perror("bind");
            exit(1);
        }

        int port = 1234;
        char host[128];
        snprintf(host, 128, "rck%02u", i + 1);

        // FOR DEBUGGING!
        snprintf(host, 128, "localhost");
        printf("connecting to %s:%d ...\n", host, port);
        port = 1234 + i;

        struct hostent *h;
        h = gethostbyname(host);
        assert(h != NULL && h->h_length == sizeof(struct in_addr));

        struct sockaddr_in sa = {
            .sin_family = AF_INET,
            .sin_port = htons(port),
            .sin_addr = *(struct in_addr *)h->h_addr_list[0]
        };

        ret = connect(sl->socket, (struct sockaddr *)&sa, sizeof(sa));
        if (ret < 0) {
            perror("connect");
            exit(1);
        }

        sl->sentbytes = 0;
        #if 0 /* do a recv with MSG_DONTWAIT */
        /* set non-blocking flag */
        int sock_fl = fcntl(sl->socket, F_GETFD);
        sock_fl |= O_NONBLOCK;
        sock_fl = fcntl(sl->socket, F_SETFD, sock_fl);
        assert(sock_fl & O_NONBLOCK);
        #endif
    }
}
#endif

//
//
//    uint64_t tscperms;
//#ifndef __linux__
//    err = sys_debug_get_tsc_per_ms(&tscperms);
//    assert(err_is_ok(err));
//
//#else
//    tscperms = 533000;
//
//#endif
//
//    printf("starting replay\n");
//
//    /* for(struct trace_entry *te = trace; te != NULL; te = te->next) { */
//    /*     static int cnt = 0; */
//    /*     printf("%d: %d, %zu, %d, %d, %d, fline %d\n", */
//    /*            cnt, te->op, te->u.fnum, te->fd, te->mode, te->pid, te->fline); */
//    /*     cnt++; */
//    /* } */
//
//    uint64_t start = rdtsc();
//
//    // Start trace replay
//    for(struct trace_entry *te = trace; te != NULL; te = te->next) {
//        // Distribute work to slaves -- either they are empty (PID ==
//        // 0) or they already execute for a PID, in which case we keep
//        // sending them that PID's work until the PID exits)
//
//        static int cnt = 0;
//
//        /* if(((cnt * 100) / linen) % 5 == 0) { */
//            /* printf("%d / %d\n", cnt, linen); */
//        /* } */
//        cnt++;
//
//        /* printall = false; */
//        /* if(cnt == 6186 || cnt == 5840) { */
//        /*     printall = true; */
//        /* } */
//
//        // If this is an exit, remove the PID and continue
//        if(te->op == TOP_Exit) {
//            int i;
//            /* printf("PIDs: "); */
//            for(i = 0; i < num_slaves; i++) {
//                /* printf("%u ", slaves[i].pid); */
//                for(int j = 0; j < MAX_PIDS; j++) {
//                    if(slaves[i].pid[j] == te->pid) {
//                        slaves[i].pid[j] = 0;
//                        goto outexit;
//                    }
//                }
//            }
//        outexit:
//            /* printf("\n"); */
//
//            if(i < num_slaves) {
//                continue;
//            } else {
//                printf("%d: exit on non-existant PID (%u), file line %d\n",
//                       cnt, te->pid, te->fline);
//                exit(EXIT_FAILURE);
//            }
//        }
//
//        if(printall) {
//            printf("find slave\n");
//        }
//
//    /* again: */
//        // Find a slave with the same PID
//        struct slave *emptyslave = NULL, *s = NULL;
//        int i;
//        for(i = 0; i < num_slaves; i++) {
//            s = &slaves[i];
//
//            /* if(s->pid == 0) { */
//            /*     /\* printf("slave %d is the empty slave\n", i); *\/ */
//            /*     emptyslave = s; */
//            /* } */
//
//            for(int j = 0; j < MAX_PIDS; j++) {
//                if(s->pid[j] == te->pid) {
//                    goto out;
//                }
//            }
//        }
//    out:
//
//        // Didn't find one, find an empty one
//        if(i == num_slaves) {
//            // No empty slave -- wait for something to happen and try again
//            if(emptyslave == NULL) {
//                // Pick one randomly
//                int randslave = rand() / (RAND_MAX / num_slaves);
//                assert(randslave < num_slaves);
//                s = &slaves[randslave];
//
//                /* printf("no empty slave\n"); */
//                /* err = event_dispatch(get_default_waitset()); */
//                /* assert(err_is_ok(err)); */
//                /* printf("past no empty slave\n"); */
//                /* goto again; */
//            } else {
//                s = emptyslave;
//            }
//        }
//
//        // Assign slave this PID
//        int j;
//        for(j = 0; j < MAX_PIDS; j++) {
//            if(s->pid[j] == 0 || s->pid[j] == te->pid) {
//                break;
//            }
//        }
//        assert(j < MAX_PIDS);
//        s->pid[j] = te->pid;
//
//        /* if(i == num_slaves) { */
//        /*     printf("found empty slave\n"); */
//        /* } else { */
//        /*     printf("found slave %d, PID %d\n", i, s->pid); */
//        /* } */
//
//        /* if(te->fline >= 41352 && te->fline <= 41365) { */
//        /*     printf("%d: %d, %zu, %d, %d, %d to slave %d, fline %d\n", */
//        /*            cnt, te->op, te->u.fnum, te->fd, te->mode, te->pid, i, te->fline); */
//        /* } */
//
//#if 1
//        if(te->op == TOP_Exit) {
//            printf("exit %u\n", te->pid);
//            // See if it was a writer and remove
//            for(struct writer *w = writers; w != NULL; w = w->next) {
//                assert(te != NULL);
//                assert(w != NULL);
//                if(w->pid == te->pid) {
//                    assert(w != NULL);
//                    if(w != writers) {
//                        assert(w != NULL);
//                        assert(w->prev != NULL);
//                        w->prev->next = w->next;
//                    } else {
//                        writers = w->next;
//                    }
//                    free(w);
//                    break;
//                }
//            }
//        }
//#endif
//
//        // If someone opens a file, we have to make sure
//        // that anyone else has stopped writing to that file.
//        if(te->op == TOP_Open || te->op == TOP_Create) {
//            /* for(;;) { */
//                if(printall) {
//                    printf("find writer\n");
//                }
//
//                struct writer *w;
//                for(w = writers; w != NULL; w = w->next) {
//                    assert(w != NULL);
//                    assert(te != NULL);
//                    if(w->fnum == te->u.fnum) {
//                        // Somebody's writing to this file -- wait for him to finish
//                        /* printf("Warning: Concurrent file writer, fline = %d, fnum = %zu\n", */
//                        /*        te->fline, te->u.fnum); */
//                        /* assert(!"NYI"); */
//                        break;
//                    }
//                }
//
//#if 0
//                // There's a writer -- wait for it to finish
//                if(w != NULL) {
//                    printf("Waiting for close from previous writer\n");
//                    err = event_dispatch(get_default_waitset());
//                    assert(err_is_ok(err));
//                } else {
//                    break;
//                }
//#endif
//            }
//
//            // Add a new writer to the list
//            if(te->mode != FLAGS_RdOnly) {
//                struct writer *w = malloc(sizeof(struct writer));
//
//                /* printf("new writer to file %zu\n", te->u.fnum); */
//
//                //                printall = true;
//
//                w->fnum = te->u.fnum;
//                w->pid = te->pid;
//                w->slave = s;
//                w->prev = NULL;
//                w->next = writers;
//                if(writers) {
//                    w->next->prev = w;
//                }
//                writers = w;
//            }
//    /* } */
//
//
//        if(printall) {
//            printf("sending\n");
//        }
//
//        assert(s != NULL);
//        if(s->queue == NULL) {
//#ifndef __linux__
// BARELLFISH -> send request
//#else
//            if(printall) {
//                printf("send_buf 1\n");
//            }
//            ssize_t r = send_buf(s, &er);
//            if(printall) {
//                printf("after send_buf 1\n");
//            }
//            /* ssize_t r = send(s->socket, &er, sizeof(er), MSG_DONTWAIT); */
//            if(r == -1) {
//                if(errno == EAGAIN) {
//                    if(printall) {
//                        printf("queueing\n");
//                    }
//                    /* printf("queueing\n"); */
//                    struct qelem *q = malloc(sizeof(struct qelem));
//                    assert(q != NULL);
//                    q->er = er;
//                    q->next = s->queue;
//                    if(s->queue != NULL) {
//                        s->queue->prev = q;
//                    } else {
//                        assert(s->qend == NULL);
//                    }
//                    q->prev = NULL;
//                    s->queue = q;
//                    if(s->qend == NULL) {
//                        s->qend = q;
//                    }
//                } else {
//                    printf("send_message to %d: %s\n", s->num, strerror(errno));
//                    abort();
//                }
//            } else {
//                if(r != sizeof(er)) {
//                    printf("send_message: r == %zd, size = %zu\n", r, sizeof(er));
//                }
//                assert(r == sizeof(er));
//            }
//#endif
//        } else {
//            // Put on slave's queue
//            if(printall) {
//                printf("queueing\n");
//            }
//            /* printf("queueing\n"); */
//            struct qelem *q = malloc(sizeof(struct qelem));
//            assert(q != NULL);
//            q->er = er;
//            q->next = s->queue;
//            if(s->queue != NULL) {
//                s->queue->prev = q;
//            } else {
//                assert(s->qend == NULL);
//            }
//            q->prev = NULL;
//            s->queue = q;
//            if(s->qend == NULL) {
//                s->qend = q;
//            }
//        }
//
//        if(printall) {
//            printf("resending\n");
//        }
//
//        // Resend items that got queued
//        for(i = 0; i < num_slaves; i++) {
//            s = &slaves[i];
//            for(struct qelem *q = s->qend; q != NULL;) {
//                // Need to keep pumping and dispatch at least one event
//
//#ifndef __linux__
//                err = s->b->tx_vtbl.event(s->b, NOP_CONT, q->er);
//                if(err_is_ok(err)) {
//                    if(printall) {
//                        printf("resent %d\n", q->er.fline);
//                    }
//                    struct qelem *oldq = q;
//                    s->qend = q = q->prev;
//                    free(oldq);
//                    if(s->qend == NULL) {
//                        s->queue = NULL;
//                    }
//                } else if(err_no(err) != FLOUNDER_ERR_TX_BUSY) {
//                    DEBUG_ERR(err, "error");
//                    abort();
//                } else {
//                    // still busy, can't dequeue anything
//                    /* printf("busy2\n"); */
//                    err = event_dispatch(get_default_waitset());
//                    assert(err_is_ok(err));
//                    break;
//                    /* printf("still busy\n"); */
//                    /* qend = q = q->prev; */
//                }
//#else
//                if(printall) {
//                    printf("send_buf 2\n");
//                }
//                ssize_t r = send_buf(s, &q->er);
//                if(printall) {
//                    printf("after send_buf 2\n");
//                }
//                /* ssize_t r = send(s->socket, &q->er, sizeof(q->er), MSG_DONTWAIT); */
//                if(r == -1) {
//                    if(errno == EAGAIN) {
//                        break;
//                    } else {
//                        printf("send_message to %d: %s\n", s->num, strerror(errno));
//                        abort();
//                    }
//                } else {
//                    if(r != sizeof(er)) {
//                        printf("send_message: r == %zd, size = %zu\n", r, sizeof(er));
//                    }
//                    assert(r == sizeof(er));
//                    struct qelem *oldq = q;
//                    s->qend = q = q->prev;
//                    free(oldq);
//                    if(s->qend == NULL) {
//                        s->queue = NULL;
//                    }
//                }
//#endif
//            }
//        }
//    }
//
//    printf("draining\n");
//
//    // Drain the queue
//    for(int i = 0; i < num_slaves; i++) {
//        struct slave *s = &slaves[i];
//        for(struct qelem *q = s->qend; q != NULL;) {
//#ifndef __linux__
//            err = s->b->tx_vtbl.event(s->b, NOP_CONT, q->er);
//            if(err_is_ok(err)) {
//                /* printf("resent %d\n", q->er.fline); */
//                struct qelem *oldq = q;
//                s->qend = q = q->prev;
//                free(oldq);
//                if(s->qend == NULL) {
//                    s->queue = NULL;
//                }
//            } else if(err_no(err) != FLOUNDER_ERR_TX_BUSY) {
//                DEBUG_ERR(err, "error");
//                abort();
//            } else {
//                // still busy, can't dequeue anything
//                break;
//                /* printf("still busy\n"); */
//                /* qend = q = q->prev; */
//            }
//#else
//            ssize_t r = send_buf(s, &q->er);
//            /* ssize_t r = send(s->socket, &q->er, sizeof(q->er), MSG_DONTWAIT); */
//            if(r == -1) {
//                if(errno == EAGAIN) {
//                    break;
//                } else {
//                    printf("send_message to %d: %s\n", s->num, strerror(errno));
//                    abort();
//                }
//            } else {
//                if(r != sizeof(q->er)) {
//                    printf("send_message: r == %zd, size = %zu\n", r, sizeof(q->er));
//                }
//                assert(r == sizeof(q->er));
//                struct qelem *oldq = q;
//                s->qend = q = q->prev;
//                free(oldq);
//                if(s->qend == NULL) {
//                    s->queue = NULL;
//                }
//            }
//#endif
//        }
//    }
//
//    for(int i = 0; i < num_slaves; i++) {
//        struct slave *s = &slaves[i];
//        replay_eventrec_t er = {
//            .op = TOP_End
//        };
//#ifndef __linux__
//        err = s->b->tx_vtbl.event(s->b, NOP_CONT, er);
//        assert(err_is_ok(err));
//#else
//        ssize_t r = send_buf(s, &er);
//        if(r == -1) {
//            if(errno == EAGAIN) {
//                printf("buffer full\n");
//                abort();
//            } else {
//                printf("send_message to %d: %s\n", s->num, strerror(errno));
//                abort();
//            }
//        }
//#endif
//    }
//
//    do {
//        err = event_dispatch(get_default_waitset());
//        assert(err_is_ok(err));
//    } while(num_finished < num_slaves);
//
//    uint64_t end = rdtsc();
//
//#if 0
//    // Wait for 5 seconds
//    uint64_t beg = rdtsc();
//    while(rdtsc() - beg < tscperms * 5000) {
//#ifndef __linux__
//        thread_yield();
//#else
//        sched_yield();
//#endif
//    }
//#endif
//
//    printf("replay done, took %" PRIu64" ms\n", (end - start) / tscperms);
//
//
