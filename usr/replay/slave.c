#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>
#ifndef __linux__
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>
#include <barrelfish/nameservice_client.h>
#include <if/replay_defs.h>
#include <barrelfish/bulk_transfer.h>
#else
#include <stdbool.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/ip.h>
#include <netdb.h>
#include <errno.h>
#include <sched.h>
#include <inttypes.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif

#include "defs.h"

#define MIN(x,y) (x < y ? x : y)

static char *defdir;
static struct {
    uint64_t op_ticks[TOPs_Total];
    uint64_t op_count[TOPs_Total];
    uint64_t total_ticks;
} Stats = {{0}, {0}, 0};

//static uint64_t total_ticks=0, open_ticks=0, read_ticks=0, unlink_ticks=0;

#ifndef __linux__
static void export_cb(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));
    char name[256];
    snprintf(name, 256, "replay_slave.%u", disp_get_core_id());
    msg("%s:%s() :: === registering %s\n", __FILE__, __FUNCTION__, name);
    err = nameservice_register(name, iref);
    assert(err_is_ok(err));
}
#else
static int connsock = -1;
#endif

#define MAX_FD_CONV     256
#define MAX_DATA        (2*1024*1024)
#define WBUF_SIZE       (16*1024*1024)

struct wbuf;

struct wbuf_entry {
    struct wbuf *next, *prev;
};

typedef struct wbuf {
    int           w_fd;
    size_t        w_i;
    unsigned char w_buf[WBUF_SIZE];
    struct wbuf   *next, *prev;
} wbuf_t;

static struct {
    struct wbuf *head;
    int cnt;
} Wbufs = {
    .head= NULL,
    .cnt = 0
};

static void
wb_add(wbuf_t *wb)
{
    if (Wbufs.head == NULL) {
        assert(Wbufs.cnt == 0);
        Wbufs.head = wb->next = wb->prev = wb;
    } else {
        struct wbuf *head = Wbufs.head;
        wb->next = head;
        wb->prev = head->prev;
        head->prev->next = wb;
        head->prev = wb;
    }
    Wbufs.cnt++;
}

static void
wb_remove(wbuf_t *wb)
{
    if (Wbufs.cnt == 1) {
        Wbufs.head = NULL;
    } else {
        wb->next->prev = wb->prev;
        wb->prev->next = wb->next;
    }
    Wbufs.cnt--;
}

static int openfiles = 0, read_fails=0, write_fails=0, seek_fails=0;
static int tfd2fd[MAX_FD_CONV] = {0};    /* trace fd -> fd */
static int tfd2fname[MAX_FD_CONV] = {0}; /* trace fd -> name */
wbuf_t    *tfd2wb[MAX_FD_CONV] = {0};    /* trace fd -> write buffer */
static char data[MAX_DATA];

#ifndef __linux__
static struct bulk_transfer_slave bulk_slave;
static uint64_t tscperms;
#endif

#ifdef __linux__
static int
disp_get_core_id(void)
{
    return getpid();
}
static inline uint64_t rdtsc(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
}
#endif

#ifndef __linux__
static void handle_init(struct replay_binding *b, struct capref shared_mem, uint32_t size)
{
    errval_t err;
    vregion_flags_t vspace_fl;

    #ifdef __scc__
    vspace_fl = VREGION_FLAGS_READ_WRITE_MPB;
    #else
    vspace_fl = VREGION_FLAGS_READ_WRITE;
    #endif

    // Map the frame in local memory
    void *pool;
    err = vspace_map_one_frame_attr(&pool, size, shared_mem, vspace_fl, NULL, NULL);
    assert(pool != NULL);
    assert(err_is_ok(err));

    // Init receiver
    err = bulk_slave_init(pool, size, &bulk_slave);
    assert(err_is_ok(err));
    msg("%s:%s: done\n", __FILE__, __FUNCTION__);

    err = b->tx_vtbl.slave_init_reply(b, NOP_CONT);
    assert(err_is_ok(err));
}

static void handle_finish(struct replay_binding *b)
{
    errval_t err;
    err = b->tx_vtbl.slave_finish_reply(b, NOP_CONT);
    assert(err_is_ok(err));
}

void cache_print_stats(void);
static void handle_print_stats(struct replay_binding *b)
{
    errval_t err;
    msg("SLAVE[%u]: END took %" PRIu64 " ticks (%lf ms)\n", disp_get_core_id(), Stats.total_ticks, (double)Stats.total_ticks/(double)tscperms);
    for (int i=0; i<TOPs_Total; i++) {
        uint64_t op_cnt = Stats.op_count[i];
        double op_time = (double)Stats.op_ticks[i]/(double)tscperms;
        msg(" op:%-10s cnt:%8" PRIu64  " time:%13.2lf avg:%9.3lf\n", top2str[i], op_cnt, op_time, op_time/(double)op_cnt);
    }
    msg("SLAVE[%u]: CACHE STATISTICS\n", disp_get_core_id());
    cache_print_stats();
    err = b->tx_vtbl.slave_print_stats_reply(b, NOP_CONT);
    assert(err_is_ok(err));
}
#endif

static void
do_handle_event(replay_eventrec_t *er)
{
    static int pid = 0;
    static int op_id = 0;
    const int open_mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;

    /* pick a file for this operation */
    // (only needed for Open/Create/Unlink)
    char fname[256];
    snprintf(fname, 256, "%s/data/%u", defdir, er->fnumsize);


    op_id++;
    enum top op = er->op;
    dmsg("SLAVE[%u]: REQ pid:%d op:%d fd:%d fnumsize:%d [op_id:%d] er:%p\n", disp_get_core_id(), er->pid, op, er->fd, er->fnumsize, op_id, er);

    /* - master will send consecutive operations with the same pid
     * - the pid will change after an TOP_Exit, and a subsequent Open/Create */
    if (pid == 0) {
        assert(er->op == TOP_Open || er->op == TOP_Create);
        pid = er->pid;
        dmsg("SLAVE[%u]: got new pid:%d\n", disp_get_core_id(), pid);
        op_id = 0;
    } else if (er->pid != pid) {
        printf("REQ does not match current pid (%u). Aborting\n", pid);
        assert(0);
    } else if (op == TOP_Exit) {
        pid = 0;
    }

    int open_flags = 0;

    switch(op) {
    case TOP_Create:
        open_flags = O_CREAT;
    /* fallthrough */
    case TOP_Open:
        /* set flags */
        switch(er->mode) {
        case FLAGS_RdOnly:
            open_flags |= O_RDONLY;
            break;

        case FLAGS_WrOnly:
            open_flags |= O_WRONLY;
            break;

        case FLAGS_RdWr:
            open_flags |= O_RDWR;
            break;
        }

        /* allocate a write buffer */
        if (open_flags != O_RDONLY) {
            wbuf_t *wb = tfd2wb[er->fd] = malloc(sizeof(wbuf_t));
            assert(wb);
            wb->w_i = 0;
            wb->w_fd = er->fd;
            wb_add(wb);
        }

        /* assert(fd2fptr[er->fd] == NULL);
         * the above assertion will fail, because some close() operations are
         * missing from the file:
         *  $ egrep -c -e 'close' <kernelcompile.trace.anon
         *  10779
         *  $ egrep -c -e '(open|creat)' <kernelcompile.trace.anon
         *  10974  */

        /* open the file */
        tfd2fd[er->fd] = open(fname, open_flags, open_mode);
        tfd2fname[er->fd] = er->fnumsize;
        if (tfd2fd[er->fd] != -1) {
            openfiles++;
        } else {
            printf("Open file:%s (%d) failed\n", fname, open_flags);
            perror(fname);
            assert(0);
        }
        break;

    case TOP_Unlink: {
        int ret = unlink(fname);
        assert(ret != -1);
        break;
    }

    case TOP_Read: {
        //uint64_t ticks = rdtsc();
        if (er->fnumsize > MAX_DATA) {
            printf("er->fnumsize == %u\n", er->fnumsize);
            assert(0);
        }
        int fd = tfd2fd[er->fd];
        int ret = read(fd, data, er->fnumsize);
        if (ret != er->fnumsize) {
            dmsg("[R] op_id:%d er->fnumsize:%u, read:%d fname:%d pid:%d error:%d eof:%d pos:%ld\n", op_id, er->fnumsize, ret, fd2fname[er->fd], er->pid, ferror(fptr), feof(fptr), ftell(fptr));
            read_fails++;
        }
        //ticks = rdtsc() - ticks;
        //msg("SLAVE[%d] READ %d took %lu ticks (%lf ms)\n", disp_get_core_id(), rdcnt++, ticks, (double)ticks/(double)tscperms);
        break;
    }

    case TOP_Write: {
        if (er->fnumsize > MAX_DATA) {
            printf("er->fnumsize == %u\n", er->fnumsize);
            assert(0);
        }
        wbuf_t *wb = tfd2wb[er->fd];
        int fd = tfd2fd[er->fd];
        assert(wb);
        size_t rem_bytes = er->fnumsize; /* remaining bytes to write */
        for (;;) {
            size_t buff_bytes = WBUF_SIZE - wb->w_i; /* wbuf available bytes */
            if (buff_bytes > rem_bytes) {
                memcpy(wb->w_buf + wb->w_i, data, rem_bytes);
                wb->w_i += rem_bytes;
                break;
            } else {
                memcpy(wb->w_buf + wb->w_i, data, buff_bytes);
                int ret = write(fd, wb->w_buf, WBUF_SIZE);
                if (ret != WBUF_SIZE) {
                    write_fails++;
                }
                rem_bytes -= buff_bytes;
                wb->w_i = 0;
            }
        }
        break;
    }

    case TOP_Seek: {
        int fd = tfd2fd[er->fd];
        int ret = lseek(fd, er->fnumsize, SEEK_SET);
        if (ret != er->fnumsize) {
            seek_fails++;
            //msg("[S] op_id:%d er->fnumsize:%u, seek:%d fname:%d pid:%d error:%d eof:%d pos:%ld\n", op_id, er->fnumsize, ret, fd2fname[er->fd], er->pid, ferror(fptr), feof(fptr), ftell(fptr));
        }
        break;
    }

    case TOP_Close: {
        wbuf_t *wb = tfd2wb[er->fd];
        int fd = tfd2fd[er->fd];
        if (wb != NULL) {
            /* flush write buffer */
            int r = write(fd, wb->w_buf, wb->w_i);
            if (r != wb->w_i) {
                write_fails++;
            }
            wb_remove(wb);
            free(wb);
            tfd2wb[er->fd] = NULL;
        }
        int ret = close(fd);
        assert(ret == 0);
        openfiles--;
        tfd2fd[er->fd] = 0;
        tfd2fname[er->fd] = 0;
        break;
    }

    case TOP_Exit: {
        wbuf_t *wb;
        wb = Wbufs.head;
        for (int i=0; i<Wbufs.cnt; i++) {
            wbuf_t *wb_tmp = wb->next;
            int fd = tfd2fd[wb->w_fd];
            int r = write(fd, wb->w_buf, wb->w_i);
            if (r != wb->w_i) {
                write_fails++;
            }
            tfd2wb[wb->w_fd] = 0;
            free(wb);
            wb = wb_tmp;
        }
        Wbufs.cnt  = 0;
        Wbufs.head = NULL;
        dmsg("SLAVE[%u]: TOP_Exit on %d\n", disp_get_core_id(), er->pid);
        break;
    }

    default:
        printf("Invalid request: %d\n", op);
        assert(0);
        break;
    }
}

static void
handle_event(replay_eventrec_t *er)
{
    uint64_t handle_ticks = rdtsc();
    enum top op = er->op;

    do_handle_event(er);

    /* update stats */
    handle_ticks = (rdtsc() - handle_ticks);
    Stats.total_ticks += handle_ticks;
    Stats.op_count[op]++;
    Stats.op_ticks[op] += handle_ticks;
}
#ifndef __linux__

static void handle_new_task(struct replay_binding *b, uint64_t bulk_id, uint32_t tes_size)
{
    errval_t err;
    replay_eventrec_t *tes;
    size_t tes_nr;
    int pid;

    tes = bulk_slave_buf_get_mem(&bulk_slave, bulk_id, NULL);
    tes_nr = tes_size / sizeof(replay_eventrec_t);
    assert(tes_size % sizeof(replay_eventrec_t) == 0);

    if (tes->op != TOP_Open && tes->op != TOP_Create) {
        msg("ABORTING: task with pid:%d starts with op:%d\n", tes->pid, tes->op);
        assert(false);
    }
    pid = tes->pid;
    for (size_t i=0; i<tes_nr; i++) {
        replay_eventrec_t *er = tes + i;
        handle_event(er);
    }

    err = b->tx_vtbl.task_completed(b, NOP_CONT, pid, bulk_id);
    assert(err_is_ok(err));
}


static struct replay_rx_vtbl replay_vtbl = {
    .new_task          = handle_new_task,
    .slave_init        = handle_init,
    .slave_finish      = handle_finish,
    .slave_print_stats = handle_print_stats
};

static errval_t connect_cb(void *st, struct replay_binding *b)
{
    b->rx_vtbl = replay_vtbl;
    return SYS_ERR_OK;
}
#endif

int main(int argc, char *argv[])
{
#ifndef __linux__
    assert(argc >= 4);
#else
    const int default_port = 1234;
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <data_dir> [port (default:%d)]\n", argv[0], default_port);
        exit(1);
    }
#endif
    defdir = argv[1];

    msg("===================> replay slave up\n");
#ifndef __linux__
    assert(err_is_ok(sys_debug_get_tsc_per_ms(&tscperms)));

    errval_t err = vfs_mkdir(argv[2]);
    if(err_is_fail(err) && err_no(err) != FS_ERR_EXISTS) {
        DEBUG_ERR(err, "vfs_mkdir");
    }
    /* assert(err_is_ok(err)); */

    err = vfs_mount(argv[2], argv[3]);
    assert(err_is_ok(err));

    err = replay_export(NULL, export_cb, connect_cb,
                        get_default_waitset(),
                        IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(err));

    msg("%s:%s() :: slave starts servicing requests\n", __FILE__, __FUNCTION__);
    for(;;) {
        err = event_dispatch(get_default_waitset());
        assert(err_is_ok(err));
    }
#else
    /* { */
    /*     struct rlimit rl; */
    /*     rl.rlim_cur = 2048; */
    /*     rl.rlim_max = 2050; */
    /*     int r = setrlimit(RLIMIT_NOFILE, &rl); */
    /*     if(r == -1) { */
    /*         printf("setrlimit errno = %s\n", strerror(errno)); */
    /*     } */
    /*     assert(r == 0); */
    /* } */

    int port = argc > 2 ? atoi(argv[2]) : default_port;
    // Listen on port 1234
    int listensock = socket(AF_INET, SOCK_STREAM, 0);
    assert(listensock != -1);
    struct sockaddr_in a = {
        .sin_family = PF_INET,
        .sin_port = htons(port),
        .sin_addr = {
            .s_addr = htonl(INADDR_ANY)
        }
    };
    int r = bind(listensock, (struct sockaddr *)&a, sizeof(a));
    if(r == -1) {
        printf("bind: %s\n", strerror(errno));
    }
    assert(r == 0);
    r = listen(listensock, 5);
    assert(r == 0);

    socklen_t sizea = sizeof(a);
    connsock = accept(listensock, (struct sockaddr *)&a, &sizea);
    assert(connsock != -1);
    assert(sizea == sizeof(a));

    int from = (ntohl(a.sin_addr.s_addr) & 0xff) - 1;
    printf("got connection from %d\n", from);


    /* circular buffer:
     *  - writer writes at byte grain
     *  - reader reads at sizeof(replay_eventrec_t) grain */
    const size_t er_size_elems = 1024;   /* size in elements */
    replay_eventrec_t ers[er_size_elems];
    const size_t er_size = sizeof(ers);   /* size in bytes */
    uint64_t er_r, er_w;                  /* full indices (in bytes) */

    er_r = er_w = 0;

    for(;;) {

        dmsg("r:%zd w:%zd er_size:%zd\n", er_r, er_w, er_size);
        size_t er_avail = er_size - (er_w - er_r);
        size_t er_w_idx = er_w % er_size;
        char *xfer_start = (char *)ers + er_w_idx;
        size_t xfer_len = MIN(er_avail, er_size - er_w_idx);

        if (xfer_len == 0) {
            continue;
        }

        /* fetch events */
        dmsg("RECV: from:%lu len:%zd\n", xfer_start - (char  *)ers, xfer_len);
        ssize_t ret = recv(connsock, xfer_start, xfer_len, 0);
        if(ret == -1) {
            perror("recv");
            exit(1);
        } else if (ret == 0) {
            printf("end of session\n");
            break;
        }
        dmsg("GOT DATA=%zd!\n", ret);
        er_w += ret;

        /* handle events */
        assert(er_r % sizeof(replay_eventrec_t) == 0);
        assert(er_w > er_r);
        while (er_w - er_r >= sizeof(replay_eventrec_t)) {
            size_t er_r_items = er_r / sizeof(replay_eventrec_t);
            replay_eventrec_t *er = ers + (er_r_items % er_size_elems);

            handle_event(er);

            // notify server that we are done with a task
            if (er->op == TOP_Exit) {
                uint16_t pid = er->pid;
                dmsg("SENDING PID: %d\n", pid);
                if (send(connsock, &pid, sizeof(pid), 0) != sizeof(pid)) {
                    perror("send");
                    exit(1);
                }
            }

            // increase read pointer
            er_r += sizeof(replay_eventrec_t);
        }
    }
#endif

    return 0;
}
