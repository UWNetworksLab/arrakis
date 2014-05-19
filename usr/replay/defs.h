#ifndef DEFS_H
#define DEFS_H

#define MAX_PIDS        64
#define TOTAL_PIDS      3000

enum top {
    TOP_Open   = 0,
    TOP_Create = 1,
    TOP_Unlink = 2,
    TOP_Read   = 3,
    TOP_Write  = 4,
    TOP_Close  = 5,
    TOP_Exit   = 6,
    TOP_Seek   = 7,
    TOPs_Total,
};

static char __attribute__((unused)) *top2str[] = {
   [TOP_Open  ] = "open",
   [TOP_Create] = "create",
   [TOP_Unlink] = "unlink",
   [TOP_Read  ] = "read",
   [TOP_Write ] = "write",
   [TOP_Close ] = "close",
   [TOP_Exit  ] = "exit",
   [TOP_Seek  ] = "seek",
   [TOPs_Total] = "___YOU_SHOULD_NOT_SEE_THIS___"
};

enum flags {
    FLAGS_RdOnly,
    FLAGS_WrOnly,
    FLAGS_RdWr,
};

typedef struct {
    uint32_t fnumsize; /* for Open/Create/Unlink -> fnum, for Read/Write/Seek -> offset */
    uint16_t pid;
    uint8_t op;
    uint8_t fd;
    uint8_t mode;
    /* uint32 fline; */
} replay_eventrec_t;

struct trace_entry {
    enum top op;
    union {
        size_t fnum;
        size_t size;
    } u;
    int fd;
    enum flags mode;
    int pid;
    int fline;

    struct trace_entry *next;
};

#if 0 //__linux__
struct _replay_eventrec__struct {
    uint8_t op;
    uint32_t fnumsize;
    uint8_t fd;
    uint8_t mode;
    uint32_t fline;
    uint16_t pid;
};
typedef struct _replay_eventrec__struct replay_eventrec_t;
#endif

#define dbg_print_str__ ">>>>>>>>>>>>>> %s() [%s +%d]"
#define dbg_print_arg__ __FUNCTION__, __FILE__, __LINE__
#define dbg_print(msg ,fmt, args...)\
    printf(dbg_print_str__ " " msg fmt , dbg_print_arg__ , ##args)

//#define XDEBUG
#define msg(fmt,args...)      dbg_print("msg:",fmt, ##args)
#ifdef XDEBUG
    #define dmsg(fmt,args...) dbg_print("dbg:",fmt, ##args)
#else
    #define dmsg(fmt,args...) do { } while (0)
#endif


#endif
