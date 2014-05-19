/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * ECLiPSe C SOURCE MODULE
 */

/*
 * IDENTIFICATION	emu_par.c
 *
 * AUTHOR		Joachim Schimpf
 *
 * DESCRIPTION		Engine functions related to parallelism
 *
 *			eng_...() functions are called by the scheduler
 *
 */

#include "config.h"
#include "sepia.h"
#include "pds.h"      /* pds.h has to be placed before types.h*/
#include "types.h"
#include "embed.h"
#include "error.h"
#include "io.h"
#include "mem.h"
#include "dict.h"
#include "emu_export.h"
#include "opcode.h"
#include "wm.h"

typedef void *eng_handle_t;	/* Dummy type for struct machine */

#include "sch_eng_interface.h"
#include <sys/time.h>
#include "trace.h"

#define COPY_LOCAL_STACK_COMPLETELY	1

#define Set_Load(load) \
	if (LOAD < 0) { \
	    Disable_Int(); \
	    EVENT_FLAGS &= ~COUNT_DOWN; \
	    Enable_Int(); \
	} LOAD = load;

#define WStat_Incr(item)	wstat_.item++
#define WStat_Add(item,size)	wstat_.item += size

/*
 * Declarations
 */

extern vmcode		fail_code_[];
extern vmcode		par_fail_code_[];
extern vmcode		*wb_fail_code_;
extern vmcode		*do_idle_code_;
extern vmcode		*idle_ret_code_;

extern aport_id_t	aports[];		/* this process' aports */
extern aport_id_t	wm_high_aport_id;


#ifdef __STDC__
void	short_sleep(int);
#else
void	short_sleep();
#endif

/*
 * Message passing
 */

#define ECLIPSE_IO_INTERFACE	1027

struct io_msg {
	aport_id_t reply_aport;		/* MDT_APORTID */
	stream_id nst;			/* MDT_ADDRESS */
	int action;			/* MDT_INT32 */
};

static amsg_typedef_t mdt_iorequest[] = {
	MDT_BEGIN,
	MDT_STRUCT_OPEN,
	    MDT_APORTID, MDT_ADDRESS, MDT_INT32,
	MDT_STRUCT_CLOSE,
	MDT_END };

static amsg_type_t MDT_IOREQUEST;

struct io_reply_msg {
	int err_code;			/* MDT_INT32 */
};

/*
 * Global variables, private to this worker
 */

/* The message that contains the currently followed oracle.
 * This variable is used to free the message at the end of the oracle.  */
static amsg_t		oracle_msg = (amsg_t) 0;

/* The message that contains the current parallel goal (par_findall etc) */
amsg_t			par_goal_msg_ = (amsg_t) 0;

/* Where to send the engine_migrate message when recomputation is done */
static st_handle_t	dest_branch;

/*
 * This variable is just used to pass the root node to where the
 * first choicepoint is created. Temporary.
 */
st_handle_t eng_root_branch;

static int copy_count_;		/* debugging */


/*---------------------------------------------
 * Testing
 *---------------------------------------------*/

#define Notify(text) { if (GlobalFlags & ENG_TRACE_FLAG) {\
	p_fprintf(current_err_, text); ec_flush(current_err_); }}


/*---------------------------------------------
 * Initialisation
 *---------------------------------------------*/

int
par_present(void)
{
    return(1);	/* parallelism extensions are present in this kernel */
}


/*ARGSUSED*/
void
parallel_init(int flags)
{
    st_handle_t *theleaf, eng_root_node;
    int first = ec_options.parallel_worker==1;

    (void) amsg_type_define(ECLIPSE_IO_INTERFACE, 1, mdt_iorequest,
					&MDT_IOREQUEST);

    sch_create_leaf(aports[SCH_APORT_NUMBER], (eng_handle_t) 0, &theleaf);
    if (first)
        sch_create_root(aports[SCH_APORT_NUMBER], &eng_root_node);
    else 
        eng_root_node = *get_root_id(theleaf);

    sch_genesis(&eng_root_node, theleaf, &eng_root_branch, first);

    LEAF = theleaf;	/* engine = engine_init(&eng_root_branch, theleaf); */
    if (first)
        root_node_register(wm_high_aport_id, &eng_root_node);
}


/*ARGSUSED*/
void
worker_init(int flags)	/* after restoring saved state */
{
    if (flags & INIT_SHARED)
    {
	LoadReleaseDelay = -5;
	PublishingParam = 0;
    }
    copy_count_ = 0;
    wstat_init();
    LOG_INIT(ec_options.parallel_worker > 1 ? IDLE : WORKING);
}


par_chp_statistics(int n)
{
    WStat_Incr(parallel_chpts);
    WStat_Add(parallel_alts, n);
}


/*---------------------------------------------
 * Parallel choicepoint support
 *---------------------------------------------*/

st_handle_t nil_node = {0, 0, 0};

/*
 * After setting B to a new value (scheduler-imposed backtrack
 * or stack copy) this routine adjusts the engine state accordingly.
 * This is necessary (and cannot be left to the subsequently executed
 * engine instructions) because we may receive a state donation request
 * before having had a chance to return to the engine.
 * No need to install argument registers and other stuff here,
 * that can be left to Try_clause.
 * A parallel choicepoint is on top of the control stack now.
 */

void
_adjust_engine(void)
{
    pword **old_tt, *pw2 = BPrev(B.args);
    if (TO) {
	TO = BOracle(B.args);
	O_Set_Alt(TO, ChpPar(pw2)->alt);
	O_Reset_Try_Count(TO);
    }
    SP = EB = Chp(pw2)->sp;
    TG = GB = Chp(pw2)->tg;
    LD = Chp(pw2)->ld;
    E = Chp(pw2)->e;
    Push_Witness;
    Adjust_GcTg_and_TgSl(TG);
    old_tt = ChpPar(pw2)->tt;
    Untrail_Variables(old_tt);
}


/* 
 * int eng_publish(m, max, &remaining_load)
 *
 * Publish at most max parallel choicepoints.
 * Returns the remaining unpublished load in remaining_load.
 * Returns the number of published alternatives as return value.
 * 
 * We publish from older to newer, required by the scheduler!
 * Therefore we first build a reversed list in pb, then process it.
 */
/*ARGSUSED*/
int
eng_publish(eng_handle_t m, int max, int *remaining_load)
{
    pword *pb = 0;
    pword *b_aux;
    int alternatives = 0;
    int published = 0;
    int choicepoints = 0;

    if (g_emu_.nesting_level > 1)
    {
	p_fprintf(current_err_, "ERROR: eng_publish with level > 1\n");
	ec_flush(current_err_);
	*remaining_load = LOAD;
	return 0;
    }

    Notify("Publishing\n");
    WStat_Incr(publish_count);

    if (LOAD <= 0)		/* no load or not yet released */
    {
	*remaining_load = 0;
	return 0;
    }

    if (PublishingParam > 0)
	max = PublishingParam;

    /* collect the parallel frames */
    /* could be optimised if PB_MAINTAINED */
    for (b_aux = B.args; b_aux > PPB; b_aux = BPrev(b_aux))
    {
	if (IsUnpubParFrame(BTop(b_aux)))
	{
	    BPar(b_aux)->ppb = pb;
	    choicepoints += 1;
	    alternatives += BPar(b_aux)->alt - 1;
	    pb = b_aux;
	}
    }

    while (pb && max)		/* now publish bottom-up */
    {
	pword *nextpb;
	int hybrid;
	int alts;
	alts = (BBp(pb) == wb_fail_code_) ? -1 : BPar(pb)->alt-1;
	/*
	 * alts >  0 : normal parallel choicepoint, not exhausted
	 * alts =  0 : a par_true/0 choicepoint
	 * alts = -1 : a worker_boundary/0 choicepoint
	 */
#ifdef PROLOG_SCHED
#else /* PROLOG_SCHED */
	hybrid = (BPrev(pb) != PPB) ? 1 : 0;
	sch_load_publish_one(LEAF, alts, &BPar(pb)->node, hybrid);
#endif /* PROLOG_SCHED */
	BBp(pb) += 2;		/*  skip Retry_seq */
	nextpb = BPar(pb)->ppb;
	BPar(pb)->ppb = PPB;
	if (alts > 0)
	{
	    WStat_Add(published_alts, alts);
	    published += alts;
	    max -= 1;
	}
	choicepoints -= 1;
	WStat_Incr(published_chpts);
	PPB = pb;
	pb = nextpb;
    }
    /* reconstruct the rest of the chain if PB_MAINTAINED */
    Set_Load(choicepoints);
    *remaining_load = alternatives - published;
    return published;
}


/*ARGSUSED*/
void
eng_undo_publish(eng_handle_t m, const st_handle_t *until)
{
    pword *pb = PPB;

    Notify("Unpublishing\n");
    while (!ComnNode(&BPar(pb)->node, until))
    {
	if (IsRecursionFrame(BTop(pb)))
	{
	    p_fprintf(current_err_, "ERROR: eng_undo_publish()\n");
	    ec_flush(current_err_);
	    PPB = pb;
	    return;
	}
	BPar(pb)->node = nil_node;
	BBp(pb) = par_fail_code_;
	pb = BPar(pb)->ppb;
    }
    BPar(pb)->node = *until;	/* same node but different handle */
#ifdef PB_MAINTAINED
    PB =
#endif
    PPB = pb;
}

/*ARGSUSED*/
void
eng_fail(eng_handle_t m)
{
    Notify("Got failure\n");
    PP = fail_code_;
}

/*ARGSUSED*/
void
eng_stop(eng_handle_t m)
{
    Notify("Pruned - Waiting for a job\n");
    PP = do_idle_code_;
    Set_Load(0);
    WStat_Incr(prune_count);
}


/*ARGSUSED*/
void
eng_msg_trigger(eng_handle_t m)
{
    Disable_Int();
    EVENT_FLAGS |= SCH_MSG_PENDING;
    Enable_Int();
    if (g_emu_.nesting_level == 1)
    {
	Fake_Overflow;
    }
    /* else Fake_Overflow will be done later in re_fake_overflow() */
}

/*ARGSUSED*/
void
eng_msg_reset(eng_handle_t m)
{
    Disable_Int();
    EVENT_FLAGS &= ~SCH_MSG_PENDING;
    Enable_Int();
}


/*
 * All engine and leaf messages are received by this procedure.
 * Note that a message handler may install new stacks or a new PP!
 *
 * We switch here to polling for intra-domain (shared memory) messages
 * on order to reduce signaling overhead. This is essential for performance!
 * 
 * If called from emulator:
 *	(EVENT_FLAGS & SYNC_MSG_PENDING) && (PP != do_idle_code_)
 * If called from get_job() or cut_public():
 *	(PP == do_idle_code_)
 */

#define MAX_POLLS	100000

void
eng_msg_loop(void)		/* nesting_level == 1 */
{
    unsigned volatile polling = 0;

    if (EVENT_FLAGS & SYNC_MSG_PENDING)
    {
_handle_sync_:
	do
	{
	    if (EVENT_FLAGS & ENG_MSG_PENDING)
	      {
		eng_sync_msg_handler();
	      }
	    if (EVENT_FLAGS & SCH_MSG_PENDING)
	      {
		LOG_EVENT(SCHEDULING);
		sch_sync_msg_hdls(LEAF);
	      }
	} while (EVENT_FLAGS & SYNC_MSG_PENDING);
    }
    if (PP != do_idle_code_)
    {
	/* return to engine as soon as possible */
	if (polling)
	{
	    (void) bmsg_set_option(BMSG_INTRA_DOMAIN_TRIGGERING, BMSG_ON);
	}
	LOG_EVENT(PO?ENG_RECOMPUTING:WORKING);
	return;
    }
    LOG_EVENT(IDLE);
    do
    {
	if (!polling)
	{
	    (void) bmsg_set_option(BMSG_INTRA_DOMAIN_TRIGGERING, BMSG_OFF);
	}
	polling = MAX_POLLS;
	do
	{
	    (void) bmsg_trigger(BMSG_INTRA_DOMAIN);
	    if (EVENT_FLAGS & SYNC_MSG_PENDING)
	    {
		WStat_Incr(polls_succeeded);
		goto _handle_sync_;
	    }
	} while (--polling > 0);
	WStat_Incr(polls_expired);

	(void) bmsg_set_option(BMSG_INTRA_DOMAIN_TRIGGERING, BMSG_ON);

	short_sleep(60000000);	/* normally terminated by signal */

    } while (!(EVENT_FLAGS & SYNC_MSG_PENDING));
    goto _handle_sync_;
}


void
msg_nopoll(void)
{
    if (ec_options.parallel_worker)
	(void) bmsg_set_option(BMSG_INTRA_DOMAIN_TRIGGERING, BMSG_ON);
}


/*ARGSUSED*/
void
eng_backtrack(eng_handle_t m, const st_handle_t *until, unsigned int alt)
{
    pword *pb;

    for (pb = PPB; !ComnNode(&BPar(pb)->node, until); pb = BPar(pb)->ppb)
    {}

    BPar(pb)->node = *until;	/* same node but different handle */
#ifdef PB_MAINTAINED
    PB =
#endif
    PPB = B.args = pb;

    if (alt)			/* trigger engine to continue */
    {
	Notify("Got a job\n");
	WStat_Incr(job_count);
	PP = BBp(pb) + 2;	/* skip Fail_clause, set to Try_clause */
	BPar(pb)->alt = alt;
    }
    _adjust_engine();
}

/*
 * Trust alternative alt of topmost published choicepoint
 */
/*ARGSUSED*/
void
eng_trust(eng_handle_t m, unsigned int alt)
{
    pword *pb;

    Notify("Got a job\n");
    WStat_Incr(job_count);
    B.args = pb = PPB;
#ifdef PB_MAINTAINED
    PB =
#endif
    PPB = BPar(pb)->ppb;	/* this will tell Try_clause to pop it */
    PP = BBp(pb) + 2;		/* skip Fail_clause, set to Try_clause */
    BPar(pb)->alt = alt;
    BBp(pb) = par_fail_code_;	/* not really needed */
    BPar(pb)->node = nil_node;	/* not really needed */
    _adjust_engine();
}


/*
 * Called from the emulator when the cut affects a published choicepoint.
 * Returns true when the cut succeeds.
 * The cut has already been done in the engine, PPB has its new value.
 */
int
cut_public(void)	/* PPB */
{
    vmcode *cont_pp;

    if (g_emu_.nesting_level > 1)
    {
	p_fprintf(current_err_, "ERROR: cut_public with level > 1\n");
	ec_flush(current_err_);
	return 1;
    }
    Set_Load(0);
    cont_pp = PP;
    PP = do_idle_code_;
    Notify("Public cut\n");
#ifdef PROLOG_SCHED
#else /* PROLOG_SCHED */
    LOG_EVENT(SCH_CUT);
    sch_cut(LEAF, &BPar(PPB)->node);
#endif /* PROLOG_SCHED */
    eng_msg_loop();
    if (PP != idle_ret_code_)
    {
	Notify("Got new job in cut\n");
	return 0;	/* cut unsuccessful, other job instead */ 
    }
    Notify("Public cut successful\n");
    PP = cont_pp;	/* cut successful, continue */
    WStat_Incr(cut_count);
    return 1;
}


/*ARGSUSED*/
void
eng_cut_ok(eng_handle_t m, const st_handle_t *cut_parent)
{
    Notify("Got cut ok\n");
    BPar(PPB)->node = *cut_parent;	/* same node but different handle */
    if (PP != do_idle_code_)
    {
	p_fprintf(current_err_, "ERROR: eng_cut_ok not in idle state\n");
	ec_flush(current_err_);
	return;
    }
    PP = idle_ret_code_;
}


/*
 * This is called from the emulator in a clean state (failed to a // chp)
 */

void
get_job(void)
{
    if (g_emu_.nesting_level > 1)
    {
	p_fprintf(current_err_, "ERROR: get_job with level > 1\n");
	ec_flush(current_err_);
	BPar(B.args)->alt -= 1;
	return;
    }

    PP = do_idle_code_;
    LOG_EVENT(SCH_BACKTRACK);
    Notify("Backtrack - Waiting for job\n");
    sch_backtrack(LEAF);
    eng_msg_loop();
}

void
end_of_oracle(void)
{
    pword *pb1, *pb2;

    /*
     * Oracle following does not link the published frames and
     * does not set PPB. We do it here.
     */
    for (pb1 = B.args, pb2 = (pword *) 0; pb1 > PPB; pb1 = BPrev(pb1))
    {
	if (IsUnpubParFrame(BTop(pb1)))
	{
	    BPar(pb1)->ppb = pb2;
	    pb2 = pb1;
	}
    }
    while (pb2)
    {
	pb1 = BPar(pb2)->ppb;
	BPar(pb2)->ppb = PPB;
	BBp(pb2) += 2;		/*  skip Retry_seq */
	PPB = pb2;
	pb2 = pb1;
    }

    PO = FO = (char *) 0;
    if (oracle_msg)
    {
	amsg_free(oracle_msg);
	oracle_msg = (amsg_t) 0;
    }
    Notify("End of recomputation - Waiting for job\n");
    /* tell the destination node that we are ready for a job */
    sch_engine_migrate(LEAF, &dest_branch);
    PP = do_idle_code_;
    eng_msg_loop();
}

/*---------------------------------------------
 * General memory copying via shared memory
 *---------------------------------------------*/

#define COPY_BUF_UNITS 4096
#define COPY_INCR_UNITS 512
#define Units(n) ((n) ? ((n) - 1)/sizeof(copy_header) + 1 : 0)

typedef union {
    double align;
    struct {
	    generic_ptr dest;
	    int size;
    } h;
} copy_header;

struct copy_session_desc {
    copy_header * buf;
    copy_header * buf_end;
    copy_header * volatile r;	/* buf .. buf_end */
    copy_header * volatile w;	/* buf .. buf_end */
};

void
_init_memcpy(struct copy_session_desc *csd)
{
    csd->buf = (copy_header*) hg_alloc_size(COPY_BUF_UNITS*sizeof(copy_header));
    csd->buf_end = csd->buf + COPY_BUF_UNITS;
    csd->r = csd->buf_end;	/* buf+1 .. buf_end */
    csd->w = csd->buf;		/* buf .. buf_end-1 */
}

void
_cleanup_memcpy(struct copy_session_desc *csd)
{
    hg_free_size((generic_ptr) csd->buf,
			(char *) csd->buf_end - (char *) csd->buf);
}

void
_get_memory(struct copy_session_desc *csd)
{
    bmem_size_t size;
#if 0
    p_fprintf(current_err_, "get_memory\n");
    ec_flush(current_err_);
#endif
    for (;;)
    {
	copy_header *w, *r = csd->r;
	w = csd->w;			/* read the volatile value once */
	if (r == w || (r == csd->buf_end && w == csd->buf))
	{
	    do {			/* wait until buffer non-empty */
		w = csd->w;		/* read the volatile value once */
	    } while (r == w || (r == csd->buf_end && w == csd->buf));
	}
	if (r == csd->buf_end)
	    r = csd->buf;
	size = r->h.size;
	WStat_Add(copy_from_bytes, size);
	if (size == sizeof(word))
	    *(word *)(r->h.dest) = *(word *)(r+1);
	else if (size == sizeof(pword))
	    *(pword *)(r->h.dest) = *(pword *)(r+1);
	else if (size > 0)
	{
#if 0
	    p_fprintf(current_err_, "   memcpy(0x%x,0x%x,%d)\n", r->h.dest, r+1, size);
	    ec_flush(current_err_);
#endif
	    bmem_cpy(r->h.dest, (generic_ptr)(r+1), size);
	}
	if (!r->h.dest)			/* hit the terminator */
	{
	    csd->r = r + Units(size) + 1;	/* trigger sender */
	    break;
	}
	csd->r = r + Units(size) + 1;		/* trigger sender */
    }
}

#ifdef COPY_CHECK
void
_chk_memory(struct copy_session_desc *csd)
{
    for (;;)
    {
	copy_header *w, *r = csd->r;
	w = csd->w;			/* read the volatile value once */
	if (r == w || (r == csd->buf_end && w == csd->buf))
	{
	    do {			/* wait until buffer non-empty */
		w = csd->w;		/* read the volatile value once */
	    } while (r == w || (r == csd->buf_end && w == csd->buf));
	}
	if (r == csd->buf_end)
	    r = csd->buf;
	if (r->h.size > 0)
	{
	    char *from = (char*) (r+1);
	    char *to = (char*) r->h.dest;
	    int i;
	    for (i = r->h.size; i; i--, from++, to++)
	    {
		if (*from != *to)
		{
		    p_fprintf(current_err_, "PROBLEM at %x\n", to);
		    ec_flush(current_err_);
		    emu_break();
		}
	    }
	}
	if (!r->h.dest)			/* hit the terminator */
	{
	    csd->r = r + Units(r->h.size) + 1;	/* trigger sender */
	    break;
	}
	csd->r = r + Units(r->h.size) + 1;	/* trigger sender */
    }
}
#endif /* COPY_CHECK */

void
_put_memory(struct copy_session_desc *csd,
	generic_ptr from,
	int size)			/* we allow blocks with size == 0 */
{
    int units_free, units_to_copy, size_in_units;
    bmem_size_t size_to_copy;

    WStat_Add(copy_to_bytes, size);
    size_in_units = Units(size);
    do
    {
	copy_header *r, *w = csd->w;
	r = csd->r;			/* read the volatile value once */
	if (w+1 == r)
	{
	    do {			/* wait for space in buffer */
		r = csd->r;		/* read the volatile value once */
	    } while (w+1 == r);
	}
	units_free = r > w ? r - w - 1 : csd->buf_end - w;
	/* there may be 1..COPY_BUF_UNITS-1 units free */
	if (units_free > COPY_INCR_UNITS)	/* limit blocksize */
	    units_free = COPY_INCR_UNITS;
	if (units_free > size_in_units)		/* need header+size_in_units */
	{
	    units_to_copy = size_in_units;	/* copy all at once */
	    size_to_copy = size;
	}
	else
	{
	    units_to_copy = units_free-1;	/* copy only partly */
	    size_to_copy = units_to_copy*sizeof(copy_header);
	}
	w->h.dest = from;
	w->h.size = size_to_copy;
	if (size_to_copy == sizeof(word))
	    *(word *)(w+1) = *(word *)from;
	else if (size_to_copy == sizeof(pword))
	    *(pword *)(w+1) = *(pword *)from;
	else if (size_to_copy > 0)
	    bmem_cpy((generic_ptr) (w+1), from, size_to_copy);
	w += units_to_copy + 1;
	csd->w = w == csd->buf_end ? csd->buf : w;	/* trigger receiver */
	from = (generic_ptr) ((char *) from + size_to_copy);
	size -= size_to_copy;
	size_in_units -= units_to_copy;
    } while (size > 0);
}


/*---------------------------------------------
 * Oracle handling
 *---------------------------------------------*/

union node_to_char {
	st_handle_t	nodeid;
	char		bytes[sizeof(st_handle_t)];
};

char *
_write_node(char *bufp, st_handle_t *node)
{
    int i;
    for (i = sizeof(st_handle_t)-1; i >= 0; i--)
	*--bufp = ((union node_to_char *) node)->bytes[i];
    return bufp;
}

char *
read_node(char *bufp, st_handle_t *node)
{
    int i;
    for (i = 0; i < sizeof(st_handle_t); i++)
	((union node_to_char *) node)->bytes[i] = *bufp++;
    return bufp;
}

int
oracle_size(pword *bcommon, pword *bnew)
{
    pword *b_aux;
    pword *po;
    int size;

    size = STOPSIZE;
    b_aux = bnew ? bnew : B.args;
    po = bnew ? BOracle(bnew) : TO;
    while (po && b_aux >= bcommon)
    {
	while (IsGcFrame(BTop(b_aux))
	 || IsSmallFrame(BTop(b_aux))
	 || IsCatchFrame(BTop(b_aux))
	 || IsExceptionFrame(BTop(b_aux)))
	{
	    b_aux = BPrev(b_aux);
	}
	for (; po > BOracle(b_aux); po = OPrev(po))
	{
	    if (OCount(po))
		size += CountSize(OCount(po));
	    size += AltSize(OAlt(po));
	}
	if (OCount(po) && b_aux != bnew)
	    size += CountSize(OCount(po));
	if (IsPubParFrame(BTop(b_aux))) {
	    size += AltSize(OAlt(po)) + NODESIZE;
	} else if (IsUnpubParFrame(BTop(b_aux))) {
	    size += AltSize(OAlt(po)) + NODESIZE;
	} else {
	    size += AltSize(OAlt(po));
	}
        po = OPrev(po);
	b_aux = BPrev(b_aux);
    }
    return size;
}

void
retrieve_oracle(char *buf, int size, pword *bcommon, pword *bnew)
{
    pword *b_aux;
    pword *po;
    char *bufp;

    bufp = buf + size/sizeof(char);
    Write_Stop(bufp);

    b_aux = bnew ? bnew : B.args;
    po = bnew ? BOracle(bnew) : TO;
    while ( po && b_aux >= bcommon)
    {
	while (IsGcFrame(BTop(b_aux))
	 || IsSmallFrame(BTop(b_aux))
	 || IsCatchFrame(BTop(b_aux))
	 || IsExceptionFrame(BTop(b_aux)))
	{
	    b_aux = BPrev(b_aux);
	}
	for (; po > BOracle(b_aux); po = OPrev(po))
	{
	    /* oracle of cut/trusted par/seq choicepoint */
	    if (OCount(po)) {
		Write_Count(bufp, OCount(po));
	    }
	    if (OFlagged(po, O_CHK_ORACLE)) {
		Write_Alt(bufp, OAlt(po), CHK_FLAG);
	    } else {
		Write_Alt(bufp, OAlt(po), (OParallel(po)?PAR_FLAG:0));
	    }
	}
	if (po != BOracle(b_aux))
	{
	    p_fprintf(current_err_, "Oracle corrupted!\n");
	    ec_flush(current_err_);
	}
	if (OCount(po) && b_aux != bnew) {
	    Write_Count(bufp, OCount(po));
	}
	if (IsPubParFrame(BTop(b_aux))) {
	    bufp = _write_node(bufp, &BPar(b_aux)->node);
	    Write_Alt(bufp, OAlt(po), PAR_FLAG|CREATE_FLAG);
	} else if (IsUnpubParFrame(BTop(b_aux))) {
	    bufp = _write_node(bufp, &BPar(b_aux)->node);
	    Write_Alt(bufp, OAlt(po), PAR_FLAG|CREATE_FLAG);
	} else if (BBp(b_aux) == par_fail_code_) {
	    Write_Alt(bufp, OAlt(po), PAR_FLAG);
	} else {
	    Write_Alt(bufp, OAlt(po), CREATE_FLAG);
	}
        po = OPrev(po);
	b_aux = BPrev(b_aux);
    }
    if (buf != bufp)
    {
	p_fprintf(current_err_, "ERROR: Oracle size mismatch!\n");
	ec_flush(current_err_);
    }
}

void
print_oracle(char *po)
{
    int i;
    while(!FoIsStop(i=FoHeader(po)))
    {
	if (FoIsCount(i))
	    p_fprintf(current_output_, "skip(%d)\n", FoCount(po,i));
	else
	{
	    if (FoIsPar(i))
	    {
		if (FoIsCreate(i))
		{
		    st_handle_t node;
		    p_fprintf(current_output_, "par(%d", FoAlt(po,i));
		    Fo_Node(po,&node);
		    p_fprintf(current_output_, ", {%x,%x,%x})\n",
			node.site, node.edge, node.knot);
		}
		else
		    p_fprintf(current_output_, "followp(%d)\n", FoAlt(po,i));
	    }
	    else if (FoIsChk(i))
	    {
		p_fprintf(current_output_, "check(%d)\n", FoAlt(po,i));
	    }
	    else
	    {
		if (FoIsCreate(i))
		    p_fprintf(current_output_, "seq(%d)\n", FoAlt(po,i));
		else
		    p_fprintf(current_output_, "follows(%d)\n", FoAlt(po,i));
	    }
	}
    }
    ec_flush(current_output_);
}

/*---------------------------------------------
 * State copying
 *---------------------------------------------*/

extern int	domain_slot;


/*
 * Registers to copy
 * SP, TG, E, EB, GB are taken from the choicepoint.
 */

struct copy_data
{
    volatile int receipt;	/* set when we start receiving */
    volatile int receipt_ack;	/* receipt received */

    pword * volatile	bcopy;	/* these are passed from receiver to donor */
    pword * volatile	sp_copy;
    pword * volatile	tg_copy;
    pword ** volatile	tt_copy;

    uword *	b_end;		/* how much stack to map */
    uword *	sp_end;
    uword *	tg_end;
    uword *	tt_end;
    pword *	b_limit;	/* stack limits for overflow checks */
    pword *	sp_limit;
    pword *	tg_limit;
    pword **	tt_limit;
    pword *	b;		/* top of control stack */
    pword **	tt;             /* top of trail stack */
    pword *	lca;		/* last cut action */
    pword *	wl;		/* global woken lists */
    int		wp;		/* woken goal priority */
    pword	wp_stamp;
    pword *	oracle;		/* oracle registers */
    pword *	gctg;		/* tg after last gc */
    pword *	gcb;		/* last garbage collected choice point */
    pword *	tg_soft_lim;	/* garbage collection trigger point */
    long	segment_size;	/* garbage collection interval */
    pword *	bcommon;
    pword *	global_variable;
    st_handle_t	new_anc_branch;	/* node field of topmost common par chp */
    st_handle_t	new_dest_branch; /* node field of destination par chp */

    int		from_worker;	/* for debugging only */
    int		copy_id;	/* for debugging only */

    int		domain_slot;	/* fd stuff, hopefully temporary */

    struct copy_session_desc stacks;
};


/*
 * Scan the trail between tt and ttcommon for trailings of locations older
 * that the common ancestor (spcommon, tgcommon), and send the current
 * values of these locations to the state receiver.
 */

void
_put_modifications(struct copy_session_desc *csd, pword **tt, pword **ttcommon, pword *spcommon, pword *tgcommon)
{
    pword *addr;

    while (tt < ttcommon)
    {
	switch ((word) *tt & 3)
	{
	case TRAIL_ADDRESS:
	    addr = *tt++;
	    if (addr < tgcommon || addr >= spcommon)
		_put_memory(csd, (generic_ptr) addr, sizeof(pword));
	    break;
	case TRAIL_TAG:
	    addr = *(tt+1);
	    if (addr < tgcommon || addr >= spcommon)
		_put_memory(csd, (generic_ptr) addr, sizeof(pword));
	    tt += 2;
	    break;
	case TRAIL_MULT:
	    addr = (pword *) ((uword *) *(tt+1) + TrailedOffset((word) *tt));
	    if (addr < tgcommon || addr >= spcommon)
		_put_memory(csd, (generic_ptr) addr,
		    ((int) TrailedNumber((word) *tt) + 1) * sizeof(uword));
	    tt += TrailedNumber((word) *tt) + 3;
	    break;
	case TRAIL_EXT:
	    /* currently nothing to do here */
	    tt += TrailedEsize(*tt);
	    break;
	}
    }
}


/*
 * Deal with external handles in the copied stack segment:
 * Find them by looking for undo-trails and invoke their remote_copy method.
 */

void
_copy_external_data(pword **tt, pword **ttcommon)
{
    while (tt < ttcommon)
    {
	switch ((word) *tt & 3)
	{
	case TRAIL_ADDRESS:
	    tt++;
	    break;
	case TRAIL_TAG:
	    tt += 2;
	    break;
	case TRAIL_MULT:
	    tt += TrailedNumber((word) *tt) + 3;
	    break;
	case TRAIL_EXT:
	    if (TrailedEtype(*tt) == TRAIL_UNDO || TrailedEtype(*tt) == TRAIL_UNDO_STAMPED)
	    {
		pword *addr = TrailedLocation(tt);
		if (IsTag(addr->tag.kernel, TEXTERN) && ExternalClass(addr)->remote_copy)
		{
		    if (ExternalData(addr))
			addr[1].val.ptr = (pword *) ExternalClass(addr)->remote_copy(ExternalData(addr));
		    else
			addr[1].val.ptr = (pword *) ExternalData(addr);
		    p_fprintf(current_err_, "REMOTE_COPY\n");
		    ec_flush(current_err_);
		}
	    }
	    tt += TrailedEsize(*tt);
	    break;
	}
    }
}


/* 
 * This routine copies the engine state between
 * common_anc and dest_node to the recv_leaf.
 * Copying is done via shared memory.
 */

/*ARGSUSED*/
void
eng_donate_state(eng_handle_t m, const st_handle_t *dest_node, const st_handle_t *common_anc, const st_handle_t *recv_leaf)
{
    amsg_t msg;
    amsg_data_t * msg_data;
    aport_id_t recv_eng_port;
    pword *bcommon, *bnew, *broot, *bfirst;

    if (g_emu_.nesting_level > 1)
    {
	p_fprintf(current_err_, "ERROR: eng_donate_state with level > 1\n");
	ec_flush(current_err_);
	return;
    }

    LOG_EVENT_PUSH(COPY_TO);

    bnew = PPB;				/* find new top choicepoint */
    while (!ComnNode(&BPar(bnew)->node, dest_node))
	bnew = BPar(bnew)->ppb;
    bfirst = bnew;			/* find first above root */
    bcommon = bnew;			/* find common ancestor */
    while (!ComnNode(&BPar(bcommon)->node, common_anc))
    {
	bfirst = bcommon;
	bcommon = BPar(bcommon)->ppb;
    }
    broot = bcommon;			/* find root node */
    while (!IsRecursionFrame(BTop(broot)))
    {
	bfirst = broot;
	broot = BPar(broot)->ppb;
    }

    if (VM_FLAGS & ORACLES_ENABLED)
    {
	int i, from_root;
	amsg_size_t prefix_size;
	char *buf;

	if (bnew == broot)
	{
	    p_fprintf(current_err_, "ERROR: eng_donate_state(root)\n");
	    ec_flush(current_err_);
	    return;
	}
	if (bcommon == broot)
	{
	    prefix_size = 2*NODESIZE + AltSize(1);
	    bcommon = bfirst;		/* start from first above root */
	    from_root = 1;
	}
	else
	{
	    prefix_size = NODESIZE;
	    from_root = 0;
	    Notify("Sending oracle - start\n");
	}
	if (!IsPubParFrame(BTop(bcommon)) || !IsPubParFrame(BTop(bnew)))
	{
	    p_fprintf(current_err_, "ERROR: recomputation path\n");
	    ec_flush(current_err_);
	    Notify("Sending oracle (root) - start\n");
	}

	/* find the engine port on the receiving site */
	recv_eng_port = aport_id(aport_bport_id(recv_leaf->site), ENG_APORT_NUMBER);

	/* Allocate a message buffer for the oracle */
	i = oracle_size(bcommon, bnew);
	WStat_Add(copy_to_bytes, i+prefix_size);
	if (amsg_alloc(i+prefix_size, &msg_data, &msg))
	{
	    p_fprintf(current_err_, "ERROR: amsg_alloc()\n");
	    ec_flush(current_err_);
	    return;
	}

	/* Fill the buffer with the oracle */
	buf = (char *) msg_data + prefix_size;
	retrieve_oracle(buf, i, bcommon, bnew);

	if (from_root)
	{
	    buf = _write_node(buf, &BPar(broot)->node);
	    Write_Alt(buf, 1, PAR_FLAG|CREATE_FLAG);
	}

	buf = _write_node(buf, dest_node);
	if (buf != (char *) msg_data)
	{
	    p_fprintf(current_err_, "ERROR: msg buffer size mismatch\n");
	    ec_flush(current_err_);
	}

	/* Send it off */
	if (amsg_send(recv_eng_port, msg, MDT_BYTE, i+prefix_size, 0) != AMSG_OK)
	{
	    p_fprintf(current_err_, "ERROR: amsg_send()\n");
	    ec_flush(current_err_);
	    return;
	}

	/* Now send the goal message if necessary */
	if (from_root)
	{
	    if (!par_goal_msg_)
	    {
		p_fprintf(current_err_, "ERROR: No parallel goal defined\n");
		ec_flush(current_err_);
		return;
	    }
	    if (amsg_alloc(amsg_size(par_goal_msg_), &msg_data, &msg))
	    {
		p_fprintf(current_err_, "ERROR: amsg_alloc()\n");
		ec_flush(current_err_);
		return;
	    }
	    bmem_cpy(	(generic_ptr) msg_data,
			(generic_ptr) amsg_data(par_goal_msg_),
			amsg_size(par_goal_msg_));
	    if (amsg_send(recv_eng_port, msg, MDT_BYTE, amsg_size(msg), 0)
		!= AMSG_OK)
	    {
		p_fprintf(current_err_, "ERROR: amsg_send()\n");
		ec_flush(current_err_);
		return;
	    }
	}

    }
    else	/* use stack copying */
    {
	struct copy_data *cd;
	pword *bcopy;
	int i;

	Notify("Sending stacks - start\n");

	/* make the first copy packet: the engine registers */
	cd = (struct copy_data *) hg_alloc_size(sizeof(struct copy_data));
	cd->receipt = 0;
	cd->receipt_ack = 0;
	_init_memcpy(&cd->stacks);
	cd->b = bnew;
	cd->tt = TT;
	cd->lca = LCA;
	cd->wl = WL;
	cd->wp = WP;
	cd->wp_stamp = WP_STAMP;
	cd->oracle = ORC;
	cd->gctg = GCTG;
	Compute_Gcb(cd->gcb);
	Save_Tg_Soft_Lim(cd->tg_soft_lim);
	cd->segment_size = TG_SEG;
	cd->bcommon = bcommon;
	cd->new_anc_branch = BPar(bcommon)->node;
	cd->new_dest_branch = *dest_node;
	cd->b_end = g_emu_.control_local[0].end;
	cd->sp_end = g_emu_.control_local[1].end;
	cd->tg_end = g_emu_.global_trail[0].end;
	cd->tt_end = g_emu_.global_trail[1].end;
	cd->b_limit = g_emu_.b_limit;
	cd->sp_limit = g_emu_.sp_limit;
	cd->tg_limit = TG_LIM;
	cd->tt_limit = TT_LIM;
	cd->from_worker = ec_options.parallel_worker;
	cd->copy_id = ++copy_count_;
	cd->domain_slot = domain_slot;
	cd->global_variable = g_emu_.global_variable;

	/* find the engine port on the receiving site */
	recv_eng_port =
		aport_id(aport_bport_id(recv_leaf->site), ENG_APORT_NUMBER);

	/* Send the shared memory address of the copy structure
	 * to the receiver's engine port
	 */
	if (amsg_alloc(sizeof(struct copy_data *), &msg_data, &msg))
	{
	    p_fprintf(current_err_, "ERROR: amsg_alloc()\n");
	    ec_flush(current_err_);
	    return;
	}
	* (struct copy_data **) msg_data = cd;
	if (amsg_send(recv_eng_port, msg, MDT_ADDRESS, 1, 0) != AMSG_OK)
	{
	    p_fprintf(current_err_, "ERROR: amsg_send()\n");
	    ec_flush(current_err_);
	    return;
	}

	if (!(GlobalFlags & FULL_COPY))
	{

	    /* Now copy the stacks above bcommon to shared memory.
	     */
	    _put_memory(&cd->stacks, (generic_ptr) bcommon,
		(char *)bnew - (char*)bcommon);
#ifdef COPY_LOCAL_STACK_COMPLETELY
	    /* While we have no solution for the problem of environment cell
	     * initialization, we copy the local stack completely */
	    WStat_Add(copy_to_extra,
		    ((char*)BChp(broot)->sp - (char*)BChp(bcommon)->sp));
	    _put_memory(&cd->stacks, (generic_ptr) BChp(bnew)->sp,
		(char*)BChp(broot)->sp - (char *)BChp(bnew)->sp);
#else
	    _put_memory(&cd->stacks, (generic_ptr) BChp(bnew)->sp,
		(char*)BChp(bcommon)->sp - (char *)BChp(bnew)->sp);
#endif
	    _put_memory(&cd->stacks, (generic_ptr) TT,
		(char *)BChp(bcommon)->tt - (char*)TT);
	    _put_memory(&cd->stacks, (generic_ptr) BChp(bcommon)->tg,
		(char *)BChp(bnew)->tg - (char*)BChp(bcommon)->tg);

	    /* Wait for the bcopy information to be sent back */
	    while (!cd->receipt)
	    {
		/* short_sleep(1000); */
	    }

	    bcopy = cd->bcopy;
	    if (cd->sp_copy != BChp(bcopy)->sp
	     || cd->tg_copy != BChp(bcopy)->tg
	     || cd->tt_copy != BChp(bcopy)->tt)
	    {
		p_fprintf(current_err_,
		    "ERROR: Stack pointer mismatch in eng_donate_state()\n");
		ec_flush(current_err_);
	    }

	    /* Now bcopy .. bcommon */
	    _put_memory(&cd->stacks, (generic_ptr) bcopy,
		(char *)bcommon - (char*)bcopy);
#ifndef COPY_LOCAL_STACK_COMPLETELY
	    _put_memory(&cd->stacks, (generic_ptr) BChp(bcommon)->sp,
		(char*)BChp(bcopy)->sp - (char *)BChp(bcommon)->sp);
#endif
	    _put_memory(&cd->stacks, (generic_ptr) BChp(bcommon)->tt,
		(char*)BChp(bcopy)->tt - (char *)BChp(bcommon)->tt);
	    _put_memory(&cd->stacks, (generic_ptr) BChp(bcopy)->tg,
		(char *)BChp(bcommon)->tg - (char*)BChp(bcopy)->tg);
#ifdef COPY_LOCAL_STACK_COMPLETELY
	    _put_modifications(&cd->stacks, TT , BChp(bcopy)->tt,
		BChp(broot)->sp, BChp(bcopy)->tg);
#else
	    _put_modifications(&cd->stacks, TT , BChp(bcopy)->tt,
		BChp(bcopy)->sp, BChp(bcopy)->tg);
#endif
	    _put_memory(&cd->stacks, (generic_ptr) 0, 0);	/* terminate */

	}
	else
	{
	    /* Now copy the stacks to shared memory.
	     */
	    _put_memory(&cd->stacks, (generic_ptr) broot,
		(char *)bnew - (char*)broot);
	    _put_memory(&cd->stacks, (generic_ptr) BChp(bnew)->sp,
		(char*)BChp(broot)->sp - (char *)BChp(bnew)->sp);
	    _put_memory(&cd->stacks, (generic_ptr) TT,
		(char *)BChp(broot)->tt - (char*)TT);
	    _put_memory(&cd->stacks, (generic_ptr) BChp(broot)->tg,
		(char *)BChp(bnew)->tg - (char*)BChp(broot)->tg);
	    _put_memory(&cd->stacks, (generic_ptr) 0, 0);	/* terminate */

	    /* Wait until receiver starts copying - not really necessary */
	    while (!cd->receipt)
	    {
		/* short_sleep(1000); */
	    }
	}
	cd->receipt_ack = 1;
	Notify("Sending stacks - done\n");
    }

    WStat_Incr(copy_to_count);
    LOG_EVENT_POP;
}


void
eng_accept_state(amsg_t msg, amsg_data_t *msg_data, amsg_count_t msg_size)
{
    LOG_EVENT_PUSH(COPY_FROM);

    if (VM_FLAGS & ORACLES_ENABLED)
    {
	pword *pb;
	unsigned alt;
	st_handle_t until;

	if (oracle_msg)
	    amsg_free(oracle_msg);	/* just in case it has not been freed */
	oracle_msg = msg;

	WStat_Add(copy_from_bytes, msg_size);
	FO = (char *) msg_data;
	Fo_Node(FO,&dest_branch);	/* where to send the receipt message */

	/* Process first oracle entry: goto common ancestor */
	alt = FoHeader(FO);
	alt = FoAlt(FO,alt);
	Fo_Node(FO, &until);

	for (pb = PPB; !ComnNode(&BPar(pb)->node, &until); pb = BPar(pb)->ppb)
	{}

	BPar(pb)->node = until;	/* same node but different handle */
#ifdef PB_MAINTAINED
	PB =
#endif
	PPB = B.args = pb;
	_adjust_engine();

#if 0
	if (GlobalFlags & ENG_TRACE_FLAG)
	{
	    p_fprintf(current_err_, "FO = 0x%x, B = 0x%x\n", FO, B.args);
	}
#endif

	PO = FO;
	NTRY = 0;
	if (IsRecursionFrame(BTop(pb)))
	{
	    amsg_ret_t ret;

	    Notify("Receiving oracle (root)\n");
	    FO = 0;			/* install pending oracle later */
	    PP = BBp(pb) + 2;	/* skip Fail_clause, set to Try_clause */
	    BPar(pb)->alt = 1;	/* start of slave init code */

	    while ((ret = amsg_receive(aports[ENG_APORT_NUMBER], &par_goal_msg_,
				(amsg_data_t **) 0, (amsg_type_t *) 0,
				(amsg_count_t *) 0, 0))
		    != AMSG_OK)
	    {
		if (ret != AMSG_NOMESSAGE)
		{
		    p_fprintf(current_err_, "ERROR: amsg_receive()\n");
		    ec_flush(current_err_);
		}
		(void) bmsg_trigger(BMSG_INTRA_DOMAIN);
	    }
	}
	else
	{
	    Notify("Receiving oracle\n");
	    PP = BBp(pb) + 2;	/* skip Fail_clause, set to Try_clause */
	    BPar(pb)->alt = alt;	/* start of oracled path */
	    O_Set_Alt(BOracle(pb), alt);
	    O_Reset_Try_Count(BOracle(pb));
	}

    }
    else	/* use stack copying */
    {
	int i;
	struct copy_data *cd;

	Notify("Receiving stacks - start\n");
	cd = *(struct copy_data **) msg_data;
	amsg_free(msg);

	if (!(GlobalFlags & FULL_COPY))
	{
	    pword *my_gcb, *min_gcb, *bcopy;
	    Compute_Gcb(my_gcb);

	    /* First use sender's bcommon and its GCB to find out
	     * from where to copy.
	     */
	    bcopy = cd->bcommon;
	    min_gcb = cd->gcb < my_gcb ? cd->gcb: my_gcb;
	    if (bcopy > min_gcb && cd->gcb != my_gcb)
	    {
		Notify("Copying more because of GC\n");
		WStat_Incr(copy_more_count);
		while (bcopy > min_gcb)		/* reduce until below GCBs */
		    bcopy = BPar(bcopy)->ppb;
	    }

	    cd->bcopy = bcopy;		/* tell the result to the sender */
	    cd->sp_copy = BChp(bcopy)->sp;
	    cd->tg_copy = BChp(bcopy)->tg;
	    cd->tt_copy = BChp(bcopy)->tt;
	    cd->receipt = 1;

	    /* Backtrack to bcopy, where we want to copy from */
	    Untrail_Variables(BChp(bcopy)->tt);

	    if (bcopy < my_gcb)
		my_gcb = bcopy;
	}
	else
	{
	    cd->receipt = 1;
	}

#ifdef PB_MAINTAINED
	PB =
#endif
	PPB = B.args = cd->b;		/* get the register values */
	GCTG = cd->gctg;
	TT = cd->tt;
	LCA = cd->lca;			/* may have to be lowered ??? */
	WL = cd->wl;
	WP_STAMP = cd->wp_stamp;
	WP = cd->wp;
	ORC = cd->oracle;
	TG_SEG = cd->segment_size;
	BPar(cd->bcommon)->node = cd->new_anc_branch; /* if it is not copied */
	g_emu_.global_variable = cd->global_variable;

	domain_slot = cd->domain_slot;

	/* map stacks exactly like in the sender */
	if (!adjust_stacks(g_emu_.control_local, cd->b_end, cd->sp_end, 0) ||
	    !adjust_stacks(g_emu_.global_trail, cd->tg_end, cd->tt_end, 0))
	{
	    ec_panic(MEMORY_P, "accept_state()");
	}
	g_emu_.b_limit = cd->b_limit;
	g_emu_.sp_limit = cd->sp_limit;
	TG_LIM = cd->tg_limit;
	TT_LIM = cd->tt_limit;
	Restore_Tg_Soft_Lim(cd->tg_soft_lim);
	dest_branch = cd->new_dest_branch;

	_get_memory(&cd->stacks);		/* get the stack data */
#ifdef COPY_CHECK
	if (GlobalFlags & CHECK_COPY)
	    _chk_memory(&cd->stacks);	/* check the stack data */
#endif

	/*
	 * Treat handles of external data 
	 */
	_copy_external_data(TT, BChp(cd->bcommon)->tt);

	_cleanup_memcpy(&cd->stacks);

	/* Make the copied state sufficiently clean
	 * for further copying sessions
	 */
	_adjust_engine();

	/* make sure sender got receipt (before freeing the descriptor!) */
	while (!cd->receipt_ack)
	{
	    /* short_sleep(1000); */
	}
	hg_free_size((generic_ptr) cd, sizeof(struct copy_data));
	Notify("Received stacks - Waiting for job\n");

	/* tell the destination node that we are ready for a job */
	sch_engine_migrate(LEAF, &dest_branch);
    }
    WStat_Incr(copy_from_count);
    LOG_EVENT_POP;
}


/*---------------------------------------------
 * Engine message port
 *---------------------------------------------*/

/*
 * The asynchronous engine port message handler.
 */
/*ARGSUSED*/
void
eng_port_upcall(aport_id_t aport)
{
    Disable_Int();
    EVENT_FLAGS |= ENG_MSG_PENDING;
    Enable_Int();
    if (g_emu_.nesting_level == 1)
    {
	Fake_Overflow;
    }
}


/*
 * The synchronous engine port message handler.
 * Currently there is only one type of message for state installation.
 */
eng_sync_msg_handler(void)			/* synchronous handler */
{
    amsg_data_t * msg_data;
    amsg_count_t size;
    amsg_ret_t ret;
    amsg_t msg;

    Disable_Int();
    while ((ret = amsg_receive(aports[ENG_APORT_NUMBER], &msg, &msg_data,
		    (amsg_type_t *) 0, &size, 0))
	    != AMSG_NOMESSAGE)
    {
	if (ret != AMSG_OK)
	{
	    p_fprintf(current_err_, "ERROR: amsg_receive()\n");
	    ec_flush(current_err_);
	    continue;
	}
	Enable_Int();

	/* handle the message */
	eng_accept_state(msg, msg_data, size);

	Disable_Int();
    }
    /* Checking that there are no messages (AMSG_NOMESSAGE) and
     * resetting the ENG_MSG_PENDING bit must be uninterruptable! */
    EVENT_FLAGS &= ~ENG_MSG_PENDING;
    Enable_Int();
}


/*---------------------------------------------
 * I/O in parallel system
 *---------------------------------------------*/

void
my_io_aport(aport_id_t *port)
{
    *port = aports[IO_APORT_NUMBER];
}

void
io_port_upcall(aport_id_t port)
{
    amsg_data_t * msg_data;
    amsg_ret_t ret;
    amsg_t msg;
    stream_id nst;
    int action;
    int reply;
    aport_id_t reply_aport;

    /* We are not sure in which context we are executing this handler.
     * The signaling may be disabled which would be fatal if we are going
     * to execute a long, maybe blocking I/O operation. Therefore we
     * force BMSG_INTRA_DOMAIN_TRIGGERING on.
     */
    (void) bmsg_set_option(BMSG_INTRA_DOMAIN_TRIGGERING, BMSG_ON);

    while ((ret = amsg_receive(port, &msg, &msg_data,
		    (amsg_type_t *) 0, (amsg_count_t *) 0, 0))
	    != AMSG_NOMESSAGE)
    {
	if (ret != AMSG_OK)
	{
	    continue;
	}
	reply_aport = ((struct io_msg *) msg_data)->reply_aport;
	nst = ((struct io_msg *) msg_data)->nst;
	action = ((struct io_msg *) msg_data)->action;
	amsg_free(msg);

	reply = do_io_action(nst, action);

	/* now send the reply */
	if (amsg_alloc(sizeof(struct io_reply_msg), &msg_data, &msg) != AMSG_OK)
	{
	    p_fprintf(current_err_, "ERROR: amsg_alloc()\n");
	    ec_flush(current_err_);
	    return;
	}
	((struct io_reply_msg *) msg_data)->err_code = reply;
	if (amsg_send(reply_aport, msg, MDT_INT32, 1, 0) != AMSG_OK)
	{
	    p_fprintf(current_err_, "ERROR: amsg_send()\n");
	    ec_flush(current_err_);
	    return;
	}
    }
}

int
io_rpc(stream_id nst, int action)
{
    /* send the request */
    {
	amsg_t msg;
	amsg_data_t * msg_data;

	if (amsg_alloc(sizeof(struct io_msg), &msg_data, &msg) != AMSG_OK)
	{
	    return MPS_ERROR;
	}
	((struct io_msg *) msg_data)->reply_aport = aports[IO_REPLY_APORT_NUMBER];
	((struct io_msg *) msg_data)->nst = nst;
	((struct io_msg *) msg_data)->action = action;
	if (amsg_send(nst->aport, msg, MDT_IOREQUEST, 1, 0) != AMSG_OK)
	{
	    return MPS_ERROR;
	}
    }

    /* wait for the reply */
    {
	amsg_data_t * msg_data;
	amsg_ret_t ret;
	amsg_t msg;
	int reply;
	int was_disabled = 0;
	
	while (InterruptsDisabled)
	{
	    Enable_Int();
	    ++was_disabled;
	}
	while ((ret = amsg_receive(aports[IO_REPLY_APORT_NUMBER],
			&msg, &msg_data, 
			(amsg_type_t *) 0, (amsg_count_t *) 0, 0))
		== AMSG_NOMESSAGE)
	{
	    WStat_Incr(rpc_sleep);
	    short_sleep(1000);
	}
	while (was_disabled)
	{
	    Disable_Int();
	    --was_disabled;
	}
	if (ret != AMSG_OK)
	{
	    p_fprintf(current_err_, "ERROR: amsg_receive()\n");
	    ec_flush(current_err_);
	    return MPS_ERROR;
	}
	reply = ((struct io_reply_msg *) msg_data)->err_code;
	amsg_free(msg);
	return reply;
    }
}


/*---------------------------------------------
 * short delay for synchronisation loops
 *---------------------------------------------*/

void
short_sleep(int usec)
{
#ifdef HAVE_SELECT
    struct timeval sleep_time;
    sleep_time.tv_sec  = usec / 1000000;
    sleep_time.tv_usec = usec % 1000000;
    (void) select(0, (fd_set *)0, (fd_set *)0, (fd_set *)0, &sleep_time);
#endif
}
