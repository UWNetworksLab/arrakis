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
 * Copyright (C) 1997-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, IC-Parc
 * 
 * END LICENSE BLOCK */
/*----------------------------------------------------------------------
* System:	ECLiPSe Constraint Logic Programming System
* Version:	$Id: edge_finder.c,v 1.1.1.1 2006/09/23 01:53:26 snovello Exp $
*----------------------------------------------------------------------*/

/*
 * Description:		Edge-finder in C
 *			Nuijten's algorithms
 *
 * Author:		J.Schimpf, IC-Parc
 *
 */

#include "eclipse.h"

#ifdef STDC_HEADERS
#include <stdlib.h>	/* for malloc() */
#endif

#define BOUND2
#define BOUND3
#define SET_BOOLEANS

#ifndef NULL
#define NULL 0
#endif

#define DOMAIN_MINF (-10000000)
#define DOMAIN_PINF (10000000)

#define OPT_QUADR	0
#define OPT_CUBIC	1	/* bit-significant */
#define OPT_BOOLS	2	/* bit-significant */

typedef struct {
    long	est, lst;	/* earliest/latest start time */
    long	ect, lct;	/* earliest/latest completion time */
    long	size;		/* size (resource usage) */
    long	sz;		/* size index */
    long	dur;		/* (min) duration */
    long	area;		/* area, ie. size*duration */
    long	lb, ub;		/* new bounds on start time */
} task_t;

typedef struct {
    task_t	**asc_lct;	/* [ntasks] */
    task_t	**asc_est;	/* [ntasks] */
    task_t	*tasks;		/* [ntasks] */
    long	*sizes;		/* [ntasks], only nsizes used */
    long	*_G;		/* [ntasks*nsizes_max], may get reallocated */
    long	*ECT;		/* [ntasks], also LST */
    long	ntasks;
    long	nsizes;		/* number of valid entries in sizes[] */
    long	nsizes_max;	/* tracks the maximum of nsizes */
    long	some_size_changed; /* bool: one or more tasks changed size */
    long	capacity;
    long	refcnt;
    long	option;
} ef_t;

#define G(i)	(ef->_G + (i)*ef->nsizes)
#define Size(j)	(ef->sizes[j])
#define LST	ECT
#define DELTA	ECT
#define TaskNr(t)	((t)-ef->tasks)

static void _ef_desc_free();
static t_ext_ptr _ef_desc_copy();
static pword _ef_desc_get();

static t_ext_type ef_desc = {
    _ef_desc_free,
    _ef_desc_copy,
    NULL,NULL,NULL,NULL,
    _ef_desc_copy,
    _ef_desc_get,
    NULL
};


static void
_ef_desc_free(obj)
t_ext_ptr obj;
{
    ef_t *ef = (ef_t *)obj;
    if (--ef->refcnt == 0)
    {
	free(ef->tasks);
	free(ef->asc_est);
	free(ef->asc_lct);
	free(ef->sizes);
	free(ef->ECT);
	if (ef->_G)
	    free(ef->_G);
	free(ef);
    }
}

static t_ext_ptr
_ef_desc_copy(obj)
t_ext_ptr obj;
{
    ((ef_t *)obj)->refcnt++;
    return obj;
}

static pword
_ef_desc_get(obj, i)
t_ext_ptr obj;
int i;
{
    return ec_term(ec_did("task",2),
/*
    return ec_term(ec_did("task",9),
    	 ec_long(((ef_t *) obj)->tasks[i].est),
    	 ec_long(((ef_t *) obj)->tasks[i].lst),
    	 ec_long(((ef_t *) obj)->tasks[i].ect),
    	 ec_long(((ef_t *) obj)->tasks[i].lct),
    	 ec_long(((ef_t *) obj)->tasks[i].sz),
    	 ec_long(((ef_t *) obj)->tasks[i].dur),
    	 ec_long(((ef_t *) obj)->tasks[i].area),
*/
    	 ec_long(((ef_t *) obj)->tasks[i].lb),
    	 ec_long(((ef_t *) obj)->tasks[i].ub)
    );
}

int
ec_init_ef( /* +N, +Capacity, +Option, -EfHandle */ )
{
    ef_t *ef;
    long i,n,cap,opt;

    if (ec_get_long(ec_arg(1), &n) != PSUCCEED)
    	return TYPE_ERROR;
    if (ec_get_long(ec_arg(2), &cap) != PSUCCEED)
    	return TYPE_ERROR;
    if (n <= 0)
    	return RANGE_ERROR;
    if (ec_get_long(ec_arg(3), &opt) != PSUCCEED)
    	return TYPE_ERROR;

    ef = (ef_t *) malloc(n*sizeof(ef_t));
    ef->tasks = (task_t *) malloc(n*sizeof(task_t));
    ef->asc_lct = (task_t **) malloc(n*sizeof(task_t*));
    ef->asc_est = (task_t **) malloc(n*sizeof(task_t*));
    ef->sizes = (long *) malloc(n*sizeof(long));
    ef->ECT = (long *) malloc(n*sizeof(long));
    for (i=0; i<n; i++)
    {
	ef->asc_lct[i] = ef->asc_est[i] = &ef->tasks[i];
	ef->tasks[i].sz = n;	/* uninit */
    }
    ef->capacity = cap;
    ef->ntasks = n;
    ef->nsizes = ef->nsizes_max = 0;
    ef->_G = NULL;
    ef->refcnt = 1;
    ef->option = opt;
    ef->some_size_changed = 1;

    return ec_unify_arg(4, ec_handle(&ef_desc, (t_ext_ptr)ef));
}

int
ec_init_task( /*EfHandle,I,Est,Lst,Ect,Lct,Sz,Dur,Area*/ )
{
    ef_t *ef;
    long i, size;
    
    if (ec_get_handle(ec_arg(1), &ef_desc, (t_ext_ptr*)&ef) != PSUCCEED ||
	ec_get_long(ec_arg(2), &i) != PSUCCEED ||
	ec_get_long(ec_arg(3), &ef->tasks[i].est) != PSUCCEED ||
	ec_get_long(ec_arg(4), &ef->tasks[i].lst) != PSUCCEED ||
	ec_get_long(ec_arg(5), &ef->tasks[i].ect) != PSUCCEED ||
	ec_get_long(ec_arg(6), &ef->tasks[i].lct) != PSUCCEED ||
	ec_get_long(ec_arg(7), &size) != PSUCCEED ||
	ec_get_long(ec_arg(8), &ef->tasks[i].dur) != PSUCCEED ||
	ec_get_long(ec_arg(9), &ef->tasks[i].area) != PSUCCEED
    )
    {
    	return TYPE_ERROR;
    }

    ef->tasks[i].lb = ef->tasks[i].est;
    ef->tasks[i].ub = ef->tasks[i].lst;

    if (!ef->some_size_changed  &&  size != ef->tasks[i].size)
	ef->some_size_changed = 1;
    ef->tasks[i].size = size;

    return PSUCCEED;
}

static void
_construct_size_table(ef)
ef_t *ef;
{
    long i;
    ef->nsizes = 0;
    for (i=0; i<ef->ntasks; ++i)
    {
	long j = 0;		/* lookup */
	while (j < ef->nsizes  &&  ef->tasks[i].size != Size(j))
	    ++j;
	if (j == ef->nsizes)	/* new size */
	{
	    Size(j) = ef->tasks[i].size;
	    ++ef->nsizes;
	}
	ef->tasks[i].sz = j;
    }
    if (ef->nsizes > ef->nsizes_max)
    {
	/* we have more sizes than ever: need a bigger array for G */
	ef->nsizes_max = ef->nsizes;
	if (ef->_G)
	    ef->_G = (long*) realloc(ef->_G, sizeof(long)*ef->nsizes_max*ef->ntasks);
	else
	    ef->_G = (long*) malloc(sizeof(long)*ef->nsizes_max*ef->ntasks);
    }
    ef->some_size_changed = 0;
    return;
}

static int
_asc_lct(t1, t2)
task_t **t1, **t2;
{
    return (*t1)->lct > (*t2)->lct ? 1 : (*t1)->lct < (*t2)->lct ? -1 : 0;
}

static int
_asc_est(t1, t2)
task_t **t1, **t2;
{
    return (*t1)->est > (*t2)->est ? 1 : (*t1)->est < (*t2)->est ? -1 : 0;
}

#define upd_max(max, expr) {\
	long _tmp = (expr);\
	if (_tmp > max) max = _tmp;\
}

#define upd_min(min, expr) {\
	long _tmp = (expr);\
	if (_tmp < min) min = _tmp;\
}

#define upd_max_f(max, expr) {\
	double _tmp = (expr);\
	if (_tmp > max) max = _tmp;\
}

#define upd_min_f(min, expr) {\
	double _tmp = (expr);\
	if (_tmp < min) min = _tmp;\
}

#define Before(ti,tj) {\
	int err = _before(ef, bools, ti, tj);\
	if (err != PSUCCEED) return err;\
}


/* Set boolean such that Ti is before tj */

static int
_before(ef, bools, ti, tj)
ef_t *ef;
pword bools;
task_t *ti, *tj;
{
    int err, idx;
    long zero_one;
    pword var;
    int i = TaskNr(ti);
    int j = TaskNr(tj);
    if (i < j) { idx = j*(j-1)/2+i+1; zero_one = 0; }
    else if (i > j) { idx = i*(i-1)/2+j+1; zero_one = 1; }
    else return RANGE_ERROR;
    err = ec_get_arg(idx, bools, &var);
    if (err != PSUCCEED)
    	return err;
    return ec_unify(var, ec_long(zero_one));
}

int
ec_ef_disj( /* EfHandle */ )
{
    ef_t *ef;
    int y, x, i;
    long g, h, p;
    task_t **X, **Y;
    pword bools;
    bools = ec_arg(2);

    if (ec_get_handle(ec_arg(1), &ef_desc, (t_ext_ptr*)&ef) != PSUCCEED)
    	return TYPE_ERROR;

    if (ec_get_nil(bools) != PSUCCEED)
	ef->option |= OPT_BOOLS;

    if (ef->some_size_changed)
	_construct_size_table(ef);

    qsort((void*) (ef->asc_lct), ef->ntasks, sizeof(task_t*), _asc_lct);
    qsort((void*) (ef->asc_est), ef->ntasks, sizeof(task_t*), _asc_est);

    Y = ef->asc_lct;
    X = ef->asc_est;
    for (y = 0; y < ef->ntasks; ++y)		/* asc lct */
    {
	if (y == ef->ntasks-1 || Y[y]->lct != Y[y+1]->lct)
	{
	    p = 0;
	    g = DOMAIN_MINF;
#ifdef BOUND2
	    ef->ECT[ef->ntasks-1] = DOMAIN_PINF;
#endif

	    for (i = ef->ntasks-1; i >= 0; --i)	/* desc est */
	    {
		if (X[i]->lct <= Y[y]->lct)
		{
		    p += X[i]->dur;
		    upd_max(g, X[i]->est + p);
		    if (g > Y[y]->lct)
		    	return PFAIL;
#ifdef BOUND2
		    upd_min(ef->ECT[i], X[i]->ect);
#endif
		}
		ef->_G[i] = g;
#ifdef BOUND2
		if (i > 0)
		    ef->ECT[i-1] = ef->ECT[i];
#endif
	    }

	    h = DOMAIN_MINF;
	    for (x = 0; x < ef->ntasks; ++x)	/* asc est */
	    {
		if (X[x]->lct > Y[y]->lct)
		{
		    if (X[x]->est + p + X[x]->dur > Y[y]->lct)
		    {
			upd_max(X[x]->lb, ef->_G[x]);
#ifdef SET_BOOLEANS
			if (ef->option & OPT_BOOLS)
			{
			    int v;
			    for (v = x+1; v < ef->ntasks && X[v]->est < Y[y]->lct; ++v)
			    {
			        if (X[v]->lct <= Y[y]->lct)
				    Before(X[v],X[x]);
			    }
			}
#endif
		    }
		    if (h + X[x]->dur > Y[y]->lct)
		    {
			upd_max(X[x]->lb, g);
#ifdef SET_BOOLEANS
			if (ef->option & OPT_BOOLS)
			{
			    int v;
			    for (v = 0; v < ef->ntasks && X[v]->est < Y[y]->lct; ++v)
			    {
			        if (X[v]->lct <= Y[y]->lct)
				    Before(X[v],X[x]);
			    }
			}
#endif
		    }
		}
		else
		{
		    upd_max(h, X[x]->est + p);
		    p -= X[x]->dur;
		}
#ifdef BOUND2
		if (ef->option & OPT_CUBIC)
		{
		    int w;
		    /* duration of those that must start later than w and
		     * finish before y */
		    long Pp = p + X[x]->dur;
		    /* what remains if x is first: */
		    long avail = Y[y]->lct - X[x]->est;
		    for (w = x-1; w >= 0; --w)	/* desc est */
		    {
			if (X[w]->lct <= Y[y]->lct)
			{
			    if (ef->ECT[w] <= X[x]->est)
			    	break;
			    Pp += X[w]->dur;
			    if (Pp > avail)
				/* x can't be first, must be after w */
				upd_max(X[x]->lb, ef->ECT[w]);
			}
		    }
		}
#endif
	    }
	}
    }

    Y = ef->asc_est;
    X = ef->asc_lct;
    for (y = ef->ntasks-1; y >= 0; --y)		/* desc est */
    {
	if (y == 0 || Y[y]->est != Y[y-1]->est)
	{
	    p = 0;
	    g = DOMAIN_PINF;
#ifdef BOUND2
	    ef->LST[0] = DOMAIN_MINF;
#endif

	    for (i = 0; i < ef->ntasks; ++i)	/* asc lct */
	    {
		if (X[i]->est >= Y[y]->est)
		{
		    p += X[i]->dur;
		    upd_min(g, X[i]->lct - p);
		    if (g < Y[y]->est)
		    	return PFAIL;
#ifdef BOUND2
		    upd_max(ef->LST[i], X[i]->lst);
#endif
		}
		ef->_G[i] = g;
#ifdef BOUND2
		if (i < ef->ntasks-1)
		    ef->LST[i+1] = ef->LST[i];
#endif
	    }

	    h = DOMAIN_PINF;
	    for (x = ef->ntasks-1; x >= 0; --x)	/* desc lct */
	    {
		if (X[x]->est < Y[y]->est)
		{
		    if (X[x]->lct - p - X[x]->dur < Y[y]->est)
		    {
			upd_min(X[x]->ub, ef->_G[x] - X[x]->dur);
#ifdef SET_BOOLEANS
			if (ef->option & OPT_BOOLS)
			{
			    int v;
			    for (v = x-1; v >= 0 && X[v]->lct > Y[y]->est; --v)
			    {
			        if (X[v]->est >= Y[y]->est)
				    Before(X[x],X[v]);
			    }
			}
#endif
		    }
		    if (h - X[x]->dur < Y[y]->est)
		    {
			upd_min(X[x]->ub, g - X[x]->dur);
#ifdef SET_BOOLEANS
			if (ef->option & OPT_BOOLS)
			{
			    int v;
			    for (v = ef->ntasks-1; v >= 0 && X[v]->lct > Y[y]->est; --v)
			    {
			        if (X[v]->est >= Y[y]->est)
				    Before(X[x],X[v]);
			    }
			}
#endif
		    }
		}
		else
		{
		    upd_min(h, X[x]->lct - p);
		    p -= X[x]->dur;
		}
#ifdef BOUND2
		if (ef->option & OPT_CUBIC)
		{
		    int w;
		    long Pp = p + X[x]->dur;
		    long avail = X[x]->lct - Y[y]->est;
		    for (w = x+1; w < ef->ntasks; ++w)	/* asc lct */
		    {
			if (X[w]->est >= Y[y]->est)
			{
			    if (ef->LST[w] >= X[x]->lct)
			    	break;
			    Pp += X[w]->dur;
			    if (Pp > avail)
				upd_min(X[x]->ub, ef->LST[w] - X[x]->dur);
			}
		    }
		}
#endif
	    }
	}
    }
    return PSUCCEED;
}

int
ec_ef_cum( /* EfHandle */ )
{
    ef_t *ef;
    int y, x, i, j;
    long l, Ar, cap;
    long *g;
    double H;
    task_t **X, **Y;

    if (ec_get_handle(ec_arg(1), &ef_desc, (t_ext_ptr*)&ef) != PSUCCEED)
    	return TYPE_ERROR;

    if (ef->some_size_changed)
	_construct_size_table(ef);

    cap = ef->capacity;

    qsort((void*) (ef->asc_lct), ef->ntasks, sizeof(task_t*), _asc_lct);
    qsort((void*) (ef->asc_est), ef->ntasks, sizeof(task_t*), _asc_est);

    Y = ef->asc_lct;
    X = ef->asc_est;
    for (y = 0; y < ef->ntasks; ++y)		/* asc lct */
    {
	if (y == ef->ntasks-1 || Y[y]->lct != Y[y+1]->lct)
	{
	    /*
	     * We are looking at all task intervals ending at Y[y]->lct
	     */

	    Ar = 0;
	    l = DOMAIN_MINF;
	    g = G(ef->ntasks-1);	/* first i in next loop */
	    for (j = 0; j < ef->nsizes; ++j)
		g[j] = DOMAIN_MINF;
#ifdef BOUND2
	    ef->ECT[ef->ntasks-1] = DOMAIN_PINF;
#endif

	    for (i = ef->ntasks-1; i >= 0; --i)	/* desc est */
	    {
		if (X[i]->lct <= Y[y]->lct)
		{
		    /*
		     * We are looking at the task interval between
		     * X[i]->est .. Y[y]->lct (the actual upper end is l).
		     * We compute g[sz] which is a lower bound for the
		     * start time of a task of size sz which starts
		     * after this task interval.
		     */

		    Ar += X[i]->area;
		    if (X[i]->est + (Ar-1+cap)/cap > Y[y]->lct)
		    	return PFAIL;
#ifdef BOUND2
		    upd_min(ef->ECT[i], X[i]->ect);
#endif
		    upd_max(l, X[i]->lct);
		    for (j = 0; j < ef->nsizes; ++j)
		    {
			long size_j = Size(j);
			long rest = Ar - (l-X[i]->est) * (cap-size_j);
			if (rest > 0)
			    upd_max(g[j], X[i]->est + (rest-1+size_j)/size_j);
		    }
		}
		if (i > 0)	/* init g for the next iteration */
		{
		    for (j = 0; j < ef->nsizes; ++j)
			G(i-1)[j] = g[j];
		    g = G(i-1);
#ifdef BOUND2
		    ef->ECT[i-1] = ef->ECT[i];
#endif
		}
	    }

	    /* We have now
	     * Ar	area of the largest task interval ending at Y[y]->lct
	     *		(we reconstruct the smaller ones by subtracting).
	     * G[i,sz]	For each task interval X[i]->est..Y[y]->lct and
	     *		each size sz of possible subsequent task: a lower
	     *		bound on the start time of such a subsequent task.
	     *		(the G[i] for tasks not belonging to the task
	     *		intervals have the same contents as the next
	     *		subsequent member).
	     */
	    /*
	     * H is an obvious lower bound on the start times, computed by
	     * assuming that all tasks in a task interval are packed to the
	     * and executed with full capacity (fuly elastic relaxation).
	     * This is a float because we can't round up and we would lose
	     * information by rounding down.
	     */
	    H = (double) DOMAIN_MINF;
	    for (x = 0; x < ef->ntasks; ++x)	/* asc est */
	    {
		if (X[x]->lct > Y[y]->lct)
		{
		    /* X[x] is a task not belonging to the
		     * task intervals under consideration
		     */
		    if (Ar + X[x]->area > (Y[y]->lct - X[x]->est) * cap)
		    {
			/*
			 * X[x] must be after all tasks between
			 * X[x]->est..Y[y]->lct.
			 * We could set ordering booleans here.
			 */
			/* Apply lower bound G on starting time */
			upd_max(X[x]->lb, G(x)[X[x]->sz]);
		    }

		    /* If X[x] doesn't fit in before Y[y]->lct, it
		     * must start after _all_ tasks before Y[y]->lct
		     */
		    if (H + ((double) X[x]->area)/cap > Y[y]->lct)
		    {
			upd_max(X[x]->lb, g[X[x]->sz]);
		    }
#ifdef BOUND3
		    if ((ef->option & OPT_CUBIC) && Ar > 0)
		    {
			long Arp = Ar;
			/* find the next member k of the task interval */
			int k = x;
			while (X[k]->lct > Y[y]->lct)
			    ++k;
			while (Arp > 0 && X[k]->est < X[x]->ect)
			{
			    if (Arp + (X[x]->ect - X[k]->est) * Size(X[x]->sz)
			      > (Y[y]->lct - X[k]->est) * cap)
			    {
				upd_max(X[x]->lb, G(x)[X[x]->sz]);
			    }
			    Arp -= X[k]->area;
			    ++k;
			    if (Arp > 0)
				while (X[k]->lct > Y[y]->lct)
				    ++k;
			}
		    }
#endif
		}
		else	/* X[x] is a member of some task interval */
		{
		    upd_max_f(H, X[x]->est + ((double) Ar)/cap);
		    Ar -= X[x]->area;
		}
#ifdef BOUND2
		if (ef->option & OPT_CUBIC)
		{
		    int w;
		    /* area of tasks w..y without x */
		    long Arp = Ar;
		    /* what remains if x is first: */
		    long ect_x = X[x]->ect < Y[y]->lct ? X[x]->ect : Y[y]->lct;
		    for (w = x-1; w >= 0; --w)	/* desc est */
		    {
			if (X[w]->lct <= Y[y]->lct)
			{
			    if (ef->ECT[w] <= X[x]->est)
			    	break;
			    Arp += X[w]->area;
			    if (Arp + (ect_x - X[w]->est) * Size(X[x]->sz)
			      > (Y[y]->lct - X[w]->est) * cap)
				/* x can't be first, must be after w */
				upd_max(X[x]->lb, ef->ECT[w]);
			}
		    }
		}
#endif
	    }
	}
    }

    Y = ef->asc_est;
    X = ef->asc_lct;
    for (y = ef->ntasks-1; y >= 0; --y)		/* desc est */
    {
	if (y == 0 || Y[y]->est != Y[y-1]->est)
	{
	    Ar = 0;
	    l = DOMAIN_PINF;
	    g = G(0);			/* first i in next loop */
	    for (j = 0; j < ef->nsizes; ++j)
		g[j] = DOMAIN_PINF;
#ifdef BOUND2
	    ef->LST[0] = DOMAIN_MINF;
#endif

	    for (i = 0; i < ef->ntasks; ++i)	/* asc lct */
	    {
		if (X[i]->est >= Y[y]->est)
		{
		    Ar += X[i]->area;
		    if (X[i]->lct - (Ar-1+cap)/cap < Y[y]->est)
		    	return PFAIL;
#ifdef BOUND2
		    upd_max(ef->LST[i], X[i]->lst);
#endif
		    upd_min(l, X[i]->est);
		    for (j = 0; j < ef->nsizes; ++j)
		    {
			long size_j = Size(j);
			long rest = Ar - (X[i]->lct-l) * (cap-size_j);
			if (rest > 0)
			    upd_min(g[j], X[i]->lct - (rest-1+size_j)/size_j);
		    }
		}
		if (i < ef->ntasks-1)
		{
		    for (j = 0; j < ef->nsizes; ++j)
			G(i+1)[j] = g[j];
		    g = G(i+1);
#ifdef BOUND2
		    ef->LST[i+1] = ef->LST[i];
#endif
		}
	    }

	    H = (double) DOMAIN_PINF;
	    for (x = ef->ntasks-1; x >= 0; --x)	/* desc lct */
	    {
		if (X[x]->est < Y[y]->est)
		{
		    if (Ar + X[x]->area > (X[x]->lct - Y[y]->est) * cap)
			upd_min(X[x]->ub, G(x)[X[x]->sz] - X[x]->dur);

		    if (H - ((double) X[x]->area)/cap < Y[y]->est)
			upd_min(X[x]->ub, g[X[x]->sz] - X[x]->dur);
#ifdef BOUND3
		    if ((ef->option & OPT_CUBIC) && Ar > 0)
		    {
			long Arp = Ar;
			int k = x;
			while (X[k]->est < Y[y]->est)
			    --k;
			while (Arp > 0 && X[k]->lct > X[x]->lst)
			{
			    if (Arp + (X[k]->lct - X[x]->lst) * Size(X[x]->sz)
			      > (X[k]->lct - Y[y]->est) * cap)
			    {
				upd_min(X[x]->ub, G(x)[X[x]->sz] - X[x]->dur);
			    }
			    Arp -= X[k]->area;
			    --k;
			    if (Arp > 0)
				while (X[k]->est < Y[y]->est)
				    --k;
			}
		    }
#endif
		}
		else
		{
		    upd_min_f(H, X[x]->lct - ((double) Ar)/cap);
		    Ar -= X[x]->area;
		}
#ifdef BOUND2
		if (ef->option & OPT_CUBIC)
		{
		    int w;
		    long Arp = Ar;
		    long lst_x = X[x]->lst > Y[y]->est ? X[x]->lst : Y[y]->est;
		    for (w = x+1; w < ef->ntasks; ++w)	/* asc lct */
		    {
			if (X[w]->est >= Y[y]->est)
			{
			    if (ef->LST[w] >= X[x]->lct)
			    	break;
			    Arp += X[w]->area;
			    if (Arp + (X[w]->lct - lst_x) * Size(X[x]->sz)
			      > (X[w]->lct - Y[y]->est) * cap)
				upd_min(X[x]->ub, ef->LST[w] - X[x]->dur);
			}
		    }
		}
#endif
	    }
	}
    }
    return PSUCCEED;
}


int
ec_ef_quad( /* EfHandle */ )
{
    ef_t *ef;
    int i, j, k;
    task_t **Y;
    long Sjk;
    long Pjk;
    long delta;

    if (ec_get_handle(ec_arg(1), &ef_desc, (t_ext_ptr*)&ef) != PSUCCEED)
    	return TYPE_ERROR;

    qsort((void*) (ef->asc_lct), ef->ntasks, sizeof(task_t*), _asc_lct);

    Y = ef->asc_lct;
    for (j = 0; j < ef->ntasks; ++j)		/* asc lct */
    {
	Pjk = 0;
	delta = DOMAIN_PINF;
	for (k = 0; k <= j; ++k)
	{
	    if (Y[k]->lst >= Y[j]->lst)
	    	Pjk += Y[k]->dur;
	    /* Sjk = DOMAIN_MINF; upd_min(delta, Y[k]->lct - Sjk); */
	    ef->DELTA[k] = delta;
	}
	for (; k < ef->ntasks; ++k)
	{
	    if (Y[k]->lst >= Y[j]->lst)
	    	Pjk += Y[k]->dur;
	    Sjk = Pjk;
	    upd_min(delta, Y[k]->lct - Sjk);
	    ef->DELTA[k] = delta;
	}

	for (i = 0; i < ef->ntasks; ++i)
	{
	    if (Y[i]->lst < Y[j]->lst)
	    {
		if (Y[i]->lst > delta)
		    upd_max(Y[i]->lb, Y[j]->lst);
	    }
	    else
	    {
		if ((i>0 && Y[i]->lst > ef->DELTA[i-1])  ||  Y[i]->est > delta)
		    upd_max(Y[i]->lb, Y[j]->lst);
	    }
	}
    }

#if 0
    qsort((void*) (ef->asc_est), ef->ntasks, sizeof(task_t*), _asc_est);

    Y = ef->asc_est;
    for (y = ef->ntasks-1; y >= 0; --y)		/* desc est */
    {
	
    }
#endif

    return PSUCCEED;
}
