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
 * Copyright (C) 1993-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/**************************************************************************/
/*                                                                        
  FILE:           constraints_elipsys.c
  AUTHOR:         Andre Veron
  CREATION:       June 1993
  
  DESCRIPTION:    
                                                                          */
/**************************************************************************/

#include <stdio.h>
#include "config.h"
#include "sepia.h"
#include "types.h"
#include        "embed.h"
#include "mem.h"
#include "dict.h"
#include "fd.h"
#include "error.h"


#define Assert(ex)	{if (!(ex)){(void) p_fprintf(current_err_, "Elipsys FD internal error: file \"%s\":%d\n", __FILE__, __LINE__); p_reset();}}


#define Append_List3(fct, tag1,val1,tag2,val2,tag3,val3, list) {\
        register pword *_pw1,*_pw2;             \
	_pw1 = TG ;                             \
	Push_Struct_Frame((fct));		\
	 CopyToPrologWord(_pw1[1],(val1),(tag1));\
	 CopyToPrologWord(_pw1[2],(val2),(tag2));\
	 CopyToPrologWord(_pw1[3],(val3),(tag3));\
 	_pw2 = TG;                              \
	Push_List_Frame();                      \
	    _pw2[0].val.ptr = _pw1;		\
	    _pw2[0].tag.kernel = TCOMP;		\
	if (*(list)) {				\
	    _pw2[1].val.ptr = *(list);		\
	    _pw2[1].tag.kernel = TLIST;		\
	} else {				\
	    _pw2[1].tag.kernel = TNIL; 		\
	}					\
	*(list) = _pw2;				\
    }



#define Append_List(wd, tag1, val1, tag2, val2, list){   \
        register pword *_pw1,*_pw2;             \
	_pw1 = TG ;                             \
	Push_Struct_Frame(wd);			\
	 CopyToPrologWord(_pw1[1],(val1),(tag1));\
	 CopyToPrologWord(_pw1[2],(val2),(tag2));\
 	_pw2 = TG;                              \
	Push_List_Frame();                      \
	    _pw2[0].val.ptr = _pw1;		\
	    _pw2[0].tag.kernel = TCOMP;		\
	if (*(list)) {				\
	    _pw2[1].val.ptr = *(list);          \
	    _pw2[1].tag.kernel = TLIST;		\
	} else {				\
	    _pw2[1].tag.kernel = TNIL; 		\
	}					\
	*(list) = _pw2;				\
    }


#define CopyToPrologWord(to,value,tagg)                         \
   (to).val.all = (value);                                      \
   (to).tag.kernel = (tagg);


/* Compatibility package from ElipSys to Eclipse */


#define INLINE
#define FALSE PFAIL
#define FAIL  PFAIL
#define TRUE  PSUCCEED
#define SUCCEED PSUCCEED
#define _False(v) ((v) == FALSE)
#define _True(v) (!((v) == FALSE))
#define DELAY PSUCCEED
#define d_arity(did) DidArity((did))
#define StructArgs(s) (((s)->val.ptr) + 1)
#define StrArg(s,n)   (((s)->val.ptr) + 1 + (n))
#define Functor(s)    (((s)->val.ptr)->val.did)
#define IVal(p)       ((p)->val.nint)
#define IsInt(p) IsInteger((p)->tag)
#define IsDomVar(p) IsMeta((p)->tag)
#define IsDvar(p) IsMeta((p)->tag)
#define IsStruct(p) IsCompound((p)->tag)
#define Unsigned unsigned long
#define Int long
#define BOOLEAN int
#define PrologWord pword
#define MetaTerm(pw)                       ((pw) + 1)


#ifdef __STDC__
static PrologWord       *dereference(PrologWord *);
static Int              dmax(PrologWord *);
static Int              dmin(PrologWord *);
static BOOLEAN          dupdate_min(PrologWord *,Int, pword **);
static BOOLEAN          dupdate_max(PrologWord *,Int, pword **);
static Int              gmin(PrologWord *);
static Int              gmax(PrologWord *);

#else
static PrologWord       *dereference();
static Int              dmax();
static Int              dmin();
static BOOLEAN          dupdate_min();
static BOOLEAN          dupdate_max();
static Int              gmin();
static Int              gmax(); 

#endif

static BOOLEAN		contigs(pword *StructTable, pword *Sequences, pword *Item, pword *Occurences, pword *Contigs, pword **list);
static BOOLEAN		disjunctive(pword *StructStarts, pword *StructDurations, pword *StructOrientations, pword **list);
static BOOLEAN		disjunction_choose(pword *x, pword *Dx, pword *y, pword *Dy, pword *branch, pword **list);
static BOOLEAN		sequences(pword *StructTable, pword *Sequences, pword *Item, pword *Occurences, pword **list);

/*
 * EXTERNAL VARIABLE DEFINITIONS:
 */
static int UNI_RESULT;


/*
 * EXTERNAL VARIABLE DECLARATIONS:
 */

/*
 * STATIC VARIABLE DEFINITIONS:
 */
static int	p_disjunctive_interface(value Val1, type Tag1, value Val2, type Tag2, value Val3, type Tag3, value vl, type tl),
		p_disjunction_choose_interface(value Val1, type Tag1, value Val2, type Tag2, value Val3, type Tag3, value Val4, type Tag4, value Val5, type Tag5, value vl, type tl),
		p_contigs_interface(value Val1, type Tag1, value Val2, type Tag2, value Val3, type Tag3, value Val4, type Tag4, value Val5, type Tag5, value vl, type tl),
		p_sequences_interface(value Val1, type Tag1, value Val2, type Tag2, value Val3, type Tag3, value Val4, type Tag4, value vl, type tl);

static dident	d_update_min,
		d_update_max,
		d_update_any,
		d_greatereq;

void
bip_elipsys_fd_init(int flags)
{
    d_update_min = in_dict("update_min", 2);
    d_update_max = in_dict("update_max", 2);
    d_update_any = in_dict("update_any", 2);
    d_greatereq = in_dict("greatereq", 3);

    if (!(flags & INIT_SHARED))
	return;

    (void) exported_built_in(in_dict("disjunctive_interface", 4),
		p_disjunctive_interface, B_UNSAFE);
    (void) exported_built_in(in_dict("disjunction_choose_interface", 6),
		p_disjunction_choose_interface, B_UNSAFE);
    (void) exported_built_in(in_dict("contigs_interface", 6),
		p_contigs_interface, B_UNSAFE);
    (void) exported_built_in(in_dict("sequences_interface", 5),
		p_sequences_interface, B_UNSAFE);
}

static void
FunifyIntLocal(pword *p, long int i)
{
  PrologWord temp_unify_int;                                                              
  Make_Integer(&temp_unify_int,(i));
  UNI_RESULT = ec_unify(*p, temp_unify_int);
}

    
static INLINE PrologWord * _Ptrbody(pword *p)
{
  PrologWord *temp;

  Var_Domain(p,temp);

  return temp;
}

static INLINE PrologWord *dereference(pword *p)
{
  PrologWord *temp;

  temp =  p;
  Dereference_(temp);
  return  temp;
}


static INLINE Int dmax(pword *p)
{
  Int max,min;

  (void) dom_range(p,&min,&max);
  return max;
}


static INLINE Int dmin(pword *p)
{
  Int max,min;

  (void) dom_range(p,&min,&max);
  return min;
}

static INLINE BOOLEAN
dupdate_min(pword *p, long int newmin, pword **list)
{
  Append_List(d_update_min,TINT,newmin,p->tag.kernel,p->val.all, list);
  return TRUE;
}


static INLINE BOOLEAN
dupdate_max(pword *p, long int newmax, pword **list)
{
    Append_List(d_update_max,TINT,newmax,p->tag.kernel,p->val.all, list);
    return TRUE;
}


static BOOLEAN setup_domain_greatereq(pword *ArgX, pword *ArgY, pword *ArgNb, pword **list)
{

  Append_List3(d_greatereq,ArgX->tag.kernel, ArgX->val.all,
	     ArgY->tag.kernel,ArgY->val.all,
	     ArgNb->tag.kernel,ArgNb->val.all, list);
  return TRUE;
}


static INLINE BOOLEAN
dremove_value(pword *p, long int v, pword **list)
{
  int res;
  PrologWord *domain;
  pword		inst;

  domain = _Ptrbody(p);

  res = dom_remove_element(domain, v, (long) TINT, &inst);

  /* Debbugging phase only */
  Assert(res == RES_ANY || res == RES_MIN || res == RES_MAX || res == RES_INSTANTIATED);

  switch (res) {
    case RES_ANY:{
      Append_List(d_update_any,TINT,v,p->tag.kernel,p->val.all, list);
      break;
    }
    case RES_MIN:{
      Append_List(d_update_min,TINT,v,p->tag.kernel,p->val.all, list);
      break;
    }
    case RES_MAX:      {
      Append_List(d_update_max,TINT,v,p->tag.kernel,p->val.all, list);
      break;
    }
    case RES_INSTANTIATED:{
      FunifyIntLocal(p,dmax(domain));
      break;
    }
  }
  return SUCCEED;
}


static INLINE BOOLEAN present(pword *domain, long int v)
{
  int res;


  res = dom_check_in(v, tint, domain);

  /* Debbugging phase only */
  Assert(res == 0 || res == 1);

  if (res == 0)
     return FAIL;
  else
     return SUCCEED;
}


static INLINE Int gmin(pword *p)
{

  if (IsInt(p)) 
     return IVal(p);
  else
     return dmin(_Ptrbody(p));
}

static INLINE Int gmax(pword *p)
{
  if (IsInt(p)) 
     return IVal(p);
  else 
     return dmax(_Ptrbody(p));
}





/* disjunctive/3 core constraint */


static int
p_disjunctive_interface(value Val1, type Tag1, value Val2, type Tag2, value Val3, type Tag3, value vl, type tl)
{

  pword P1,P2,P3;
  pword		*list = 0;
  int		res;

  CopyToPrologWord(P1,Val1.all,Tag1.kernel);
  CopyToPrologWord(P2,Val2.all,Tag2.kernel);
  CopyToPrologWord(P3,Val3.all,Tag3.kernel);

    res = disjunctive(&P1,&P2,&P3, &list);
    if (res == PSUCCEED) {
	if (list == (pword *) 0) {
	    Return_Unify_Nil(vl, tl)
	} else {
	    Return_Unify_List(vl, tl, list)
	}
    } else
	return res;
}






#define DOMAIN_MAX     200000000
#define DOMAIN_MIN     0
#define MAX_NUMBER_TASKS 128

#define MASK_POSSIBLE_LAST  (Unsigned)0x1
#define MASK_POSSIBLE_FIRST (Unsigned)0x2
#define MASK_LAST           (unsigned)0x4
#define MASK_FIRST          (Unsigned)0x8 
#define MASK_MIDDLE         (Unsigned)0x10



Unsigned states_cache[MAX_NUMBER_TASKS];
Int      increasing_starts[MAX_NUMBER_TASKS*4];
Int      increasing_ends[MAX_NUMBER_TASKS*4];

#define  _KeyDate(i)         (4*(i))
#define  _EndDate(i)         (4*(i) + 1)
#define  _Index(i)           (4*(i) + 2)

static void
siftup(long int *array, long int i, long int n)
{
  Int j;
  Int loc;

  
  while (2*i <= n) {
    j = 2*i;
    if (j < n)
       if (array[_KeyDate(j)] < array[_KeyDate(j+1)] ||
	   (array[_KeyDate(j)] == array[_KeyDate(j+1)] &&
	    array[_EndDate(j)] < array[_EndDate(j+1)]))
	  j = j + 1;
    if (array[_KeyDate(i)] < array[_KeyDate(j)] ||
	(array[_KeyDate(i)] == array[_KeyDate(j)] &&
	 array[_EndDate(i)] < array[_EndDate(j)])) {
	   
      loc = array[_KeyDate(j)];
      array[_KeyDate(j)] = array[_KeyDate(i)];
      array[_KeyDate(i)] = loc;
      
      loc = array[_EndDate(j)];
      array[_EndDate(j)] = array[_EndDate(i)];
      array[_EndDate(i)] = loc;

      loc = array[_Index(j)];
      array[_Index(j)] = array[_Index(i)];
      array[_Index(i)] = loc;

      i = j;
    }
    else
       i = n + 1;
  }
}
 

static INLINE BOOLEAN false(void)
{
  return FALSE;
}









/* To implement the proposition 9 of Carlier and Pinson's paper */
#define PROPOSITION_9  1

/* To implement the proposition 12 of Carlier and Pinson's paper */
#define PROPOSITION_12 1

/* To implement the proposition 6 and 7 of Carlier and Pinson's paper */
#define PROPOSITION_67 1

/* To enable the interruption of the constraint by other constraints */
#define PREEMPTION     0


#if __STDC__
BOOLEAN schedule_as_before(PrologWord *,PrologWord *,PrologWord *,Int,Int);
BOOLEAN schedule_as_after(PrologWord *,PrologWord *,PrologWord *,Int,Int);
#else
BOOLEAN schedule_as_before();
BOOLEAN schedule_as_after();
#endif




static BOOLEAN
disjunctive(pword *StructStarts, pword *StructDurations, pword *StructOrientations, pword **list)
{
  Int arity,sum_subset,n_tasks_to_schedule,n_partial_schedule;
  Int i,k;
  PrologWord *AuxStart,*AuxPPW;
  Unsigned value_state;
  BOOLEAN condition1,condition2,condition3;



  /* Arrays to memoize accesses and computations */

  Int min_starts[MAX_NUMBER_TASKS];
  Int max_ends[MAX_NUMBER_TASKS];
  Int durations[MAX_NUMBER_TASKS];

  Int subset[MAX_NUMBER_TASKS];
  Int total_index[MAX_NUMBER_TASKS];
  Int partial_index[MAX_NUMBER_TASKS];


#if PREEMPTION
  Int old_Bpropagation_index;
#endif

  

  arity = d_arity(Functor(StructStarts));

  /* Sort the tasks by increasing starting dates and by increasing ending dates */
  /* A priority queue is used to perform a heap sort.                           */
  /* The algorithm is taken from the Handbook of Algorithms and Data Structures */
  /* by G.H Gonnet (International Compuetr Science Series).                     */

  /* The constraint is only applied to the tasks which have not yet been scheduled */
  /* at the end or at the beginning of the schedule on the machine.                */
  /* The states held in StructStates describe whether a task has already been      */
  /* scheduled in such a fashion.                                                  */
  /* Note: This is currently not implemented.                                      */
  /*       All tasks are considered for each invokation of the constraint.         */

  n_tasks_to_schedule = 0;
  for (i = arity - 1; i >= 0 ; i --) {
    AuxPPW = dereference(StrArg(StructStarts,i));

    /* Fill up the queue and initialize the array used to memoize the computation of */
    /* starting and ending date of tasks.                                            */

    durations[i] = IVal(dereference(StrArg(StructDurations,i)));
    increasing_starts[_KeyDate(i+1)] = min_starts[i] = gmin(AuxPPW);
    increasing_starts[_EndDate(i+1)] = max_ends[i] = gmax(AuxPPW) +  durations[i];
    increasing_starts[_Index(i+1)] = i;
    increasing_ends[_KeyDate(i+1)] = gmax(AuxPPW) +  durations[i];
    increasing_ends[_EndDate(i+1)] = 0;

    /* Map the tasks in the sorted sequence to the tasks in the input data structures */
    total_index[n_tasks_to_schedule] = i;
    n_tasks_to_schedule ++;
  }

  for (i = n_tasks_to_schedule / 2; i >= 1; i --) {
    siftup(increasing_starts,i,n_tasks_to_schedule);
    siftup(increasing_ends,i,n_tasks_to_schedule);
  }

  for (i = n_tasks_to_schedule; i >= 1; i --)  {
    Int loc;

    siftup(increasing_starts,1L,i);

    loc = increasing_starts[_KeyDate(1)];
    increasing_starts[_KeyDate(1)] = increasing_starts[_KeyDate(i)];
    increasing_starts[_KeyDate(i)] = loc;

    loc = increasing_starts[_EndDate(1)];
    increasing_starts[_EndDate(1)] = increasing_starts[_EndDate(i)];
    increasing_starts[_EndDate(i)] = loc;

    loc = increasing_starts[_Index(1)];
    increasing_starts[_Index(1)] = increasing_starts[_Index(i)];
    increasing_starts[_Index(i)] = loc;

    siftup(increasing_ends,1L,i);

    loc = increasing_ends[_KeyDate(1)];
    increasing_ends[_KeyDate(1)] = increasing_ends[_KeyDate(i)];
    increasing_ends[_KeyDate(i)] = loc;

    loc = increasing_ends[_EndDate(1)];
    increasing_ends[_EndDate(1)] = increasing_ends[_EndDate(i)];
    increasing_ends[_EndDate(i)] = loc;

    loc = increasing_ends[_Index(1)];
    increasing_ends[_Index(1)] = increasing_ends[_Index(i)];
    increasing_ends[_Index(i)] = loc;

  }

  /* In the Carlier and Pinson's constraints described in their 1989 paper */
  /* they use a clique C of tasks on one machine.                          */
  /* This clique is left unspecified. One could apply the constraints to all */
  /* the possible subsets of the set of tasks. This would obviously be     */
  /* very expensive.                                                       */
  /* Reinhard Enders (SIEMENS/ZFE) has noticed that one could restrict     */
  /* oneself to the application of the constraints on maximal subsets      */
  /* where maximality is defined with regard to the following partial      */
  /* relation between set of tasks:                                        */
  /* S1 <= S2 iff min_starts(S1) = min_starts(S2) /\                       */
  /*              max_ends[(S1)   = max_ends[(S2)   /\                       */
  /*              S1 included in S2                                        */

  /* Iteration on all the maximal subsets                                  */


  
  {
    Int ptr_starts,ptr_ends;
    Int xcurrent_start,xcurrent_end;
    Int ptr_subset;
    Int latest_end,current_end;
    Int earliest_start,current_start;

    /* To be able to detect interruptions */
#if PREEMPTION
    old_Bpropagation_index = Bpropagation_index;
#endif

    for (ptr_starts = 1; ptr_starts <= n_tasks_to_schedule;) {
      for (ptr_ends = 1 ; ptr_ends <= n_tasks_to_schedule; ptr_ends ++){ 

	/* The minimum starting time  in a maximal subset can only be smaller */
	/* than its maximumum ending time.                                    */

	if (increasing_starts[_KeyDate(ptr_starts)] > increasing_ends[_KeyDate(ptr_ends)])
	   continue;
	
	/* Skip to the last task with the current ending date. Hence all tasks with */
	/* ending dates smaller than the current ending date will be included in the*/
	/* maximal subset.                                                          */

	{
	  Int temp_ending_date;
	  temp_ending_date = increasing_ends[_KeyDate(ptr_ends)];
	  ptr_ends ++;
	  for (;ptr_ends <= n_tasks_to_schedule &&
	       increasing_ends[_KeyDate(ptr_ends)] == temp_ending_date;
	       ptr_ends ++);
	  ptr_ends --;
	}
	/* Initialise the position descriptor of a task within all possible schedules */
	/* of the current maximal subset.                                             */

	for (i = n_tasks_to_schedule - 1; i >= 0; i --)  {
	  states_cache[total_index[i]] = MASK_POSSIBLE_FIRST | MASK_POSSIBLE_LAST;
	  subset[total_index[i]] = 0;
	}

	
	n_partial_schedule = 0;
	sum_subset = 0;
	latest_end = DOMAIN_MIN;
	earliest_start = DOMAIN_MAX;

	for (ptr_subset = ptr_starts; ptr_subset <= n_tasks_to_schedule; 
	     ptr_subset ++) {
	  Int index_i;

	  xcurrent_start = increasing_starts[_KeyDate(ptr_subset)];
	  xcurrent_end = increasing_starts[_EndDate(ptr_subset)];

	  /* Halts the loop - End of the maximal subset */
	  if (xcurrent_start > increasing_ends[_KeyDate(ptr_ends)])
	     break;

	  /* This task ends later that the current maximum end in the maximal */
	  /* subset beeig constructed. It cannot be put in the maximal subset */ 
	  if (xcurrent_end > increasing_ends[_KeyDate(ptr_ends)])
	     continue;

	  /* Records the index in the input data structure of the tasks in the maximal subset */

	  index_i = partial_index[n_partial_schedule] = increasing_starts[_Index(ptr_subset)];

	  /* Records which tasks in t input data structure are in the current */
	  /* maximal subset.                                                  */

	  subset[partial_index[n_partial_schedule]] = 1;

	  /* Compute the sum of the durations of the tasks in the curent maximal subset */

	  sum_subset = sum_subset + durations[partial_index[n_partial_schedule]];

	  /* Compute the minimum starting date and the maximu ending date of the tasks in the */
	  /* maximal subset.                                                                  */

	  current_end = max_ends[index_i];
	  current_start = min_starts[index_i];
	  if (latest_end < current_end) {
	    latest_end = current_end;
	  }
	  if (earliest_start > current_start) {
	    earliest_start = current_start;
	  }
	  
	  n_partial_schedule ++;
	}

	/* Empty maximal subset nothing to do */
	if (n_partial_schedule == 0)
	   continue;


	{
      {
	    for (k = n_tasks_to_schedule - 1; k >= 0; k --) {
	      Int index_k = total_index[k];
	      if (subset[index_k] != 1) {
		value_state = states_cache[index_k];


		/* Condition 1 */
		if (value_state & MASK_POSSIBLE_FIRST) {

		  /* Test whether task k can be scheduled as the first of the tasks   */
		  /* which have not yet been scheduled at the beginning or at the end */
		  /* of any schedule of the maximal subset                            */

		  if (min_starts[index_k] + sum_subset + durations[index_k] > latest_end ) {
		    states_cache[index_k] =  states_cache[index_k] & ~MASK_POSSIBLE_FIRST;
		  } 
		}


		/* Condition 2 */
		if (value_state & MASK_POSSIBLE_LAST) {

		  /* Test whether task k can be scheduled as the first of the tasks   */
		  /* which have not yet been scheduled at the beginning or at the end */
		  /* of any schedule of the maximal subset                            */

		  if (earliest_start + sum_subset + durations[index_k]> max_ends[index_k]){
		    states_cache[index_k] =  states_cache[index_k] & ~MASK_POSSIBLE_LAST;
		  }
		}
	      }
	    }
	  }
	}


#if PROPOSITION_67
	{
	  /* For all operations in the subset which have not yet been scheduled at the end   */
	  /* or at the beginning of the partial schedule, apply the proposition 6 and 7.     */
	  /* The following variables are used:                                               */

	  /* index_k:    index on the tasks we try to position with respect to the current   */
	  /*             subset.                                                             */
	  /* condition1: BOOLEAN if set to TRUE means that the index_k task can be scheduled */
	  /*             after the current subset.                                           */
	  /* condition2: BOOLEAN if set to TRUE means that the index_k task can be scheduled */
	  /*             before the current subset.                                          */
	  /* condition3: BOOLEAN if set to TRUE means that the index_k task can not be       */
	  /*             scheduled within the  current subset.                               */
		     

	  /* For all tasks .... */
	  for (k = n_tasks_to_schedule - 1 ; k >= 0; k -- ) {
	    Int index_k = total_index[k];
	    BOOLEAN already_scheduled;

	    /* ... not in the current maximal subset */
	    if (subset[index_k] != 1) {

#if PREEMPTION
	      if (Bpropagation_index != old_Bpropagation_index)
		 return PREEMPTED;
#endif


	      /* Detect whether the task could be scheduled within the boundaries of the */
	      /* partial schedule.                                                       */
		
	      /* We can do it by testing whether there is enough room within the         */
	      /* boundaries of the partial schedule.                                     */

	      if (earliest_start + sum_subset  + durations[index_k] > latest_end)
		 condition3 = TRUE;
	      else
		 condition3 = FALSE;

	      /* We can also do it by checking the pairwise orientations of the task */
	      /* and the tasks within the partial schedule.                          */

	      condition1 = condition2 = FALSE;
	      already_scheduled = FALSE;


	      if (_False(condition3)) {
		condition3 = TRUE;
		for (i = n_partial_schedule - 1  ;  i >= 0; i -- )  {
		  Int index_i = partial_index[i];
		  Int ix;
		  if (index_k < index_i) {
		    ix = index_k*arity + index_i;
		    AuxPPW = dereference(StrArg(StructOrientations,ix));
		    if (IsInt(AuxPPW)) {

		      /* Debugging phase only */
		      Assert(IVal(AuxPPW) == 1 || IVal(AuxPPW) == 2);

		      if (IVal(AuxPPW) == 2) {
			condition1 = TRUE;

			/* If the task can be before some tasks and  after some others */
			/* it must be within the partial schedule.                     */

			if (_True(condition2)) {
			  condition3 = FALSE;
			  break;
			}
		      }
		      
		      else  {
			
			condition2 = TRUE;

			/* If the task can be before some tasks and  after some others */
			/* it must be within the partial schedule.                     */
			  
			if (_True(condition1)) {
			  condition3 = FALSE;
			  break;
			}
		      }
		    }
		    else {
		      /* If no orientation is given for a pair there is nothing to do */
		      condition3 = FALSE;
		      break;
		    }
		  }
		  else 
		     if (index_k > index_i) {
		       ix = index_i*arity + index_k;
		       AuxPPW = dereference(StrArg(StructOrientations,ix));
		       if (IsInt(AuxPPW)) {

			 /* Debugging phase only */
			 Assert(IVal(AuxPPW) == 1 || IVal(AuxPPW) == 2);


			 if (IVal(AuxPPW) == 2) {
			   condition2 = TRUE;

			   /* If the task can be before some tasks and  after some others */
			   /* it must be within the partial schedule.                     */

			   if (_True(condition1)) {
			     condition3 = FALSE;
			     break;
			   }

			 }
			 else {
			   condition1 = TRUE;

			   /* If the task can be before some tasks and  after some others */
			   /* it must be within the partial schedule.                     */

			   if (_True(condition2)) {
			     condition3 = FALSE;
			     break;
			   }
			 }
		       }
		       else {
			 /* If no orientation is given for a pair there is nothing to do */
			 condition3 = FALSE;
			 break;
		       }
		    
		     }
		}

		/* The operation can be in "the middle of the subset". Nothing to be done */
		if (_False(condition3))
		   continue;
		else
		   already_scheduled = TRUE;
	      }

	      if (_False(condition3))
		 continue;


	      /* if we get to here we have been able to decide that the task  was either */
	      /* before or after the subset.                                             */
		
	      /* if we do not yet know on which side it should be then we can try to find out */

	      /* if we already know we do not do anything */
	      if (_False(condition1) && _False(condition2)) {
		value_state = states_cache[index_k];
		if (value_state == MASK_POSSIBLE_FIRST) {
		  condition2 = TRUE;
		}
		else
		   condition2 = FALSE;
	    
		if (value_state == MASK_POSSIBLE_LAST) {
		  condition1 = TRUE;
		}
		else
		   condition1 = FALSE;



		/* We inspect the array of orientations describing how tasks are  */
		/* pairwise oriented.                                             */
		/* If we find an orientation which is already set we can conclude */

		if (_False(condition1) && _False(condition2)) {
		  for (i = n_partial_schedule - 1 ;  i >= 0; i -- )  {
		    Int index_i = partial_index[i];
		    Int ix;

		    /* Debugging phase only */
		    Assert(index_k != index_i);

		    if (index_k < index_i) {
		      ix = index_k*arity + index_i;
		      AuxPPW = dereference(StrArg(StructOrientations,ix));
		      if (IsInt(AuxPPW)) {

			/* Debugging phase only */
			Assert(IVal(AuxPPW) == 1 || IVal(AuxPPW) == 2);

			if (IVal(AuxPPW) == 2) {
			  condition1 = TRUE;
			  condition2 = FALSE;
			}
			else {
			  condition1 = FALSE;
			  condition2 = TRUE;
			}
			break;
		      }
		    }

		    /* index_k > index_i */

		    else {
		      ix = index_i*arity + index_k;
		      AuxPPW = dereference(StrArg(StructOrientations,ix));
		      if (IsInt(AuxPPW)) {

			/* Debugging phase only */
			Assert(IVal(AuxPPW) == 1 || IVal(AuxPPW) == 2);

			if (IVal(AuxPPW) == 2) {
			  condition1 = FALSE;
			  condition2 = TRUE;
			}
			else  {
			  condition1 = TRUE;
			  condition2 = FALSE;
			}
			break;
		      }

		    }
		  }

		  /* If we still have not been able to decide then we can not do anything */
		  if (_False(condition1) && _False(condition2))
		     continue;
		}
	      }


	      /* Debugging phase only */
	      Assert(condition1 != condition2);


	      /* Proposition 6 */
	      if (_True(condition1)) { 

		states_cache[index_k] = MASK_LAST;
		if (_False(already_scheduled)) {
		  for (i = n_partial_schedule - 1; i >= 0; i --) {
		    Int index_i = partial_index[i];
		    if (_False(schedule_as_after(StructOrientations,StructStarts,StructDurations,
						 index_k,index_i)))
		       return false();
		  }
		}

#if PROPOSITION_9
		{
		  /* Proposition 9 */

		    

		  AuxStart= dereference(StrArg(StructStarts,index_k));
		  if (IsInt(AuxStart)) {


		    if (earliest_start + sum_subset > IVal(AuxStart))
		       return false();
		  }
		  else {

		    if (_False(dupdate_min(AuxStart,earliest_start + sum_subset, list)))
		       return false();

		    /* Maintain the coherence of the auxilliary data structures */

		    {
		      AuxStart = dereference(StrArg(StructStarts,index_k));
		      min_starts[index_k] = gmin(AuxStart);
		    }

		  }
		}
#endif				/* PROPOSITION_9 */
	      
		/* Next operation */
		continue;
	      }

	      /* Proposition 7 */
	      if (_True(condition2)) {

		states_cache[index_k] = MASK_FIRST;
		if (_False(already_scheduled)) {
		  for ( i = n_partial_schedule -1 ; i >= 0; i --) {
		    Int index_i = partial_index[i];
		    if (_False(schedule_as_before(StructOrientations,StructStarts,StructDurations,
						  index_k,index_i)))
		       return false();
		  }
		}


#if PROPOSITION_9 
		/* Proposition 9 */
		{

		  AuxStart = dereference(StrArg(StructStarts,index_k));
		  if (IsInt(AuxStart)) {

		    if (latest_end - sum_subset < IVal(AuxStart) + durations[index_k])
		       return false();
		  }

		  else {

		    if (_False(dupdate_max(AuxStart,latest_end - sum_subset - durations[index_k], list)))
		       return false();

		    /* Maintain the coherence of the auxilliary data structures */
		    {
		      AuxStart= dereference(StrArg(StructStarts,index_k));
		      max_ends[index_k] = gmax(AuxStart) + durations[index_k];
		    }
		  }

		}
#endif				/* PROPOSITION_9 */

		/* Next operation */
		continue;
	      }





	    }
	  }

	}
#endif				/* PROPOSITION_67 */



#if PROPOSITION_12
	  
	/* Proposition 12 */
	  
	{
	  Int min_end_possible_firsts;
	  Int max_start_possible_lasts;
	  Int temp;

	  min_end_possible_firsts = DOMAIN_MAX;
	  max_start_possible_lasts = DOMAIN_MIN;

	  for (i = n_partial_schedule - 1; i >= 0; i --) {
	    Int index_i = partial_index[i];
	    Int duration_i = durations[index_i];

	    temp = max_ends[index_i] - duration_i;
	    if (max_start_possible_lasts < temp)
	       max_start_possible_lasts = temp;

	    temp = min_starts[index_i] + duration_i;
	    if (min_end_possible_firsts > temp)
	       min_end_possible_firsts = temp;
	  }


	  for (i = n_tasks_to_schedule - 1; i >= 0; i --) {
	    Int index_i = total_index[i];
	    if (subset[index_i] != 1) {
	      value_state = states_cache[index_i];
	      AuxStart = dereference(StrArg(StructStarts,index_i));

	      if (value_state == MASK_POSSIBLE_FIRST){
		if (IsInt(AuxStart)) {
		  if (IVal(AuxStart) + durations[index_i] > max_start_possible_lasts)
		     return false();
		}
		else {
		  if (_False(dupdate_max(AuxStart,max_start_possible_lasts-durations[index_i], list)))
		     return false();

		  /* Maintain the coherence of the cache data structures */
		  AuxStart= dereference(StrArg(StructStarts,index_i));
		  max_ends[index_i] = gmax(AuxStart) + durations[index_i];
		}
	      }



	      if (value_state ==  MASK_POSSIBLE_LAST){
		if (IsInt(AuxStart)) {
		  if (IVal(AuxStart) < min_end_possible_firsts )
		     return false();
		}
		else {
		  if (_False(dupdate_min(AuxStart, min_end_possible_firsts, list)))
		     return false();


		  /* Maintain the coherence of the cache data structures */
		  AuxStart= dereference(StrArg(StructStarts,index_i));
		  min_starts[index_i] = gmin(AuxStart);


		}
	      }
	    }
	  }
	}
#endif				/* PROPOSITION_12 */


      }

      /* Skip the tasks which have the same starting date has the current starting */
      /* date for the maximal subsets.                                             */
      /* If they were not skipped the constraint might consider subsets of maximal */
      /* subsets.                                                                  */
      /* For instance in the following case:                                       */
      /*                                                                           */
      /*  increasing_starts: [10 , 20] [10, 30] [ 10, 40] [12 20] ...              */
      /*  increasing_ends  :                     ........ [15 20] [15 30] [15 40]  */
      /* after generating the maximal subset with starting date 10, one should     */
      /* subsets with 12 as minimal starting date.                                 */

      {
	Int temp_starting_date;
	temp_starting_date = increasing_starts[_KeyDate(ptr_starts)];

	for (;ptr_starts <= n_tasks_to_schedule &&
	     increasing_starts[_KeyDate(ptr_starts)] == temp_starting_date;
	     ptr_starts++);
      }

    }
  }

  return DELAY;

}





#undef  _KeyDate
#undef  _EndDate
#undef  _Index







/* i before j */
/*ARGSUSED*/
BOOLEAN schedule_as_before(pword *StructOrientations, pword *StructStarts, pword *StructDurations, long int i, long int j)
{
  Int index;
  Int arity;
  

  arity = d_arity(Functor(StructStarts));
 
  /* Debugging phase only */
  Assert(i != j);


  /* j < i */
  if (j < i) {
    index = j*arity + i;

    FunifyIntLocal(StrArg(StructOrientations,index),2L);
    if (_False(UNI_RESULT))
       return false();

  }

  /* i < j */
  else  {
    index = i*arity + j;

    FunifyIntLocal(StrArg(StructOrientations,index),1L);
    if (_False(UNI_RESULT))
       return false();

  }
  return SUCCEED;
}

/* i after j */
/*ARGSUSED*/
BOOLEAN schedule_as_after(pword *StructOrientations, pword *StructStarts, pword *StructDurations, long int i, long int j)
{
  Int index;
  Int arity;
  


  /* Debugging phase only */
  Assert(i != j);

  arity = d_arity(Functor(StructStarts));

  /* j < i */
  if (j < i) {
    index = j*arity + i;
    FunifyIntLocal(StrArg(StructOrientations,index),1L);
    if (_False(UNI_RESULT))
       return false();


  }

  /* i < j */
  else {
    index = i*arity + j;

    FunifyIntLocal(StrArg(StructOrientations,index),2L);
    if (_False(UNI_RESULT))
       return false();

  }

  return SUCCEED;
}


static int
p_contigs_interface(value Val1, type Tag1, value Val2, type Tag2, value Val3, type Tag3, value Val4, type Tag4, value Val5, type Tag5, value vl, type tl)
{

  pword P1,P2,P3,P4,P5;
  pword		*list = 0;
  int		res;

  CopyToPrologWord(P1,Val1.all,Tag1.kernel);
  CopyToPrologWord(P2,Val2.all,Tag2.kernel);
  CopyToPrologWord(P3,Val3.all,Tag3.kernel);
  CopyToPrologWord(P4,Val4.all,Tag4.kernel);
  CopyToPrologWord(P5,Val5.all,Tag5.kernel);

    res = contigs(&P1,&P2,&P3,&P4,&P5, &list);
    if (res == PSUCCEED) {
	if (list == (pword *) 0) {
	    Return_Unify_Nil(vl, tl)
	} else {
	    Return_Unify_List(vl, tl, list)
	}
    } else
	return res;
}


static INLINE BOOLEAN false_contigs(void)
{
  return FALSE;
}



static BOOLEAN contigs(pword *StructTable, pword *Sequences, pword *Item, pword *Occurences, pword *Contigs, pword **list)
{
  PrologWord * Table;
  Int end;

  /* Smallest length of subsequence of item */
  Int at_least_length;

  /* Greatest length of a possible sequence of items found */
  /* during one iteration.                                 */
  Int at_most_length;

  /* End of a longest possible sequence of items found    */
  /* during one iteration.                                */
  Int at_most_length_end;

  /* Number of subsequences of the longest possible length */
  /* found during one iteration.                           */
  Int at_most_length_count;

  /* Greatest number of occurences of item in one of the */
  /* longest possible subsequences of item.              */
  Int at_most_placed;


  /* Non volatile (not reset at each iteration) length of a */
  /* longest possible sequence of item.                     */
  Int non_volatile_greatest_length;


  /* contains the current length of the current possible sequence */
  Int length_possible_sequence;

  Int length_prefix_possible_sequence;

  /* contains the number of occurence of items in the current     */
  /* possible sequence.                                           */
  Int occurences_in_possible_sequence;

  /* contains the length of the current sequence of items.  */
  Int length_current_sequence;

  Int item_value;
  Int upper_limit_length_subsequence;
  Int lower_limit_length_subsequence;

  /* The maximal number of occurences in the sequence*/
  Int upper_limit_occurences;


  Int at_least_occurences;
  Int at_most_occurences;


  /* The minimal number of occurences in the sequence */
  Int lower_limit_occurences;

  /* Counter for the dvariables present in the sequence */
  Int count_dvars;

  /* Length of the sequence of items */
  Int length_global_sequence;


  BOOLEAN new_sequence;
  Int at_most_contigs, at_least_contigs;
  Int counter_contigs;

  Int remaining_values;
  Int counter_different_items;
  Int purge_done;
  BOOLEAN iterate;


  Occurences = dereference(Occurences);
  Sequences = dereference(Sequences);
  Contigs = dereference(Contigs);

  /* Debugging phase only */
  Assert(IsDomVar(Occurences) || IsInt(Occurences));
  Assert(IsDomVar(Sequences) || IsInt(Sequences));
  Assert(IsDomVar(Contigs) || IsInt(Contigs));
  Assert(IsInt(Item));
  Assert(IsStruct(StructTable));


  Table = StructArgs(StructTable);
  end  = d_arity(Functor(StructTable)) - 1;
  length_global_sequence  = d_arity(Functor(StructTable));

  purge_done = FALSE;
  item_value = IVal(Item);


  /* Set up the limits for the possible subsequences of items within */
  /* the sequence.                                                   */

  if (IsInt(Sequences)) {
    upper_limit_length_subsequence = IVal(Sequences);
    lower_limit_length_subsequence = IVal(Sequences);
  }
  else {
    upper_limit_length_subsequence = dmax(_Ptrbody(Sequences));
    lower_limit_length_subsequence = dmin(_Ptrbody(Sequences));
  }


  /* Set up the upper limit for the number of times the item is present */
  /* in the sequence.                                                   */           

  if (IsInt(Occurences)) {
    upper_limit_occurences = IVal(Occurences);
    lower_limit_occurences = IVal(Occurences);
  }
  else {
    upper_limit_occurences = dmax(_Ptrbody(Occurences));
    lower_limit_occurences = dmin(_Ptrbody(Occurences));
  }

  /* Set up the upper limit for the number of contigs in the sequence */

  if (IsInt(Contigs)) {
    at_most_contigs = IVal(Contigs);
    at_least_contigs = IVal(Contigs);
  }
  else {
    at_most_contigs = dmax(_Ptrbody(Contigs));
    at_least_contigs = dmin(_Ptrbody(Contigs));
  }



  non_volatile_greatest_length = 0;

  /* The iteration loop.                 */
  /* Loop until the constraint fixpoints */
  do 
     {
       Int i;
       PrologWord *temp_item;

       iterate = FALSE;
       count_dvars = 0;

       remaining_values = upper_limit_occurences;
       counter_different_items = 0;

       length_current_sequence = 0;
       length_possible_sequence = 0;
       length_prefix_possible_sequence = 0;
       occurences_in_possible_sequence = 0;


       at_most_length = 0;
       at_most_length_count = 0;
       at_most_length_end = 0;
       at_least_length = 0;
       at_most_placed = 0;
       
       new_sequence = TRUE;
       counter_contigs = 0;

       /* A trivial case of failure */
       /* The shortest sequence contains more items than is allowed */

       if (lower_limit_length_subsequence > upper_limit_occurences)
	  return false_contigs();


       /* Scan the global sequence */
       for ( i = 0; i <= end; i ++) {

	 temp_item = dereference(&Table[i]);



	 /* Update the counters and recorders */
	 if (at_least_length < length_current_sequence)  {
	   at_least_length = length_current_sequence;
	 }

	 if (at_most_length <= length_possible_sequence) {
	   at_most_length = length_possible_sequence;

	   /* To record the position of the latest encountered longest possible */
	   /* sequence.                                                         */
	   if (length_possible_sequence == non_volatile_greatest_length) {
	     at_most_length_count ++;
	     at_most_length_end = i;
	   }
	 }

	 if (at_most_placed < occurences_in_possible_sequence) {
	   at_most_placed = occurences_in_possible_sequence;
	 }


	 /* Trivial case of failure */
	 /* The start of the current sequence contains already more */
	 /* items than is allowed.                                  */

	 if (length_current_sequence > upper_limit_length_subsequence)
	    return false_contigs();
	  

	 if (IsInt(temp_item)) {

	   /* One more item ... */
	   if (IVal(temp_item) == item_value) {
	     
	     /* ....  but no more items are allowed */
	     if (remaining_values == 0)
		return false_contigs();

	     /* If it is the beginning of a new sequence one should record this */
	     if (_True(new_sequence)) {
	       counter_contigs ++;
	       new_sequence = FALSE;
	     }

	     /* The current and possible sequence goes on */
	     length_current_sequence ++;

	     if (length_possible_sequence == length_prefix_possible_sequence)
		length_prefix_possible_sequence ++;

	     length_possible_sequence ++;
	     occurences_in_possible_sequence ++;

	     remaining_values --;

	   }

	   else {

	     
	     /* Start a new sequence */
	     length_current_sequence = 0;
	     new_sequence = TRUE;

	     /* Start a new possible sequence */
	     length_possible_sequence = 0;
	     occurences_in_possible_sequence = 0;
	     
	     /* Count the number of items different from the allowed one */
	     counter_different_items ++;
	   }

	   /* Scan the rest of the global sequence */
	   continue;
	 }


	 /* Fuzzy element ... */
	 if (IsDvar(temp_item)) {
 
	   count_dvars += 1;

	   /* The item can be at the current place */
	   if (_True(present(_Ptrbody(temp_item),item_value))) {


	     /* No more items are available to append to the current sequence */
	     /* Hence the current element can not have the item as value.     */

	     if (remaining_values == 0) {
	       /* Satisfy lint */
	       if (dremove_value(temp_item,item_value, list)) {;}
	       iterate = TRUE;
	       
	       /* Start a new sequence */
	       length_current_sequence = 0;

	       /* Start a new possible sequence */
	       length_possible_sequence = 0;
	       length_prefix_possible_sequence = 0;
	       occurences_in_possible_sequence = 0;

	       /* Update the counter of the number of items different from the */
	       /* right one.                                                  */
	       counter_different_items ++;

	       /* Scan the rest of the global sequence */
	       continue;
	     }


	     /* It is not allowed         to have more items in this  */
	     /* sequence because its length is already the maximum    */
	     /* possible.                                             */

	     if (length_current_sequence == upper_limit_length_subsequence) {
	       /* Satisfy lint */
	       if (dremove_value(temp_item,item_value, list)){;}
	       iterate = TRUE;
	       
	       /* Start a new sequence */
	       length_current_sequence = 0;

	       /* Start a new possible sequence */
	       length_possible_sequence = 0;
	       length_prefix_possible_sequence = 0;
	       occurences_in_possible_sequence = 0;

	       /* Update the counter of the number of items different from the */
	       /* right one.                                                  */
	       counter_different_items ++;

	       /* Scan the rest of the global sequence */
	       continue;
	     }

	     /* If a sequence of the minimum length has not yet been found*/
	     /* some available items must be used NOW if we hope to be    */
	     /* able to fill a sequence of the minimum length.            */

	     if (remaining_values < lower_limit_length_subsequence &&
		 at_least_length < lower_limit_length_subsequence) {

	       FunifyIntLocal(temp_item,item_value);
	       if (_False(UNI_RESULT))
		  return false_contigs();

	       iterate = TRUE;

	       remaining_values --;

	       /* The sequence goes on */
	       length_current_sequence ++;

	       /* The possible sequence goes on */
	       length_possible_sequence ++;
	       length_prefix_possible_sequence ++;

	       /* Scan the rest of the global sequence */
	       continue;
	     }


	     /* Nothing definitive can be said about the current element.     */
	     /* The current sequence stops here whereas the possible sequence */
	     /* can go on.                                                    */
	     {

	       /* Start a new sequence */
	       length_current_sequence = 0;

	       /* The possible sequence goes on */
	       length_possible_sequence ++;

	       /* Scan the rest of the global sequence */
	       continue;
	     }
	   }

	   /* The item can not be at the current place */
	   else {

	     /* The current sequence and the possible sequence must stop here */
	     {
	       /* Start a new sequence */
	       length_current_sequence = 0;
	       new_sequence = TRUE;

	       /* Start a new possible sequence */
	       length_possible_sequence = 0;
	       length_prefix_possible_sequence = 0;
	       occurences_in_possible_sequence = 0;

	       /* Update the counter of the number of items different from the */
	       /* right one.                                                  */
	       counter_different_items ++;

	       /* Scan the rest of the global sequence */
	       continue;
	     }
	   }
	 }
       }


       /* Update the counters and recorders */
       if (at_least_length < length_current_sequence)  {
	 at_least_length = length_current_sequence;
       }

       if (at_most_length < length_possible_sequence) {
	 at_most_length = length_possible_sequence;
       }

       if (at_most_placed < occurences_in_possible_sequence) {
	 at_most_placed = occurences_in_possible_sequence;
       }

       if (at_most_length < length_possible_sequence) {
	 at_most_length = length_possible_sequence;
       }

       /* The scan of the global sequence has been done in one direction hence */
       /* some information discovered in the later steps of the scan should    */
       /* back propagated.                                                     */

       /* 1/                                                                   */
       /* There are two many different items. The minimal number of occurences */
       /* can not be reached any more.                                         */
       if (length_global_sequence - counter_different_items < lower_limit_occurences) {
	 return false_contigs();
       }

       /* 2/                                                                   */
       /* the number of items different from the right one can only be known   */
       /* after a complete scan. An additional iteration is needed when  the   */
       /* number of occurences is reduced.                                     */
       if (length_global_sequence - counter_different_items < upper_limit_occurences) {
	 upper_limit_occurences = length_global_sequence - counter_different_items;
	 iterate = TRUE;
	 continue;
       }
       
       /* 3/                                                                    */
       /* The maximum number of available items has been found in the global    */
       /* sequence (information only available after the completion of the scan)*/
       /* Hence the item must be removed from the domain of all variables       */
       /* remaining in the sequence.                                            */

       if (remaining_values == 0 && count_dvars != 0 &&  _False(purge_done)) {
	 purge_done = TRUE;
	 for ( i = 0; i <= end; i ++) {
	   temp_item = dereference(&Table[i]);
	   if (IsDvar(temp_item)) {
	     /* Satisfy lint */
	     if (dremove_value(temp_item,item_value, list)) {;}
	     iterate = TRUE;
	   }
	 }
	 continue;
       }

       /* 4/                                                                    */
       /* There are remaining items to be put somewhere and there is exactly the*/
       /* corresponding amount of empty slots. The items MUST fit in these slots*/

       if (upper_limit_occurences - remaining_values + count_dvars 
	   == lower_limit_occurences){
	 for ( i = 0; i <= end; i ++) {
	   temp_item = dereference(&Table[i]);
	   if (IsDvar(temp_item)) {
	     if (_True(present(_Ptrbody(temp_item),item_value))) {
	       FunifyIntLocal(temp_item,item_value);
	       if (_False(UNI_RESULT))
		  return false_contigs();
	       iterate = TRUE;
	     }
	     else
		return false_contigs();
	   }
	   continue;
	 }
       }

       /* 5/                                                                   */
       /* Record the length of the longest sequence ever found                 */
       if (non_volatile_greatest_length < at_most_length) {
	 non_volatile_greatest_length = at_most_length;
	 iterate = TRUE;
	 continue;
       }

       /* 6/                                                                   */
       /* If the least count of possible contigs is already equal to the       */
       /* maximal allowed count of contigs then the holes in the global        */
       /* sequence which can potentially create new contigs should be filled   */
       /* accordingly.                                                         */
       
       /*        1 _ _ _ _ 1 1 1 _ _ _ 1   and ncontigs == 1                   */
       /* can be automatically transformed into:                               */
       /*        1 1 1 1 1 1 1 1 1 1 1 1                                       */

       if (counter_contigs == at_most_contigs) { 
	 Int temp_start,j;
	 PrologWord *temp_ppw;

	 temp_start = -1;
	 for ( i = 0; i <= end; i ++) {
	   temp_item = dereference(&Table[i]);
	   if (IsInt(temp_item) && IVal(temp_item) == item_value) {

	     /* If we have found the end of a       possible contig then we fill */
	     /* the contig.                                                      */
	     if (temp_start >= 0) {
	       for (j = temp_start + 1; j < i; j ++) {

		 /* If there is an item in the list whose value has not been fixed    */
		 /* and which is between two items which have the searched value, the */
		 /* not yet fixed item must be given the searched value. Otherwise the*/
		 /* number of contigs would increase.                                 */

		 temp_ppw = dereference(&Table[j]);
		 if (!IsInt(temp_ppw)) {
		   FunifyIntLocal(temp_ppw,item_value);
		   if (_False(UNI_RESULT))
		      return false_contigs();
		   iterate = TRUE;
		 }
	       }
	     
	       /* The end of this contig is the start of the next one */
	       temp_start = i;
	     }
	     else {

	       /* The start of a contig */
	       temp_start = i;
	     }
	   }
	 }
       }

       

     } while (_True(iterate));


  /* Propagation on size of the longest chunk of consecutive value */
  /* in the sequence.                                              */

  /* Already Dereferenced */
  if (IsDvar(Sequences)) {

    /* The least possible number of items present in the sequence */
    /* is known and can propagated to the domain variable which   */
    /* gives the possible values for this length.                 */

    if (_False(dupdate_min(Sequences,at_least_length, list)))
       return false_contigs();
    
    Sequences = dereference(Sequences);
    

    
    /* The greatest possible number of items present in the sequence */
    /* is known and can propagated to the domain variable whic       */
    /* gives the possible values for this length.                    */

    /* Maybe the domain variable has been reduced to an integer by the */
    /* previous update.                                                */

    if (IsInt(Sequences)) {
      if (IVal(Sequences) > at_most_length)
	 return false_contigs();
    }

    if (IsDvar(Sequences)){
      if (_False(dupdate_max(Sequences,at_most_length, list)))
	 return false_contigs();
    }
  }

  else if (IsInt(Sequences)) {

    /* Fail when there is necessarily more items than required */
    if (at_least_length > IVal(Sequences))
       return false_contigs();

    /* If there is only place where a sequence of the correct maximal */
    /* length can be found, then the sequence must be put there.      */

    if (at_most_length_count == 1 && at_most_length == IVal(Sequences)) {
      Int i;
      for (i = at_most_length_end - at_most_length - 1;
	   i < at_most_length_end;
	   i ++) 
	 {
	   PrologWord *temp;
	   temp = dereference(&Table[i]);
	   if (IsDvar(temp)) {
	     FunifyIntLocal(temp,item_value);
	     if (_False(UNI_RESULT))
		return false_contigs();
	   }
	 }
    }
  }

  /* Check whether there is enough occurences of the item so as to */
  /* enable to have a subsequnce of the minimum length.            */

  Sequences = dereference(Sequences);

  {
    Int min_length_necessary;

    if (IsInt(Sequences))
       min_length_necessary = IVal(Sequences);
    else
       min_length_necessary = dmin(_Ptrbody(Sequences));

    if (at_least_length < min_length_necessary)
       if ((at_most_placed + remaining_values) < min_length_necessary) 
	  return false_contigs();
  }


  /* Propagation on the number of times the value is present */
  /* in the sequence.                                        */


  /* There are too many items or not enough items in the sequence */
  /* Already Dereferenced */

  if (IsInt(Occurences)) {

    /* Not enough */
    if (count_dvars == 0 && remaining_values != 0)
       return false_contigs();

    /* Too many */
  }

  else if (IsDvar(Occurences)) {
    at_least_occurences = upper_limit_occurences -  remaining_values;
    at_most_occurences = length_global_sequence - counter_different_items;

    /* When no variables are left the number of occurences of the item in the */
    /* sequence is known.                                                     */
    if (count_dvars == 0) {
      if (_True(present(_Ptrbody(Occurences), at_least_occurences))) {
	FunifyIntLocal(Occurences,at_least_occurences);
	if (_False(UNI_RESULT))
	   return false_contigs();
      }
      else 
	 return false_contigs();
    }
      
    else {

      /* The minimal number of occurences is known; it can be */
      /* propagated to the domain variable describing the     */
      /* possible occurences number.                          */

      if (_False(dupdate_min(Occurences,at_least_occurences, list)))
	 return false_contigs();

      Occurences= dereference(Occurences);

      if (IsInt(Occurences))
	 if (IVal(Occurences) > at_most_occurences)
	    return false_contigs();

      if (IsDvar(Occurences)) 
	 if _False(dupdate_max(Occurences,at_most_occurences, list))
	    return false_contigs();
    }
  }


  if (at_least_contigs < counter_contigs)
     at_least_contigs = counter_contigs;

  if (count_dvars == 0) {
    at_most_contigs = counter_contigs;
    at_least_contigs = counter_contigs;
  }

  if (IsInt(Contigs)) {
    if (IVal(Contigs) < at_least_contigs)
       return false_contigs();
    if (IVal(Contigs) > at_most_contigs)
       return false_contigs();
  }
  else {
    if (_False(dupdate_min(Contigs,at_least_contigs, list)))
       return false_contigs();

    Contigs = dereference(Contigs);

    if (IsInt(Contigs)) {
       if (IVal(Contigs) > at_most_contigs)
	  return false_contigs();
     }
    else {
      if (_False(dupdate_max(Contigs,at_most_contigs, list)))
	 return false_contigs();
    }
       
  }

 
  if (count_dvars == 0)
     return SUCCEED;

  return DELAY;
}



static int
p_sequences_interface(value Val1, type Tag1, value Val2, type Tag2, value Val3, type Tag3, value Val4, type Tag4, value vl, type tl)
{

  pword P1,P2,P3,P4;
  pword		*list = 0;
  int		res;

  CopyToPrologWord(P1,Val1.all,Tag1.kernel);
  CopyToPrologWord(P2,Val2.all,Tag2.kernel);
  CopyToPrologWord(P3,Val3.all,Tag3.kernel);
  CopyToPrologWord(P4,Val4.all,Tag4.kernel);

    res = sequences(&P1,&P2,&P3,&P4, &list);
    if (res == PSUCCEED) {
	if (list == (pword *) 0) {
	    Return_Unify_Nil(vl, tl)
	} else {
	    Return_Unify_List(vl, tl, list)
	}
    } else
	return res;
}



static INLINE BOOLEAN false_sequences(void)
{
  return FALSE;
}

static BOOLEAN sequences(pword *StructTable, pword *Sequences, pword *Item, pword *Occurences, pword **list)
{
  PrologWord * Table;
  Int end;

  /* Smallest length of subsequence of item */
  Int at_least_length;

  /* Greatest length of a possible sequence of items found */
  /* during one iteration.                                 */
  Int at_most_length;

  /* End of a longest possible sequence of items found    */
  /* during one iteration.                                */
  Int at_most_length_end;

  /* Number of subsequences of the longest possible length */
  /* found during one iteration.                           */
  Int at_most_length_count;

  /* Greatest number of occurences of item in one of the */
  /* longest possible subsequences of item.              */
  Int at_most_placed;


  /* Non volatile (not reset at each iteration) length of a */
  /* longest possible sequence of item.                     */
  Int non_volatile_greatest_length;



  /* contains the current length of the current possible sequence */
  Int length_possible_sequence;

  Int length_prefix_possible_sequence;

  /* contains the number of occurence of items in the current     */
  /* possible sequence.                                           */
  Int occurences_in_possible_sequence;

  /* contains the length of the current sequence of items.  */
  Int length_current_sequence;

  Int item_value;
  Int upper_limit_length_subsequence;
  Int lower_limit_length_subsequence;

  /* The maximal number of occurences in the sequence*/
  Int upper_limit_occurences;




  Int at_least_occurences;
  Int at_most_occurences;


  /* The minimal number of occurences in the sequence */
  Int lower_limit_occurences;

  /* Counter for the dvariables present in the sequence */
  Int count_dvars;

  /* Length of the sequence of items */
  Int length_global_sequence;


  Int remaining_values;
  Int counter_different_items;

  Int purge_done;

  BOOLEAN iterate;


  Occurences = dereference(Occurences);
  Sequences = dereference(Sequences);

  /* Debugging phase only */
  Assert(IsDomVar(Occurences) || IsInt(Occurences));
  Assert(IsDomVar(Sequences) || IsInt(Sequences));
  Assert(IsInt(Item));
  Assert(IsStruct(StructTable));


  Table = StructArgs(StructTable);
  end  = d_arity(Functor(StructTable)) - 1;
  length_global_sequence  = d_arity(Functor(StructTable));

  purge_done = FALSE;
  item_value = IVal(Item);


  /* Set up the limits for the possible subsequences of items within */
  /* the sequence.                                                   */

  if (IsInt(Sequences)) {
    upper_limit_length_subsequence = IVal(Sequences);
    lower_limit_length_subsequence = IVal(Sequences);
  }
  else {
    upper_limit_length_subsequence = dmax(_Ptrbody(Sequences));
    lower_limit_length_subsequence = dmin(_Ptrbody(Sequences));
  }


  /* Set up the upper limit for the number of times the item is present */
  /* in the sequence.                                                   */           

  if (IsInt(Occurences)) {
    upper_limit_occurences = IVal(Occurences);
    lower_limit_occurences = IVal(Occurences);
  }
  else {
    upper_limit_occurences = dmax(_Ptrbody(Occurences));
    lower_limit_occurences = dmin(_Ptrbody(Occurences));
  }

  /* A trivial case of failure */
  /* The shortest sequence contains more items than is allowed */
  if (lower_limit_length_subsequence > upper_limit_occurences)
     return false_sequences();


  non_volatile_greatest_length = 0;

  /* The iteration loop.                 */
  /* Loop until the constraint fixpoints */
  do 
     {
       Int i;
       PrologWord *temp_item;

       iterate = FALSE;
       count_dvars = 0;

       remaining_values = upper_limit_occurences;
       counter_different_items = 0;

       length_current_sequence = 0;
       length_possible_sequence = 0;
       length_prefix_possible_sequence = 0;
       occurences_in_possible_sequence = 0;


       at_most_length = 0;
       at_most_length_count = 0;
       at_most_length_end = 0;
       at_least_length = 0;
       at_most_placed = 0;
       

       /* Scan the global sequence */
       for ( i = 0; i <= end; i ++) {

	 temp_item = dereference(&Table[i]);



	 /* Update the counters and recorders */
	 if (at_least_length < length_current_sequence)  {
	   at_least_length = length_current_sequence;
	 }

	 if (at_most_length <= length_possible_sequence) {
	   at_most_length = length_possible_sequence;

	   if (length_possible_sequence == non_volatile_greatest_length) {
	     at_most_length_count ++;
	     at_most_length_end = i;
	   }
	 }

	 if (at_most_placed < occurences_in_possible_sequence) {
	   at_most_placed = occurences_in_possible_sequence;
	 }


	 /* Trivial case of failure */
	 /* The start of the current sequence contains already more */
	 /* items than is allowed.                                  */

	 if (length_current_sequence > upper_limit_length_subsequence)
	    return false_sequences();
	  

	 if (IsInt(temp_item)) {

	   /* One more item ... */
	   if (IVal(temp_item) == item_value) {
	     
	     /* ....  but no more items are allowed */
	     if (remaining_values == 0)
		return false_sequences();

	     /* The current and possible sequence go on */
	     length_current_sequence ++;

	     if (length_possible_sequence == length_prefix_possible_sequence)
		length_prefix_possible_sequence ++;

	     length_possible_sequence ++;
	     occurences_in_possible_sequence ++;

	     remaining_values --;

	   }

	   else {

	     /* Start a new sequence */
	     length_current_sequence = 0;

	     /* Start a new possible sequence */
	     length_possible_sequence = 0;
	     occurences_in_possible_sequence = 0;
	     
	     /* Count the number of items different from the allowed one */
	     counter_different_items ++;
	   }

	   /* Scan the rest of the global sequence */
	   continue;
	 }


	 /* Fuzzy element ... */
	 if (IsDvar(temp_item)) {
 
	   count_dvars += 1;

	   /* The item can be at the current place */
	   if (_True(present(_Ptrbody(temp_item),item_value))) {


	     /* No more items are available to append to the current sequence */
	     /* Hence the current element can not have the item as value.     */

	     if (remaining_values == 0) {
	       /* Satisfy lint */
	       if (dremove_value(temp_item,item_value, list)) {;}
	       iterate = TRUE;
	       
	       /* Start a new sequence */
	       length_current_sequence = 0;

	       /* Start a new possible sequence */
	       length_possible_sequence = 0;
	       length_prefix_possible_sequence = 0;
	       occurences_in_possible_sequence = 0;

	       /* Update the counter of the number of items different from the */
	       /* right one.                                                  */
	       counter_different_items ++;

	       /* Scan the rest of the global sequence */
	       continue;
	     }


	     /* It is not allowed         to have more items in this  */
	     /* sequence because its length is already the maximum    */
	     /* possible.                                             */

	     if (length_current_sequence == upper_limit_length_subsequence) {
	       /* Satisfy lint */
	       if (dremove_value(temp_item,item_value, list)){;}
	       iterate = TRUE;
	       
	       /* Start a new sequence */
	       length_current_sequence = 0;

	       /* Start a new possible sequence */
	       length_possible_sequence = 0;
	       length_prefix_possible_sequence = 0;
	       occurences_in_possible_sequence = 0;

	       /* Update the counter of the number of items different from the */
	       /* right one.                                                  */
	       counter_different_items ++;

	       /* Scan the rest of the global sequence */
	       continue;
	     }

	     /* If a sequence of the minimum length has not yet been found*/
	     /* some available items must be used NOW if we hope to be    */
	     /* able to fill a sequence of the minimum length.            */

	     if (remaining_values < lower_limit_length_subsequence &&
		 at_least_length < lower_limit_length_subsequence) {

	       FunifyIntLocal(temp_item,item_value);
	       if (_False(UNI_RESULT))
		  return false_sequences();

	       iterate = TRUE;

	       remaining_values --;

	       /* The sequence goes on */
	       length_current_sequence ++;

	       /* The possible sequence goes on */
	       length_possible_sequence ++;
	       length_prefix_possible_sequence ++;

	       /* Scan the rest of the global sequence */
	       continue;
	     }


	     /* Nothing definitive can be said about the current element.     */
	     /* The current sequence stops here whereas the possible sequence */
	     /* can go on.                                                    */
	     {

	       /* Start a new sequence */
	       length_current_sequence = 0;

	       /* The possible sequence goes on */
	       length_possible_sequence ++;

	       /* Scan the rest of the global sequence */
	       continue;
	     }
	   }

	   /* The item can not be at the current place */
	   else {

	     /* The current sequence and the possible sequence must stop here */
	     {
	       /* Start a new sequence */
	       length_current_sequence = 0;

	       /* Start a new possible sequence */
	       length_possible_sequence = 0;
	       length_prefix_possible_sequence = 0;
	       occurences_in_possible_sequence = 0;

	       /* Update the counter of the number of items different from the */
	       /* right one.                                                  */
	       counter_different_items ++;

	       /* Scan the rest of the global sequence */
	       continue;
	     }
	   }
	 }
       }


       /* Update the counters and recorders */
       if (at_least_length < length_current_sequence)  {
	 at_least_length = length_current_sequence;
       }

       if (at_most_length < length_possible_sequence) {
	 at_most_length = length_possible_sequence;
       }

       if (at_most_placed < occurences_in_possible_sequence) {
	 at_most_placed = occurences_in_possible_sequence;
       }

       if (at_most_length < length_possible_sequence) {
	 at_most_length = length_possible_sequence;

       }


       /* The scan of the global sequence has been done in one direction hence */
       /* some information discovered in the later steps of the scan should    */
       /* back propagated.                                                     */

       /* 1/                                                                   */
       /* There are two many different items. The minimal number of occurences */
       /* can not be reached any more.                                         */
       if (length_global_sequence - counter_different_items < lower_limit_occurences) {
	 return false_sequences();
       }

       /* 2/                                                                   */
       /* the number of items different from the right one can only be known   */
       /* after a complete scan. An additional iteration is needed when  the   */
       /* number of occurences is reduced.                                     */
       if (length_global_sequence - counter_different_items < upper_limit_occurences) {
	 upper_limit_occurences = length_global_sequence - counter_different_items;
	 iterate = TRUE;
	 continue;
       }
       
       /* 3/                                                                    */
       /* The maximum number of available items has been found in the global    */
       /* sequence (information only available after the completion of the scan)*/
       /* Hence the item must be removed from the domain of all variables       */
       /* remaining in the sequence.                                            */

       if (remaining_values == 0 && count_dvars != 0 &&  _False(purge_done)) {
	 purge_done = TRUE;
	 for ( i = 0; i <= end; i ++) {
	   temp_item = dereference(&Table[i]);
	   if (IsDvar(temp_item)) {
	     /* Satisfy lint */
	     if (dremove_value(temp_item,item_value, list)) {;}
	     iterate = TRUE;
	   }
	 }
	 continue;
       }

       /* 4/                                                                    */
       /* There are remaining items to be put somewhere and there is exactly the*/
       /* corresponding amount of empty slots. The items MUST fit in these slots*/

       if (upper_limit_occurences - remaining_values + count_dvars 
	   == lower_limit_occurences){
	 for ( i = 0; i <= end; i ++) {
	   temp_item = dereference(&Table[i]);
	   if (IsDvar(temp_item)) {
	     FunifyIntLocal(temp_item,item_value);
	     if (_False(UNI_RESULT))
		return false_sequences();
	     iterate = TRUE;
	   }
	   continue;
	 }
       }

       /* 5/                                                                   */
       /* Record the length of the longest sequence ever found                 */
       if (non_volatile_greatest_length < at_most_length) {
	 non_volatile_greatest_length = at_most_length;
	 iterate = TRUE;
	 continue;
       }



     } while (_True(iterate));


  /* Propagation on size of the longest chunk of consecutive value */
  /* in the sequence.                                              */

  /* Already Dereferenced */
  if (IsDvar(Sequences)) {

    /* The least possible number of items present in the sequence */
    /* is known and can propagated to the domain variable which   */
    /* gives the possible values for this length.                 */

    if (_False(dupdate_min(Sequences,at_least_length, list)))
       return false_sequences();
    
    Sequences = dereference(Sequences);
    

    
    /* The greatest possible number of items present in the sequence */
    /* is known and can propagated to the domain variable whic       */
    /* gives the possible values for this length.                    */

    /* Maybe the domain variable has been reduced to an integer by the */
    /* previous update.                                                */

    if (IsInt(Sequences)) {
      if (IVal(Sequences) > at_most_length)
	 return false_sequences();
    }

    if (IsDvar(Sequences)){
      if (_False(dupdate_max(Sequences,at_most_length, list)))
	 return false_sequences();
    }
  }

  else if (IsInt(Sequences)) {

    /* Fail when there is necessarily more items than required */
    if (at_least_length > IVal(Sequences))
       return false_sequences();

    /* If there is only place where a sequence of the correct maximal */
    /* length can be found, then the sequence must be put there.      */

    if (at_most_length_count == 1 && at_most_length == IVal(Sequences)) {
      Int i;
      for (i = at_most_length_end - at_most_length - 1;
	   i < at_most_length_end;
	   i ++) 
	 {
	   PrologWord *temp;
	   temp = dereference(&Table[i]);
	   if (IsDvar(temp)) {
	     FunifyIntLocal(temp,item_value);
	     if (_False(UNI_RESULT))
		return false_sequences();
	   }
	 }
    }
  }

  /* Check whether there is enough occurences of the item so as to */
  /* enable to have a subsequnce of the minimum length.            */

  Sequences = dereference(Sequences);

  {
    Int min_length_necessary;

    if (IsInt(Sequences))
       min_length_necessary = IVal(Sequences);
    else
       min_length_necessary = dmin(_Ptrbody(Sequences));

    if (at_least_length < min_length_necessary)
       if ((at_most_placed + remaining_values) < min_length_necessary) 
	  return false_sequences();
  }


  /* Propagation on the number of times the value is present */
  /* in the sequence.                                        */


  /* There are too many items or not enough items in the sequence */
  /* Already Dereferenced */

  if (IsInt(Occurences)) {

    /* Not enough */
    if (count_dvars == 0 && remaining_values != 0)
       return false_sequences();

    /* Too many */
  }

  else if (IsDvar(Occurences)) {
    at_least_occurences = upper_limit_occurences -  remaining_values;
    at_most_occurences = length_global_sequence - counter_different_items;

    /* When no variables are left the number of occurences of the item in the */
    /* sequence is known.                                                     */
    if (count_dvars == 0) {
      if (_True(present(_Ptrbody(Occurences), at_least_occurences))) {
	FunifyIntLocal(Occurences,at_least_occurences);
	if (_False(UNI_RESULT))
	   return false_sequences();
      }
      else 
	 return false_sequences();
    }
      
    else {

      /* The minimal number of occurences is known; it can be */
      /* propagated to the domain variable describing the     */
      /* possible occurences number.                          */

      if (_False(dupdate_min(Occurences,at_least_occurences, list)))
	 return false_sequences();

      Occurences = dereference(Occurences);

      if (IsInt(Occurences))
	 if (IVal(Occurences) > at_most_occurences)
	    return false_sequences();

      if (IsDvar(Occurences)) 
	 if _False(dupdate_max(Occurences,at_most_occurences, list))
	    return false_sequences();
    }
  }



 
  if (count_dvars == 0)
     return SUCCEED;

  return DELAY;

  
}


static int
p_disjunction_choose_interface(value Val1, type Tag1, value Val2, type Tag2, value Val3, type Tag3, value Val4, type Tag4, value Val5, type Tag5, value vl, type tl)
{

  pword P1,P2,P3,P4,P5;
  pword		*list = 0;
  int		res;

  CopyToPrologWord(P1,Val1.all,Tag1.kernel);
  CopyToPrologWord(P2,Val2.all,Tag2.kernel);
  CopyToPrologWord(P3,Val3.all,Tag3.kernel);
  CopyToPrologWord(P4,Val4.all,Tag4.kernel);
  CopyToPrologWord(P5,Val5.all,Tag5.kernel);

    res = disjunction_choose(&P1,&P2,&P3,&P4,&P5, &list);
    if (res == PSUCCEED) {
	if (list == (pword *) 0) {
	    Return_Unify_Nil(vl, tl)
	} else {
	    Return_Unify_List(vl, tl, list)
	}
    } else
	return res;
}



static BOOLEAN disjunction_choose(pword *x, pword *Dx, pword *y, pword *Dy, pword *branch, pword **list)
{
  PrologWord *X,*Y,*Branch;
  BOOLEAN x_after_y = FALSE;
  BOOLEAN y_after_x = FALSE;
  BOOLEAN res;
  PrologWord *dx, *dy;

  Branch = dereference(branch);
  dx = dereference(Dx);
  dy = dereference(Dy);
  x = dereference(x);
  y = dereference(y);

  /* Debugging phase only */
  Assert(IsDomVar(Branch) || IsInt(Branch));
  Assert(IsInt(dx) && IsInt(dy));

  /* An easy case which does not require to go through all the machinery */

  if (IsInt(Branch)) {
    if (IVal(Branch) == 1) {

      /* From now on the constraint is going to behave has an inequality */
      /* Instead of keeping it active as such, it better to replace it by*/
      /* a true inequality which is less reactive to the updates of its  */
      /* arguments. Useless wake-ups will then be avoided.               */

      res = setup_domain_greatereq(y,x,dx, list);

      /* Solve this constraint */
      if (res == DELAY) 
	 return SUCCEED;
      return res;
    }
    if (IVal(Branch) == 2) {

      /* From now on the constraint is going to behave has an inequality */
      /* Instead of keeping it active as such, it better to replace it by*/
      /* a true inequality which is less reactive to the updates of its  */
      /* arguments. Useless wake-ups will then be avoided.               */

      res = setup_domain_greatereq(x,y,dy, list);

      /* Solve this constraint */
      if (res == DELAY) 
	 return SUCCEED;
      return res;
    }

    /* Should never get there */
    Assert(0);
  }


  X = dereference(x);
  Y = dereference(y);

  y_after_x = TRUE;
  x_after_y = TRUE;

  if (gmin(X) + IVal(dx) > gmax(Y))
     y_after_x = FALSE;
  if (gmin(Y) + IVal(dy) > gmax(X))
     x_after_y = FALSE;

  if (_False(y_after_x) && _False(x_after_y))
     return FAIL;
  
  if (_False(y_after_x)) {
    FunifyIntLocal(Branch,2L);

    res = setup_domain_greatereq(x,y,dy, list);

    /* Solve this constraint */
    if (res == DELAY) 
       return SUCCEED;
    return res;
  }

  if (_False(x_after_y)) {
    FunifyIntLocal(Branch,1L);

    res = setup_domain_greatereq(y,x,dx, list);

    /* Solve this constraint */
    if (res == DELAY) 
       return SUCCEED;
    return res;
  }


  return DELAY;
}

