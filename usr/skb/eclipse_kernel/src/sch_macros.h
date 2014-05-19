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

/**********************************************************************
**      System: Parallel ECLiPSe Scheduler
**        File: sch_macros.h
**      Author: Liang-Liang Li
** Description: Macro Definitions
**
***********************************************************************/

/**********************************************************************
** Runtime Consistency Checking
***********************************************************************/

#if defined(__STDC__)
/*
    extern int fprintf(FILE * stream, const char * format, ...);
    extern int printf(const char * format, ...);
*/
#else /* __STDC__ */
    extern int fprintf();
    extern int printf();
#endif /* __STDC__ */

#if defined(PRINTAM)

#define assert(ex) {					\
    if (!(ex)) {					\
	void sch_break();				\
	sch_break();					\
        (void) fprintf(stderr, "Assertion Failed at ");	\
        (void) fprintf(stderr, "file \"%s\"", __FILE__);\
        (void) fprintf(stderr, " line %d\n", __LINE__); \
	(void) ec_panic("Assertion Failed", "Scheduler");	\
    }							\
}
#else /* PRINTAM */
#define assert(ex)
#endif /* PRINTAM */

#define error(err) {                                    \
        (void) fprintf(stderr, "error: \"%s\"", (err)); \
        assert(zero());                                 \
}


#if defined(__STDC__)
int global_flags(int,int);
#else /* __STDC__ */
int global_flags();
#endif /* __STDC__ */

#define SCH_TRACE_FLAG  0x00000008
#define sch_trace_on (global_flags(0,0)&SCH_TRACE_FLAG)

#if defined(SDEBUG) || defined(NSHDLNOTICE)

#define Sch_Trace_Begin()
#define Sch_Event_Notify1(name, type, t1)
#define Sch_Event_Notify2(name, type, t1, t2)
#define Sch_Event_Notify3(name, type, t1, t2, t3)

#else /* SDEBUG || NSHDLNOTICE */

#define Sch_Trace_Begin() {	\
	printf("BEGIN\n");	\
}
#define Sch_Event_Notify1(name, type, t1) {				\
    if (sch_trace_on)							\
	printf( "%s %s ' %lx %lx %lx '\n", name, type,			\
	   (unsigned long)Site(t1),(unsigned long)Knot(t1),(unsigned long)Edge(t1));	\
}
#define Sch_Event_Notify2(name, type, t1, t2) {				\
    if (sch_trace_on)							\
	printf("%s %s ' %lx %lx %lx '  ' %lx %lx %lx '\n", name, type,	\
	   (unsigned long)Site(t1),(unsigned long)Knot(t1),(unsigned long)Edge(t1),	\
	   (unsigned long)Site(t2),(unsigned long)Knot(t2),(unsigned long)Edge(t2));	\
}
#define Sch_Event_Notify3(name, type, t1, t2, t3) {			\
    if (sch_trace_on)							\
	printf("%s %s ' %lx %lx %lx '  ' %lx %lx %lx '  ' %lx %lx %lx '\n", name, type, \
	   (unsigned long)Site(t1),(unsigned long)Knot(t1),(unsigned long)Edge(t1),	\
	   (unsigned long)Site(t2),(unsigned long)Knot(t2),(unsigned long)Edge(t2),	\
	   (unsigned long)Site(t3),(unsigned long)Knot(t3),(unsigned long)Edge(t3));	\
}
#endif /* SDEBUG || NSHDLNOTICE */

#define Smsg_Hdl_Notify(smsg, recv, send) {		\
	if (ComnSite((recv),(send)))			\
	    Scheduler(Site(recv))->smsg_count_intra++;	\
	Scheduler(Site(recv))->smsg_count_hdl++;	\
	Scheduler(Site(recv))->smsg_subcount[smsg]++;	\
	Sch_Event_Notify2("RECV_SMSG",smsg_name[(smsg)], (send), (recv)); \
}
#define Smsg_Snd_Notify(smsg, recv, send) {		\
	Scheduler(Site(send))->smsg_count_snd++;	\
	Sch_Event_Notify2("SEND_SMSG",smsg_name[(smsg)], (send), (recv)); \
}

#define InitSchOffset(site) (Scheduler(site)->sch_base = 0)
#define Scheduler(site)     (&(scheduler[(site)-(site)]))
#define LeafEngine(leaf)    (Scheduler(Site(leaf))->engine)
#define SiteLeaf(site)      ((st_id_t *)&(Scheduler(site)->leaf))
#define NewSiteLeaf(leaf)   {			\
	Scheduler(Site(leaf))->lmp = 0;		\
	*SiteLeaf(Site(leaf)) = *(leaf);	\
}

/* Info about a scheduler branch (subtree) */
#define INFO_LIFE	0x0f
#define INFO_LMP	0x10
#define INFO_RICH	0x20
#define INFO_POOR_SPINE	0x40

/* Life status of a scheduler branch */
#define ST_ALIVE        0x1
#define ST_CHOPPED      0x2
#define ST_LODGED       0x3
#define ST_CUT          0x4
#define ST_BACKTRACK    0x6
#define ST_DYING        0x7
#define ST_DEAD         0x8
#define ST_INIT         0x9
#define ST_IDLE         0xa
#define ST_IDLING       0xb

/* (st_id_t *) '(t)' */ 
#define PackSite(t,s)	(Site(t) = (s))
#define PackKnot(t,k) ((t)->knot = (unsigned long)(k)-Scheduler(Site(t))->sch_base)
#define PackEdge(t,e) ((t)->edge = (unsigned long)(e)-Scheduler(Site(t))->sch_base)

#define Site(t) ((t)->site)
#define Knot(t) ((st_knot_t *)(Scheduler(Site(t))->sch_base + (t)->knot))
#define Edge(t) ((st_edge_t *)(Scheduler(Site(t))->sch_base + (t)->edge))

#define ComnSite(t1,t2) (Site(t1)==Site(t2))
#define ComnKnot(t1,t2) (Knot(t1)==Knot(t2))
#define ComnEdge(t1,t2) (Edge(t1)==Edge(t2))

/* Trunk: the edge leading to the suptree;
** Twig : the edge leading to one subtree.
*/
#define Trunk(t) (&(Knot(t)->trunk))
#define Twig(t) Edge(t)

#define SupTree(t) (&(Trunk(t)->tree))
#define SubTree(t) (&(Twig(t)->tree))

#define K2T(k) ((st_edge_t *) (k))
#define T2K(b) ((st_knot_t *) (b))

#define IsLeaf(t) (Knot(t)->tip)
#define IsNode(t) (!IsLeaf(t))

#define SchRoot(t) (Knot(t)==Knot(SupTree(t))&&ComnSite((t),SupTree(t)))
#define JsRoot(t) (Knot(t)->jroot)
#define PsRoot(t) (Knot(t)->proot)
#define Local(t) (Knot(t)->local)
#define Hybrid(t) (Knot(t)->hybrid)
#define SetJsRoot(t) (Knot(t)->jroot = 1)
#define ResetJsRoot(t) (Knot(t)->jroot = 0)
#define SetPsRoot(t) (Knot(t)->proot = 1)
#define ResetPsRoot(t) (Knot(t)->proot = 0)
#define SetLocal(t) (Knot(t)->local = 1)
#define ResetLocal(t) (Knot(t)->local = 0)

#define Twigless(t) (Trunk(t)==Trunk(t)->next)
#define SetTwigless(t)  {Trunk(t)->next = Trunk(t); Trunk(t)->prev = Trunk(t);}

#define Exhausted(t) (!Knot(t)->nxtcls)
#define SetExhausted(t) { Knot(t)->nxtcls = 0; Knot(t)->nxtcls_b = 0; }
#define Tk_NextClause(t)  (Knot(t)->nxtcls   ? Knot(t)->nxtcls-- : 0)
#define Bk_NextClause(t)  (Knot(t)->nxtcls_b ? Knot(t)->nxtcls_b-- : 0)
#define Bk_NextClause0(t) { if (Knot(t)->nxtcls_b) Knot(t)->nxtcls_b--; }

#define AliveTwigs(t) (Knot(t)->alive_twigs)
#define Monad(t) (AliveTwigs(t)==1)
#define YoungestTwig(t) (Trunk(t)->prev == Twig(t))

#define Suspended(t) (Knot(t)->suspended)

#define NilTreeId(t) (PackSite(t,0))
#define IsNilTreeId(t) (Site(t)==0)

/* The super tree area for an incarnated leaf is overloaded to
** point to its corpse. This is needed only for handling 'cut'.
*/
#define Corpse(t) SupTree(t)

#define Life(b)		((b)->info & INFO_LIFE)
#define Rest(b)		((b)->info & ~INFO_LIFE)

#define Alive(b)	(Life(b) == ST_ALIVE)
#define Chopped(b)	(Life(b) == ST_CHOPPED)
#define Lodged(b)	(Life(b) == ST_LODGED)
#define Idle(b)		(Life(b) == ST_IDLE)
#define Idling(b)	(Life(b) == ST_IDLING)
#define Dying(b)	(Life(b) == ST_DYING)
#define SetAlive(b)	((b)->info = (Rest(b) | ST_ALIVE))
#define SetNalive(b,s) { (b)->info = (Rest(b) | (s)); SetPoor(b); }

#define PoorSpine(t)	(Trunk(t)->info &  INFO_POOR_SPINE)
#define SetPoorSpine(t)	(Trunk(t)->info |= INFO_POOR_SPINE)
#define SetRichSpine(t)	(Trunk(t)->info &= ~INFO_POOR_SPINE)

#define Rich(b)		((b)->info & INFO_RICH)
#define Eldest(b)	((b)->info & INFO_LMP)
#define SetRich(b)	((b)->info |= INFO_RICH)
#define SetEldest(b)	((b)->info |= INFO_LMP)
#define SetPoor(b)	((b)->info &= ~INFO_RICH)
#define ResetEldest(b)	((b)->info &= ~INFO_LMP)


#define Reincarnat(old,new) {			\
	*Corpse(new) = *(old);			\
	SetNalive(Trunk(new), ST_CUT);		\
	SetNalive(Trunk(old), ST_DYING);	\
	ResetJsRoot(old);			\
	assert(!Suspended(old));		\
	Suspended(new) = (st_susp_t *) 0;	\
}

#define Quit_Chain(b) {				\
	(b)->prev->next = (b)->next;		\
	(b)->next->prev = (b)->prev;		\
}

#if defined(PRINTAM)

#define check_no_duplicate(b,n) {			\
	st_edge_t *x = (n);				\
	do {						\
	   assert(x != (b));				\
	   x = x->next;					\
	} while (x != (n));				\
}
#else /* PRINTAM */
#define check_no_duplicate(b,n)
#endif /* PRINTAM */

#define Join_Chain_Next(b,n) {			\
	/* check_no_duplicate((b),(n)); */	\
	(b)->next = (n)->next;			\
	(b)->prev = (n);			\
	(n)->next = (b);			\
	(b)->next->prev = (b);			\
}

/* Create an alive twig, leading to a leaf
** Add_Alive_Twig(st_id_t *, st_id_t *, st_knot_t *)
*/

#define Add_Alive_Twig_Next(twig,leaf,knot,next) {\
	Sch_Alloc_Edge(Site(knot),(twig));	\
	PackKnot((twig),(knot));		\
	SetAlive(Edge(twig));			\
        *SubTree(twig) = *(leaf);		\
	Join_Chain_Next(Edge(twig),(next));	\
	(knot)->alive_twigs++;			\
}

/* Add_Lodge_Twig(st_id_t *,st_id_t *,st_knot_t *)
*/
#define Add_Lodge_Twig(twig,leaf,knot) {	\
	Sch_Alloc_Edge(Site(knot),(twig));	\
	PackKnot((twig),(knot));		\
						\
        SetNalive(Edge(twig),ST_LODGED);	\
        *SubTree(twig) = *(leaf);		\
						\
	Join_Chain_Next(Edge(twig),Trunk(twig)->prev);\
}

/* Intra-Site scheduler message flows are to be short-cut.
** A special attention is given to the tail-recursive cases.
** And assume atomic msg handling supported.
*/

#if !defined(NO_SHORTCUT)
/*
** void Smsg_ShortCut_Mid(st_id_t *,st_id_t *,void,void)
**      this function will not give up holding the 'from'
*/
#define Smsg_ShortCut_Mid(from,to,hdl,snd) {	\
	if ( ComnSite((from),(to)) &&		\
	     (!Intrasite_Smsg(Site(from))) &&	\
	     Sch_Lock(to) ) {			\
	   Inc_ShortCut_Count(Site(from));	\
	   (hdl);				\
	} else  {				\
	   (snd);				\
	}					\
}

/*
** void Smsg_ShortCut_End(st_id_t *, st_id_t *,void,void)
**      this function will give up holding 'from'
*/
#define Smsg_ShortCut_End(from,to,hdl,snd) {	\
	if ( ComnSite((from),(to)) &&		\
	     (!Intrasite_Smsg(Site(from))) &&	\
	     Sch_Lock(to) ) {			\
	   Sch_Unlock(from);			\
	   Inc_ShortCut_Count(Site(from));	\
	   (hdl);				\
	} else {				\
           (snd);				\
           Sch_Unlock(from);			\
        }					\
}

#else /* NO_SHORTCUT */

/*
** void Smsg_ShortCut_Mid(st_id_t *,st_id_t *,void,void) 
**      this function will not give up holding the 'from'
*/
#define Smsg_ShortCut_Mid(from,to,hdl,snd) (snd)

/*
** void Smsg_ShortCut_End(st_id_t *, st_id_t *,void,void)
**      this function will give up holding 'from' 
*/
#define Smsg_ShortCut_End(from,to,hdl,snd) {	\
        (snd);					\
        Sch_Unlock(from);			\
}

#endif  /* NO_SHORTCUT */

#define Sch_JS_Install(self, lodg, coma, leaf) {	\
	st_id_t twig;					\
	st_edge_t *x = Trunk(self)->prev;		\
	st_edge_t *y = (st_edge_t *) 0;			\
	while (x != Trunk(self)) {			\
	   if (Alive(x)||Idle(x)) {			\
	      y = x;					\
	      if (ComnSite(self,&(x->tree)))		\
		 break;					\
	   }						\
           x = x->prev;					\
	}						\
	if (y) {					\
	   twig = *(self);				\
	   PackEdge(&twig,y);				\
	   Smsg_ShortCut_End(&twig, SubTree(&twig),	\
sch_msg_hdl_js_install(SubTree(&twig),&twig,(lodg),(coma),(leaf)),	\
sch_msg_snd_js_install(SubTree(&twig),&twig,(lodg),(coma),(leaf))	\
		);					\
	   return;					\
	}						\
}

#if defined(PRINTAM)
#define CheckAllPoor(x) {				\
	st_edge_t *y = (x)->next;			\
	while (y!=(x)) {				\
	   assert(!Alive(y) || !Rich(y));		\
	   y = y->next;					\
	}						\
}
#else /* PRINTAM */
#define CheckAllPoor(x)
#endif /* PRINTAM */


#define Sch_JS_Trav_Up(self,coma,leaf)	{		\
	if (!Alive(Trunk(self)) ||			\
	   (!JsRoot(self) && !(Local(self) && ComnSite((self),(leaf))))) { \
	   st_id_t *ncoma = (coma);			\
	   if (ComnNode((self),(coma))) {		\
	      ncoma = SupTree(self);			\
	      SetPoorSpine(self);			\
	   }						\
	   Smsg_ShortCut_End((self), SupTree(self),	\
		sch_msg_hdl_js_trav_up(SupTree(self),(self),ncoma,(leaf)),\
		sch_msg_snd_js_trav_up(SupTree(self),(self),ncoma,(leaf))\
	       );					\
	} else if (AliveTwigs(self) ||			\
		  !(Local(self) && ComnSite((self),(leaf)))) { \
	   st_susp_t *x;				\
	   CheckAllPoor(Trunk(self));			\
	   Sch_Alloc_Suspended(Site(self), x);		\
	   x->next = Suspended(self);			\
	   x->leaf = *(leaf);				\
	   x->coma = *(coma);				\
	   Suspended(self) = x;				\
	   if (JsRoot(self))				\
	      SetRich(Trunk(self));			\
	   Sch_Unlock(self);				\
	} else {					\
	   int alt0 = Hybrid(self) ? (-1):0;		\
	   SetNalive(Trunk(self), ST_DYING);		\
	   Smsg_ShortCut_End((self),SupTree(self),	\
		sch_msg_hdl_straighten(SupTree(self),(self),(leaf),(self),alt0),\
		sch_msg_snd_straighten(SupTree(self),(self),(leaf),(self),alt0) \
		);					\
	}						\
}

#define Next_X(s,x) ((Scheduler(s)->left_first) ? (x)->next : (x)->prev)
#define Sch_JS_Trav_Down(self,coma,leaf)	{	\
	if (Rich(Trunk(self))) {			\
	   st_edge_t *x = Next_X(Site(self),Trunk(self));\
	   st_id_t twig;				\
	   while (x != Trunk(self)) {			\
	      if (Alive(x) && Rich(x)) {		\
		 twig = *self;				\
		 PackEdge(&twig,x);			\
		 Smsg_ShortCut_End(&twig, SubTree(&twig),\
	      sch_msg_hdl_js_trav_dn(SubTree(&twig),&twig,(coma),(leaf)),\
	      sch_msg_snd_js_trav_dn(SubTree(&twig),&twig,(coma),(leaf))\
		      );				\
		 return;				\
	      }						\
	      x = Next_X(Site(self), x);		\
	   }						\
	   if (!Knot(self)->nxtcls_b && !JsRoot(self)) {\
	      SetPoor(Trunk(self));			\
	   }						\
	}						\
}

#define Sch_WakeUp_Suspended(self) {			\
	   st_susp_t * x;				\
	   while (x= Suspended(self)) {			\
	      Smsg_ShortCut_Mid((self),&(x->leaf),	\
		   sch_msg_hdl_js_in_vain(&(x->leaf),(self)),	\
		   sch_msg_snd_js_in_vain(&(x->leaf),(self))	\
	          );					\
	      Suspended(self) = x->next;		\
	      Sch_Gc_Suspended(Site(self),x);		\
	   }						\
}

#define Sch_Alloc_Knot(s,t) {				\
	Sch_Alloc_Edge((s),(t))				\
	PackKnot((t),T2K(Edge(t)));			\
	*Knot(t) = *knot_template;			\
	SetTwigless(t);					\
}

#define Sch_Gc_Knot(t) Sch_Gc_Edge(Site(t),Trunk(t))

#define Sch_Alloc_Edge(site, tree) {			\
	edge_buffer_t *b = &(Scheduler(site)->edge_buffer); \
	if (b->count > 20 && Simp_Lock(b)) {		\
	    b->count--;					\
	    PackEdge((tree),b->head.trunk.prev);	\
	    Quit_Chain(Edge(tree));			\
	    Simp_Unlock(b);				\
	} else {					\
	    PackEdge((tree), (st_edge_t *)		\
			hp_alloc(sizeof(st_knot_t)));	\
	}						\
	PackSite((tree),(site));			\
	Edge(tree)->info = 0;				\
}

#if !defined(SEPARATE_INSTALL)
#define None_Install_Req(leaf)
#else /* SEPARATE_INSTALL */
#define None_Install_Req(leaf) assert(!Install_Req(Site(leaf)))
#define Install_Req(site) (Scheduler(site)->install_req)
#define Sch_Gc_Inst_Req(site, req)   (hp_free((void *)(req)))
#define Sch_Alloc_Inst_Req(site, req) 			\
	((req) = (install_req_t *) hp_alloc(sizeof(install_req_t)))
#endif /* SEPARATE_INSTALL */


#define Sch_Gc_Edge(site, edge) {			\
	edge_buffer_t *b = &(Scheduler(site)->edge_buffer); \
	if (b->count < 1000 && Simp_Lock(b)) {		\
	   b->count++;					\
	   Join_Chain_Next((edge), K2T(&(b->head)));	\
	   Simp_Unlock(b);				\
	} else						\
	hp_free((void *)(edge));			\
}

#define Sch_Alloc_Suspended(site, susp) {		\
	susp_buffer_t *b = &(Scheduler(site)->susp_buffer); \
	if (Simp_Lock(b)) {				\
	   if (b->count) {				\
	      b->count--;				\
	      (susp) = b->next;				\
	      b->next = (susp)->next;			\
	   } else {					\
	      Simp_Unlock(b);				\
	      (susp) = (st_susp_t *)			\
			hp_alloc(sizeof(st_susp_t));	\
	   }						\
	} else						\
	   (susp) = (st_susp_t *)			\
			hp_alloc(sizeof(st_susp_t));	\
}

#define Sch_Gc_Suspended(site, susp) {		\
	susp_buffer_t *b = &(Scheduler(site)->susp_buffer); \
	if (Simp_Lock(b)) {			\
	   b->count++;				\
	   (susp)->next = b->next;		\
	   b->next = (susp);			\
	   Simp_Unlock(b);			\
	} else					\
	hp_free((void *)(susp));		\
}

#if defined(PROLOG_LMP) 
#define CheckLmp(twig) {				\
	if (Eldest(Trunk(twig)) && Monad(twig))		\
	   SetEldest(Twig(twig));			\
	else 						\
	   ResetEldest(Twig(twig));			\
}

#define UpdateLmp(old) {				\
	if (Eldest(Twig(old)) && !Monad(old)) {		\
	   st_id_t new;					\
	   st_edge_t *x = Twig(old)->next;		\
	   do {						\
	      if (Alive(x)) {				\
		 PackSite(&new,(old));			\	
		 PackKnot(&new,Knot(old));		\
		 PackEdge(&new,x);			\
		 SetEldest(x);				\
		 Smsg_ShortCut_Mid(&new, SubTree(&new),	\
		      sch_msg_hdl_lmp(SubTree(&new), &new),\
		      sch_msg_snd_lmp(SubTree(&new), &new) \
		     );					\
		 break;					\
	      } else 					\
	         x = x->next;				\
	   } while (x!=Trunk(old)) {			\
	   assert(x!=Trunk(old));			\
	   ResetEldest(Twig(old));			\
	}						\
}
#else /* PROLOG_LMP */
#define CheckLmp(twig)
#define UpdateLmp(twig)
#endif /* PROLOG_LMP */

#if !defined(NO_SHORTCUT)
#define Intrasite_Smsg(site)	 (Scheduler(site)->intrasite_smsg.count)
#define Inc_ShortCut_Count(site) (Scheduler(site)->smsg_count_intra_shortcut++)

#define IntraSiteCheckIn(recv,send) {		\
	if ((recv)==(send)) {			\
	   Disable_Int();			\
	   Intrasite_Smsg(recv) += 1;		\
	   Enable_Int();			\
	}					\
}
#define IntraSiteCheckOut(recv,send) {		\
	if ((recv)==(send)) {			\
	   Disable_Int();			\
	   Intrasite_Smsg(recv) -= 1;		\
	   assert(Intrasite_Smsg(recv) >= 0);	\
	   Enable_Int();			\
	}					\
}
#else /* NO_SHORTCUT */
#define IntraSiteCheckIn(recv,send)
#define IntraSiteCheckOut(recv,send)
#define Intrasite_Smsg(site) 1
#endif  /* NO_SHORTCUT */

#define LoadReportPublish(site) (Scheduler(site)->load_report_eager)

#define Sch_Initlock(t) Simp_Initlock(Knot(t))
#define Sch_Unlock(t)   Simp_Unlock(Knot(t))
#define Sch_Lock(t)     Simp_Lock(Knot(t))

#define Simp_Initlock(obj) ((obj)->lock = 0)
/* #define Simp_Unlock(obj)   (simp_unlock(&((obj)->lock))) */
/* #define Simp_Lock(obj)     (simp_lock(&((obj)->lock))) */
#define Simp_Lock(obj) ((obj)->lock ? 0 : ++((obj)->lock))
#define Simp_Unlock(obj) {	\
	assert((obj)->lock);	\
	(obj)->lock = 0;	\
}
