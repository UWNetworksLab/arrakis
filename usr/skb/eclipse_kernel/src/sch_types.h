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
**        File: sch_types.h
**      Author: Liang-Liang Li
** Description: Scheduler Tree Definitions
**
***********************************************************************/

/* SITE: Every worker has one and only one scheduler port */ 
typedef  aport_id_t  site_id_t;

/* simplified definition of an engine handle */
typedef void * eng_handle_t;

/* STREE ID */
typedef struct st_id_ds {
        site_id_t       site;
        pds_uint32  	edge;
        pds_uint32  	knot;
} st_id_t;

/* simplified definition of st_id_t for use in the engine interface.
 * this must have the same size as the real definition!
 */
#if !defined(ST_HANDLE_DS_DEFINED)
typedef struct st_handle_ds {
        site_id_t       site;
        pds_uint32	edge;
        pds_uint32	knot;
} st_handle_t;                                  /* st_id_t */
#endif /* ST_HANDLE_DS_DEFINED */

#define ST_HANDLE_DS_DEFINED 1

/* EDGE of scheduler tree */
typedef struct st_edge_ds {
        struct st_edge_ds * next;       /* double links of branches    */
        struct st_edge_ds * prev;
        struct st_id_ds     tree;       /* SUP-tree|SUB-tree           */
	int		    info;	/* stree status                */
} st_edge_t;



/* KNOT of scheduler tree */
typedef struct st_knot_ds {
	struct st_edge_ds       trunk;  /* edge to parent node         */
	struct st_susp_ds * suspended;  /* suspended job-search leaves */
	site_id_t                site;  /* site (worker) id	       */
	int               alive_twigs;  /* the # of alive twigs        */
	int                    nxtcls;  /* # alternative clauses       */
	int                  nxtcls_b;  /* # alternatives for booking  */
	unsigned	     hybrid:1;  /* hidden sequential chpts     */
	unsigned	      jroot:1;  /* job-search tree root: Y/N   */
	unsigned	      proot:1;  /* a pseudo-root node/dummy node
					** only a chop or a direct backtrack
					** message will cause it to die. i.e.
					** a trust (streighten) optimizaton
					** will not go across these nodes.
					** it is created to protect the engine
					** stacks delimited by this node and
					** its parent from being popped.
					*/
	unsigned	      local:1;  /* the sequential chpts between
					** this and its parent nodes have
					** to be resumed by the worker/engine
					** that creates this node.
					*/
	unsigned	        tip:1;  /* leaf or subtree: Y/N        */
	char volatile       	 lock;  /* mutual lock for msg-handler */
} st_knot_t;


/* suspended leave */
typedef struct st_susp_ds {
        struct st_susp_ds  * next;
        struct st_id_ds   leaf;         /* a suspended leaf            */
        struct st_id_ds   coma;         /* its comn ancestor 	       */
} st_susp_t;


typedef struct susp_buffer_ds {
       st_susp_t *next;
       int       count;
       char      lock;
} susp_buffer_t;

typedef struct edge_buffer_ds {
       struct st_knot_ds head;
       int       count;
       char      lock;
} edge_buffer_t;

#if !defined(SEPARATE_INSTALL)
#else /* SEPARATE_INSTALL */
typedef struct install_req_ds {
    struct install_req_ds *next;
    struct st_id_ds     lodge; 
    struct st_id_ds     comn; 
    struct st_id_ds     leaf; 
} install_req_t;
#endif /* SEPARATE_INSTALL */


typedef struct scheduler_ds {
    aport_id_t port;
    struct st_handle_ds leaf;
    struct st_handle_ds root;
    eng_handle_t engine; 
    amsg_type_t smsg_type[5][5];
    unsigned long sch_base;

#if !defined(SEPARATE_INSTALL)
#else /* SEPARATE_INSTALL */
    struct install_req_ds *install_req;
#endif /* SEPARATE_INSTALL */

    struct susp_buffer_ds susp_buffer;
    struct edge_buffer_ds edge_buffer;

#if !defined(NO_SHORTCUT)
    struct {
       int    count;
       char   lock; 
    } intrasite_smsg;
#endif /* NO_SHORTCUT */

    int state_donate;
    int smsg_count_hdl;
    int smsg_count_snd;
    int smsg_count_intra;
    int smsg_count_intra_shortcut;
    int smsg_subcount[40+1];
    int max_to_publish;
    unsigned load_report_eager:1;
    unsigned async_hdl :1;
    unsigned root_first:1;
    unsigned left_first:1;

    unsigned idling    :1;
    unsigned waking    :1;
    unsigned lmp       :1;
} scheduler_t;

typedef struct sch_status_ds {
    int state_donate;
    int smsg_count_hdl;
    int smsg_count_snd;
    int smsg_count_intra;
    int smsg_subcount[40+1];
    int max_to_publish;
    unsigned root_first:1;
} sch_status_t;
