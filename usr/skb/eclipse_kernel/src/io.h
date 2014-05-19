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
 * Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): ECRC GmbH
 * 
 * END LICENSE BLOCK */


/*
 * SEPIA INCLUDE FILE
 *
 * VERSION	$Id: io.h,v 1.1 2008/06/30 17:43:56 jschimpf Exp $
 */

/*
 * IDENTIFICATION		io.h
 *
 * DESCRIPTION			Definitions concerning the I/O.
 *	
 * CONTENTS:
 *
 */

#define TTY_BUF_SIZE		128

#define NO_PROMPT		D_UNKNOWN
#define NO_BUF			((unsigned char *) 0)
#define NO_PROC			((pri *) 0)
#define NO_STREAM		((stream_id) 0)

#define EOB_MARK		0

#define StreamId(n)		(StreamDescriptors[n])	/* stream_id from int */
#define StreamNr(nst)		((nst)->nr)	/* int from stream_id */

#define StreamUnit(nst)		(nst)->unit
#define StreamMode(nst)		(nst)->mode
#define StreamOutputMode(nst)	(nst)->output_mode
#define StreamPrintDepth(nst)	(nst)->print_depth
#define StreamType(nst)		((nst)->mode & STYPE)
#define StreamNref(nst)		(nst)->nref
#define StreamCnt(nst)		(nst)->cnt
#define StreamSize(nst)		(nst)->size
#define StreamBuf(nst)		(nst)->buf
#define StreamWBuf(nst)		(nst)->wbuf
#define StreamPtr(nst)		(nst)->ptr
#define StreamLexSize(nst)	(nst)->lex_size
#define StreamLexAux(nst)	(nst)->lex_aux
#define StreamLine(nst)		(nst)->line
#define StreamName(nst)		(nst)->name
#define StreamOffset(nst)	(nst)->offset
#define StreamPrompt(nst)	(nst)->prompt
#define StreamEncoding(nst)	(nst)->encoding
#define StreamPromptStream(nst)	(nst)->prompt_stream
#define StreamEvent(nst)	(nst)->event
#define StreamRand(nst)		(nst)->rand
#define StreamLastWritten(nst)	(nst)->last_written
#define StreamFILE(nst)		((FILE *) (nst)->stdfile)
#define StreamMethods(nst)	(* (io_channel_t *) (nst)->methods)
#define SetStreamMethods(nst,m)	(nst)->methods = (void_ptr) (m);

/* some of the data is used for sockets differently */
#define SocketInputStream(nst)	StreamPromptStream(nst)
#define SocketUnix(nst)		StreamPrompt(nst)
#define SocketType(nst)		StreamLexSize(nst)
#define SocketConnection(nst)	StreamLexAux(nst)

#define IsOpened(nst)		(StreamMode(nst) != SCLOSED)

#define IsNullStream(nst)	((StreamMode(nst) & STYPE) == SNULL)
#define IsTty(nst)		((StreamMode(nst) & STYPE) == STTY)
#define IsStringStream(nst)	((StreamMode(nst) & STYPE) == SSTRING)
#define IsQueueStream(nst)	((StreamMode(nst) & STYPE) == SQUEUE)
#define IsPipeStream(nst)	((StreamMode(nst) & STYPE) == SPIPE)
#define IsSocket(nst)		((StreamMode(nst) & STYPE) == SSOCKET)
#define IsInvalidSocket(nst)	((StreamMode(nst) & (STYPE | SREAD)) == (SSOCKET | SREAD))

#define IsFileStream(nst)	((StreamMode(nst) & STYPE) == SFILE)
#define IsReadStream(nst)	(StreamMode(nst) & SREAD)
#define IsWriteStream(nst)	(StreamMode(nst) & SWRITE)
#define IsReadWriteStream(nst)	((StreamMode(nst) & SRDWR) == SRDWR)
#define IsReadlineStream(nst)	(StreamMode(nst) & READLINE)

/* streams which can recover from being at eof */
#define IsSoftEofStream(nst)	(IsTty(nst) || IsQueueStream(nst) || IsNullStream(nst))

#define SystemStream(nst)	(				\
				    (nst) == current_input_ ||	\
				    (nst) == current_output_ ||	\
				    (nst) == current_err_ ||	\
				    (nst) == warning_output_ ||	\
				    (nst) == log_output_ ||	\
				    (nst) == null_		\
				)


/****
***** THIS IS PRIVATE TO THE I/O PART
***** BE CAREFUL IF YOU INTEND TO USE THIS
****/

#define BUFSIZE 	1024

#define NO_UNIT		(-1)

#define MAX_NREF	32767	/* size of the nref field	*/

/* stream mode flags */
#define SCLOSED		0x0000	/* the channel is closed	*/
#define SREAD		0x0001	/* READ allowed			*/
#define SWRITE		0x0002	/* WRITE allowed		*/
#define SRDWR		0x0003
#define SAPPEND		0x0004	/* always with SWRITE, never with SREAD	*/
				/* only in SFILE streams	*/

/* stream type field */
#define STYPE_SHIFT	3
#define STYPE_NUM	((STYPE>>STYPE_SHIFT)+1)
#define STYPE		0x0038	/* type mask			*/
#define SFILE		0x0000
#define SSTRING		0x0008
#define SPIPE		0x0010
#define SQUEUE		0x0018
#define SNULL		0x0020
#define SSOCKET		0x0028
#define STTY		0x0030
/* #define 		0x0038 */

/* other stream properties */
#define SEOLCR		0x0040	/* output CR(+LF) at end of line */
#define SSYSTEM		0x0080	/* one of the system streams	*/
#define DONT_PROMPT	0x0100	/* don't print the next prompt	*/
#define MREAD		0x0200	/* we have read a buffer	*/
#define MWRITE		0x0400	/* we wrote into the buffer	*/
#define MEOF		0x0800	/* eof has been read		*/
#define SSCRAMBLE	0x1000	/* scramble stream data		*/
#define SYIELD		0x2000	/* queues only: yield on eof or flush */
#define REPROMPT_ONLY	0x4000	/* suppress initial prompts on this stream */
#define SFLUSHEOL	0x8000	/* flush stream at end-of-line	*/
// #define READLINE	0x8000	/* use readline() on this stream */
#define SNONBLOCKING	0x10000	/* don't block if stream not ready */
#define SSIGIO		0x20000	/* SIGIO is enabled for this stream */
#define SNOMACROEXP	0x40000	/* suppress macro expansion	*/
#define SCOMPRESS	0x80000	/* try to compress output on this stream */
#define SSELECTABLE	0x100000 /* stream supports select/3 */


/* how many characters can be ungotten */
#define LOOKAHEAD		4


/*
 * action codes for io_rpc()
 */
#define IO_FLUSH	1
#define IO_FILL		2
#define IO_CLOSE	3
#define IO_TTYIN	4
#define IO_BIND		5
#define IO_CONNECT	6
#define IO_LISTEN	7
#define IO_ACCEPT	8

#define Lock_Stream(nst) {              \
        if (ec_options.parallel_worker) {          \
           a_mutex_lock(&nst->lock);    \
        }                               \
}
#define Unlock_Stream(nst) {            \
        if (ec_options.parallel_worker) {          \
           a_mutex_unlock(&nst->lock);  \
        }                               \
}

#define RemoteStream(nst) \
	(nst->fd_pid && nst->fd_pid != own_pid && nst->aport)

/*
 * The stream number can be the $$stream property of some atom. The
 * value of the property stores the stream number and the tag is TINT.
 */

/*
 * Set the stream number as a property of the atom's did. The counter for
 * the previous stream, if any, is decremented, and new new one is incremented.
 */
#define Set_Stream(sdid, nst)						\
	{								\
		pword   *prop;						\
		prop = get_property(sdid, STREAM_PROP);			\
		if (prop == (pword *) NULL)				\
		    prop = set_property(sdid, STREAM_PROP);		\
		else							\
		{							\
		    StreamNref(StreamId(prop->val.nint))--;		\
		}							\
		prop->val.nint = StreamNr(nst);				\
		prop->tag.kernel = TINT;				\
		if (StreamNref(nst)++ == MAX_NREF)			\
		    return(TOO_MANY_NAMES);				\
	}

/* defines for the output functions */

#define	DEFAULT_OUTPUT_MODE	ATTRIBUTE

#define	CANONICAL		1	/* ignore operators		*/
#define	FULLDEPTH		2	/* ignore depth			*/
#define	DOTLIST			4	/* write lists in dot notation	*/
#define	QUOTED			8	/* print quotes when needed	*/
#define	VAR_NUMBERS	     0x10	/* print var number only	*/
#define	VAR_NAMENUM	     0x20	/* print var name (if any) and number */
#define	PRINT_CALL	     0x40	/* print was called, use portray */
#define	WRITE_COMPACT	     0x80	/* don't print unnecessary spaces */
#define	ATTRIBUTE	    0x100	/* print metaterm attribute	*/
#define	WRITE_GOAL	    0x200	/* printed term is a goal	*/
#define	STD_ATTR	    0x400	/* attribute in standard form	*/
#define	NO_MACROS	    0x800	/* don't apply write macros	*/
#define	WRITE_CLAUSE	   0x1000	/* printed term is a clause	*/
#define	DONT_QUOTE_NL	   0x2000	/* print newlines even when quoted */
#define	VAR_ANON	   0x4000	/* print variables as _		*/
#define	OUT_DOLLAR_VAR	   0x8000	/* print $VAR(I) as variables	*/
#define	PORTRAY_VAR	  0x10000	/* call portray even for variables */
#define	OUTPUT_MODES	       17

#define	PRINT		  0x20000	/* a portray predicate exists	*/
#define	PRINT1		  0x40000	/* only portray/1 exists	*/
#define	VARTERM		  0x80000	/* print variables as '_'(...)	*/

/* context of the term being written */

#define NOARG		0x00000000
#define ARGTERM		0x00000001
#define ARGOP		0x00000002
#define ARGPREF		0x00000004
#define ARGSIGN		0x00000008
#define ARGLIST		0x00000010
#define ARGYF		0x00000020	/* parent is yf[x] */
#define ARGLAST		0x00000040	/* last subterm of its parent operator*/


/*
 * Types
 */

typedef struct {
    int		io_type;
    int		mode_defaults;
    int		buf_size_hint;
    int		(*close) ARGS((int));
    int		(*ready) ARGS(());
    int		(*read) ARGS((int,char*,int));
    int		(*write) ARGS((int,char*,int));
    int		(*at) ARGS((stream_id nst, long int*));
    int		(*at_eof) ARGS((stream_id));
    int		(*buffer_nonempty) ARGS((stream_id));
    int		(*truncate) ARGS(());
    int		(*seek) ARGS((stream_id,long,int));
    int		(*flush) ARGS((stream_id));
    int		(*size) ARGS((stream_id));
    int		(*content) ARGS((stream_id,char*));
    int		(*outf) ARGS((stream_id,char*,int));
} io_channel_t;


/*
 * EXTERNAL VARIABLE DECLARATIONS: 
 */

Extern int		own_pid;
Extern void		my_io_aport();

Extern	int	ec_pwrite ARGS((int,int,stream_id,value,type,int,int,dident,type));
Extern	int	ec_tty_in ARGS((stream_id));
Extern	int	ec_tty_out ARGS((stream_id, int));
Extern	int	ec_tty_outs ARGS((stream_id, char*, int));
Extern	int	ec_seek_stream ARGS((stream_id,long,int));
Extern	int	ec_stream_at ARGS((stream_id,long*));
Extern	int	ec_close_stream ARGS((stream_id));
Extern	int	ec_outfw ARGS((stream_id, word));
Extern	int	ec_outfc ARGS((stream_id, int));
Extern	int	ec_getch ARGS((stream_id));
Extern	int	ec_ungetch ARGS((stream_id));

Extern	int	fill_buffer ARGS((stream_id));
Extern	int	io_flush_out ARGS((stream_id));
Extern	int	set_sigio ARGS((int));
Extern	int	reset_sigio ARGS((int));
Extern	int	ec_stream_set_sigio ARGS((stream_id, int));
Extern	int	ec_stream_reset_sigio ARGS((stream_id, int));
Extern	int	ec_setup_stream_sigio_thread ARGS((stream_id));
Extern	int	ec_reenable_sigio ARGS((stream_id, int, int));
Extern	void	mark_dids_from_streams ARGS((void));
Extern	int	p_reset ARGS((void));
Extern stream_id find_free_stream ARGS((void));
Extern void	init_stream ARGS((stream_id,int unit,int mode,dident name,
			dident prompt,stream_id pstream, int size));
Extern stream_id ec_open_file ARGS((char*,int,int*));
Extern stream_id get_stream_id ARGS((value,type,int,int*));
Extern char	*ec_getstring ARGS((stream_id,long,long*));
Extern char	*string_to_number ARGS((char *start, pword *result, stream_id nst, int syntax));


