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
 * VERSION	$Id: mem.h,v 1.1 2008/06/30 17:43:57 jschimpf Exp $
 *
 * IDENTIFICATION	mem.h
 *
 * DESCRIPTION		Definitions for heap memory management
 *
 * AUTHORS:		Joachim Schimpf, Emmanuel van Rossum, Micha Meier
 *
 ********************************************************************/

#ifndef NULL
#define		NULL	0
#endif

/*---------------------------------------------------------------------
 * Abstract data type "Unbounded Stack" (description see mem.c)
 *---------------------------------------------------------------------*/

#define Stack_Declare(Stack)			unbounded_stack Stack;

#define Stack_Create(Stack, MinimumWords)	stack_create(&(Stack), MinimumWords)

#define Stack_Destroy(Stack)			stack_destroy(&(Stack))

#define StackEmpty(Stack)			((Stack)->top == (Stack)->limit)

#define StackTop(Stack)				((Stack)->top)

#define Stack_Pop_To(Stack, OldTop)		stack_pop_to(&(Stack), OldTop)

#define Stack_Push(Stack, WordsNeeded) {				\
	if (((Stack)->top -= (WordsNeeded)) < (uword *)((Stack) + 1))	\
	    stack_push(&(Stack), WordsNeeded);				\
	}

#define Stack_Pop(Stack, NumberWords) {					\
	if (((Stack)->top += (NumberWords)) >= (Stack)->limit)		\
	    stack_pop(&(Stack), NumberWords);				\
	}

#define Stack_Reset(Stack)				\
	for (;;) {					\
	    (Stack)->top = (Stack)->limit;		\
	    if (!(Stack)->down) break;			\
	    (Stack) = (Stack)->down;			\
	}


typedef struct stack_header {
	struct stack_header	*down,		/* previous segment */
				*up;		/* next segment */
	uword			*top,		/* stack top inside this segment */
				*limit;		/* end of this segment */
} *unbounded_stack;


extern void stack_create ARGS((struct stack_header **pstack, uword words_needed));
extern void stack_pop_to ARGS((struct stack_header **pstack, uword *old_top));
extern void stack_push ARGS((struct stack_header **pstack, uword words_needed));
extern void stack_pop ARGS((struct stack_header **pstack, uword word_offset));
extern void stack_destroy ARGS((struct stack_header **pstack));

/*---------------------------------------------------------------------
 * Abstract data type "Temporary Memory" (description see mem.c)
 *---------------------------------------------------------------------*/

#define Temp_Create(Temp, MinimumBytes)	temp_create(&(Temp), MinimumBytes)

#define Temp_Destroy(Temp)		temp_destroy(&(Temp))

#define TempAlloc(Temp, BytesNeeded)	temp_alloc(&(Temp), BytesNeeded)

#define Temp_Align(Temp, Size)		temp_align(&(Temp), Size)
	
#define Temp_Reset(Temp) {			\
	    (Temp) = (Temp)->first;		\
	    (Temp)->top = (char *)((Temp)+1);	\
	}


typedef struct temp_header {
	struct temp_header	*next,		/* next segment	*/
				*first;		/* first segment */
	char			*top,
				*limit;
} *temp_area;


extern void	temp_create ARGS((struct temp_header **ptemp, uword bytes_needed));
extern void	temp_align ARGS((struct temp_header **ptemp, uword size));
extern void	temp_destroy ARGS((struct temp_header **ptemp));
extern char	*temp_alloc ARGS((struct temp_header **ptemp, uword bytes_needed));

/*---------------------------------------------------------------------
 * Abstract data type "Unbounded Buffer"
 * (description see mem.c)
 *---------------------------------------------------------------------*/

#define Buf_Declare(Buf)		unbounded_buffer Buf

#define Buf_Create(Buf, MinWords)	buffer_create(&(Buf), MinWords)

#define Buf_Reinit(Buf)			buffer_reinit(&(Buf))

#define Buf_Destroy(Buf)		buffer_destroy(&(Buf))

#define BufWriteZ(Buf)			(Buf).write_block->top

#define BufWriteR(Buf, RdPtr)					\
	((Buf).write_block = (Buf).read_block,			\
	 (Buf).write_block_end = (Buf).write_block->limit,	\
	 (Buf).write_block->top = (RdPtr))

#define Buf_Alloc(Buf, WrPtr, Words) {				\
	if ((WrPtr) + (Words) > (Buf).write_block_end) 		\
		(WrPtr) = buffer_alloc(&(Buf), WrPtr, Words);	\
	}

#define Buf_Flush(Buf, WrPtr)		{ (Buf).write_block->top = (WrPtr); }

#define BufReadA(Buf)	\
	((uword *)(((Buf).read_block = (Buf).write_block->first) + 1))

#define BufReadW(Buf, WrPtr)	\
	(((Buf).read_block = (Buf).write_block), (WrPtr))

#define BufReadZ(Buf)	\
	(((Buf).read_block = (Buf).write_block), (Buf).write_block->top)

#define Buf_Set_Read(Buf, RdPtr) {				\
	if ((RdPtr) < (uword *)((Buf).read_block + 1)		\
		|| (Buf).read_block->limit < (RdPtr))		\
	    buffer_setread(&(Buf), RdPtr);			\
	}

#define Buf_Check(Buf, RdPtr) {				\
	if ((RdPtr) == (Buf).read_block->top)		\
	    (RdPtr) = buffer_next(&(Buf), RdPtr);	\
	}

#define BufPos(Buf, AnyPtr)	buffer_pos(&(Buf), AnyPtr)


typedef struct {
	struct buffer_block_header	*read_block;
	struct buffer_block_header	*write_block;
	uword				*write_block_end;
	int				has_nonpage_buffers;
} unbounded_buffer;

struct buffer_block_header {
	struct buffer_block_header	*next;
	struct buffer_block_header	*first;
	uword				*top;
	uword				*limit;
};


extern void	buffer_create ARGS((unbounded_buffer *bd, uword minwords));
extern void	buffer_reinit ARGS((unbounded_buffer *bd));
extern void	buffer_setread ARGS((unbounded_buffer *bd, uword *ptr));
extern void	buffer_destroy ARGS((unbounded_buffer *bd));

extern uword	buffer_pos ARGS((unbounded_buffer *bd, uword *ptr));

extern uword	*buffer_alloc ARGS((unbounded_buffer *bd, uword *ptr, uword words));
extern uword	*buffer_next ARGS((unbounded_buffer *bd, uword *ptr));


/*---------------------------------------------------------------------
 * Simplified shared heap interface
 *---------------------------------------------------------------------*/

extern struct heap_descriptor global_heap;

#ifdef __STDC__
generic_ptr     hg_alloc_size(word size);
void            hg_free_size(generic_ptr, word size);
generic_ptr     hg_realloc_size(generic_ptr, word, word);
generic_ptr     hg_alloc(word size);
void            hg_free(generic_ptr);
generic_ptr     hg_resize(generic_ptr, word);
int             hg_statistics(int what);
#else /* __STDC__ */
generic_ptr     hg_alloc_size();
void            hg_free_size();
generic_ptr     hg_realloc_size();                          
generic_ptr     hg_alloc();                                 
void            hg_free();
generic_ptr     hg_resize();
int             hg_statistics();
#endif /* __STDC__ */
