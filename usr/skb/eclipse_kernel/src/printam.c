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
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/*
 * SEPIA C SOURCE MODULE
 *
 * VERSION	$Id: printam.c,v 1.4 2008/07/10 01:08:47 jschimpf Exp $
 */

/*
 * SEPIA abstract code printing
 * This is a function that prints an abstract instruction in a readable
 * form . It returns the pointer to the start of the next instruction,
 * so when it is used to print the whole code without executing it,
 * a sequence
 *                 ptr = print_am(ptr);
 * is to be used.
 */

#include	"config.h"

#ifndef NOALS		/* otherwise everything is omitted */

#include	"names.h"	/* so that the names array is defined */
#include        "sepia.h"
#include	"types.h"
#include "embed.h"
#include	"error.h"
#include	"mem.h"
#include	"opcode.h"
#include 	"io.h"
#include 	"dict.h"
#include	"emu_export.h"
#include	"database.h"
#include	"gencode.h"
#include	"module.h"
#include	"debug.h"

extern vmcode	fail_code_[];
void		print_port(stream_id nst, int port);
static void	_print_label(vmcode **ptr);
static vmcode	*_print_init_mask(vmcode *code, int name);
static void	_print_edesc(uword);

/* this one should also check >= brk(0) */
#define InvalidAddress(ptr)	((ptr) == NULL || (uword) (ptr) & 0x3)
#define H_did(start)	*((start) - 1)
#define Arg(addr)	((pword *) addr - g_emu_.emu_args)
#define Atom		{p_fprintf(current_output_,"%s ", DidName(*code)); code++;}
#define VarOffset	p_fprintf(current_output_,"%d ", (int)(*code++)/(long)sizeof(pword))
#define Integer		p_fprintf(current_output_,"%d ", (int)(*code++))
#define ArgDesc		p_fprintf(current_output_,"<%x> ", (int)(*code++))
#define Float		p_fprintf(current_output_,"%f ", * (float *) code++)
#define String \
	{   value v;\
	    v.str = (char *) *code++;\
	    (void) ec_outf(current_output_, StringStart(v), (int) StringLength(v));\
	}
#define Structure	\
		{p_fprintf(current_output_,"%s/", DidName(*code));\
		 p_fprintf(current_output_, "%d ", DidArity(*code)); code++;}
#define Code_Label		\
	if (*(vmcode **) code == FailCode)			\
		(void) ec_outfs(current_output_,"Fail ");		\
	else 							\
		_print_label((vmcode **) code);			\
	code++;
#define Save_Label		\
	if (*(vmcode **) code == FailCode)			\
		(void) ec_outfs(current_output_,"Fail ");		\
	else {							\
		*label = (vmcode *)(*code);			\
		_print_label((vmcode **) code);}		\
	code++;
#define Print_Label(p)		\
	if (*(vmcode**)(p) == FailCode)			\
		(void) ec_outfs(current_output_,"Fail ");		\
	else 							\
		_print_label((vmcode**)(p));
#ifdef PRINTAM
#define Consttag \
	if (TagTypeC((word)(*code)) < 0 || TagTypeC((word)(*code)) > NTYPES) \
	    p_fprintf(current_output_,"<illegal tag> <%x>", (int)(*code)); \
	else p_fprintf(current_output_,"%s <%x> ", \
	    DidName(tag_desc[TagTypeC((word)(*code))].tag_name), (int)(*code)); \
	code++;
#else
#define Consttag \
	if (TagTypeC((word)(*code)) < 0 || TagTypeC((word)(*code)) > NTYPES) \
	    p_fprintf(current_output_,"<illegal tag> "); \
	else p_fprintf(current_output_,"%s ", \
	    DidName(tag_desc[TagTypeC((word)(*code))].tag_name)); \
	code++;
#endif
#define Const           p_fprintf(current_output_,"const <%x> ", (int)(*code++))
#define NamedVar	{if (IsNamed(*code)) \
			    p_fprintf(current_output_,"%s ",\
			    DidName(TagDid(*code)));\
			else\
			    p_fprintf(current_output_,"_ ");\
			code++;}
#define RelLabel	p_fprintf(current_output_,"%d ", (int)(*code++))
#define SaveRelLabel    RelLabel
#define Am		p_fprintf(current_output_,"A%d ", Arg(*code++))
#ifdef Ar
#undef Ar
#endif
#define Ar		(void) ec_outfs(current_output_,"rA1 ")
#define Temp		p_fprintf(current_output_,"T%d ", (*code++)/(long)sizeof(pword))
#define TempR		(void) ec_outfs(current_output_,"rT ")
#define Perm		p_fprintf(current_output_,"Y%d ", (*code++)/(long)sizeof(pword))
#define Nl		(void) ec_newline(current_output_);
#define Else		(void) ec_outfs(current_output_,"Else ");
#ifdef PRINTAM
#define Addr		p_fprintf(current_output_, "0x%x ", *code++);
#else
#define Addr		p_fprintf(current_output_, "0x%x ", *code++ & 0xfff);
#endif
#define Proc		\
	{did1 = PriDid((pri *) *code);\
	if (PriScope((pri *) *code) == QUALI)\
	    p_fprintf(current_output_,"%s:", DidName(PriHomeModule((pri *) *code))); \
	p_fprintf(current_output_,"%s/%d ", DidName(did1), DidArity(did1));\
	code++; }
#define EsuName		\
	{p_fprintf(current_output_,"%s/", DidName(((pri *) *code)->did));\
	 p_fprintf(current_output_,"%d ",DidArity(((pri *)*code)->did));\
	 code++;}


#define ExtName EsuName
#define ExtCallName EsuName

#define Port	print_port(current_output_, *(word*)code++)

#define Atom_Table2						\
	{							\
		uword		*ptr = (uword *) *code++;	\
		uword		*end;				\
								\
		end = (uword *) ((pword *) ptr + *code++);	\
		do						\
		{						\
			p_fprintf(current_output_,		\
				"\n\t\t\t%s:\t",		\
				DidName((dident)*ptr));		\
			ptr++;					\
			_print_label((vmcode **) ptr);		\
			ptr++;					\
		} while (ptr < end);				\
		(void) ec_outfs(current_output_, "\n\t\t\tdefault:");\
	}

#define Integer_Range_Table					\
	{							\
		uword		*ptr = (uword *) *code++;	\
	       	uword		*end;				\
							\
		p_fprintf(current_output_, " %d", *code);	\
		p_fprintf(current_output_, "\n\t\t\t< %d:\t", (int) *ptr);\
		_print_label((vmcode **) (ptr + 1));		\
		p_fprintf(current_output_, "\n\t\t\t> %d:\t", (int) *(ptr+2));\
		_print_label((vmcode **) (ptr + 3));		\
		ptr += 4;					\
		end = (uword *) ((pword *) ptr + *code++);	\
		while (ptr < end)				\
		{						\
			p_fprintf(current_output_,		\
				"\n\t\t\t%d:\t",		\
				(int) (*ptr));			\
			ptr++;					\
			Print_Label(ptr);			\
			ptr++;					\
		}						\
		(void) ec_outfs(current_output_, "\n\t\t\telse:\t");\
		Code_Label					\
		(void) ec_outfs(current_output_, "\n\t\t\tdefault:");\
		Code_Label					\
	}

#define Integer_Table2						\
	{							\
		uword		*ptr = (uword *) *code++;	\
		uword		*end;				\
								\
		end = (uword *) ((pword *) ptr + *code++);	\
		do						\
		{						\
			p_fprintf(current_output_,		\
				"\n\t\t\t%d:\t",		\
				(int) (*ptr));			\
			ptr++;					\
			_print_label((vmcode **) ptr);		\
			ptr++;					\
		} while (ptr < end);				\
		(void) ec_outfs(current_output_, "\n\t\t\tdefault:");\
	}

#define Functor_Table2						\
	{							\
		uword		*ptr = (uword *) *code++;	\
		uword		*end;				\
								\
		end = (uword *) ((pword *) ptr + *code++);	\
		do						\
		{						\
			p_fprintf(current_output_,		\
				"\n\t\t\t%s/%d:\t",		\
				DidName((dident) *ptr),		\
				DidArity((dident) *ptr));	\
			ptr++;					\
			_print_label((vmcode **) ptr);		\
			ptr++;					\
		} while (ptr < end);				\
		(void) ec_outfs(current_output_, "\n\t\t\tdefault:");\
	}

#define EnvDesc _print_edesc(*code++);

#define PortName(Port)		ec_debug_ports[(Port) & PORT_MASK]

static char  *ec_debug_ports[] =
{
    " NOPORT ",
    " CALL ",
    " EXIT ",
    "*EXIT ",
    " REDO ",
    " FAIL ",
    " RESUME ",
    " LEAVE ",
    " DELAY ",
    " NEXT ",
    " UNIFY ",
    " SPYTERM ",
    " MODIFY ",
    " ELSE ",
    " ???? ",
    " ???? ",
    " OTHER "
};

#define ALS	1	/* whole procedure being listed, not just one instr */
#define PROCLAB	2	/* print a symbolic address with each instruction */

vmcode       *
print_am(register vmcode *code,
	vmcode **label,
	int *res,
	int option)		/* ALS|PROCLAB */
{
    dident	did1;
    int		inst;

    if (*label == code)
	*label = NULL;		/* the label is about to being printed */

    if (InvalidAddress(code))
	inst = Inst_Error;
    else
	inst = Get_Int_Opcode(code++);

    if (inst < 0 || inst > NUMBER_OP)
    {
	    p_fprintf(current_output_, "Undefined opcode in print_am: %d",
		    inst);
	    code = 0;
	    *res = PFAIL;
    }
    if (inst == Code_end) {
	*res = PSUCCEED;
	return 0;
    }
    else if (inst == Comment)
	return (vmcode *) code + (*code + 1);
    else
    {
#ifdef PRINTAM
	if (option & PROCLAB)	/* try to print the location */
	{
	    extern pri *ec_code_procedure(vmcode *code);
	    pri *pd = ec_code_procedure(code-1);
	    if (pd)
		p_fprintf(current_output_,"%s/%d+%d:\n",
			DidName(PriDid(pd)), DidArity(PriDid(pd)),
			code - PriCode(pd) - 1);
	}
#endif
	p_fprintf(current_output_, "\t%s\t", inst_name[inst]);
	switch (inst)

	{
	case Failure:
	case Nop:
	case Clause:
	    break;

	case Read_void:
	case Read_variable:
	case Read_reference:
	case Read_nil:
	case Read_test_var:
	case Write_variable:
	case Write_void:
	case Write_nil:
	case Write_list:
	case Write_first_list:
	case Match_meta:
	case Match_last_meta:
	case First:
	case Push_void:
	case Push_variable:
	case Push_nil:
	case Push_list:
	case Puts_variable:
	case Puts_list:
	case Puts_nil:
	case Occur_check_next:
	case Dfid_test:
#if (NREGTMP > 0)
	case FirstTR:
#endif /* NREGTMP */
	case Inst_Error:
	case Continue_after_exception:
	case Refail:
		break;

	case Write_named_void:
	case Write_named_variable:
	case Push_self_reference:
	case Write_meta:
		NamedVar;
		break;

	case CutAM:
	case MoveAM:
	case Get_nilAM:
	case Out_get_nilAM:
	case In_get_nilAM:
	case Read_variableAM:
	case Read_referenceAM:
	case Read_valueAM:
	case Read_matched_valueAM:
	case Write_valueAM:
	case Write_local_valueAM:
	case Put_nilAM:
	case Out_get_listAM:
	case Get_list_argumentsAM:
	case Get_structure_argumentsAM:
	case Write_variableAM:
	case Put_variableAM:
	case Put_global_variableAM:
	case Put_listAM:
	case Push_variableAM:
	case Push_valueAM:
	case Push_local_valueAM:
	case Puts_valueAM:
	case SavecutAM:
	case BI_Exit:
	case BI_SetBipError:
	case BI_GetBipError:
	case BI_Free:
	case BI_Var:
	case BI_NonVar:
	case BI_Atom:
	case BI_Integer:
	case BI_Float:
	case BI_Breal:
	case BI_Real:
	case BI_Rational:
	case BI_String:
	case BI_Number:
	case BI_Atomic:
	case BI_Compound:
	case BI_Meta:
	case BI_IsSuspension:
	case BI_IsHandle:
	case BI_IsEvent:
	case BI_IsList:
	case BI_Bignum:
	case BI_Callable:
		Am;
		break;
	
	case Write_named_variableAM:
	case Put_named_variableAM:
		Am;
		NamedVar;
		break;

	case Put_named_variableAML:
		Am;
		Perm;
		NamedVar;
		break;

	case Put_referenceAM:
		Am;
	case Puts_reference:
		VarOffset;
		NamedVar;
		break;

	case Put_referenceAML:
		Am;
	case Puts_referenceL:
		Perm;
		VarOffset;
		NamedVar;
		break;

	case Move3AMAM:
	        Am;
		/* fall through */
	case ShiftAMAMAMAMAM:
	        Am;
		/* fall through */
	case ShiftAMAMAMAM:
	case Move2AMAM:
	    	Am;
		/* fall through */

	case ShiftAMAMAM:
	case RotAMAMAM:
	case BI_NotIdentList:
	        Am;
		/* fall through */

	case BI_Identical:
	case BI_NotIdentical:
	case BI_Inequality:
	case SwapAMAM:
	case Read_variable2AM:
	case Write_variable2AM:
	case Write_local_value2AM:
	case Push_local_value2AM:
	case Put_variable2AM:
	        Am;
	        Am;
		break;

	case BI_MakeSuspension:
	    	Am;
		/* fall through */

	case BI_Add:
	case BI_Sub:
	case BI_Mul:
	case BI_Quot:
	case BI_Div:
	case BI_Rem:
	case BI_FloorDiv:
	case BI_FloorRem:
	case BI_And:
	case BI_Or:
	case BI_Xor:
	case BI_Lt:
	case BI_Le:
	case BI_Gt:
	case BI_Ge:
	case BI_Eq:
	case BI_Ne:
	case BI_Arg:
	        Am;
		/* fall through */

	case BI_Minus:
	case BI_Bitnot:
	case BI_CutToStamp:
	case BI_Arity:
	        Am;
	        Am;
	        ArgDesc;
		break;

	case BI_Addi:
	        Am;
	        Integer;
	        Am;
	        ArgDesc;
		break;

#define NREGARG 0
#if (NREGARG > 0)
	case MoveAR:
	case Get_nilAR:
	case Out_get_nilAR:
	case In_get_nilAR:
	case Read_variableAR:
	case Read_valueAR:
	case Read_matched_valueAR:
	case Write_valueAR:
	case Write_local_valueAR:
	case Put_nilAR:
	case Out_get_listAR:
	case Get_list_argumentsAR:
	case Get_structure_argumentsAR:
	case Write_variableAR:
	case Put_variableAR:
	case Put_listAR:
	case Push_variableAR:
	case Push_valueAR:
	case Push_local_valueAR:
	case Puts_variableAR:
	case Puts_valueAR:
	case Test_varAR:
	case Test_groundAR:
	case Push_referenceAR:
		Ar;
		break;

	case Write_named_variableAR:
	case Put_named_variableAR:
		Ar;
		NamedVar;
		break;
#endif /* NREGARG */

	case Read_variableL:
	case Read_referenceL:
	case Write_variableL:
	case Read_valueL:
	case Read_matched_valueL:
	case Write_valueL:
	case Write_local_valueL:
	case Push_init_variableL:
	case Push_variableL:
	case Push_valueL:
	case Push_local_valueL:
	case Puts_variableL:
	case Puts_valueL:
	case Put_global_variableL:
		Perm;
		break;

	case Write_named_variableL:
	case Put_named_variableL:
		Perm;
		NamedVar;
		break;

	case Initialize:
	    code = _print_init_mask(code, 0);
	    break;

	case Initialize_named:
	    code = _print_init_mask(code, 1);
	    break;

	case Read_valueTM:
	case Read_matched_valueTM:
	case Match_next_metaTM:
	case Match_metaTM:
	case Write_valueTM:
	case Write_local_valueTM:
	case NextTM:
	case ModeTM:
	case Push_valueTM:
	case Push_local_valueTM:
	case Puts_valueTM:
	case Write_next_listTM:
		Temp;
		break;

#if (NREGTMP > 0)
	case Read_valueTR:
	case Read_matched_valueTR:
	case Write_valueTR:
	case Write_local_valueTR:
	case NextTR:
	case ModeTR:
	case Push_valueTR:
	case Push_local_valueTR:
	case Puts_valueTR:
	case Push_variableTR:
	case Read_variableTR:
	case Write_variableTR:
	case Push_referenceTR:
		TempR;
		break;

	case Write_named_variableTR:
		TempR;
		NamedVar;
		break;

#endif /* NREGTMP */

	case Move3AML:
	        Am;
		Perm;
	case Move2AML:
	case Put_global_variable2AML:
	        Am;
		Perm;
	case MoveAML:
	case Get_valueAML:
	case Get_matched_valueAML:
	case Put_variableAML:
	case Put_unsafe_valueAML:
	case Put_global_variableAML:
	case Read_variable2AML:
	case Write_variable2AML:
		Am;
		Perm;
		break;

	case MoveNAML:
	        Integer;
		Am;
		Perm;
		break;

#if (NREGARG > 0)
	case MoveARL:
	case Get_valueARL:
	case Get_matched_valueARL:
	case Put_variableARL:
	case Put_unsafe_valueARL:
		Ar;
		Perm;
		break;

	case Put_named_variableARL:
		Ar;
		Perm;
		NamedVar;
		break;
#endif /* NREGARG */

	case Put_unsafe_valueAMTM:
	case Get_valueAMTM:
	case Get_matched_valueAMTM:
		Am;
		Temp;
		break;

	case MoveTMAM:
		Temp;
		Am;
		break;

#if (NREGARG > 0)
	case MoveARAM:
		Ar;
		Am;
		break;
#endif /* NREGARG */

#if (NREGARG > 0 && NREGTMP > 0)
	case MoveTRAR:
		TempR;
		Ar;
		break;
#endif /* NREGARG && NREGTMP */

#if (NREGTMP > 0)
	case MoveTRAM:
		TempR;
		Am;
		break;
#endif /* NREGTMP */

#if (NREGARG > 0)
	case MoveTMAR:
		Temp;
		Ar;
		break;
#endif /* NREGARG */


#if (NREGTMP > 0)
	case Get_valueAMTR:
	case Get_matched_valueAMTR:
	case MoveAMTR:
		Am;
		TempR;
		break;
#endif /* NREGTMP */

#if (NREGARG > 0)
	case Put_unsafe_valueARTM:
	case Get_valueARTM:
	case Get_matched_valueARTM:
		Ar;
		Temp;
		break;
#endif /* NREGARG */

#if (NREGARG > 0 && NREGTMP > 0)
	case Get_valueARTR:
	case Get_matched_valueARTR:
	case MoveARTR:
		Ar;
		TempR;
		break;
#endif /* NREGARG && NREGTMP */

	case Get_variableNAML:
		VarOffset;
		Am;
		Perm;
		break;

	case Move3LAM:
		Perm;
	        Am;
	case Move2LAM:
		Perm;
	        Am;
	case MoveLAM:
		Perm;
		Am;
		break;

	case MoveNLAM:
	        Integer;
		Perm;
		Am;
		break;

#if (NREGARG > 0)
	case Get_variableNARL:
		VarOffset;
		Ar;
		Perm;
		break;

	case MoveLAR:
		Perm;
		Ar;
		break;
#endif /* NREGARG */

	case MoveAMAM:
	case Get_valueAMAM:
	case Get_matched_valueAMAM:
		Am;
		Am;
		break;

	case Move3LL:
	        Perm;
	        Perm;
		/* falls through */
	case Move2LL:
	        Perm;
	        Perm;
		/* falls through */
	case MoveLL:
	case Get_valueLL:
	case Write_variable2L:
	case Write_local_value2L:
	case Push_local_value2L:
	case Read_variable2L:
		Perm;
		Perm;
		break;

#if (NREGARG > 0)
	case MoveAMAR:
	case Get_valueAMAR:
	case Get_matched_valueAMAR:
		Am;
		Ar;
		break;
#endif /* NREGARG */

	case Get_atom2AM:
		Am;
		Atom;

	case Get_atomAM:
	case Out_get_atomAM:
	case In_get_atomAM:
	case Put_atomAM:
	case Put_moduleAM:
		Am;
		Atom;
		break;

#if (NREGARG > 0)
	case Get_atomAR:
	case Out_get_atomAR:
	case In_get_atomAR:
	case Put_atomAR:
		Ar;
		Atom;
		break;
#endif /* NREGARG */

	case Get_atomintegerAMAM:
		Am;
		Atom;
		Am;
		Integer;
		break;

	case Get_integer2AM:
		Am;
		Integer;

	case Get_integerAM:
	case Out_get_integerAM:
	case In_get_integerAM:
	case Put_integerAM:
		Am;
		Integer;
		break;

#if (NREGARG > 0)
	case Get_integerAR:
	case Out_get_integerAR:
	case In_get_integerAR:
	case Put_integerAR:
		Ar;
		Integer;
		break;
#endif /* NREGARG */

	case Get_floatAM:
	case In_get_floatAM:
	case Out_get_floatAM:
	case Put_floatAM:
		Am;
		Float;
		break;

#if (NREGARG > 0)
	case Get_floatAR:
	case In_get_floatAR:
	case Out_get_floatAR:
	case Put_floatAR:
		Ar;
		Float;
		break;
#endif /* NREGARG */

	case Get_stringAM:
	case In_get_stringAM:
	case Out_get_stringAM:
	case Put_stringAM:
		Am;
		String;
		break;

#if (NREGARG > 0)
	case Get_stringAR:
	case In_get_stringAR:
	case Out_get_stringAR:
	case Put_stringAR:
		Ar;
		String;
		break;
#endif /* NREGARG */

	case Get_structureAM:
	case In_get_structureAM:
		Am;
		Structure;
		Code_Label;
		break;

	case Put_structureAM:
	case Out_get_structureAM:
		Am;
		Structure;
		break;

#if (NREGARG > 0)
	case Get_structureAR:
	case In_get_structureAR:
		Ar;
		Structure;
		Code_Label;
		break;

	case Out_get_structureAR:
	case Put_structureAR:
		Ar;
		Structure;
		break;
#endif /* NREGARG */

	case Get_listAM:
	case In_get_listAM:
	case In_get_metaAM:
		Am;
		Code_Label;
		break;

	case Get_metaAM:
		Am;
		NamedVar;
		break;

#if (NREGARG > 0)
	case Get_listAR:
	case In_get_listAR:
		Ar;
		Code_Label;
		break;
#endif /* NREGARG */

	case Read_variableNL:
	case Read_referenceNL:
	case Write_variableNL:
		VarOffset;
		Perm;
		break;

	case Write_named_variableNL:
		VarOffset;
		Perm;
		NamedVar;
		break;

	case Read_atom2:
	        Atom;
	        /* falls through */
	case Read_atom:
	case Puts_atom:
		Atom;
		break;

	case Read_atominteger:
	        Atom;
		Integer;
		break;

	case Read_integeratom:
		Integer;
	        Atom;
		break;

	case Read_integer2:
	case Write_integer2:
	        Integer;
	        /* falls through */
	case Read_integer:
	case Write_integer:
	case Push_integer:
	case Puts_integer:
	case Exit_emulator:
	case Bounce:
		Integer;
		break;

	case Read_float:
	case Write_float:
	case Push_float:
	case Puts_float:
		Float;
		break;

	case Read_string:
	case Write_string:
	case Push_string:
	case Puts_string:
		String;
		break;

	case Write_did2:
	        Structure;
		/* falls through */
	case Write_structure:
	case Write_first_structure:
	case Write_did:
	case Puts_structure:
		Structure;
		break;

	case Write_didinteger:
		Structure;
		Integer;
		break;
	    
	case Write_integerdid:
		Integer;
		Structure;
		break;

	case Read_structure:
	case Read_last_structure:
		Structure;
		Code_Label;
		break;

	case Read_meta:
	case Read_last_meta:
		NamedVar;
	case Read_list:
	case Read_last_list:
		Code_Label;
		break;

	case Read_structureTM:
	case Read_next_structureTM:
	case Write_next_structureTMlab:
		Structure;
	case NextTMlab:
	case ModeTMlab:
	case Read_listTM:
	case Read_next_listTM:
	case Write_next_listTMlab:
		Temp;
		Code_Label;
		break;

	case Write_next_structureTM:
	        Structure;
		Temp;
		break;

	case Read_metaTM:
	case Read_next_metaTM:
		Temp;
		NamedVar;
		Code_Label;
		break;

#if (NREGTMP > 0)
	case Read_structureTR:
	case Read_next_structureTR:
		Structure;
	case NextTRlab:
	case ModeTRlab:
	case Read_listTR:
	case Read_next_listTR:
		TempR;
		Code_Label;
		break;
	case Get_constantAR:
        case Out_get_constantAR:
	case In_get_constantAR:
	       Ar; Const; Consttag; 
	       break;
        case Put_constantAR:
	       Ar; Consttag; Const; 
	       break;


#endif /* NREGTMP */

	case Puts_constant:
	        Consttag; Const;
	        break;

	case Read_constant:
	case Write_constant:
	case Push_constant:
	        Const; Consttag;
	        break;

	case Get_constantAM:
        case Out_get_constantAM:
	case In_get_constantAM:
	       Am; Const; Consttag; 
	       break;

        case Put_constantAM:
	       Am; Consttag; Const; 
	       break;

	case Retry_me_else:
	case Retry:
		Port;
		Code_Label;
		break;

	case Retry_inline:
		Port;
		Code_Label;
		EnvDesc;
		break;

	case Trust:
		Port;
		Code_Label;
		Nl;
		break;

	case Trust_inline:
		Port;
		Code_Label;
		EnvDesc;
		Nl;
		break;

	case Branchs:
		VarOffset;
	case Branch:
		Code_Label;
		break;

	case Set_bp:
	case New_bp:
		Code_Label;
		break;

	case Try_me_else:
		Port;
		Integer;
		Code_Label;
		break;

	case Retry_me_inline:
		Port;
		Code_Label;
		EnvDesc;
		break;

	case Trust_me_inline:
		Port;
		EnvDesc;
		break;

	case Try_parallel:
		{
		    long	nalt;
		    uword	*ptr;

		    nalt = (long) *code;
		    Integer;
		    Integer;
		    ptr = (uword *) *code++;
		    if (ptr)
		    {
			do
			{
			    p_fprintf(current_output_, "\n\t\t\t\t");
			    _print_label((vmcode **) ptr);
			    ptr++;
			} while (nalt--);
		    }
		}
		break;

	case Retry_seq:
	case Try_clause:
		Addr;
		break;

	case GuardL:
		VarOffset;
		Code_Label;
		break;

	case Try:
		Port;
		Integer;
		Code_Label;
		break;

	case Trylab:
		Port;
		Integer;
		Code_Label;
		Code_Label;
		Nl;
		break;

	case Retrylab:
		Port;
		Code_Label;
		Code_Label;
		Nl;
		break;

	case Try_me_dynamic:
	case Retry_me_dynamic:
#ifdef OLD_DYNAMIC
		Integer;
		Integer;
		Save_Label;
		if (*code == SRC_CLAUSE_ARITY)
		    p_fprintf(current_output_,"SOURCE ");
		p_fprintf(current_output_,"%d ",
			(*code++) & SRC_CLAUSE_ARITY_MASK);
		Code_Label;
#endif
		break;

	case Push_referenceAM:
		Am;
	case Allocate:
	case Wake_init:
	case Space:
	case Exits:
	case Push_structure:
	case Push_reference:
	case Push_void_reference:
	case Read_attribute:
	case Read_voidN:
	case Write_voidN:
	case Push_voidN:
	case Puts_valueG:
	case Push_valueG:
		VarOffset;
		break;

	case Gc_testA:
		Integer;
	case Gc_test:
	case Gc:
		Integer;
		break;

	case Cut:
	case Cut_single:
		VarOffset;
		break;

	case MoveLAMCallfA:
	        Perm;
		Am;
	case CallfA:
	case CallA:
		Addr;
		EnvDesc;
		break;

	case Put_global_variableAMLCallfA:
		Am;
	        Perm;
		Addr;
		EnvDesc;
		break;

	case JmpdAs:
		VarOffset;
	case JmpA:
	case JmpdA:
	case ChainA:
	case ChaincA:
	case ChaindA:
	case Meta_jmpA:
		Addr;
		Nl;
		break;

	case MoveLAMChainA:
	        Perm;
		Am;
		Addr;
		Nl;
		break;

	case MoveLAMCallfP:
	        Perm;
		Am;
	case CallfP:
	case CallP:
		Proc;
	case Metacall:
	case Handler_call:
	case Suspension_call:
	case Fail_clause:
		EnvDesc;
		break;

	case Put_global_variableAMLCallfP:
	        Am;
		Perm;
	        Proc;
		EnvDesc;
		break;

	case Fastcall:
		Port;
		EnvDesc;
		break;

	case MoveLAMChainP:
	        Perm;
		Am;
	case JmpP:
	case JmpdP:
	case ChainP:
	case ChaincP:
	case ChaindP:
		Proc;
		Nl;
		break;

	case Ret:
	case Retn:
	case Retd:
	case Retd_nowake:
	case Ret_nowake:
	case Exit:
	case Exitd:
	case Exitd_nowake:
	case Exitc:
		Nl;
		break;

	case Savecut:
	case Neckcut:
	case Neckcut_par:
	case Deallocate:
	case Restore_bp:
	case Catch:
	case Throw:
	case Meta_jmp:
	case Suspension_jmp:
	case Explicit_jmp:
	case Wake:
		break;

	case Trust_me:
		Port;
		break;

	case SavecutL:
	case SoftcutL:
	case Dfid_testL:
	case Depth:
		Perm;
		break;

	case CutL:
	case Push_referenceL:
	case Push_init_referenceL:
		Perm;
		VarOffset;
		break;

	case CutAMN:
		Am;
		VarOffset;
		break;

	case ExtCall:
		ExtCallName;
		break;

	case Escape:
		EsuName;
		break;

	case External:
	case External0:
	case External1:
	case External2:
	case External3:
	case Call_dynamic:
		Proc;
		Addr;
		break;

	case Debug_call:
	        Proc;
		Port;
		Atom;
		Integer;
		Integer;
		Integer;
		break;

	case Debug_call_simple:
	        Proc;
		Port;
		Atom;
		Integer;
		Integer;
		Integer;
	case Debug_exit_simple_args:
		Integer;	/* argument descriptor minitags */
		Integer;	/* offset */
	case Debug_exit_simple:
		break;

	case List_switchL:
	    	Perm;
		goto _list_switch_;

	case List_switchAM:
		Am;
_list_switch_:
		if (option & ALS) {
		    Code_Label;
		    Code_Label;
		    Code_Label;
		}
		break;

#if (NREGARG > 0)
	case List_switchAR:
		Ar;
		if (option & ALS) {
		    Code_Label;
		    Code_Label;
		    Code_Label;
		}
		break;
#endif /* NREGARG */

#if (NREGARG > 0)
	case Atom_switchAR:
		Ar;
		if (option & ALS) {
		    Atom_Table2;
		    Code_Label;
		}
		break;
#endif /* NREGARG */

	case Atom_switchL:
	    	Perm;
		goto _atom_switch_;

	case Atom_switchAM:
		Am;
_atom_switch_:
		if (option & ALS) {
		    Atom_Table2;
		    Code_Label;
		}
		break;
		
	case Functor_switchL:
	    	Perm;
		goto _functor_switch_;

	case Functor_switchAM:
		Am;
_functor_switch_:
		if (option & ALS) {
		    Functor_Table2;
		    Code_Label;
		}
		break;
		
#if (NREGARG > 0)
	case Functor_switchAR:
		Ar;
		if (option & ALS) {
		    Functor_Table2;
		    Code_Label;
		}
		break;
#endif /* NREGARG */
		
	case Integer_switchL:
	    	Perm;
		goto _integer_switch_;

	case Integer_switchAM:
		Am;
_integer_switch_:
		if (option & ALS) {
		    Integer_Table2;
		    Code_Label;
		}
		break;
		
#if (NREGARG > 0)
	case Integer_switchAR:
		Ar;
		if (option & ALS) {
		    Integer_Table2;
		    Code_Label;
		}
		break;
#endif /* NREGARG */

	case Integer_range_switchL:
	    	Perm;
		goto _integer_range_switch_;

	case Integer_range_switchAM:
		Am;
_integer_range_switch_:
		if (option & ALS) {
		    Integer_Range_Table;
		}
		break;

	case Switch_on_typeL:
	    	Perm;
		goto _switch_on_type_;

	case Switch_on_typeAM:
		Am;
_switch_on_type_:
		if (option & ALS)
		{
			int	i;
			for (i = 0; i < NTYPES; i++)
			{
				p_fprintf(current_output_, "\n\t\t\t%-16s",
					DidName(tag_desc[i].tag_name));
				Code_Label;
			}
		}
		break;

#if (NREGARG > 0)
	case Switch_on_typeAR:
		Ar;
		if (option & ALS)
		{
			int	i;
			for (i = 0; i < NTYPES; i++)
			{
				p_fprintf(current_output_, "\n\t\t\t%d: ", i);
				Code_Label;
			}
		}
		break;
#endif /* NREGARG */

	case Ress:
		VarOffset;
	case Res:
		Integer;
		EnvDesc;
		break;

	case Continue_after_event:
	case Continue_after_event_debug:
	case Debug_exit:
	case BI_ContDebug:
		break;

	case Puts_proc:
	case Undefined:
		Proc;
		break;

	default:
		p_fprintf(current_output_, "Undefined opcode in print_am: %d", *(code - 1));
		code = 0;
	}
    }
    (void) ec_newline(current_output_);	/* to flush if tty */
    return code;
}

static void
_print_label(vmcode **ptr)
{
    char	*instr;
    int		inst;

	p_fprintf(current_output_,"%d(", (word) (*ptr)
#ifndef PRINTAM
						    & 0xfff
#endif
							    );
    if (InvalidAddress(*ptr))
	ec_outfs(current_output_, "BAD ADDRESS");
    else {
	inst = Get_Int_Opcode(*ptr);
	if (inst < 0 || inst > NUMBER_OP)
	    inst = Inst_Error;
	instr = inst_name[inst];
	while (*instr != ' ')
	    (void) ec_outfc(current_output_, *instr++);
    }
    (void) ec_outfc(current_output_, ')');
}

static vmcode *
_print_init_mask(vmcode *code, int name)
{
    long	pos = (*code++)/(long)sizeof(pword);
    unsigned	init_mask = *code++;

    if (name)
    {
	if (IsTag(*code,TNAME))
	    p_fprintf(current_output_,"%s-", DidName(TagDid(*code)));
	code++;
    }
    p_fprintf(current_output_,"Y%d ", pos++);
    while (init_mask)
    {
	if (init_mask & 1)
	{
	    if (name)
	    {
		if (IsTag(*code,TNAME))
		    p_fprintf(current_output_,"%s-", DidName(TagDid(*code)));
		code++;
	    }
	    p_fprintf(current_output_,"Y%d ", pos);
	}
	init_mask >>= 1;
	pos++;
    }
    return code;
}


static void
_print_edesc(uword edesc)
{
    if (EdescIsSize(edesc))
    {
	/* size might be -1 */
	p_fprintf(current_output_,"%d ", (word)edesc/(word)sizeof(pword));
    }
    else
    {
	uword pos = 1;
	int first = 1;
	uword *eam_ptr = EdescEamPtr(edesc);
	p_fprintf(current_output_,"Y[");
	do {
	    int i;
	    uword eam = EamPtrEam(eam_ptr);
	    for(i=EAM_CHUNK_SZ;i>0;--i) {
		if (eam & 1) {
		    if (first) {
			first = 0;
			p_fprintf(current_output_,"%d", pos);
		    } else {
			p_fprintf(current_output_,",%d", pos);
		    }
		}
		eam >>= 1;
		pos++;
	    }
	} while (EamPtrNext(eam_ptr));
	p_fprintf(current_output_,"]");
    }
}


void
print_port(stream_id nst, int port)
{
    (void) p_fprintf(nst,"%s%s%s%s%s%s",
	port & FIRST_CALL ? "F|" : "",
	port & LAST_CALL ? "L|" : "",
	port & NO_ARGS ? "NA|" : "",
	port & INLINE_PORT ? "I|" : "",
        port & BREAKPOINT ? "B|" : "",
	PortName(port) + 1
	);
}

#ifdef PRINTAM

/*
 * Utility for debugging
 */

#define EnQueue_(pw, arity) {			\
        if (queue_head) {			\
            queue_tail[1].val.ptr = (pword *) hg_alloc_size(2*sizeof(pword));\
            queue_tail = queue_tail[1].val.ptr;	\
        } else                                                          \
            queue_tail = queue_head = (pword *) hg_alloc_size(2*sizeof(pword));\
	    queue_tail[0].val.ptr = (pw);	\
        queue_tail[0].tag.kernel = (arity);	\
        queue_tail[1].val.ptr = (pword *) 0;	\
}
 
#define DeQueue_(pw, arity) {			\
        register pword *elem = queue_head;	\
        (pw) = elem[0].val.ptr;			\
        (arity) = elem[0].tag.kernel;		\
        queue_head = elem[1].val.ptr;		\
        hg_free_size((generic_ptr)elem, 2*sizeof(pword)); \
}
 
#define EmptyQueue() (!queue_head)

#define TUNKNOWN	 (TFORWARD-1)

static char * tag_string[] = {
    "????    ",		/* -7 */
    "TFORWARD",		/* -6 */
    "TSTAMP  ",		/* -5 */
    "TUNIV   ",		/* -4 */
    "TMETA   ",		/* -3 */
    "TNAME   ",		/* -2 */
    "TVAR    ",		/* -1 */
    "TLIST   ",
    "TCOMP   ",
    "TSUSP   ",
    "THANDLE ",
    "TSTRG   ",
    "TBIG    ",
    "TIVL    ",
    "TRAT    ",
    "TDBL    ",
    "TNIL    ",
    "TINT    ",
    "TDICT   ",
    "TPTR    ",
    "TPROC   ",
    "TEND    ",
    "TDE     ",
    "TGRS    ",
    "TGRL    ",
    "TEXTERN ",
    "TBUFFER "
    "TVARNUM ",
    };

p_pw(value v, type t)
{
    pword pw;
    pw.val.all = v.all;
    pw.tag.all = t.all;
    return ppw(&pw);
}

ppw(pword *pw)				/* print prolog words */
          
{

    int arity = 1;
    pword *queue_head = (pword *) 0;
    pword *queue_tail = (pword *) 0;

    for (;;)
    {
	char region;
	int t = TagType(pw->tag);

	if (t < TFORWARD || t > TBUFFER)
	    t = TUNKNOWN;

	if (TG_ORIG <= pw && pw < TG) region = 'g';
	else if (SP <= pw && pw < SP_ORIG) region = 'l';
	else if (B_ORIG <= pw && pw < B.args) region = 'c';
	else if (TT <= (pword **) pw && (pword **) pw < TT_ORIG) region = 't';
	else if (address_in_heap(&global_heap, (generic_ptr) pw)) region = 'h';
	else region = '?';

	p_fprintf(current_output_, "%c 0x%08x:  0x%08x 0x%08x  %s ", region,
			pw, pw->val.all, pw->tag.all, tag_string[t-TUNKNOWN]);
	switch (t)
	{
	case TFORWARD:
	case TMETA:
	case TNAME:
	    if (pw != pw->val.ptr)
	    {
		ec_outfs(current_output_, "--->");
		EnQueue_(pw->val.ptr, 1);
	    }
	    else
	    {
		ec_outfs(current_output_, IsNamed(pw->tag.kernel) ?
					DidName(TagDid(pw->tag.kernel)) : "_");
	    }
	    break;
	case TVAR_TAG:
	    if (pw != pw->val.ptr)
	    {
		ec_outfs(current_output_, "--->");
		EnQueue_(pw->val.ptr, 1);
	    }
	    else
		ec_outfs(current_output_, "_");
	    break;
	case TLIST:
	    EnQueue_(pw->val.ptr, 2);
	    break;
	case TCOMP:
	    if (pw->val.ptr)
		EnQueue_(pw->val.ptr, DidArity(pw->val.ptr->val.did)+1);
	    break;
	case TSTRG:
	    ec_outfs(current_output_, StringStart(pw->val));
	    break;
	case TSUSP:
	    break;
	case TDE:
	    break;
	case THANDLE:
	    break;
	case TNIL:
	    break;
	case TINT:
	    p_fprintf(current_output_, "%d", pw->val.nint);
	    break;
	case TDICT:
	    ec_outfs(current_output_, DidName(pw->val.did));
	    if (DidArity(pw->val.did))
		p_fprintf(current_output_, "/%d", DidArity(pw->val.did));
	    break;
	case TPTR:
	    break;
	case TPROC:
	case TEND:
	case TVARNUM:
	case TGRS:
	case TGRL:
	case TEXTERN:
	case TBUFFER:
	    break;
	case TDBL:
	    p_fprintf(current_output_, "%f", Dbl(pw->val));
	    break;
	case TBIG:
	case TRAT:
	default:
	    if (t >= 0 && t <= NTYPES)
	    {
		(void) tag_desc[t].write(QUOTED, current_output_,
			    pw->val, pw->tag);
	    }
	    break;
	}
	ec_newline(current_output_);
	if (--arity > 0)
	{
	    pw++;
	    continue;
	}
	ec_newline(current_output_);
	if (EmptyQueue())
	    break;
	DeQueue_(pw, arity);
    }
    Succeed_;
}

#endif /* PRINTAM */

#ifdef THREADED

int
get_int_opcode(code)
vmcode	*code;
{
    register vmcode	op = *code;
    register vmcode	*p, *stop;

    if (op == Code_end)
	return Code_end;
    p = &op_addr[0];
    stop = &op_addr[NUMBER_OP - 1];
    while (p <= stop)
	if (op == *p++)
	    return p - &op_addr[1];


    return Inst_Error;
}

#endif /* THREADED */

#endif /* NOALS */
