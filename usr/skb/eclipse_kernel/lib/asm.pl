% ----------------------------------------------------------------------
% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1992-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: asm.pl,v 1.5.2.1 2009/01/03 07:48:40 jschimpf Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	asm.pl
%
% AUTHORS:		Joachim Schimpf
%                       Pierre Lim
%                       Kish Shen -- Major changes to add disasm/2 and
%                                    generalisation of code
%
% CONTENTS:		asm(+PredSpec, +WAMList)
%                       disasm(+PredSpec, -WAMList)
%                       pasm(+WAMList, -Size, -BTPos, -WordList)
%                       wam(+PredSpec)
%
% DESCRIPTION:
%
%	asm(+PredSpec, +WAMList) creates the predicate PredSpec
%	with the code specified by WAMList.
%
%       disasm(+PredSpec, -WAMList) unifies WAMList to the WAM code of a 
%       currently defined predicate PredSpec.
%
%       pasm(+WAMList, -Size, -BTPos, -WordList) partially assembles WAMList to a
%       platform independent format of the words that need to be stored into
%       memory. BTPos is offset in words from start of code to the port/break,
%       table, or 0 if none.
%
%	A single instruction is a term whose functor specifies the instruction
%	and whose arguments are the instruction operands, e.g. 
%	
%		get_integer(a(3), 99)
%		move(a(1), y(3))
%		branch(ref(Label))
%
%	Format of labels:
%
%		label(<variable>)  variable should not occur in other labels
%
%
%	Instruction operands are of the form:
%
%		ref(<variable>)	   reference (refers to a label(<variable>))
%		<int>		   integer constant or
%                                  offset
%		<float>		   float value
%		<atom>		   atom did or
%                                  named variable or
%                                  attribute name
%		a(N)		   argument register N
%		y(N)		   permanent variable N
%		t(N)		   temporary variable N
%		N/A                procedure descriptor for predicate N/A
%               M:N/A              procedure descriptor for predicate N/A in M
%		N/A 	           did for functor N/A
%               y(<vmask>)         vmask 
%               y(<named vmask>)   named vmask
%               tags(<Tag switch list>)   
%                                  switch labels for switch_on_type
%                                     
%               <switch table>     entries for a switch table
%               <try refs>         switches for try_parallel instruction
%
%       vmask is a list of int, where each int is a variable to be initialised.
%       The first element should be the smallest argument number
%
%       named vmask is a list of VarName-<int>, where VarName is an atom 
%       representing the name of the variable to be initialised. First element
%       should be the smallest argument number
%
%       Tag switch list is a list of TagName:<label>, where TagName is
%       a tag type. Each tag type can occur at most once in the list. 
%       Unmentioned tag types are assumed to have ref(fail) as labels
%
%       switch table is a list of Key-<label>. Keys in an integer table
%       must be ordered. Range tables has the same form as an integer
%       switch table, except that the first two entries are the minimum
%       and maximum of the range (and thus not ordered with the rest)
%
%       try_refs is a list of references to switch to for try_parallel instr.
%
%
%
% CAUTION:    -	The integer opcodes of the abstract instructions are
%		currently hardcoded in this file. The mapping must
%		correspond to the one in opcode.h.
%	      
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(asm).

:- comment(summary, "Assemble and disassemble predicates").

:- comment(desc, "\
    The asm library provide tools for assembling and disassembling the WAM
    code representation of predicates to and from memory. It also allows
    the formatted printing of the WAM code. This library is used by the
    fcompile library to generate the object code, which is a form of the
    WAM code that can be read back in and assembled into the predicates."
).

:- comment(asm / 2, [
	summary:"Assemble the WAM instructions WAMCode into ECLiPSe as predicate PredSpec. 

",
	template:"asm(+PredSpec, +WAMCode)",
	desc:html("   Assembles the WAM instruction WAMCode into the current ECLiPSe session
   as the predicate PredSpec. The WAM code is in the form of a list, with
   each element representing one WAM instruction. The format of the WAMCode
   is the same as that generated by disasm/2,3. Thus the predicate can be
   used to load a predicate previously dissasembled by disasm/2,3 without
   having to compile the source Prolog form of the predicate.

<P>
   If PredSpec is an existing defined predicate, the older definition will
   be replaced. If WAMCode is not in the correct format, an exception will
   be generated and the predicate PredSpec would not be redefined. 

<P>
"),
	args:["+PredSpec" : "Atom/Integer.", "+WAMCode" : "A list of WAM instructions in the right format."],
	resat:"   No.",
	fail_if:"   None.",
	exceptions:[5 : "PredSpec is not in correct form.", 6 : "WAMCode is not in correct form.  "],
	see_also:[asm / 3, disasm / 2, disasm / 3, pasm / 4]]).

:- comment(asm / 3, [
	summary:"Assemble the WAM instructions WAMCode into ECLiPSe in module Module as
predicate PredSpec.  

",
	template:"asm(+PredSpec, +WAMCode, +Flags)",
	desc:html("   Assembles the WAM instruction WAMCode into the current ECLiPSe session
   in an existing module Module as the predicate PredSpec. The WAM code is
   in the form of a list, with each element representing one WAM
   instruction. The format of the WAMCode is the same as that generated by
   disasm/2,3. Thus the predicate can be used to load a predicate
   previously dissasembled by disasm/2,3 without having to compile the
   source Prolog form of the predicate.

<P>
   If PredSpec is an existing defined predicate, the older definition will
   be replaced. If WAMCode is not in the correct format, an exception will
   be generated and the predicate PredSpec would not be redefined. 

<P>
"),
	args:["+PredSpec" : "Atom/Integer.",
		"+WAMCode" : "A list of WAM instructions in the right format.",
		"+Flags" : "An integer."],
	resat:"   No.",
	fail_if:"   None.",
	exceptions:[5 : "PredSpec or Module is not in correct form.", 6 : "WAMCode is not in correct form.  ", 80 : "Module is not an existing module."],
	see_also:[asm / 2, disasm / 2, disasm / 3, pasm / 4]]).

:- comment(disasm / 2, [
	summary:"Disassemble an existing predicate PredSpec in the current module to its WAM
abstract machine representation WAMCode.

",
	template:"disasm(+PredSpec, ?WAMCode)",
	desc:html("   Unifies WAMCode with the WAM instructions representing the abstract
   machine code for the predicate specified by PredSpec (in Name/Arity
   form). The WAM code is in the form of a list, with each element
   representing one WAM instruction. The format of the WAMCode is the same
   as that used by asm/2,3 and pasm/4 to assemble a predicate. Thus, the
   WAM code generated by disasm/2,3 can be used to load the predicate into
   ECLiPSe without having to compile the source Prolog form. 

<P>
   The library asm must be loaded to use diasm/2.

<P>
   Currently asm/2 cannot disassemble dynamic predicates.

<P>
"),
	args:["+PredSpec" : "Atom/Integer.", "?WAMCode" : "Variable or a list of WAM instructions in the right format."],
	resat:"   No.",
	fail_if:"   Fails if WAMCode is initially instantiated and does not unify with the WAM code generated by asm/1 for the predicate, or if PredSpec is dynamic.",
	exceptions:[5 : "PredSpec is not in correct form.", 60 : "PredSpec does not exist in current module."],
	eg:"
   for fruit/1 defined by:

      fruit(orange).

   ?- disasm(fruit / 1, W).
   W = [get_atom(a(1), orange), retd, code_end]



",
	see_also:[disasm / 3, asm / 2, asm / 3, pasm / 4, wam / 1]]).

:- comment(disasm / 3, [
	summary:"Disassemble an existing predicate PredSpec in the module Module to its WAM
abstract machine representation WAMCode.

",
	template:"disasm(+PredSpec, ?WAMCode, +Module)",
	desc:html("   Unifies WAMCode with the WAM instructions representing the abstract
   machine code for the predicate specified by PredSpec (in Name/Arity
   form) in module Module. The WAM code is in the form of a list, with each
   element representing one WAM instruction. The format of the WAMCode is
   the same as that used by asm/2,3 and pasm/4 to assemble a
   predicate. Thus, the WAM code generated by disasm/2,3 can be used to
   load the predicate into ECLiPSe without having to compile the source
   Prolog form.

<P>
   The library asm must be loaded to use diasm/3.

<P>
   Currently disasm/3 cannot disassemble dynamic predicates.

<P>
   If PredSpec is dynamic.

<P>
"),
	args:["+PredSpec" : "Atom/Integer.", "?WAMCode" : "Variable or a list of WAM instructions in the right format.", "+Module" : "Atom"],
	resat:"   No.",
	fail_if:"   Fails if WAMCode is initially instantiated and does not unify with the WAM code generated by asm/1 for the predicate, or if PredSpec is dynamic.",
	exceptions:[5 : "PredSpec or Module not in correct form.", 60 : "PredSpec does not exist in module Module.", 80 : "Module is not an existing module."],
	see_also:[disasm / 2, asm / 2, asm / 3, pasm / 4, wam / 1]]).

:- comment(pasm / 4, [
	summary:"Partially assemble WAMCode into an object format.

",
	desc:html("   Partially assemble the WAM instructions given WAMCode without loading it
   into the current session. Instead, an object format is generated. This
   object format can be loaded into an ECLiPSe session using the low level
   built-in store_pred/9. fcompile/1,2 uses this predicate to generate the
   object code for predicates. BTPos is the offset in words to the break/
   port table, which are the addresses to the positions in the code for the 
   predicate where a breakpoint can be set (body goals which are tracable).

<P>
   The partially assembled code consists of Object, which is a typed
   representation of the words that need to be stored into memory; and
   Size, the size in words that this object code will occupy in memory.

<P>
"),
	amode:(pasm(+,-,-,-) is semidet),
	args:["WAMCode" : "A list of WAM instructions in the right format.",
		"Size" : "Variable or integer",
		"BTPos" : "Variable or integer",
		"Object" : "A list of object words in the right format."],
	resat:"   No.",
	fail_if:"   If WAMCode is not in correct format.",
	see_also:[asm / 2, asm / 3, disasm / 2, disasm / 3, fcompile / 1, fcompile / 2, store_pred/9, portable_object_code/2]]).

:- comment(portable_object_code / 1, [
	summary:"Check whether abstract machine code is 32/64 bit portable",
	desc:html("\
   This check can be run on the output of pasm/4.
   <P>
   ECLiPSe runtime engines on 32/64 bit hardware use different abstract
   machine instructions when processing integers that are between 32 and
   64 bits in size.  Code (and .eco files) that contain such instructions
   cannot be used on a runtime with different word-size from where it was
   assembled.  This predicate prints warnings and fails if the given code
   contains such constructs.
"),
	amode:(portable_object_code(++) is semidet),
	args:["Object" : "A list of object words, as produced by pasm/4."],
	fail_if:"If Object is not portable between 32/64 bit.",
	see_also:[pasm/4, store_pred/9]]).

:- comment(wam / 1, [
	summary:"Prints the formatted WAM code for predicate PredSpec.

",
	template:"wam(+PredSpec)",
	desc:html("   Prints the WAM instructions representing the predicate specified by
   PredSpec from the current module in a formatted form. Requires the 
   library asm to be loaded.

<P>
   If PredSpec is an atom (i.e. no arity is given), then a predicate with
   that name is printed, and if there are more than one predicate defined
   (i.e. same name but different arities), then these different predicates
   will be printed by backtracking.

<P>
   This predicate is intended as a replacement for the lower level als/1,
   which performs the same function. The differences are that the abstract
   instruction names are printed in a more human oriented form (rather than
   the internal names used by ECLiPSe), and labels and their references are
   printed symbolically. Note that the predicate is implemented via the
   disasm/3 predicate of the library, and hence the same restrictions
   applies: it cannot be used to print the code for dynamic predicates.

<P>
"),
	args:["+PredSpec" : "Atom, or Atom/Integer"],
	resat:"   Yes.",
	fail_if:"   If PredSpec is a dynamic predicate.",
	exceptions:[5 : "PredSpec not in correct form.", 60 : "PredSpec not defined in the current module."],
	see_also:[disasm / 2, disasm / 3, wam / 2, als / 1]]).

:- comment(wam / 2, [
	summary:"Prints the formatted WAM code for predicate PredSpec from module Module.",
	template:"wam(+PredSpec, +Module)",
	desc:html("   Prints the WAM instructions representing the predicate specified by
   PredSpec in a formatted form. Requires the library asm to be loaded.

<P>
   If PredSpec is an atom (i.e. no arity is given), then a predicate with
   that name is printed, and if there are more than one predicate defined
   (i.e. same name but different arities), then these different predicates
   will be printed by backtracking.

<P>
   This predicate is intended as a replacement for the lower level als/1,
   which performs the same function. The differences are that the abstract
   instruction names are printed in a more human oriented form (rather than
   the internal names used by ECLiPSe), and labels and their references are
   printed symbolically. Note that the predicate is implemented via the
   disasm/3 predicate of the library, and hence the same restrictions
   applies: it cannot be used to print the code for dynamic predicates.

<P>
"),
	args:["+PredSpec" : "Atom, or Atom/Integer", "+Module" : "Atom."],
	resat:"   Yes.",
	fail_if:"   If PredSpec is a dynamic predicate.",
	exceptions:[5 : "PredSpec or Module not in correct form.", 60 : "PredSpec not defined in module Module.", 80 : "Module is not an existing module."],
	see_also:[disasm / 2, disasm / 3, wam / 1, als / 1]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- export asm/2, asm/3,
          disasm/2, disasm/3,
	  wam/1, wam/2,
	  print_wam/1,
	  portable_object_code/1,
	  pasm/4.

:- local struct(label(add,label)), struct(tab(type,table)), 
	 struct(try(table,size,ref)).

:- tool(asm/2, asm_/3), 
   tool(asm/3, asm_/4),
   tool(disasm/2, disasm/3),
   tool(wam/1, wam/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- import get_bip_error/1,  % for error handling
          set_bip_error/1 
   from sepia_kernel.

:- import store_pred/9,
          retrieve_code/3, 
          meta_index/2, 
	  decode_code/2, 
	  integer_list/3,
          functor_did/2 
   from sepia_kernel.

:- lib(hash).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*                  INSTRUCTION TABLE

   instr/3 lists the abtract instructions for ECLiPSe's WAM:

         instr(WAM, OpCode, TypeList)

   where 

         WAM      is the symbolic WAM form of the instruction
         OpCode   is the instruction's Op-code
         TypeList is a list of the types expected for the arguments of the
                  instruction, in the order in which they are stored in memory.

  The following types are recognised:

      Syntax             Meaning                                       Size
      =====================================================================
	a(A)		 argument register A                              1
	y(Y) 		 permanent variable Y                             1
	t(X) 		 temporary variable X                             1
        pw(N)		 pword offset                                     1
        edesc(EnvDesc)	 environment activity descriptor
			 (integer N, or eam(Bitmap)               1[+table]
        i(I)             integer value I                                  1
        f(Z)  		 floating value Z                                 1
        atom(C)          atom did for C                                   1
        s(S)             string pointer for string S                      1
        ref(L)           reference to  label L                            1
        func(D)          functor did for functor D                        1
        proc(P)          procedure pri for procedure P                    1
        vmask(V)         vmask V                                          2
        nvmask(V)        named vmask                                      2+
        nv(V)            named var V                                      1
        tags(L)          switch labels for switch_on_type        # tagtypes
        tags(L,Ref)      labels for switch_on_type + default     # tagtypes
        port(P)          port                                             1
        brk_port(P)      port (can be used to indicate breakpoint)        1
        tagval(C)        tag + value for constant C (in that order)       2
        valtag(C)        value + tag for constant C (in that order)       2
        mv(M)            meta variable M                                  1
        an(Name)         symbolic name of attribute                       1
        try(Table,Size,Ref)    try table                            1+table
        tab(Type,Table)  switch table of Type (int,atom,functor,
                                               range)               2+table
        o(O)             opcode O. Should only appear in typed-list if    1
                         instruction is `hidden'.
        tref(L)          references to data labels, i.e. outside the      1
                         `code' portion of WAM code.
        skip(S)          skip the next S words                            1

Missing:
    	c(CFunction)	address of this C function (for external* instructions,
			currently using i(CAddress))

*/


instr(label(X), 	             pseudo, [label(X)]). % asm pseudo instr

instr(code_end,				  0, []).
instr(move(a(A)),                         1, [a(A)]).
instr(move(a(A1),a(A2)),       		  2, [a(A1),a(A2)]). 
instr(move(a(A),y(Y)),                    3, [a(A),y(Y)]). 
instr(move(y(Y),a(A)),                    4, [y(Y),a(A)]).
instr(move(t(X),a(A)),                    5, [t(X),a(A)]).
instr(get_variable(N,a(A),y(Y)),          6, [pw(N),a(A),y(Y)]).
instr(get_value(a(A1),a(A2)), 		  7, [a(A1),a(A2)]). 
instr(get_value(a(A),y(Y)),		  8, [a(A),y(Y)]). 
instr(get_value(y(Y),a(A)),		  8, [a(A),y(Y)]). 	% alias
instr(get_value(a(A),t(X)), 	          9, [a(A),t(X)]).
instr(get_nil(a(A)), 		         10, [a(A)]).
instr(get_integer(a(A),C), 		 11, [a(A),i(C)]).
instr(get_float(a(A),C),	         12, [a(A),f(C)]).
instr(get_atom(a(A),C),		         13, [a(A),atom(C)]).
instr(get_string(a(A),C),	         14, [a(A),s(C)]).
instr(get_list(a(A),ref(L)),	         15, [a(A),ref(L)]).
instr(get_structure(a(A),D,ref(L)),    	 16, [a(A),func(D),ref(L)]).
instr(in_get_nil(a(A)), 	         17, [a(A)]).
instr(in_get_integer(a(A),C), 	         18, [a(A),i(C)]).
instr(in_get_float(a(A),C), 	         19, [a(A),f(C)]).
instr(in_get_atom(a(A),C), 		 20, [a(A),atom(C)]).
instr(in_get_string(a(A),C), 	         21, [a(A),s(C)]).
instr(in_get_list(a(A),ref(L)),		 22, [a(A),ref(L)]).
instr(in_get_structure(a(A),D,ref(L)),   23, [a(A),func(D),ref(L)]).
instr(out_get_nil(a(A)),                 24, [a(A)]).
instr(out_get_integer(a(A),C),           25, [a(A),i(C)]).
instr(out_get_float(a(A),C), 	         26, [a(A),f(C)]).
instr(out_get_atom(a(A),C), 	         27, [a(A),atom(C)]).
instr(out_get_string(a(A),C), 	         28, [a(A),s(C)]).
instr(out_get_list(a(A)),	         29, [a(A)]). 
instr(out_get_structure(a(A),D),         30, [a(A),func(D)]).
instr(get_list_arguments(a(A)),          31, [a(A)]).
instr(get_structure_arguments(a(A)),     32, [a(A)]).
instr(write_void,  		         33, []).
instr(read_void, 		         34, []).
instr(write_variable, 		         35, []).
instr(read_variable,   		         36, []).
instr(write_variable(a(A)), 	         37, [a(A)]).
instr(read_variable(a(A)), 		 38, [a(A)]).
instr(write_variable(N,y(Y)), 	         39, [pw(N),y(Y)]).
instr(read_variable(N,y(Y)), 	         40, [pw(N),y(Y)]).
instr(write_variable(y(Y)), 	         41, [y(Y)]).
instr(read_variable(y(Y)), 		 42, [y(Y)]).
instr(write_value(a(A)), 	         43, [a(A)]).
instr(read_value(a(A)),		         44, [a(A)]).
instr(read_matched_value(a(A)),	         45, [a(A)]).
instr(write_local_value(a(A)),           46, [a(A)]).
instr(write_value(y(Y)), 	         47, [y(Y)]).
instr(read_value(y(Y)), 		 48, [y(Y)]).
instr(read_matched_value(y(Y)), 	 49, [y(Y)]).
instr(write_local_value(y(Y)), 	         50, [y(Y)]).
instr(write_value(t(X)),	         51, [t(X)]).
instr(read_value(t(X)),		         52, [t(X)]).
instr(read_matched_value(t(X)),          53, [t(X)]).
instr(write_local_value(t(X)), 	         54, [t(X)]).
instr(write_nil, 		         55, []).
instr(read_nil, 			 56, []).
instr(write_integer(C), 		 57, [i(C)]).
instr(read_integer(C), 		         58, [i(C)]).
instr(write_float(C), 		         59, [f(C)]).
instr(read_float(C), 			 60, [f(C)]).
instr(write_did(C), 		         61, [func(C)]).
instr(write_atom(C), 		         61, [atom(C)]). % = write_did
instr(read_atom(C), 		         62, [atom(C)]).
instr(write_string(C), 		         63, [s(C)]).
instr(read_string(C), 		         64, [s(C)]).
instr(write_list, 		         65, []).
instr(write_structure(D), 	         66, [func(D)]).
instr(read_list(ref(L)),	         67, [ref(L)]).
instr(read_list(t(X),ref(L)), 	         68, [t(X),ref(L)]).
instr(read_next_list(t(X),ref(L)),       69, [t(X),ref(L)]).
instr(read_last_list(ref(L)), 	         70, [ref(L)]).
instr(read_structure(D,ref(L)),	         71, [func(D),ref(L)]).
instr(read_structure(D,t(X),ref(L)), 	 72, [func(D),t(X),ref(L)]).
instr(read_next_structure(D,t(X),ref(L)),73, [func(D),t(X),ref(L)]).
instr(read_last_structure(D,ref(L)), 	 74, [func(D),ref(L)]).
instr(push_void, 		         75, []).
instr(push_variable(a(A)), 		 76, [a(A)]).
instr(push_variable(y(Y)), 		 77, [y(Y)]).
instr(push_variable, 			 78, []).
instr(push_value(a(A)),		         79, [a(A)]).
instr(push_value(y(Y)),			 80, [y(Y)]).
instr(push_value(t(X)),			 81, [t(X)]).
instr(push_local_value(a(A)), 		 82, [a(A)]).
instr(push_local_value(y(Y)), 		 83, [y(Y)]).
instr(push_local_value(t(X)), 		 84, [t(X)]).
instr(push_nil, 			 85, []).
instr(push_integer(C), 		         86, [i(C)]).
instr(push_float(C), 			 87, [f(C)]).
instr(push_init_variable(y(Y)),		 88, [y(Y)]).
instr(push_string(C), 			 89, [s(C)]).
instr(push_list, 			 90, []).
instr(push_structure(N), 		 91, [pw(N)]).
instr(bounce(P),                         92, [proc(P)]).
instr(first, 				 93, []).
instr(next(t(X)), 			 94, [t(X)]).
instr(mode(t(X)), 			 95, [t(X)]).
instr(next(t(X),ref(L)),		 96, [t(X),ref(L)]).
instr(mode(t(X),ref(L)),		 97, [t(X),ref(L)]).
instr(put_variable(a(A),y(Y)), 		 98, [a(A),y(Y)]).
instr(put_variable(a(A)), 		 99, [a(A)]).
instr(put_unsafe_value(a(A),y(Y)),	100, [a(A),y(Y)]).
instr(put_nil(a(A)), 			101, [a(A)]).
instr(put_integer(a(A),C), 		102, [a(A),i(C)]).
instr(put_float(a(A),C),		103, [a(A),f(C)]).
instr(put_atom(a(A),C),			104, [a(A),atom(C)]).
instr(put_string(a(A),C),		105, [a(A),s(C)]).
instr(put_list(a(A)), 			106, [a(A)]).
instr(put_structure(a(A),D), 		107, [a(A),func(D)]).
instr(puts_variable, 			108, []).
instr(puts_variable(y(Y)), 		109, [y(Y)]).
instr(puts_value(a(A)),			110, [a(A)]).
instr(puts_value(y(Y)),		        111, [y(Y)]).
instr(puts_value(t(X)),			112, [t(X)]).
instr(puts_nil, 			113, []).
instr(puts_integer(C), 			114, [i(C)]).
instr(puts_float(C), 			115, [f(C)]).
instr(puts_atom(C), 			116, [atom(C)]).
instr(puts_string(C), 			117, [s(C)]).
instr(puts_list, 			118, []).
instr(puts_structure(D), 		119, [func(D)]).
instr(integer_switch(a(A),IT,ref(Ld)),  120, [a(A),tab{type:int,
                                              table:IT},ref(Ld)]).
instr(atom_switch(a(A),AT,ref(Ld)),	121, [a(A),tab{type:atom,
                                              table:AT},ref(Ld)]).
instr(list_switch(a(A),ref(Ll),ref(Ln),ref(Ld)),	
                                        122, [a(A),ref(Ll),ref(Ln),ref(Ld)]).
instr(functor_switch(a(A),FT,ref(Ld)),	123, [a(A),tab{type:functor,
					      table:FT},ref(Ld)]).
instr(switch_on_type(a(A),LSt), 	124, [a(A),tags(LSt)]).
instr(atom_switch(y(Y),AT,ref(Ld)),	125, [y(Y),tab{type:atom,
                                              table:AT},ref(Ld)]).
instr(functor_switch(y(Y),FT,ref(Ld)),	126, [y(Y),tab{type:functor,
					      table:FT},ref(Ld)]).
instr(integer_switch(y(Y),IT,ref(Ld)),  127, [y(Y),tab{type:int,
                                              table:IT},ref(Ld)]).
instr(try_me_else(D,N,ref(L)), 		128, [port(D),i(N),ref(L)]).
instr(try(D,N,ref(L)), 			129, [port(D),i(N),ref(L)]).
instr(try(D,N,ref(La),ref(L)),		130, [port(D),i(N),ref(La),ref(L)]).
instr(retry_me_else(D,ref(L)), 		131, [port(D),ref(L)]).
instr(retry(D,ref(L)), 			132, [port(D),ref(L)]).
instr(retry(D,ref(La),ref(L)), 		133, [port(D),ref(La),ref(L)]).
instr(trust_me(D), 			134, [port(D)]).
instr(trust(D,ref(L)), 			135, [port(D),ref(L)]).
instr(allocate(N), 			136, [pw(N)]).
instr(space(N), 			137, [pw(N)]).
instr(initialize(y(VList)), 		138, [vmask(VList)]).
instr(branch(ref(L)), 			139, [ref(L)]).
instr(call(ref(L),N), 			140, [ref(L),edesc(N)]).
instr(call(P,N), 			141, [proc(P),edesc(N)]).
instr(callf(ref(L),N), 			142, [ref(L),edesc(N)]).
instr(callf(P,N), 			143, [proc(P),edesc(N)]).
instr(chain(ref(L)), 			144, [ref(L)]).
instr(chain(P), 			145, [proc(P)]).
instr(chainc(ref(L)), 			146, [ref(L)]).
instr(chainc(P), 			147, [proc(P)]).
instr(chaind(ref(L)), 			148, [ref(L)]).
instr(chaind(P), 			149, [proc(P)]).
instr(jmp(ref(L)),			150, [ref(L)]).
instr(jmp(P), 				151, [proc(P)]).
instr(jmpd(ref(L)), 			152, [ref(L)]).
instr(jmpd(P), 				153, [proc(P)]).
instr(exit, 				154, []).
instr(exitd, 				155, []).
instr(exitc, 				156, []).
instr(ret, 				157, []).
instr(retd, 				158, []).
instr(retn, 				159, []).
instr(savecut, 				160, []).
instr(neckcut, 				161, []).
instr(cut1(O), 				162, [pw(O)]).	% cut(y(1),O)
instr(failure, 				163, []).
instr(continue_after_event, 		164, []).
instr(continue_after_event_debug, 	165, []).
instr(escape(P), 			166, [proc(P)]).
instr(list_switch(y(Y),ref(Ll),ref(Ln),ref(Ld)),	
                                        167, [y(Y),ref(Ll),ref(Ln),ref(Ld)]).
instr(external(P,CFun), 		168, [proc(P),i(CFun)]).
instr(puts_proc(P),			169, [proc(P)]).
instr(debug_call_simple(P,Port,Path,L,F,T,MT,NArgs), 170,
		[proc(P),brk_port(Port),atom(Path),i(L),i(F),i(T),i(MT),i(NArgs)]).
instr(gc,		 		171, []).
instr(debug_exit_simple,		172, []).
instr(refail, 				173, []).
instr(exit_emulator(N), 		174, [i(N)]).
instr(debug_exit, 			175, []).
instr(get_matched_value(a(A),y(Y)),	176, [a(A),y(Y)]).
instr(get_matched_value(y(Y),a(A)),	176, [a(A),y(Y)]).	% alias
instr(nop, 				177, []).
instr(ress(Nt,Na,Ne), 			178, [pw(Nt),i(Na),edesc(Ne)]).
instr(deallocate, 			179, []).
instr(get_constant(a(A),C), 		180, [a(A),valtag(C)]).
instr(in_get_constant(a(A),C), 		181, [a(A),valtag(C)]).
instr(out_get_constant(a(A),C),		182, [a(A),valtag(C)]).
instr(read_constant(C), 		183, [valtag(C)]).
instr(write_constant(C), 		184, [valtag(C)]).
instr(push_constant(C), 		185, [valtag(C)]).
% orders for value, tag is correct for put*_constant!!
instr(put_constant(a(A),C), 		186, [a(A),tagval(C)]).
instr(puts_constant(C), 		187, [tagval(C)]).
instr(get_matched_value(a(A1),a(A2)), 	188, [a(A1),a(A2)]).
instr(get_matched_value(a(A),t(X)), 	189, [a(A),t(X)]).
instr(debug_exit_simple(MT,ref(Args)),	190, [i(MT),ref(Args)]).
instr(put_unsafe_value(a(A),t(X)), 	191, [a(A),t(X)]).
instr(branchs(N,ref(L)),		192, [pw(N),ref(L)]).
instr(gc_test(M), 			193, [pw(M)]).
instr(gc_test(M,N), 			194, [pw(M),i(N)]).
%instr(try_me_dynamic(...), 		195, [...]).
%instr(retry_me_dynamic(...), 		196, [...]).
instr(read_test_var, 			197, []).
instr(retry_me_inline(D,ref(L),N),	198, [port(D),ref(L),edesc(N)]).
instr(trust_me_inline(D,N), 		199, [port(D),edesc(N)]).
instr(set_bp(ref(L)), 			200, [ref(L)]).
instr(restore_bp, 			201, []).
instr(new_bp(ref(L)), 			202, [ref(L)]).
instr(savecut(y(Y)), 			203, [y(Y)]).
instr(cut(y(Y),O), 			204, [y(Y),pw(O)]).
instr(jmpd(N,ref(L)), 			205, [pw(N),ref(L)]).
instr(switch_on_type(y(Y),LSt), 	206, [y(Y),tags(LSt)]).
instr(metacall(N), 			207, [edesc(N)]).
instr(fastcall(P,N), 			208, [port(P),edesc(N)]).
instr(integer_range_switch(y(Y),RT,ref(Le),ref(Ld)), 
					209, [y(Y),tab{type:range,
                                              table:RT},ref(Le),ref(Ld)]).
instr(suspension_call(N), 		210, [edesc(N)]).
instr(throw, 				211, []).
instr(savecut(a(A)), 			212, [a(A)]).
instr(cut_single,	 		213, []).
instr(initialize_named(y(NVList)),      214, [nvmask(NVList)]).
instr(write_named_void(N), 		215, [nv(N)]).
instr(write_named_variable(N), 		216, [nv(N)]).
instr(write_named_variable(a(A),N), 	217, [a(A),nv(N)]).
instr(write_named_variable(y(Y),N),	218, [y(Y),nv(N)]).
instr(write_named_variable(O,y(Y),N),   219, [pw(O),y(Y),nv(N)]).
instr(put_reference(a(A),O,N), 		220, [a(A),pw(O),nv(N)]).
instr(put_reference(a(A),y(Y),O,N), 	221, [a(A),y(Y),pw(O),nv(N)]).
instr(push_self_reference(N), 		222, [nv(N)]).
instr(push_void_reference(O), 		223, [pw(O)]).
instr(push_reference(O), 		224, [pw(O)]).
instr(push_reference(a(A),O), 		225, [a(A),pw(O)]).
instr(push_reference(y(Y),O), 		226, [y(Y),pw(O)]).
instr(puts_reference(O,N), 		227, [pw(O),nv(N)]).
instr(puts_reference(y(Y),O,N),		228, [y(Y),pw(O),nv(N)]).
instr(occur_check_next, 		229, []).
instr(softcut(y(Y)), 			230, [y(Y)]).
instr(dfid_test(y(Y)), 			231, [y(Y)]).
instr(dfid_test, 			232, []).
instr(depth(y(Y)), 			233, [y(Y)]).
%instr(meta_jmp(...), 			234, [...]).
%instr(undefined(P), 			235, [proc(P)]).
%instr(label, 				236, []).		% PSEUDO
instr(comment(S), 			237, [skip(S)]).	% PSEUDO
%instr(reserve, 			238, []).		% PSEUDO
instr(get_meta(a(A),M),			239, [a(A),mv(M)]).
instr(in_get_meta(a(A),ref(L)),		240, [a(A),ref(L)]).
instr(write_meta(N), 			241, [nv(N)]).
instr(match_meta, 			242, []).
instr(match_next_meta(t(X)), 		243, [t(X)]).
instr(match_meta(t(X)),			244, [t(X)]).
instr(match_last_meta, 			245, []).
instr(read_meta(N,ref(L)),		246, [nv(N),ref(L)]).
instr(read_next_meta(t(X),N,ref(L)), 	247, [t(X),nv(N),ref(L)]).
instr(read_meta(t(X),N,ref(L)),		248, [t(X),nv(N),ref(L)]).
instr(read_last_meta(N,ref(L)),		249, [nv(N),ref(L)]).
instr(continue_after_exception, 	250, []).
instr(cut(a(A)), 			251, [a(A)]).
instr(catch, 				252, []).
instr(res(Arity,Size), 			253, [i(Arity),edesc(Size)]).
instr(handler_call(O), 			254, [edesc(O)]).
instr(retd_nowake, 			255, []).
instr(push_init_reference(y(Y),O), 	256, [y(Y),pw(O)]).
instr(exitd_nowake, 			257, []).
instr(meta_jmp, 			258, []).
instr(suspension_jmp, 			259, []).
instr(explicit_jmp, 			260, []).
instr(read_reference(N,y(Y)), 		261, [pw(N),y(Y)]).
instr(read_reference(y(Y)), 		262, [y(Y)]).
instr(read_reference(a(A)), 		263, [a(A)]).
instr(read_reference, 			264, []).
instr(read_void(N), 			265, [pw(N)]).
instr(integer_range_switch(a(A),RT,ref(Le),ref(Ld)), 
					266, [a(A),tab{type:range,
                                              table:RT},ref(Le),ref(Ld)]).
instr(puts_value(G), 			267, [pw(G)]).
instr(push_value(G), 			268, [pw(G)]).
instr(guard(y(Y),ref(L)),		269, [y(Y),ref(L)]).
instr(try_parallel(Size,Ar,TT,O), 	270, [i(Size),i(Ar),try{table:TT,size:Size,ref:Lt}, 
    /*retry_seq(ref(Lt))*/                    o(271),tref(Lt),
    /*fail_clause(O)*/	                      o(272),edesc(O), 
    /*try_clause(ref(Lt))*/                   o(273),tref(Lt)]).       
instr(read_attribute(At), 		274, [an(At)]).
instr(wake_init(N), 			275, [edesc(N)]).
instr(wake, 				276, []).
instr(ret_nowake, 			277, []).
instr(neckcut_par, 			278, []).
instr(extcall(P), 			279, [proc(P)]).
instr(external0(P,CFun), 		280, [proc(P),i(CFun)]).
instr(external1(P,CFun), 		281, [proc(P),i(CFun)]).
instr(external2(P,CFun), 		282, [proc(P),i(CFun)]).
instr(external3(P,CFun), 		283, [proc(P),i(CFun)]).
instr(clause,		 		284, []).

% new instructions for ECLiPSe 6.0
instr(put_global_variable(a(A),y(Y)),	285, [a(A),y(Y)]).
instr(put_global_variable(y(Y)),	286, [y(Y)]).
instr(put_global_variable(a(A)),	287, [a(A)]).
instr(move(y(Y1),y(Y2)),		288, [y(Y1),y(Y2)]).
instr(get_value(y(Y1),y(Y2)),		289, [y(Y1),y(Y2)]).
%instr(escape(P,Args),			290, [proc(P),arglist(Args)]).

% new WAM instructions for inlined builtins
instr(bi_exit(a(A)),			291, [a(A)]).
instr(bi_bignum(a(A1)),			292, [a(A1)]).
instr(bi_callable(a(A1)),		293, [a(A1)]).
instr(bi_cut_to_stamp(a(A1),a(A2),0),	294, [a(A1),a(A2),0]).
instr(bi_set_bip_error(a(A1)),		295, [a(A1)]).
instr(bi_get_bip_error(a(A1)),		296, [a(A1)]).
instr(bi_free(a(A1)),			297, [a(A1)]).
instr(bi_var(a(A1)),			298, [a(A1)]).
instr(bi_nonvar(a(A1)),			299, [a(A1)]).
instr(bi_atom(a(A1)),			300, [a(A1)]).
instr(bi_integer(a(A1)),		301, [a(A1)]).
instr(bi_float(a(A1)),			302, [a(A1)]).
instr(bi_breal(a(A1)),			303, [a(A1)]).
instr(bi_real(a(A1)),			304, [a(A1)]).
instr(bi_rational(a(A1)),		305, [a(A1)]).
instr(bi_string(a(A1)),			306, [a(A1)]).
instr(bi_number(a(A1)),			307, [a(A1)]).
instr(bi_atomic(a(A1)),			308, [a(A1)]).
instr(bi_compound(a(A1)),		309, [a(A1)]).
instr(bi_meta(a(A1)),			310, [a(A1)]).
instr(bi_is_suspension(a(A1)),		311, [a(A1)]).
instr(bi_is_handle(a(A1)),		312, [a(A1)]).
instr(bi_is_event(a(A1)),		313, [a(A1)]).
instr(bi_is_list(a(A1)),		314, [a(A1)]).
instr(bi_identical(a(A1),a(A2)),	315, [a(A1),a(A2)]).
instr(bi_not_identical(a(A1),a(A2)),	316, [a(A1),a(A2)]).
instr(bi_inequality(a(A1),a(A2)),	317, [a(A1),a(A2)]).
instr(bi_not_ident_list(a(A1),a(A2),a(A3)),	318, [a(A1),a(A2),a(A3)]).
instr(bi_cont_debug,			319, []).
instr(bi_minus(a(A1),a(UA2),4),		320, [a(A1),a(UA2),i(4)]).
instr(bi_addi(a(A1),I,a(A2),24),	321, [a(A1),i(I),a(A2),i(24)]).
instr(bi_add(a(A1),a(A2),a(UA3),16),	322, [a(A1),a(A2),a(UA3),i(16)]).
instr(bi_sub(a(A1),a(A2),a(UA3),16),	323, [a(A1),a(A2),a(UA3),i(16)]).
instr(bi_mul(a(A1),a(A2),a(UA3),16),	324, [a(A1),a(A2),a(UA3),i(16)]).
instr(bi_quot(a(A1),a(A2),a(UA3),16),	325, [a(A1),a(A2),a(UA3),i(16)]).
instr(bi_div(a(A1),a(A2),a(UA3),16),	326, [a(A1),a(A2),a(UA3),i(16)]).
instr(bi_rem(a(A1),a(A2),a(UA3),16),	327, [a(A1),a(A2),a(UA3),i(16)]).
instr(bi_fdiv(a(A1),a(A2),a(UA3),16),	328, [a(A1),a(A2),a(UA3),i(16)]).
instr(bi_mod(a(A1),a(A2),a(UA3),16),	329, [a(A1),a(A2),a(UA3),i(16)]).
instr(bi_and(a(A1),a(A2),a(UA3),16),	330, [a(A1),a(A2),a(UA3),i(16)]).
instr(bi_or(a(A1),a(A2),a(UA3),16),	331, [a(A1),a(A2),a(UA3),i(16)]).
instr(bi_xor(a(A1),a(A2),a(UA3),16),	332, [a(A1),a(A2),a(UA3),i(16)]).
instr(bi_bitnot(a(A1),a(UA2),4),	333, [a(A1),a(UA2),i(4)]).
instr(bi_lt(a(A1),a(A2),a(A3),0),	334, [a(A1),a(A2),a(A3),i(0)]).
instr(bi_lt(a(A1),a(A2),M,48),		334, [a(A1),a(A2),atom(M),i(48)]).
instr(bi_le(a(A1),a(A2),a(A3),0),	335, [a(A1),a(A2),a(A3),i(0)]).
instr(bi_le(a(A1),a(A2),M,48),		335, [a(A1),a(A2),atom(M),i(48)]).
instr(bi_gt(a(A1),a(A2),a(A3),0),	336, [a(A1),a(A2),a(A3),i(0)]).
instr(bi_gt(a(A1),a(A2),M,48),		336, [a(A1),a(A2),atom(M),i(48)]).
instr(bi_ge(a(A1),a(A2),a(A3),0),	337, [a(A1),a(A2),a(A3),i(0)]).
instr(bi_ge(a(A1),a(A2),M,48),		337, [a(A1),a(A2),atom(M),i(48)]).
instr(bi_eq(a(A1),a(A2),a(A3),0),	338, [a(A1),a(A2),a(A3),i(0)]).
instr(bi_eq(a(A1),a(A2),M,48),		338, [a(A1),a(A2),atom(M),i(48)]).
instr(bi_ne(a(A1),a(A2),a(A3),0),	339, [a(A1),a(A2),a(A3),i(0)]).
instr(bi_ne(a(A1),a(A2),M,48),		339, [a(A1),a(A2),atom(M),i(48)]).
instr(bi_arg(a(A1),a(A2),a(UA3),16),	340, [a(A1),a(A2),a(UA3),i(16)]).
instr(bi_arg(I,a(A2),a(UA3),18),	340, [i(I),a(A2),a(UA3),i(18)]).
instr(bi_make_suspension(a(A1),a(A2),a(A3),a(A4),0),	    
					341, [a(A1),a(A2),a(A3),a(A4),i(0)]).	
instr(debug_call(P,Port,Path,L,F,T),	342, [proc(P),brk_port(Port),atom(Path),i(L),i(F),i(T)]).
                                             /* caution: p_proc_flags() and p_proc_set_flags()
                                                in bip_db.c relies on the above argument order!
                                             */
instr(retry_inline(D,ref(L),N),		343, [port(D),ref(L),edesc(N)]).
instr(trust_inline(D,ref(L),N),		344, [port(D),ref(L),edesc(N)]).
instr(put_named_variable(a(A),N), 	345, [a(A),nv(N)]).
instr(put_named_variable(y(Y),N),	346, [y(Y),nv(N)]).
instr(put_named_variable(a(A),y(Y),N),	347, [a(A),y(Y),nv(N)]).
%instr(call_dynamic(P,ref(L)),		348, [proc(P),ref(L)]).
% more new instructions for ECLiPSe 6.0 - generated by peephole optimizer
instr(write_void(N), 			349, [pw(N)]).
instr(push_void(N), 			350, [pw(N)]).
instr(move(N, y(Y), a(A)),		351, [i(N), y(Y), a(A)]).
instr(move(N, a(A), y(Y)),		352, [i(N), a(A), y(Y)]).
instr(move(y(Y1),a(A1),y(Y2),a(A2)),    353, [y(Y1),a(A1),y(Y2),a(A2)]).
instr(move(y(Y1),a(A1),y(Y2),a(A2),y(Y3),a(A3)),
      					354, [y(Y1),a(A1),y(Y2),a(A2),y(Y3),a(A3)]).
instr(move(a(A1),y(Y1),a(A2),y(Y2)),    355, [a(A1),y(Y1),a(A2),y(Y2)]).
instr(move(a(A1),y(Y1),a(A2),y(Y2),a(A3),y(Y3)),
      					356, [a(A1),y(Y1),a(A2),y(Y2),a(A3),y(Y3)]).
instr(move(a(A1),a(A2),a(A3),a(A4)),    357, [a(A1),a(A2),a(A3),a(A4)]).
instr(move(a(A1),a(A2),a(A3),a(A4),a(A5),a(A6)),
      					358, [a(A1),a(A2),a(A3),a(A4),a(A5),a(A6)]).
instr(move(y(Y1),y(Y2),y(Y3),y(Y4)),    359, [y(Y1),y(Y2),y(Y3),y(Y4)]).
instr(move(y(Y1),y(Y2),y(Y3),y(Y4),y(Y5),y(Y6)),
      					360, [y(Y1),y(Y2),y(Y3),y(Y4),y(Y5),y(Y6)]).
instr(swap(a(A1),a(A2)),		361, [a(A1),a(A2)]).
instr(shift(a(A1),a(A2),a(A3)), 	362, [a(A1),a(A2),a(A3)]).
instr(shift(a(A1),a(A2),a(A3),a(A4)), 	363, [a(A1),a(A2),a(A3),a(A4)]).
instr(shift(a(A1),a(A2),a(A3),a(A4),a(A5)), 	
      					364, [a(A1),a(A2),a(A3),a(A4),a(A5)]).
instr(read_variable2(a(A1),y(Y2)),	365, [a(A1),y(Y2)]).
instr(read_variable2(a(A1),a(A2)),	366, [a(A1),a(A2)]).
instr(read_variable2(y(Y1),y(Y2)),	367, [y(Y1),y(Y2)]).
instr(write_variable2(a(A1),y(Y2)),	368, [a(A1),y(Y2)]).
instr(write_variable2(a(A1),a(A2)),	369, [a(A1),a(A2)]).
instr(write_variable2(y(Y1),y(Y2)),	370, [y(Y1),y(Y2)]).
instr(write_local_value2(a(A1),a(A2)),	371, [a(A1),a(A2)]).
instr(write_local_value2(y(Y1),y(Y2)),	372, [y(Y1),y(Y2)]).
instr(push_local_value2(a(A1),a(A2)),	373, [a(A1),a(A2)]).
instr(push_local_value2(y(Y1),y(Y2)),	374, [y(Y1),y(Y2)]).
instr(put_global_variable2(a(A1),y(Y1),a(A2),y(Y2)),	
      	                                375, [a(A1),y(Y1),a(A2),y(Y2)]).
instr(put_variable2(a(A1),a(A2)),       376, [a(A1),a(A2)]).
instr(get_atom2(a(A1),C1,a(A2),C2),     377, [a(A1),atom(C1),a(A2),atom(C2)]).
instr(get_integer2(a(A1),C1,a(A2),C2),  378, [a(A1),i(C1),a(A2),i(C2)]).
instr(get_atominteger(a(A1),C,a(A2),I), 379, [a(A1),atom(C),a(A2),i(I)]).
instr(write_first_structure(D),         380, [func(D)]).
instr(write_first_list,                 381, []).
instr(write_next_structure(D,t(X)),     382, [func(D),t(X)]).
instr(write_next_list(t(X)),            383, [t(X)]).
instr(write_next_structure(D,t(X),ref(L)),     
       					384, [func(D),t(X),ref(L)]).
instr(write_next_list(t(X),ref(L)),     385, [t(X),ref(L)]).
instr(read_atom2(C1,C2),		386, [atom(C1),atom(C2)]).
instr(read_integer2(C1,C2),		387, [i(C1),i(C2)]).
instr(read_integeratom(C1,C2),		388, [i(C1),atom(C2)]).
instr(read_atominteger(C1,C2),		389, [atom(C1),i(C2)]).
instr(write_did2(C1,C2),		390, [func(C1),func(C2)]).
instr(write_atom2(C1,C2),		390, [atom(C1),atom(C2)]).  %=write_did2
instr(write_atomdid(C1,C2),		390, [atom(C1),func(C2)]).  %=write_did2
instr(write_didatom(C1,C2),		390, [func(C1),atom(C2)]).  %=write_did2
instr(write_integer2(C1,C2),		391, [i(C1),i(C2)]).
instr(write_integerdid(C1,C2),		392, [i(C1),func(C2)]).
instr(write_integeratom(C1,C2),		392, [i(C1),atom(C2)]).     %=write_integerdid
instr(write_didinteger(C1,C2),		393, [func(C1),i(C2)]).
instr(write_atominteger(C1,C2),		393, [atom(C1),i(C2)]).     %=writedidinteger
instr(move_callf(y(Y),a(A),ref(L),N),	394, [y(Y),a(A),ref(L),edesc(N)]).
instr(move_callf(y(Y),a(A),P,N),	395, [y(Y),a(A),proc(P),edesc(N)]).
instr(move_chain(y(Y),a(A),ref(L)),	396, [y(Y),a(A),ref(L)]).
instr(move_chain(y(Y),a(A),P),		397, [y(Y),a(A),proc(P)]).
instr(put_global_variable_callf(a(A),y(Y),ref(L),N),	
                                        398, [a(A),y(Y),ref(L),edesc(N)]).
instr(put_global_variable_callf(a(A),y(Y),P,N),	
                                        399, [a(A),y(Y),proc(P),edesc(N)]).
instr(rot(a(A1),a(A2),a(A3)),		400, [a(A1),a(A2),a(A3)]).
instr(bi_arity(a(A1),a(UA2),4),		401, [a(A1),a(UA2),i(4)]).
instr(exits(N),                         402, [pw(N)]).
instr(cut(a(A),O), 			403, [a(A),pw(O)]).
instr(put_module(a(A),C),		404, [a(A),atom(C)]).


/***************************************************************************
 assemble
****************************************************************************/
%
% IMPLEMENTATION:
%
%	asm/2 is based on the low-level builtins store_pred/9.
%	store_pred(+PredSpec, +CodeList, +Size, +BTPos, +Flags, 
%                  +File, +Line, +Offset, +Module)
%	maps every element of CodeList into  memory. CodeList is a session
%       independent representation of the WAM code for PredSpec. Each
%       element generally maps onto one memory word, except for switch
%       tables which must be sorted at load-time. Size is the size in words
%       for the WAM code (including break-table) of PredSpec. BTPos is the
%       offset in words from the start of code to the break-table, or 0 if
%       there is no break-table. File, Line, Offset gives source
%       information for the predicate, but is unused here (all set to 0)
%
% FORMAT
%
%      The CodeList for a predicate consists of two parts: the instruction
%      part, followed by the data part. The instruction part contains the
%      WAM instructions, and the data part the tables for the predicate.
%      The tables must be pword aligned. 

% asm(+PredSpec, +ListOfInstructions, +Module)


asm_(Pred, WAMList, Module) :-
	asm_(Pred, WAMList, 0, Module).


asm_(Pred, WAMList, Flags, Module) :-
	( integer(Flags) ->
	    (Pred = F/A, is_proc(F/A) ->
		pass1(WAMList, _H, WordList0, BrkTable0),
		!,
		(BrkTable0 = [0] ->
		    % no break ports, terminating 0 only
		    BTSize = 0, BTPos = 0, BrkTable = []
		;
		    BTPos = CSize, BrkTable = BrkTable0,
		    length(BrkTable, BTSize)
		),
		link(WordList0, 0, CSize, OutputList, BrkTable),
		Size is CSize + BTSize,
		store_pred(Pred, OutputList, Size, BTPos, Flags, 0, 0, 0, Module)
	    ;   
		set_bip_error(5)
	    )
	; atom(Flags) ->
	    % backward compatibility: allow asm(Pred,WAM,Module)
	    asm_(Pred, WAMList, 0, Flags)
	;
	    set_bip_error(5)
	).
asm_(Pred, WAMList, Flags, Module) :-
	get_bip_error(E),
	error(E, asm(Pred,WAMList,Flags), Module).


/* pasm(+WAMList, -Size, -BTPos, -OutList)

   WAMList:  list of WAM code for PredSpec
   Size:     size in words of assembled code, including break-table
   BTPos:    offset from start of code in words to the break-points table
   OutList:  a flat word list of assembled code, with tables which cannot be
             resolved until load time bracketed by dep_table(..)
*/
pasm(WAMList, Size, BTPos, OutList) :-
	pass1(WAMList, _H, WordList0, BrkTable0),
	!,
        (BrkTable0 = [0] ->
            % no break ports, terminating 0 only
            BTSize = 0, BTPos = 0, BrkTable = []
        ;
            BTPos = CSize, BrkTable = BrkTable0,
            length(BrkTable, BTSize)
        ),
        link(WordList0, 0, CSize, OutList, BrkTable),
        Size is CSize + BTSize.
pasm(WAMList, Size, BTPos, OutList) :-
	get_bip_error(E),
	error(E, pasm(WAMList, Size, BTPos, OutList)).


/* pass1(+WAMList, -Hash, -WordList, -BrkList)

   generates the independent typed WordList from the WAM instructions list
   WAMList, and a list BrkList of ref to brk_port words, which forms the
   break-table (terminated by a 0).

   This typed word list has no low level dependencies in that the switch
   tables whose entries may be session dependent are stored in source form
   at the place where the table is to be inserted in memory. The format
   for this list is what is accepted by the allocate_code built-in

   Hash is used to store the label index to variable mapping - this allow
   label in WAMList to be an integer, which is replaced with a variable.
*/
pass1(WAMList, H, WordList, BrkList) :-
        hash_create(H),
        instr(code_end, Code_end, _), 
        % made sure there is a code_end at end of instructions
	pass1(WAMList, H, WordList, [o(Code_end)|TList], TList, BrkList).


pass1([], _, IList, IList, [], [0]).  % leave IList tail as var, terminating 0 for BrkList
pass1([Instr|Instrs], H, IList0, IList, TList0, BrkList0) :-
	(instr(Instr, Opc, Typed) ->
	    (fill_wordlist(Opc, Typed, H, IList0, IList1, TList0, TList1, BrkList0, BrkList1) ->
		pass1(Instrs, H, IList1, IList, TList1, BrkList1)
	    ;   printf(error, "%w contains unexpected arguments.%n", [Instr]),
	        set_bip_error(6)
	    )
	;   printf(error, "%w instruction not recognised.%n", [Instr]),
            set_bip_error(6)
        ).



fill_wordlist(pseudo, Typed, H, IList0, IList, TList0, TList, BList0, BList) :- !,
% asm psuedo instruction, do not add Opc to list
	asm_args(Typed, H, IList0, IList, TList0, TList, BList0, BList).
fill_wordlist(Opc, Typed, H, IList0, IList, TList0, TList, BList0, BList) :-
	integer(Opc), 
	IList0 = [o(Opc)|IList1],
	asm_args(Typed, H, IList1, IList, TList0, TList, BList0, BList).


asm_args([], _H, IList0, IList, TList0, TList, BList0, BList) ?- 
	IList0 = IList, TList0 = TList, BList0 = BList.
asm_args([Arg|Args], H, IList0, IList, TList0, TList, BList0, BList) ?-
	asm_arg(Arg, H, IList0, IList1, TList0, TList1, BList0, BList1),
	asm_args(Args, H, IList1, IList, TList1, TList, BList1, BList).

/* asm_arg(+Arg, +Hash, -InstrIn, -InstrOut, +TableIn, -TableOut, +BrkIn, -BrkOut)

   assembles an argument for a WAM instr. It performs some simple type checking
   on Arg and then creates the corresponding typed word(s).

     Arg:       current argument  
     Hash:      hash table storing the mapping for any integer index label
                to their variable label replacements
     Instr:     independent word list pair, where the corresponding typed
                word(s) to Arg will be generated
     Table:     table word list pair, where any tables generated by an
                argument is placed. This list is later appended to the end
                of the instruction list 
     BList:     list of ref to brk_port words in code for predicate. This 
                list is later appended to the end of the word list (after 
                Table, with a terminating 0, to form the full independent 
                word list. 
     The cut in each clause is not strictly needed as the clauses are mutually
     exclusive. They are used to distinguish the type testing and the typed
     word creation phases.

     Arg's type corresponds to the types used in instr/3
*/
asm_arg(a(A), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	integer(A), A >= 0, !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [a(A)|IList].
asm_arg(arglist(AList), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	nonvar(AList), !,
	TList0 = TList,
        BList0 = BList,
	( foreach(Arg,AList), fromto(IList0,[Arg|IList1],IList1,IList) do
	    nonvar(Arg), Arg = a(A), integer(A), A>0
	).
asm_arg(y(Y), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	integer(Y), Y >= 0, !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [y(Y)|IList].
asm_arg(t(X), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	integer(X), !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [t(X)|IList].
asm_arg(pw(N), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	integer(N), !, % no wrappers 
	TList0 = TList,
        BList0 = BList,
	IList0 = [pw(N)|IList]. 
asm_arg(edesc(EDesc), _H, IList0, IList, TList0, TList, BList0, BList) ?- !,
        BList0 = BList,
	encode_edesc(EDesc, IList0, IList, TList0, TList).
asm_arg(i(N), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	integer(N), !, 
	TList0 = TList,
        BList0 = BList,
	IList0 = [N|IList].
asm_arg(f(N), _H, IList0, IList, TList0, TList, BList0, BList) ?- 
	float(N), !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [N|IList]. 
asm_arg(atom(A), _H, IList0, IList, TList0, TList, BList0, BList) ?- 
	atom(A), !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [A|IList].
asm_arg(s(S), _H, IList0, IList, TList0, TList, BList0, BList) ?- 
	string(S), !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [S|IList].
asm_arg(ref(L0), H, IList0, IList, TList0, TList, BList0, BList) ?-
	valid_reflab(L0), !,
        label_idx_to_var(L0, H, L),
	TList0 = TList,
        BList0 = BList,
	IList0 = [ref(L)|IList].
asm_arg(label(L0), H, IList0, IList, TList0, TList, BList0, BList) ?-
	( var(L0) ->
            L0 = L
        ; integer(L0) ->
            label_idx_to_var(L0, H, L)
        ;
            fail
        ), !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [label(L)|IList].
asm_arg(func(N/A), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	atom(N), integer(A), !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [functor(N/A)|IList].
asm_arg(proc(P), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	is_proc(P), !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [proc(P)|IList].
asm_arg(vmask(VList), _H, IList0, IList, TList0, TList, BList0, BList) ?- 
	sort(VList,Sorted),
	(foreach(E,Sorted) do integer(E)), !,
	Sorted = [First|_],
	TList0 = TList,
        BList0 = BList,
	IList0 = [y(First),ymask(Sorted)|IList]. 
asm_arg(nvmask(NVList), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	sort(1, <, NVList, NVSorted),
	split_varsnames(NVSorted, VList, NList), !,
	TList0 = TList,
        BList0 = BList,
	VList = [First|_],
	% NList elements already in form nv(Name)
	append([y(First),ymask(VList)|NList], IList, IList0).
asm_arg(nv(VN), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	atom(VN), !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [nv(VN)|IList].
asm_arg(tags(LList), H, IList0, IList, TList0, TList, BList0, BList) ?-
	decode_code(tags, Tags),
	functor(Tags, tags, Arity),
	functor(TagLabels, tags, Arity), % create 
	(foreach(Tag:Ref,LList), param([TagLabels,Tags]) do
            valid_ref(Ref),
	    find_arg_in_struct(Tag, Tags, Pos),
	    arg(Pos, TagLabels, Ref)
	),
	TList0 = TList,
        BList0 = BList,
	(foreacharg(Ref0,TagLabels), 
         fromto(IList0,[Ref|IList1],IList1,IList), param(H)  do
             ( var(Ref0) -> 
                 Ref = ref(fail) 
             ; 
                 Ref0 = ref(L0), 
                 Ref = ref(L),
                 label_idx_to_var(L0, H, L)
             )
	).
asm_arg(tags(LList,DefRef), H, IList0, IList, TList0, TList, BList0, BList) ?-
	valid_ref(DefRef0),
        DefRef0 = ref(L0),
        DefRef = ref(L),
        label_idx_to_var(L0, H, L),
	decode_code(tags, Tags),
	functor(Tags, tags, Arity),
	functor(TagLabels, tags, Arity), % create 
	(foreach(Tag:Ref,LList), param([TagLabels,Tags]) do
            valid_ref(Ref),
	    find_arg_in_struct(Tag, Tags, Pos),
	    arg(Pos, TagLabels, Ref)
	),
	TList0 = TList,
        BList0 = BList,
	(foreacharg(Ref0,TagLabels), 
         fromto(IList0,[Ref|IList1],IList1,IList), param(DefRef, H) do
             (var(Ref0) -> 
                 Ref = DefRef 
             ; 
                 Ref0 - ref(L0),
                 Ref = ref(L),
                 label_idx_to_var(L0, H, L)
             )
	).
asm_arg(port(P), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	integer(P), !, % treat in raw form as an int for now
	TList0 = TList,
        BList0 = BList,
	IList0 = [P|IList].
asm_arg(brk_port(P), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	integer(P), !, % treat in raw form as an int for now
	TList0 = TList,
        BList0 = [ref(BLab)|BList],
	IList0 = [brk_port(P,BLab)|IList].        
asm_arg(valtag(C), _H, IList0, IList, TList0, TList, BList0, BList) ?- 
	ground(C), !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [val(C),tag(C)|IList].
asm_arg(tagval(C), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	ground(C), !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [tag(C),val(C)|IList].
asm_arg(mv(M), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	atom(M), !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [mv(M)|IList].
asm_arg(an(Att), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	atom(Att), !,
	TList0 = TList,
        BList0 = BList,
	IList0 = [an(Att)|IList].
asm_arg(try{table:Table,ref:TLabel}, H, IList0, IList, TList0, TList, BList0, BList) ?-
	(foreach(Branch, Table) do 
           valid_ref(Branch)
        ), !,
        BList0 = BList,
	IList0 = [ref(TLabel0)|IList],
        label_idx_to_var(TLabel0, H, TLabel),
	append([align(2),label(TLabel)|Table], TList, TList0).
asm_arg(tab{table:Table,type:Type}, H,  IList0, IList, TList0, TList, BList0, BList) ?- !,
	table_size(Table, Type, Size),
	IList0 = [ref(TLabel), Size|IList],
        BList0 = BList,
	make_switch_table(Type, H, TLabel, Table, TList0, TList).
asm_arg(o(N), _H, IList0, IList, TList0, TList, BList0, BList) ?-
	integer(N), N >= 0, !, 
	TList0 = TList,
        BList0 = BList,
	IList0 = [o(N)|IList].
asm_arg(tref(L), H, IList0, IList, TList0, TList, BList0, BList) ?- !,
% for assembly, treat just like normal refs
        BList0 = BList,
	asm_arg(ref(L), H, IList0, IList, TList0, TList, BList0, BList).
asm_arg(skip(_S), _H, _IList0, _IList, _TList0, _TList, _BList0, _BList) ?- !,
	printf(error, "comment instruction not supported for asm/3"),
	set_bip_error(6).


is_proc(M:N/A) ?- atom(M), atom(N), integer(A).
is_proc(N/A) ?- atom(N), integer(A).

valid_functor(N/A) ?- atom(N), integer(A).


valid_ref(ref(L)) ?- valid_reflab(L).

valid_reflab(L) :- var(L), !.
valid_reflab(I) :- integer(I), !.  % Code segment index (new compiler)
valid_reflab(L) :- valid_symbol(L).


% Warn and fail if the code contains bignums between 32 and 64 bits
portable_object_code(Ws) :-
	portable_object_code(Ws, Res),
	Res = true.	% fail late

portable_object_code([], true).
portable_object_code([W|Ws], Result) :-
	( W = tag(C), integer(C), -2^63 =< C, C =< 2^63-1 ->
	    Result = false,
	    printf(warning_output,
	   	"WARNING: A bignum between -2^63 and 2^63 found in code (%w)%n", [C]),
	    portable_object_code(Ws, _)
	;
	    portable_object_code(Ws, Result)
	).


% encode/decode environment descriptors
% We allow the following specifications
%	- integer (environment size, for a transition period)
%	- eam(integer or bignum) (environment activity bitmap)
%	- SlotList (list of active Y slot numbers)
encode_edesc(ESize, Is, Is0, Ts, Ts0) :-
	integer(ESize), !,
	Is = [pw(ESize)|Is0], Ts = Ts0.
encode_edesc(eam(EAM), Is, Is0, Ts, Ts0) ?- !,
	( EAM =< 2147483647 -> 
	    % small bitmap, store inline
	    Is = [MarkedChunk|Is0], Ts = Ts0,
	    shl_32bit(EAM, ShiftedChunk),
	    MarkedChunk is ShiftedChunk+1		% mark as inline bitmap
	;
	    % large bitmap, store separately, pointer in code
	    % The pointer is tagged by adding 2.
	    Is = [refm(BigMap,2)|Is0],
	    Ts = [label(BigMap)|Ts1],
	    integer_list(EAM, 31, Chunks),		% make 31-bit chunks
	    (
		fromto(Chunks,[Chunk|Chunks1],Chunks1,[ChunkN]),
		fromto(Ts1,[ShiftedChunk|Ts2],Ts2,[MarkedChunkN|Ts0])
	    do
		shl_32bit(Chunk, ShiftedChunk)
	    ),
	    MarkedChunkN is shl_32bit(ChunkN) + 1	% mark as last chunk
	).
encode_edesc(Bits, Is, Is0, Ts, Ts0) :-
	is_list(Bits),
	( foreach(Bit,Bits), fromto(0,EAM1,EAM2,EAM) do
	    EAM2 is setbit(EAM1, Bit-1)
	),
	encode_edesc(eam(EAM), Is, Is0, Ts, Ts0).


    % shift left by one, simulating 32-bit two's complement arithmetic
    shl_32bit(X, R) :-
	( X >= 16'40000000 ->
	    % shift would overflow signed 32 bits: subtract 2^32, i.e.
	    % R is X<<1 - 2^32, then rewrite to avoid bignums:
	    R is -2147483647 - 1 + (X-16'40000000)<<1
	;
	    R is X << 1
	).


/* link(+CodeList, +PosIn, -PosOut, -FinalCodeList)

   generates the final code list that will be stored by store_pred/9. It takes
   the CodeList generated by pass1, and fills in the references, remove 
   labels, fills in alignments, and computes the size in words for the final 
   code list.

   PosIn: current position in final code list (in words) from start of list
   PosOut: final position at end of final code list, i.e. size of list
*/
link([], Size, Size, Output, Output).
link([label(Displ)|Ws], Pos0, Pos, Output, OutputT) ?- !,
	Displ = Pos0,
	link(Ws, Pos0, Pos, Output, OutputT).
link([align(N)|Ws], Pos0, Pos, Output, OutputT) ?- !,
	insert_nops(N, Pos0, Pos1, Output, Output1),
	link(Ws, Pos1, Pos, Output1, OutputT).
link([table(Table,Size)|Ws], Pos0, Pos, Output, OutputT) ?- !,
	Pos1 is Pos0 + Size,
	Output = [table(Table,Size)|Output1],
	link(Ws, Pos1, Pos, Output1, OutputT).
link([brk_port(P,Displ)|Ws], Pos0, Pos, Output, OutputT) ?- !,
        Displ = Pos0,
        Output = [P|Output1],
        Pos1 is Pos0 + 1,
        link(Ws, Pos1, Pos, Output1, OutputT).
link([W|Ws], Pos0, Pos, Output, OutputT) ?-
	Output = [W|Output1],
	Pos1 is Pos0 + 1,
	link(Ws, Pos1, Pos, Output1, OutputT).





make_switch_table(int, H, TL, Table, [align(2),label(TL)|Ts0], Ts) :- !,
	% add alignment and label for table
	keysort(Table, SortedTable),
	insert_table(SortedTable, int, H, Ts0, Ts).
make_switch_table(range, H, TL, Table, [align(2),label(TL)|Ts0], Ts) :- !,
	% add alignment and label for table
	Table = [Min,Max|TableRest],
	keysort(TableRest, SortedTableRest),
	insert_table([Min,Max|SortedTableRest], int, H, Ts0, Ts).
make_switch_table(Type, H, TL, Table, [align(2),label(TL)|Ts0], Ts) :-
	% add alignment and label for table
	(Type == atom ; Type == functor), !,
	make_nonint_table(Table, H, Ts0, Ts).


make_nonint_table(Table, H, Ts0, Ts) ?-
	length(Table, N),
	Size is N * 2, % 2 words per entry. Size here is for *all* entries
	( foreach(Key-ref(LabIdx),Table), foreach(Key-ref(Lab),Table1), param(H) do
	    label_idx_to_var(LabIdx, H, Lab)
	),
	Ts0 = [table(Table1,Size)|Ts].

table_size(Table, Type, Size) :-
	length(Table, Size0),
	extra_table_entries(Type, Extra),
	% extra entries are not included in size calculations
	Size is Size0 - Extra.


% insert entries of switch table to the tables word list
insert_table([], _, _H, Ts, Ts).
insert_table([Key-L0|Es], Type, H, [Key,L|Ts0], Ts) :-
% ref. may already be instantiated as linking is in progress
	valid_ref(L0), 
	valid_key(Type, Key),
        L0 = ref(Lab0),
        L = ref(Lab),
        label_idx_to_var(Lab0, H, Lab),
	insert_table(Es, Type, H, Ts0, Ts).

valid_key(int, I) ?- integer(I).
% Key is an integer either because it is an integer or an address
valid_key(atom, A) ?- atom(A).
valid_key(functor, F) ?- valid_functor(F).


% the number of Padding words (as nops) that has to be inserted for alignment
% to N multiple of words in word list In, at the Lth word.
insert_nops(N, L, NewL, In, Out) :- 
     Padding is N - (((L - 1) mod N) + 1),
     NewL is Padding + L,
     instr(nop, NopCode, _), 
     (for(_,1,Padding), param(NopCode), 
      fromto(In, [o(NopCode)|Pads], Pads, Out) do true
     ).


/* label_idx_to_var(+LabelIn, +Hash, -LabelOut)
    maps LabelIn to LabelOut: if LabelIn is a integer index label, LabelOut
    is the equivalent variable label. This predicate assumes LabelIn is a 
    valid label, i.e. it should only be called after a call to valid_reflab/1
*/
label_idx_to_var(L0, H, L) :-
     ( integer(L0) ->
         ( hash_find(H, L0, L) -> true ; hash_add(H, L0, L) /*new var*/)
     ; L0 = L
     ).


/**************************************************************************
 disassemble
***************************************************************************/
%
% IMPLEMENTATION:
%
%	disasm/2 is based on the low-level builtins retrieve_code/3
%	and decode_code/2. retrieve_code retrieves the machine word
%       representation of the code for a predicate, and decode_code is
%       used to help decode the words into WAM representation
%

%disasm(+PredSpec, -ListOfInstructions, +Module) 
disasm(Pred, WAMList, Module) :-
%	is_existing_pred(Pred, Module),
	hash_create(H),
	retrieve_code(Pred, [code(Base,WordList)|_], Module),
	interpret_pred(WordList, Base, H, 0, WAMList0, InstrStarts),
	hash_list(H, _, Labels),
	sort(add of label, <, Labels, SortedLs),
	add_labels(SortedLs, InstrStarts, WAMList0, WAMList), !.
%	pretty_print(WAMList).
disasm(Pred, WAMList, Module) :-
	get_bip_error(E),
	error(E, disasm(Pred,WAMList,Module), Module).


/* interpret_pred(+WordList, +Base, +HashTable, +IStart, -WAMList, -Starts)

   generates the initial WAM code for a predicate from the memory word list.


   WordList:  current tail of the list of integers in memory representing the
              predicate
   Base:      the base address of the predicate in memory
   HashTable: hash-table used to store references to labels encountered
   WAMList:   current tail of initial WAM list
   IStart:    the offset to the start of the current WAM instruction
   Starts:    current tail of the start list, where each element represents 
              the offset in words from the base of the current WAM instruction.
              IStart is the next start position to be added to this list
*/

interpret_pred([IWord|Ws0], Base, H, IStart, WAM0, Starts0) :-
	decode_code(o(IWord), OpCode),
	(
	    instr(Instr, OpCode, Args),		% nondet
	    FirstPos is IStart + 1,		% +1 for opc 
	    disasm_args(Args, Ws0, Ws1, Base, H, FirstPos, PosEnd) % may fail
	->
	    ( Instr = code_end ->
		Starts0 = [IStart], WAM0 = [Instr]
	    ; Instr = comment(_) ->
		interpret_pred(Ws1, Base, H, PosEnd, WAM0, Starts0)
	    ;
		Starts0 = [IStart|Starts1], WAM0 = [Instr|WAM1],
		interpret_pred(Ws1, Base, H, PosEnd, WAM1, Starts1)
	    )
	;   
	    printf(error, "Unrecognised opcode (%w) or invalid instruction arguments.%n", [OpCode]),
	    set_bip_error(6)
	).


/* disasm_args(+ArgTypes, +WordListIn, -WordListOut, +Base, +Hash,
               +PosIn, -PosOut)

   disassembles the arguments of a WAM instruction:

   ArgTypes:     types of remaining args in WAM instruction
   WordListIn:  remaining memory list of values of consecutive words in memory 
                representing the predicate. The head corresponds to the (start
                of) the binary value for ArgType.
   WordListOut: remaining memory list after current instruction has been 
                disassembled.
   Base:        Base address of predicate
   Hash:        Hash table for storing references to label
   PosIn:       offset from base in words for current argument.
   PosOut:      will contain the offset at the end of current instruction

*/
disasm_args([], Ws0, Ws, _, _, Pos0, Pos) ?- Ws0 = Ws, Pos0 = Pos.
disasm_args([Arg|Args], Ws0, Ws, Base, H, Pos0, Pos) ?-
	disasm_arg(Arg, Ws0, Ws1, Base, H, Pos0, Pos1),
	disasm_args(Args, Ws1, Ws, Base, H, Pos1, Pos).

disasm_arg(a(A), [D|Ws1], Ws, _, _, Pos0, Pos) ?- 
	decode_code(a(D), A),
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(y(Y), [D|Ws1], Ws, _, _, Pos0, Pos) ?-
	decode_code(y(D), Y),
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(t(T), [D|Ws1], Ws, _, _, Pos0, Pos) ?- 
	decode_code(t(D), T),
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(pw(O), [D|Ws1], Ws, _, _, Pos0, Pos) ?- 
	decode_code(pw(D), O),
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(edesc(EDesc), [D|Ws1], Ws, _, _, Pos0, Pos) ?- 
	decode_code(edesc(D), EDesc),
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(i(I), [W|Ws1], Ws, _, _, Pos0, Pos) ?-
	W = I,  
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(f(Z), [D|Ws1], Ws, _, _, Pos0, Pos) ?-
	decode_code(float(D), Z),
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(atom(A), [D|Ws1], Ws, _, _, Pos0, Pos) ?-
	decode_code(atom(D), A),
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(s(S), [D|Ws1], Ws, _, _, Pos0, Pos) ?-
	decode_code(string(D), S),
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(ref(Lab), [D|Ws1], Ws, Base, H, Pos0, Pos) ?-
	add_label_to_hashed(D, ref(Lab), Base, H),
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(func(Pred), [D|Ws1], Ws, _, _, Pos0, Pos) ?-
	decode_code(functor(D), Pred),
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(proc(Proc), [D|Ws1], Ws, _, _, Pos0, Pos) ?-
	decode_code(proc(D), Proc),
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(vmask(VList), [First,Mask|Ws1], Ws, _, _, Pos0, Pos) ?-
	decode_code(init(First,Mask),VList),
	Ws1 = Ws, 
	Pos is Pos0 + 2.
disasm_arg(nvmask(NVList), [First,Mask|Ws1], Ws, _, _, Pos0, Pos) ?-
	decode_code(init(First,Mask),VList),
	Pos1 is Pos0 + 2,
	construct_nvlist(VList, Ws1, Ws, NVList, Pos1, Pos).
disasm_arg(nv(N), [D|Ws1], Ws, _, _, Pos0, Pos) ?-
	decode_code(nv(D), N),
	Ws1 = Ws,
	Pos is Pos0 + 1.
disasm_arg(tags(Ls), Ws0, Ws, Base, H, Pos0, Pos) ?-
	decode_code(tags, Tags),
	Tags =.. [tags|TagsL],
	Pos1 is Pos0 + 1, % start counting at 1
	(foreach(Tag, TagsL), fromto(Ls, Ls1,Ls2, []), count(_, Pos1, Pos), 
	 fromto(Ws0,[Add|Ws1],Ws1,Ws), param([Base,H]) do
             decode_code(ref(Add,Base), Mapped),
	     (integer(Mapped) ->
	         Ls1 = [Tag:TRef|Ls2],
		 add_label_to_hashed(Add, TRef, Base, H)
	     ;   % do not add external refs to Ls
	         Ls1 = Ls2
	     )
	).
disasm_arg(port(P), [D|Ws1], Ws, _, _, Pos0, Pos) ?-
	P = D,  % just use raw form for now
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(brk_port(P), [D|Ws1], Ws, _, _, Pos0, Pos) ?-
        P is D /\ port_mask,  % mask out the non-Port bits
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(valtag(C), [V,T|Ws1], Ws, _, _, Pos0, Pos) ?-
	decode_code(constant(V,T), C),
	Ws1 = Ws, 
	Pos is Pos0 + 2.
disasm_arg(tagval(C), [T,V|Ws1], Ws, _, _, Pos0, Pos) ?-
	decode_code(constant(V,T), C),
	Ws1 = Ws, 
	Pos is Pos0 + 2.
disasm_arg(mv(M), [D|Ws1], Ws, _, _, Pos0, Pos) ?- 
	decode_code(mv(D), M),
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(an(Att), [D|Ws1], Ws, _, _, Pos0, Pos) ?- 
	decode_code(pw(D), I), % raw form in # bytes -> pw offset
	meta_index(Att, I),
	Ws1 = Ws, 
	Pos is Pos0 + 1.
disasm_arg(try{table:Table,size:Size,ref:Offset}, [Address|Ws1], Ws, 
    Base, H, Pos0, Pos) ?-
	decode_code(ref(Address,Base), Offset),
	Pos is Pos0 + 1,
	Ws1 = Ws, 
	decode_code(try_table(Address,Size), Trys),
	(foreach(Try, Trys), foreach(TryRef, Table), param([Base,H]) do 
               add_label_to_hashed(Try, TryRef, Base, H)
		
	).
disasm_arg(tab{type:Type,table:Table}, [Address,Size0|Ws1], Ws, 
    Base, H, Pos0, Pos) ?-
	extra_table_entries(Type, Extra),
	Size is Size0 + Extra,  % number of actual entries to extract
	decode_code(table(Address,Size), Entries),
	Ws1 = Ws, 
	Pos is Pos0 + 2,
        interpret_switch_table(Entries, Type, Base, H, Table).
disasm_arg(o(Op), [W|Ws1], Ws, _, _, Pos0, Pos) ?- 
% `hidden' instruction
	(decode_code(o(W), Op) -> % check op-code is as expected
	    Ws1 = Ws, 
	    Pos is Pos0 + 1
        ;   printf(error, "Expected op-code %w not found.%n", [Op]), 
            fail
        ).
disasm_arg(tref(TL), [W|Ws1], Ws, Base, _, Pos0, Pos) ?-
% trefs are references to data, not added to hash table
	decode_code(ref(W,Base), Offset),
	(TL = Offset ->
	    Ws1 = Ws, 
	    Pos is Pos0 + 1
	;   printf(error, "inconsistent references to data tables.%n"),
	    fail
	).
disasm_arg(skip(N), [W|Ws1], Ws, _, _, Pos0, Pos) ?-
	% skip the next W words
	N is W,
	Pos is Pos0 + W + 1, % +1 for the skip arg itself
        skip_words(W, Ws1, Ws).


skip_words(0, Ws, Ws) :- !.
skip_words(N, [_|Ws0], Ws) :-
	N > 0, N1 is N - 1,
	skip_words(N1, Ws0, Ws).


/* add_labels(+LabelList, +StartList, +WAMIn, -WAMOut)

   takes the initial WAM list and adds labels to it

   LabelList: a sorted list of all labels found in predicate
   StartList: current tail of start positions of each WAM instruction.
              head is start for current WAM instruction
   WAMIn:     current position in the initial WAM list (where labels
              have not yet been added)
   WAMOut:    final remaining WAM list (with label inserted)
*/
add_labels([], _, WAM0, WAM) ?- WAM0 = WAM.
add_labels(Ls0, [Current|Ss], [Instr|WAM0], WAM) :-
	Ls0 = [label{add:Offset,label:N}|Ls1], 
	( Offset == Current ->
	    WAM = [label(N),Instr|WAM1], 
            Ls = Ls1
        ; Offset > Current ->
	    WAM = [Instr|WAM1], 
	    Ls = Ls0
        ; printf(error, "Label not at instruction boundary: %w%n", [Instr]),
          set_bip_error(6)
        ),
	add_labels(Ls, Ss, WAM0, WAM1).


add_label_to_hashed(Absolute, ref(LabelRef), Base, H) :-
	decode_code(ref(Absolute,Base), Mapped),
	(valid_symbol(Mapped) -> 
	    % label is an outside symbol; not added to hash
	    LabelRef = Mapped
	;integer(Mapped) -> % label is coverted to a displacement   
	    % LabelRef is left as var.
	    Label = label{add:Mapped,label:LabelRef},
	    (hash_find(H, Mapped, Label) -> 
		true   % unified with existing label
	    ;   hash_add(H, Mapped, Label)   % add new label
	    )
	).


valid_symbol(fail) ?- true.
valid_symbol(par_fail) ?- true.

split_varsnames([], [], []).
split_varsnames([V-N|NVs], [V|Vs], [nv(N)|Ns]) :-
	(integer(V) ->
	    split_varsnames(NVs, Vs, Ns)
	;   writeln(error, "Namedvars mask contains non-integers arg. positions"), 
	    set_bip_error(5)
        ).

construct_nvlist([], Ws, Ws, [], Pos, Pos).
construct_nvlist([Y|Ys], [W|Ws1], Ws, [Y-Name|NVs], Pos0, Pos) :-
	decode_code(nv(W), Name),
	Pos1 is Pos0 + 1,
	construct_nvlist(Ys, Ws1, Ws, NVs, Pos1, Pos).


interpret_switch_table([], _, _, _, []).
interpret_switch_table([Key-A|Entries], Type, Base, H, [TKey-Ref|Table]) :-
	add_label_to_hashed(A, Ref, Base, H),
	typed_key(Type, Key, TKey),
	interpret_switch_table(Entries, Type, Base, H, Table).

% the key types for switch table entries
typed_key(atom, V, A) :- decode_code(atom(V), A).
typed_key(int, V, V) :- integer(V).
typed_key(functor, V, F) :- decode_code(functor(V), F).
typed_key(range, V, V) :- integer(V).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


is_existing_pred(F/A, Module) ?-
	is_proc(F/A), !,
	((current_predicate(F/A)@Module ; current_built_in(F/A)@Module) ->
	    true ; set_bip_error(60)
	).
is_existing_pred(_, _) :-
	set_bip_error(5).

/* find_arg_in_struct(?Term, +Struct, -Pos)
   returns the position Pos in Struct for the first occurrance of Term
*/
find_arg_in_struct(Term, Struct, Pos) :-
	functor(Struct, _, Arity),
	find_arg_in_struct1(Arity, Term, Struct, Pos).

find_arg_in_struct1(N, Term, Struct, Pos) :-
	N > 0,
	arg(N, Struct, Arg),
	(Arg == Term ->
	    Pos = N
	;   N1 is N - 1,
	    find_arg_in_struct1(N1, Term, Struct, Pos)
	).


% extra_table entries specifies the number of extra entries for particular
% types of switch tables that is not included in the Size word of the
% instruction. Currently only range tables have extra entries for the
% two cases outside the range
extra_table_entries(range, Extra) ?- !, Extra = 2.
extra_table_entries(_, 0).

% Mask for brk_port(..) word to mask out the port bits. Must match
% PORT_MASK in debug.h
port_mask(16'3f).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pretty print

wam(Pred0, Module) :-
	find_pred(Pred0, Pred, Module),
	disasm(Pred, WAM, Module), !,
	printf("%w:%n", [Pred]),
	print_wam(WAM).
wam(Pred, Module) :-
	get_bip_error(E),
	error(E, wam(Pred, Module), Module).

print_wam(WAM) :-
	\+ \+ (
	    fill_label(WAM, 0),
	    pretty_print(WAM)
	).


find_pred(F/A, Pred, Module) ?- !,
	Pred = F/A,
	is_existing_pred(Pred, Module).
find_pred(F, Pred, Module) :-
	atom(F), !,
	(current_predicate(F/_)@Module ->
	    current_predicate(F/A)@Module,
	    Pred = F/A
	;   set_bip_error(60)
        ).
find_pred(_, _, _) :-
	set_bip_error(5).

fill_label([], _).
fill_label([WAM|WAMs], N) :-
	(WAM = label(L), var(L) ->
	    concat_atom(['L',N], L),
	    N1 is N + 1
	; N1 = N
        ),
	fill_label(WAMs, N1).


pretty_print([]).
pretty_print([label(N)|Is]) ?- !,
	printf("%Vw", [label(N)]), writeln(":"),
	pretty_print(Is).
pretty_print([I|Is]) :-
	( instr(I, _, ArgTypes) ->
	    pretty_print_instr(I, ArgTypes),
	    pretty_print(Is)
	;
	    printf(error, "Unrecognised instruction %w.%n", [I]),
	    abort
	).

pretty_print_instr(I, ArgTypes) :-
	I =.. [Name|Args],
	printf("\t%-20s ",[Name]), 
	pretty_print_args(Args, ArgTypes).

pretty_print_args([], []) :-  nl.
pretty_print_args([Table,ref(E),ref(D)|As], 
                  [tab{type:range,table:Table},ref(E),ref(D)|ATs]) :-
	!,
	nl,
        print_rangetable(Table, E, D),
	pretty_print_args(As, ATs).
pretty_print_args([Table,ref(D)|As], [tab{table:Table},ref(D)|ATs]) :-
	!, 
        nl,
        (foreach(Key-Ref, Table) do
             printf("\t\t%QDVw: \t%w%n", [Key,Ref])
	),
	printf("\t\tdefault: \t%w%n", [ref(D)]),
	pretty_print_args(As, ATs).
pretty_print_args([ref(L)|As], [ref(L)|ATs]) :- !,
	printf("    %DVw ", [ref(L)]),
        pretty_print_args(As, ATs).
pretty_print_args([N|As], [edesc(N)|ATs]) :- !,
	( N = eam(EAM) ->
	    integer_bits(EAM, Ys),
	    printf("    Y%DKw ", [Ys])
	; N = [_|_] ->
	    printf("    Y%DKw ", [N])
	;
	    printf("    %DVw ", [N])
	),
        pretty_print_args(As, ATs).
pretty_print_args([Ls|As], [tags(Ls)|ATs]) :-
	!,
	nl,
        (foreach(Tag:Ref, Ls) do
             printf("\t\t%QDVw: \t%w%n", [Tag,Ref])
	),
	pretty_print_args(As, ATs).
pretty_print_args([A|As], [_|ATs]) :-
	printf("    %QDVw ", [A]),
        pretty_print_args(As, ATs).


integer_bits(N, Bits) :-
	(
	    fromto(N,N1,N2,0),
	    count(I,1,_),
	    fromto(Bits,Bits1,Bits2,[])
	do
	    N2 is N1 >> 1,
	    ( getbit(N1,0,1) -> Bits1 = [I|Bits2] ; Bits1 = Bits2 )
	).


print_rangetable([Min-MinRef,Max-MaxRef|Sws], E, D) :-
	printf("\t\tdefault:\t%w%n", [D]),
	printf("\t\t< % 11d:\t%w%n", [Min,MinRef]),
	printf("\t\t> % 11d:\t%w%n", [Max,MaxRef]),
        print_rangetable1(Sws),
	printf("\t\telse:\t\t%w%n", [E]).

print_rangetable1([]).
print_rangetable1([N-Ref|Sws]) :-
	printf("\t\t  % 11d:\t%w%n", [N,Ref]),
        print_rangetable1(Sws).





