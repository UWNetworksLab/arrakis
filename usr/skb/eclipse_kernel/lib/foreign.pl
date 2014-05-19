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
% Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: foreign.pl,v 1.1 2008/06/30 17:43:46 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 * IDENTIFICATION:	foreign.pl 
 *
 * sccsid("%W%          %E%").
 */

:- module(foreign).

:- comment(summary, "Simple foreign interface like SICStus or Quintus").
:- comment(author, "Micha Meier, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:46 $").
:- comment(desc, ascii("
Simple foreign interface like SICStus or Quintus(TM). For every C function
an interface C function is generated which converts the arguments
from and to the Sepia ones. Note that this is faster (and uses more
C code) than one generic interface procedure which dispatches all
external calls like in the above systems. Use it as follows:

1) load this file with lib(foreign)
2) compile (Sepia) the file with the foreign/3 clauses
3) call make_simple_interface/0 or /1; this will generate the file
   e_interface.c
4) compile (cc) e_interface.c and the other C source files
      On a SVR4 system (e.g. Solaris 2.x) produce one .so file
      from all the .o files that should be loaded, including
      the file e_interface.o. On Solaris this can be done with
      something like
	cc -I$ECLIPSEDIR/include -G -o myfile.so file1.c file2.c \
		... e_interface.c
5) call load_foreign_files/2. This assumes that the foreign/3 clauses
   are still available.

This means that a file which contains both foreign/3 facts and
a goal :- load_foreign_files(...) must be split so that
make_simple_interface/0 can be called before the goal.
However, once the file e_interface.c is created, this file can be used
in the original form.

The main purpose of the interface is to port foreign files from
SICStus or Quintus to Sepia, or to automate interfacing of
other existing C functions. When writing new external functions,
it is more efficient to use the Sepia external interface directly.


User-defined types and conversion can also be specified.
If some additional declarations and function calls are necessary
before or after calling the C function, the type can be specified
as a user type:
	[+-]user(Type)

When such a type is encountered, this library will call the predicate

foreign_user_type(+Type, +I, -CType, -Local, -TypeCheck, -InConv,
	-Call, -OutConv, -Unif)

which is supposed to yield the data to be inserted into the
source of the interface file. The arguments are as follows:
Type	- the name of the user type with + or -, e.g. +mystring
I	- the argument position; used to make distinction
	  among several args of the same type
CType	- the corresponding C type
Local	- declaration of local variables for this type
TypeCheck - type testing of the predicate arguments
InConv	- calls to be made before calling the foreign function
Call	- the actual argument passed to the foreign function
OutConv	- calls to be made after the foreign call and before
	  the unification of output arguments
Unif	- unification of the output argument

The source of the generated interface for a declaration
foreign(funct, c, funct(+integer, +user(ut)))
looks as follows:

p_funct(val1, tag1, val2, tag2)
value	val1, val2;
type	tag1, tag2;
{
	LOCAL

	Check_Integer(tag1);
	TYPECHECK
	INCONV
	funct(val1.nint, CALL)
	OUTCONV
	Return_UNIF
}
where the capitalized ids are substituted by the user definition.
")).

% NOTE: Sepia strings are not (yet) supported by this interface
% (keyword sepia_string), but their inclusion is trivial.

:- dynamic
	foreign/3,		% foreign/2 not supported
	foreign_file/2.		% ignored

:- export
	declare_externals/1,
	declare_externals/2.

:- export tr_foreign/3.
:- export macro(foreign/3, tr_foreign/3, [clause]).

:- export
	load_foreign_files/2,
	make_simple_interface/0,
	make_simple_interface/1.

:- import
	external_/3,
	external_body/2
    from sepia_kernel.

:- tool(load_foreign_files/2, load_foreign_files/3).
:- tool(make_simple_interface/0, make_simple_interface_body/1).
:- tool(make_simple_interface/1, make_simple_interface/2).
:- tool(declare_externals/1, declare_externals/2).

:- op(300, yfx, '.').

tr_foreign(E, E, M) :-
	E = foreign(_, _, P),
	functor(P, F, A),
	external_body(F/A, M).

make_simple_interface_body(M) :-
	make_simple_interface([], M).

make_simple_interface(Incl, M) :-
	open('e_interface.c', write, c),
	write(c, "/* This file has been automatically generated\n   by make_simple_interface/1\n*/\n\n"),
	write(c, '#include	"external.h"'),
	write_incls(Incl),
	printf(c, "\n\nextern char *\tmalloc();\n\n", []),
	call(foreign(CName, _, Proc))@M,
	(make_interface(CName, Proc, M) ->
	    fail
	;
	    printf(error, "*** The function %s failed to interface!\n%b", CName),
	    fail
	).
make_simple_interface(_, _) :-
	close(c).

write_incls([]).
write_incls([I|Incl]) :-
	printf(c, '%n#include	"%s"', [I]),
	write_incls(Incl).

load_foreign_files(Files, Libraries, M) :-
	(Files = :(Module, List) ->
		true
	;
		Module = M,
		List = Files
	),
	(get_flag(object_suffix, "o") ->
	    list_to_string(Libraries, "", LS),
	    list_to_string(List, "", FS),
	    concat_string([FS, " e_interface.o ", LS], FilesString),
	    load(FilesString)
	;
	    List = [File],
	    load(File)
	),
	declare_externals(Module, M).

list_to_string([], S, S).
list_to_string([L|Libs], S, String) :-
	concat_string([S, " ", L], S1),
	list_to_string(Libs, S1, String).

declare_externals(Mod, M) :-
	call(foreign(CName, _, Proc))@M,
	concat_atoms(p_, CName, P_Name),
	functor(Proc, F, A),
	external_(F/A, P_Name, Mod),
	fail.
declare_externals(_, _).

make_interface(CName, Proc, M) :-
	concat_atoms(p_, CName, P_Name),
	Proc =.. [_|Args],
	proc_type(Args, CArity, PArity, ProcType, OutArg, M),
	printf(c, "%s(", P_Name),
	write_args(1, PArity),
	printf(c, ")\n", []),
	write_declaration(PArity),
	printf(c, "{\n", []),
	local_variables(1, Args, 0, Count, M),
	write_checks(1, Args, M),
	input_conversion(1, Args, M),
	(OutArg > 0 ->
		printf(c, "\tout%w = ", [OutArg])
	;
		write(c, '\t')
	),
	printf(c, "(%s) %s(", [ProcType, CName]),
	write_call(1, CArity, Args, M),
	printf(c, ");\n", []),
	output_conversion(1, Args, M),
	write_unification(1, Args, Count, M),
	(Count > 1 ->
	    printf(c, "\tReturn_Unify;\n", [])
	;
	Count = 0 ->
	    printf(c, "\tSucceed;\n", [])
	;
	    true
	),
	printf(c, "}\n\n", []).

write_args(I, Arity) :-
	I > Arity,
	!.
write_args(Arity, Arity) :-
	!,
	printf(c, "val%d, tag%d", [Arity, Arity]).
write_args(I, Arity) :-
	printf(c, "val%d, tag%d, ", [I, I]),
	I1 is I + 1,
	write_args(I1, Arity).

write_declaration(Arity) :-
	write_values(Arity),
	write_types(Arity).

write_values(0) :- !.
write_values(Arity) :-
	printf(c, "value\tval1", []),
	write_ids(val, 2, Arity),
	printf(c, ";\n", []).

write_types(0) :- !.
write_types(Arity) :-
	printf(c, "type\ttag1", []),
	write_ids(tag, 2, Arity),
	printf(c, ";\n", []).

write_ids(_, I, Arity) :-
	I > Arity,
	!.
write_ids(Id, I, Arity) :-
	printf(c, ", %s%d", [Id, I]),
	I1 is I + 1,
	write_ids(Id, I1, Arity).

local_variables(_, [], C, C, _) :-
	(C > 1 -> printf(c, "\tPrepare_Requests;\n", []) ; true),
	nl(c).
local_variables(I, [-user(Type)|Args], C, Count, M) :-
	!,
	call(foreign_user_type(-Type, I, _, Local, _, _, _, _, _))@M,
	printf(c, "\t%s\n", [Local]),
	I1 is I + 1,
	local_variables(I1, Args, C, Count, M).
local_variables(I, [OArg|Args], C, Count, M) :-
	(OArg = [-Arg] ->
	    true
	;
	OArg = -Arg ->
	    true
	),
	arg_type(Arg, Type, M),
	!,
	printf(c, "\t%s\tout%d;\n", [Type, I]),
	(Arg = string(_) -> printf(c, "\tchar *\tstr%d;\n", [I]); true),
	(Arg = user(T) ->
		call(foreign_user_type(-T, I, _, Local, _, _, _, _, _))@M,
		printf(c, "\t%s\n", [Local])
	;
		true
	),
	I1 is I + 1,
	C1 is C + 1,
	local_variables(I1, Args, C1, Count, M).
local_variables(I, [+string(_)|Args], C, Count, M) :-
	!,
	printf(c, "\tchar *\tstr%d;\n", I),
	I1 is I + 1,
	local_variables(I1, Args, C, Count, M).
local_variables(I, [+user(Type)|Args], C, Count, M) :-
	!,
	call(foreign_user_type(+Type, I, _, Local, _, _, _, _, _))@M,
	printf(c, "\t%s\n", [Local]),
	I1 is I + 1,
	local_variables(I1, Args, C, Count, M).
local_variables(I, [_|Args], C, Count, M) :-
	I1 is I + 1,
	local_variables(I1, Args, C, Count, M).

input_conversion(_, [], _).
input_conversion(I, [+string(N)|Args], M) :-
	!,
	printf(c, "\t{\n\t\tregister char *\tcp;\n\t\tstr%d = malloc(%d + 1);\n", [I, N]),
	printf(c, "\t\tstrncpy(str%d, DidName(val%d.did), %d);\n", [I, I, N]),
	printf(c, "\t\tcp = str%d + %d;\n\t\twhile(!*cp && cp > str%d)\n", [I, N, I]),
	printf(c, "\t\t\t*cp-- = ' ';\n\t}\n", []),
	I1 is I + 1,
	input_conversion(I1, Args, M).
input_conversion(I, [-string(N)|Args], M) :-
	!,
	printf(c, "\tstr%d = malloc(%d + 1);\n", [I, N]),
	I1 is I + 1,
	input_conversion(I1, Args, M).
input_conversion(I, [[-string(N)]|Args], M) :-
	!,
	printf(c, "\tstr%d = malloc(%d + 1);\n", [I, N]),
	I1 is I + 1,
	input_conversion(I1, Args, M).
input_conversion(I, [+user(Type)|Args], M) :-
	!,
	call(foreign_user_type(+Type, I, _, _, _, InConv, _, _, _))@M,
	printf(c, "\t%s\n", [InConv]),
	I1 is I + 1,
	input_conversion(I1, Args, M).
input_conversion(I, [-user(Type)|Args], M) :-
	!,
	call(foreign_user_type(-Type, I, _, _, _, InConv, _, _, _))@M,
	printf(c, "\t%s\n", [InConv]),
	I1 is I + 1,
	input_conversion(I1, Args, M).
input_conversion(I, [_|Args], M) :-
	I1 is I + 1,
	input_conversion(I1, Args, M).

proc_type(Args, CArity, PArity, Type, Output, M) :-
	proc_params(Args, 0, PArity, Type, Output, M),
	(Output = 0 ->
	    CArity = PArity
	;
	    CArity is PArity - 1
	).
	
proc_params([], I, I, void, 0, _).
proc_params([[-Arg]|Args], I, Arity, Type, I1, M) :-
	arg_type(Arg, Type, M),
	!,
	I1 is I + 1,
	proc_params(Args, I1, Arity, _, _, M).
proc_params([_|Args], I, Arity, Type, Output, M) :-
	I1 is I + 1,
	proc_params(Args, I1, Arity, Type, Output, M).

write_checks(_, [], _).
write_checks(I, [OArg|Args], M) :-
	(OArg = [Arg] -> true; Arg = OArg),
	arg_check(Arg, Check, M),
	!,
	(Check = 'Check_Atom_Or_Nil' ->
	    printf(c, "\t%s(val%d, tag%d);\n", [Check, I, I])
	;
	Check = '' ->
	    true
	;
	    printf(c, "\t%s(tag%d);\n", [Check, I])
	),
	I1 is I + 1,
	write_checks(I1, Args, M).

write_call(_, _, [], _).
write_call(I, Arity, [[_]|Args], M) :-
	!,
	I1 is I + 1,
	write_call(I1, Arity, Args, M).
write_call(I, Arity, [Arg|Args], M) :-
	call_arg(I, Arg, M),
	(I < Arity -> write(c, ', '); true),
	I1 is I + 1,
	write_call(I1, Arity, Args, M).

call_arg(I, +string(_), _) :-
	!,
	printf(c, "str%d", [I]).
call_arg(I, -string(_), _) :-
	!,
	printf(c, "&str%d", [I]).
call_arg(I, +user(Type), M) :-
	!,
	call(foreign_user_type(+Type, I, _, _, _, _, Call, _, _))@M,
	printf(c, "%s", [Call]).
call_arg(I, -user(Type), M) :-
	!,
	call(foreign_user_type(-Type, I, _, _, _, _, Call, _, _))@M,
	printf(c, "%s", [Call]).
call_arg(I, +Arg, M) :-
	!,
	concat_atom([val, I], Val),
	concat_atom([tag, I], Tag),
	arg_access(Arg, Val, Tag, Access, M),
	printf(c, "%.w", [Access]).
call_arg(I, -_, _) :-
	!,
	printf(c, "&out%d", I).

write_unification(_, [], _, _).
write_unification(I, [Arg|Args], C, M) :-
	out_string(Arg),
	!,
	unif_type(C, Type),
	printf(c, "\t%s_Unify_Atom(val%d, tag%d, in_dict(out%d, 0));\n", [Type, I, I, I]),
	I1 is I + 1,
	write_unification(I1, Args, C, M).
write_unification(I, [Arg|Args], C, M) :-
	out_buffer(Arg),
	!,
	unif_type(C, Type),
	printf(c, "\t%s_Unify_Atom(val%d, tag%d, in_dict(str%d, 0));\n", [Type, I, I, I]),
	I1 is I + 1,
	write_unification(I1, Args, C, M).
/*
write_unification(I, [Arg|Args], C, M) :-
	out_sepia_string(Arg),
	!,
	unif_type(C, Type),
	printf(c, "\t%s_Unify_String(val%d, tag%d, str%d);\n", [Type, I, I, I]),
	I1 is I + 1,
	write_unification(I1, Args, C, M).
*/
write_unification(I, [-user(T)|Args], C, M) :-
	!,
	unif_type(C, Type),
	call(foreign_user_type(-T, I, _, _, _, _, _, _, Unif))@M,
	printf(c, "\t%s_%s\n", [Type, Unif]),
	I1 is I + 1,
	write_unification(I1, Args, C, M).
write_unification(I, [-Arg|Args], C, M) :-
	!,
	unif_type(C, Type),
	arg_unif(Arg, Unif, Cast, M),
	printf(c, "\t%s_Unify_%s(val%d, tag%d, %sout%d);\n",
		[Type, Unif, I, I, Cast, I]),
	I1 is I + 1,
	write_unification(I1, Args, C, M).
write_unification(I, [[-Arg]|Args], C, M) :-
	!,
	unif_type(C, Type),
	arg_unif(Arg, Unif, Cast, M),
	printf(c, "\t%s_Unify_%s(val%d, tag%d, %sout%d);\n",
		[Type, Unif, I, I, Cast, I]),
	I1 is I + 1,
	write_unification(I1, Args, C, M).
write_unification(I, [_|Args], C, M) :-
	I1 is I + 1,
	write_unification(I1, Args, C, M).

unif_type(1, 'Return') :- !.
unif_type(_, 'Request').

output_conversion(_, [], _).
output_conversion(I, [[-string(N)]|Args], M) :-
	!,
	printf(c, "\tstrncpy(str%d, out%d, %d + 1);\n", [I, I, N]),
	I1 is I + 1,
	output_conversion(I1, Args, M).
output_conversion(I, [-user(T)|Args], M) :-
	!,
	call(foreign_user_type(-T, I, _, _, _, _, _, OutConv, _))@M,
	printf(c, "\t%s\n", [OutConv]),
	I1 is I + 1,
	output_conversion(I1, Args, M).
output_conversion(I, [_|Args], M) :-
	I1 is I + 1,
	output_conversion(I1, Args, M).

arg_type(integer,	'long	', _).
arg_type(float,	double, _).
arg_type(atom,		'unsigned long', _).
arg_type(string,	'char *', _).
arg_type(string(_),	'char *', _).
arg_type(address,	'char *', _).
arg_type(address(Type),	T, _) :-
	concat_atoms(Type, ' *', T).
arg_type(user(Type), T, M) :-
	call(foreign_user_type(+Type, 0, T, _, _, _, _, _, _))@M.

arg_check(+integer, 'Check_Integer', _).
arg_check(-integer, 'Check_Output_Integer', _).
arg_check(+float, 'Check_Float', _).
arg_check(-float, 'Check_Output_Float', _).
arg_check(+atom, 'Check_Atom_Or_Nil', _).
arg_check(-atom, 'Check_Output_Atom', _).
arg_check(+string, 'Check_Atom_Or_Nil', _).
arg_check(-string, 'Check_Output_Atom', _).
arg_check(+string(_), 'Check_Atom', _).
arg_check(-string(_), 'Check_Output_Atom', _).
arg_check(+address, 'Check_Integer', _).
arg_check(-address, 'Check_Output_Integer', _).
arg_check(+address(_), 'Check_Integer', _).
arg_check(-address(_), 'Check_Output_Integer', _).
arg_check(+user(T), Check, M) :-
	call(foreign_user_type(+T, 0, _, _, Check, _, _, _, _))@M.
arg_check(-user(T), Check, M) :-
	call(foreign_user_type(-T, 0, _, _, Check, _, _, _, _))@M.

arg_access(integer, Val, _, Val.nint, _).
arg_access(float, Val, Tag, 'FloatVal'(Val,Tag), _).
arg_access(atom, Val, _, Val.did, _).
arg_access(sepia_string, Val, _, 'StringStart'(Val), _).
arg_access(string, Val, _, 'DidName'(Val.did), _).
arg_access(string(_), _, _, _, _).
arg_access(address, Val, _, Val.str, _).
arg_access(address(T), Val, _, F.did, _) :-
	functor(F, Val, 1),
	arg(1, F, A),
	functor(A, T, 1),
	arg(1, A, *),
	op(50, xf, Val),
	op(50, fx, T).

arg_unif(integer, 'Integer', '', _).
arg_unif(float, 'Float', '', _).
arg_unif(atom, 'Atom', '', _).
arg_unif(string, 'Atom', '', _).
arg_unif(sepia_string, 'String', '', _).
arg_unif(address, 'Integer', '(unsigned long) ', _).
arg_unif(address(_), 'Integer', '(unsigned long) ', _).

/*
out_sepia_string(-sepia_string).
out_sepia_string([-sepia_string]).
out_sepia_string(-sepia_string(_)).
out_sepia_string([-sepia_string(_)]).
*/

out_string(-string).
out_string([-string]).

out_buffer(-string(_)).
out_buffer([-string(_)]).

