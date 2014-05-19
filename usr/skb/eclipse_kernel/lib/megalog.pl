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
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: megalog.pl,v 1.2 2008/08/20 22:57:33 jschimpf Exp $
% ----------------------------------------------------------------------


/*
**      File   : megalog.pl
**      Author : Michael Dahmen
**      Content: MegaLog compatibility package for Sepia
**
**	This package provides compatibility to MegaLog in Sepia.
**	It is intended that a user who moves his/her programs from
**	MegaLog to Sepia will use this package, if (s)he wants to
**	prevent changes in his/her sources. Please note that only
**	a partial compatibility is achieved, however the missing parts
**	are really minor. Look for %% to find predicates with incomplete
**	compatibility. Also note that at some points (e.g. retract/1)
**	compatibility effectively means reduced functionality.
**	We therefore recommend to remove those parts of this file
**	that are unwanted.
**
**	If the user wants to ignore the module system of Sepia it
**	is sufficient to load this file, change to module megalog,
**	and never leave that module. If the module system is used
**	one must take care of tool predicates and visibility of
**	predicates.
*/

:- module(megalog).

/*
		--------------------------------
		| Import the MegaLog Libraries |
		--------------------------------
*/

:- import database_kernel.

:- (use_module(library(kb)) -> true).	% ptags...

:- use_module(library(oldio)).

:- import knowledge_base.

/* simpler syntax for predicate overwriting (ala common-lisp) */
/* This syntax is removed at the end of this file */

:- set_flag(macro_expansion, on).
:- op(800,xfx,::).

transform_module_qualifier(Module::Goal,call_explicit(Goal,Module)).
:- define_macro((::)/2,transform_module_qualifier/2,[]).

%
% Megalog operators, called here before op/3 is redefined.
%
:- op(950, fx, foreach).
:- op(955, xfx, do).
:- op(1000, fy, (ls)).
:- op(0, xfx, from).

:- local
	op/3,
	compound/1.


/*
	----------------------------------------
	| Section 9 -- MegaLog Window Debugger |
	----------------------------------------
*/

/*
There are several differences in the predicate interface to the
debugger (e.g. nospy, debug_parameter). Also the functionality
of the debuggers are different. Some of the very useful MegaLog
functionalities (e.g. source display, zoom) are only available via
KEGI or Opium.
*/


/*
		------------------------------
		| Section 13.1 -- Arithmetic |
		------------------------------
*/

/*
MegaLog : X is Expression  
Sepia   : same

Note 	: %% hyperbolic radial functions are missing in Sepia because
	  of limited portability
	  %% X is cputime refers to absolute value in Sepia (delta in MegaLog)
*/
	
/*
MegaLog : extra random/3, different name
*/

random(Min,Max,Value) :-
	Min > Max,
	!,
	error(6,random(Min,Max,Value)). % error 6 = out_of_range
random(Min,Max,Value) :- 
	random(R),
	Value is (R mod (Max - Min + 1)) + Min.
	
srandom(Seed) :- seed(Seed).

/*
		-------------------------------------
		| Section 13.1 -- Internal Database |
		-------------------------------------
*/

/*
MegaLog : abolish, dynamic -- takes bracket list as argument [a,b,c]
	  retract -- succeed only once
Sepia   : abolish, dynamic -- takes comma list as argument    a,b,c
	  retract -- succeed many times

Note    : The interpretation of :- dynamic f/1, assert(f(1))
	  is different. In MegaLog the first is a declaration, the
	  second a goal. In Sepia it is seen as a single declaration
	  which then generates an error.
*/

abolish [] :- !.
abolish [H|T] :- !, abolish H, abolish T.
abolish P  :- is_predicate(P), !, sepia_kernel::abolish(P).
abolish _.	% succeed if it doesn't exist

clauses(Name/Arity,Clauses) :-
	length(List,Arity),
	Head =.. [Name|List],
	findall((Head :- Body),clause((Head :- Body)),Clauses).

dynamic [] :- !.
dynamic [H|T] :- !, sepia_kernel::dynamic(H), dynamic T.
dynamic P  :- sepia_kernel::dynamic(P).

listing [] :- !.
listing [H|T] :- !, sepia_kernel::listing(H), listing T.
listing P  :- sepia_kernel::listing(P).


ls X :- listing X.

retract(Clause) :- sepia_kernel::retract(Clause),!.
% Note    : This compatibility is probably unwanted

/*
MegaLog : If assert, retract etc. is applied to a compiled procedure
	  the procedure is abolished and an empty dynamic procedure
	  is created
Sepia   : Error 63 is raised in such a situation

MegaLog : If a procedure is compiled the asserted clauses are removed
Sepia	: Asserted and compiled clauses coexist

Note    : The error handler defined below solves some of these problems
	  but not all. We recomment to review your program wrt. use
	  of asserted clauses.
	  %%
*/

error_handler_63(63,Culprit) :-
	sepia_kernel::compound(Culprit),
	Culprit =.. [Functor,Clause],
	member(Functor,[assert, asserta, assertz, retract]),
	(Clause = (Head :- _) ; Clause = Head),
	functor(Head,F,Arity),
	((sepia_kernel::is_predicate(F/Arity), abolish(F/Arity));true),
	dynamic(F/Arity),
	!,
	call(Culprit).
	
error_handler_63(63,retract_all(Head)) :-
	Head =.. [F|Args],
	length(Args,Arity),
	abolish(F/Arity),
	dynamic(F/Arity),
	!.
error_handler_63(63,Culprit) :-
	sepia_kernel::error_handler(63,Culprit).
	
:- sepia_kernel::set_error_handler(63,error_handler_63/2).


/*
		--------------------------
		| Section 13.3 -- Arrays |
		--------------------------
*/

/*
MegaLog : Arity two
Sepia   : Arity three
*/

is_array(Name,Arity) :-
	functor(Array, Name, Arity),
	current_array(Array, _).

/*
MegaLog : erase_array/1 succeeds silently if array does not exist
Sepia   : raise error 41

Note    : This has a global effect in all modules
%%
*/

error_handler_41(41,erase_array(_),_) :- 
	!.
error_handler_41(41,getval(_,_),_) :-
	% This clause achieves that non existing global variables
	% are initialized with an unbound variable as value
	% such an error is defined by EKS
	!.
error_handler_41(41,Goal,Module) :- 
	sepia_kernel::undef_array_handler(41,Goal,Module).  

:- sepia_kernel::set_error_handler(41,error_handler_41/3).

/*
MegaLog : make_array/1 /2 succeeds silently if array already exists
	  exists with same boundaries
	  error and failure if array exists with different boundaries
Sepia   : raise warning and succeed in any case
%%
*/


/*

		----------------------------------
		| Section 13.4 -- Error Handling |
		----------------------------------
*/

/*
MegaLog : return highest valid error code
%% The error codes are completely different.
%% Any usage of set_error_handler/2 in user program need a revision.
%% The condition under which builtins raise errors are different.
*/

max_error(340). 	% 340 is first user defined error

set_error_handler(Code,Goal) :-
	writeln("Warning : Error Code and Handler are different in MegaLog"),
        writeln("Handler not installed"), 
        writeln(set_error_handler(Code,Goal)).
	
/*
		--------------------------
		| Section 13.5 -- Blocks |
		--------------------------
*/

/*
MegaLog : block/3 is deterministic
Sepia   : block/3 backtracks
*/

block(Goal,Tag,Recover) :- sepia_kernel::block((Goal,!),Tag,Recover).

/*
MegaLog : unwind protect operations (defined in pred0.pl)
*/

/* <event>_protect(Goal,Action)
**
** will protect the Action in the context of <event> i.e. Action is
** executed when Goal is left with <event>
** Action may be any Prolog goal, however there will be no backtracking
** back into Action later on. There will also be no backtracking into
** <event>_protect.
**
** <event> is one of
**        exit
**        exit_fail
**        exit_success
**        exit_fail_success
** and selects against which events there is a protection (one of).
** Protection against exit refers to the use of exit_block/1 in
** context of block/3
**
** %% Note that protection against cut and cut_fail is not supported 
** in Sepia. This feature was marked non-portable in the MegaLog
** manual.
*/

exit_protect(Goal,Action) :-
        block(Goal,Tag,(once_not_fail(Action),exit_block(Tag))).
 
exit_fail_protect(Goal,Action) :-
        block(Goal,Tag,(once_not_fail(Action),exit_block(Tag))),
        !.
exit_fail_protect(_Goal,Action) :-
        once(Action),
        fail.
 
exit_success_protect(Goal,Action) :-
        block(Goal,Tag,(once_not_fail(Action),exit_block(Tag))),
        once_not_fail(Action).
 
exit_fail_success_protect(Goal,Action) :-
        block(Goal,Tag,(once_not_fail(Action),exit_block(Tag))),
        once_not_fail(Action),
        !.
exit_fail_success_protect(_Goal,Action) :-
        once(Action),
        fail.
 

/*
		-----------------------
		| Section 13.6 -- I/O |
		-----------------------
*/

/*
** %% Handling of cyclic terms is different
** MegaLog : cyclic terms are detected and printed with <-> #n#
** Sepia   : default printing is with depth limitation
*/

/*
** Argument order is different
** MegaLog : Data, Stream
** Sepia   : Stream, Data
**
** Note    : This is solved by an 'lib(oldio), import(oldio)' *before*
**           calling 'lib(megalog), import(megalog)'
**	     The two warnings about name clashes on get/2 / get0/2 can be
**	     ignored, they only reflect that these two predicates are
**           redefined for MegaLog compatibility.
*/

/*
** %% Flush handling is different
** MegaLog : flushs after each term output
** Sepia   : extra flush predicate, flush before terminal input, flush at
**	     top level
** Note    : in effect there is no real difference for 'normal' users
*/

/*
MegaLog : get ignores blanks, get0 takes all, skip over input
Sepia   : get takes all
*/

get(X)    :- repeat,sepia_kernel::get(X),X > 32,!.
get(X,S)  :- repeat,sepia_kernel::get(S,X),X > 32,!. 
% since get/1 /2 is originally Prolog defined this definition must be
% compiled before any client code is compiled

get0(X)   :- sepia_kernel::get(X).
get0(X,S) :- sepia_kernel::get(S,X).
skip(C)   :- repeat,sepia_kernel::get_char(C),!.
skip(C,S) :- repeat,sepia_kernel::get_char(S,C),!.

/*
MegaLog : buffered I/O level 	(stream_id = 'C' pointer)
Sepia   : raw I/O level  	(stream_id = 0,1,2,...)

Note    : get_stream/2 is identical
*/

%% set_io(A,B).		% difficult to achieve, questionable wether used

/*
MegaLog : accept string as second argument
Sepia   : must be an atom
*/

open(File,Mode,Stream) :-
	string(Mode),
	!,
	atom_string(ModeA,Mode),
	open(File,ModeA,Stream).
open(File,Mode,Stream) :-
	sepia_kernel::open(File,Mode,Stream).
	

/*
MegaLog : exists in arity two and three
Sepia   : arity three only
*/

readvar(Term,VarList) :- sepia_kernel::readvar(input,Term,VarList).

/*
MegaLog : used in PCE connection only (no flush), not in manual
*/

%% writeq_nf(Term).
%% writeq_nf(Term,Stream).

/*
MegaLog : allows to replace variable names during output
	  while all others are written in a read back style
Note    : questionable, one feature is pretty-print like, one is read
	  back (i.e. internal use) like
*/

%% writeqvar(Term,VariableList).
%% writeqvar(Term,VariableList,Stream).

/*
		---------------------------
		| Section 13.7 -- Control |
		---------------------------
*/

abort :- exit_block(abort).

once_not_fail(Goal) :- once(Goal),!.
once_not_fail(_).

/*
MegaLog : debugger related, mentioned in manual therefore may be used
*/

object_call(Goal) :- call(Goal).
%% call_with_cutpt(Goal,CutPt).		% not needed, not in manual
%% object_call_with_cutpt(Goal,CutPt).	% not needed, not in manual

/*
		--------------------------------
		| Section 13.8 -- Object Files |
		--------------------------------
*/

/*
MegaLog : capability to generate and load object files
Sepia   : use either saved states or Prolog term files 
Note    : program development environment only
*/

silent_compile(F) :- compile(F).

%% clear_directives.
%% load_object(File).
%% save_object(Object,Source).

/*
MegaLog : capability to compile on a clause base, compare and execute
	  these clauses, remove clauses compiled

Note    : not needed if no compiled code in external KB,
	  code in ml_ecc.pl / ml_bang_builtins.pl is modified accordingly
	  not in manual
*/

%% '$execute'(Code,Argument).
%% '$execute_with_cutpt'(Code,Argument,CutPt).
%% compile_clause(Clause,Code).
%% emulator_register(register,Value).
%% eq_clauses(Code1, Code2).
%% install_proc(Name,Arity,Code).
%% remove_clause(Code).

/*
		------------------------------------------------
		| Section 13.9, 13.10, 13.11 -- Knowledge Base |
		------------------------------------------------
*/

/* This is copied completely from MegaLog into Sepia-MegaLog integration */


/*
		----------------------------------
		| Section 13.12 -- All Solutions |
		----------------------------------
*/

/* Identical !!! ??? */

/*
		---------------------------------------
		| Section 13.13 -- String & Coversion |
		---------------------------------------
*/

/*
MegaLog : accept atoms, string, integers, reals
	  result is string, atom given fails
*/

concat(A,B,C) :- atom(A), !, atom_string(A,A1), concat(A1,B,C).
concat(A,B,C) :- atom(B), !, atom_string(B,B1), concat(A,B1,C).
concat(A,B,C) :- number(A), !, term_string(A,A1), concat(A1,B,C).
concat(A,B,C) :- number(B), !, term_string(B,B1), concat(A,B1,C).
concat(A,B,C) :- sepia_kernel::concat_strings(A,B,C).

/*
MegaLog : only different name, rsp. differnt arity
*/

strlength(String,Length) :- string_length(String,Length).
substring(String1,String2) :- substring(String1,String2,_Position).

/*
MegaLog : Substring search for upper/lower case are considered equal

Note    : Defined in ml_builtins.c, global in Sepia-MegaLog integration
	  in manual, however only used for demo (libdb) by Michel Kuntz)

	  substring_ignore_case(String1,String2).
	  substring_ignore_case(String1,String2,Position).
*/

:- external(substring_ignore_case/2, 'SBIsubstring_ignore_case2').
:- external(substring_ignore_case/3, 'SBIsubstring_ignore_case3').

/*
MegaLog : allow to use a variable list during conversion (both ways)

Note    : Requires MegaLog parser/printer and can therefore not be ported
	  to Sepia.
*/

%% term_string(Term,String,VariableList).

/*
		--------------------------------
		| Section 13.14 -- Environment |
		--------------------------------
*/

/*
MegaLog : access to a number of internal variable (database & Prolog)

Note    : Defined in ml_builtins.c, global in Sepia-MegaLog integration
	  mentioned in manual
	  Prolog machine related parameter will raise a warning in 
	  Sepia-MegaLog integration
	  
	  prolog_parameter(Name,Value).
*/

:- external(prolog_parameter/2, 'SBIprolog_parameter').

/*
MegaLog : Garbage Collector interface
*/

force_stack_gc :- garbage_collect.

%% force_code_gc.		% No need for a code GC in Sepia
%% force_did_gc.		% No DID GC yet in Sepia

/*
MegaLog : histoy handling
Sepia   : lib(history)
*/

history :- history::h.
history_length(_).	%% What is the length of the Sepia history ?

/*
MegaLog : Invocation of editor on source code
Note    : Debugger related functionality (implementation wise)
	  therefore dropped
*/

%% editC PredSpec.
%% edit  PredSpec.

/* 
MegaLog : Operators global visible, must be abolished before redefinition 
Sepia   : Operators either local or global, direct overwrite possible, 
          local abolish impossible if not local defined 
Note    : All operator definitions are done only locally in module megalog
          operator are not removed 
*/ 
 
op(0,Assoc,Name) :- !, 
        writeln("Warning : Operators are module sensitive in Sepia"),
        write(op(0,Assoc,Name)),
        writeln("-- operator not abolished"). 
op(Prec,Assoc,Name) :- 
        writeln("Warning : Operators are module sensitive in Sepia"),
        write(op(Prec,Assoc,Name)),
        writeln("-- defined locally in module megalog only"),
        sepia_kernel::op(Prec,Assoc,Name).


current_symbol(Name / 0) :-
	current_atom(Name).
current_symbol(Name / Arity) :-
	current_functor(Name / Arity).


/*
MegaLog : returns builtin, prolog, dynamic
*/

defined_procedure(Name,Arity,builtin) :- is_built_in(Name/Arity),!.
defined_procedure(Name,Arity,dynamic) :- is_dynamic(Name/Arity),!.
defined_procedure(Name,Arity,prolog ) :- is_predicate(Name/Arity).
	
/*
MegaLog : get source file and line of definition
Sepia   : information available with get_flag/3

Note    : menitoned in manual, used by debugger/editor only
*/

procedure_table(Name,Arity,File,Line) :-
	get_flag(Name/Arity,source_file,File),
	get_flag(Name/Arity,source_line,Line).

/*
		-------------------------------
		| Section 13.15 -- Statistics |
		-------------------------------
*/

/*
MegaLog : implicit difference computation
Sepia   : absolute values and only cputime/1 exists

Note    : one should use current_time/2 and delta_time/2 instead
*/

:- setval(memory_cpu,0), setval(memory_elapsed,0).
:- setval(memory_system,0), setval(memory_user,0).

cputime(Delta) :-
	current_time(cpu,Now),
	getval(memory_cpu,Last),
	Delta is Now - Last,
	setval(memory_cpu,Now).
	
elapsed_time(Delta) :-
	current_time(elapsed,Now),
	getval(memory_elapsed,Last),
	Delta is Now - Last,
	setval(memory_elapsed,Now).
	
system_cpu_time(Delta) :-
	current_time(system_cpu,Now),
	getval(memory_system,Last),
	Delta is Now - Last,
	setval(memory_system_,Now).
	
user_cpu_time(Delta) :-
	current_time(user_cpu,Now),
	getval(memory_user,Last),
	Delta is Now - Last,
	setval(memory_user,Now).
	
/*
MegaLog : number of timers from database system

Note    : Defined in ml_builtins.c, global in Sepia-MegaLog integration
	  mentioned in manual

	  current_time(timer,Value).
	  delta_time(timer,Value).
*/

/*
MegaLog : statistics support

Note    : Defined in ml_builtins.c, global in Sepia-MegaLog integration
	  mentioned in manual

	  resource(A,B,C).		% UNIX access
	  statistics_heap.		% Heap Organization

Note    : Defined in ml_bang_builtins.c, global in Sepia-MegaLog integration
	  mentioned in manual

	  statistics_bang.
	  statistics_bang_join.
	  statistics_desc.
	  statistics_relation(R).

Note    : Defined in ml_lock.c, global in Sepia-MegaLog integration
	  mentioned in manual

	  statistics_lock.		% shared memory system only
*/

statistics_heap :- statistics.

%% print_gc_statistics.== statistics_gc % old name, not in MegaLog manual

%% statistics_code.			% Not need in Sepia
%% statistics_did.			% symbol table,  must be copied
%% statistics_gc.			% stack GC, may already exist

/*
		----------------------------------
		| Section 13.16 -- OS Connection |
		----------------------------------
*/

/*
MegaLog : return atom, no trailing /
	  accept atom or string, optional trailing / without effect
Sepia   : return string, with trailing /
          accept atom or string, optional trailing / without effect
*/
 
chdir(Path_ma) :- var(Path_ma), !, 
	get_flag(cwd,Path_ss),
	append_strings(Path_ms,"/",Path_ss),
	atom_string(Path_ma,Path_ms),
	!.
chdir(Path_ma) :-
	set_flag(cwd,Path_ma).

/*
MegaLog : atom
Sepia   : string with newline character at end
*/

date(X) :-
	sepia_kernel::date(Y),
	append_strings(Z,"\n",Y),
	atom_string(X,Z),
	!.

/*
MegaLog : Test is file exists, checks permissions and ignored directories 

Note    : Defined in builtins.c
	  mentioned in manual
	  exists/1 in Sepia checks existance, even directories

	  file_exist(File,Mode).
*/

file_exists(File, read) :-
	!,
	get_file_info(File, mode, M),
	M /\ 8'40000 =:= 0,
	M /\ 8'444 =\= 0.
file_exists(File, write) :-
	!,
	get_file_info(File, mode, M),
	M /\ 8'40000 =:= 0,
	M /\ 8'200 =\= 0.

/*
MegaLog : Get last modification time of a file

	  file_modify_time(file,Time).
*/

file_modify_time(File, Time) :-
	get_file_info(File, mtime, Time).

/*
MegaLog : Follows symbolic links, expands relative paths

Note    : Defined in builtins.c
	  limited portability to non BSD systems

	  full_path_name(name, FullName).
*/

:- external(full_path_name/2, 'SBIfull_path_name').

/*
MegaLog : prompt and dark type-in 

Note    : Defined in ml_builtins.c, global in Sepia-MegaLog integration
          used by EKS if at all

	  getpass(Prompt,PassWord).
*/

:- external(getpass/2, 'SBIgetpass').

/*
MegaLog : accepts also reals and sleep less than a second
Sepia   : accepts integers only

Note    : Sepia will sleep longer, however, it is anyway not a real time
	  system where one could rely on the sleep duration
*/

sleep(Time) :- X is fix(Time) + 1, sepia_kernel::sleep(X).

/*
		------------------------------
		| Section 13.17 -- Utilities |
		------------------------------
*/

conc(A,B,C) :- append(A,B,C).

/*
MegaLog : this was added by the EKS team 
*/

do(foreach( Firstgoal), SecondGoal) :-
        call(Firstgoal),
        not(SecondGoal),
        !, fail.
do(foreach(_), _).

/*
		------------------------------------
		| Section 13.18 -- Term Comparison |
		------------------------------------
*/

/* identical */


/*
		---------------------------------
		| Section 13.18 -- Type Testing |
		---------------------------------
*/

/*
MegaLog : discriminate compound and list
Sepia   : only compound

Note    : This also affects arg/3, functor/3, =../2 which are more
	  general in Sepia
	  %% should they be redefined to generate more errors ??
*/

compound(Term) :-
	sepia_kernel::compound(Term),
	Term \= [_|_].

list([_|_]) ?- true.

type_of([_|_],Type) ?- Type=list.
type_of(Term,Type) :- sepia::type_of(Term,Type).


/*
		-----------------
		| Not in manual |
		-----------------
*/

/*
MegaLog : not mentioned in manual, but maybe used by users
*/

ask_if_more :- writeln("More ?"), get(C), char_int(';',C), !, fail.
ask_if_more.

/*
MegaLog : Arity two, spelling
Sepia   : Arity one, different spelling
*/

is_builtin(Name,Arity) :- is_built_in(Name/Arity).

/*
MegaLog : used by some simple tools and demos only (see Antoine's work)

Note    : not needed
*/

%% get_dic_and_gc(X).
%% get_stack_limits(A,B,C).

/*
MegaLog : computes a hash value

Note    : Defined in builtins.c
	  declared as external in module 'kb' (ecc.pl)

	  hash(Term,Value).
*/


/*
MegaLog : additions for TP-1 benchmark, not in manual
Sepia   : Prolog written replacement as below

Note    : expo_random/2, random_ab/3, random_01/1 return float values
*/

expo_random(Mean,Value) :-
	random(R),
	Value is - ln(R / (2^31 - 1)) * Mean.

init_random_generation.

init_random_generation(_).

random_ab(Min,Max,Value) :-
	Min > Max,
	!,
	error(6,random(Min,Max,Value)). % error 6 = out_of_range
random_ab(Min,Max,Value) :-
	random(R),
	Value is (R / (2^31 - 1)) * (Max - Min) + Min.

random_01(Value) :- random_ab(0,1,Value).


/*
		------------------------------------
		| Database backwards compatibility |
		------------------------------------
*/

/*
** old predicates included for compatibility
** 
** These predicates are not documented, however they were documented 
** in earlier versions of the system. They should not be used any more.
*/

savedb :-
	bang_register(dbdirectory, Path),
	closedb,
	opendb(Path).
	
quit :- (closedb; true), halt.

bang_commit :-
	transaction_commit.

bang_undo :-
	transaction_undo.

destroyrel(X) :- bang_destroyrel(X).  

getglob(R,V) :- bang_register(R,V).

initglob.

elapsed_time_value(X) :- current_time(elapsed,X).

statistics_bang_join.

nbratts(Relname, Number) :- bang_arity(Relname, Number).

nbrtups(Relname, Number) :- bang_cardinality(Relname, Number).

att_type(Relname, Position, Type) :- bang_attribute(Relname, Position, Type).

bang_createrel(RelName, Format) :- 
	bang_createrel(RelName, Format, [permanent]).

bang_createrel_temp(RelName, Format) :-
	bang_createrel(RelName, Format, [temporary]).

current_relation(Name/Arity, permanent) :-
	current_relation(Name/Arity).
current_relation(Name/Arity, temporary) :- 
        current_temp_relation(Name/Arity).

bang_select_temp(Rel, CondT, ProjL, RelOut) :-
	bang_select(Rel, CondT, ProjL, RelOut, 0).
bang_select_temp(Rel, CondT, ProjL, RelOut, Action) :-
	bang_select(Rel, CondT, ProjL, RelOut, Action).

bang_join_temp(R1, R2, CondT, ProjL, RelOut) :-
	bang_join(R1, R2, CondT, ProjL, RelOut, 0).
bang_join_temp(R1, R2, CondT, ProjL, RelOut, Action) :-
	bang_join(R1, R2, CondT, ProjL, RelOut, Action).

bang_diff_temp(R1, R2, CondT, ProjL, RelOut) :-
	bang_diff(R1, R2, CondT, ProjL, RelOut, 0).
bang_diff_temp(R1, R2, CondT, ProjL, RelOut, Action) :-
	bang_diff(R1, R2, CondT, ProjL, RelOut, Action).

degree_dr(Rel,Arity) :- degree(Rel,Arity).

domains(Rel,Domains) :- call_explicit(domains(Rel,Domains), kb).

help_dr(Rel) :- helpdrel(Rel).

insert_clauses_silent_from(File) :- insert_clauses_from(File).

exec(Goal) :-
	retrieve_clause((Goal :- Body)),
	call(Body, knowledge_base).

/*
Start and End Handling

If this file is loaded with the Sepia -b option one would like to
start in module megalog directly. The error handler for error 150
achieves that.

In MegaLog a user defined predicate halt_cleanup/0 was executed after
calling halt/0. Error 152 is raised when Sepia terminates.

The close operation on the the MegaLog database and (optional) disconnect
from shared memory is done in 'C' (see sepia/bip_control.c: p_exit() ).
*/

handle_150(150,megalog).

:- sepia_kernel::set_error_handler(150, handle_150/2).

handle_152(152,_) :-
	is_predicate(halt_cleanup/0),
	!,
	halt_cleanup.
handle_152(152,_).

:- sepia_kernel::set_error_handler(152, handle_152/2).

/* remove special syntax used in this file (see top of file) */
:- erase_macro((::)/2).
:- abolish_op((::),xfx).




