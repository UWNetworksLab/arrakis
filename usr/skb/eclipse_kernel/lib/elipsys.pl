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
% Copyright (C) 1993-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: elipsys.pl,v 1.2 2008/08/20 22:57:33 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	elipsys.pl
%
% AUTHOR:		Michael Dahmen
%			Joachim Schimpf
%			Steven Prestwich
%			Liang-Liang Li
%
% CONTENTS:		name/arity
%
% DESCRIPTION:		ElipSys compatibility package, based on
%			Michael Dahmen's MegaLog compatibility package.
%

%--------------------------------------------------------------------------
:- module_interface(elipsys).
%--------------------------------------------------------------------------
:- pragma(nodebug).

:- use_module(library(fd)).
:- use_module(library(oldio)).
:- use_module(library(par_util)).

:-  op(955, xfx, do),
    op(950, fx, foreach),
    op(920, xfy, (:)),
    op(900, fy, mode),
    op(900, fy, [unspy,unskipped_predicate,unprofile]),
    op(900, fy, [skipped_predicate,profile]),
    op(900, fy, (ls)),
    op(900, fx, [parallel,heuristics,dynamic]),
    op(650, xfx, (~~)),
    op(700, xfx, (::)),
    op(650, xfx, (<>)),
    op(600, fy, [abs,float,fix]),
    op(500, fx, (@)),
    op(50, fx, [min,max]).

:- export
	(::)/2,
	alldifferent/1,
	(mode)/1,
	(abolish)/1,
	block/3,
	chdir/1,
	clauses/2,
	concat/3,
	conc/3,
	cputime/1,
	current_array/3,
	current_symbol/1,
	cut_protect/2,
	cut_fail_protect/2,
	date/1,
	deepfreeze/2,
	deepwait/2,
	defined_procedure/3,
	(do)/2,
	(dynamic)/1,
	elapsed_time/1,
	exit_fail_protect/2,
	exit_fail_success_protect/2,
	exit_protect/2,
	exit_success_protect/2,
	file_exist/2,
	file_modify_time/2,
	force_stack_gc/0,
	force_code_gc/0,
	force_did_gc/0,
	freeze/2,
	get/1,
	get/2,
	get0/1,
	get0/2,
	history/0,
	history_length/1,
	is_array/1,
	list/1,
	(listing)/1,
	load_object/1,
	(ls)/1,
	maplist/3,
	max_error/1,
	ms_clock/1,
	object_call/1,
	once_not_fail/1,
	oneof/1,
	par_select/3,
	partrace/1,
	prolog_parameter/2,
	random/3,
	readvar/2,
	(retract)/1,
	save_object/2,
	set_error_handler/2,
	set_io/2,
	silent_compile/1,
	skip/1,
	skip/2,
	srandom/1,
	statistics_did/0,
	statistics_gc/0,
	statistics_heap/0,
	strlength/2,
	system_clock/1,
	system_cpu_time/1,
	term_string/3,
	time/2,
	time/4,
	type_of/2,
	us_clock/1,
	user_clock/1,
	user_cpu_time/1,
	worker/1,
	workerid/1,
	writeqvar/2,
	writeqvar/3,
	minimize_on/0,
	minimize_off/0,
	minimize/1.

:- tool((mode)/1).
:- tool((do)/2).
:- tool(freeze/2).
:- tool(deepfreeze/2).
:- tool(deepwait/2).
:- tool(maplist/3).
:- tool(object_call/1).
:- tool(once_not_fail/1).
:- tool(oneof/1).
:- tool(silent_compile/1).
:- tool(time/2).
:- tool(time/4).

%--------------------------------------------------------------------------
:- begin_module(elipsys).
%--------------------------------------------------------------------------

:- import
	abolish_body/2,
	assert_/2,
	compiled_stream/1,
	clause_body/2,
	current_array_body/3,
	dynamic_body/2,
	eval/3,
	get_flag_body/4,
	listing_body/2,
	retract_body/2,
	(delay)/3,
	make_suspension/3,
	insert_suspension/4,
	is_predicate_/2,
	block/4
   from sepia_kernel.


%--------------------------------------
% mode/1 declaration
%--------------------------------------

:- tool((mode)/1, mode_body/2).

mode_body(G,Module) :-
   functor(G,GF,GA),
   compiled_stream(S),
   read_modes(S,M),
   delay_clauses([G|M],D),
   read_procedure(GF/GA,S,P),
   ( P == [] ->
	printf(error, "WARNING: mode declaration for %q must be just before the clauses,\n",[GF/GA]),
	printf(error, "         otherwise it will be ignored.\n%b",[])
   ;
	true
   ),
   append(D,P,DP),
   compile_term(DP)@Module.

delay_clauses([],[]).
delay_clauses([G|M],[(delay G1 if Conds)|D]) :-
   functor(G,F,A),
   functor(G1,F,A),
   G=..[_|Modes],
   G1=..[_|Vars],
   modes_conds(Modes,Vars,Conds),
   delay_clauses(M,D).

modes_conds([],[],true).
modes_conds([Mode|Modes],[Var|Vars],Conds) :-
   mode_cond(Mode,Var,Cond),
   modes_conds(Modes,Vars,Conds1),
   conjoin(Cond,Conds1,Conds).

mode_cond('+',V,var(V)) :- !.
mode_cond('++',V,nonground(V)) :- !.
mode_cond('-',_,true) :- !.
mode_cond('?',_,true) :- !.
mode_cond(X,_,true) :-
   printf(error, "WARNING: unrecognised elipsys mode symbol ", [X]).

conjoin(true,X,X) :- !.
conjoin(X,true,X) :- !.
conjoin(X,Y,(X,Y)).

read_modes(S,M) :-
   at(At,S),
   read(X,S),
   (X=(:- mode G) ->
      M=[G|L],
      read_modes(S,L)
   ;X=(?- mode G) ->
      M=[G|L],
      read_modes(S,L)
   ;  seek(At,S),
      M=[]).

read_procedure(G,S,Po) :-
   at(At,S),
   read(C,S),
   (same_predicate(C,G) ->
      Po=[C|Po1],
      read_procedure(G,S,Po1)
   ;  seek(At,S),
      Po=[]).

same_predicate((H:-_),F/A) :-
   !,
   functor(H,F,A).
same_predicate(H,F/A) :-
   functor(H,F,A).

%--------------------------------------------------------------------------
% B.1 Arithmetic
%--------------------------------------------------------------------------

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
	Value is abs(random) mod (Max - Min + 1) + Min.
	
srandom(Seed) :- seed(Seed).


% A simple arithmetic predicate, e.g. +/3 or abs/2 got an expression
% as input, which has to be evaluated.

eval_handler(_, Goal) :-
        functor(Goal, F, A),
        functor(NewGoal, F, A),
        arg(1, Goal, X),
        arg(1, NewGoal, X1),
        eval(X, X1, elipsys),
        (number(X1) -> true ; var(X1) -> true ; error(5, Goal)),
	( A == 3 ->
	    arg(2, Goal, Y),
	    arg(2, NewGoal, Y1),
	    eval(Y, Y1, elipsys),
	    (number(Y1) -> true ; var(Y1) -> true ; error(5, Goal))
	;
	    true
	),
	arg(A, Goal, Res),
	% next line to avoid looping
	(number(Res) -> true ; var(Res) -> true ; error(5, Goal)),
	arg(A, NewGoal, Res),
        call(NewGoal).                  % we don't have the caller module!

:- call_explicit(set_error_handler(24, eval_handler/2), sepia_kernel).

%--------------------------------------------------------------------------
% B.2 Dynamic Database
%--------------------------------------------------------------------------

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

:- tool((abolish)/1, abolish_b/2).
abolish_b([], _) :- !.
abolish_b([H|T], M) :- !, abolish_b(H,M), abolish_b(T,M).
abolish_b(P, M)  :- is_predicate_(P, M), !, abolish_body(P,M).
abolish_b(_, _).	% succeed if it doesn't exist

:- tool(clauses/2,clauses_body/3).
clauses_body(Name/Arity,Clauses,Module) :-
	functor(Head,Name,Arity),
	findall((Head :- Body),clause_body((Head :- Body),Module),Clauses).

:- tool((dynamic)/1, dynamic_b/2).
dynamic_b([], _) :- !.
dynamic_b([H|T], M) :- !, dynamic_body(H,M), dynamic_b(T,M).
dynamic_b(P, M)  :- dynamic_body(P,M).

:- tool((ls)/1, listing_b/2).
:- tool((listing)/1, listing_b/2).
listing_b([], _) :- !.
listing_b([H|T], M) :- !, listing_body(H,M), listing_b(T,M).
listing_b(P, M)  :- listing_body(P,M).

:- tool(retract/1, retract_b/2).
retract_b(Clause, Module) :- retract_body(Clause, Module),!.
% Note    : This compatibility is probably unwanted

/*
MegaLog : If assert, retract etc. is applied to a compiled procedure
	  the procedure is abolished and an empty dynamic procedure
	  is created
Sepia   : Error 63 is raised in such a situation

MegaLog : If a procedure is compiled the asserted clauses are removed
Sepia	: Asserted and compiled clauses coexist

Note    : The error handler defined below solves some of these problems
	  but not all. We recommend to review your program wrt. use
	  of asserted clauses.
	  %%
*/

error_handler_63(_,Culprit,Module) :-
	get_flag_body((dynamic)/1,definition_module,elipsys,Module),
	!,	% only for modules using elipsys library
	error_handler_63(Culprit,Module).
error_handler_63(N,Culprit,Module) :-
	error(default(N),Culprit,Module).

error_handler_63(Culprit,Module) :-
	call_explicit(compound(Culprit), sepia_kernel),
	Culprit =.. [Functor,Clause],
	memberchk(Functor,[assert, asserta, assertz, retract]),
	!,
	(Clause = (Head :- _) -> true ; Clause = Head),
	functor(Head,F,Arity),
	( is_predicate_(F/Arity, Module) ->
	    abolish_body(F/Arity, Module)
	;
	    true
	),
	dynamic_body(F/Arity, Module),
	call(Culprit, Module).
error_handler_63(retract_all(Head),Module) :-
	!,
	functor(Head,F,Arity),
	abolish_body(F/Arity, Module),
	dynamic_body(F/Arity, Module).
error_handler_63(Culprit,Module) :-
	error(default(63),Culprit,Module).

:- call_explicit(set_error_handler(63,error_handler_63/3), sepia_kernel).


%--------------------------------------------------------------------------
% B.3 Global Variables and Arrays
%--------------------------------------------------------------------------

is_array(Array) :-
	current_array(Array, _).

/*
MegaLog : erase_array/1 succeeds silently if array does not exist
Sepia   : raise error 41

Note    : This has a global effect in all modules
%%
*/

error_handler_41(41,erase_array(_),_) :- 
	!.
error_handler_41(41,Goal,Module) :- 
	call_explicit(undef_array_handler(41,Goal,Module), sepia_kernel).  

:- call_explicit(set_error_handler(41,error_handler_41/3), sepia_kernel).

/*
MegaLog : make_array/1 /2 succeeds silently if array already exists
	  exists with same boundaries
	  error and failure if array exists with different boundaries
Sepia   : raise warning and succeed in any case
%%
*/


%--------------------------------------------------------------------------
% B.4 Error Handling
%--------------------------------------------------------------------------

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
	

%--------------------------------------------------------------------------
% B.5 Blocks
%--------------------------------------------------------------------------


/*
MegaLog : block/3 is deterministic
Sepia   : block/3 backtracks
*/

:- tool(block/3, block_body/4).
block_body(Goal,Tag,Recover,Module) :-
	block((Goal,!),Tag,Recover,Module).

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
 
cut_protect(A, B) :- error(267, cut_protect(A, B)).
cut_fail_protect(A, B) :-error(267, cut_fail_protect(A, B)). 


%--------------------------------------------------------------------------
% B.6 I/O
%--------------------------------------------------------------------------

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
** Note    : This is solved by use_module(library(oldio)) above
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

get(X)    :- repeat,call_explicit(get(X), sepia_kernel),X > 32,!.
get(X,S)  :- repeat,call_explicit(get(S,X), sepia_kernel),X > 32,!. 
% since get/1 /2 is originally Prolog defined this definition must be
% compiled before any client code is compiled

get0(X)   :- call_explicit(get(X), sepia_kernel).
get0(X,S) :- call_explicit(get(S,X), sepia_kernel).
skip(C)   :- repeat,call_explicit(get_char(C), sepia_kernel),!.
skip(C,S) :- repeat,call_explicit(get_char(S,C), sepia_kernel),!.

/*
MegaLog : buffered I/O level 	(stream_id = 'C' pointer)
Sepia   : raw I/O level  	(stream_id = 0,1,2,...)

Note    : get_stream/2 is identical
*/

% difficult to achieve, questionable wether used

set_io(A,B) :- error(267, set_io(A,B)).

/*
MegaLog : exists in arity two and three
Sepia   : arity three only
*/

readvar(Term,VarList) :- readvar(input,Term,VarList).

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

writeqvar(Term,VariableList) :-
	error(267, writeqvar(Term,VariableList)).
writeqvar(Term,VariableList,Stream) :-
	error(267, writeqvar(Term,VariableList,Stream)).


%--------------------------------------------------------------------------
% B.7 Control
%--------------------------------------------------------------------------

:- tool(once_not_fail/1, once_not_fail/2).
once_not_fail(Goal, Module) :- call(Goal, Module), !.
once_not_fail(_, _).

:- tool(oneof/1, oneof/2).
oneof(Goal, Module) :- nonvar(Goal), call(Goal, Module), !.

/*
MegaLog : debugger related, mentioned in manual therefore may be used
*/

:- tool(object_call/1, call/2).


%--------------------------------------------------------------------------
% B.8 Compilation and Storing Compiled Code
%--------------------------------------------------------------------------

/*
MegaLog : capability to generate and load object files
Sepia   : use either saved states or Prolog term files 
Note    : program development environment only
*/

:- tool(silent_compile/1, silent_compile/2).
silent_compile(F, M) :- compile(F, M).

load_object(File) :- error(267, load_object(File)).
save_object(Object,Source) :- error(267, save_object(Object,Source)).


%--------------------------------------------------------------------------
% B.9 All Solutions
%--------------------------------------------------------------------------

/*  identical */

%--------------------------------------------------------------------------
% B.10 Strings & Conversion
%--------------------------------------------------------------------------

/*
MegaLog : accept atoms, string, integers, reals
	  result is string, atom given fails
*/

concat(A,B,C) :- atom(A), !, atom_string(A,A1), concat(A1,B,C).
concat(A,B,C) :- atom(B), !, atom_string(B,B1), concat(A,B1,C).
concat(A,B,C) :- number(A), !, term_string(A,A1), concat(A1,B,C).
concat(A,B,C) :- number(B), !, term_string(B,B1), concat(A,B1,C).
concat(A,B,C) :- call_explicit(concat_strings(A,B,C), sepia_kernel).

/*
MegaLog : only different name, rsp. differnt arity
*/

strlength(String,Length) :- string_length(String,Length).

/*
MegaLog : allow to use a variable list during conversion (both ways)

Note    : Requires MegaLog parser/printer and can therefore not be ported
	  to Sepia.
*/

term_string(Term,String,VariableList) :-
	error(267, term_string(Term,String,VariableList)).

%--------------------------------------------------------------------------
% B.11 Environment
%--------------------------------------------------------------------------

/*
MegaLog : access to a number of internal variable (database & Prolog)

Note    : Defined in ml_builtins.c, global in Sepia-MegaLog integration
	  mentioned in manual
	  Prolog machine related parameter will raise a warning in 
	  Sepia-MegaLog integration
	  
	  prolog_parameter(Name,Value).
*/

:- get_flag(extension, megalog) ->
	external(prolog_parameter/2, 'SBIprolog_parameter')
    ;
	compile_term((prolog_parameter(A, B) :- printf(error, " *** prolog_parameter(%w, %w) ignored\n\b", [A, B]))).


/*
MegaLog : Garbage Collector interface
*/

force_stack_gc :- garbage_collect.

force_code_gc.

force_did_gc :-
	get_flag(gc_interval_dict, Old),
	set_flag(gc_interval_dict, 1),
	concat_atoms('$$$$$$', 'dummy_atom', _),
	set_flag(gc_interval_dict, Old).

/*
MegaLog : histoy handling
Sepia   : kernel.pl
*/

history :- writeln(error, "Use h. to print history").
history_length(30).

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
 

current_symbol(Symbol) :-
	current_functor(Symbol).

:- tool(current_array/3, current_array_body/4).

current_array_body(Atom, ListOfBounds, Type, Module) :-
        current_array_body(Array, [Type|_], Module),
        Array =.. [Atom|ListOfBounds].

	
%--------------------------------------------------------------------------
% B.12 Statistics
%--------------------------------------------------------------------------

/*
MegaLog : implicit difference computation
Sepia   : absolute values and only cputime/1 exists

Note    : one should use current_time/2 and delta_time/2 instead
*/

:-  make_local_array(memory_elapsed), setval(memory_elapsed,0).
:-  make_local_array(memory_system), setval(memory_system,0).
:-  make_local_array(memory_user), setval(memory_user,0).

cputime(Delta) :-
	user_cpu_time(Delta).
	
elapsed_time(Delta) :-
	statistics(session_time, NowS),
	Now is fix(1000*NowS),
	Delta is Now - getval(memory_elapsed),
	setval(memory_elapsed,Now).
	
system_cpu_time(Delta) :-
	statistics(times, [_,NowS,_]),
	Now is fix(1000*NowS),
	Delta is Now - getval(memory_system),
	setval(memory_system,Now).
	
user_cpu_time(Delta) :-
	statistics(times, [NowS,_,_]),
	Now is fix(1000*NowS),
	Delta is Now - getval(memory_user),
	setval(memory_user,Now).

system_clock(Time) :-
	statistics(times,[_User,System,_Elapsed]),
	Time is fix(1000 * System).

user_clock(Time) :-
	statistics(times,[User,_System,_Elapsed]),
	Time is fix(1000 * User).

ms_clock(Time) :-
	statistics(session_time,Ts),
	Time is fix(Ts * 1000).

us_clock(Time) :-
	statistics(session_time,Ts),
	Time is fix(Ts * 1000000).

:- tool(time/2,time_tool/3).

time_tool(Goal,Time,Module) :-
	statistics(session_time,T0),
	(call(Goal,Module), fail ;true),
	statistics(session_time,T1),
	Time is fix((T1 - T0) * 1000.0 + 0.5).

:- tool(time/4,time_tool/5).

time_tool(Goal,Time,Template,Solutions,Module) :-
	statistics(session_time,T0),
	findall(Template,call(Goal,Module),Solutions),
	statistics(session_time,T1),
	Time is fix((T1 - T0) * 1000.0 + 0.5).


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

statistics_heap :- some_statistics(_, "_heap_").
statistics_gc :- some_statistics(_, "_stack_"), some_statistics(1, "gc_").
statistics_did :- some_statistics(1, "dict").

some_statistics(Pos, Match) :-
	statistics(What, Value),
	atom_string(What, WhatS),
	substring(WhatS, Match, Pos),
	Fill is 30 - atom_length(What),
	printf("%w:%*c%w\n", [What, Fill, 0' , Value]),
	fail.
some_statistics(_, _).

%--------------------------------------------------------------------------
% B.13 Operating System Connection
%--------------------------------------------------------------------------

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
	cd(Path_ma).

/*
MegaLog : atom
Sepia   : string with newline character at end
*/

date(X) :-
	call_explicit(date(Y), sepia_kernel),
	append_strings(Z,"\n",Y),
	atom_string(X,Z),
	!.


% file_exist(Name, Mode)
% Test if file exists, checks permissions and ignores directories 
% exists/1 in ECLiPSe checks only existance, even directories

file_exist(Name, Mode) :-
	exists(Name),
	get_file_info(Name, mode, M),
	M /\ 8'170000 =\= 8'40000,	% not a directory
	( Mode == read ->
		get_file_info(Name, readable, on)
	; Mode == write ->
		get_file_info(Name, writable, on)
	).

% Get last modification time of a file

file_modify_time(Name, Time) :-
	get_file_info(Name, mtime, Time).


/*
MegaLog : Follows symbolic links, expands relative paths

Note    : Defined in builtins.c
	  limited portability to non BSD systems

	  full_path_name(name, FullName).
*/

% :- external(full_path_name/2, 'SBIfull_path_name').


%--------------------------------------------------------------------------
% B.14 Utilities
%--------------------------------------------------------------------------

conc(A,B,C) :- append(A,B,C).

/*
MegaLog : this was added by the EKS team 
*/

:- tool(do/2, do/3).
do(foreach Firstgoal, SecondGoal, Module) :-
        call(Firstgoal, Module),
	( call(SecondGoal, Module) ->
	    fail
	;
	    !, fail
	).
do(foreach _, _, _).

%--------------------------------------------------------------------------
% B.15 Term Comparison
%--------------------------------------------------------------------------

/* identical */

%--------------------------------------------------------------------------
% B.16 Type Testing and Conversion
%--------------------------------------------------------------------------

list(X) :- var(X), !, fail.
list([_|_]).

type_of([_|_],Type) :- -?-> !, Type=list.
type_of(Term,Type) :- call_explicit(type_of(Term,Type), sepia_kernel).


/*
MegaLog : returns builtin, prolog, dynamic
*/

defined_procedure(Name,Arity,builtin) :- is_built_in(Name/Arity),!.
defined_procedure(Name,Arity,dynamic) :- is_dynamic(Name/Arity),!.
defined_procedure(Name,Arity,prolog ) :- is_predicate(Name/Arity).

%--------------------------------------------------------------------------
% B.17 Parallel Execution (most are in library(par_util))
%--------------------------------------------------------------------------

% par_select/3 is rather useless.
:- parallel par_select/3.
par_select( X, [X|R], R ).
par_select( X, [_|L], R ) :- par_select( X, L, R ).


% The ElipSys maplist/3 is not in par_util because its semantics
% differs from the commonly accepted definition of maplist/3.

:- tool(maplist/3, maplist_body/4).

maplist_body(Pred, In, Out, Module) :-
	findall(Sol, map_elements(Pred, In, Sol, Module), Out0),
	sort(1, >=, Out0, Out1),
	strip_key(Out1, Out).

map_elements(Pred, In, I-Xout, Module) :-
        Pred =.. [Fct, _, _|Others],
        Call =.. [Fct, Xin, Xout|Others],
	InArr =.. [in|In],
	functor(InArr, in, N),
	N1 is N+1,
	fork(N, I),
	I1 is N1-I,
	arg(I1, InArr, Xin),
	( call(Call, Module), true -> true ).

strip_key([], []).
strip_key([_-X|Xs], [X|Ys]) :- strip_key(Xs, Ys).


worker(I) :-
	var(I), !,
	get_flag(hostname, Host),
	get_flag(workers, Host:I).
worker(I) :-
	integer(I), !,
	set_flag(workers, _:I).
worker(I) :-
	error(5, worker(I)).


workerid(X) :- get_flag(worker,X).

%--------------------------------------------------------------------------
% B.18 Parallel Execution Tracer
%--------------------------------------------------------------------------

partrace(Mode) :- error(267, partrace(Mode)).

%--------------------------------------------------------------------------
% B.19 Symbolic Constraints
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
% B.20 Arithmetic Constraints
% mostly in library(fd)
%--------------------------------------------------------------------------

X :: A~~B :- !, call_explicit(X :: A..B, fd_domain).
X :: A<>B :- !, call_explicit(X :: A..B, fd_domain).
X :: Other :- call_explicit(X :: Other, fd_domain).

alldifferent([]).
alldifferent([A,B|C]) :-
   alldifferent(C,A,B),
   alldifferent(C).

delay alldifferent(L,_,_) if var(L).

alldifferent([],_,_).
alldifferent([A,B|C],D,E) :-
   A+B ## D+E,
   alldifferent(C,D,E).


%--------------------------------------------------------------------------
% B.21 Generalised Propagation
%--------------------------------------------------------------------------

/* in library(propia) */

%--------------------------------------------------------------------------
% B.22 Metaprogramming
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
% B.23 Suspension and Suspension Handling
%--------------------------------------------------------------------------

:- tool(freeze/2, freeze_body/3).

freeze_body(X, Goal, Module) :-
	var(X), !,
	make_suspension(Goal, Susp, Module),
	insert_suspension(X, Susp, 1, top).
freeze_body(_, Goal, Module) :-
	call(Goal, Module).


% deepwait(Term, Goal) -- the same as delay/2 in ECLiPSe

:- tool(deepwait/2, deepwait_body/3).

deepwait_body(Term, Goal, Module) :-
	delay(Term, Goal, Module).


:- tool(deepfreeze/2, deepfreeze_body/3).

deepfreeze_body(Term, Goal, Module) :-
	term_variables(Term, Vars),
	copy_term(Vars, VCopy),
	make_suspension(deepfreeze_body(VCopy, Vars, Goal, Module), Susp),
	insert_suspension(Vars, Susp, 1, top).

deepfreeze_body(VCopy, Vars, Goal, Module) :-
	compare_instances(R, Vars, VCopy),
	( R == '<' ->
	    call(Goal, Module)
	;
	    % redelay
	    % (we could filter out nonvariables from VCopy and Vars)
	    make_suspension(deepfreeze_body(VCopy, Vars, Goal, Module), Susp),
	    insert_suspension(Vars, Susp, 1, top)
	).


%--------------------------------------------------------------------------
% B.24 Optimization Meta-Predicates
%--------------------------------------------------------------------------

:-  make_local_array(minimize_verbose), setval(minimize_verbose,off).
minimize(On) :- getval(minimize_verbose,On).
minimize_off :- setval(minimize_verbose,off).
minimize_on  :- setval(minimize_verbose,on).

/* in library(fd) */

%--------------------------------------------------------------------------
% B.25 Enumeration and Heuristics
%--------------------------------------------------------------------------

/* in library(fd) */

%--------------------------------------------------------------------------
% B.26 Miscellaneous
%--------------------------------------------------------------------------

handle_152(152,_) :-
	is_predicate(halt_cleanup/0),
	!,
	halt_cleanup.
handle_152(152,_).

:- call_explicit(set_error_handler(152, handle_152/2), sepia_kernel).

