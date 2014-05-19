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
% Version:	$Id: sicstus.pl,v 1.1.2.1 2008/12/19 05:41:11 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	sicstus.pl
 *
 * DESCRIPTION: 	SICStus Prolog compatibility package
 *
 *
 * CONTENTS:     
 *
 */

:- module(sicstus).

:- comment(summary, 'SICStus Prolog Compatibility Package').
:- comment(author, 'Micha Meier, ECRC Munich').
:- comment(copyright, 'Cisco Systems, Inc').
:- comment(date, '$Date: 2008/12/19 05:41:11 $').
:- comment(desc, html('
    ECLiPSe includes a SICStus Prolog compatibility package to ease
    the task of porting SICStus Prolog applications to ECLiPSe Prolog. 
    This package includes the C-Prolog compatibility package (lib(cprolog))
    and the Quintus-Prolog compatibility package (lib(quintus)). 
    <P>
    Please note that this appendix does not detail the functionality
    of SICStus Prolog, refer to the SICStus Prolog documentation for
    this information. 
    <P>
    The effect of the compatibility library is local to the module where
    it is loaded. For maximum compatibility, a Sicstus program should
    be wrapped in a separate module starting with a directive like
    <PRE>
    :- module(mymodule, [], sicstus).
    </PRE>
    In this case, Eclipse-specific language constructs will not be available.
    <P>
    If the compatibility package is loaded into a standard module, e.g. like
    <PRE>
    :- module(mymixedmdule).
    :- use_module(library(sicstus)).
    </PRE>
    then Sicstus and Eclipse language features can be used together. 
    However, ambiguities must be resolved explicitly and confusion may
    arise from the different meaning of quotes in Eclipse vs Sicstus-Prolog.
    <P>
    A sockets library is provided for compatibility with the sockets
    manipulation predicates of SICStus.  To use these predicates, the
    sockets library has to be loaded: 
    <PRE>
    :- use_module(library(sockets)).
    </PRE>
    For SICStus 3.0, the sockets predicates are also in a sockets library,
    so no changes are needed to load the library.  However, for older
    versions of SICStus, the predicates are available as built-ins, and no
    library has to be loaded.  So if the code is written for older
    versions of SICStus, then the above line has to be added. 
    <P>
    The sockets library can be used independently of the sicstus library. 
    Note also that ECLiPSe also provides its own socket manipulation
    predicates that provides similar functionalities to the sockets library. 
    <P>
    Since the SICStus package contains the Quintus one, the syntax
    differences are the same.
    ')).
:- comment(see_also, [library(cio),library(cprolog),library(quintus),
	library(sockets),library(swi)]).

:- comment(call_residue/2, [template:'call_residue(+Goal,-Residue)',
    summary:'This is only approximate, the variables in the second argument are dummies'
    ]).

% suppress deprecation warnings for reexported builtins
:- pragma(deprecated_warnings(not_reexports)).

:- reexport quintus except
	load/1.

:- export
	op(1150, fx, block).

:- export
	(block)/1,
	call_residue/2,
	dif/2,
	freeze/2,
	frozen/2,
	(if)/3,
	load/1,
	on_exception/3,
	raise_exception/1,
	when/2.
	
:- export
        chtab(0'\,escape).  % character escapes are on by default in SICStus

:- local
	op(1100,  xfy, (do)),
	op(650,   xfx, (@)).

:- system.		% compiler directive to add the SYSTEM flag

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- import
	(*->)/2,
	(block)/4,
	compiled_stream/1,
	suspend_body/4,
	erase_macro_/2,
	import_body/2,
	read_/3,
	subcall/3,
	untraced_call/2
   from sepia_kernel.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- tool(freeze/2, freeze_body/3).
:- inline(freeze/2, tr_freeze/2).

tr_freeze(freeze(Var, Goal),
	( var(Var) -> suspend(Goal, 2, (Var->suspend:1)) ; Goal )).

:- system_debug.
freeze_body(X, Goal, Module) :-
	var(X), !,
	suspend_body(Goal, 2, (X->suspend:1), Module).
freeze_body(_, Goal, Module) :-
	untraced_call(Goal, Module).

:- system.
frozen(Var, Goals) :-
	var(Var),
	delayed_goals(Var, List),
	list_to_comma(List, Goals).

list_to_comma([], true) :- !.
list_to_comma([G], G) :- !.
list_to_comma([H|T], (H,Rest)) :-
	list_to_comma(T, Rest).

dif(A, B) :-
	A ~= B.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sicstus's block-directives are translated as follows:
%	:- block p(-,?).
%	p(a,b).
% into
%	p(A,B) :- var(A), !, make_suspension(p(A,B),0,S), insert_suspension([A],S,1,suspend).
%	p(A,B) :- 'p unblocked'(A,B).
%	'p unblocked'(a,b).
% i.e. new clauses are generated to implement the delay conditions, and
% the original predicate is renamed with the help of a clause macro.

:- tool((block)/1, block_body/2).
block_body(List, M) :-
	block_to_clauses(List, Clauses, [(Head:-Call)], Name/Arity),
	!,
	functor(Head, Name, Arity),
	rename_functor(Head, Call),
	compile_term(Clauses)@M,
	local(macro(Name/Arity,sicstus:rename_head/2,[clause]))@M.
block_body(List, M) :-
	printf(error, '*** Error in block-declaration %w%n', [block(List)])@M,
	fail.

:- export rename_head/2.
rename_head((OldHead:-Body), Renamed) ?- !,
	Renamed = (NewHead:-Body),
	rename_functor(OldHead, NewHead).
rename_head(OldHead, NewHead) :-
	rename_functor(OldHead, NewHead).

    rename_functor(Term, NewTerm) :-
	functor(Term, OldName, Arity),
	concat_atoms(OldName, ' unblocked', NewName),
	functor(NewTerm, NewName, Arity),
	( for(I,1,Arity), param(Term,NewTerm) do
	    arg(I,Term,Arg), arg(I,NewTerm,Arg)
	).
	
block_to_clauses((B1,B2), D1, C, Pred) :-
	!,
	block_to_clauses(B1, D1, C0, Pred),
	block_to_clauses(B2, C0, C, Pred).
block_to_clauses(B, [(Head:-Body)|C], C, Name/Arity) :-
	functor(B, Name, Arity),
	B =.. [Name|Args],
	arg_and_body(Args, H, Body, BC, Vars, []),
	Head =.. [Name|H],
	BC = (!, make_suspension(Head,0,S), insert_suspension(Vars, S, 1, suspend)).

:- mode arg_and_body(+, -, -, ?, -, ?).
arg_and_body([], [], BC, BC, V, V).
arg_and_body([?|A], [_|H], B, BC, V, VC) :-
	!,
	arg_and_body(A, H, B, BC, V, VC).
arg_and_body([-|A], [X|H], (var(X),B), BC, [X|V], VC) :-
	arg_and_body(A, H, B, BC, V, VC).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- tool(when/2, when_body/3).

:- system_debug.
when_body(Condition, Goal, Module) :-
	condition_fails(Condition, Vars),
	!,
	suspend(when_body(Condition, Goal, Module), 2, (Vars->inst)).
when_body(_Condition, Goal, Module) :-
	untraced_call(Goal, Module).

:- system.
:- mode condition_fails(?,-).
condition_fails(Condition, _) :- var(Condition), !, fail.
condition_fails(nonvar(X), X) :- var(X), !.
condition_fails(ground(X), V) :- nonground(X, V), !.
condition_fails(X == Y, [X|Y]) :- X \== Y, !.
condition_fails((C1;C2), [V1|V2]) :-
	condition_fails(C1, V1),
	condition_fails(C2, V2).
condition_fails((C1,_C2), V) :-
	condition_fails(C1, V), !.
condition_fails((_C1,C2), V) :-
	condition_fails(C2, V).


% call_residue/2 is not quite ok - the variables in the
% residue list are only dummies, unrelated to the goals

:- system_debug.
:- tool(call_residue/2, call_residue_body/3).
call_residue_body(Goal, Residue, Module) :-
	subcall(Goal, Delayed, Module),
	add_dummy_variables(Delayed, Residue).

:- tool((if)/3, if_body/4).
if_body(A, B, C, M) :-
	*->(untraced_call(A, M), untraced_call(B, M)) ; untraced_call(C, M).

:- system.
add_dummy_variables([], []).
add_dummy_variables([G|Gs], [_-G|Rs]) :-
	add_dummy_variables(Gs, Rs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- tool(load/1, load_body/2).

load_body([File|Files], M) :-
	!,
	load_body(File, M),
	load_body(Files, M).
load_body(Module:File, _) :-
	!,
	compile(File, Module).
load_body(File, M) :-
	compile(File, M).

:- export fcompile/1.
:- tool(fcompile/1, fcompile/2).
fcompile(File, Module) :-
	fcompile:fcompile(File)@Module.

:- tool(on_exception/3, on_exception_body/4).

:- system_debug.
on_exception_body(Tag, Goal, Recovery, M) :-
	block(Goal, Tag, Recovery, M).

:- system.
raise_exception(Tag) :-
	exit_block(Tag).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- skipped
	dif/2,
	frozen/2.

:- unskipped
	freeze_body/3,
	call_residue_body/3,
	on_exception_body/4.

:- untraceable
	freeze_body/3,
	call_residue_body/3,
	add_dummy_variables/2.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	MODULE INITIALIZATION
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

