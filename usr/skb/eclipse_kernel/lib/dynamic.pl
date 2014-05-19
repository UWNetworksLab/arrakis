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
% Copyright (C) 1989-2007 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: dynamic.pl,v 1.7 2008/10/02 11:45:53 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	dynamic.pl
 *
 * DESCRIPTION: 	This file contains all the Prolog predicates
 *			that handle dynamic predicates.
 *
 * CONTENTS:     
 *
 * REVISION HISTORY:
 *
 * AUTHOR	VERSION	 DATE	REASON
 * periklis		26.9.89	Major revision for the logical update semantics.
 * micha		20.3.89	Moved all the dynamic-related predicates
 *				from db.pl into this file.
 * joachim		2007	Radically simplified record-based version
 */

:- begin_module(sepia_kernel).

:- system.		% compiler directive to add the SYSTEM flag

:- export
	(abolish)/1,
	assert/1,
	asserta/1,
	assertz/1,
	(dynamic)/1,
	is_dynamic/1,
	clause/1,clause/2,
	(listing)/0,(listing)/1,
	retract/1,
	retract_all/1,
	retractall/1,
	writeclause/1,
	writeclause/2.


/*
 * TOOL DIRECTIVES
 * (body names are chosen for backward compatibility)
 */

:- tool((abolish)/1, abolish_body/2).
:- tool(assert/1, assert_/2).
:- tool(asserta/1, asserta_/2).
:- tool(assertz/1, assert_/2).
:- tool(clause/1, clause_body/2).
:- tool(clause/2, clause_body/3).
:- tool((dynamic)/1, dynamic_body/2).
:- tool(is_dynamic/1, is_dynamic_body/2).
:- tool(listing/1, listing_body/2).
:- tool(listing/0, listing_body/1).
:- tool(retract/1, retract_body/2).
:- tool(retract_all/1, retract_all_body/2).
:- tool(retractall/1, retract_all_body/2).
:- tool(write_goal/3, write_goal/4).		% exported, for opium



%
% Dynamic clauses are recorded in source form in the indexed database,
% under an anonymous SrcHandle.
% 
% Additionally every dynamic predicate has the following stub code,
% which contains that SrcHandle.  E.g. for p/3:
%
%	p(A, B, C) :-
%	    call_dynamic_(<SrcHandle for p/3>, p(A,B,C), <home module>).
%
% call_dynamic_/3 is common code, essentially an interpreter for the
% clauses stored under SrcHandle.  Note that cuts in the clause body
% must cut the recorded-choicepoint that backtracks over the clauses!

call_dynamic_(SrcHandle, Goal, Module) :-
	get_cut(Cut),
	recorded(SrcHandle, (Goal:-Body)),
	call_with_cut(Body, Module, Module, Cut).



% Dynamic declaration - we allow several variants:
%	dynamic n/a
%	dynamic n/a, n/a, n/a		% Sepia
%	dynamic [n/a, n/a, n/a]		% Quintus, ISO, ...

dynamic_body((F1, F2), Module) ?-
	dynamic_body_enum((F1,F2), Module),
	!.
dynamic_body([F|Fs], Module) ?-
	dynamic_body_list([F|Fs], Module),
	!.
dynamic_body(F, Module) :-
	dynamic_body_single(F, Module),
	!.
dynamic_body(Preds, Module) :-
	get_bip_error(E),
	error(E, dynamic(Preds), Module).

    dynamic_body_enum((F1,F2), Module) ?- !,
    	dynamic_body_enum(F1, Module),
    	dynamic_body_enum(F2, Module).
    dynamic_body_enum(F, Module) :-
    	dynamic_body_single(F, Module).

    dynamic_body_list(Fs, _Module) :- var(Fs), !,
    	set_bip_error(4).
    dynamic_body_list([], _Module) :- !.
    dynamic_body_list([F|Fs], Module) :- !,
    	dynamic_body_single(F, Module),
	dynamic_body_list(Fs, Module).
    dynamic_body_list(_, _) :-
    	set_bip_error(5).

    dynamic_body_single(Name/Arity, Module) ?-
    	atom(Name), integer(Arity), Arity >= 0, !,
	dynamic_create_(Name, Arity, Module).
    dynamic_body_single(Pred, _) :-
	nonground(Pred) -> set_bip_error(4) ; set_bip_error(5).


is_dynamic_body(Functor, Module) :-
	( check_predspec(Functor, Module) ->
		Functor = Name/Arity,
		is_dynamic_(Name, Arity, Module)
	;
		bip_error(is_dynamic(Functor), Module)
	).


% abolish/1 gets rid of the definition of the predicate specified 
% by the argument. Name must be fully instantiated.
% Arity must be fully instantiated (we differ from BSI). 

abolish_body(Name/Arity, Module ) :- !,
	( abolish_(Name,Arity,Module) ->
	    true
	;
	    get_bip_error(Error),
	    error(Error, abolish(Name/Arity), Module)
	).
abolish_body( (F1, F2), Module ) :- !,
        abolish_body( F1, Module ),
        abolish_body( F2, Module ).
abolish_body(Functor, Module ) :-
	error(5, abolish(Functor), Module).


% Auxiliary to check and decompose clause arguments

clause_info(Clause, _N, _A, _NormClause) :- var(Clause), !,
    	set_bip_error(4).
clause_info(Clause, N, A, NormClause) :- Clause = (Head:-_), !,
	NormClause = Clause,
	check_callable(Head),
	functor(Head, N, A).
clause_info(Head, N, A, NormClause) :-
	NormClause = (Head:-true),
	check_callable(Head),
	functor(Head, N, A).


% Handler for event 70

undef_dynamic_handler(N, assert(Clause), Module) :- !,
	undef_dynamic_handler(N, assertz(Clause), Module).
undef_dynamic_handler(_N, asserta(Clause), Module) :-
	clause_info(Clause, Name, Arity, _NormClause),
	dynamic_create_(Name, Arity, Module),
	!,
	asserta(Clause)@Module.
undef_dynamic_handler(_N, assertz(Clause), Module) :-
	clause_info(Clause, Name, Arity, _NormClause),
	dynamic_create_(Name, Arity, Module),
	!,
	assertz(Clause)@Module.
undef_dynamic_handler(N, Goal, _) :-
	( get_bip_error(E) ->
	    error_handler(E, Goal)
	;
	    error_handler(N, Goal)
	).


asserta_(Clause, Module) :-
	( clause_info(Clause, N, A, NormClause),
	  dynamic_source_(N, A, SrcHandle, Module) ->
	    recorda(SrcHandle, NormClause)
	;
	    bip_error(asserta(Clause), Module)
	).


assert_(Clause, Module) :-
	( clause_info(Clause, N, A, NormClause),
	  dynamic_source_(N, A, SrcHandle, Module) ->
	    recordz(SrcHandle, NormClause)
	;
	    bip_error(assertz(Clause), Module)
	).


clause_body(Clause, Module) :-
	( clause_info(Clause, N, A, NormClause),
	  dynamic_source_(N, A, SrcHandle, Module) ->
	    recorded(SrcHandle, NormClause)
	;
	    bip_error(clause(Clause), Module)
	).


clause_body(Head, Body, Module) :-
	( check_callable(Head),
	  functor(Head, N, A),
	  dynamic_source_(N, A, SrcHandle, Module) ->
	    recorded(SrcHandle, (Head:-Body))
	;
	    bip_error(clause(Head, Body), Module)
	).


retract_body(Clause, Module) :-
	( clause_info(Clause, N, A, NormClause),
	  dynamic_source_(N, A, SrcHandle, Module) ->
	    recorded(SrcHandle, NormClause, DbRef),
	    ( erase(DbRef) -> true ; true )
	;
	    bip_error(retract(Clause), Module)
	).


retract_all_body(Head, Module) :-
	( check_callable(Head),
	  functor(Head, N, A),
	  dynamic_source_(N, A, SrcHandle, Module) ->
	    erase_all(SrcHandle, Head :- _)@Module
	;
	    bip_error(retract_all(Head), Module)
	).


listing_body(Pred, Module) :-
	( check_predspec(Pred),
	  Pred = N/A,
	  dynamic_source_(N, A, SrcHandle, Module) ->
	    (
		recorded(SrcHandle, Clause)@Module,
		writeclause(Clause)@Module,
		fail
	    ;
		true
	    )
	;
	    bip_error(listing(Pred), Module)
	).


listing_body(Module) :-
	(
	    Pred = N/A,
	    current_predicate_body(Pred, Module),
	    is_dynamic_(N, A, Module),
	    proc_flags(Pred, 0, Module, Module), % definition module = Module
	    listing_body(Pred, Module),
	    nl(output),
	    fail
	;
	    true
	).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% some predicates to output a clause (used in listing)

writeclause_body(C,M):-	
	writeclause_body(output, C, M).

writeclause_body(Stream, Clause, Module) :-
	write_clause(Stream, Clause, Module),
	put_separated(Stream, 0'., Module),
	nl(Stream).

put_separated(Stream, Char, Module) :-
	get_chtab(Char, Class),
	(
	    get_stream_info(Stream, last_written, LastChar),	% may fail
	    get_chtab(LastChar, Class)@Module
	->
	    put(Stream, 0' )
	;
	    true
	),
	put(Stream, Char).

write_clause(Str, (H?-B), M) ?- !,
	write_bracketed_if_needed(Str, H, 1199, M), 
	write_(Str, ' ?-', M), 
	nl(Str),
	write_goal(Str, B, 2, 1199, M).
write_clause(Str, (H:-B), M) ?- !,
	(B == true ->
		writeq_(Str, H, M)
	;
		write_bracketed_if_needed(Str, H, 1199, M), 
		write_(Str, ' :-', M), 
		nl(Str),
		write_goal(Str, B, 2, 1199, M)
	).
write_clause(Str, (H-->B), M) ?- !,
	write_bracketed_if_needed(Str, H, 1199, M), 
	write_(Str, '-->', M), 
	nl(Str),
	write_goal(Str, B, 2, 1199, M).
write_clause(Str, H, M):-
	writeq_(Str, H, M).


write_goal(Str, Term, Indent, M) :-
	write_goal(Str, Term, Indent, 1200, M).

% be careful not to instantiate input arguments!
write_goal(Str, B, Indent, _Prec, M):-
	var(B), !,
	indent(Str, Indent, M),
	writeq_(Str, B, M).
write_goal(Str, (B,C), Indent, _Prec, M):- !,
	write_goal(Str, B, Indent, 999, M), 
	put(Str, 0',), nl(Str),
	write_goal(Str, C, Indent, 1000, M).
write_goal(Str, (IfThen;D), Indent, _Prec, M) :-
	nonvar(IfThen),
	IfThen = (B->C),
	!,
	Ind1 is Indent+1,
	indent(Str, Indent, M),
	put(Str, 0'(), nl(Str),
	write_goal(Str, B, Ind1, 1049, M), 
	nl(Str),
	indent(Str, Indent, M),
	write_(Str, '->', M), 
	nl(Str),
	write_goal(Str, C, Ind1, 1050, M),
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0';), nl(Str),
	write_goal(Str, D, Ind1, 1100, M),
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0')).
write_goal(Str, (B;C), Indent, _Prec, M):- !,
	Ind1 is Indent+1,
	indent(Str, Indent, M),
	put(Str, 0'(), nl(Str),
	write_goal(Str, B, Ind1, 1099, M), 
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0';), nl(Str),
	write_goal(Str, C, Ind1, 1100, M),
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0')).
write_goal(Str, (B->C), Indent, _Prec, M):- !,
	Ind1 is Indent+1,
	indent(Str, Indent, M),
	put(Str, 0'(), nl(Str),
	write_goal(Str, B, Ind1, 1049, M), 
	nl(Str),
	indent(Str, Indent, M),
	write_(Str, '->', M), 
	nl(Str),
	write_goal(Str, C, Ind1, 1050, M),
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0')).
write_goal(Str, (B do C), Indent, _Prec, M):- !,
	Ind1 is Indent+1,
	indent(Str, Indent, M),
	put(Str, 0'(), nl(Str),
	write_goal(Str, B, Ind1, 1099, M), 
	nl(Str),
	indent(Str, Indent, M),
	write_(Str, do, M), 
	nl(Str),
	write_goal(Str, C, Ind1, 1100, M),
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0')).
write_goal(Str, (-?-> B), Indent, _Prec, M):- !,
	indent(Str, Indent, M),
	write_(Str, '-?->', M), 
	nl(Str),
	write_goal(Str, B, Indent, 1179, M).
write_goal(Str, '{}'(B), Indent, _Prec, M):- !,
	Ind1 is Indent+1,
	indent(Str, Indent, M),
	put(Str, 0'{), nl(Str),
	write_goal(Str, B, Ind1, 1200, M), 
	nl(Str),
	indent(Str, Indent, M),
	put(Str, 0'}).
write_goal(Str, B, Indent, Prec, M):-
	indent(Str, Indent, M),
	write_bracketed_if_needed(Str, B, Prec, M).


% this is just to fix the bugs, better code is in the public domain write.pl

write_bracketed_if_needed(Str, Term, MaxPrec, M) :-
	compound(Term),
	functor(Term, F, A),
	current_op_body(Prec, Assoc, F, M),
	atom_length(Assoc) =:= A + 1,	% Functor is an operator
	Prec > MaxPrec,
	!,				% Term might needs brackets
	put(Str, 0'(),
	writeq_(Str, Term, M),
	put(Str, 0')).
write_bracketed_if_needed(Str, Term, _Prec, M) :-
	writeq_(Str, Term, M).


indent(_, 0, _) :- !.
indent(Str, 1, M) :- !,
	write_(Str, '    ', M).	%  write 4 spaces
indent(Str, N, M) :-
	N >= 2, N1 is N-2,
	write_(Str, '\t', M),
	indent(Str, N1, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

?- skipped
	(abolish)/1,
	abolish_body/2,
	clause/1,
	clause/2,
	clause_body/2,
	clause_body/3,
	(dynamic)/1,
	dynamic_body/2,
	is_dynamic/1,
	is_dynamic_body/2,
	(listing)/0,
	(listing)/1,
	listing_body/1,
	listing_body/2,
	retract/1,
	retract_all/1,
	retract_all_body/2,
	retract_body/2,
	writeclause/1,
	writeclause/2,
	writeclause_body/2,
	writeclause_body/3.
