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
% Copyright (C) 1991-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: source_storage.pl,v 1.1 2008/06/30 17:43:49 jschimpf Exp $
% ----------------------------------------------------------------------

%
% IDENTIFICATION:	source_storage.pl (part of modes.pl)
% AUTHOR:		Joachim Schimpf
% PROJECT:		IDLE
%
%
% source program storage
% The source is asserted into the module 'source'
%

:- module(source_storage).
%--------------------------------------------------------

:- use_module(library(apply_macros)).

:- export read_source/1.
:- export get_clauses/2, undefined_predicate/1.

:- import
	assert_/2,
	clause_body/3,
	current_record_body/2,
	file_query/2,
	is_predicate_/2,
	local_record_body/2,
	local_body/2,
	get_flag_body/4,
	recordz_body/3,
	recorded_list_body/3
    from sepia_kernel.
:- current_module(source) -> true ; create_module(source).

source_clause(Head, Body) :-
	clause_body(Head, Body, source).

undefined_predicate(PredSpec) :-
	( get_flag_body(PredSpec, definition_module, source, source) ->
	    fail
	; get_flag_body(PredSpec, declared, on, source) ->
	    fail
	;
	    true
	).

get_clauses(Call, Clauses) :-
	functor(Call, F, N),
	functor(Head, F, N),
	(get_flag_body(F/N, tool, on, source) ->
	    Head =.. HeadL,
	    append(HeadL, [dummy_module], BodyL),
	    Body =.. BodyL,
	    Clauses = [Head :- Body]
	; get_flag_body(F/N, stability, dynamic, source) ->
	    findall((Head :- Body), source_clause(Head, Body), Clauses)
	;
	    printf(error, "*** Warning: No source for predicate %w (assuming worst case)\n%b",
									[F/N]),
	    Clauses = []
	).

/*
get_clauses(Call, Clauses) :-
	functor(Call, F, N),
	(get_flag_body(F/N, tool, on, source) ->
	    Head =.. HeadL,
	    append(HeadL, [dummy_module], BodyL),
	    Body =.. BodyL,
	    Clauses = [Head :- Body]
	;
	    recorded_list_body(Call, Clauses, source)
	).
*/

read_source([H|T]) :-
	!,
	erase_module(source),
	create_module(source),
	local_body((plus/3, trace/1), source),
	read_source0([H|T]).
read_source(File) :-
	read_source([File]).

read_source0([]).
read_source0([H|T]) :-
	read_source1(H),
	read_source0(T).

read_source1(File) :-
	printf("reading %w\n", [File]),
	flush(output),
	open_source_file(File, Stream),
	read(Stream, Term),
	process_input_term(Stream, Term).


process_input_term(Stream, end_of_file) :- !,
	close(Stream).
process_input_term(Stream, (:- Query)) :- !,
	( additional_execute(Query) ->
	    call(Query)@source
	;
	    file_query(Query, read_source1(_))
	),
	read(Stream, Term),
	process_input_term(Stream, Term).
process_input_term(Stream, (?- Query)) :- !,
	( additional_execute(Query) ->
	    call(Query)@source
	;
	    file_query(Query, read_source1(_))
	),
	read(Stream, Term),
	process_input_term(Stream, Term).
process_input_term(Stream, (Head :- Body)) :- !,
	preprocess_control([(Head :- Body)], Clause1),
%	Clause1 = (Head :- Body),
	assert_pp_list(Clause1),
	read(Stream, Term),
	process_input_term(Stream, Term).
process_input_term(Stream, Fact) :-
	preprocess_control([(Fact :- true)], Clause1),
%	Clause1 = (Fact :- true),
	assert_pp_list(Clause1),
	read(Stream, Term),
	process_input_term(Stream, Term).

additional_execute(local(_)).
additional_execute(dynamic(_)).

%local_recordz_body(Key, Value, Module) :-
%	( current_record_body(Key, source) ->
%	    true
%	;
%	    functor(Key, F, N),
%	    local_record_body(F/N, source)
%	),
%	recordz_body(Key, Value, Module).

assert_pp_list([]).
assert_pp_list([Clause|Clauses]) :-
	preprocess_aliasing(Clause, Clause1),
	assert(Clause1)@source,
	assert_pp_list(Clauses).

open_source_file(File, Stream) :-
        (string(File) ->                        % first convert to a string
                FileS = File
        ; atom(File) ->
                atom_string(File, FileS)
	),
	(
                get_flag(prolog_suffix, Suffixes),
                member(Suffix, Suffixes),
                concat_strings(FileS, Suffix, PlFile)
        ),
	exists(PlFile),
        !,
	open(PlFile, read, Stream).
open_source_file(File, _Stream) :-
	printf(error, "*** Cannot open source file %w\n%b", [File]),
	fail.


preprocess_aliasing((Head :- Body), (NewHead :- NewBody)) :-
	preprocess_body(Body, NewBody),
	mark_aliases(Head, NewHead).


preprocess_body(Goal, Goal) :-
	var(Goal), !,
	printf(error, "*** possible problem: variable goal in %w\n%b", [Goal]).
preprocess_body((Goals1 , Goals2), (NewGoals1 , NewGoals2)) :- !,
	preprocess_body(Goals1, NewGoals1),
	preprocess_body(Goals2, NewGoals2).
preprocess_body(X=Y, X=Y) :- !.		% handled in the interpreter
preprocess_body(Goal, NewGoal) :-
	mark_aliases(Goal, NewGoal).


mark_aliases(Term, MarkedTerm) :-
	critical_variables(Term, Vars),
	( Vars == [] ->
	    Term = MarkedTerm
	;
	    mapargs(mark_alias(Vars), Term, MarkedTerm)
	).


mark_alias(Vars, Term, MarkedTerm) :-
	var(Term), !,
	( occurs(Term, Vars) ->
	    MarkedTerm = '$alias'(Term)
	;
	    MarkedTerm = Term
	).
mark_alias(Vars, Term, MarkedTerm) :-
	( compound(Term) ->
	    mapargs(mark_alias(Vars), Term, MarkedTerm)
	;
	    Term = MarkedTerm
	).


% critical_variables(+Term, -Vars)
%
% finds the variables which occur multiply in Term

critical_variables(Term, Vars) :-
	var(Term), !,
	printf(error, "*** possible problem: variable goal in %w\n%b", [Term]),
	Vars = [].			% wrong: metacalled variable
critical_variables(Term, Vars) :-
	copy_term(Term, Copy),
	functor(Term, _, A),
	critical_variables(A, Term, Copy, 0, _, [], Vars).

critical_variables(0, _, _, N, N, Vars, Vars) :- !.
critical_variables(A, Term, Copy, N0, N, Vars0, Vars) :-
	A1 is A-1,
	arg(A, Copy, Carg),
	arg(A, Term, Targ),
	check_arg(Carg, Targ, N0, N1, Vars0, Vars1),
	critical_variables(A1, Term, Copy, N1, N, Vars1, Vars).


check_arg('$ARG'(N0), _Targ, N0, N, Vars, Vars) :- !, N is N0+1.	% new variable
check_arg('$ARG'(_), Targ, N, N, Vars0, [Targ|Vars0]) :- !.		% already seen
check_arg(Carg, Targ, N0, N, Vars0, Vars) :-
	functor(Carg, _, Ar),
	critical_variables(Ar, Targ, Carg, N0, N, Vars0, Vars).


%-------------------------------------------------------------------
% Transform clauses into disjunction-free form.
% Also eliminate: \+ not fail_if call once.
% We don't care about cuts here, since the analyser
% ignores them anyway.
%-------------------------------------------------------------------

:- setval(aux_counter, 0).

preprocess_control(OldClauses, NewClauses) :-
	eliminate_disj(OldClauses, [], NewClauses).

eliminate_disj([], NewClauses, NewClauses).
eliminate_disj([OldClause|OldClauses], NewClauses0, [NewClause|NewClauses1]) :-
	eliminate_disj(OldClause, NewClause, [], AuxClauses),
	eliminate_disj(AuxClauses, NewClauses2, NewClauses1),
	eliminate_disj(OldClauses, NewClauses0, NewClauses2).

eliminate_disj((Head :- Body), NewClause, AuxCl0, AuxCl) :-
	comma_to_list(Body, BodyList, []),
	( split_body(BodyList, LBody, Disj, RBody) ->
	    collect_vars(Head, [], Vars0),
	    collect_vars(LBody, Vars0, Vars1),
	    collect_vars(RBody, Vars1, OuterVars),
	    collect_vars(Disj, [], DisjVars),
	    common_vars(OuterVars, DisjVars, [], AuxVars),
	    incval(aux_counter), getval(aux_counter, N),
	    concat_atom(['$disj_',N], AuxName),
	    AuxPred =.. [AuxName|AuxVars],
	    append(LBody, [AuxPred|RBody], NewBodyList),
	    list_to_comma(NewBodyList, NewBody),
	    disj_to_clauses(Disj, AuxPred, AuxCl1, AuxCl),
	    eliminate_disj((Head :- NewBody), NewClause, AuxCl0, AuxCl1)
	;
	    list_to_comma(BodyList, NewBody),
	    NewClause = (Head :- NewBody),
	    AuxCl0 = AuxCl
	).

vars(X, Vars, [X|Vars]) :- var(X), !.
vars(_, Vars, Vars).

collect_vars(Term, Vars0, Vars) :-
	sumnodes(vars, Term, Vars0, Vars).

comma_to_list(Goal, [Goal|List], List) :-
	var(Goal), !.
comma_to_list((LGoals , RGoals), List0, List) :- !,
	comma_to_list(LGoals, List0, List1),
	comma_to_list(RGoals, List1, List).
comma_to_list((LGoals -> RGoals), List0, List) :- !,
	comma_to_list(LGoals, List0, List1),
	comma_to_list(RGoals, List1, List).
comma_to_list((\+ Goal), [(Goal,fail;true)|List], List) :- !.
comma_to_list(not(Goal), [(Goal,fail;true)|List], List) :- !.
comma_to_list(fail_if(Goal), [(Goal,fail;true)|List], List) :- !.
comma_to_list(call(Goal), [Goal|List], List) :- !.
comma_to_list(call(Goal,_), [Goal|List], List) :- !.
comma_to_list(once(Goal), [Goal|List], List) :- !.
comma_to_list(once(Goal,_), [Goal|List], List) :- !.
comma_to_list(findall(T, G, L), [(G,fail;true),'$findall'(T, G, L)|List], List) :- !.
comma_to_list(bagof(T, G, L), [(G,fail;true),'$bagof'(T, G, L)|List], List) :- !.
comma_to_list(setof(T, G, L), [(G,fail;true),'$setof'(T, G, L)|List], List) :- !.
comma_to_list(coverof(T, G, L), [(G,fail;true),'$coverof'(T, G, L)|List], List) :- !.
comma_to_list(block(G,T,R), [(G;R),'$block'(G,T,R)|List], List) :- !.
comma_to_list(block(G,T,R,_), [(G;R),'$block'(G,T,R)|List], List) :- !.

comma_to_list(gc_block_once(G,T,R), [(G;R),'$block'(G,T,R)|List], List) :- !.
comma_to_list(gc_block_once(G,T,R,_), [(G;R),'$block'(G,T,R)|List], List) :- !.
comma_to_list(gc_once(Goal), [Goal|List], List) :- !.
comma_to_list(gc_once(Goal,_), [Goal|List], List) :- !.
comma_to_list(gc_prove(Goal), [Goal|List], List) :- !.
comma_to_list(gc_prove(Goal,_), [Goal|List], List) :- !.

comma_to_list(Goal, [Goal|List], List).

list_to_comma([], true).
list_to_comma([G], G) :- !.
list_to_comma([G|T], (G,Gs)) :-
	list_to_comma(T, Gs).

% split_body(Subgoals, LeftSubgoals, Disj, RightSubgoals)
% find the leftmost disjunction and split the body in three parts

split_body([Goal|Goals], LGoals, Disj, RGoals) :-
	nonvar(Goal),
	Goal = (_;_) ->
	    LGoals = [], Disj = Goal, RGoals = Goals
	;
	    LGoals = [Goal|LGoals0],
	    split_body(Goals, LGoals0, Disj, RGoals).

% common_vars(VarList1, VarList2, CommonIn, CommonOut)

common_vars([], _, Common, Common).
common_vars([V|Vs], Ws, Common0, Common) :-
	    occurs(V, Ws),
	    \+ occurs(V, Common0)
	->
	    common_vars(Vs, Ws, [V|Common0], Common)
	;
	    common_vars(Vs, Ws, Common0, Common).

% disj_to_clauses(Disjunction, AuxHead, ClausesIn, ClausesOut)
% make clauses from a disjunction, using the given head

disj_to_clauses((LGoals ; RGoals), Head, Clauses0, Clauses) :- !,
	disj_to_clauses(LGoals, Head, Clauses1, Clauses),
	disj_to_clauses(RGoals, Head, Clauses0, Clauses1).
disj_to_clauses(Goals, Head, Clauses, [(Head :- Goals)|Clauses]).

