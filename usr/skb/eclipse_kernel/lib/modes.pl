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
% Version:	$Id: modes.pl,v 1.2 2008/07/20 18:16:51 jschimpf Exp $
% ----------------------------------------------------------------------

%
% IDENTIFICATION:	modes.pl
% AUTHOR:		Joachim Schimpf
% PROJECT:		IDLE
%
% An abstract interpreter for mode ananlysis.
%
% The abstract interpreter uses dataflow-driven computation with
% streams to do the fixpoint iteration, which is based on the ideas of
%
% "An Implementation Technique for the Abstract Interpretation of Prolog"
% by Annika Waern (SICS), ICLP 88
%
% The abstract terms are represented with ECLiPSe metaterms. 
% For a description of the abstract domain see below.
%

%--------------------------------------------------------
:- module(modes).
%--------------------------------------------------------

:- comment(summary, "An abstract interpreter for mode ananlysis").
:- comment(author, "Joachim Schimpf, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/07/20 18:16:51 $").
:- comment(desc, html("
    This library provides a static mode analysis tool for ECLiPSe
    programs.  It takes as input a Prolog program and a goal pattern, and
    performs an analysis of the modes that the predicates in the program
    will be called with, given the specified goal pattern.  The mode
    analyser is loaded with
    <PRE>
	:- lib(modes).
    </PRE>
    The usage is simple.  The predicate read_source/1 is used to read in
    the source file to be analysed.  The predicate analyze/1 starts the
    analysis with a given call pattern and prints the results in the form
    of a compilable mode declaration:
    <PRE>
    [eclipse 2]: read_source(qsort).
    reading qsort

    yes.
    [eclipse 3]: analyze(qsort(++,-)).

    % Analysed 3 call patterns of 3 predicates in 0.05 sec
    % Number of pending goals: 29
    % Global Stack used:       12892
    :- mode partition(++, ++, -, -).
    :- mode qsort(++, -).
    :- mode qsort(++, -, ++).

    yes.
    [eclipse 4]: analyze(qsort(+,-)).

    % Analysed 4 call patterns of 3 predicates in 0.13 sec
    % Number of pending goals: 54
    % Global Stack used:       35968
    :- mode partition(?, ?, -, -).
    :- mode qsort(+, -).
    :- mode qsort(+, -, +).

    yes.
    </PRE>
    Restrictions: Programs that use coroutining features cannot be analysed. 
    ")).

:- export analyze/1, analyze_exits/1.


:- import get_clauses/2, undefined_predicate/1 from source_storage.

:- pragma(nodebug).

:- use_module(library(apply_macros)).
:- setval(collect_builtin_modes, off).

:- meta_attribute(modes, [ copy_term:copy_mode_attr/2 ]).

:- import replace_attribute/3 from sepia_kernel.

:- local merge/3.

%--------------------------------------------------------

analyze(Call) :-
	analyze(Call, Lookup),
	lookup_to_list(Lookup, LookupList),
	keysort(LookupList, SortedLookupList),
	print_entry_lubs(SortedLookupList).

analyze_exits(Call) :-
	analyze(Call, Lookup),
	lookup_to_list(Lookup, LookupList),
	keysort(LookupList, SortedLookupList),
	print_entry_exits(SortedLookupList).

analyze(Call, Lookup) :-
	check_call_arguments(Call),
	setval(calls,0),
	setval(predicates,0),
	cputime(T0),
	% Use subcall/2 to get rid of the delayed goals
	block(
	    subcall(prove_goal(Call, Lookup, _Exit), Residue),
	    Tag,
	    printf("*** %w\n", [Tag])
	),
	T is cputime-T0,
	statistics(global_stack_used, G),
	length(Residue, GoalNr),
	getval(calls, N),
	getval(predicates, P),
	printf("\n%% Analysed %d call patterns of %d predicates in %.2f sec\n",
		[N, P, T]),
	printf("%% Number of pending goals: %d\n", [GoalNr]),
	printf("%% Global Stack used:       %d\n", [G]).

check_call_arguments(Call) :-
	Call =.. [_|L],
	check_argument_list(L).

check_argument_list([]).
check_argument_list([M|L]) :-
	(nonvar(M), pattern(M) ->
		true
	;
	    printf(error, "*** Bad call argument: %w\n%b", [M]),
	    abort
	),
	check_argument_list(L).

collect_keys(T, []) :- var(T), !.
collect_keys([], []).
collect_keys([K-_|Ps], [K|Ks]) :-
	collect_keys(Ps, Ks).

add_functor_keys([], []).
add_functor_keys([H|T], [F-H|FT]) :-
	functor(H, F, _),
	add_functor_keys(T, FT).

print_modes(Modes) :-
	add_functor_keys(Modes, Keyed),
	keysort(Keyed, KeySorted),
	print_values(KeySorted).

print_values([]).
print_values([_-Mode|T]) :-
	printf(":- mode %QDvOw", Mode), writeln(.),
	print_values(T).

print_entry_lubs([]).
print_entry_lubs([_ - info(Entries, _Exits)|T]) :-
	print_entry_lub(Entries),
	print_entry_lubs(T).

print_entries(X) :-
	var(X), !.
print_entries([H|T]) :-
	writeln(H),
	print_entries(T).

print_entry_lub(Entries) :-
	get_lub(Entries, '$bottom$', Lub),
	(
	    compound(Lub),
	    \+ aux_predicate(Lub)
	->
	    printf(":- mode %QDvOw", Lub),
	    writeln(.)
	;
	    true
	).

print_entry_exits([]).
print_entry_exits([_ - info(Entries, Exits)|T]) :-
	print_entries(Entries),
	print_exits(Exits),
	print_entry_exits(T).

print_exits([]).
print_exits([H|T]) :-
	put(output, 0'	),
	writeln(H),
	print_exits(T).

aux_predicate(Call) :-
	functor(Call, Name, _),
	atom_string(Name, NameS),
	substring(NameS, "$disj_", 1).

%--------------------------------------------------------
% the abstract interpreter
%--------------------------------------------------------

prove_goal(Call, Lookup, ExitStream) :-
	functor(Call, F, A),
	prove_goal(Call, Lookup, ExitStream, F/A).

prove_goal(Call, Lookup, ExitStream, PredSpec) :-
	undefined_predicate(PredSpec),
	builtin(Call, ExitStream), !,
	( getval(collect_builtin_modes, on) ->
	    lookup_or_enter(Lookup, PredSpec, Info),
	    ( var(Info) ->
		RawEntryStream = [Call|_],
		stream_to_lubstream('$bottom$', RawEntryStream, _EntryStream),
		Info = info(RawEntryStream, _)
	    ;
		Info = info(RawEntryStream, not_used),
		get_tail(RawEntryStream, RawEntryT),
		RawEntryT = [Call|_]
	    )
	;
	    true
	).
prove_goal(Call, Lookup, ExitStream, PredSpec) :-
	lookup_or_enter(Lookup, PredSpec, Info),
	( var(Info) ->				% new predicate
	    RawEntryStream = [Call|_],
	    stream_to_lubstream('$bottom$', RawExitStream, ExitStream),
	    stream_to_lubstream('$bottom$', RawEntryStream, EntryStream),
	    Info = info(RawEntryStream, ExitStream),
%	    writeln('New predicate' - PredSpec),
	    incval(predicates),
	    prove_call(EntryStream, Lookup, RawExitStream)
	;
	    Info = info(RawEntryStream, WholeExitStream),
	    get_tail(RawEntryStream, RawEntryT),
	    RawEntryT = [Call|_],				% order?
	    get_last_sofar(WholeExitStream, ExitStream),	% order?
	    true
	).


delay prove_call(CallStream, _, _) if var(CallStream).

prove_call([], _, []).
prove_call([Call|Calls], Lookup, RawExitStream) :-
%	writeln(Call),
	incval(calls),
	get_clauses(Call, Clauses),
	merge(ExitStream1, ExitStream2, RawExitStream),
	prove_clauses(Clauses, Call, ExitStream1, Lookup),	% order ?
	prove_call(Calls, Lookup, ExitStream2).


% prove_clauses(Clauses, Call, ExitStream, Lookup)
% apply the call pattern Call to all Clauses in the list Clauses,
% constructions a stream of exit patterns ExitStream
%
% If Clauses is [], then we have no source information (e.g. dynamic predicate)
% and we use the worst case exit modes (by unifying every argument with ?).

prove_clauses([], Call, [Exit], _) :-
	mapargs(=(?), Call, Exit).
prove_clauses([Clause|Clauses], Call, ExitStream, Lookup) :-
	(Clauses == [] ->
	    prove_clause(Clause, Call, ExitStream, Lookup)
	;
	    merge(ExitStream1, ExitStream2, ExitStream),
	    prove_clause(Clause, Call, ExitStream1, Lookup),
	    prove_clauses(Clauses, Call, ExitStream2, Lookup)
	).

prove_clause((Head :- Body), Call, ExitStream, Lookup) :-
	pattern_to_term(Call, Head),
	prove_subgoal(Lookup, Body, Head, EnvStream),
	mapstream(term_to_pattern, EnvStream, ExitStream).


% prove_subgoal(Lookup, Goal-Env, EnvStream)
% prove_subgoal(Lookup, Goal, Env, EnvStream)
%
% Interpret the goal Goal in the environment Env, generating a stream
% EnvStream of possible instances of Env (resulting from the execution
% of Goal). Goal shares variables with Env, Env is usually the rest
% of the clause that contains Goal.

prove_subgoal(Lookup, Goal-Env, EnvStream) :-
	prove_subgoal(Lookup, Goal, Env, EnvStream).

prove_subgoal(_Lookup, Goal, Env, EnvStream) :- var(Goal), !,
	writeln(error, "*** Problem: Metacalled goal unknown at analysis time"),
	EnvStream = [Env].	% ignore metacalled abstract terms (wrong!)
prove_subgoal(_Lookup, true, Env, EnvStream) :- !,
	EnvStream = [Env].
prove_subgoal(Lookup, (Goal1 , Goal2), Env, EnvStream) :- !,
	prove_subgoal(Lookup, Goal1, Goal2-Env, Env1Stream),
	merge_stream_of_streams(Env2StreamStream, EnvStream),
	mapstream(prove_subgoal(Lookup), Env1Stream, Env2StreamStream).
/******
prove_subgoal(_Lookup, Term1=Term2, Env, EnvStream) :- !,
	(unify(Term1, Term2) ->
	    EnvStream = [Env]
	;
	    EnvStream = []
	).
*******/
prove_subgoal(Lookup, Goal, Env, EnvStream) :-
	term_to_pattern(Goal, Call),
%	writeln(Call),
	prove_goal(Call, Lookup, GoalExitStream),
	mapstream(make_alt_envs(Goal, Env), GoalExitStream, EnvStream).
%	mapstream(update_env(Goal, Env), GoalExitStream, EnvStream).


% make a copy of Env, instantiated according to GoalExit
%
% (To avoid keeping substitutions explicit, we store copies
% of clause parts with the current instantiations)

make_alt_envs(Goal, Env, GoalExit, NewEnv) :-
%	put(9), writeln(GoalExit),
	copy_term([Goal|Env], [GoalCopy|NewEnv0]),
	pattern_to_term(GoalExit, GoalCopy),
	NewEnv = NewEnv0.		% propagate only now

% when the exitstream is lub'd, we can update the old environment
% NO! because this will cause left-propagation of bindings, i.e.
% we might evaluate new alternatives to a left hand goal using
% instantiations from a right hand one.
%
% update_env(Goal, Env, GoalExit, NewEnv) :-
%	put(9), writeln(GoalExit),
%	(pattern_to_term(GoalExit, Goal) ->
%		true
%	;
%		writeln(unexpected_failure(pattern_to_term(GoalExit, Goal)))
%	),
%	NewEnv = Env.


%--------------------------------------------------------
% Handling of abstract and mixed terms
%
% Abstract terms are represented by metaterms whose attribute is the mode.
%
% Aliasing: Variables which have multiple occurrences in the same
% head/subgoal, are marked with '$alias'/1 by the source preprocessor.
% To achieve safe handling of aliasing, we turn '$alias'({-})
% into ? rather than - .
%--------------------------------------------------------

is_moded(_{Attr}) :- -?->
	nonvar(Attr),
	!.

get_mode(_{Attr}, Mode) :- -?->
	nonvar(Attr),
	Mode = Attr.

copy_mode_attr(_{Attr}, Copy) :- -?-> 
	add_attribute(Copy, Attr).


% pattern_to_term(Call, Head) or pattern_to_term(Exit, SubGoal)
%
% convert a call/exit pattern into a mixed representation goal term

pattern_to_term(Abstr, Term) :-
	mapargs(abstraction_to_term, Abstr, Term).


% convert an abstract descriptor into a mixed representation term
%
% the !(P) solutions is unsatisfactory, it works only for builtin returns.
% Better: don't use unification for exit-patterns, but overwriting.
% In that case be careful, so that builtin descriptions always transfer
% the call mode properly to the exit mode in case the arg is not affected.

abstraction_to_term(!(P), X) :- !,
	( is_moded(X) ->
	    replace_attribute(X, P, modes)
	;
	    unify_pattern_any(P, X)
	).
abstraction_to_term(P, X) :-
	unify_pattern_any(P, X).

% convert a Goal in mixed representation into its abstract pattern form

term_to_pattern(Call, Pattern) :-
	mapargs(term_to_abstraction, Call, Pattern).


% convert a term in mixed representation into its abstract descriptor

term_to_abstraction(X, P) :-
	var(X), !,
	( get_mode(X, P) ->
	    true
	;
	    P = -
	).
term_to_abstraction('$alias'(X), P) :- !,
	term_to_abstraction(X, P0),
	aliasing_effect(P0, P).
term_to_abstraction(X, P) :-
	sumargs(compose_abstraction, X, ++, P).

compose_abstraction(X, P0, P) :-
	term_to_abstraction(X, Px),
	composition(P0, Px, P).

/***********
%
% unify two mixed abstract/concrete terms
%
% (this should be done by directly unifying the two terms and defining
%  the proper handlers for the metaterms. The only thing that currently
%  prevents this is the '$alias'/1 wrapper. If aliasing is represented
%  with metaterm information as well, it can be done.)

unify(X, Y) :-
	var(X), !,
	( meta(X) ->
	    unify_abstr_any(X, Y)
	;
	    X=Y
	).
unify('$alias'(X), Y) :- !,
	unify(X, Y).
unify(X, Y) :-
	unify_concr_any(X, Y).

unify_concr_any(X, Y) :-
	var(Y), !,
	( meta(Y) ->
	    unify_abstr_concr(Y, X)
	;
	    X=Y
	).
unify_concr_any(X, '$alias'(Y)) :- !,
	unify_concr_any(X, Y).
unify_concr_any(X, Y) :-
	unify_concr_concr(X, Y).

unify_concr_concr(X, Y) :-
	(compound(X) ->
	    functor(X, F, A),
	    functor(Y, F, A),		% this can fail !
	    mapargs(unify, X, Y)
	;
	    X=Y				% this can fail !
	).

unify_abstr_any(X, Y) :-
	var(Y), !,
	( meta(Y) ->
	    unify_abstr_abstr(X, Y)
	;
	    X=Y
	).
unify_abstr_any(X, '$alias'(Y)) :- !,
	unify_abstr_any(X, Y).
unify_abstr_any(X, Y) :-
	unify_abstr_concr(X, Y).

unify_abstr_abstr(X, Y) :-
	meta_term(X, Xa),
	meta_term(Y, Ya),
	=(Xa, Ya, Za),
	meta_term(Z, Za),
	meta_bind(X, Z),
	meta_bind(Y, Z).

unify_abstr_concr(X, Y) :-
	meta_term(X, Xa),
	unify_pattern_any(Xa, Y),
	term_to_abstraction(Y, Ya),	% this doesn't look quite right ...
	=(Xa, Ya, Za),
	meta_term(Z, Za),
	meta_bind(X, Z).

**********/

unify_pattern_any(Xp, Y) :-		% can't fail
	var(Y), !,
	( get_mode(Y, Ya) ->
	    =(Xp, Ya, Za),
	    replace_attribute(Y, Za, modes)
	;
	    add_attribute(Y, Xp)	% only here we _create_ meta terms!
	).
unify_pattern_any(Xp, '$alias'(Y)) :- !,
	unify_pattern_any(Xp, Y).
unify_pattern_any(P, Y) :-
	compound(Y), !,
	functor(Y, _, N),
	decomposition(P, Parg),
	unify_pattern_any(N, Y, Parg).
unify_pattern_any(_, _).

unify_pattern_any(0, _, _) :- !.
unify_pattern_any(N, Y, P) :-
	arg(N, Y, Arg),
	unify_pattern_any(P, Arg),
	N1 is N-1,
	unify_pattern_any(N1, Y, P).


%--------------------------------------------------------
%
% The abstract domain
%
% Abstract domain partial ordering:
%
%	     ?
%           / \
%	  -+   +
%         / \ /
%	 /   +-
%       -    |
%	 \   ++
%         \ /
%	$bottom$
%
% ++	ground term
% +	non-variable
% -	uninstantiated variable
% +-	non-variable term without internal shared variables
% -+	- or +-
% ?	+ or -, ie any term
%
%--------------------------------------------------------

% pattern(P)
%
% the valid abstract term descriptors

pattern(-).
pattern(++).
pattern(+-).
pattern(-+).
pattern(+).
pattern(?).


% composition(P0, Pn, P)
%
% If an argument of type Pn is added as an additional argument to
% a structure of type P0, then the type of the new structure is P

composition(++, ++, ++) :- !.	% groundness preservation
composition(++, +-, +-) :- !.
composition(++, -+, +-) :- !.
composition(++, -, +-) :- !.
composition(+-, ++, +-) :- !.	% non-sharing preservation
composition(+-, +-, +-) :- !.
composition(+-, -+, +-) :- !.
composition(+-, -, +-) :- !.
composition(_, _, +).		% otherwise: worst case for compound terms


% decomposition(P, Parg)
%
% If a structure has type P, its arguments are of type Parg

decomposition(-, -).
decomposition(++, ++).
decomposition(+-, -+).
decomposition(-+, -+).
decomposition(+, ?).
decomposition(?, ?).


% aliasing_effect(P0, P)

aliasing_effect(-, ?) :- !.
aliasing_effect(-+, ?) :- !.
aliasing_effect(+-, +) :- !.
aliasing_effect(P, P).


% lub(PX, PY, PLub)
%
% least upper bound of 2 modes (cf. lattice above)

lub(-, Y, LUB) :- 'lub-'(Y, LUB).
lub(++, Y, LUB) :- 'lub++'(Y, LUB).
lub(+-, Y, LUB) :- 'lub+-'(Y, LUB).
lub(-+, Y, LUB) :- 'lub-+'(Y, LUB).
lub(+, Y, LUB) :- 'lub+'(Y, LUB).
lub(?, _, ?).

'lub-'(-, -).
'lub-'(++, -+).
'lub-'(+-, -+).
'lub-'(-+, -+).
'lub-'(+, ?).
'lub-'(?, ?).

'lub+'(-, ?).
'lub+'(++, +).
'lub+'(+-, +).
'lub+'(-+, ?).
'lub+'(+, +).
'lub+'(?, ?).

'lub++'(-, -+).
'lub++'(++, ++).
'lub++'(+-, +-).
'lub++'(-+, -+).
'lub++'(+, +).
'lub++'(?, ?).

'lub+-'(-, -+).
'lub+-'(++, +-).
'lub+-'(+-, +-).
'lub+-'(-+, -+).
'lub+-'(+, +).
'lub+-'(?, ?).

'lub-+'(-, -+).
'lub-+'(++, -+).
'lub-+'(+-, -+).
'lub-+'(-+, -+).
'lub-+'(+, ?).
'lub-+'(?, ?).

%
% =(PX, PY, PResult)
%
% unification of two abstract terms
%

=(-, Y, Y).
=(++, _, ++).
=(+-, Y, Z) :-	'+- ='(Y,Z).
=(-+, Y, Z) :-	'-+ ='(Y,Z).
=(+, Y, Z) :-	'+ ='(Y,Z).
=(?, Y, Z) :-	'? ='(Y,Z).

'+ ='(-, +).
'+ ='(++, ++).
'+ ='(+-, +).
'+ ='(-+, +).
'+ ='(+, +).
'+ ='(?, +).

'+- ='(-, +-).
'+- ='(++, ++).
'+- ='(+-, +-).
'+- ='(-+, +-).
'+- ='(+, +).
'+- ='(?, +).

'-+ ='(-, -+).
'-+ ='(++, ++).
'-+ ='(+-, +-).
'-+ ='(-+, -+).
'-+ ='(+, +).
'-+ ='(?, ?).

'? ='(-, ?).
'? ='(++, ++).
'? ='(+-, +).
'? ='(-+, ?).
'? ='(+, +).
'? ='(?, ?).


%--------------------------------------------------------
% operations on streams
%--------------------------------------------------------

delay merge_stream_of_streams(StreamStream, _) if var(StreamStream).

merge_stream_of_streams([], []).
merge_stream_of_streams([SubStream|SubStreams], Merged) :-
	merge(SubStream, MergedRest, Merged),
	merge_stream_of_streams(SubStreams, MergedRest).


% merge streams keeping duplicates

delay merge(Stream1, Stream2, _) if var(Stream1), var(Stream2).

merge(Stream1, Stream2, MergedTail) :-
	var(Stream2) ->
	    merge1(Stream1, Stream2, MergedTail)
	;
	    merge1(Stream2, Stream1, MergedTail).

merge1([], Stream2, Stream2).
merge1([New|Tail], Stream2, [New|MergedTail1]) :-
	merge(Tail, Stream2, MergedTail1).


get_last(Rest, X, Last) :-
	var(Rest), !,
	X = Last.
get_last([], X, X).
get_last([H|T], _, Last) :-
	get_last(T, H, Last).

get_last_sofar(List, Last) :-
	var(List), !,
	List = Last.
get_last_sofar(List, Last) :-
	get_last(List, Last).

get_last(List, Last) :-
	var(List), !,
	writeln(problem is get_last(List, Last)),
	abort.
get_last(List, Last) :-
	List = [_|T],
	(var(T) ->
	    Last = List
	;
	    get_last(T, Last)
	).

get_tail(List, Tail) :-
	var(List), !,
	List = Tail.
get_tail([_|T], Tail) :-
	get_tail(T, Tail).

get_lub(List, Lub, Lub) :-
	var(List), !.
get_lub([H|T], Lub0, Lub) :-
	lub_pattern(Lub0, H, Lub1),
	get_lub(T, Lub1, Lub).
	


delay stream_to_lubstream(_OldLub, Stream, _LubStream) if var(Stream).

stream_to_lubstream(_OldLub, [], []).
stream_to_lubstream(OldLub, [H|T], LubT) :-
	lub_pattern(OldLub, H, NewLub),
	( OldLub == NewLub ->
	    stream_to_lubstream(OldLub, T, LubT)
	;
	    LubT = [NewLub|NewLubT],
	    stream_to_lubstream(NewLub, T, NewLubT)
	).


lub_pattern('$bottom$', T2, Tlub) :- !,
	T2 = Tlub.
lub_pattern(T1, T2, Tlub) :-
	functor(T1, F, N),
	functor(T2, F, N),
	functor(Tlub, F, N),
	lub_pattern(N, T1, T2, Tlub).

lub_pattern(0, _, _, _) :- !.
lub_pattern(N, T1, T2, Tlub) :-
	arg(N, T1, A1),
	arg(N, T2, A2),
	arg(N, Tlub, Alub),
	lub(A1, A2, Alub),
	N1 is N-1,
	lub_pattern(N1, T1, T2, Tlub).


%--------------------------------------------------------
% lookup table
%	table(Key, Value, Left, Right)
%
% Key = Name/Arity
% Value = info(EntryStream, ExitStream)
%--------------------------------------------------------


new_lookup(_).

lookup_or_enter(Table, Key, Value) :-
	var(Table), !,
	Table = table(Key, Value, _, _).
lookup_or_enter(Table, Key, Value) :-
	Table = table(K, _, _, _),
	compare(Relation, Key, K),
	descend(Relation, Key, Value, Table).

descend(=, _, Value, table(_, Value, _, _)).
descend(<, Key, Value, table(_, _, L, _)) :-
	lookup_or_enter(L, Key, Value).
descend(>, Key, Value, table(_, _, _, R)) :-
	lookup_or_enter(R, Key, Value).

lookup_to_list(Table, List) :-
	lookup_to_list(Table, List, []).

lookup_to_list(Table, List, Tail) :-
	var(Table), !,
	List = Tail.
lookup_to_list(table(K, V, L, R), List, Tail) :-
	lookup_to_list(L, List, [K-V|Rlist]),
	lookup_to_list(R, Rlist, Tail).


%--------------------------------------------------------
% some builtin descriptions
%
% specify the exit modes (success patterns) that will result
% from the given input mode. [] means it will surely fail.
% - be careful not to weaken the call modes! Use =/3 to avoid this.
% - it helps the analysis when the exit patterns are free of
%   instantiation fault patterns
% - in case one argument is not bound by the builtin, use '-' in the exit mode
%   This means that the caller argument is not affected
% - The form !(<mode>) can be used to specify that the exit mode of an input
%   argument can be reduced with respect to the call mode, but it is not
%   possible to specify by unification, e.g. var(?) ---> var(!(-))
%   (exit unification would not be able to change ? to something else)
% - take care of builtins that can cause aliasing, currently:
%	=/2, =../2, arg/3, sort/2, keysort/2
%   library predicates:
%	member/2, delete/3
%   they must never return -, -+ or +- !
%
% (=/2 should be handled directly by the abstract interpreter above)
%--------------------------------------------------------

:- mode builtin(++, ?).

builtin(X = Y,			[Z = Z]) :- =(X,Y,Z0), aliasing_effect(Z0,Z).
builtin(X \= Y,			[X \= Y]).
builtin(X == Y,			[X == Y]).	% could be handled better
builtin(X \== Y,		[X \== Y]).
builtin(X @< Y,			[X @< Y]).
builtin(X @> Y,			[X @> Y]).
builtin(X @=< Y,		[X @=< Y]).
builtin(X @>= Y,		[X @>= Y]).

builtin(!,			[!]).
builtin(repeat,			[repeat]).
builtin(fail,			[]).
builtin(abort,			[]).

% The metacalling builtins are assumed to be preprocessed to cover
% the calling aspect. Here we describe only the instantiation effect.
builtin('$findall'(T,G,L),	['$findall'(T,G1,L1)]) :-
	=(+, G, G1), =(+, L, L1).
builtin('$bagof'(T,G,L),	['$bagof'(T,G1,L1)]) :-
	=(+, G, G1), =(+, L, L1).
builtin('$setof'(T,G,L),	['$setof'(T,G1,L1)]) :-
	=(+, G, G1), =(+, L, L1).
builtin('$coverof'(T,G,L),	['$coverof'(T,G1,L1)]) :-
	=(+, G, G1), =(+, L, L1).
builtin('$block'(G,_,R),	['$block'(G,?,R)]).

builtin(var(++),		[]) :- !.
builtin(var(+),			[]) :- !.
builtin(var(+-),		[]) :- !.
builtin(var(_),			[var(!(-))]).
builtin(nonground(++),		[]) :- !.
builtin(nonground(_),		[nonground(-)]).
builtin(nonvar(-),		[]) :- !.
builtin(nonvar(-+),		[nonvar(+-)]) :- !.
builtin(nonvar(?),		[nonvar(+)]) :- !.
builtin(nonvar(_),		[nonvar(-)]).
builtin(atom(-),		[]) :- !.
builtin(atom(_),		[atom(++)]).
builtin(atomic(-),		[]) :- !.
builtin(atomic(_),		[atomic(++)]).
builtin(number(-),		[]) :- !.
builtin(number(_),		[number(++)]).
builtin(integer(-),		[]) :- !.
builtin(integer(_),		[integer(++)]).
builtin(float(-),		[]) :- !.
builtin(float(_),		[float(++)]).
builtin(string(-),		[]) :- !.
builtin(string(_),		[string(++)]).
builtin(compound(-),		[]) :- !.
builtin(compound(-+),		[compound(+-)]) :- !.
builtin(compound(?),		[compound(+)]) :- !.
builtin(compound(_),		[compound(-)]).
builtin(type_of(_,_),		[type_of(-,++)]).
builtin(get_var_info(_,_,_),	[get_var_info(-,++,++)]).
builtin(get_cut(_),		[get_cut(++)]).
builtin(cut_to(_),		[cut_to(++)]).

builtin(_ is _,			[++ is ++]).
builtin(_ > _,			[++ > ++]).
builtin(_ >= _,			[++ >= ++]).
builtin(_ < _,			[++ < ++]).
builtin(_ =< _,			[++ =< ++]).
builtin(_ =:= _,		[++ =:= ++]).
builtin(_ =\= _,		[++ =\= ++]).
builtin(max(_,_,_),		[max(++,++,++)]).
builtin(min(_,_,_),		[min(++,++,++)]).

% in univ, both arguments must have the same exit modes, but at least +
builtin(++ =.. _,		[++ =.. ++]) :- !.
builtin(_ =.. ++,		[++ =.. ++]) :- !.
builtin(_ =.. _,		[+ =.. +]).
builtin(arg(_,++,_),		[arg(++,++,++)]) :- !.
builtin(arg(_,_,++),		[arg(++,+-,++)]) :- !.
builtin(arg(_,_,_),		[arg(++,+,?)]).
builtin(atom_string(_,_),	[atom_string(++,++)]).
builtin(char_int(_,_),		[char_int(++,++)]).
builtin(copy_term(X,_),		[copy_term(X,X)]).
builtin(functor(_,_,_),		[functor(+-,++,++)]).
builtin(integer_atom(_,_),	[integer_atom(++,++)]).
builtin(name(_,_),		[name(++,++)]).
builtin(string_list(_,_),	[string_list(++,++)]).
builtin(term_string(_,_),	[term_string(-,++)]).

builtin(concat_strings(_,_,_),	[concat_strings(++,++,++)]).
builtin(concat_atoms(_,_,_),	[concat_atoms(++,++,++)]).

builtin(read(_),		[read(?)]).
builtin(read(_,_),		[read(++,?)]).
builtin(read_(_,_,_),		[read_(++,?,++)]).
builtin(get(_),			[get(++)]).
builtin(write(_),		[write(-)]).
builtin(write(_,_),		[write(++,-)]).
builtin(writeln(_),		[writeln(-)]).
builtin(writeln(_,_),		[writeln(++,-)]).
builtin(writeq(_),		[writeq(-)]).
builtin(writeq(_,_),		[writeq(++,-)]).
builtin(writeq_(_,_,_),		[writeq_(++,-,++)]).
builtin(display(_),		[display(-)]).
builtin(put(_),			[put(++)]).
builtin(ttynl,			[ttynl]).
builtin(ttyput(_),		[ttyput(++)]).
builtin(get_stream(_,_),	[get_stream(++,++)]).
builtin(set_stream(_,_),	[set_stream(++,++)]).
builtin(get_prompt(_,_,_),	[get_prompt(++,++,++)]).
builtin(set_prompt(_,_,_),	[set_prompt(++,++,++)]).
builtin(flush(_),		[flush(++)]).
builtin(nl,			[nl]).
builtin(nl(_),			[nl(++)]).
builtin(open(_,_,_),		[open(++,++,++)]).
builtin(close(_),		[close(++)]).

builtin(asserta(_),		[asserta(-)]).
builtin(assert(_),		[assert(-)]).
builtin(retract(_),		[retract(+)]).
builtin(retract_all(_),		[retract_all(-)]).
builtin(clause(_),		[clause(+)]).
builtin(clause(_,_),		[clause(+,+)]).
builtin(abolish(_),		[abolish(++)]).

builtin(recorda(_,_),		[recorda(+-,?)]).
builtin(recorda(_,_,_),		[recorda(+-,?,++)]).
builtin(recordz(_,_),		[recordz(+-,?)]).
builtin(recordz_body(_,_,_),	[recordz_body(+-,?,++)]).
builtin(recorded(_,_),		[recorded(+-,?)]).
builtin(recorded(_,_,_),	[recorded(+-,?,++)]).
builtin(recorded_body(_,_,_),	[recorded_body(+-,?,++)]).
builtin(recorded_body(_,_,_,_),	[recorded_body(+-,?,++,++)]).
builtin(erase(_),		[erase(++)]).
builtin(erase(_,_),		[erase(+-,?)]).

builtin(setval(_,_),		[setval(++,-)]).
builtin(getval(_,_),		[getval(++,?)]).
builtin(incval(_),		[incval(++)]).

builtin(op(_,_,_),		[op(++,++,++)]).
builtin(local_op(_,_,_),	[local_op(++,++,++)]).
builtin(global_op(_,_,_),	[global_op(++,++,++)]).
builtin(local_record(_),	[local_record(++)]).
builtin(local_record_body(_,_),	[local_record_body(++,++)]).
builtin(cputime(_),		[cputime(++)]).
builtin(current_op(_,_,_),	[current_op(++, ++,++)]).
builtin(current_module(_),	[current_module(++)]).
builtin(create_module(_),	[create_module(++)]).
builtin(get_flag(_,_),		[get_flag(++,++)]).	% not quite, but ...
builtin(set_flag(_,_),		[set_flag(++,++)]).	% not quite, but ...
builtin(get_flag(_,_,_),	[get_flag(++,++,++)]).
builtin(get_flag_body(_,_,_,_),	[get_flag_body(++,++,++,++)]).
builtin(set_flag(_,_,_),	[set_flag(++,++,++)]).
builtin(set_flag_body(_,_,_,_),	[set_flag_body(++,++,++,++)]).
builtin(is_predicate(_),	[is_predicate(++)]).
builtin(is_predicate_(_,_),	[is_predicate_(++,++)]).
builtin(compile_term(_),	[compile_term(-)]).
builtin(compile_term_(_,_),	[compile_term_(-,++)]).
builtin(statistics,		[statistics]).
builtin(statistics(_,_),	[statistics(++,++)]).
builtin(dynamic_body(_,_),	[dynamic_body(++,++)]).
builtin(compare(_,_,_),		[compare(++,-,-)]).

builtin(error(_,_),		[error(++,?)]).
builtin(substring(_,_,_),	[substring(++,++,++)]).
builtin(pathname(_,_),		[pathname(++,++)]).
builtin(pathname(_,_,_),	[pathname(++,++,++)]).
builtin(suffix(_,_),		[suffix(++,++)]).
builtin(exists(_),		[exists(++)]).

builtin(sort(X,Y),		[sort(Z,Z)]) :- =(X,Y,Z0), =(+-,Z0,Z).
builtin(keysort(X,Y),		[keysort(Z,Z)]) :- =(X,Y,Z0), =(+-,Z0,Z).

builtin(length(_,_),		[length(+-,++)]).
builtin(member(_,++),		[member(++,++)]) :- !. 
builtin(member(_,_),		[member(?,+)]).
builtin(delete(_,++,_),		[delete(++,++,++)]) :- !.
builtin(delete(_,_,_),		[delete(?,+,+)]).

% cprolog
builtin(prompt(_,_),		[prompt(++,++)]).
builtin(see(_),			[see(++)]).
builtin(seen,			[seen]).
builtin(tell(_),		[tell(++)]).
builtin(telling(_),		[telling(++)]).
builtin(told(_),		[told(++)]).
builtin(seeing(_),		[seeing(++)]).
builtin(skip(_),		[skip(++)]).
builtin(skip(_),		[skip(++,++)]).
builtin(tab(_),			[tab(++)]).
builtin(tab(_,_),		[tab(++,++)]).
builtin(get0(_),		[get0(++)]).


/*** to be reviewed

:- import bound_arg_/4 from sepia_kernel.

bi_mode(Call, Exit) :-
	functor(Call, F, N),
	functor(Exit, F, N),
	bi_mode(N, F/N, Call, Exit).

bi_mode(0, _, _, _) :- !.
bi_mode(N, Pred, Call, Exit) :-
	N1 is N-1,
	arg(N, Call, CallArg),
	arg(N, Exit, ExitArg),
	(
	    is_predicate_body(Pred, source),
	    bound_arg_(Pred, N, Binding, source)
	->
	    map_modes(Binding, ExitArg)
	;
	    ExitArg=CallArg
	),
	bi_mode(N1, Pred, Call, Exit).

map_modes(constant, ++).
map_modes(nonvar, +).
map_modes(ground, ++).

***/

%-----------------------------------------------------------------------

?- nl,
writeln("Mode Analysis"), nl,
writeln("1. read a source file (e.g. src.pl):       read_source(src)."),
writeln("2. analyze a call pattern (e.g. p/2):      analyze(p(++,-))"),
writeln("This will print the resulting mode declarations"),
writeln("Valid modes are ++ + - ? +- and -+"), nl,
writeln("To collect builtin call modes, use setval(collect_builtin_modes, on)."), nl.



