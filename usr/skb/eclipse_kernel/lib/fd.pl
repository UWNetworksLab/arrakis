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
% Version:	$Id: fd.pl,v 1.1 2008/06/30 17:43:45 jschimpf Exp $
% ----------------------------------------------------------------------

/*
/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * FINITE DOMAINS
 *
 * IDENTIFICATION:      fd.pl 
 *
 * AUTHOR:		Micha Meier
 *
 * DESCRIPTION:         This library implements constraints over finite
			domains in ECLiPSe.

 */


:- module(fd).

:- comment(summary, "Finite domain library").
:- comment(author, "Micha Meier, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date").

:- reexport fd_arith except
	fd_eq/1,
	fd_eq/2,
	fd_ge/1,
	fd_ge/2,
	fd_gec/5,
	fd_gec_ent/6,
	fd_ineq/1,
	fd_ineq/2,
	fd_qeq/3,
	fd_re/2,
	fd_dom_simple/2,
	fd_dom_simple/3.
:- reexport fd_chip.
:- reexport fd_elipsys.
:- reexport fd_util.

% Output Macros

:- export macro(element/5, tr_fd_out/2, [write, goal]).

:- export
	tr_fd_out/2.


/*
:- reexport	% to make them callable as fd:(...)
    #=  /2,
    #>  /2,
    #<  /2,
    #>= /2,
    #<= /2,
    #=  /3,
    #>  /3,
    #<  /3,
    #>= /3,
    #<= /3,
    ##  /2,
    ##  /3,
    #/\ /2,
    #\/ /2,
    #=> /2,
    #<=> /2,
    (#\+) /1,
    #/\ /3,
    #\/ /3,
    #=> /3,
    #<=> /3,
    (#\+) /2,
    #\= /2,
    #\= /3,
    (isd)/2
   from fd_arith.

:- reexport	% to make them callable as fd:(...)
    :: /2,
    :: /3,
    indomain/1
   from fd_domain.
   */


:- export
    alldifferent/1,
    atmost/3,
    element/3,
    integers/1,
    minimize/2,
    minimize/4,
    minimize/5,
    minimize/6,
    minimize/8,
    minimize_bound_check/0,
    min_max/2,
    min_max/4,
    min_max/5,
    min_max/6,
    min_max/8.

:-  tool(minimize/2, minimize_body/3),
    tool(minimize/4, minimize_body/5),
    tool(minimize/5, minimize_body/6),
    tool(minimize/6, minimize_body/7),
    tool(minimize/8, minimize_body/9),
    tool(min_max/2, min_max_body/3),
    tool(min_max/4, min_max_body/5),
    tool(min_max/5, min_max_body/6),
    tool(min_max/6, min_max_body/7),
    tool(min_max/8, min_max_body/9).

:- export 
    minimize_body/3,
    minimize_body/5,
    minimize_body/6,
    minimize_body/7,
    minimize_body/9,
    min_max_body/3,
    min_max_body/5,
    min_max_body/6,
    min_max_body/7,
    min_max_body/9.


:- import
	% general-purpose predicates
	call_local/1,
	maxint/1,
	minint/1,
	par_true/0,
	prune_woken_goals/1,
	worker_boundary/0,

	% FD-specific predicates
	index_values/10
    from sepia_kernel.

:- pragma(nodebug).
:- pragma(system).

fderror(N, G) :-
	error(N, G, _).


%
% Check for incompatible library (IC) loaded.
%

check_for_conflicting_module :-
	( current_module(ic_kernel) ->
	    printf(warning_output, "Warning: The fd and ic modules use different domain representations and%nshould not be mixed.%n", [])
	;
	    true
	).

:- check_for_conflicting_module.


%
% Transformation routines
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input goal transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Goal Source Transformation

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Output goal transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode tr_fd_out(+, -).
tr_fd_out(element(A, B, C, _, _), element(A, L, C)) :-
    !,
    B =.. [_|L].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Propia interface
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- export msg/3.
msg(X, Y, Z) :- dvar_msg(X, Y, Z).

:- export is_solver_var/1.
is_solver_var(X) :- is_domain(X).

:- export is_exact_solver_var/1.
is_exact_solver_var(X) :- is_domain(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Built-In Predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


integers(X) :- var(X), !, error(4, integers(X)).
integers([]).
integers([X|Xs]) :-
    default_domain(X),
    integers(Xs).

%
% alldifferent(List)
%

alldifferent([]).
alldifferent([H|T]) :-
    outof(H, T),
    alldifferent(T).


%
% atmost(Number, List, Value)
%

atmost(_, [], _).
atmost(N, List, Val) :-
    filter_vars(N, List, Val, NewList, NewN, 0, VarNo),
    (NewN >= VarNo ->
	    true
    ; NewN == 0 ->
	    outof(Val, NewList)
    ;
	    make_suspension(atmost(NewN, NewList, Val), 3, Susp),
	    insert_suspension(NewList, Susp, inst of suspend, suspend)
    ).

filter_vars(N, [], _, [], N, V, V).
filter_vars(N, [H|T], Val, NewList, NewN, V, VarNo) :-
    var(H), !,
    (dvar_domain(H, D) ->
	( dom_check_in(Val, D) ->
	    V1 is V + 1,
	    NewList = [H | R]
	; V1 = V,
	  NewList = R
        )
    ; V1 is V + 1,
      NewList = [H | R]
    ),
    filter_vars(N, T, Val, R, NewN, V1, VarNo).
filter_vars(N, [Val|T], Val, R, NewN, V, VarNo) :-
    !,
    N > 0,
    N1 is N - 1,
    filter_vars(N1, T, Val, R, NewN, V, VarNo).
filter_vars(N, [_|T], Val, R, NewN, V, VarNo) :-
    filter_vars(N, T, Val, R, NewN, V, VarNo).

%
% element(Index, List, Value)
%

element(Index, List, Value) :-
    ground(List),
    !,
    sort(List, DomainV),
    LD =.. [t|List],
    functor(LD, _, SizeI),
    (is_domain(Index) ->
	dvar_attribute(Index, fd with domain:DI),
	(dom_range(DI, _, _) ->
	    Index #> 0,
	    Index #<= SizeI
	;
	    fderror(5, element(Index, List, Value))
	)
    ;
	Index::1..SizeI,
	dvar_attribute(Index, fd with domain:DI)
    ),
    (is_domain(Value) ->
	true
    ;
	Value::DomainV
    ),
    dvar_attribute(Value, fd with domain:DV),
    StartSize is max(dom_size(DI), dom_size(DV)) + 1,
    element(Index, LD, Value, StartSize, StartSize).
element(Index, List, Value) :-
    error(4, element(Index, List, Value)).

element(VI, LD, VV, SI, SV) :-
    index_values(VI, LD, VV, SI, SV, Res, NewIDom, NewVList, NSI, NSV),
    element_res(VI, LD, VV, NSI, NSV, Res, NewIDom, NewVList).

:- mode element_res(?, ++, ?, ++, ++, ++, ++, ++).
element_res(VI, LD, VV, NSI, NSV, 0, _, _) :-			% no update
    make_suspension(element(VI, LD, VV, NSI, NSV), 3, Susp),
    insert_suspension([VI|VV], Susp, any of fd).
element_res(VI, LD, VV, NSI, NSV, 1, NewIDom, _) :-		% Index updated
    dvar_update_nocheck(VI, NewIDom, NSI),
    ( NSV == 1 ->		% ONE value
    	true
    ;
	make_suspension(element(VI, LD, VV, NSI, NSV), 3, Susp),
	insert_suspension([VI|VV], Susp, any of fd)
    ),
    wake.
element_res(VI, LD, VV, NSI, NSV, 2, _, NewV) :-		% Value updated
    ( NSV == 1 ->		% ONE value
	[VV] = NewV
    ;
	dvar_update_nocheck(VV, NewV, NSV),
	make_suspension(element(VI, LD, VV, NSI, NSV), 3, Susp),
	insert_suspension([VI|VV], Susp, any of fd),
	wake
    ).
element_res(VI, LD, VV, NSI, NSV, 3, NewI, NewV) :-		% both updated
    dvar_update_nocheck(VI, NewI, NSI),
    ( NSV == 1 ->		% ONE value
	[VV] = NewV
    ;
	(VI == VV ->
	    var_fd(VI, dom(NewV, NSV)),
	    element(VI, LD, VI, 0, 0)
	;
	    dvar_update_nocheck(VV, NewV, NSV),
	    make_suspension(element(VI, LD, VV, NSI, NSV), 3, Susp),
	    insert_suspension([VI|VV], Susp, any of fd)
	)
    ),
    wake.
element_res(VI, _, VV, _, _, 4, I, V) :-			% only one index
    VV = V,
    VI = I.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Higher-order predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This lock is needed for atomic update of the cost bound in minimize
:- local variable(minimize_lock).
:- mutex_init(minimize_lock).

:- local reference(minimize_stack).

push(StackName, Item) :-
    getval(StackName, Stack),
    setval(StackName, [Item|Stack]).

top(StackName, Top) :-
    getval(StackName, Stack),
    nonvar(Stack), Stack = [Top|_].


%
% Min-Max: Branch & Bound by restarting each time a new solution is found.
%

% Simplified versions with less arguments

min_max_body(Goal, Cost, Module) :-
    minint(Min), maxint(Max),
    term_variables(Goal, SolTemplate),
    min_max_body(Goal, SolTemplate, SolTemplate, Cost, Min, Max, 0, 0, Module).

min_max_body(Goal, Template, Solution, Cost, Module) :-
    minint(Min), maxint(Max),
    min_max_body(Goal, Template, Solution, Cost, Min, Max, 0, 0, Module).

min_max_body(Goal, Cost, Lower, Upper, Percent, Module) :-
    min_max_body(Goal, Goal, Goal, Cost, Lower, Upper, Percent, 0, Module).

min_max_body(Goal, Cost, Lower, Upper, Percent, Timeout, Module) :-
    min_max_body(Goal, Goal, Goal, Cost, Lower, Upper, Percent, Timeout, Module).


% The general min_max with all options

min_max_body(Goal, Template, Solution, Value, Lower, Upper, Percent, Timeout, Module) :-
    prune_woken_goals(Goal),
    ( var(Value) ->	List = [Value]
    ; Value = [_|_] ->	List = Value
    ;			List = [Value]
    ),
    ( max_list_range(List, expr, MinList, MaxList) ->
    	Low is max(MinList, Lower),
    	Max is min(MaxList, Upper),
	shelf_create(sol(no_solution,Max), Index),
	% the s/1 wrapper makes it fail safely if no solution
	bbr2(Goal, s(Template), s(Solution), List, Low, Max, Percent, Timeout, Index, Module)
    ;
	error(5, min_max(Goal, Value, Lower, Upper, Percent, Timeout), Module)
    ).

bbr2(Goal, Template, _Solution, List, Low, Max, Percent, Timeout, Index, Module) :-
    block(call_local(branch_and_bound_restart(Goal, Template, List,
    		Low, Max, Percent, Timeout, Index, Module)), Tag, handle_exit(Tag)).
bbr2(_Goal, _Template, Solution, _, _, _, _, _, Index, _) :-
    xget(Index, 1, Solution),	% fail here if no solution
    shelf_abolish(Index),
    fd_true.

branch_and_bound_restart(Goal, Solution, List, Low, Max, Percent, Timeout, Index, Module) :-
    worker_boundary,
    push(minimize_stack, min_max),
    par_true,	% for better incremental stack copying
    ( Timeout>0 -> event_after(fd_timeout, Timeout) ; true),

    repeat,
    xget(Index, 2, M),
    (
	constrain_max_list(List, M),		% post new cost constraints
	call(Goal)@Module,
	schedule_suspensions(postponed), wake
    ->
	max_of_min_list_domains(List, Cost),
	(nonvar(Cost) ->
	    error(280, (Cost, Goal)),
	    % In case the solution still contains variables,
	    % we want to strip most of their attributes.
	    % Otherwise we might copy the whole constraint store!
	    copy_term(Solution, StrippedSolution),
	    xset(Index, 1, StrippedSolution),
	    NewUp is min(Cost - fix(Cost * Percent)//100, Cost - 1),
	    xset(Index, 2, NewUp),
	    % According to the chipc manual this should be NewUp < Low !
	    Cost < Low				% restart from repeat
	;
	    error(4, min_max(Goal, List, Low, Max, Percent), Module)
	)
    ;
	true
    ),
    !,
    cancel_after_event(fd_timeout, _),
    fail.


%
% Minimize: Branch & Bound by backtracking, the cost limit is represented
%	    by a global variable. This version might not be as efficient
%	    as when a new (non-backtrackable) constraint is actually added
%	    to the store. It can be improved by an explicit check
%	    on every labeling step: minimize_bound_check/0
%

% Simplified versions with less arguments

minimize_body(Goal, Cost, Module) :-
    minint(Min), maxint(Max),
    term_variables(Goal, SolTemplate),
    minimize_body(Goal, SolTemplate, SolTemplate, Cost, Min, Max, 0, 0, Module).

minimize_body(Goal, Template, Solution, Cost, Module) :-
    minint(Min), maxint(Max),
    minimize_body(Goal, Template, Solution, Cost, Min, Max, 0, 0, Module).

minimize_body(Goal, Cost, Lower, Upper, Percent, Module) :-
    minimize_body(Goal, Goal, Goal, Cost, Lower, Upper, Percent, 0, Module).

minimize_body(Goal, Cost, Lower, Upper, Percent, Timeout, Module) :-
    minimize_body(Goal, Goal, Goal, Cost, Lower, Upper, Percent, Timeout, Module).


% The general minimize with all options

minimize_body(Goal, Template, Solution, Value, Lower, Upper, Percent, Timeout, Module) :-
    prune_woken_goals(Goal),
    ( var(Value) ->	List = [Value]
    ; Value = [_|_] ->	List = Value
    ;			List = [Value]
    ),
    ( max_list_range(List, var, MinList, MaxList) ->
    	Low is max(MinList, Lower),
    	Max is min(MaxList, Upper),
	shelf_create(sol(no_solution,Max), Index),
	% the s/1 wrapper makes it fail safely if no solution
	bb2(Goal, s(Template), s(Solution), List, Low, Max, Percent, Timeout, Index, Module)

    ;
	( Goal == Template ->
	    error(5, minimize(Goal, Value, Lower, Upper, Percent, Timeout), Module)
	;
	    error(5, minimize(Goal, Template, Solution, Value, Lower, Upper, Percent, Timeout), Module)
	)
    ).

bb2(Goal, Template, _Solution, List, Low, Max, Percent, Timeout, Index, Module) :-
    block(call_local(branch_and_bound(Goal, Template, List,
    		Low, Max, Percent, Timeout, Index, Module)), Tag, handle_exit(Tag)).
bb2(_Goal, _Template, Solution, _, _, _, _, _, Index, _) :-
    xget(Index, 1, Solution),	% fail here if no solution
    shelf_abolish(Index),
    fd_true.

branch_and_bound(Goal, Solution, List, Low, Max, Percent, Timeout, Index, Module) :-
    worker_boundary,
    constrain_max_list_index(List, Index),
    push(minimize_stack, List/Index),
    ( Timeout>0 -> event_after(fd_timeout, Timeout) ; true),

    call(Goal)@Module,

    max_of_min_list_domains(List, Cost),
    schedule_suspensions(postponed), wake,

    (nonvar(Cost) ->
	% Update cost if better. This must be atomic.
	mutex(minimize_lock, (
	    Cost =< xget(Index, 2),
	    error(280, (Cost, Goal)),
	    copy_term(Solution, StrippedSolution),
	    xset(Index, 1, StrippedSolution),
	    NewUp is min(Cost - fix(Cost * Percent)//100, Cost - 1),
	    xset(Index, 2, NewUp),
	    Cost < Low			% backtrack into Goal
	))
    ;
	error(4, minimize(Goal, List, Low, Max, Percent), Module)
    ),
    !,
    cancel_after_event(fd_timeout, _),
    fail.
branch_and_bound(_, _, _, _, _, _, _, _, _) :-
    cancel_after_event(fd_timeout, _),
    fail.

% Get the minimum and maximum value of a list of domain vars
% Where gets unified with 'expr' when the list contains expressions
max_list_range(List, Where, Min, Max) :-
    maxint(Maxint),
    minint(Minint),
    max_list_range(List, Where, Minint, Max, Maxint, Min).

max_list_range([], _, Max, Max, Min, Min).
max_list_range([Var|Rest], Where, SoFar, Max, MinSoFar, Min) :-
    ( dvar_domain(Var, Domain) -> true
    ; default_domain(Var), dvar_domain(Var, Domain) ),
    dom_range(Domain, DMin, DMax),
    !,
    NewMin is min(MinSoFar, DMin),
    NewMax is max(SoFar, DMax),
    max_list_range(Rest, Where, NewMax, Max, NewMin, Min).
max_list_range([Term|Rest], expr, SoFar, Max, MinSoFar, Min) :-
    term_to_linear(Term, LTerm),
    linear_term_range(LTerm, DMin, DMax),
    NewMin is min(MinSoFar, DMin),
    NewMax is max(SoFar, DMax),
    max_list_range(Rest, _Where, NewMax, Max, NewMin, Min).

% Constrain all variables in the list to be smaller than Max
constrain_max_list([], _).
constrain_max_list([Term|Rest], Max) :-
    Term #<= Max,
    constrain_max_list(Rest, Max).

% Constrain all variables in the list to be smaller than the cost bound
constrain_max_list_index([], _).
constrain_max_list_index([Term|Rest], Index) :-
    constrain_max_index(Term, Index),
    constrain_max_list_index(Rest, Index).

constrain_max_index(Term, Index) :-
    var(Term),
    xget(Index, 2, Max),
    Term #<= Max,
    make_suspension(constrain_max_index(Term, Index), 2, Susp),
    insert_suspension(Term, Susp, min of fd).
constrain_max_index(Term, Index) :-
    nonvar(Term),
    xget(Index, 2, Max),
    Term =< Max.

%
% Explicit check that can be used with minimize in additional choice points
%
minimize_bound_check :-
    ( top(minimize_stack, List/Index) ->
      check_max_list_index(List, Index)
    ;
      true    % stack empty or we are in a min_max (no check needed)
    ).

check_max_list_index([], _).
check_max_list_index([Term|Rest], Index) :-
    check_max_index(Term, Index),
    check_max_list_index(Rest, Index).

check_max_index(Term, Index) :-
    var(Term),
    xget(Index, 2, Max),
    Term #<= Max.
check_max_index(Term, Index) :-
    nonvar(Term),
    xget(Index, 2, Max),
    Term =< Max.

max_of_min_list_domains([], Cost, Cost, _, _).
max_of_min_list_domains([Term|Rest], SoFar, Max, Var, Val) :-
    nonvar(Term),
    !,
    min_domain(Term, DMin),
    (DMin > SoFar -> 
	max_of_min_list_domains(Rest, DMin, Max, Var, Val)
    ;
	max_of_min_list_domains(Rest, SoFar, Max, Var, Val)
    ).
max_of_min_list_domains([Var|Rest], SoFar, Max, Var, DMin) :-
    var(DMin),				% fail if not the first variable
    min_domain(Var, DMin),
    (DMin > SoFar -> 
	max_of_min_list_domains(Rest, DMin, Max, _, DMin)
    ;
	max_of_min_list_domains(Rest, SoFar, Max, _, DMin)
    ).

min_domain(Term, Min) :-
    nonvar(Term),
    !,
    (nonground(Term) ->
	term_to_linear(Term, LTerm),
	linear_term_range(LTerm, Min, Min)
    ;
	Min is Term
    ).
min_domain(Var, Min) :-
    dvar_domain(Var, Domain),
    dom_range(Domain, Min, _).

max_of_min_list_domains([Term|List], Cost) :-
    min_domain(Term, Min),
    (max_of_min_list_domains(List, Min, Cost, Var, Val) ->
	Var = Val
    ;
	true				% unbound if an error
    ).

% A true which is not optimized away so that we have a call instruction which
%   forces a debugger event which traces the woken goals before the exit...
fd_true.

%
% Event handling
%

handle_exit(fd_timeout) :-
    !,
    fail.
handle_exit(Tag) :-
    cancel_after_event(fd_timeout, _),
    exit_block(Tag).

:- set_event_handler(fd_timeout, exit_block/1).

:- untraceable
	branch_and_bound/9,			% called from block
	branch_and_bound_restart/9.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(/(alldifferent, 1), [
	summary:"The elements of the list List are pairwise different.

",
	template:"alldifferent(?List)",
	desc:html("   This constraint imposes the constraint ##/2 on every pair of element of
   List.

<P>
"),
	args:["?List" : "A list of integers and domain variables."],
	resat:"   No.",
	fail_if:"   Fails if two element of the list are equal.\n\n",
	see_also:[/(::, 2), /(#::, 2), /(##, 2), fd_global:alldifferent/1]]).

:- comment(/(alldistinct, 1), [
	summary:"The elements of the list List are pairwise different.

",
	template:"alldistinct(?List)",
	desc:html("\
	This constraint imposes the constraint ##/2 on every pair of
	element of List.  It is just a compatibility alias for alldifferent/1.
"),
	args:["?List" : "A list of integers and domain variables."],
	resat:"   No.",
	fail_if:"   Fails if two element of the list are equal.\n\n",
	see_also:[/(::, 2), /(#::, 2), /(##, 2), /(alldifferent, 1)]]).

:- comment(/(#/\, 2), [
	summary:"The constraint expression C1 and the constraint expression C2 is true.

",
	template:"?C1 #/\\ ?C2",
	desc:html("   This constraint states that both constraint expressions C1 and C2 must
   be true.  It is identical to normal conjunstion (C1, C2).

<P>
"),
	args:["?C1" : "An arithmetic constraint expression.", "?C2" : "An arithmetic constraint expression."],
	resat:"   No.",
	fail_if:"   Fails if one of C1 or C2 is false.\n\n",
	see_also:[/(#\/, 2), /(#=>, 2), /(#<=>, 2), /(#\+, 1)]]).

:- comment(/(#/\, 3), [
	summary:"The constraint X #/\\ Y has the truth value B.

",
	template:"#/\\(?X, ?Y, ?B)",
	desc:html("   This predicate is an evaluation constraint:  it succeeds if and only if
   the truth value of its associated constraint (the constraint with arity
   one less and the same arguments except for B) is equal to B, where the
   value 0 means false and 1 true.  This constraint can be used both to
   test the validity of the associated constraint (entailment test) and to
   impose this constraint or its negation.  For the former, B will be
   instantiated as soon as either the associated constraint or its negation
   is subsumed by the current state of its domain variables.  For the
   latter, when B is instantiated, then depending on its value, either the
   associated constraint or its negation will be imposed on its arguments.

<P>
   Boolean expressions containing arithmetic constraints are converted by
   the system to a series of evaluation constraints and arithmetic
   constraints in a way similar to arithmetic evaluation, e.g.

<P>
<PRE>
    X #&gt; Y #=&gt; T #&gt; V #/\\ E #= F
</PRE>
   is transformed into

<P>
<PRE>
    #&gt;(X, Y, B1),
    #&gt;(T, V, B2),
    #=(E, F, B3),
    #=(B2 + B3, 2, B4),
    B1 #&lt;= B4.      % i.e.  if B1 then B4
</PRE>
"),
	args:["?X" : "A constraint expression.", "?Y" : "A constraint expression.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(#\+, 2), /(#\/, 3), /(#=>, 3), /(#<=>, 3), /(isd, 2)]]).

:- comment(/(atmost, 3), [
	summary:"At most N elements of the list List have the value V.

",
	template:"atmost(+N, ?List, +V)",
	desc:html("   If List is a list of domain variables and/or integers, this constraint
   takes care that at most N element of this list have the value V. As soon
   as some domain variable from the list is updated, this constraint is
   woken and it checks if the constraint is still satisfiable and if so, if
   it is already satisfied or not.

<P>
"),
	args:["+N" : "An integer", "?List" : "A list of domain variables or integers", "+V" : "An integer"],
	resat:"   No.",
	fail_if:"   Fails if more than N elements of List are instantiated to V.\n\n",
	see_also:[/(::, 2), /(#::, 2), /(##, 2), /(element, 3)]]).

:- comment(/(constraints_number, 2), [
	summary:"The number of constraints and suspended goals associated with the variable
Var is N.

",
	template:"constraints_number(?Var, ?N)",
	desc:html("   N is the number of constraints and suspended goals currently attached to
   the variable Var.  Note that this number may not correspond to the exact
   number of different constraints attached to Var, as goals in different
   suspending lists are counted separately.  This predicate is often used
   when looking for the most or least constrained variable from a set of
   domain variables.  If Var is instantiated, N is bound to a very large
   integer, if Var is a free variable, N is zero.

<P>
"),
	args:["?Var" : "Prolog term", "?N" : "Variable or integer"],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(::, 2), /(#::, 2), /(deleteff, 3), /(deleteffc, 3)]]).

:- comment(/(default_domain, 1), [
	summary:"This predicate is called to assign a default domain to a free variable Var
which occurs in arithmetic constraints.

",
	template:"default_domain(-Var)",
	desc:html("   This is a simple hook predicate which allows the user to modify the
   domain which is by default assigned to domainless variables that occur
   in arithmetic constraints.  When such a variable is encountered, this
   predicate is invoked and it must give the variable an integer finite
   domain.  To modify the default domain, it is necessary to recompile this
   predicate in the module fd_arith.

<P>
"),
	args:["-Var" : "", "A" : "variable"],
	resat:"   No.",
	fail_if:"   None\n\n",
	eg:"
   [eclipse 13]: A #> B.
   A = A[-9999999..10000000]
   B = B[-10000000..9999999]
   ...
   [eclipse 14]: compile(user, fd_arith).
   default_domain(X) :- X::0..100000000.
   user       compiled traceable 56 bytes in 0.00 seconds
   yes.
   [eclipse 20]: X #> Y.
   X = X[1..100000000]
   Y = Y[0..99999999]



",
	see_also:[/(new_domain_var, 1), /(::, 2), /(#::, 2)]]).

:- comment(/(##, 2), [
	summary:"X is different from Y.

",
	template:"?X ## ?Y",
	desc:html("   This constraints states that the two linear terms are not equal.  It is
   suspended until at most one variable appears in it and then its domain
   is updated so that the constraint is satisfied.

<P>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term."],
	resat:"   No.",
	fail_if:"   The constraint does not hold if X=Y.\n\n",
	see_also:[/(::, 2), /(#::, 2), /(#=, 2)]]).

:- comment(/(##, 3), [
	summary:"The constraint X ## Y has the truth value B.

",
	template:"##(?X, ?Y, ?B)",
	desc:html("   This predicate is an evaluation constraint:  it succeeds if and only if
   the truth value of its associated constraint (the constraint with arity
   one less and the same arguments except for B) is equal to B, where the
   value 0 means false and 1 true.  This constraint can be used both to
   test the validity of the associated constraint (entailment test) and to
   impose this constraint or its negation.  For the former, B will be
   instantiated as soon as either the associated constraint or its negation
   is subsumed by the current state of its domain variables.  For the
   latter, when B is instantiated, then depending on its value, either the
   associated constraint or its negation will be imposed on its arguments.

<P>
   Boolean expressions containing arithmetic constraints are converted by
   the system to a series of evaluation constraints and arithmetic
   constraints in a way similar to arithmetic evaluation, e.g.

<P>
<PRE>
    X #&gt; Y #=&gt; T #&gt; V #/\\ E #= F
</PRE>
   is transformed into

<P>
<PRE>
    #&gt;(X, Y, B1),
    #&gt;(T, V, B2),
    #=(E, F, B3),
    #=(B2 + B3, 2, B4),
    B1 #&lt;= B4.      % i.e.  if B1 then B4
</PRE>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(#>=, 3), /(#<, 3), /(#<=, 3), /(#=, 3), /(#\=, 3)]]).

:- comment(/(#\=, 2), [
	summary:"X is different from Y.

",
	template:"?X #\\= ?Y",
	desc:html("   This constraints states that the two linear terms are not equal.  It is
   suspended until at most one variable appears in it and then its domain
   is updated so that the constraint is satisfied.

<P>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term."],
	resat:"   No.",
	fail_if:"   The constraint does not hold if X=Y.\n\n",
	see_also:[/(::, 2), /(#::, 2), /(#=, 2)]]).

:- comment(/(#\=, 3), [
	summary:"The constraint X #\\= Y has the truth value B.

",
	template:"#\\=(?X, ?Y, ?B)",
	desc:html("   This predicate is an evaluation constraint:  it succeeds if and only if
   the truth value of its associated constraint (the constraint with arity
   one less and the same arguments except for B) is equal to B, where the
   value 0 means false and 1 true.  This constraint can be used both to
   test the validity of the associated constraint (entailment test) and to
   impose this constraint or its negation.  For the former, B will be
   instantiated as soon as either the associated constraint or its negation
   is subsumed by the current state of its domain variables.  For the
   latter, when B is instantiated, then depending on its value, either the
   associated constraint or its negation will be imposed on its arguments.

<P>
   Boolean expressions containing arithmetic constraints are converted by
   the system to a series of evaluation constraints and arithmetic
   constraints in a way similar to arithmetic evaluation, e.g.

<P>
<PRE>
    X #&gt; Y #=&gt; T #&gt; V #/\\ E #= F
</PRE>
   is transformed into

<P>
<PRE>
    #&gt;(X, Y, B1),
    #&gt;(T, V, B2),
    #=(E, F, B3),
    #=(B2 + B3, 2, B4),
    B1 #&lt;= B4.      % i.e.  if B1 then B4
</PRE>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(#>=, 3), /(#<, 3), /(#<=, 3), /(#=, 3), /(#\=, 3)]]).

:- comment(/(dom, 2), [
	summary:"List is the list of elements in the domain of Var.

",
	template:"dom(+Var, ?List)",
	desc:html("   If Var is a domain variable, List is unified with a sorted list of all
   elements in its domain.  If Var is an integer, List is unified with a
   singleton list.

<P>
   The predicates ::/2 and #::/2 can also be used to query the domain of a domain
   variable, however they yield a list of integer intervals, which is the
   direct domain representation (it is therefore also more efficient
   because no new structures have to be created).

<P>
   Use this predicate with care, because it might expand a compact
   representation of large intervals into an explicit list of their
   elements.  Unless an explicit list representation of the domain is
   really required, the predicates working on domains should be preferred.

<P>
"),
	args:["+Var" : "A domain variable or an integer.", "?List" : "Term unifying with a list of integers."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[4 : "Var is not a domain variable."],
	eg:"
   [eclipse 9]: X::[1..3, 100..102], dom(X, List), X::I.
   List = [1, 2, 3, 100, 101, 102]
   X = X :: [1 .. 3, 100 .. 102]
   I = [1 .. 3, 100 .. 102]
   yes.



",
	see_also:[/(::, 2), /(#::, 2), /(maxdomain, 2), /(mindomain, 2)]]).

:- comment(/(dom_check_in, 2), [
	summary:"Element is in the domain Dom.

",
	template:"dom_check_in(+Element, +Dom)",
	desc:html("   Succeed if the term Element is in the domain Dom.

<P>
"),
	args:["+Element" : "A Prolog term.", "+Dom" : "A finite domain."],
	resat:"   No.",
	fail_if:"   Fails if the element does not occur in the domain.\n\n",
	see_also:[/(dom_compare, 3), /(dom_member, 2), /(dom_range, 3), /(dom_size, 2), /(dvar_domain, 2)]]).

:- comment(/(dom_compare, 3), [
	summary:"Res is the result of the comparison of the domains Dom1 and Dom2.

",
	template:"dom_compare(?Res, +Dom1, +Dom2)",
	desc:html("   Works like compare/3 for terms.  Res is unified with

<P>
  * = iff Dom1 is equal to Dom2,

<P>
  * &lt; iff Dom1 is a proper subset of Dom2,

<P>
  * &gt; iff Dom2 is a proper subset of Dom1.

<P>
"),
	args:["?Res" : "Atom or variable.", "+Dom1" : "A finite domain.", "+Dom2" : "A finite domain."],
	resat:"   No.",
	fail_if:"   Fails if neither domain is a subset of the other one.\n\n",
	see_also:[/(dom_member, 2), /(dom_range, 3), /(dom_size, 2), /(dvar_domain, 2)]]).

:- comment(/(dom_copy, 2), [
	summary:"Dom2 is a copy of the domain Dom1.

",
	template:"dom_copy(+Dom1, -Dom2)",
	desc:html("   Dom2 is a copy of the domain Dom1.  Since the updates are done in-place,
   two domain variables must not share the same physical domain and so when
   defining a new variable with an existing domain, the domain has to be
   copied first.

<P>
"),
	args:["+Dom1" : "A finite domain.", "-Dom2" : "A variable."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(dom_compare, 3), /(dom_member, 2), /(dom_range, 3), /(dom_size, 2), /(dvar_domain, 2)]]).

:- comment(/(dom_difference, 4), [
	summary:"The domain DomDifference is Dom1 \\Dom2 and Size is the number of its
elements.

",
	template:"dom_difference(+Dom1, +Dom2, -DomDiff, -Size)",
	desc:html("   The domain DomDifference is Dom1 \\Dom2 and Size is the number of its
   elements.

<P>
"),
	args:["+Dom1" : "A finite domain.", "+Dom2" : "A finite domain.", "-DomDiff" : "A variable.", "-Size" : "A variable."],
	resat:"   No.",
	fail_if:"   Fails if Dom1 is a subset of Dom2.\n\n",
	see_also:[/(dom_compare, 3), /(dom_member, 2), /(dom_range, 3), /(dom_size, 2), /(dvar_domain, 2)]]).

:- comment(/(dom_to_list, 2), [
	summary:"List is the list of elements in the domain Dom.

",
	template:"dom_to_list(+Dom, ?List)",
	desc:html("   List is unified with a sorted list of all elements in the domain Dom.

<P>
   The predicates ::/2 and #::/2 can also be used to query the domain of a domain
   variable, however they yield a list of integer intervals, which is the
   direct domain representation (it is therefore also more efficient
   because no new structures have to be created).

<P>
   Use this predicate with care, because it might expand a compact
   representation of large intervals into an explicit list of their
   elements.  Unless an explicit list representation of the domain is
   really required, the predicates working on domains should be preferred.

<P>
"),
	args:["+Dom" : "A finite domain.", "?List" : "Term unifying with a list of integers."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[4 : "Var is not a domain variable."],
	eg:"
   [eclipse 9]: X::[1..3, 100..102], dvar_domain(X, D), dom_to_list(D,
   List), X::I.
   D = [1..3, 100..102]
   List = [1, 2, 3, 100, 101, 102]
   X = X :: [1 .. 3, 100 .. 102]
   I = [1 .. 3, 100 .. 102]
   yes.



",
	see_also:[/(::, 2), /(#::, 2), /(maxdomain, 2), /(mindomain, 2)]]).

:- comment(/(dom_intersection, 4), [
	summary:"The domain DomInt is the intersection of domains Dom1 and Dom2 and Size is
the number of its elements.

",
	template:"dom_intersection(+Dom1, +Dom2, -DomInt, -Size)",
	desc:html("   The domain DomInt is the intersection of domains Dom1 and Dom2 and Size
   is the number of its elements.

<P>
"),
	args:["+Dom1" : "A finite domain.", "+Dom2" : "A finite domain.", "-DomInt" : "A variable.", "-Size" : "A variable."],
	resat:"   No.",
	fail_if:"   Fails if the intersection is empty.\n\n",
	see_also:[/(dom_compare, 3), /(dom_member, 2), /(dom_range, 3), /(dom_size, 2), /(dvar_domain, 2)]]).

:- comment(/(dom_member, 2), [
	summary:"Element is in the domain Dom.

",
	template:"dom_member(?Element, +Dom)",
	desc:html("   Successively instantiate Element to the values in the domain Dom
   (similar to indomain/1).

<P>
"),
	args:["?Element" : "A Prolog term.", "+Dom" : "A finite domain."],
	resat:"   Yes.",
	fail_if:"   None.\n\n",
	see_also:[/(dom_check_in, 2), /(indomain, 1)]]).

:- comment(/(dom_range, 3), [
	summary:"Return the minimum and maximum value in the integer domain Dom.

",
	template:"dom_range(+Dom, ?Min, ?Max)",
	desc:html("   Return the minimum and maximum value in the integer domain Dom.  This
   predicate can also be used to test if a given domain is an integer one
   or not.

<P>
"),
	args:["+Dom" : "A finite domain.", "?Min" : "An integer or a variable.", "?Max" : "An integer or a variable."],
	resat:"   No.",
	fail_if:"   Fails if Dom is a domain containing non-integer atomic elements.\n\n",
	see_also:[/(dom_check_in, 2), /(is_integer_domain, 1)]]).

:- comment(/(dvar_range, 3), [
	summary:"Return the minimum and maximum domain value of DVar",
	template:"dvar_range(+DVar, ?Min, ?Max)",
	desc:html("\
	Return the minimum and maximum domain value of the domin
	variable DVar.  This predicate can also be used to test if a
	given domain is an integer one or not. 
	If DVar is instantiated, Min and Max will be identical."),
	args:["+DVar" : "A domain variable or integer.", "?Min" : "An integer or a variable.", "?Max" : "An integer or a variable."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(dvar_domain, 2), /(dom_range, 3)]]).

:- comment(/(dom_size, 2), [
	summary:"Size is the number of elements in the domain Dom.

",
	template:"dom_size(+Dom, ?Size)",
	desc:html("   Size is the number of elements in the domain Dom.

<P>
"),
	args:["+Dom" : "A finite domain.", "?Size" : "An integer or a variable."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(dom_check_in, 2), /(dom_range, 3), /(dvar_domain, 2)]]).

:- comment(/(dom_union, 4), [
	summary:"The domain DomUnion is the union of domains Dom1 and Dom2 and Size is the
number of its elements.

",
	template:"dom_union(+Dom1, +Dom2, -DomUnion, -Size)",
	desc:html("   The domain DomUnion is the union of domains Dom1 and Dom2 and Size is
   the number of its elements.

<P>
   Note that the main use of the predicate is to yield the most specific
   generalisation of two domains, in the usual cases the domains become
   smaller, not bigger.

<P>
"),
	args:["+Dom1" : "A finite domain.", "+Dom2" : "A finite domain.", "-DomUnion" : "A variable.", "-Size" : "A variable."],
	resat:"   No.",
	fail_if:"   Fails if the union is empty.\n\n",
	see_also:[/(dom_compare, 3), /(dom_member, 2), /(dom_range, 3), /(dom_size, 2), /(dvar_domain, 2)]]).

:- comment(/(::, 2), [
	summary:"Terms in Vars have the domain Domain.

",
	template:"?Vars ::  ?Domain",
	desc:html("   The main purpose of this predicate is to create domain variables.
   Domain can be a closed integer interval denoted as Min..Max, or a sorted
   list of integer intervals and/or elements.  If Vars is already a domain
   variable, its domain will be updated according to the new domain; if it
   is instantiated, the predicate checks if the value lies in the domain.
   Otherwise, if Vars is a free variable, it is converted to a domain
   variable.  If Vars is a list, this will be performed for all its
   elements.

<P>
   If Vars is a domain variable and Domain is free, it is bound to the
   domain of the variable.

<P>
"),
	args:["?Vars" : "A variable or a list of variables.", "+Domain" : "Variable, integer, integer interval or a list of integers                and integer intervals."],
	resat:"   No.",
	fail_if:"   Fails if Vars cannot have the domain Domain.\n\n",
	see_also:[/(#::, 2), /(dom_to_list, 2), /(is_domain, 1)]]).

:- comment(/(#::, 2), [
	summary:"Terms in Vars have the domain Domain. The predicate is an alias for ::/2.

",
	template:"?Vars #::  ?Domain",
	args:["?Vars" : "A variable or a list of variables.", "+Domain" : "Variable, integer, integer interval or a list of integers                and integer intervals."],
	resat:"   No.",
	fail_if:"   Fails if Vars cannot have the domain Domain.\n\n",
	see_also:[/(::, 2), /(dom_to_list, 2), /(is_domain, 1)]]).

:- comment(/(::, 3), [
	summary:"The constraint X ::  Y has the truth value B.

",
	template:"::(?X, ?Y, ?B)",
	desc:html("   This predicate is an evaluation constraint:  it succeeds if and only if
   the truth value of its associated constraint (the constraint with arity
   one less and the same arguments except for B) is equal to B, where the
   value 0 means false and 1 true.  This constraint can be used both to
   test the validity of the associated constraint (entailment test) and to
   impose this constraint or its negation.  For the former, B will be
   instantiated as soon as either the associated constraint or its negation
   is subsumed by the current state of its domain variables.  For the
   latter, when B is instantiated, then depending on its value, either the
   associated constraint or its negation will be imposed on its arguments.

<P>
   Boolean expressions containing arithmetic constraints are converted by
   the system to a series of evaluation constraints and arithmetic
   constraints in a way similar to arithmetic evaluation, e.g.

<P>
<PRE>
    X #&gt; Y #=&gt; T #&gt; V #/\\ E #= F
</PRE>
   is transformed into

<P>
<PRE>
    #&gt;(X, Y, B1),
    #&gt;(T, V, B2),
    #=(E, F, B3),
    #=(B2 + B3, 2, B4),
    B1 #&lt;= B4.      % i.e.  if B1 then B4
</PRE>
"),
	args:["?X" : "A domain variable", "?Y" : "A linear term.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(#::, 3), /(#>=, 3), /(#<, 3), /(#<=, 3), /(#=, 3), /(#\=, 3)]]).

:- comment(/(#::, 3), [
	summary:"The constraint X #::  Y has the truth value B. The predicate is an alias for ::/3.

",
	template:"#::(?X, ?Y, ?B)",
	args:["?X" : "A domain variable", "?Y" : "A linear term.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(::, 3), /(#>=, 3), /(#<, 3), /(#<=, 3), /(#=, 3), /(#\=, 3)]]).

:- comment(/(dvar_attribute, 2), [
	summary:"Attrib is the attribute of the domain variable DVar.

",
	template:"dvar_attribute(+DVar, ?Attrib)",
	desc:html("   Attrib is the attribute of the domain variable DVar.  If DVar is
   instantiated, Attrib is bound to an attribute with a singleton domain
   and empty suspension lists.

<P>
"),
	args:["+DVar" : "A domain variable.", "?Attrib" : "Any Prolog term."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(#::, 3), /(dvar_domain, 2), /(::, 2), /(#::, 2)]]).

:- comment(/(dvar_domain, 2), [
	summary:"Dom is the domain of the domain variable DVar.

",
	template:"dvar_domain(+DVar, -Dom)",
	desc:html("   Dom is the domain of the domain variable DVar.  If DVar is instantiated,
   Dom is bound to a singleton domain.

<P>
"),
	args:["+DVar" : "A domain variable.", "-Dom" : "A variable."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(dvar_attribute, 2), /(::, 2), /(#::, 2)]]).

:- comment(/(dvar_msg, 3), [
	summary:"MsgDVar is a domain variable which is the most specific generalisation of
domain variables or atomic values DVar1 and DVar2.

",
	template:"dvar_msg(+DVar1, +DVar2, -MsgDVar)",
	desc:html("   MsgVar is a domain variable which is the most specific generalisation of
   domain variables or atomic values Var1 and Var2.

<P>
"),
	args:["+DVar1" : "A finite domain or an atomic term.", "+DVar2" : "A finite domain or an atomic term.", "-MsgDVar" : "A free variable."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(dom_union, 4), /(dom_intersection, 4), /(dvar_domain, 2)]]).

:- comment(/(dvar_remove_element, 2), [
	summary:"The element El is removed from the domain of DVar and all concerned lists
are woken.

",
	template:"dvar_remove_element(+DVar, +El)",
	desc:html("   The element El is removed from the domain of DVar and all concerned
   lists are woken.  If the resulting domain is empty, this predicate
   fails.  If it is a singleton, DVar is instantiated.  If the domain does
   not contain the element, no updates are made.

<P>
"),
	args:["+DVar" : "A domain variable.", "+El" : "An atomic term."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(dvar_attribute, 2), /(::, 2), /(#::, 2)]]).

:- comment(/(dvar_remove_greater, 2), [
	summary:"Remove all elements in the domain of DVar which are greater than the
integer El and wake all concerned lists.

",
	template:"dvar_remove_greater(+DVar, +El)",
	desc:html("   Remove all elements in the domain of DVar which are greater than the
   integer El and wake all concerned lists.  If the resulting domain is
   empty, this predicate fails, if it is a singleton, DVar is instantiated.

<P>
"),
	args:["+DVar" : "A domain variable.", "+El" : "An atomic term."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(dvar_attribute, 2), /(::, 2), /(#::, 2)]]).

:- comment(/(dvar_remove_smaller, 2), [
	summary:"Remove all elements in the domain of DVar which are smaller than the
integer El and wake all concerned lists.

",
	template:"dvar_remove_smaller(+DVar, +El)",
	desc:html("   Remove all elements in the domain of DVar which are smaller than the
   integer El and wake all concerned lists.  If the resulting domain is
   empty, this predicate fails, if it is a singleton, DVar is instantiated.

<P>
"),
	args:["+DVar" : "A domain variable.", "+El" : "An atomic term."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(dvar_attribute, 2), /(::, 2), /(#::, 2)]]).

:- comment(/(dvar_replace, 2), [
	summary:"Change the domain of the domain variable DVar to NewDom without propagating
the changes.

",
	template:"dvar_replace(+DVar, +NewDom)",
	desc:html("   If the size of the domain NewDom is 0, the predicate fails.  If the size
   of the new domain is 1, DVar is given this singleton domain.  Otherwise,
   if the size of the new domain is smaller than the size of the domain
   variable's domain, the domain of DVar is replaced by NewDom, but no
   suspensions are passed to the waking scheduler.

<P>
   This predicate is useful for local consistency checks.

<P>
"),
	args:["+DVar" : "A domain variable.", "+NewDom" : "A finite domain."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(dvar_attribute, 2), /(::, 2), /(#::, 2), /(dvar_update, 2)]]).

:- comment(/(dvar_update, 2), [
	summary:"Change the domain of the domain variable DVar to NewDom and propagate the
changes.

",
	template:"dvar_update(+DVar, +NewDom)",
	desc:html("   If the size of the domain NewDom is 0, the predicate fails.  If it is 1,
   the domain variable DVar is instantiated to the value in the domain.
   Otherwise, if the size of the new domain is smaller than the size of the
   domain variable's domain, the domain of DVar is replaced by NewDom, the
   appropriate suspension lists in its attribute are passed to the waking
   scheduler and so is the constrained list in the suspend attribute of the
   domain variable.  If the size of the new domain is equal to the old one,
   no updates and no waking is done, i.e.  this predicate does not check an
   explicit equality of both domains.  If the size of the new domain is
   greater than the old one, an error is raised.

<P>
"),
	args:["+DVar" : "A domain variable.", "+NewDom" : "A finite domain."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(dvar_attribute, 2), /(::, 2), /(#::, 2), /(dvar_replace, 2)]]).

:- comment(/(element, 3), [
	summary:"Value is the Index'th element of the integer list List.

",
	template:"element(?Index, +List, ?Value)",
	desc:html("   This constraints can be used in a variety of programs to state a
   relation between two domain variables.  List is a list of integers and
   the constraint states that its Index'th element is equal to Value, i.e.

<P>
<PRE>
                             List_Index = Value
</PRE>
   Every time Index or Value is updated, this constraint is activated and
   the domain of the other variable is updated accordingly.

<P>
"),
	args:["?Index" : "A variable or an integer.", "+List" : "A non-empty list of integers.", "?Value" : "A variable or an integer."],
	resat:"   No.",
	fail_if:"   Fails if Value is not the Index'th element of List.\n\n",
	eg:"
   [eclipse 13]: element(I, [1,3,6,3], V).
   I = I :: [1 .. 4]
   V = V :: [1, 3, 6]
   Delayed goals:
   element(I :: [1 .. 4], t(1, 3, 6, 3), V :: [1, 3, 6], 4, 3)
   yes.
   [eclipse 14]: element(I, [1,3,6,3], V), V ## 3.
   I = I :: [1, 3]
   V = V :: [1, 6]
   Delayed goals:
   element(I :: [1, 3], t(1, 3, 6, 3), V :: [1, 6], 2, 2)
   yes.



",
	see_also:[/(::, 2), /(#::, 2), /(atmost, 3)]]).

:- comment(/(#=, 2), [
	summary:"X is equal to Y.

",
	template:"?X #= ?Y",
	desc:html("   This constraints states that the two linear terms are equal.  It is
   activated whenever the maximum or minimum of a domain variable is
   updated that might require updating other domains.  When propagating
   domain updates, the system takes into account only maximum and minimum
   values of the whole domain and makes sure that these values are
   consistent with those of other domain variables.

<P>
   If one of the arguments is a non-linear polynomial, this predicate
   delays until it becomes linear.

<P>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term."],
	resat:"   No.",
	fail_if:"   Fails if the two linear terms have a different value.\n\n",
	see_also:[/(::, 2), /(#::, 2), /(##, 2)]]).

:- comment(/(#=, 3), [
	summary:"The constraint X #= Y has the truth value B.

",
	template:"#=(?X, ?Y, ?B)",
	desc:html("   This predicate is an evaluation constraint:  it succeeds if and only if
   the truth value of its associated constraint (the constraint with arity
   one less and the same arguments except for B) is equal to B, where the
   value 0 means false and 1 true.  This constraint can be used both to
   test the validity of the associated constraint (entailment test) and to
   impose this constraint or its negation.  For the former, B will be
   instantiated as soon as either the associated constraint or its negation
   is subsumed by the current state of its domain variables.  For the
   latter, when B is instantiated, then depending on its value, either the
   associated constraint or its negation will be imposed on its arguments.

<P>
   Boolean expressions containing arithmetic constraints are converted by
   the system to a series of evaluation constraints and arithmetic
   constraints in a way similar to arithmetic evaluation, e.g.

<P>
<PRE>
    X #&gt; Y #=&gt; T #&gt; V #/\\ E #= F
</PRE>
   is transformed into

<P>
<PRE>
    #&gt;(X, Y, B1),
    #&gt;(T, V, B2),
    #=(E, F, B3),
    #=(B2 + B3, 2, B4),
    B1 #&lt;= B4.      % i.e.  if B1 then B4
</PRE>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(#>=, 3), /(#<, 3), /(#<=, 3), /(#=, 3), /(#\=, 3)]]).

:- comment(/(#<=>, 2), [
	summary:"The constraint expression C1 is equivalent to the constraint expression C2.

",
	template:"?C1 #<=> ?C2",
	desc:html("   This constraint states that the constraint expressions C1 and C2
   evaluate to the same truth value, either both true of both false.  If
   this is already the case, it simply succeeds.  Otherwise it is suspended
   and after each domain change that may cause one of the constraint
   expressions to succeed to fail, it is woken and and re-evaluated.
   Subsequently, if one of the constraint expressions is true or false, the
   same truth value is imposed on the other one.

<P>
"),
	args:["?C1" : "An arithmetic constraint expression.", "?C2" : "An arithmetic constraint expression."],
	resat:"   No.",
	fail_if:"   Fails if C1 and C2 are not bothe true or both false.\n\n",
	see_also:[/(#\/, 2), /(#/\, 2), /(#=>, 2), /(#\+, 1)]]).

:- comment(/(#<=>, 3), [
	summary:"The constraint X #<=> Y has the truth value B.

",
	template:"#<=>(?X, ?Y, ?B)",
	desc:html("   This predicate is an evaluation constraint:  it succeeds if and only if
   the truth value of its associated constraint (the constraint with arity
   one less and the same arguments except for B) is equal to B, where the
   value 0 means false and 1 true.  This constraint can be used both to
   test the validity of the associated constraint (entailment test) and to
   impose this constraint or its negation.  For the former, B will be
   instantiated as soon as either the associated constraint or its negation
   is subsumed by the current state of its domain variables.  For the
   latter, when B is instantiated, then depending on its value, either the
   associated constraint or its negation will be imposed on its arguments.

<P>
   Boolean expressions containing arithmetic constraints are converted by
   the system to a series of evaluation constraints and arithmetic
   constraints in a way similar to arithmetic evaluation, e.g.

<P>
<PRE>
    X #&gt; Y #=&gt; T #&gt; V #/\\ E #= F
</PRE>
   is transformed into

<P>
<PRE>
    #&gt;(X, Y, B1),
    #&gt;(T, V, B2),
    #=(E, F, B3),
    #=(B2 + B3, 2, B4),
    B1 #&lt;= B4.      % i.e.  if B1 then B4
</PRE>
"),
	args:["?X" : "A constraint expression.", "?Y" : "A constraint expression.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(#\+, 2), /(#/\, 3), /(#\/, 3), /(#=>, 3), /(isd, 2)]]).

:- comment(/(fd_eval, 1), [
	summary:"Evaluate and state the constraint expression C.

",
	template:"fd_eval(?C)",
	desc:html("   This predicate is used to enforce the evaluation of a given constraint
   expression on runtime, without macro expansion.  This can be useful in
   situation where the compile-time macro expansion cannot process its
   argument properly, because it has no type and mode information about the
   variable arguments.

<P>
"),
	args:["?C" : "An arithmetic constraint expression."],
	resat:"   No.",
	fail_if:"   Fails C fails.\n\n",
	see_also:[/(isd, 2)]]).

:- comment(/(#>, 2), [
	summary:"The linear term X is greater than the linear term Y.

",
	template:"?X #> ?Y",
	desc:html("   This constraint states that the linear term X is greater than the linear
   term Y. It is activated whenever the maximum or minimum of a domain
   variable is updated that might require updating other domains.  When
   propagating domain updates, the system takes into account only maximum
   and minimum values of the whole domain and makes sure that these values
   are consistent with those of other domain variables.

<P>
   If one of the arguments is a non-linear polynomial, this predicate
   delays until it becomes linear.

<P>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term."],
	resat:"   No.",
	fail_if:"   Fails if X is not greater than Y.\n\n",
	see_also:[/(#>=, 2), /(#<, 2), /(#<=, 2), /(#=, 2), /(##, 2)]]).

:- comment(/(#>, 3), [
	summary:"The constraint X #> Y has the truth value B.

",
	template:"#>(?X, ?Y, ?B)",
	desc:html("   This predicate is an evaluation constraint:  it succeeds if and only if
   the truth value of its associated constraint (the constraint with arity
   one less and the same arguments except for B) is equal to B, where the
   value 0 means false and 1 true.  This constraint can be used both to
   test the validity of the associated constraint (entailment test) and to
   impose this constraint or its negation.  For the former, B will be
   instantiated as soon as either the associated constraint or its negation
   is subsumed by the current state of its domain variables.  For the
   latter, when B is instantiated, then depending on its value, either the
   associated constraint or its negation will be imposed on its arguments.

<P>
   Boolean expressions containing arithmetic constraints are converted by
   the system to a series of evaluation constraints and arithmetic
   constraints in a way similar to arithmetic evaluation, e.g.

<P>
<PRE>
    X #&gt; Y #=&gt; T #&gt; V #/\\ E #= F
</PRE>
   is transformed into

<P>
<PRE>
    #&gt;(X, Y, B1),
    #&gt;(T, V, B2),
    #=(E, F, B3),
    #=(B2 + B3, 2, B4),
    B1 #&lt;= B4.      % i.e.  if B1 then B4
</PRE>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(#>=, 3), /(#<, 3), /(#<=, 3), /(#=, 3), /(#\=, 3)]]).

:- comment(/(#>=, 2), [
	summary:"The linear term X is greater than or equal to the linear term Y.

",
	template:"?X #>= ?Y",
	desc:html("   This constraint states that the linear term X is greater than or equal
   to the linear term Y. It is activated whenever the maximum or minimum of
   a domain variable is updated that might require updating other domains.
   When propagating domain updates, the system takes into account only
   maximum and minimum values of the whole domain and makes sure that these
   values are consistent with those of other domain variables.

<P>
   If one of the arguments is a non-linear polynomial, this predicate
   delays until it becomes linear.

<P>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term."],
	resat:"   No.",
	fail_if:"   Fails if X is less than Y.\n\n",
	see_also:[/(#>, 2), /(#<, 2), /(#<=, 2), /(#=, 2), /(##, 2)]]).

:- comment(/(#>=, 3), [
	summary:"The constraint X #>= Y has the truth value B.

",
	template:"#>=(?X, ?Y, ?B)",
	desc:html("   This predicate is an evaluation constraint:  it succeeds if and only if
   the truth value of its associated constraint (the constraint with arity
   one less and the same arguments except for B) is equal to B, where the
   value 0 means false and 1 true.  This constraint can be used both to
   test the validity of the associated constraint (entailment test) and to
   impose this constraint or its negation.  For the former, B will be
   instantiated as soon as either the associated constraint or its negation
   is subsumed by the current state of its domain variables.  For the
   latter, when B is instantiated, then depending on its value, either the
   associated constraint or its negation will be imposed on its arguments.

<P>
   Boolean expressions containing arithmetic constraints are converted by
   the system to a series of evaluation constraints and arithmetic
   constraints in a way similar to arithmetic evaluation, e.g.

<P>
<PRE>
    X #&gt; Y #=&gt; T #&gt; V #/\\ E #= F
</PRE>
   is transformed into

<P>
<PRE>
    #&gt;(X, Y, B1),
    #&gt;(T, V, B2),
    #=(E, F, B3),
    #=(B2 + B3, 2, B4),
    B1 #&lt;= B4.      % i.e.  if B1 then B4
</PRE>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(#>=, 3), /(#<, 3), /(#<=, 3), /(#=, 3), /(#\=, 3)]]).

:- comment(/(#=>, 2), [
	summary:"The constraint expression C1 implies the constraint expression C2.

",
	template:"?C1 #=> ?C2",
	desc:html("   This constraint states that the constraint expression C1 implies the
   constraint expression C2.  If this is already the case, it simply
   succeeds.  Otherwise it is suspended and after each domain change that
   may cause C1 to succeed, it is woken and re-evaluated.  If C1 is true,
   C2 is imposed as a constraint.

<P>
"),
	args:["?C1" : "An arithmetic constraint expression.", "?C2" : "An arithmetic constraint expression."],
	resat:"   No.",
	fail_if:"   Fails if C1 is true and C2 is false.\n\n",
	see_also:[/(#\/, 2), /(#/\, 2), /(#<=>, 2), /(#\+, 1)]]).

:- comment(/(#=>, 3), [
	summary:"The constraint X #=> Y has the truth value B.

",
	template:"#=>(?X, ?Y, ?B)",
	desc:html("   This predicate is an evaluation constraint:  it succeeds if and only if
   the truth value of its associated constraint (the constraint with arity
   one less and the same arguments except for B) is equal to B, where the
   value 0 means false and 1 true.  This constraint can be used both to
   test the validity of the associated constraint (entailment test) and to
   impose this constraint or its negation.  For the former, B will be
   instantiated as soon as either the associated constraint or its negation
   is subsumed by the current state of its domain variables.  For the
   latter, when B is instantiated, then depending on its value, either the
   associated constraint or its negation will be imposed on its arguments.

<P>
   Boolean expressions containing arithmetic constraints are converted by
   the system to a series of evaluation constraints and arithmetic
   constraints in a way similar to arithmetic evaluation, e.g.

<P>
<PRE>
    X #&gt; Y #=&gt; T #&gt; V #/\\ E #= F
</PRE>
   is transformed into

<P>
<PRE>
    #&gt;(X, Y, B1),
    #&gt;(T, V, B2),
    #=(E, F, B3),
    #=(B2 + B3, 2, B4),
    B1 #&lt;= B4.      % i.e.  if B1 then B4
</PRE>
"),
	args:["?X" : "A constraint expression.", "?Y" : "A constraint expression.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(#\+, 2), /(#/\, 3), /(#\/, 3), /(#<=>, 3), /(isd, 2)]]).

:- comment(/(indomain, 1), [
	summary:"Instantiate Var to a value in its domain.

",
	template:"indomain(?Var)",
	desc:html("   This predicate instantiates the domain variable Var to a value from its
   domain.  Its starts with the smallest element in the domain and on
   backtracking successive elements are taken.  It is used mostly to find
   an instantiation of the variable which is consistent with the current
   set of constraints in labeling procedures.  If List is a list of all
   domain variables occurring in the program, the simplest labeling
   procedure is written as

<P>
<PRE>
        labeling([]).
        labeling([Var|Rest]) :-
            indomain(Var),
            labeling(Rest).
</PRE>
"),
	args:["?Var" : "An integer or a domain variable"],
	resat:"   Yes.",
	fail_if:"   None.\n\n",
	see_also:[/(::, 2), /(#::, 2), /(par_indomain, 1), labeling/1, fd_search:search/6]]).

:- comment(/(integer_list_to_dom, 2), [
	summary:"Convert a sorted list of integers and integer intervals into a domain Dom.

",
	template:"integer_list_to_dom(+List, -Dom)",
	desc:html("   Convert a sorted list of integers and integer intervals into a domain
   Dom.  List must be sorted and integers must not overlap with intervals.
   Note that using sort/2 on a list of integers and intervals does not
   produce correct results because intervals are considered compound terms
   and thus sorted after all integers.  This predicate converts a list of
   successive integers into an interval and/or merges it with adjacent
   intervals.  Typically, it will be used to convert a sorted list of
   integers into a list of intervals and an appropriate domain.  If the
   list is already known to contain the right intervals, it is quicker to
   use sorted_list_to_dom/2.

<P>
"),
	args:["+List" : "A list of integers and integer interval.", "-Dom" : "Variable."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[4 : "List is not ground.", 5 : "List contains an element which is neither integer nor an    integer interval.", 6 : "The size of the resulting domain is too large.", 6 : "The list is not sorted properly."],
	see_also:[/(sorted_list_to_dom, 2), /(dvar_domain, 2)]]).

:- comment(/(is_domain, 1), [
	summary:"Succeeds if Term is a domain variable.

",
	template:"is_domain(?Term)",
	desc:html("   This predicate is used to test if a term is a domain variable.

<P>
"),
	args:["?Term" : "A Prolog term."],
	resat:"   No.",
	fail_if:"   Fails if Term is not a domain variable.\n\n",
	see_also:[/(::, 2), /(#::, 2)]]).

:- comment(/(is_integer_domain, 1), [
	summary:"Succeeds if Term is a domain variable with an integer domain.

",
	template:"is_integer_domain(?Term)",
	desc:html("   This predicate is used to test if a term is a domain variable with an
   integer domain.

<P>
"),
	args:["?Term" : "A Prolog term."],
	resat:"   No.",
	fail_if:"   Fails if Term is not an integer domain variable.\n\n",
	see_also:[/(is_domain, 1), /(::, 2), /(#::, 2)]]).

:- comment(/(isd, 2), [
	summary:"The constraint expression C evaluates to the boolean value B.

",
	template:"?B isd ?C",
	desc:html("   This is an evaluation constraint.  It states that the constraint
   expression C evaluates to the boolean value B, where the value 0 means
   false and 1 true.  This constraint can be used both to test the validity
   of the constraint expression (entailment test) and to impose this
   constraint or its negation.  For the former, B will be instantiated as
   soon as either the constraint expression or its negation is subsumed by
   the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the constraint
   expression or its negation will be imposed on its arguments.

<P>
"),
	args:["?B" : "A variable with domain 0..1.", "?C" : "An arithmetic constraint expression."],
	resat:"   No.",
	fail_if:"   Fails if both C is true.\n\n",
	see_also:[/(#=>, 3), /(#<=>, 3), /(#\/, 3), /(#/\, 3), /(is, 2)]]).

:- comment(/(list_to_dom, 2), [
	summary:"Convert a list of atomic terms and integer intervals into a domain Dom.

",
	template:"list_to_dom(+List, -Dom)",
	desc:html("   Convert a list of atomic terms and integer intervals into a domain Dom.
   List does not have to be sorted and integers and intervals may overlap.

<P>
"),
	args:["+List" : "A list of atomic terms and integer interval.", "-Dom" : "Variable."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[4 : "List is not ground.", 5 : "List contains an element which is neither atomic nor an    integer interval.", 6 : "The size of the resulting domain is too large.", 6 : "The lower bound of an integer interval is greater than its    upper bound."],
	see_also:[/(sorted_list_to_dom, 2), /(dvar_domain, 2)]]).

:- comment(/(#<, 2), [
	summary:"The linear term X is less than the linear term Y.

",
	template:"?X #< ?Y",
	desc:html("   This constraint states that the linear term X is less than the linear
   term Y. It is activated whenever the maximum or minimum of a domain
   variable is updated that might require updating other domains.  When
   propagating domain updates, the system takes into account only maximum
   and minimum values of the whole domain and makes sure that these values
   are consistent with those of other domain variables.

<P>
   If one of the arguments is a non-linear polynomial, this predicate
   delays until it becomes linear.

<P>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term."],
	resat:"   No.",
	fail_if:"   Fails if X is not less than Y.\n\n",
	see_also:[/(#>=, 2), /(#>, 2), /(#<=, 2), /(#=, 2), /(##, 2)]]).

:- comment(/(#<, 3), [
	summary:"The constraint X #< Y has the truth value B.

",
	template:"#<(?X, ?Y, ?B)",
	desc:html("   This predicate is an evaluation constraint:  it succeeds if and only if
   the truth value of its associated constraint (the constraint with arity
   one less and the same arguments except for B) is equal to B, where the
   value 0 means false and 1 true.  This constraint can be used both to
   test the validity of the associated constraint (entailment test) and to
   impose this constraint or its negation.  For the former, B will be
   instantiated as soon as either the associated constraint or its negation
   is subsumed by the current state of its domain variables.  For the
   latter, when B is instantiated, then depending on its value, either the
   associated constraint or its negation will be imposed on its arguments.

<P>
   Boolean expressions containing arithmetic constraints are converted by
   the system to a series of evaluation constraints and arithmetic
   constraints in a way similar to arithmetic evaluation, e.g.

<P>
<PRE>
    X #&gt; Y #=&gt; T #&gt; V #/\\ E #= F
</PRE>
   is transformed into

<P>
<PRE>
    #&gt;(X, Y, B1),
    #&gt;(T, V, B2),
    #=(E, F, B3),
    #=(B2 + B3, 2, B4),
    B1 #&lt;= B4.      % i.e.  if B1 then B4
</PRE>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(#>=, 3), /(#<, 3), /(#<=, 3), /(#=, 3), /(#\=, 3)]]).

:- comment(/(#<=, 2), [
	summary:"The linear term X is less than or equal to the linear term Y.

",
	template:"?X #<= ?Y",
	desc:html("   This constraint states that the linear term X is less than or equal to
   the linear term Y. It is activated whenever the maximum or minimum of a
   domain variable is updated that might require updating other domains.
   When propagating domain updates, the system takes into account only
   maximum and minimum values of the whole domain and makes sure that these
   values are consistent with those of other domain variables.

<P>
   If one of the arguments is a non-linear polynomial, this predicate
   delays until it becomes linear.

<P>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term."],
	resat:"   No.",
	fail_if:"   Fails if X is greater than Y.\n\n",
	see_also:[/(#>, 2), /(#<, 2), /(#>=, 2), /(#=, 2), /(##, 2)]]).

:- comment(/(#<=, 3), [
	summary:"The constraint X #<= Y has the truth value B.

",
	template:"#<=(?X, ?Y, ?B)",
	desc:html("   This predicate is an evaluation constraint:  it succeeds if and only if
   the truth value of its associated constraint (the constraint with arity
   one less and the same arguments except for B) is equal to B, where the
   value 0 means false and 1 true.  This constraint can be used both to
   test the validity of the associated constraint (entailment test) and to
   impose this constraint or its negation.  For the former, B will be
   instantiated as soon as either the associated constraint or its negation
   is subsumed by the current state of its domain variables.  For the
   latter, when B is instantiated, then depending on its value, either the
   associated constraint or its negation will be imposed on its arguments.

<P>
   Boolean expressions containing arithmetic constraints are converted by
   the system to a series of evaluation constraints and arithmetic
   constraints in a way similar to arithmetic evaluation, e.g.

<P>
<PRE>
    X #&gt; Y #=&gt; T #&gt; V #/\\ E #= F
</PRE>
   is transformed into

<P>
<PRE>
    #&gt;(X, Y, B1),
    #&gt;(T, V, B2),
    #=(E, F, B3),
    #=(B2 + B3, 2, B4),
    B1 #&lt;= B4.      % i.e.  if B1 then B4
</PRE>
"),
	args:["?X" : "A linear term.", "?Y" : "A linear term.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(#>=, 3), /(#<, 3), /(#<=, 3), /(#=, 3), /(#\=, 3)]]).

:- comment(/(maxdomain, 2), [
	summary:"Max is the maximum element in the domain of Var.

",
	template:"maxdomain(+Var, ?Max)",
	desc:html("   If Var is a domain variable then Max is unified with the maximum value
   in the domain of Var.  If Var is an integer then Max is unified with
   Var.

<P>
"),
	args:["+Var" : "A domain variable or an integer.", "?Max" : "A variable or an integer."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(mindomain, 2), /(::, 2), /(#::, 2), /(dom_to_list, 2)]]).

:- comment(/(minimize_bound_check, 0), [
	summary:"If within a minimization, force a check of the cost variable against
the cost bound, which may lead to earlier failure.

",
	template:"minimize_bound_check",
	desc:html("   This predicate can be used within a goal that is being minimized
   using minimize/2 or any of its variants. If used in other contexts
   it has no effect just succeeds.

<P>
   The behaviour of minimize/2 is such that the cost bound is tightened
   whenever a better solution is found, but this tightening does not
   trigger an automatic check.  It can therefore be advantageous and
   lead to earlier failure when minimize_bound_check/0 is called after
   every labeling step.

<P>
"),
	args:[],
	resat:"   No.",
	fail_if:"   Fails if the current cost cannot become smaller that the cost bound.\n\n",
	eg:"
    labeling([]).
    labeling([X|Xs]) :-
\t% do the check urgently, before any propagation happens
\tcall_priority((indomain(X), minimize_bound_check), 2),
\tlabeling(Xs).




",
	see_also:[/(minimize, 2), /(minimize, 4), /(minimize, 6), /(minimize, 8)]]).

:- comment(/(min_max, 2), [
	summary:"Find the solution of Goal that minimizes the maximum of elements of C.

",
	template:"min_max(?Goal, ?C)",
	desc:html("   If C is a linear term, a solution of the goal Goal is found that
   minimizes the value of C. If C is a list of linear terms, the returned
   solution minimizes the maximum value of terms in the list.  The solution
   is found using the branch and bound method; as soon as a partial
   solution is found that is worse than a previous solution, the search is
   abandoned and a new solution is searched for.  Every time a new better
   solution is found, the event 280 is raised, its default handler prints
   the current cost.

<P>
"),
	args:["+Goal" : "A callable term.", "?C" : "A linear term or a list of linear terms."],
	resat:"   No.",
	fail_if:"   Fails if there is no solution to Goal.\n\n",
	see_also:[/(min_max, 4), /(min_max, 5), /(min_max, 6), /(min_max, 8), /(minimize, 2), /(minimize, 4), /(minimize, 5), /(minimize, 6), /(minimize, 8), /(deleteff, 3)]]).

:- comment(/(min_max, 4), [
	summary:"Find the solution of Goal that minimizes the maximum of elements of C,
and unify the minimized Template with Solution.

",
	template:"min_max(+Goal, ?Template, ?Solution, ?C)",
	desc:html("   If C is a linear term, a solution of the goal Goal is found that
   minimizes the value of C. If C is a list of linear terms, the returned
   solution minimizes the maximum value of terms in the list.  The solution
   is found using the branch and bound method; as soon as a partial
   solution is found that is worse than a previous solution, the search is
   abandoned and a new solution is searched for.  Every time a new better
   solution is found, the event 280 is raised, its default handler prints
   the current cost.

<P>
   Solutions will be unified with a copy of Template where the variables
   are replaced with their minimized values. Typically, the Template will
   contain all or a subset of Goal's variables.

<P>
   min_max/2 can be written in terms of min_max/4 as follows:

<P>
<PRE>
\tmin_max(Goal, Cost) :-
\t    min_max(Goal, Goal, Goal, Cost).
</PRE>
"),
	args:["+Goal" : "A callable term.", "?Template" : "A term containing all or some of Goal's variables", "?Solution" : "Term to be unified with the minimized Template", "?C" : "A linear term or a list of linear terms."],
	resat:"   No.",
	fail_if:"   Fails if there is no solution to Goal.\n\n",
	eg:"
    % Find the minimal C and bind X to the corresponding value
    [eclipse]: X::1..3, C #= 3-X, min_max(indomain(X), X, X, C).
    Found a solution with cost 2
    Found a solution with cost 1
    Found a solution with cost 0
    X = 3
    C = 0
    yes.

    % Find the minimal C and don't bind anything
    [eclipse]: X::1..3, C #= 3-X, min_max(indomain(X), [], [], C).
    Found a solution with cost 2
    Found a solution with cost 1
    Found a solution with cost 0
    X = X{[1..3]}
    C = C{[0..2]}

    Delayed goals:
\t    -3 + X{[1..3]} + C{[0..2]}#=0
    yes.

    % Find the minimal C and return it in MinC. Don't bind X or C.
    [eclipse]: X::1..3, C #= 3-X, min_max(indomain(X), C, MinC, C).
    Found a solution with cost 2
    Found a solution with cost 1
    Found a solution with cost 0
    X = X{[1..3]}
    MinC = 0
    C = C{[0..2]}

    Delayed goals:
\t    -3 + X{[1..3]} + C{[0..2]}#=0
    yes.




",
	see_also:[/(min_max, 2), /(min_max, 5), /(min_max, 6), /(min_max, 8), /(minimize, 2), /(minimize, 4), /(minimize, 5), /(minimize, 6), /(minimize, 8)]]).

:- comment(/(min_max, 5), [
	summary:"Find the solution of Goal that minimizes the maximum of elements of C,
within the bounds set by Lower,Upper and Percent.

",
	template:"min_max(?Goal, ?C, +Lower, +Upper, +Percent)",
	desc:html("   If C is a linear term, a solution of the goal Goal is found that
   minimizes the value of C. If C is a list of linear terms, the returned
   solution minimizes the maximum value of terms in the list.  The solution
   is found using the branch and bound method; as soon as a partial
   solution is found that is worse than a previous solution, the search is
   abandoned and a new solution is searched for.  The starting assumption
   is that the value to minimize is less than Upper and that any value less
   than Lower can be considered as a final solution.  Moreover, solutions
   whose minimized values are closer than Percent % are considered equal.
   Every time a new better solution is found, the event 280 is raised, its
   default handler prints the current cost.

<P>
   This predicate is to be used for optimization problems when the whole
   search space is too large or when a suboptimal solution is sufficient.

<P>
"),
	args:["?Goal" : "A callable term.", "?C" : "A linear term or a list of linear terms.", "+Lower" : "An integer.", "+Upper" : "An integer.", "+Percent" : "An integer."],
	resat:"   No.",
	fail_if:"   Fails if there is no solution to Goal.\n\n",
	see_also:[/(min_max, 2), /(min_max, 4), /(min_max, 6), /(min_max, 8), /(minimize, 2), /(minimize, 4), /(minimize, 5), /(minimize, 6), /(minimize, 8)]]).

:- comment(/(min_max, 6), [
	summary:"Find the solution of Goal that minimizes the maximum of elements of C,
within the bounds set by Lower,Upper and Percent in time not longer than
Timeout.

",
	template:"min_max(?Goal, ?C, +Lower, +Upper, +Percent, +Timeout)",
	desc:html("   If C is a linear term, a solution of the goal Goal is found that
   minimizes the value of C. If C is a list of linear terms, the returned
   solution minimizes the maximum value of terms in the list.  The solution
   is found using the branch and bound method; as soon as a partial
   solution is found that is worse than a previous solution, the search is
   abandoned and a new solution is searched for.  The starting assumption
   is that the value to minimize is less than Upper and that any value less
   than Lower can be considered as a final solution.  Moreover, solutions
   whose minimized values are closer than Percent % are considered equal.
   Every time a new better solution is found, the event 280 is raised, its
   default handler prints the current cost.

<P>
   If Timeout is not zero, the predicate will stop after Timeout seconds
   and report the best solution it has found so far.  Calls with specified
   Timeout cannot be nested.

<P>
   This predicate is to be used for optimization problems when the whole
   search space is too large or when a suboptimal solution is sufficient.

<P>
"),
	args:["?Goal" : "A callable term.", "?C" : "A linear term or a list of linear terms.", "+Lower" : "An integer.", "+Upper" : "An integer.", "+Percent" : "An integer.", "+Timeout" : "A number."],
	resat:"   No.",
	fail_if:"   Fails if there is no solution to Goal.\n\n",
	see_also:[/(min_max, 2), /(min_max, 4), /(min_max, 5), /(min_max, 8), /(minimize, 2), /(minimize, 4), /(minimize, 5), /(minimize, 6), /(minimize, 8)]]).

:- comment(/(min_max, 8), [
	summary:"Find the solution of Goal that minimizes the maximum of elements of C,
within the bounds set by Lower,Upper and Percent in time not longer than
Timeout.

",
	template:"min_max(+Goal, ?Template, ?Solution, ?C, +Lower, +Upper, +Percent, +Timeout)",
	desc:html("   This is the most general version of the min_max predicate with all
   options.

<P>
   If C is a linear term, a solution of the goal Goal is found that
   minimizes the value of C. If C is a list of linear terms, the returned
   solution minimizes the maximum value of terms in the list.  The solution
   is found using the branch and bound method; as soon as a partial
   solution is found that is worse than a previous solution, the search is
   abandoned and a new solution is searched for.  The starting assumption
   is that the value to minimize is less than Upper and that any value less
   than Lower can be considered as a final solution.  Moreover, solutions
   whose minimized values are closer than Percent % are considered equal.
   Every time a new better solution is found, the event 280 is raised, its
   default handler prints the current cost.

<P>
   Solutions will be unified with a copy of Template where the variables
   are replaced with their minimized values. Typically, the Template will
   contain all or a subset of Goal's variables.

<P>
   If Timeout is not zero, the predicate will stop after Timeout seconds
   and report the best solution it has found so far.  Calls with specified
   Timeout cannot be nested.

<P>
   All other variants of min_max can be written in terms of min_max/9, eg.

<P>
<PRE>
\tmin_max(Goal, Cost) :-
\t    minint(Min), maxint(Max),
\t    min_max(Goal, Goal, Goal, Cost, Min, Max, 0, 0).
</PRE>
"),
	args:["+Goal" : "A callable term.", "?Template" : "A term containing all or some of Goal's variables", "?Solution" : "Term to be unified with the minimized Template", "?C" : "A linear term or a list of linear terms.", "+Lower" : "An integer.", "+Upper" : "An integer.", "+Percent" : "An integer.", "+Timeout" : "A number."],
	resat:"   No.",
	fail_if:"   Fails if there is no solution to Goal.\n\n",
	eg:"
    % Find the minimal C and bind X to the corresponding value
    [eclipse]: X::1..3, C #= 3-X, min_max(indomain(X), X, X, C).
    Found a solution with cost 2
    Found a solution with cost 1
    Found a solution with cost 0
    X = 3
    C = 0
    yes.

    % Find the minimal C and don't bind anything
    [eclipse]: X::1..3, C #= 3-X, min_max(indomain(X), [], [], C).
    Found a solution with cost 2
    Found a solution with cost 1
    Found a solution with cost 0
    X = X{[1..3]}
    C = C{[0..2]}

    Delayed goals:
\t    -3 + X{[1..3]} + C{[0..2]}#=0
    yes.

    % Find the minimal C and return it in MinC. Don't bind X or C.
    [eclipse]: X::1..3, C #= 3-X, min_max(indomain(X), C, MinC, C).
    Found a solution with cost 2
    Found a solution with cost 1
    Found a solution with cost 0
    X = X{[1..3]}
    MinC = 0
    C = C{[0..2]}

    Delayed goals:
\t    -3 + X{[1..3]} + C{[0..2]}#=0
    yes.




",
	see_also:[/(min_max, 2), /(min_max, 4), /(min_max, 5), /(min_max, 6), /(minimize, 2), /(minimize, 4), /(minimize, 5), /(minimize, 6), /(minimize, 8)]]).

:- comment(/(mindomain, 2), [
	summary:"Min is the minimum element in the domain of Var.

",
	template:"mindomain(+Var, ?Min)",
	desc:html("   If Var is a domain variable then Min is unified with the minimum value
   in the domain of Var.  If Var is an integer then Min is unified with
   Var.

<P>
"),
	args:["+Var" : "A domain variable or an integer.", "?Min" : "A variable or an integer."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(maxdomain, 2), /(::, 2), /(#::, 2), /(dom_to_list, 2)]]).

:- comment(/(minimize, 2), [
	summary:"Find the solution of Goal that minimizes C.

",
	template:"minimize(?Goal, ?C)",
	desc:html("   A solution of the goal Goal is found that minimizes the value of C. 
   The solution is found using the branch and bound method.  Whenever
   a better solution is found, the upper cost bound is tightened and
   the search for a better solution continues.  Every time a new
   better solution is found, the event 280 is raised, its default
   handler prints the current cost.

<P>
   Note:  Operationally, the difference with min_max/2 is that, after
   finding a solution, the search is not abandoned and restarted, but
   continued with a tightened cost bound.

<P>
"),
	args:["+Goal" : "A callable term.", "?C" : "A linear term."],
	resat:"   No.",
	fail_if:"   Fails if there is no solution to Goal.\n\n",
	see_also:[/(min_max, 2), /(min_max, 4), /(min_max, 5), /(min_max, 6), /(min_max, 8), /(minimize, 4), /(minimize, 5), /(minimize, 6), /(minimize, 8), /(minimize_bound_check, 0)]]).

:- comment(/(minimize, 4), [
	summary:"Find the solution of Goal that minimizes C,
and unify the minimized Template with Solution.

",
	template:"minimize(+Goal, ?Template, ?Solution, ?C)",
	desc:html("   A solution of the goal Goal is found that minimizes the value of C. 
   The solution is found using the branch and bound method.  Whenever
   a better solution is found, the upper cost bound is tightened and
   the search for a better solution continues.  Every time a new
   better solution is found, the event 280 is raised, its default
   handler prints the current cost.

<P>
   Solutions will be unified with a copy of Template where the variables
   are replaced with their minimized values. Typically, the Template will
   contain all or a subset of Goal's variables.

<P>
   minimize/2 can be written in terms of minimize/4 as follows:

<P>
<PRE>
\tminimize(Goal, Cost) :-
\t    minimize(Goal, Goal, Goal, Cost).
</PRE>
"),
	args:["+Goal" : "A callable term.", "?Template" : "A term containing all or some of Goal's variables", "?Solution" : "Term to be unified with the minimized Template", "?C" : "A linear term or a list of linear terms."],
	resat:"   No.",
	fail_if:"   Fails if there is no solution to Goal.\n\n",
	eg:"
    % Find the minimal C and bind X to the corresponding value
    [eclipse]: X::1..3, C #= 3-X, minimize(indomain(X), X, X, C).
    Found a solution with cost 2
    Found a solution with cost 1
    Found a solution with cost 0
    X = 3
    C = 0
    yes.

    % Find the minimal C and don't bind anything
    [eclipse]: X::1..3, C #= 3-X, minimize(indomain(X), [], [], C).
    Found a solution with cost 2
    Found a solution with cost 1
    Found a solution with cost 0
    X = X{[1..3]}
    C = C{[0..2]}

    Delayed goals:
\t    -3 + X{[1..3]} + C{[0..2]}#=0
    yes.

    % Find the minimal C and return it in MinC. Don't bind X or C.
    [eclipse]: X::1..3, C #= 3-X, minimize(indomain(X), C, MinC, C).
    Found a solution with cost 2
    Found a solution with cost 1
    Found a solution with cost 0
    X = X{[1..3]}
    MinC = 0
    C = C{[0..2]}

    Delayed goals:
\t    -3 + X{[1..3]} + C{[0..2]}#=0
    yes.




",
	see_also:[/(min_max, 2), /(min_max, 4), /(min_max, 5), /(min_max, 6), /(min_max, 8), /(minimize, 2), /(minimize, 5), /(minimize, 6), /(minimize, 8), /(minimize_bound_check, 0)]]).

:- comment(/(minimize, 5), [
	summary:"Find the solution of Goal that minimizes C, within the bounds set by Lower,
Upper and Percent.

",
	template:"minimize(?Goal, ?C, +Lower, +Upper, +Percent)",
	desc:html("   A solution of the goal Goal is found that minimizes the value of C. 
   The solution is found using the branch and bound method.  Whenever
   a better solution is found, the upper cost bound is tightened and
   the search for a better solution continues.

<P>
   The starting assumption is that the value to minimize is less than
   Upper and that any value less than Lower can be considered as a
   solution.  Moreover, solutions whose minimized values are closer
   than Percent % are considered equal.  Every time a new better
   solution is found, the event 280 is raised, its default handler
   prints the current cost.

<P>
   This predicate is to be used for optimization problems when the whole
   search space is too large or when a suboptimal solution is sufficient.

<P>
"),
	args:["?Goal" : "A callable term.", "?C" : "A linear term.", "+Lower" : "Integer.", "+Upper" : "Integer.", "+Percent" : "Integer."],
	resat:"   No.",
	fail_if:"   Fails if Goal fails.\n\n",
	see_also:[/(min_max, 2), /(min_max, 4), /(min_max, 5), /(min_max, 6), /(min_max, 8), /(minimize, 2), /(minimize, 4), /(minimize, 6), /(minimize, 8), /(minimize_bound_check, 0)]]).

:- comment(/(minimize, 6), [
	summary:"Find the solution of Goal that minimizes C, within the bounds set by Lower,
Upper and Percent in time not longer than Timeout.

",
	template:"minimize(?Goal, ?C, +Lower, +Upper, +Percent, +Timeout)",
	desc:html("   A solution of the goal Goal is found that minimizes the value of C. 
   The solution is found using the branch and bound method.  Whenever
   a better solution is found, the upper cost bound is tightened and
   the search for a better solution continues.

<P>
   The starting assumption is that the value to minimize is less than
   Upper and that any value less than Lower can be considered as a
   solution.  Moreover, solutions whose minimized values are closer
   than Percent % are considered equal.  Every time a new better
   solution is found, the event 280 is raised, its default handler
   prints the current cost.

<P>
   If Timeout is not zero, the predicate will stop after Timeout seconds
   and report the best solution it has found so far.  Calls with specified
   Timeout cannot be nested.

<P>
   This predicate is to be used for optimization problems when the whole
   search space is too large or when a suboptimal solution is sufficient.

<P>
"),
	args:["?Goal" : "A callable term.", "?C" : "A linear term.", "+Lower" : "Integer.", "+Upper" : "Integer.", "+Percent" : "Integer.", "+Timeout" : "A number."],
	resat:"   No.",
	fail_if:"   Fails if Goal fails.\n\n",
	see_also:[/(min_max, 2), /(min_max, 4), /(min_max, 5), /(min_max, 6), /(min_max, 8), /(minimize, 2), /(minimize, 4), /(minimize, 5), /(minimize, 8), /(minimize_bound_check, 0)]]).

:- comment(/(minimize, 8), [
	summary:"Find the solution of Goal that minimizes the maximum of elements of C,
within the bounds set by Lower,Upper and Percent in time not longer than
Timeout.

",
	template:"minimize(+Goal, ?Template, ?Solution, ?C, +Lower, +Upper, +Percent, +Timeout)",
	desc:html("   This is the most general version of the minimize predicate with all
   options.

<P>
   A solution of the goal Goal is found that minimizes the value of C. 
   The solution is found using the branch and bound method.  Whenever
   a better solution is found, the upper cost bound is tightened and
   the search for a better solution continues.

<P>
   The starting assumption is that the value to minimize is less than
   Upper and that any value less than Lower can be considered as a
   final solution.  Moreover, solutions whose minimized values are
   closer than Percent % are considered equal.  Every time a new
   better solution is found, the event 280 is raised, its default
   handler prints the current cost.

<P>
   Solutions will be unified with a copy of Template where the variables
   are replaced with their minimized values. Typically, the Template will
   contain all or a subset of Goal's variables.

<P>
   If Timeout is not zero, the predicate will stop after Timeout seconds
   and report the best solution it has found so far.  Calls with specified
   Timeout cannot be nested.

<P>
   All other variants of minimize can be written in terms of minimize/9, eg.

<P>
<PRE>
\tminimize(Goal, Cost) :-
\t    minint(Min), maxint(Max),
\t    minimize(Goal, Goal, Goal, Cost, Min, Max, 0, 0).
</PRE>
"),
	args:["+Goal" : "A callable term.", "?Template" : "A term containing all or some of Goal's variables", "?Solution" : "Term to be unified with the minimized Template", "?C" : "A linear term or a list of linear terms.", "+Lower" : "An integer.", "+Upper" : "An integer.", "+Percent" : "An integer.", "+Timeout" : "A number."],
	resat:"   No.",
	fail_if:"   Fails if there is no solution to Goal.\n\n",
	eg:"
    % Find the minimal C and bind X to the corresponding value
    [eclipse]: X::1..3, C #= 3-X, minimize(indomain(X), X, X, C).
    Found a solution with cost 2
    Found a solution with cost 1
    Found a solution with cost 0
    X = 3
    C = 0
    yes.

    % Find the minimal C and don't bind anything
    [eclipse]: X::1..3, C #= 3-X, minimize(indomain(X), [], [], C).
    Found a solution with cost 2
    Found a solution with cost 1
    Found a solution with cost 0
    X = X{[1..3]}
    C = C{[0..2]}

    Delayed goals:
\t    -3 + X{[1..3]} + C{[0..2]}#=0
    yes.

    % Find the minimal C and return it in MinC. Don't bind X or C.
    [eclipse]: X::1..3, C #= 3-X, minimize(indomain(X), C, MinC, C).
    Found a solution with cost 2
    Found a solution with cost 1
    Found a solution with cost 0
    X = X{[1..3]}
    MinC = 0
    C = C{[0..2]}

    Delayed goals:
\t    -3 + X{[1..3]} + C{[0..2]}#=0
    yes.




",
	see_also:[/(min_max, 2), /(min_max, 4), /(min_max, 5), /(min_max, 6), /(min_max, 8), /(minimize, 2), /(minimize, 4), /(minimize, 5), /(minimize, 6), /(minimize_bound_check, 0)]]).

:- comment(/(new_domain_var, 1), [
	summary:"A user hook predicate which is called whenever a variable obtains a default
domain from the solver.

",
	template:"new_domain_var(-Var)",
	desc:html("   This predicate is a simple hook to notify about free variables being
   converted into domain variables.  Whenever the finite domain solver
   assigns a free variable a domain, it invokes this predicate in the
   module fd_domain with the variable as argument.  To use the mechanism,
   it is necessary to recompile this predicate in the module fd_domain.

<P>
"),
	args:["-Var" : "A variable."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	eg:"
   [eclipse 22]: compile(user, fd_domain).
   new_domain_var(X) :- printf(\"new domain variable created: %mw%n\", [X]).
   user       compiled traceable 88 bytes in 0.00 seconds
   yes.
   [eclipse 23]: X+Y #> T.
   new domain variable created: X[-10000000..10000000]
   new domain variable created: Y[-10000000..10000000]
   new domain variable created: T[-10000000..10000000]



",
	see_also:[/(::, 2), /(#::, 2), /(default_domain, 1)]]).

:- comment(/(#\+, 1), [
	summary:"The constraint expression C is false.

",
	template:"#\\+ ?C",
	desc:html("   This constraint states that the constraint expressions C is false.  If
   this is already the case, it simply succeeds.  Otherwise it is suspended
   and after each domain change that may cause C to succeed, it is woken
   and re-evaluated.  If C is true, the constraint fails.

<P>
"),
	args:["?C" : "An arithmetic constraint expression."],
	resat:"   No.",
	fail_if:"   Fails if both C is true.\n\n",
	see_also:[/(#/\, 2), /(#=>, 2), /(#<=>, 2), /(#\/, 2)]]).

:- comment(/(#\+, 2), [
	summary:"The constraint #\\+ X has the truth value B.

",
	template:"#\\+(?X, ?B)",
	desc:html("   This predicate is an evaluation constraint:  it succeeds if and only if
   the truth value of its associated constraint (the constraint with arity
   one less and the same arguments except for B) is equal to B, where the
   value 0 means false and 1 true.  This constraint can be used both to
   test the validity of the associated constraint (entailment test) and to
   impose this constraint or its negation.  For the former, B will be
   instantiated as soon as either the associated constraint or its negation
   is subsumed by the current state of its domain variables.  For the
   latter, when B is instantiated, then depending on its value, either the
   associated constraint or its negation will be imposed on its arguments.

<P>
   Boolean expressions containing arithmetic constraints are converted by
   the system to a series of evaluation constraints and arithmetic
   constraints in a way similar to arithmetic evaluation, e.g.

<P>
<PRE>
    X #&gt; Y #=&gt; T #&gt; V #/\\ E #= F
</PRE>
   is transformed into

<P>
<PRE>
    #&gt;(X, Y, B1),
    #&gt;(T, V, B2),
    #=(E, F, B3),
    #=(B2 + B3, 2, B4),
    B1 #&lt;= B4.      % i.e.  if B1 then B4
</PRE>
"),
	args:["?X" : "A constraint expression.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(#/\, 3), /(#\/, 3), /(#=>, 3), /(#<=>, 3), /(isd, 2)]]).

:- comment(/(#\/, 2), [
	summary:"The constraint expression C1 or the constraint expression C2 is true.

",
	template:"?C1 #\\/ ?C2",
	desc:html("   This constraint states that at least one of the two constraint
   expressions C1, C2 must be true.  If this is already the case, it simply
   succeeds.  Otherwise it is suspended and after each domain change that
   may cause one of the two expressions to fail, it is woken and
   re-evaluated.  If one of the two expressions is false, the other is
   imposed as a constraint, i.e.  it acts as disjunctive constraint.

<P>
"),
	args:["?C1" : "An arithmetic constraint expression.", "?C2" : "An arithmetic constraint expression."],
	resat:"   No.",
	fail_if:"   Fails if both C1 and C2 are false.\n\n",
	see_also:[/(#/\, 2), /(#=>, 2), /(#<=>, 2), /(#\+, 1)]]).

:- comment(/(#\/, 3), [
	summary:"The constraint X #\\/ Y has the truth value B.

",
	template:"#\\/(?X, ?Y, ?B)",
	desc:html("   This predicate is an evaluation constraint:  it succeeds if and only if
   the truth value of its associated constraint (the constraint with arity
   one less and the same arguments except for B) is equal to B, where the
   value 0 means false and 1 true.  This constraint can be used both to
   test the validity of the associated constraint (entailment test) and to
   impose this constraint or its negation.  For the former, B will be
   instantiated as soon as either the associated constraint or its negation
   is subsumed by the current state of its domain variables.  For the
   latter, when B is instantiated, then depending on its value, either the
   associated constraint or its negation will be imposed on its arguments.

<P>
   Boolean expressions containing arithmetic constraints are converted by
   the system to a series of evaluation constraints and arithmetic
   constraints in a way similar to arithmetic evaluation, e.g.

<P>
<PRE>
    X #&gt; Y #=&gt; T #&gt; V #/\\ E #= F
</PRE>
   is transformed into

<P>
<PRE>
    #&gt;(X, Y, B1),
    #&gt;(T, V, B2),
    #=(E, F, B3),
    #=(B2 + B3, 2, B4),
    B1 #&lt;= B4.      % i.e.  if B1 then B4
</PRE>
"),
	args:["?X" : "A constraint expression.", "?Y" : "A constraint expression.", "?B" : "A variable with domain 0..1."],
	resat:"   No.",
	fail_if:"   Fails if B is not the truth value of the associated constraint.\n\n",
	see_also:[/(#/\, 3), /(#\/, 3), /(#=>, 3), /(#<=>, 3), /(isd, 2)]]).

:- comment(/(par_indomain, 1), [
	summary:"Instantiate Var to a number of values in its domain in parallel.

",
	template:"par_indomain(?Var)",
	desc:html("   This predicate instantiates the domain variable Var to a value from its
   domain.  The instantiation is executed in Or-parallel, i.e.  several
   parallel jobs are created which may be executed in parallel on different
   processors.  There is no specified order of enumerating the domain of
   the variable, the user should not rely on a particular order.  It is
   used mostly to find an instantiation of the variable which is consistent
   with the current set of constraints in labeling procedures of a parallel
   program.  If List is a list of all domain variables occurring in the
   program, the simplest parallel labeling procedure is written as

<P>
<PRE>
        labeling([]).
        labeling([Var|Rest]) :-
            par_indomain(Var),
            labeling(Rest).
</PRE>
"),
	args:["?Var" : "An integer or a domain variable"],
	resat:"   Yes.",
	fail_if:"   None.\n\n",
	see_also:[/(::, 2), /(#::, 2), /(indomain, 1)]]).

:- comment(/(sorted_list_to_dom, 2), [
	summary:"Convert a sorted list of atomic terms and integer intervals into a domain
Dom.

",
	template:"sorted_list_to_dom(+List, -Dom)",
	desc:html("   Convert a sorted list of atomic terms and integer intervals into a
   domain Dom.  List is assumed to be in the correct order and format for a
   finite domain.  If it is not, the program will not work correctly.  The
   purpose of this predicate is to quickly convert small lists to domains.

<P>
"),
	args:["+List" : "A sorted list of atomic terms and integer interval.", "-Dom" : "Variable."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[4 : "List is not ground.", 5 : "List contains an element which is neither atomic nor an    integer interval.", 6 : "The size of the resulting domain is too large.", 6 : "The lower bound of an integer interval is greater than its    upper bound."],
	see_also:[/(integer_list_to_dom, 2), /(list_to_dom, 2), /(dvar_domain, 2)]]).

:- comment(/(var_fd, 2), [
	summary:"Create a domain variable with a given domain.

",
	template:"var_fd(+Var, +Dom)",
	desc:html("   If Var is a free variable, is becomes a domain variable with the domain
   Dom and with empty suspension lists.  The domain Dom is copied to make
   in-place updates logically sound.  If Var is already a domain variable,
   its domain is intersected with the domain Dom.

<P>
"),
	args:["+Var" : "A variable or a domain variable.", "+Dom" : "A finite domain."],
	resat:"   No.",
	fail_if:"   Fails if Var is not a variable.\n\n",
	see_also:[/(::, 2), /(#::, 2), /(dvar_domain, 2)]]).

:- comment(/(contigs, 5), [
	summary:"MaxLength is the longest sequence of Item occurring in List, Occurrences is
the total number of occurrences of Item in List and Contigs is the number
of sequences of Item in List.

",
	template:"contigs(+List, +Item, ?MaxLength, ?Occurrences, ?Contigs)",
	desc:html("   The constraint is satisfied when MaxLength is the longest sequence of
   Item occurring in List, Occurrences is the total number of occurrences
   of Item in List and Contigs is the number of all uninterrupted sequences
   of Item in List.

<P>
"),
	args:["+List" : "A list of finite domain variables or integers", "+Item" : "An integer", "?MaxLength" : "An integer finite domain variable or an integer.", "?Occurrences" : "An integer finite domain variable or an integer.", "?Contigs" : "An integer finite domain variable or an integer."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(sequence, 4)]]).

:- comment(/(deletemin, 3), [
	summary:"Select from List the variable Var which has the smallest lower domain bound,
and return the rest of the list.

",
	template:"deletemin(?Var, +List, -Rest)",
	desc:html("   This predicate is used in labeling procedures.  Especially when labeling
   start times in scheduling problems, it is often a good strategy to start
   with the earliest possible tasks, because fixing those will cause useful
   bounds propagation on the start times of the other tasks. This predicate
   selects from a list the variable with the smallest lower bound. Numbers
   are treated as if they were variables with singleton domains.

<P>
"),
	args:["-Var" : "A variable.", "-Rest" : "A term unifying with a list of domain variables or integers.", "+List" : "A list of domain variables or integers."],
	resat:"   No.",
	fail_if:"   Fails if List is empty.\n\n",
	eg:"
   start_time_labeling([]) :- !.
   start_time_labeling(Vars) :-
        deletemin(X, Vars, Rest),
\tindomain(X),
\tstart_time_labeling(Rest).



",
	see_also:[/(deleteff, 3), /(deleteffc, 3), /(indomain, 1), /(labeling, 1)]]).

:- comment(/(disjunction, 5), [
	summary:"Flag indicates which of the two non-overlapping tasks is scheduled as
first.  Either the one with starting time Start1 and duration Duration1
(then the value of Flag is 1), or the second one with starting time Start2
and duration Duration2 (and the Flag is 2).

",
	template:"disjunction(?Start1, +Duration1, ?Start2, +Duration2, ?Flag)",
	desc:html("   This constraint can be used to model two non-overlapping tasks with
   known durations.  Given the starting times and durations, this
   constraint uses constructive disjunction to remove all inconsistent
   values from the domains of Start1 and Start2.  In addition to
   maintaining their bounds in a consistent state, it also locally executes
   both cases (first task before the second one or voce versa) and removes
   all values which are not consistent with any of these two alternatives.

<P>
   If Duration1 or Duration2 is not integer, it obtains a default domain.

<P>
"),
	args:["?Start1" : "A finite domain variable or integer", "+Duration1" : "An integer", "?Start2" : "A finite domain variable or integer", "+Duration2" : "An integer", "?Flag" : "A finite domain variable or integer"],
	resat:"   No.",
	fail_if:"   Fails if there is no possibility to schedule given two tasks in any\n   order under given conditions.\n\n",
	eg:"
   [eclipse 5]: [X, Y]::1..10, disjunction_choose(X, 5, Y, 7, F).
   X = X[1..10]
   Y = Y[1..10]
   F = F[1, 2]
   Delayed goals:
   disjunction_choose_1(X[1..10], 5, Y[1..10], 7, F[1, 2])
   yes.
   [eclipse 6]: [X, Y]::1..10, disjunction(X, 5, Y, 7, F).
   X = X[1..5, 8..10]
   Y = Y[1..3, 6..10]
   F = F
   Delayed goals:
   disjunction(X[1..5, 8..10], 5, Y[1..3, 6..10], 7, F)
   yes.



",
	see_also:[/(disjunctive, 3), /(disjunction_choose, 5)]]).

:- comment(/(disjunction_choose, 5), [
	summary:"Flag indicates which of the two non-overlapping tasks is scheduled as
first.  Either the one with starting time Start1 and duration Duration1
(then the value of Flag is 1), or the second one with starting time Start2
and duration Duration2 (and the Flag is 2).

",
	template:"disjunction_choose(?Start1, +Duration1, ?Start2, +Duration2, ?Flag)",
	desc:html("   This constraint can be used, if there are two tasks that have to be
   scheduled on the same machine.  It states which of the two tasks given
   by their starting times and durations is scheduled as first.  It is
   activated whenever the maximum or minimum of domain variables Start1 or
   Start2 resp.  changes or the Flag is set to an integer.

<P>
"),
	args:["?Start1" : "A finite domain variable or integer", "+Duration1" : "An integer", "?Start2" : "A finite domain variable or integer", "+Duration2" : "An integer", "?Flag" : "A variable or integer"],
	resat:"   No.",
	fail_if:"   Fails if there is no possibility to schedule given two tasks in any\n   order under given conditions.\n\n",
	exceptions:[4 : "Duration1 or Duration2 is not instantiated.", 5 : "Duration1 or Duration2 or Flag is instantiated but not an    integer."],
	eg:"
   [eclipse 5]: [X, Y]::1..10, disjunction_choose(X, 5, Y, 7, F).
   X = X[1..10]
   Y = Y[1..10]
   F = F[1, 2]
   Delayed goals:
   disjunction_choose_1(X[1..10], 5, Y[1..10], 7, F[1, 2])
   yes.
   [eclipse 6]: [X, Y]::1..10, disjunction_choose(X, 5, Y, 7, 1).
   X = X[1..5]
   Y = Y[6..10]
   Delayed goals:
   Y[6..10] - X[1..5]#>=5
   disjunction_choose_1(X[1..5], 5, Y[6..10], 7, 1)
   yes.
   [eclipse 8]: [X, Y]::1..5, disjunction_choose(X, 5, Y, 7, 2).
   no (more) solution.



",
	see_also:[/(disjunctive, 3), /(disjunction, 5)]]).

:- comment(/(disjunctive, 3), [
	summary:"Succeeds if there exists a sequential ordering of non-overlapping tasks
with starting times Starts and durations Durations, with respect to the
ordering priorities of single pairs of tasks (elementary disjunctions)
represented in the list Flags.

",
	template:"disjunctive(?Starts, +Durations, ?Flags)",
	desc:html("   This constraint can be used by job-shop problems on single machine.  The
   tasks to be scheduled on one machine are represented by their starting
   times (Starts) and durations (Durations).  The actual ordering of tasks
   is represented by list of priority flags (Flags).  The core constraint
   performs an exhaustive scheme based on maximal subset using extended
   Carlier and Pinson techniques.  This constraint reacts to reduction
   events on the flags of the elementary disjunctions it is connected to,
   in turn it might perform reductions on those flags.  It also updates
   starting times of operations according to the necessary partial
   orderings it discovers.

<P>
   If some of the elements in the lists Starts or Flags is not ground, this
   predicate delays.

<P>
"),
	args:["?Starts" : "A list of finite domain variables or integers", "+Durations" : "A list of integers", "?Flags" : "A list of finite domain variables or integers"],
	resat:"   No.",
	fail_if:"   Fails, if no possible ordering of given list of tasks exist.\n\n",
	eg:"
   [eclipse 3]: [X, Y]::1..10, disjunctive([X, Y], [5, 7], F).
   X = X[1..10]
   Y = Y[1..10]
   F = [_g1124[1, 2]]
   Delayed goals:
   disjunction_choose_1(X[1..10], 5, Y[1..10], 7, _g1124[1, 2])
   disjunctive(starts(X[1..10], Y[1..10]), durations(5, 7), [_g1124[1, 2]],
   flags(_g998, _g1124, _g1002, _g1004))
   yes.
   [eclipse 3]: [X, Y, Z]::1..10, disjunctive([X, Y, Z], [3, 7, 5], [1, 2,
   1]).
   no (more) solution.
   [eclipse 4]: [X, Y, Z]::1..10, disjunctive([X, Y, Z], [3, 7, 5], [1, 1,
   2]).
   X = X[1, 2]
   Y = Y[9, 10]
   Z = Z[4, 5]
   Delayed goals:
   Y[9, 10] - X[1, 2]#>=3
   Y[9, 10] - Z[4, 5]#>=5
   Z[4, 5] - X[1, 2]#>=3
   Z[4, 5] - X[1, 2]#>=3
   disjunction_choose_1(X[1, 2], 3, Z[4, 5], 5, 1)
   disjunction_choose_1(X[1, 2], 3, Y[9, 10], 7, 1)
   disjunction_choose_1(Y[9, 10], 7, Z[4, 5], 5, 2)
   disjunctive(starts(X[1, 2], Y[9, 10], Z[4, 5]), durations(3, 7, 5), [1,
   1, 2], flags(_g1080, 1, 1, _g1086, _g1088, 2, _g1092, _g1094, _g1096))
   yes.



",
	see_also:[/(disjunction_choose, 5)]]).

:- comment(/(sequence, 4), [
	summary:"MaxLength is the longest sequence of Item occurring in List and Occurrences
is the total number of occurrences of Item in List.

",
	template:"sequence(+List, +Item, ?MaxLength, ?Occurrences)",
	desc:html("   The constraint is satisfied when MaxLength is the longest sequence of
   Item occurring in List and Occurrences is the total number of
   occurrences of Item in List.

<P>
"),
	args:["+List" : "A list of finite domain variables or integers", "+Item" : "An integer", "?MaxLength" : "An integer finite domain variable or an integer.", "?Occurrences" : "An integer finite domain variable or an integer."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	see_also:[/(contigs, 5)]]).

:- comment(/(deleteff, 3), [
	summary:"Select from List the variable Var which has the smallest domain and return
the rest of the list.

",
	template:"deleteff(?Var, +List, -Rest)",
	desc:html("   This predicate is used in the labeling procedures.  When we look for the
   instantiation of a list of domain variables which is consistent with a
   set of constraints, it is better to start with instantiations which are
   most likely to fail, because this will cut down the size of the search
   space considerably.

<P>
   The predicate deleteff/3 selects the variable with the smallest
   domain. If there are several variables with the same domain size,
   the leftmost one in the list is taken. Constants are treated like
   variables of domain size one. The list Rest is the same as List
   except that the selected element is missing. In particular, any
   existing list order is preserved.

<P>
"),
	args:["-Var" : "A variable.", "-Rest" : "A term unifying with a list of domain variables or integers.", "+List" : "A list of domain variables or integers."],
	resat:"   No.",
	fail_if:"   Fails if List is nil.\n\n",
	see_also:[/(deleteffc, 3), /(indomain, 1), fd_search:search/6]]).

:- comment(/(deleteffc, 3), [
	summary:"Select from List the variable Var which has the smallest domain and most
constraints and return the rest of the list.

",
	template:"deleteffc(?Var, +List, -Rest)",
	desc:html("   This predicate is used in the labeling procedures.  When we look for the
   instantiation of a list of domain variables which is consistent with a
   set of constraints, it is better to start with instantiations which are
   most likely to fail, because this will cut down the size of the search
   space considerably.  The predicate deleteffc/3 selects among the
   variables with the smallest domain that one which has most constraints
   attached to it.  This choice is based on the heuristics that variables
   with more constraints are more likely to fail when instantiated.

<P>
   If several variables are equally eligible, the leftmost one in the
   list is taken.  Constants are treated like variables of domain size
   one with no constraints attached.  The list Rest is the same as
   List except that the selected element is missing.  In particular,
   any existing list order is preserved.

<P>
"),
	args:["-Var" : "A variable.", "-Rest" : "A term unifying with a list of domain variables or integers.", "+List" : "A list of domain variables or integers."],
	resat:"   No.",
	fail_if:"   Fails if List is nil.\n\n",
	see_also:[/(constraints_number, 2), /(deleteff, 3), /(indomain, 1), fd_search:search/6]]).

:- comment(integers/1, [
        summary: "Constrains Vars to be integers.",
	amode: integers(+),
	args: ["Vars": "List of variables or integers."],
	resat: "  No.",
	fail_if: "   Fails if Vars contains non-integers, or variables with non-integer domains.",
	exceptions:[5: "Vars is a variable."],
        desc: html("\
  Constrains the list Vars to be integers. If Vars contains non-domain 
  variable, such variables will be given the default domain.")
]).


:- comment(labeling/1, [
        summary: "Instantiate all variables in a list to values in their domain",
	amode: labeling(+),
	args: ["Vars": "List of variables or ground terms."],
	resat: "Yes",
	desc:html("\
    This predicate instantiates all variables in a list to values in their
    domain, using the indomain/1 predicate. It is simply defined as:
<PRE>
        labeling([]).
        labeling([Var|Rest]) :-
            indomain(Var),
            labeling(Rest).
</PRE>
    The list can contain ground terms, i.e. variables that are already
    instantated. These are just skipped, in other words, they are
    considered as variables with a single value in their domain.
    "),
    see_also:[/(indomain, 1), fd_search:search/6]
]).



:- comment((#)/3, [
    summary:"The cardinality operator",
    args:[
	"Min":"Integer or domain variable",
	"Cstrs":"A list of constraint expressions",
	"Max":"Integer or domain variable"
    ],
    desc:html("
	This is a meta constraint known in the literature as the
	cardinality operator.  CstList is a list of constraint
	expressions and this operator states that at least Min and at
	most Max out of them are valid. 
    "),
    see_also:[/(#/\, 3), /(isd, 2)]]).
