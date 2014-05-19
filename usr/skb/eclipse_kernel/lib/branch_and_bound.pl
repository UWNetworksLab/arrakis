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
% Copyright (C) 1999-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: branch_and_bound.pl,v 1.1 2008/06/30 17:43:42 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(branch_and_bound).

:- comment(summary, "Generic branch-and-bound primitives").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(author, "Joachim Schimpf, Vassilis Liatsos, IC-Parc, Imperial College, London").
:- comment(date, "$Date: 2008/06/30 17:43:42 $").
:- comment(index, ["branch-and-bound","dichotomic search"]).
:- comment(desc, html("
	This is a solver-independent library implementing branch-and-bound
	primitives. It can be used with any nondeterministic search routine
	that instantiates a cost variable when a solution is found. The cost
	variable can be an arbitrary numerical domain variable or even a
	simple domain-less variable.
	<P>
	The main predicates are bb_min/3, bb_min/6 and, as a shorthand,
	minimize/2.
	<P>
	Note on the treatment of bounded reals: The library allows the cost
	to be instantiated to a number of type breal. This is useful e.g.
	when using lib(ic) to solve problems with continuous variables.
	When the variable domains have been narrowed sufficiently, the
	problem variables (in particular the cost variable) should be
	instantiated to a bounded real, e.g. using the following idiom:
	<PRE>
		X is breal_from_bounds(get_min(X),get_max(X))
	</PRE>
	Bounded reals contain some uncertainty about their true value. If
	this uncertainty is too large, the branch-and-bound procedure may
	not be able to compare the quality of two solutions. In this case,
	a warning is issued and the search terminated prematurely.  The
	problem can be solved by increasing the delta-parameter, or by
	locating the cost value more precisely.
	")).

:- export
	bb_min/3, bb_min/4,	% main predicate
	bb_min/6, bb_min/7,
	minimize/2, minimize/3,

	bb_init/2,		% underlying primitives
	bb_cost/2,
	bb_solution/2,
	bb_finish/1,
	bb_probe/7.

:- export struct(bb_options(
	    strategy,		% atom
	    from,		% number
	    to,			% number
	    delta,		% number
	    factor,		% number
	    timeout,		% number
	    probe_timeout,	% number
	    report_success,	% N/A or GoalPrefix
	    report_failure	% N/A or GoalPrefix
	)).

:- lib(timeout).

:- import
	% general-purpose predicates
	call_local/1,
	par_true/0,
	worker_boundary/0
    from sepia_kernel.


%----------------------------------------------------------------------
% Toplevel
%----------------------------------------------------------------------

:- tool(minimize/2, minimize/3).
minimize(Goal, Cost, Module) :-
	bb_min(Goal, Cost, _DefaultOptions, Module).

:- tool(bb_min/3, bb_min/4).
bb_min(Goal, Cost, Options, Module) :-
	% we used to use the Goal directly as the Template, but that can
	% cause problems when Goal contains strange stuff, like handles
	term_variables(Goal, Template),
	bb_min(Goal, Cost, Template, Solution, Opt, Options, Module),
	% instantiate to the optimum solution (atomically)
	Template-Cost = Solution-Opt.

:- tool(bb_min/6, bb_min/7).
bb_min(Goal, Cost, Template, Solution, Opt, Options, Module) :-
	( var(Cost) ; number(Cost) ),
	default_options(Options),
	check_options(Options),
	!,
	initial_bounds(Cost, Options, From, To),
	bb_init(To, Handle),
	Options = bb_options{timeout:Timeout},
        ( Timeout > 0, Timeout < 1.0Inf ->
	    timeout(
		bb_any(From, To, Goal, Template, Cost, Handle, Module, Options),
		Timeout,
		printf(log_output, "Branch-and-bound timeout!%n%b", [])
	    )
	;
	    bb_any(From, To, Goal, Template, Cost, Handle, Module, Options)
	),
	bb_solution(Handle, Solution),	% may fail
	bb_cost(Handle, Opt),
	bb_finish(Handle).
bb_min(Goal, Cost, _Template, _Solution, _Opt, Options, Module) :-
	error(5, bb_min(Goal, Cost, Options), Module).

    bb_any(From, To, Goal, Template, Cost, Handle, Module, Options) :-
	% The following line is for parallelism (scheduling and copying)
	%worker_boundary, par_true,
	Options = bb_options{strategy:Strategy},
	( Strategy == continue ->
	    bb_continue(From, To, Goal, Template, Cost, Handle, Module, Options)
	; Strategy == restart ->
	    bb_delta(From, To, Goal, Template, Cost, Handle, Module, Options)
	; Strategy == step ->
	    bb_delta(From, To, Goal, Template, Cost, Handle, Module, Options)
	; Strategy == dichotomic ->
	    bb_dichotomic(From, To, Goal, Template, Cost, Handle, Module, Options)
	;
	    error(6, bb_min(Goal, Cost, Options), Module)
	).
    bb_any(_From,_To,_Goal,_Template,_Cost,_Handle,_Module,_Options).


default_options(bb_options{
	    from:From,to:To,factor:Factor,strategy:Strategy,
	    delta:Delta,timeout:Timeout,probe_timeout:ProbeTimeout}) :-
	set_default(Strategy, continue),
	set_default(From, -1.0Inf),
	set_default(To, 1.0Inf),
	( Strategy==dichotomic -> set_default(Factor, 0.5)
	; set_default(Factor, 1) ),
	set_default(Delta, 1),
	set_default(Timeout, 0),
	set_default(ProbeTimeout, 0).

    set_default(X, X) :- !.
    set_default(_, _).

check_options(bb_options{from:From,to:To,factor:Factor,
	     delta:Delta,timeout:Timeout,probe_timeout:ProbeTimeout}) :-
	precise_number(From),
	precise_number(To),
	precise_number(Delta),
	precise_number(Factor),
	From =< To,
	0 < Factor, Factor =< 1, 
	0 < Delta,
	0 =< Timeout,
	0 =< ProbeTimeout.

precise_number(X) :- breal(X), !,
	printf(error, "branch_and_bound: parameter must not be a bounded real: %w%n", [X]),
	fail.
precise_number(X) :- number(X).

%----------------------------------------------------------------------
% Stepwise improvement
% bb_delta always fails or aborts!
%----------------------------------------------------------------------

bb_delta(From, To, Goal, Template, Cost, Handle, Module, Options) :-
	( bb_probe(From, To, Goal, Template, Cost, Handle, Module,Options) ->	% may fail
	    Best is bb_cost(Handle),
	    step(From, Best, Options, NewTo),		% may fail
	    bb_delta(From, NewTo, Goal, Template, Cost, Handle, Module, Options)
	;
	    report_failure(Options, From..To, Handle, Module),
	    fail
	).

    % fails if there is no range left to explore
    % PRE: no breals among the inputs, except Best
    step(From, Best, bb_options{factor:Factor,delta:Delta}, NewTo) :-
	To is number_max(Best),	% we are trying to improve the guaranteed best
	Gap is To - From,
	Gap > 0,		% termination condition
	( Gap < 1.0Inf ->
	    NewTo is min(From + Gap*Factor, To-Delta),
	    NewTo < To		% can only be violated if precision problems
	;
	    NewTo is To - Delta
	),
	( NewTo < number_min(Best) ->
	    true
	;
	    % this prevents looping with overlapping costs
	    writeln(warning_output, "WARNING: bb_min: search terminated prematurely - cost uncertainty too large."),
	    writeln(warning_output, "Either increase bb_min delta parameter, or compute cost more precisely."),
	    fail
	).

    % like breal_max, but preserve the type for non-breals
    number_max(X, Max) :- breal(X), !, breal_max(X, Max).
    number_max(X, X).

    % like breal_min, but preserve the type for non-breals
    number_min(X, Min) :- breal(X), !, breal_min(X, Min).
    number_min(X, X).

%----------------------------------------------------------------------
% Stepwise improvement without restart
% bb_continue always fails or aborts!
%
% The search tree is only traversed once. Every time a solution is
% found, the new cost bound gets imposed dynamically for the rest
% of the search. To do that, we use the fail-event mechanism to trigger
% a demon that imposes the current global bound on the cost variable.
% This is necessary in two situations:
% 1. If we fail across a change of the global bound (after a new solution)
% 2. If we fail across the imposition of a new bound on the cost variable
%    (we have to do it again because the effect is lost)
% For the sake of completeness, we also take care of the case where
% the cost variable is not a domain variable and cannot have a bound
% imposed. In that case, the demon must additionally be woken on
% instantiation of the cost variable to effect the check.
%----------------------------------------------------------------------

:- import
	request_fail_event/3,
	timestamp_init/2,
	timestamp_update/2
    from sepia_kernel.

:- set_event_handler('branch_and_bound:apply_bound', trigger/1).


bb_continue(From, To, Goal, Template, Cost, Handle, Module, Options) :-
	(
	    Stamp = stamp(_),
	    timestamp_init(Stamp, 1),
	    timestamp_update(Stamp, 1),
	    suspend(bb_impose_bound(Cost, Handle, Stamp, From),
		2,	% this should be between 1 (debug) and 2 (constraints)
		trigger('branch_and_bound:apply_bound'), Susp),
	    ( cannot_impose_bound(Cost) ->
		insert_suspension(Cost, Susp, inst of suspend, suspend)
	    ;
		true
	    ),
	    call_local((
		    % impose initial bounds, if possible
		    set_var_bounds(Cost, From, To),
		    Goal,
		    kill_suspension(Susp)
		))@Module,
	    ( var(Cost) ->
		writeln(error, "bb_min: search did not instantiate cost variable"),
		abort
	    ;
		true
	    ),
	    % In case the solution still contains variables,
	    % we want to strip most of their attributes.
	    % Otherwise we might copy the whole constraint store!
	    copy_term(Template, StrippedSolution),
	    % Compute the new bound
	    ( step(From, Cost, Options, NewTo) ->
		% Set all shelf fields atomically (timeout!)
		shelf_set(Handle, 0, sol(s(StrippedSolution),Cost,NewTo)),
		request_fail_event(Stamp, 1, 'branch_and_bound:apply_bound')
	    ;
		% Set all shelf fields atomically (timeout!)
		shelf_set(Handle, 0, sol(s(StrippedSolution),Cost,From)),
	    	% We found an optimal solution, no need to continue search
		!
	    ),
	    report_success(Options, Cost, Handle, Module),
	    fail	% continue search, fail into Goal
	;
	    shelf_get(Handle, 3, NewTo),
	    report_failure(Options, From..NewTo, Handle, Module),
	    fail
	).


% This demon will be woken after failures when a global bound needs
% to be re-imposed. It will itself schedule another event to be raised
% in case we fail across the current demon invocation.
% The bound is imposed using set_var_bounds/3. In case the Cost
% variable does not have a domain which can represent that,
% we need to wake also when Cost gets instantiated.

:- demon bb_impose_bound/4.
bb_impose_bound(Cost, _Handle, _Stamp, _From) :-
	% Optimization: if Cost is a free variable, we can't impose bound
	% neither now nor further up the search tree. No point scheduling
	% further fail-events, need to wait for instantiation of Cost.
	free(Cost), !.
bb_impose_bound(Cost, Handle, Stamp, From) :-
	% (number(Cost) ; meta(Cost))
	% when we fail across this point, we will need to re-apply the bound!
	request_fail_event(Stamp, 1, 'branch_and_bound:apply_bound'),
	% this will apply the bound, if Cost is a domain variable or constat
	NewTo is bb_bound(Handle),
%	writeln(bb_impose_bound(Cost::From..NewTo)),
	set_var_bounds(Cost, From, NewTo).


% succeed if the argument cannot have a bound imposed
% This fails for breals, although the bound cannot really be imposed if
% it overlaps, but we take care of that elsewhere.

cannot_impose_bound(X) :- free(X).
cannot_impose_bound(X) :- meta(X),
	get_var_bounds(X, L, H),
	H =:= 1.0Inf,
	L =:= -1.0Inf,
	% try imposing a bound and check if it worked
	call_priority((
		set_var_bounds(X, L, 0),
		get_var_bounds(X, _, H1),
		H1 =:= 1.0Inf
	    ), 1).


%----------------------------------------------------------------------
% Dichotomic search
% bb_dichotomic always fails or aborts!
%----------------------------------------------------------------------

bb_dichotomic(From, To, Goal, Template, Cost, Handle, Module, Options) :-
	% look for an initial solution
	( bb_probe(From, To, Goal, Template, Cost, Handle, Module, Options) ->	% may fail
	    Best is bb_cost(Handle),
	    bb_dichotomic1(From, false, Best, Goal, Template, Cost, Handle, Module, Options)
	;
	    report_failure(Options, From..To, Handle, Module),
	    fail
	).

    % We assume that there is a solution with cost Best.
    % If FromNoSol==true then we know there is no solution with cost From
    bb_dichotomic1(From, FromNoSol, Best, Goal, Template, Cost, Handle, Module, Options) :-
	split(From, FromNoSol, Best, Options, Split), 	% may fail
	( bb_probe(From, Split, Goal, Template, Cost, Handle, Module, Options) ->
	    NewBest is bb_cost(Handle),
	    bb_dichotomic1(From, FromNoSol, NewBest, Goal, Template, Cost, Handle, Module, Options)
	;
	    report_failure(Options, From..Split, Handle, Module),
	    bb_dichotomic1(Split, true, Best, Goal, Template, Cost, Handle, Module, Options)
	).

    % PRE: no breals among the inputs, except Best
    split(From, FromNoSol, Best, bb_options{factor:Factor,delta:Delta}, Split) :-
	To is number_max(Best),	% we are trying to improve the guaranteed best
	Gap is To - From,
	( FromNoSol == true ->
	    Gap > Delta		% termination condition
	;
	    Gap >= Delta	% termination condition
	),
	( Gap < 1.0Inf ->
	    % Normally, Split is From + Gap*Factor, but we
	    % have to consider all sorts of special cases...
	    FromPlusDelta is From + Delta,
	    ToMinusDelta is To - Delta,
	    NormalSplit is From + Gap*Factor,
	    ( FromPlusDelta > ToMinusDelta ->
		% Gap size between Delta and 2*Delta: split in the middle,
		% so both probe-range and improvement are at least Delta/2
		Split is From + Gap*0.5
	    ; NormalSplit < FromPlusDelta ->
		% make sure the probe covers at least a Delta-range,
		% otherwise proving optimality can take a long time
	    	Split = FromPlusDelta
	    ; NormalSplit > ToMinusDelta ->
		% make sure the improvement is at least Delta,
		% otherwise we might iterate through too many solutions
	    	Split = ToMinusDelta
	    ;
		% normal case: split according to factor
	    	Split = NormalSplit
	    ),
	    % Following line is normally redundant, but can be violated in
	    % case of precision/rounding problems. It then prevents looping.
	    Split < To
	; From >= 0 ->
	    Split is From + 10000000
	; To =< 0 ->
	    Split is To - 10000000
	;
	    Split = 0.0
	),
	( Split < number_min(Best) ->
	    true
	;
	    % this prevents looping with overlapping costs
	    writeln(warning_output, "WARNING: bb_min: search terminated prematurely - cost uncertainty too large."),
	    writeln(warning_output, "Either increase bb_min delta parameter, or compute cost more precisely."),
	    fail
	).

%----------------------------------------------------------------------
% Primitives
%----------------------------------------------------------------------

bb_init(ExtremeCost, Handle) :-
	shelf_create(sol(no_solution,ExtremeCost,ExtremeCost), Handle).

bb_cost(Handle, Cost) :-
	shelf_get(Handle, 2, Cost).

bb_solution(Handle, Solution) :-
	shelf_get(Handle, 1, s(Solution)).	% fail here if no solution

bb_bound(Handle, Bound) :-
	shelf_get(Handle, 3, Bound).

bb_finish(Handle) :-
	shelf_abolish(Handle).


% bb_probe tries to find a solution for Goal in the range From..To.
% If there is a solution, its Template and Cost are stored in Handle,
% the computation is undone, and bb_probe succeeds.
% If there is no solution, Handle is not changed and bb_probe fails.

bb_probe(From, To, Goal, Template, Cost, Handle, Module) :-
	bb_probe(From, To, Goal, Template, Cost, Handle, Module,
		bb_options{report_success:true/0}).

bb_probe(From, To, Goal, Template, Cost, Handle, Module, Options) :-
	(
	    call_local((
		    % impose bounds early if possible
		    set_var_bounds(Cost, From, To),
		    Goal,
		    % in case there was no set_bounds handler:
		    eclipse_language:(From =< Cost),
		    eclipse_language:(Cost =< To)
		))@Module
	->
	    ( var(Cost) ->
	    	writeln(error, "bb_min: search did not instantiate cost variable"),
		abort
	    ;
		true
	    ),
	    % In case the solution still contains variables,
	    % we want to strip most of their attributes.
	    % Otherwise we might copy the whole constraint store!
	    copy_term(Template, StrippedSolution),
	    % Set all shelf fields atomically (timeout!)
	    shelf_set(Handle, 0, sol(s(StrippedSolution),Cost,Cost)),
	    report_success(Options, Cost, Handle, Module),
	    fail	% to undo the call-effect and succeed with 2nd clause
	;
	    !,
	    fail
	).
bb_probe(_From, _To, _Goal, _Template, _Cost, _Handle, _Module, _Options).


% Get an initial lower and upper bound for the search by intersecting
% the bounds from propagation with the user option input (if any)

initial_bounds(Cost, Options, Lower, Upper) :-
	Options = bb_options{from:UserFrom, to:UserTo},
	get_var_bounds(Cost, CostL, CostU),
	Lower is max(UserFrom, CostL),
	Upper is min(UserTo, CostU).


report_success(bb_options{report_success:Spec}, Cost, Handle, Module) :-
	report_result(Spec, "Found a solution with cost %q%n", Cost, Handle, Module).

report_failure(bb_options{report_failure:Spec}, Range, Handle, Module) :-
	report_result(Spec, "Found no solution with cost %q%n", Range, Handle, Module).

    report_result(N/A, _Msg, Range, Handle, Module) ?- !,
	functor(Goal, N, A),
	Goal =.. [_|Args],
	append(Args, _Optional, [Range, Handle, Module]),
	call(Goal)@Module.
    report_result(Prefix, _Msg, Range, Handle, Module) :-
	( atom(Prefix) ; compound(Prefix) ),
	!,
	Prefix =.. [N|FixedArgs],
	append(FixedArgs, [Range, Handle, Module], Args),
	Goal =.. [N|Args],
	call(Goal)@Module.
    report_result(_, Msg, Cost, _, _) :-
	printf(log_output, Msg, Cost).


%----------------------------------------------------------------------
% Documentation
%----------------------------------------------------------------------

:- comment(minimize/2, [
    summary:"Find a minimal solution using the branch-and-bound method",
    desc:html("This is a shorthand for
    	<PRE>
	bb_min(+Goal, ?Cost, _DefaultOptions)
	</PRE>
	See bb_min/3 for details."),
    template:"minimize(+Goal, ?Cost)",
    see_also:[bb_min/3]]).

/*
:- comment(minimize/3, [
    summary:"Find a minimal solution using the branch-and-bound method",
    desc:html("See bb_min/3 for details."),
    template:"minimize(+Goal, ?Cost, +Module)",
    see_also:[bb_min/3,minimize/2]]).

:- comment(bb_min/4, [
    summary:"Find a minimal solution using the branch-and-bound method",
    desc:html("See bb_min/3 for details."),
    template:"bb_min(+Goal, ?Cost, ?Options, +Module)",
    see_also:[bb_min/3]]).
*/

:- comment(bb_min/3, [
    summary:"Find a minimal solution using the branch-and-bound method",
    see_also:[bb_min/6],
    desc:html("\
	A solution of the goal <EM>Goal</EM> is found that minimizes
	the value of <EM>Cost</EM>.  <EM>Cost</EM> should be a
	variable that is affected, and eventually instantiated, by
	<EM>Goal</EM>.  Usually, <EM>Goal</EM> is the search procedure
	of a constraint problem and <EM>Cost</EM> is the variable
	representing the cost.  The solution is found using the branch
	and bound method:  as soon as a solution is found, it gets
	remembered and the search is continued or restarted with an
	additional constraint on the <EM>Cost</EM> variable which
	requires the next solution to be better than the previous one. 
	Iterating this process yields an optimal solution in the end.
	<P>
	The possible options are
	<DL>
	<DT><STRONG>strategy:</STRONG></DT><DD>
	    <DL>
	    <DT>continue (default)</DT>
	    	<DD>after finding a solution, continue search with the newly
		found bound imposed on Cost</DD>
	    <DT>restart</DT>
	    	<DD>after finding a solution, restart the whole search with
		the newly found bound imposed on Cost</DD>
	    <DT>step</DT>
	    	<DD>a synonym for 'restart'</DD>
	    <DT>dichotomic</DT>
	    	<DD>after finding a solution, split the remaining cost range
		and restart search to find a solution in the lower sub-range.
		If that fails, assume the upper sub-range as the remaining
		cost range and split again.</DD>
	    </DL>
	    The new bound or the split point, respectively, are computed
	    from the current best solution, while taking into account the
	    parameters delta and factor.
	    </DD>
	<DT><STRONG>from:</STRONG></DT>
	    <DD>number - an initial lower bound for the cost (default -1.0Inf)</DD>

	<DT><STRONG>to:</STRONG></DT>
	    <DD>number - an initial upper bound for the cost (default +1.0Inf)</DD>

	<DT><STRONG>delta:</STRONG></DT>	
	    <DD>number - minimal absolute improvement required for each step
	    (default 1.0), applies to all strategies</DD>

	<DT><STRONG>factor:</STRONG></DT>
	    <DD>number - minimal improvement ratio (with respect to the lower
	    cost bound) for strategies 'continue' and 'restart' (default 1.0),
	    or split factor for strategy 'dichotomic' (default 0.5)</DD>

	<DT><STRONG>timeout:</STRONG></DT>
	    <DD>number - maximum seconds of cpu time to spend (default: no limit)</DD>

	<DT><STRONG>report_success:</STRONG></DT>
	    <DD>GoalPrefix - an atom (predicate name) or structure (goal prefix),
	    specifying a goal to be invoked whenever the branch-and-bound
	    process finds a better solution.  The invoked goal is constructed
	    by adding three arguments (Cost, Handle, Module) to GoalPrefix.
	    Cost is a float number representing the cost of the solution found,
	    Handle is a handle as accepted by bb_cost/2 or bb_solution/2,
	    and Module is the context module of the minimisation.
	    The default handler prints a message.</DD>

	<DT><STRONG>report_failure:</STRONG></STRONG></DT>
	    <DD>GoalPrefix - an atom (predicate name) or structure (goal prefix),
	    specifying a goal to be invoked whenever the branch-and-bound
	    process cannot find a solution in a cost range.  The invoked goal
	    is constructed by adding three arguments (Cost, Handle, Module) to
	    GoalPrefix.  Cost is a From..To structure representing the range
	    of cost in which no solution could be found, Handle is a handle
	    as accepted by bb_cost/2 or bb_solution/2, and Module is the
	    context module of the minimisation.
	    The default handler prints a message.</DD>
	</DL>
	The default options can be selected by passing a free variable as
	the Options-argument. To specify other options, pass a bb_options-
	structure in struct-syntax, e.g.
	<PRE>
	bb_options{strategy:dichotomic, timeout:60}
	</PRE>
	In order to maximize instead of minimizing, introduce a negated
	cost variable in your model and minimize that instead.
	<P>
	Compatibility note: For backward compatibility, the report_success and
	report_failure options also accept Name/Arity specifications with
	maximum arity 3 for the handler goals. The three optional arguments
	are then Cost, Handle, and Module.
	"),
    args:["Goal":"The (nondeterministic) search goal",
	"Cost":"A (usually numeric domain) variable representing the cost",
	"Options":"A bb_options structure or variable"],
    amode:bb_min(+,?,?),
    eg:"
?- bb_min(member(X,[9,6,8,4,7,2,4,7]), X, O).
Found a solution with cost 9
Found a solution with cost 6
Found a solution with cost 4
Found a solution with cost 2
Found no solution with cost -1.0Inf .. 1
X = 2
O = bb_options(continue, -1.0Inf, 1.0Inf, 1, 1, 0, 0, _, _)
yes.

[eclipse 6]: bb_min(member(X,[9,6,8,4,7,2,4,7]), X, bb_options{delta:4}).
Found a solution with cost 9
Found a solution with cost 4
Found no solution with cost -1.0Inf .. 0
X = 4
yes.

[eclipse 10]: bb_min(member(X,[99,60,80,40,70,30,70]), X,
	bb_options{strategy:dichotomic, from:0}).
Found a solution with cost 99
Found a solution with cost 40
Found no solution with cost 0.0 .. 20.0
Found a solution with cost 30
Found no solution with cost 20.0 .. 25.0
Found no solution with cost 25.0 .. 27.5
Found no solution with cost 27.5 .. 28.75
Found no solution with cost 28.75 .. 29.0

X = 30
yes.
"]).

/*
:- comment(bb_min/7, [
    summary:"Find a minimal solution using the branch-and-bound method",
    desc:html("See bb_min/6 for details."),
    template:"bb_min(+Goal, ?Cost, +Template, ?Solution, ?Optimum, ?Options, +Module)",
    see_also:[bb_min/6, bb_min/3]]).
*/

:- comment(bb_min/6, [
    summary:"Find a minimal solution using the branch-and-bound method",
    see_also:[bb_min/3],
    desc:html("\
	A solution of the goal <EM>Goal</EM> is found that minimizes
	the value of <EM>Cost</EM>.  <EM>Cost</EM> should be a
	variable that is affected, and eventually instantiated, by
	<EM>Goal</EM>.  Usually, <EM>Goal</EM> is the search procedure
	of a constraint problem and <EM>Cost</EM> is the variable
	representing the cost.  The solution is found using the branch
	and bound method:  as soon as a solution is found, it gets
	remembered and the search is continued or restarted with an
	additional constraint on the <EM>Cost</EM> variable which
	requires the next solution to be better than the previous one. 
	Iterating this process yields an optimal solution in the end.
	<P>
	The possible options are
	<DL>
	<DT><STRONG>strategy:</STRONG></DT><DD>
	    <DL>
	    <DT>continue (default)</DT>
	    	<DD>after finding a solution, continue search with the newly
		found bound imposed on Cost</DD>
	    <DT>restart</DT>
	    	<DD>after finding a solution, restart the whole search with
		the newly found bound imposed on Cost</DD>
	    <DT>step</DT>
	    	<DD>a synonym for 'restart'</DD>
	    <DT>dichotomic</DT>
	    	<DD>after finding a solution, split the remaining cost range
		and restart search to find a solution in the lower sub-range.
		If that fails, assume the upper sub-range as the remaining
		cost range and split again.</DD>
	    </DL>
	    The new bound or the split point, respectively, are computed
	    from the current best solution, while taking into account the
	    parameters delta and factor.
	    </DD>
	<DT><STRONG>from:</STRONG></DT>
	    <DD>number - an initial lower bound for the cost (default -1.0Inf)</DD>

	<DT><STRONG>to:</STRONG></DT>
	    <DD>number - an initial upper bound for the cost (default +1.0Inf)</DD>

	<DT><STRONG>delta:</STRONG></DT>	
	    <DD>number - minimal absolute improvement required for each step
	    (default 1.0), applies to all strategies</DD>

	<DT><STRONG>factor:</STRONG></DT>
	    <DD>number - minimal improvement ratio (with respect to the lower
	    cost bound) for strategies 'continue' and 'restart' (default 1.0),
	    or split factor for strategy 'dichotomic' (default 0.5)</DD>

	<DT><STRONG>timeout:</STRONG></DT>
	    <DD>number - maximum seconds of cpu time to spend (default: no limit)</DD>

	<DT><STRONG>report_success:</STRONG></DT>
	    <DD>GoalPrefix - an atom (predicate name) or structure (goal prefix),
	    specifying a goal to be invoked whenever the branch-and-bound
	    process finds a better solution.  The invoked goal is constructed
	    by adding three arguments (Cost, Handle, Module) to GoalPrefix.
	    Cost is a float number representing the cost of the solution found,
	    Handle is a handle as accepted by bb_cost/2 or bb_solution/2,
	    and Module is the context module of the minimisation.
	    The default handler prints a message.</DD>

	<DT><STRONG>report_failure:</STRONG></STRONG></DT>
	    <DD>GoalPrefix - an atom (predicate name) or structure (goal prefix),
	    specifying a goal to be invoked whenever the branch-and-bound
	    process cannot find a solution in a cost range.  The invoked goal
	    is constructed by adding three arguments (Cost, Handle, Module) to
	    GoalPrefix.  Cost is a From..To structure representing the range
	    of cost in which no solution could be found, Handle is a handle
	    as accepted by bb_cost/2 or bb_solution/2, and Module is the
	    context module of the minimisation.
	    The default handler prints a message.</DD>
	</DL>
	The default options can be selected by passing a free variable as
	the Options-argument. To specify other options, pass a bb_options-
	structure in struct-syntax, e.g.
	<PRE>
	bb_options{strategy:dichotomic, timeout:60}
	</PRE>
	In order to maximize instead of minimizing, introduce a negated
	cost variable in your model and minimize that instead.
	<P>
	Unlike bb_min/3, bb_min/6 does <STRONG>not</STRONG> affect Goal or Cost after
	the optimum has been found. Instead, the optimum cost value is returned
	in Optimum, and the Solution argument gets unified with an instance of
	Template where the variables have the values that correspond to the
	optimal solution. Note that bb_min/3 is actually based on bb_min/6
	and can be defined as:
	<PRE>
	bb_min(Goal, Cost, Options) :-
	    bb_min(Goal, Cost, Goal, Goal, Cost, Options).
	</PRE>
	<P>
	Compatibility note: For backward compatibility, the report_success and
	report_failure options also accept Name/Arity specifications with
	maximum arity 3 for the handler goals. The three optional arguments
	are then Cost, Handle, and Module.
	"),
    args:["Goal":"The (nondeterministic) search goal",
	"Cost":"A (usually numeric domain) variable representing the cost",
	"Template":"A term containing all or some problem variables",
	"Solution":"A term which will be unified with the optimized Template",
	"Optimum":"A variable which will be set to the optimum value of Cost",
	"Options":"A bb_options structure or variable"],
    amode:bb_min(+,?,?,?,?,?)
    ]).

:- comment(bb_init/2, [
    summary:"Low-level primitive for building branch-and-bound-style search procedures",
    template:"bb_init(++ExtremeCost, -Handle)",
    see_also:[bb_probe/7]
    ]).

:- comment(bb_cost/2, [
    summary:"Low-level primitive for building branch-and-bound-style search procedures",
    template:"bb_cost(++Handle, -Cost)",
    see_also:[bb_probe/7]
    ]).

:- comment(bb_solution/2, [
    summary:"Low-level primitive for building branch-and-bound-style search procedures",
    template:"bb_solution(++Handle, -Solution)",
    see_also:[bb_probe/7]
    ]).

:- comment(bb_finish/1, [
    summary:"Low-level primitive for building branch-and-bound-style search procedures",
    template:"bb_finish(++Handle)",
    see_also:[bb_probe/7]
    ]).

:- comment(bb_probe/7, [
    summary:"Low-level primitive for building branch-and-bound-style search procedures",
    template:"bb_probe(++From, ++To, +Goal, ?Template, ?Cost, ++Handle, ++Module)",
    desc:html("
	bb_probe tries to find a solution for Goal in the range From..To.
	If there is a solution, its Template and Cost are stored in Handle,
	the computation is undone, and bb_probe succeeds.
	If there is no solution, Handle is not changed and bb_probe fails.
	The primitive set_var_bounds/3 is used to impose cost bounds
	during the search process in a generic way."),
    see_also:[bb_init/2,bb_cost/2,bb_solution/2,bb_finish/1,bb_min/3,bb_min/6,
    	set_var_bounds/3],
    eg:"% a simple branch-and-bound procedure
my_minimize(Goal, Cost, Solution, OptCost, Module) :-
	bb_init(1000000, Handle),
	(
	    bb_delta(0, 1000000, Goal, Cost, Handle, Module)
	;
	    bb_solution(Handle, Solution),
	    bb_cost(Handle, OptCost)
	),
	bb_finish(Handle).

bb_delta(From, To, Goal, Cost, Handle, Module) :-
	bb_probe(From, To, Goal, Goal, Cost, Handle, Module),
	NewTo is bb_cost(Handle) - 1,
	bb_delta(From, NewTo, Goal, Cost, Handle, Module).
    "]).

