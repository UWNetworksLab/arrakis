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
% Copyright (C) 2001 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Mark Wallace, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% 
% Complete search method for resource-constrained scheduling problems
%
% System:       ECLiPSe Constraint Logic Programming System
% Author/s:     Mark Wallace, IC-Parc
% Version:      $Id: ic_probing_for_scheduling.pl,v 1.2 2008/06/20 13:41:14 jschimpf Exp $
%
%       Many thanks to Hani El Sakkout, on whose ideas this search
%       implementation is based, and on whose benchmarks it was tested.
%       
%
% Todo:
%       - add facilities to control propagation during probing
%
%         (Hani's benchmarks run faster when ic is switched off during search!
%           - I'm not sure if this is still true with IC)
% ----------------------------------------------------------------------

:- module(ic_probing_for_scheduling).

:- export 

        probe_sched/5,      
        probe_cstr_sched/7,
	fun_to_cons_var/3.

:- lib(ic).
:- lib(repair).
:- lib(eplex).
:- lib(ic_edge_finder).
:- lib(linearize).
:- lib(branch_and_bound).

:- use_module(ic_probe).  % For the probe count 
:- use_module(ic_make_overlap_bivs).
:- use_module(ic_probe_support).  % For the task structure: task(id,start,duration)
:- use_module(ic_probe_search).

/* 
probe_sched(
	+ Starts,         
	+ Durations,
	+ Resources,
	+MaxResource,
	? CostFunction
        )

Starts:       List of integers or ic variables (Task start times)
Durations:    List of integers or ic variables (Task durations)
Resources:    List of integers or ic variables (Task resource consumption)
MaxResource:  Integer - maximum available resource
CostFunction: Expression in terms of task start times and durations, 
              to be minimised.

This procedure initialises a cost variable.  
It builds a constraint on the cost, which will be passed to the "probe".  
Finally it initialises some options.
*/
probe_sched(Starts,Durations,Resources,Resource,CostFun) :-
    check_cost_fun(CostFun,Starts-Durations-Resources),
    fun_to_cons_var(CostFun,Constraints,Cost),
    ( 
      UserOptions = [granularity(1),priority(5)] -> true
      ;
      UserOptions = options with [granularity:1,priority:5]
    ),
    probe_cstr_sched(Starts,Durations,Resources,Resource,
                     Constraints,Cost,UserOptions).


/*
probe_cstr_sched(
	+ Starts,
	+ Durations,
	+ Resources,
	++MaxResource,
	+ Constraints,
	- Cost,
	++Options)

Starts, Durations, Resources, MaxResource: as above.
Constraints: A list of constraints, each having the form
             'Term1 =:=Term2', 'Term1 >=Term2', 'Term1>Term2', 
             'Term1 =<Term2' or 'Term1 <Term2'.             
Cost - a finite domain variable
Options - an options structure: 
          Options= [granularity(G),priority(P)]
          (or for compatibility Options= options(G,P))
          'G' is the temporal granularity
          'P' is the priority of the probe.
*/

 
probe_cstr_sched(Starts,
	         Durations,
		 Resources,
		 Resource,
		 Constraints,
		 Cost,
		 UserOptions)  :-
    create_options(UserOptions,Options),
    ic_edge_finder:cumulative(Starts,Durations,Resources,Resource),
    task_structure(Tasks,Starts,Durations,Resources),
    run_probe(Tasks,Constraints,Resource,Cost,Options).

/* For compatibility with initial release */
create_options(options(G,P), options(G,P)) :- !.
create_options(List,options with [granularity:G,priority:P]) :-
	( remove1(granularity(G),List,List2) -> check_granularity(G) 
          ; G=1, List2=List 
        ),
	( remove1(priority(P),List2,Rest) -> check_priority(P) 
          ; P=5, Rest=List2 ),
        ( Rest=[] -> true
          ; write(error,'Error: unrecognised options: '),
	    writeln(error,Rest),
	    abort
        ).

check_granularity(G) :- 
        integer(G), G>0 -> true ;
        writeln(error,'Error: granularity must be a positive integer'), 
        abort.
check_priority(P) :-
        integer(P), P>2 -> true ;
        writeln(error,'Error: priority must be an integer greater than 2'), 
        abort.


/* run_probe(Tasks,Constraints,Resource,Cost,Options) */
run_probe(Tasks,Constraints,Resource,Cost,Options) :-
    set_probect(0),
    set_up_probe(Tasks,Constraints,Cost,Options,Handle),
    tent_init(Tasks),
    make_overlap_bivs(Tasks,Bivs,BivSums,Resource,Options),
    SearchGoal = 
	 ( task_durs(Tasks,Options,Handle),
	   probe_search(Bivs,BivSums,Resource,Options,Handle),
           set_to_tent(Tasks)
         ),
    Options = options with granularity:G,
    bb_min((SearchGoal,set_to_min(Cost)),Cost,
                          bb_options with [delta:G]
          ),
    eplex:lp_cleanup(Handle),
    get_probect(Count),
    writeln(log_output,probes(Count)).

tent_init(Tasks) :-
	term_variables(Tasks,Vars),
        ( foreach(Var,Vars)
        do
	  ( has_tentative_value(Var) -> true
	  ;    get_min(Var,Min),
	       Var tent_set Min
          )
        ).

has_tentative_value(Var) :-
	Var tent_get Val,
	nonvar(Val).


% Implements a heuristic that tries not allowing any durations to be
% increased while establishing resource feasibility.

% If durations are allowed to increase, for the rare case when 
% constraints between tasks necessitate this.  The check ensures there
% is at least one duration that COULD increase!

task_durs(Tasks,Options,Handle) :- 
	member(task with duration:D,Tasks),
	get_max(D) > tent_get(D),
	!,
	constrain_task_durs(Tasks,Options,Handle).
task_durs(_,_,_).


constrain_task_durs(Tasks,Options,Handle) :-
    (    foreach(task with duration:D,Tasks),
         foreach(D=<Old_D,Constraints) 
    do
         D tent_get Old_D
    ),
    add_con(Constraints,[ic,linear(Handle)],Options).
constrain_task_durs(_,_,_).



/*
% Introduced because 'cumulative' cannot deal with variable durations
my_cumulative(Starts,Durations,Resources,Resource) :-
    (
%     foreach(_S,Starts),
     foreach(D,Durations),
     foreach(MinD,MinDurations)
%    , foreach(_R,Resources)
     do
        get_min(D,MinD)
    ),
    cumulative(Starts,MinDurations,Resources,Resource).
*/

check_cost_fun(CostFun,Tasks) :-
	term_variables(CostFun,CostVars),
	term_variables(Tasks,TaskVars),
        member(Var,CostVars),
	not member(Var,TaskVars), !,
	write(warning_output, 'Warning: The cost function '), 
        write(warning_output, CostFun), 
        writeln(warning_output, ' has a variable that is not linked to the tasks!').
check_cost_fun(_,_).

fun_to_cons_var(CostFun,[Cost=:=CostExpr|ConsList],Cost) :-
	pos_expr_to_cons_var(CostFun,ConsList,CostExpr).

pos_expr_to_cons_var(PosEpr1+PosExpr2,ConsList,LinExpr1+LinExpr2) :- !,
	pos_expr_to_cons_var(PosEpr1,ConsList1,LinExpr1),
	pos_expr_to_cons_var(PosExpr2,ConsList2,LinExpr2),
	append(ConsList1,ConsList2,ConsList).
pos_expr_to_cons_var(Integer*PosExpr,ConsList,Integer*LinExpr) :- !,
	integer(Integer),
	pos_expr_to_cons_var(PosExpr,ConsList,LinExpr).
pos_expr_to_cons_var(abs(Term),[Var >= LinExpr,Var >= -LinExpr],Var) :- !,
	linearize_expr(Term,LinExpr).
pos_expr_to_cons_var(max(ExprList),ConsList,Var) :- !,
	max_cons(ExprList,Var,ConsList).
pos_expr_to_cons_var(Other,[],LinExpr) :-
	linearize_expr(Other,LinExpr).

linearize_expr(Term,LinExpr) :-
	linearize(Term,ListExpr,NonLin),
	check_lin(NonLin),
	list_to_sum(ListExpr,LinExpr).

check_lin([]) :- !.
check_lin([NonLin|_NonLins]) :-
	write(error,'Error: the cost function has a non-linear part: '),
	writeln(error,NonLin),
	abort.


% Assume the list produced by linearize is always non-empty
list_to_sum([Last],Last) :- !.
list_to_sum([H|T],H+LinExpr) :-
	list_to_sum(T,LinExpr).

max_cons(List,Var,ConsList) :-
	( foreach(Term,List),
	  foreach(Con,ConsList),
	  param(Var)
	do
	  linearize_expr(Term,LinExpr),
	  Con= (Var>=LinExpr)
        ).

%----------------------------------------------------------------------
% User documentation
%----------------------------------------------------------------------

:- comment(summary, "Probing for Scheduling").
:- comment(author, "Mark Wallace, Hani El Sakkout").
:- comment(date, "$Date: 2008/06/20 13:41:14 $").
:- comment(copyright, "Cisco Systems, Inc.").

:- comment(desc, html("
    This is a complete search method for resource-constrained scheduling 
    problems
    </P><P>
    The user interface is similar to the cumulative constraint, with a 
    list of task start times, durations and resources; and a maximum 
    resource limit.  There is one extra argument, a cost function which 
    is to be minimised. 
    </P><P>
    Probing for scheduling differs radically from the cumulative constraint
    because it includes a search routine.  In its behaviour it is an
    optimisation procedure and not simply a constraint which enforces 
    consistency. 
    </P><P>
    The search is focussed towards an optimal vlaue of the cost function.
    </P>
    ")).


:- comment(eg, "
% Example program: simple schedule optimisation

:- lib(ic_probing_for_scheduling).
:- lib(ic).

ex1([X,Y,Z],Cost) :-
        [OldX,OldY,OldZ]=[1,5,5],
        Durations=[10,5,5],
        Resources=[1,2,1],
        MaxResource=2,
        NewStarts=[X,Y,Z],
        ic:(NewStarts::1..10),
        CostFun= abs(X-OldX) + abs(Y-OldY) + abs(Z-OldZ),
	probe_sched(NewStarts,Durations,Resources,MaxResource,CostFun),
	Cost is CostFun.

ex2([X,Y,Z],Cost) :-
        [OldX,OldY,OldZ]=[1,5,5],
        Durations=[10,5,5],
        Resources=[1,2,1],
        MaxResource=2,
        NewStarts=[X,Y,Z],
        ic:(NewStarts::1..10),
        Constraints=[
                     XDiff >= X-OldX, XDiff >= OldX-X,
                     YDiff >= Y-OldY, YDiff >= OldY-Y,
		     ZDiff >= Z-OldZ, ZDiff >= OldZ-Z,
		     Cost =:= XDiff+YDiff+ZDiff
                    ],
        Options=[granularity(1),priority(5)],
	probe_cstr_sched(NewStarts,Durations,Resources,MaxResource,
                         Constraints,Cost,Options).

ex3(Starts,MaxResource,Cost) :-
	Starts=[S1,S2,S3,S4],
	ic:(S1::1..10),
	ic:(S2::3..10),
	ic:(S3::1..5),
	ic:(S4::3..10),
        [D1,D2,D3,D4]=[5,5,3,3],
        Durations=[D1,D2,D3,D4],
	Resources=[1,1,1,1],
	CostFun= max([S1+D1,S2+D2,S3+D3,S4+D4]),
	probe_sched(Starts,Durations,Resources,MaxResource,CostFun),
	ic:(Cost =:= eval(CostFun)).
     
").

:- comment(fun_to_cons_var/3, [
    summary: "Convert a cost expression to a variable and a list of constraints, 
suitable to pass into probe_cstr_sched/7",
    amode: fun_to_cons_var(+,-,-),

    args:[ "CostFun": "a cost function of the form accepted by probe_sched/5",
           "ConsList": " a variable which will be instantiated to a list of constraints, 
suitable to pass into probe_cstr_sched/7",
           "CostVar": "a variable constrained to take a value greater than or equal to 
the cost function"],
    resat:no,
    see_also:[probe_sched/5,probe_cstr_sched/7],
    desc:html("<P>
If the user needs to use probe_cstr_sched instead of probe_sched, this predicate can be used to convert the cost function to a list of constraints and a cost variable suitable for passing to probe_cstr_sched
"),
    eg:"
fun_to_cons_var(abs(X-10)+abs(Y-3),ConsList,Var)
"]).

:- comment(probe_sched/5, [ 
    summary: "Find a resource-feasible schedule that minimises the
cost function",
    amode:probe_sched(+,+,+,+,?),
    args:["Starts": " a list of (n) task start times (integers or
finite domain variables)",
          "Durations":"a list of (n) task durations (integers or
finite domain variables)",
          "Resources":"a list of (n) task resource needs (integers or 
finite domain variables)",
          "MaxResource":"The available resource, not to be exceeded (integer)",
          "CostFun":"An expression involving start times and durations
to be minimised"
        ],
        resat:no,
        see_also:[probe_cstr_sched/7,min_max/2,set_up_probe/5,make_overlap_bivs/5,probe_search/5,maxlist/2],
	desc:html("<P>
	This predicate finds start times for a set of tasks, which
minimise the value of a given cost function.
</P><P>
The cost function is the only information the search algorithm can use to
focus on the optimum.  It cannot guide the search if the cost is a variable, 
only linked to the tasks start times by constraints.  For this reason the 
cost function admits the special functions <B>abs</B> and <B>maxlist</B>.
</P><P>
The syntax for cost functions is:
<PRE>
CostFunction ::- PosExpr | PosExpr + PosExpr | Integer * PosExpr
PosExpr ::- abs(LinearExpr) | maxlist([LinearExpr]) | LinearExpr.
</PRE>
</P><P>
The algorithm is described in more detail in the documentation of 
<B>probe_cstr_sched/7</B>. 
</P>
"),
       eg:"
probe_schedule(Starts,CostFun) :-
	Starts=[X,Y,Z],
        ic:(Starts::1..10),
        Durations=[10,5,5],
        Resources=[R1,R2,R3],
        ic:(R1::1..2), R2=2, R3=1,
        MaxResource=2,
        [OldX,OldY,OldZ]=[1,5,5],
        CostFun= abs(X-OldX)+abs(Y-OldY)+abs(Z-OldZ),
	probe_sched(Starts,Durations,Resources,MaxResource,CostFun).
"
]).


:- comment(probe_cstr_sched/7, [ 
    summary: "Find a resource-feasible schedule that minimises the
cost, subject to the constraints",
    amode:probe_cstr_sched(+,+,+,++,+,-,++),
    args:["Starts": " A list of (n) task start times (integers or
finite domain variables)",
          "Durations":"A list of (n) task durations (integers or
finite domain variables)",
          "Resources":"A list of (n) task resource needs (integers)",
          "MaxResource":"The available resource, not to be exceeded",
          "Constraints": "A list of numeric equations and inequations, 
using functors '=:=', '>=', '>', '=<' and '<'.",  
          "Cost":"A numeric variable which will be minimised during search",
          "Options":" A list, '[granularity(G),priority(P)]',
    where
    'G' is an integer specifying the time granularity, and
    'P' is the priority of the probe demon."
        ],
        resat:no,
        see_also:[probe_sched/5,min_max/2,set_up_probe/5,make_overlap_bivs/5,probe_search/5],
	desc:html("<P> 
This offers the same functionality as <B>probe_sched/5</B>, but with added 
flexibility, and a more complex user interface.  The extra arguments offer 
the user  more control.  
</P><P>
The <B>Cost</B> argument is a variable, and it must be linked to the task 
variables by the list of linear constraints.  The user can add not only 
linear constraints on the cost function, but also constraints between the 
task variables.  Only constraints made explicit in this list are 'seen' by 
the probe.   <B>probe_cstr_sched</B> also posts them to the ic solver.
</P><P>
The options offer user control over
the temporal granularity, and the priority of the probe.
</P><P>
The algorithm uses <B>min_max</B>, but directs the search using a probe
which focusses the search on the optimum.  The probe is a procedure
that finds optimal solutions to a relaxed problem ignoring resource
limits.  For details see <B>setup_probe</B>.  Additionally the algorithm
sets up a binary variable between each pair of tasks, see
<B>make_overlap_bivs</B>.  Whenever the probe returns tentative start
times, these are propagated to the overlap binary variables yielding a
total resource usage which reveals any bottlenecks, where needed
resources exceed thos available.  <B>probe_search</B> then
non-deterministically introduces a new constraint which reduces the
bottleneck.
The probe_sched call:
<PRE>
probe_sched(Ss,Ds,Rs,MaxR,abs(X-X1)+Y)
</PRE>
translates into the probe_cstr_sched call:
<PRE>
probe_cstr_sched(Ss,Ds,Rs,MaxR,[Cost=:=E1+Y,E1>=X-X1,E1>=X1-X],Cost,
[granularity(1),priority(5)])
</PRE>
Thus making the default granularity '1' and the default priority '5'.
")
]).





