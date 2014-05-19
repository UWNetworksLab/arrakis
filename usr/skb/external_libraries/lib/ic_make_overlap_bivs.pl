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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Mark Wallace and Hani El Sakkout, IC-Parc
% 
% END LICENSE BLOCK
:- module(ic_make_overlap_bivs).


:- lib(ic).
:- lib(repair).
:- use_module(bin_info).
:- use_module(ic_probe_support).
:- lib(ic_global).

:- export make_overlap_bivs/5.


/*
make_overlap_bivs(+Tasks,-BivLists,-BivSums,++Resource,++Options)
In principle a bivalued variable is set up between each pair of tasks.
This variable will be set to the amount of resource used by the second 
task,  if the second task is in progress at the start of the first task.  
If not, the variable will be set to zero.  The sum of these variables 
records the amount of resource in use at the time the first task starts.
*/


make_overlap_bivs(Tasks,BivLists,BivSums,Resource,Options) :-
    (foreach(Task1,Tasks), 
     foreach(BivList,BivLists), 
     foreach(BivSum,BivSums),
        param(Tasks,Options,Resource)
        do
        (foreach(Task2,Tasks),
         fromto([],ThisBivs,NextBivs,BivList),
             param(Task1,Options)
             do
             set_up_biv(Task1,Task2,ThisBivs,NextBivs,Options)
        ),
        Options= options with priority:Priority,
        Prior is Priority-1,
	sumlist(BivList,BivSum),
        check_resource_limit(BivSum,Resource,Prior)
    ),
    init_repair_bivs(BivLists,BivSums,Options).

/* Check the minimum resource required at a certain task start time is
still within the resource limit.  Redundant if the 'cumulative'
constraint is also enforced.
*/
check_resource_limit(BivSum,Resource,Priority) :-
	Prior is Priority+1,
	demon_suspend(check_resource(BivSum,Resource),
                      Priority,
                      BivSum->ic:min,  
                      S),
        demon_suspend(kill_check(BivSum,Resource,S,Susp),
                      Prior,
                      BivSum->ic:max,
                      Susp).

:- demon check_resource/2.
check_resource(BivSum,Resource) :-
       get_min(BivSum)=<Resource.

:- demon kill_check/4.
kill_check(BivSum,Resource,S,Susp) :-
	(get_max(BivSum)=<Resource -> 
	    kill_suspension(S),
	    kill_suspension(Susp)
         ; true
        ).
/*
set_up_biv(+Task1,+Task2,+InBivs,+OutBivs,++Options)
Task1, Task2 - tasks
InBivs, OutBivs - a list of bivalued variables
Options - option structure

set_up_biv sets up a Bivalued Variable between the two tasks, if necessary. 
If created, the variable is added to the appropriate list.
If the tasks do not overlap, then no bivalued variable
is set up, (since the value would be zero).  If the tasks do
overlap, the bivalued variable is set to the amount of resource needed by the 
second task.
*/  

set_up_biv(Task1,Task2,InBivs,OutBivs,_O) :-
        Task2==Task1, !,
        Task1 = task with resource:R,
        OutBivs=[R|InBivs].
set_up_biv(Task1,Task2,InBivs,OutBivs,_O) :-
        Task1 = task with start:S1,
        Task2 = task with [start:S2,duration:D2],
	no_possible_overlap(S1,S2,D2), !, 
	OutBivs=InBivs.
set_up_biv(Task1,Task2,InBivs,OutBivs,_O) :-
        Task1 = task with start:S1,
	Task2 = task with [start:S2,duration:D2,resource:R2],
	must_overlap(S1,S2,D2), !,
	OutBivs=[R2|InBivs].
% If R2 is a variable, then Biv isn't really bivalued!
set_up_biv(Task1,Task2,InBivs,OutBivs,Options) :-
        Options = options with priority:Priority,
        Task1 = task with start:S1,
	Task2 = task with [start:S2,duration:D2,resource:R2],
        get_domain_as_list(R2,R2DomList),
	ic:(Biv::[0|R2DomList]),
	OutBivs=[Biv|InBivs],
	add_bin_info(Biv,(Task1,Task2)),
        Prior is Priority+1,
	demon_suspend(set_biv(S1,S2,D2,R2,Biv,Susp1),
                      Prior,
                      [[S1,S2,D2]->min,[S1,S2,D2]->max],
                      Susp1
                     ).

/*
The following predicates use ic to check for overlaps. 
*/
no_possible_overlap(S1,S2,D2) :-
	(get_min(S1)>=get_max(S2)+get_max(D2)), !.
no_possible_overlap(S1,S2,_) :-
	(get_min(S2)>get_max(S1)), !.
must_overlap(S1,S2,D2) :-
	(get_min(S1)>=get_max(S2)),
	(get_max(S1)<get_min(S2)+get_min(D2)).


/*
set_biv(S1,S2,D2,R2,Biv,Susp)
This sets the bivalued variable as soon as the bounds on the task start times
and durations mean they can't or must overlap.
*/
:- demon set_biv/6.
set_biv(S1,S2,D2,R2,Biv,Susp) :-
	  must_overlap(S1,S2,D2), !,
	  Biv=R2,
	  kill_suspension(Susp).
set_biv(S1,S2,D2,_R2,Biv,Susp) :-
	  no_possible_overlap(S1,S2,D2), !, 
	  Biv=0,
	  kill_suspension(Susp).
set_biv(_S1,_S2,_D2,_R2,_biv,_Susp).


/* 
init_repair_bivs(+Bivs,+BivSums,++Options)
This sets up the propagation from start time and duration tentative
    values to bivalued tentative values, and from bivalued tentative
    values to tentative values of the bivalued sums.  The propagation
    is performed by the 'my_tent_call' predicate as the ECLiPSe built-in
    tent_call propagates too early and sometimes fails as a result.
*/
init_repair_bivs(Bivs,BivSums,Options) :-
    Options = options with [granularity:Granularity, priority:Priority],
    Prior is Priority+1,
    (foreach(BivList,Bivs),foreach(BivSum,BivSums),
     param(Granularity,Prior)
     do
         (foreach(Biv,BivList),
          param(Granularity,Prior)
             do
             (
              get_bin_info(Biv,(Task1,Task2)) ->
	         Task1 = task with start:S1,
	         Task2 = task with [start:S2,duration:D2,resource:R2],
                 Cons= (S1>=S2,S1+Granularity=<S2+D2),
                 my_tent_call( (S1,S2,D2,R2), 
                               reified_cons_check(Cons,R2,0,Biv),
                               Biv,
                               Prior
                             )
              ; true  % Biv is 0 or a resource quantity
             )
         ),
         my_tent_call(BivList,sum(BivList,BivSum),BivSum,Prior)
    ).

/*
reified_cons_check(+Cons, ?True, ?False, -Biv)
This sets to boolean Bool to True if the constraint Cons succeeds and to 
False otherwise.  Cons is any ECLiPSe goal.
*/
reified_cons_check(Cons,True,False,Bool) :-
	call(Cons) -> Bool=True ; Bool=False.


:- comment(summary, "Probe Search").
:- comment(author, "Mark Wallace, Hani El Sakkout").
:- comment(date, "$Date: 2006/09/23 01:53:47 $").
:- comment(copyright, "Cisco Systems, INc.").


:- comment(make_overlap_bivs/5, [ 
    summary: "Make a set of overlap bivalued variables for a set of tasks.
Introduce a set of 'bivalued sum' variables, equal to the sum of the binaries at
an overlap.  The bivalued sum variables represent the total resources needed",
    amode:make_overlap_bivs(+,-,-,++,++),
    args:["Tasks": "A list of task structures - see library(ic_probe_support)",
          "BivLists": "A list of lists of integer variables",
          "BivSums": "A list of integer variables, each one the sum of a list 
                      of binaries",
          "Resource":"An integer quantity of resource available",
          "Options":"An options structure"
        ],
        resat:no,
        see_also:[probe_cstr_sched/7,add_con/3],
	desc:html("<P>
    Based on the tentative assignments, <B>probe_search</B> finds a task
    start time where the resources are not sufficient to make the
    tentative assignment feasible.  In case a bottleneck task has a
    variable resource requirement, this is reduced to its minimum
    possible value. Otherwise, <B>probe_search</B> chooses a bivalued
    'overlap' variable at this bottleneck and using <B>add_con</B> it adds 
    a constraint trying to eliminate the overlap.
</P>
")]).
