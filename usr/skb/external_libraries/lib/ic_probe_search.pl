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
:- module(ic_probe_search).

:- lib(ic).
:- lib(repair).
:- lib(eplex).

:- use_module(bin_info).
:- use_module(ic_probe).
:- use_module(ic_make_overlap_bivs).
:- use_module(ic_probe_support).

:- export probe_search/5.
/*
probe_search(?BivLists,?BivSums,+Resource,+Options,+Handle)
Arguments:
All these arguments are described above, viz:

BivLists - A list of lists of binary variables, each of which has an attribute
    recording which two tasks it relates
BivSums - A list of finite domain variables recording the sum of the
    binaries associated with a given task start time
Resource - An integer indicating how many resources are available
Cost - A variable whose value will be minimised during search
Options - options structure
Handle - Passed to add_cons.  Currently this is not used.

    Based on the tentative assignments, find_bottleneck finds a task
    start time where the resources are not sufficient to make the
    tentative assignment feasible.  In case a bottleneck task has a
    variable resource requirement, this is reduced to its minimum
    possible value. Otherwise, find_bottleneck chooses a binary
    "overlap" variable at this bottleneck.  add_biv_cstr then adds 
    a constraint trying to eliminate the overlap.

*/


probe_search(Bivs,BivSums,Resource,Options,Handle) :-
    ( find_bottleneck(Bivs,BivSums,Resource,Biv)
          ->  add_biv_cstr(Biv,Options,Handle),
              probe_search(Bivs,BivSums,Resource,Options,Handle)
      ;       true
    ).

/* The following predicates are used to distiguish between binary variables, 
representing the potential resource need if two tasks overlap, and 
resource variables representing a (variable) resource need, because 
the associated tasks DO overlap!
*/

biv_var(Biv) :-
	get_bin_info(Biv,_).

resource_var(Res) :-
	var(Res),
	get_min(Res)>0.



/*
add_biv_cstr(-Biv,+Options,Handle)
Arguments:
Biv - a binary variable with an attribute recording which two tasks
       it relates
Options - options structure


add_biv_cstr is used in search.  If the variable is a resource 
variable, it is simply set to its minimum value.  In this case the
predicate is deterministic.

Otherwise, add_biv_cstr tries setting the bivalued
variable Biv to 0 and then on backtracking sets it to 1.  The
subtlety lies in this: that Biv has an attribute recording the two
tasks whose overlap it represents.  When setting Biv to 0,
add_biv_cstr adds a constraint forcing these tasks apart.  When
setting it to 1, add_biv_cstr forces the two tasks to overlap.

It first tries forcing the first 
task to start after the second has finished.  On backtracking once it 
"gives up", sets the binary variable to the amount of resource required,
and forces the two tasks to overlap.  On backtracking again it then tries 
to force the second task to start after the first.  

*/

add_biv_cstr(Biv,_Options,_Handle) :-
     resource_var(Biv), !,
     set_to_min(Biv).

add_biv_cstr(Biv,Options,Handle) :-
     get_bin_info(Biv,(Task1,Task2)),
     Task1 = task with start:S1,
     Task2 = task with [start:S2,duration:D2,resource:R2],	
     (Biv=0,add_con([S1>=S2+D2],[ic,linear(Handle)],Options)
      ; 
      Biv=R2,
      add_con([S1>=S2,S1<S2+D2],[ic,linear(Handle)],Options)
      ;
      Biv=0,
      add_con([S2>S1],[ic,linear(Handle)],Options)
     ).

/*
find_bottleneck(+BivLists,+BivSums,++Resource,-Biv)
BivLists - A list of (lists of binaries) - each list corresponding to
a single task
BivSums - A list of binary sums: each one, the sum of the list of
binaries in the previous argument.
Resource - The (integer) amount of resurce available
Biv - The binary representing the two tasks which should be forced
apart at the next choice point.

find_bottleneck first of all chooses the bottleneck tasks.  These are
the ones whose binary sums have the largest tentative value (which
exceeds the resource limit).  All such tasks have the same tentative
binary sum, 'Max'.  The difference between the resource limit and
this upper bound, 'Excess', is the amount by which the resources must 
be reduced to meet the resource limit.

The tightest binary is the one with the biggest overlap (from the
previous probe) that must be set to zero to achieve feasibility.
*/
find_bottleneck(AllBivLists,BivSums,Resource,Biv) :-
        max_tent_sum(AllBivLists,BivSums,Resource,Max,BivLists),
        Excess is Max-Resource,
        Excess>0,
        find_tightest_biv(BivLists,Excess,Biv).

/* Find all the bottlenecks - i.e. those tasks start times where the 
resource excess is greatest.  Each such bottleneck has an associated 
list of binary variables, BivList, and an associated sum of the binaries, 
BivSum.  All these bottlenecks have the same (excessive) resource
requirement, Max.
*/
max_tent_sum(AllBivLists,BivSums,Resource,Max,BivLists) :-
        (foreach(BivList,AllBivLists),
	 foreach(BivSum,BivSums),
         fromto(-1,TMax,NMax,Max),
         fromto([],TList,NList,BivLists),
         param(Resource)
         do  (BivSum tent_get TSum,
                ( TSum<Resource -> NMax=TMax, NList=TList 
                ; TSum>TMax ->   NMax=TSum, NList=[BivList] 
                ; TSum=TMax ->   NMax=TMax, NList=[BivList|TList] 
                ;                NMax=TMax, NList=TList
                )
             )
        ). 

/* 
find_tightest_biv(?BivLists,+Excess,-OutBiv)
BivLists - The list of lists, comprising the list of binaries for
           each of the bottleneck tasks 
Excess - the (integer) amount by which the resources must be reduced 
         to meet the resource limit
OutBiv - The chosen "tightest" binary

find_tightest_biv finds a resource variable at a bottleneck, 
if there is one.

Otherwise, for each bottleneck task, it returns the tightest
binary, and gives it a score (the tentative overlap).  The chosen
binary is the one with the greatest score.

*/
find_tightest_biv(BivLists,_,OutBiv) :-
	member(BivList,BivLists),
	member(OutBiv,BivList),
	resource_var(OutBiv), !.



find_tightest_biv(BivLists,Excess,OutBiv) :-
	(foreach(BivList,BivLists),
	 foreach((Olap-Biv),OlapBivs),
         param(Excess) do
            ftb(BivList,Excess,Olap,Biv)
        ),
        greatest_overlap(OlapBivs,OutBiv).

/* Returns the binary variable with the greatest associated overlap
*/
greatest_overlap(OlapBivs,OutBiv) :-
	( foreach(Olap-Biv,OlapBivs),
	  fromto(0,ThisLeast,NextLeast,_),
	  fromto(_,ThisBiv,NextBiv,OutBiv)
	do
	  (ThisLeast>=Olap -> [NextLeast,NextBiv] = [ThisLeast,ThisBiv]
           ;                  [NextLeast,NextBiv] = [Olap,Biv]
          )
        ).


/*
ftb(?BivList,+Excess,-Olap,-Biv)
ftb finds, among the binary variables with a non-zero tentative value, 
the "tightest" one that would still have to be set to zero even if we 
started from the least tight.  By "tighter" we mean "linking two tasks 
that have a larger overlap"
*/

ftb(BivList,Excess,Olap,Biv) :-
        tent_olaps_only(BivList,BivVars,TentBivSum),  
        RemainingExcess is TentBivSum-Excess,     
	add_olap_key(BivVars,OlapBivList),
        sort(1, '>=', OlapBivList, SortedOlapBivList),
        tightest_non_olap(RemainingExcess,SortedOlapBivList,Olap-Biv).

/* Select only binary variables that have a non-zero tentative value
*/
tent_olaps_only(List,Vars,TentSum) :-
	( foreach(El,List),
	  fromto([],This,Next,Vars),
          fromto(0,ThisSum,NextSum,TentSumExpr)
          do  (
	         var(El), El tent_get T, T\==0 -> 
		          Next=[El|This],
                          NextSum = ThisSum+T 
                  ;       Next=This,
                          NextSum = ThisSum
              )
        ),
        TentSum is TentSumExpr.

/*
Select the "tightest" binary variable that would still have to be set 
to zero even if we started from the least tight.
*/ 
tightest_non_olap(Excess,[Key-Biv|Bivs],Result) :-
	RemainingExcess is Excess - tent_get(Biv),
        (
	    RemainingExcess<0 -> Result = Key-Biv
         ;  
	    tightest_non_olap(RemainingExcess,Bivs,Result)
        ).

/* add_olap_key(List,OlapList)
List - a list of binaries (with attribute bin_info)
OlapList - a corresponding list of terms of the form Olap-Biv
       where Olap is an integer representing the "tightness of the
       binary.

       These binaries represent the overlaps for a particular task
       start time.  The aim is to find the task to force apart from
       the given task so as to reduce the number of overlapping tasks.

       The overlap is the minimum you would have to "push" the tasks
       to prevent them overlapping.  Notice that this is NOT the 
       minimum you would have to "push" the tasks to set the overlap
       binary to zero.  This can be achieved while the tasks still
       overlap, as long as the start time of task1 does not overlap
       with task 2. 
*/
 
add_olap_key(List,OlapList) :-
	( foreach(Biv,List), foreach(Olap-Biv,OlapList)
        do
             (    get_bin_info(Biv,(Task1,Task2)),
                  Task1 = task with [start:S1,duration:D1],
                  Task2 = task with [start:S2,duration:D2],
                  Olap is min(tent_get(S2)+tent_get(D2)-tent_get(S1),
                              tent_get(S1)+tent_get(D1)-tent_get(S2))
             )
        ).



:- comment(summary, "Probe Search").
:- comment(author, "Mark Wallace, Hani El Sakkout").
:- comment(date, "$Date: 2006/09/23 01:53:47 $").
:- comment(copyright, "Cisco Systems, Inc.").

:- comment(desc, html("
    A search routine which fixes resource bottlenecks by forcing tasks not to overlap.
    ")).

:- comment(probe_search/5, [ 
    summary: "Add alternative constraints to try and repair infeasible probes",
    amode:probe_search(+,+,++,++,+),
    args:["Bivs": "A list of lists of binary integer variables",
          "BivSums": "A list of integer variables, each one the sum of a list 
                      of binaries",
          "Resource":"An integer quantity of resource available",
          "Options":"An options structure",
          "Handle":"A linear solver handle"
        ],
        resat:no,
        see_also:[probe_cstr_sched/7,add_con/3],
	desc:html("<P>
    Based on the tentative assignments, <B>probe_search</B> finds a task
    start time where the resources are not sufficient to make the
    tentative assignment feasible.  In case a bottleneck task has a
    variable resource requirement, this is reduced to its minimum
    possible value. Otherwise, <B>probe_search</B> chooses a binary
    'overlap' variable at this bottleneck and using <B>add_con</B> it adds 
    a constraint trying to eliminate the overlap.
</P>
")]).
