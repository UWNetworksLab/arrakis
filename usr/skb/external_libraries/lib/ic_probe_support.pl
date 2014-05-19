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
:- module(ic_probe_support).

:- lib(repair).
:- lib(ic).
:- lib(ic_kernel).

:- export iddiff/3,
          idmemb/2,
          remove1/3,
          replace_list/4,
          set_to_tent/1,
          set_to_min/1,
          my_tent_call/4,
          demon_suspend/4,
          task_structure/4.
:- export struct(task(start,duration,resource)).
:- export struct(options(granularity,priority)).

:- import
        get_priority/1,
        set_priority/1
    from sepia_kernel.

:- tool(demon_suspend/4,demon_suspend/5).
:- tool(my_tent_call/4, my_tent_call/5).

/************************************************************/
/**************  Library Predicates   ***********************/
/************************************************************/

task_structure(Tasks,Starts,Durations,Resources) :-
	(foreach(task with [start:S,duration:D,resource:R],Tasks),
	 foreach(S,Starts),
	 foreach(D,Durations),
	 foreach(R,Resources)
         do check_resource(R)
        ).

check_resource(R) :- get_min(R) > 0, !.
check_resource(R) :-
	write(error,'Error: a task has a potentially zero resource: '),
	writeln(error,R),
	abort.	
 
/*
remove1(+Value,+InList,-OutList)
Arguments:
Value - The value or structure to be removed from a list
InList - The input list from which the value is to be removed
OutList - The remainder of the list after removing the value
*/ 
remove1(X,[H|T],T) :-
	nonvar(H), H=X, !.
remove1(X,[H|T],[H|R]) :-
	remove1(X,T,R).


/*
iddiff(+List1,+List2,-OutList)
Arguments:
List1 - an (input) list of variables
List2 - an (input) list of variables
OutList - an (output) variable

iddiff returns a list of variables occurring in the first list
    List1, but not in the second list List2
*/

iddiff(List1,List2,OutList) :-
	foreach(Var,List1),
	fromto([],This,Next,OutList),
	param(List2)
	do   (idmemb(Var,List2) -> Next=This ; Next=[Var|This]).

/* 
idmemb(?Var,+List)
Var is a variable
List is a list

idmemb succeeds if the variable is a member of the list: idmemb uses 
identity to test for membership, rather than unification as used by member.
*/

idmemb(Var,[H|_]) :- Var==H, !.
idmemb(Var,[_|T]) :- idmemb(Var,T).

/*
replace_list(+VarList,?Term,+ValList,?NewTerm)
Arguments:
VarList - a list of variables
Term - a term (containing some or all of those variables, among others)
ValList - a list of "things" (variables, atoms, terms) of the same
    length as the VarList
NewTerm - an (output) variable

Same as:
        copy_term_vars(VarList,
                       VarList-Term,
                       ValList-NewTerm),
but doesn't keep finite domains.

replace_list makes a new term NewTerm from an existing term Term, by
    replacing all the variables in the VarList by the values in the
    ValList 
*/
replace_list(VarList,Term,ValList,NewTerm) :-
	foreach(Var,VarList),foreach(Val,ValList),
	fromto(Term,This,Next,NewTerm)
	do replace(Var,This,Val,Next).

replace(Var,Term,Val,Val) :- Term==Var, !.
replace(_Var,Term,_Val,Term) :- (var(Term) ; atomic(Term)), !.
replace(Var,Term,Val,NewTerm) :-
	Term=..[F|Args],
        replace_each(Var,Args,Val,NewArgs),
	NewTerm=..[F|NewArgs].

replace_each(Var,List,Val,NewList) :-
	foreach(El,List), foreach(NewEl,NewList), param(Var,Val)
	do replace(Var,El,Val,NewEl).

/* Should be an ECLiPSe built-in */
set_to_tent(Term) :-
        Term tent_get Term.

set_to_min(Cost) :-
    get_min(Cost,MinCost),
    Cost=MinCost.



/*
demon_suspend(Goal,Prior,Cond,Susp)
This should probably be a built-in in ECLiPSe.  It suspends a
goal, but it also calls it immediately.
*/
:- inline(demon_suspend/4, tr_demon_suspend/2).
tr_demon_suspend(
	demon_suspend(Goal,Prior,Cond,Susp),
	(
	    suspend(Goal,Prior,Cond,Susp),
	    schedule_woken([Susp]),
	    wake
	)).

demon_suspend(Goal,Prior,Cond,Susp,Module) :-
          suspend(Goal,Prior,Cond,Susp)@Module,
          schedule_woken([Susp]),
          wake.

/*
my_tent_call(?InTerm,?Goal,?OutTerm,+Priority,+Module)
Arguments: 
InTerm - Any term containing variables whose tentative values are
    input to the propagation
Goal - Any goal which can be called in ECLiPSe
OutTerm - Any term containing variables whose tentative values will be
    set by this propagation.  

my_tent_call sets up two demons, one to perform propagation whenever the
    tentative values change, and the other to kill the first demon
    whenever the variables all become instantiated.
*/  
my_tent_call(InTerm,Goal,OutTerm,Priority,Module) :-
        shelf_create(tent_val(nil),GlobVar),
        term_variables(InTerm,InVars),
        term_variables(OutTerm,OutVars1),
	iddiff(OutVars1,InVars,OutVars),
	replace_list(InVars,Goal,InParams,Goal2),
	replace_list(OutVars,Goal2,OutParams,NewGoal),  
        Prior is Priority+1,      
        demon_suspend(tent_call_prior(InVars,InParams,NewGoal,GlobVar,OutParams,OutVars,Module),Priority,InVars->ga_chg,Susp),
        demon_suspend(kill_tent_check(InVars,Goal,Susp,S,Module),Prior,InVars->inst,S).


:- demon kill_tent_check/5.
kill_tent_check(InVars,Goal,Susp,S,Module) :-
	ground(InVars), !,
	call(Goal)@Module,
	kill_suspension(Susp),
	kill_suspension(S).
kill_tent_check(_,_,_,_,_).

/*
tent_call_prior(?InVars,?Goal,?OutVars,+Module)
Variables occurring in both the InVars and the OutVars are treated as input.
    The input variables are replaced by their tentative values and the
    output variables by new variables.  The resulting goal is then
    invoked, and the output values tentatively assigned to the output
    variables. 
*/
:- demon tent_call_prior / 7.
tent_call_prior(InVars,InParams,NewGoal,GlobVar,OutParams,_OutVars,Module) :-
        InVars tent_get Value,true,
        InParams = Value,
	( call(NewGoal)@Module -> 
           shelf_set(GlobVar,1,OutParams) ;
           shelf_set(GlobVar,1,nil)
        ), fail.
tent_call_prior(_InVars,_InParams,_NewGoal,GlobVar,_OutParams,OutVars,_Module) :-
        shelf_get(GlobVar,1,OutParams),
        (OutVars=nil -> fail ; OutVars tent_set OutParams).

:- comment(summary, "Probe Support Library").
:- comment(author, "Mark Wallace, Hani El Sakkout").
:- comment(date, "$Date: 2006/09/23 01:53:48 $").
:- comment(copyright, "Cisco Systems, Inc.").

:- comment(desc, html("
    A library exporting structures and predicates used in probing for scheduling.
<P>
The following two structures are exported:
<PRE>
task(start,duration,resource)
options(granularity,priority)
</PRE>
<P>
    ")).

