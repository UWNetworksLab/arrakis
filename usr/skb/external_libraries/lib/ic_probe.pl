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
:- module(ic_probe).

:- lib(ic).
:- lib(repair).
:- lib(eplex).
:- use_module(ic_probe_support).

:- export set_up_probe/5.
:- export add_con/3.
:- export set_probect/1, get_probect/1.

:- local variable(probect).

drop_warnings_in_xpress :-
        % Check whether XPRESS is loaded, in a way that won't die if no
	% solver could be loaded (for building on machines with no licences).
	( getval(loaded_solver, loaded(xpress, _))@eplex_s ->
            lp_set(warning_channel,-(warning_output))
        ; true
        ).

%:- drop_warnings_in_xpress.

set_probect(X) :-
    setval(probect,X).
get_probect(X) :- 
    getval(probect,X).

/*
add_con(?Con,++Solvers,+Options)
ConsList - a list of numeric equations or inequations: X=:=Y, X>=Y, X>Y, X=<Y, X<Y
Solvers - A list of solvers - [ic,linear] or any subset
Options - options structure 

The constraint is added to the listed solvers at one higher than the 
specified priority 

*/

add_con(ConsList,Solvers,Options) :-
      Options = options with [granularity:Gran,priority:Priority],
      handle_diseq(ConsList,Gran,NewConsList),
      ThisPrior is Priority-1,	
      call_priority(
                    (foreach(Solver,Solvers),param(NewConsList) 
                         do add_ineq(Solver,NewConsList)  
                    ),
                    ThisPrior).

handle_diseq(ConsList,Gran,NewConsList) :-
	( foreach(Cons,ConsList),
	  foreach(NewCons,NewConsList),
	  param(Gran)
        do
	  diseq_to_ineq(Cons,Gran,NewCons)
        ).

diseq_to_ineq(X > Y, Gran, X >= Y+Gran) :- !.
diseq_to_ineq(X < Y, Gran, X+Gran =< Y) :- !.
diseq_to_ineq(Other,_,Other).

add_ineq(ic,ConsList) :-
      ( foreach(Cons,ConsList) 
      do
        convert_to_type(ic,Cons,TypedCons),	
        ic:TypedCons
      ).

add_ineq(linear(Handle),ConsList) :-
      lp_add_constraints(Handle,ConsList,[]).

%  If the following lines are uncommented, the number of probes
%  increases dramatically!


%add_ineq(linear(Handle),ConsList) :-
%     term_variables(ConsList,Vars),
%      select_ic_vars(Vars,ICVars),
%      normalise_cstrs(ConsList,LinCons,[]),
%      lp_add(Handle,LinCons,ICVars),
%      lp_add(Handle,LinCons,[]),
%      ( test_tent(ConsList) -> true 
%      ;  
%        schedule_suspensions(lp_add),
%	wake
%      ).
%
%select_ic_vars(InVars,OutVars) :-
%	( foreach(Var,InVars),
%	  fromto([],This,Next,OutVars)
%        do
%	  (is_solver_var(Var) -> Next = [Var|This] ; Next=This)
%        ).
%
%test_tent(ConsList) :-
%	ConsList tent_get GConsList,
%        ( foreach(GCons,GConsList) do (ground(GCons), call(GCons))).

convert_to_type(ic,X=:=Y,X#=Y).
convert_to_type(ic,X>=Y,X#>=Y).
convert_to_type(ic,X=<Y,X#=<Y).



/*
set_up_probe(+Tasks,+Constraints,-Cost,++Options,-Handle)
Tasks - A list of tasks using the task structure defined in probe_lib
Constraints - A list of numeric equations and inequations, as above
Cost - A variable, to be minimised during search
Options - options structure

set_up_probe simply adds all the constraints to the linear solver.
They are added to the ic store as well - otherwise the cost
never becomes constrained and min_max returns a cost of 0 immediately!

The method must be primal as the dual becomes infeasible when a new
constraint is added, and thus causes an error:
Eplex: Optimization aborted (optimizer status = 3) in cplex_optimise...
*/

set_up_probe(Tasks,Constraints,Cost,Options,Handle) :-
        add_con(Constraints,[ic],Options),
        eplex:integers([Cost]),
        Options=options with priority:Priority,
        Prior is Priority-1,
        eplex:lp_demon_setup(min(Cost), 
                  Cost,
                  [    initial_solve(no),
                       collect_from(none),
                       method(primal),
                       sync_bounds(yes),
                       priority(Priority),
                       presolve(no)], 
                  [    suspension(Susp),
                       new_constraint,
                       pre(incval(probect)),
                       post(set_ans_to_tent(Handle))],
                  Handle),
        set_up_deviating_bounds_demon(Tasks, Prior, Susp, Handle),
        add_con(Constraints,[linear(Handle)], Options).

set_ans_to_tent(Handle) :-
       eplex:lp_get(Handle,vars,VarVector),
       (foreacharg(Var,VarVector), param(Handle) do
           eplex:lp_var_get(Handle, Var, solution, Sol0),
           get_solver_type(Var, VType),
           ( VType == integer -> 
               Sol is fix(round(Sol0)) 
           ; integer(Var) ->
               Sol is fix(round(Sol0)) 
           ;
               Sol0 = Sol
           ),
           Var tent_set Sol
       ).

set_up_deviating_bounds_demon( Tasks,Priority,Susp,Handle) :- 
    (foreach(task with [start:S,duration:D], Tasks),
     fromto([],This,Next,List)
     do Next =[S,D|This]
    ),
    (foreach(V, List), param(Priority,Handle,Susp) do
        ( nonvar(V) -> true 
        ; is_solver_var(V) ->
            lp_add_vars(Handle, [V]),
            suspend(deviating_bounds_demon(V,Handle,Susp,OwnSusp), Priority,
                    [V->ic:min,V->ic:max], OwnSusp)
        ; lp_var_occurrence(V, Handle, _) -> true
        ; fail
        )
    ).

tolerance(1e-05).

:- demon deviating_bounds_demon/4.
deviating_bounds_demon(X, Handle, Susp, OwnSusp) :-
        tolerance(Tol),
        (lp_var_get(Handle, X, solution, Sol) ->
            ic:get_bounds(X, Min, Max),
            (Min-Tol =< Sol, Sol =< Max+Tol ->
                schedule_suspensions(1, eplex([Susp])),
                wake
            ;
                true
            ),
            (nonvar(X) -> kill_suspension(OwnSusp) ; true)
        ;
            true  % no solution yet
        ).

:- comment(summary, "Probing").
:- comment(author, "Mark Wallace, Hani El Sakkout").
:- comment(date, "$Date: 2006/09/23 01:53:47 $").
:- comment(copyright, "Cisco Systems, Inc.").

:- comment(desc, html("<P>
    This implementation of probing is a call to an external linear solver, 
    whose optimal solution is assigned to the problem variables as tentative 
    variables.
</P><P>
A counter is created an initialised to zero.  At every probe it is incremented.
The counter can be set using <B>set_probect(N)</B>, and can be read using 
<B>get_probect(N)</B>
</P>" )
          ).

:- comment(set_up_probe/5, [ 
    summary : "Sets up a linear probe for a set of constraints",
    amode : set_up_probe(+,+,-,++,-),
    args :["Tasks" : "A term containing some variables",
          "Constraints" :"A list of numerical constraints",
          "Cost" :"A cost variable",
          "Options" :"An options structure",
          "Handle" :"A variable which will record the handle of the matrix 
used by the linear solver" 
        ],
        resat :no,
        see_also :[probe_cstr_sched/7,add_con/3,lp_demon_setup/5],
	desc :html("<P>
The constraints are passed to <B>add_con</B>, which adds them to both the 
ic and linear solvers.  The cost is declared to be an integer.  All the 
finite domain variables in the term <B>Tasks</B> are associated with a demon 
which forwards their bounds to the linear solver.  (The priority of this 
demon is one higher than that of the probe.)
</P><P>
<B>lp_demon_setup</B> is then invoked to set up the linear solver, with the 
priority specified in the <B>Options</B> parameter.  Solutions returned
by the linear solver are automatically used to update the tentative values  
of all the variables.
</P>
"),
       eg:"
?- Options = options{granularity:3,priority:5},
   Cost=X1,
   set_up_probe([X1,X2,X3],[X1>X2,X2>X3],Cost,Options,H).
"]).

:- comment(add_con/3 , [ 
    summary : "Add a constraint to the ic and linear solvers",
    amode :add_con(+,++,+),
    args :["Constraint": "A numerical constraint, with functor 
                         '=:=', '>=', '=<', '>' or '<'",
          "Options":"An options structure",
          "Handle":"A linear solver handle"
        ],
        resat :no,
        see_also :[set_up_probe/5],
	desc :html(" <P>
If the inequality is strict, <B>X>Y</B> or <B>X'<'Y</B>, then the granularity 
 specified in the options is added to the smaller term to create a non-strict 
 inequality, which can be passed to the linear solver.  Thus if the granularity
 is 3, then for <B>X>Y</B> the constraint <B>X>=Y+3</B> is added to the 
 linear  solver and  <B>X#>=Y+3</B>  is added to the ic solver.
</P>
")]).


