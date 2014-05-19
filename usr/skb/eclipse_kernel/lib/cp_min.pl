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
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: cp_min.pl,v 1.1 2008/06/30 17:43:44 jschimpf Exp $
% ----------------------------------------------------------------------

/*
/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * COST-PARALLEL MINIMIZATION
 *
 * IDENTIFICATION:      cp_min.pl 
 *
 * AUTHOR:		Steven Prestwich and Shyam Mudambi
 *
 * DESCRIPTION:     This library implements the cost-parallel
                    minimization predicates. There are currently
		    4 strategies implemented:
		i. min : the base strategy (an improved version
	                 of minimize/5).
	       ii. omin: cost-parallel version of min.
	      iii. pmin: min with pessimistic search (used only for
	                 sub-optimal solutions).
	       iv. opmin: cost-parallel version of pmin.

 */

:- module(cp_min).
:- use_module(library(fd)).
:- use_module(library(par_util)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- TopPreds = (
    cp_minimize/4,
    cp_minimize/8,
    cp_par_member/3,
    cp_par_indomain/2),
    export(TopPreds).

:-  tool(cp_minimize/4, cp_minimize_body/5),
    tool(cp_minimize/8, cp_minimize_body/9).

:- setval(verbose,0). /* set this to 1 for verbose output */

/* minimum distance to maintain between cost-parallel searches */
/* Should be user tunable? */
delta(10).

/* CP-Flag specifies whether to use optimisitic searches */
/* CP-Flag = on -> omin else min */
cp_minimize_body(Goal,Cost,CP_Flag,CostMonitor,Module) :-
   (var(Cost) ; Cost \= [_|_]),
   !,
   cp_minimize_body(Goal, [Cost], CP_Flag, CostMonitor, Module).
cp_minimize_body(Goal,List,CP_Flag,CostMonitor,Module) :-
   (check_list_domain(List) ->
       max_list_domain(List,Max),
       min_list_domain(List,Min), 
       cp_minimize_body(Goal,List,Min,Max,0,CP_Flag, off, CostMonitor,Module)
   ;
	error(5, cp_minimize(Goal, List, CP_Flag, CostMonitor), Module)
    ).

/* Psm_Flag specifies whether to use pessimistic search when
searching for sub-optimal solutions */
cp_minimize_body(Goal,Cost,L,H,P,CP_Flag, Psm_Flag, CostMonitor,Module) :-
   (var(Cost); Cost \= [_|_]),
   !,
   cp_minimize_body(Goal,[Cost],L,H,P,CP_Flag, Psm_Flag, CostMonitor,Module).
cp_minimize_body(Goal,List,L,H,P,CP_Flag, Psm_Flag, CostMonitor,Module) :-
   (check_list_domain(List) ->
        write_debug("Cost domain is {"),
	write_debug(L),
	write_debug(".."),
	write_debug(H),
	writeln_debug("}"),
	HP is ((100 * H) // (100 - P)) + 1, % ensures first solution when P>0
	/* Utility predicates */
	get_flag(hostname,Host),
	get_flag(workers,Host:W),
	initialise_cost,
	initialise_soln,
	set_cost(HP),
	(CP_Flag = on ->
	    delta(Delta),
	    search_factors(W,L,H,Delta,Factors),
	    write_debug("Speculation factors = "),
	    writeln_debug(Factors),
	    FactorsArr =.. [f|Factors],
	    functor(FactorsArr, f, Numspec),
	    multiple_searches(FactorsArr-Numspec,Goal,List,P,L,H,CostMonitor,Psm_Flag,Module) 
	;
	    multiple_searches(f-0,Goal,List,P,L,H,CostMonitor,Psm_Flag,Module)),   
	(get_soln(Goal,List,Optimal) ->
	    write("Optimal cost "),
	    write(Optimal),
	    (P==0 ->
		nl
	    ;  write(" to "),
	    write(P),
	    writeln("%"))
	;  
	    writeln("   warning: cp_minimization found no solutions"),
	    fail)
    ;
	!,
	error(5, cp_minimize(Goal,List,L,H,P,CP_Flag, Psm_Flag, CostMonitor),
	      Module)).


search_factors(W,L,H,Delta,Factors) :-
   ((H - L)/ Delta - W =< 0 ->
       Lnfactor is H - L,
       Delta1 is 1
   ;
       Lnfactor is (H - L)/ Delta - W,
       Delta1 is Delta),
   Alpha is max(0, ln(Lnfactor) / W),
   Beta is max(0,ln(Delta1)),
   search_factors(1,W,Alpha,Beta,Delta1,Factors).

search_factors(I,W,Alpha,Beta,Delta,[Factor|Factors]) :-
   I < W,
   !,
   I1 is I + 1,
   search_factors(I1,W,Alpha,Beta,Delta,Factors),
   Factor is fix(Delta * (I - 1) + exp(Alpha * I + Beta) + 0.5).
search_factors(_,_,_,_,_,[]).

:- parallel multiple_searches/9.

multiple_searches(Factors-Numspec,Goal,Cost,P,L,H,CostMonitor,_Psm_Flag,Module) :-
   fork(Numspec, Search),
   arg(Search,Factors,F),
   exhaustive_search(spec,Search,Goal,Cost,P,L,H,F,CostMonitor,Module).
multiple_searches(_,Goal,Cost,P,L,H,CostMonitor,_Psm_Flag,Module) :-
   not exhaustive_search(cons,dummy,Goal,Cost,P,L,H,P,CostMonitor,Module),
   !.
multiple_searches(_,Goal,Cost,P,L,H,CostMonitor, on,Module) :-
   P > 0,
   exhaustive_search(zero,dummy,Goal,Cost,P,L,H,0,CostMonitor,Module).

exhaustive_search(SearchType,Index,Goal,Cost,P,L,_H,Factor,CostMonitor,
	Module) :-
	P100 is 100 - P,
	(SearchType == zero ->
	    CostMonitor = zero(Cost,P100),
	    writeln_debug("   Zero")
	;
	    SearchType == spec ->
	       write_debug("   Spec "),
	       writeln_debug(Factor),
	       (P == 0 ->
		   CostMonitor = spec(Cost,Factor,Index)
	       ;  
	           CostMonitor = spec(Cost,Factor,Index,P100))
	    ;
	       P == 0 ->
	           writeln_debug("   Cons"),
		   CostMonitor = cons(Cost)
	       ;  
	           writeln_debug("   Cons"),
		   CostMonitor = cons(Cost,P100)),
        anti_thrashing(CostMonitor),
        set_list_gt(Cost, L), 
	call(Goal,Module),
	anti_thrashing(CostMonitor),
	max_list_domain(Cost, Max),
	NewMax is Max - 1,
	set_cost(NewMax),
	set_soln(Goal,Cost,Max),
	(getval(verbose,1) ->
	    write_debug("Current cost "),
	    writeln_debug(Max)
	;
	    true),
	fail.
exhaustive_search(_,_,_,_,_,_,_,Factor,_,_) :-
	write_debug("   Halt "),
	writeln_debug(Factor),
	fail.

% COST-PARALLELISM PROGRAMMING TOOLS

cp_par_indomain(X,I) :-
   dom(X,D),
   cp_par_member(X,D,I).

cp_par_member(X,L,I) :-
   functor(I,Func,_),
   (Func == cons ->
      anti_thrashing(I),
      cp_par_member1(X,L,I)
   ;Func == spec ->
     cp_member(X,L,I)          % (1) without hybridisation
%      arg(3,I,Index),           % (2) with
%      spec_member(Index,X,L,I)  %     hybridisation
   ;  cp_member(X,L,I)).

spec_member(N,X,L,I) :-
   N2 is N // 2,
   Parities is N2 + 2,
   Direction is N - N2 * 2,
   (Direction == 0 ->
      countdown(Parities,1,Parity,I)
  ;   countup(1,Parities,Parity,I)),
   parity_mem(L,Parities,Parity,X,I).

parity_mem(L,Parities,Parity,X,I) :-
   Parity1 is Parity - 1,
   discard(Parity1,L,L1),
   (L1=[X|_]
   ;N is Parities - Parity1,
    discard(N,L1,L2),
    anti_thrashing(I),
    parity_mem(L2,Parities,Parity,X,I)).

discard(0,L,L) :- !.
discard(N,[_|L],R) :-
   N1 is N - 1,
   discard(N1,L,R).

countup(A,_,A,_).
countup(A,B,C,I) :-
   A < B,
   anti_thrashing(I),
   D is A + 1,
   countup(D,B,C,I).

countdown(A,_,A,_).
countdown(A,B,C,I) :-
   A > B,
   anti_thrashing(I),
   D is A - 1,
   countdown(D,B,C,I).

cp_member(X,[X|_],_I).
cp_member(X,[_,H|T],I) :-
   anti_thrashing(I),
   cp_member(X,[H|T],I).

:- parallel cp_par_member1/3.

cp_par_member1(X,[X|_],_I).
cp_par_member1(X,[_,H|T],I) :-
   anti_thrashing(I),
   cp_par_member1(X,[H|T],I).

anti_thrashing(cons(Cost)) :- !,
   get_cost(CC),
   set_list_lteq(Cost,CC).
anti_thrashing(cons(Cost,P100)) :- !,
   get_cost(CC),
   CCF is (CC * P100 + 50) // 100,
   set_list_lteq(Cost,CCF).
anti_thrashing(spec(Cost,Factor,_)) :- !,
   get_cost(CC),
   CCF is CC - Factor,
   set_list_lteq(Cost,CCF).
anti_thrashing(spec(Cost,Factor,_,P100)) :- !,
   get_cost(CC),
   CCF is (CC * P100 + 50) // 100 - Factor,
   set_list_lteq(Cost,CCF).
anti_thrashing(zero(Cost,P100)) :-
   get_cost(CC),
   CCF is (CC * P100 + 50) // 100,
   CCF < CC,
   set_list_lteq(Cost,CC).


/* Utility predicates */
initialise_cost :-
   set_cost(dummy).

get_cost(C) :-
     getval(limiting_cost, C).

set_cost(C) :-
     setval(limiting_cost, C).

initialise_soln :-
   setval(sol,dummy).

get_soln(Goal,Cost,Optimal) :-
   getval(sol,lc(Goal,Cost,Optimal)).

set_soln(Goal,Cost,Optimal) :-
   setval(sol,lc(Goal,Cost,Optimal)).

conv_to_dvars([],[]).
conv_to_dvars([H|L],[H|R]) :-
	is_integer_domain(H),
	!,
	conv_to_dvars(L,R).
conv_to_dvars([H|L],[V|R]) :-
	V #= H,
	conv_to_dvars(L,R).

check_list_domain([]).
check_list_domain([H|R]) :- 
	is_integer_domain(H),
	check_list_domain(R).

max_list_domain([H|Rest],Max) :-
	maxdomain(H,CurMax),
	max_list_domain(Rest,CurMax,Max).

max_list_domain([],Max,Max).
max_list_domain([H|Rest],CurMax,Max) :-
	maxdomain(H,HMax),
	(HMax > CurMax ->
	    max_list_domain(Rest,HMax, Max)
	;
	    max_list_domain(Rest,CurMax,Max)).

min_list_domain([H|Rest],Min) :-
	mindomain(H,CurMin),
	min_list_domain(Rest,CurMin,Min).

min_list_domain([],Min,Min).
min_list_domain([H|Rest],CurMin,Min) :-
	mindomain(H,HMin),
	(HMin < CurMin ->
	    min_list_domain(Rest,HMin, Min)
	;
	    min_list_domain(Rest,CurMin,Min)).

set_list_gt([],_).
set_list_gt([C|Cs],Val) :-
	C #> Val,
	set_list_gt(Cs,Val).

set_list_lteq([],_).
set_list_lteq([C|Cs], Val) :-
	C #<= Val,
	set_list_lteq(Cs,Val).


writeln_debug(String) :-
	(getval(verbose,1) ->
	    writeln(String)
	;
	    true).

write_debug(String) :-
	(getval(verbose,1) ->
	    write(String)
	;
	    true).

nl_debug :-
	(getval(verbose,1) ->
	    nl
	;
	    true).
