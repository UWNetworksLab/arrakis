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
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: fd_elipsys.pl,v 1.1 2008/06/30 17:43:45 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * IDENTIFICATION:      scheduling.pl
 *
 * AUTHOR:		Andre Veron
 *			Micha Meier
 *
 * DESCRIPTION:         This file contains the Prolog part of Elipsys
			finite domain constraints available in ECLiPSe

 * CONTENTS:	        disjunctive/3
                        disjunction/5
			disjunction_choose/5
			contigs/5
			sequences/4

 *
 * REVISION HISTORY:
	May 1993        Created the file
	January 1994	Ported to 3.4 [Micha Meier]

 * BUGS:                No type checking on the arguments of disjunctive/3
                        No type checking on the arguments of disjunction/5
 */


:- module(fd_elipsys).

:- import fd_arith.
:- import fd_chip.

:- export
    disjunctive/3,
    disjunction/5,
    disjunction_choose/5,
    contigs/5,
    sequence/4.

:- import
	contigs_interface/6,
	disjunctive_interface/4,
	disjunction_choose_interface/6,
	sequences_interface/5,
	setarg/3
    from sepia_kernel.

disjunctive(Starts,Durations,Flags):-
	% Same number of starting dates and durations
	length(Starts,Ls),
	length(Durations,Ld),
	Ls = Ld,

	% Create the arrays (structures) used by the constraint
	% Array of starting dates
	% Array of durations
	% Array of flags/orientations

	SStarts =.. [starts|Starts],
	SDurations =.. [durations|Durations],
	SS is Ls * Ls,
	functor(SFlags,flags,SS),

	% The elementary disjunctions monitoring the scheduling within
	% pairs of tasks.

	setup_disjunctions(Starts,Durations,Flags,SFlags),

	% The actual constraint
	disjunctive(SStarts,SDurations,Flags,SFlags),
	true.

disjunctive(Starts,Durations,Flags,SFlags):-

	% Perform a reduction
 	disjunctive_interface(Starts,Durations,SFlags, List),
	handle_requests(List),

	% Resuspend the constraint
	make_suspension(disjunctive(Starts,Durations,Flags,SFlags), 4, Susp),
	insert_suspension(Flags, Susp, inst of suspend, suspend).



setup_disjunctions(Starts,Durations,Flags,SFlags):-
	length(Starts,Arity),
	setup_disjunctions(Starts,Durations,Flags,SFlags,0,1,Arity).

setup_disjunctions([],[],[],_,_,_,_).
setup_disjunctions([S|Ss],[D|Ds],Flags,SFlags,I,J,Arity):-
        setup_disjunctions(Ss,S,Ds,D,Flags,FlagsTail,SFlags,I,J,Arity),
	I1 is I + 1,
	J1 is I1 + 1,
        setup_disjunctions(Ss,Ds,FlagsTail,SFlags,I1,J1,Arity).

setup_disjunctions([],_,[],_,Flags,Flags,_,_,_,_).
setup_disjunctions([S|Ss],SS,[D|Ds],DD,[Flag|Flags],FlagsOut,SFlags,I,J,Arity):-
	Flag :: 1..2,
	
	% Set up one elementary disjunction
	disjunction_choose(SS,DD,S,D,Flag),
%	disjunction(SS,DD,S,D,Flag),

	% Put the flag in the structure so that it can be accessed by the main 
	% constraint.
	% Flag of disjunction between Task_I and Task_J (I < J, I,J in [0..nstarts-1]) is the
	% (I*n_starts + J + 1)-th argument of the structure.

	P is I*Arity + J + 1,
	arg(P,SFlags,Flag),
	J1 is J + 1,
        setup_disjunctions(Ss,SS,Ds,DD,Flags,FlagsOut,SFlags,I,J1,Arity).

disjunction_choose(X1,D1,X2,D2,F) :-
    Goal = disjunction_choose(X1,D1,X2,D2,F),
    check_integer(D1, Goal),
    check_integer(D2, Goal),
    check_domain(X1, Goal),
    check_domain(X2, Goal),
    check_var(F, Goal),
    
    F :: 1 .. 2,
    disjunction_choose_1(X1,D1,X2,D2,F).

disjunction_choose_1(X1,D1,X2,D2,F):-
	disjunction_choose_interface(X1,D1,X2,D2,F, List),
	handle_requests(List),

	(var(F) ->
	    % Resuspend the constraint
	    make_suspension(disjunction_choose_1(X1,D1,X2,D2,F), 3, Susp),
	    insert_suspension(X1-X2, Susp, min of fd, fd),
	    insert_suspension(X1-X2, Susp, max of fd, fd),
	    insert_suspension(F, Susp, inst of suspend, suspend)
	;
	    % else we are done
	    true
	).

check_integer(X, _) :-
    integer(X),
    !.
check_integer(X, Goal) :-
    var(X),
    !,
    error(4, Goal).
check_integer(_, Goal) :-
    error(5, Goal).

check_domain(X, _) :-
    integer(X),
    !.
check_domain(X, _) :-
    is_integer_domain(X),
    !.
check_domain(X, Goal) :-
    var(X),
    !,
    error(4, Goal).
check_domain(_, Goal) :-
    error(5, Goal).

check_var(X, _) :-
    integer(X),
    !.
check_var(X, _) :-
    var(X),
    !.
check_var(_, Goal) :-
    error(5, Goal).


/* The contigs/5 constraint of ElipSys */


contigs(ListItems,Item,MaxSequences,Occurences,Contigs):-

	% Missing type checking - The list must be built with integers or 
	% with finite domain variables over integers

	% Array of items to speed up the accesses 
	ArrayItems =.. [items|ListItems],

	% Trivial reductions

	length(ListItems,Temp1),
	Temp2 :: 0 .. Temp1,
	Temp2 = MaxSequences,

	Temp3 :: 0 .. Temp1,
	Temp3 = Occurences,

	Temp4 is fix((Temp1 + 1)/2),
	Temp5 :: 1 .. Temp4,
	Contigs = Temp5,

	
	contigs_1(ArrayItems,MaxSequences,Item,Occurences,Contigs).

contigs_1(ArrayItems,MaxSequences,Item,Occurences,Contigs):-
	
	contigs_interface(ArrayItems,MaxSequences,Item,Occurences,Contigs, List),
	handle_requests(List),
	
	% Resuspend the constraint

	make_suspension(contigs_1(ArrayItems,MaxSequences,Item,Occurences,Contigs), 4, Susp),
	insert_suspension(ArrayItems-MaxSequences-Occurences-Contigs, Susp, bound of suspend, suspend),
	insert_suspension(ArrayItems-MaxSequences-Occurences-Contigs, Susp, min of fd, fd),
	insert_suspension(ArrayItems-MaxSequences-Occurences-Contigs, Susp, max of fd, fd).

sequence(ListItems,Item,MaxSequences,Occurences):-

	% Missing type checking - The list must be built with integers or 
	% with finite domain variables over integers

	% Array of items to speed up the accesses 
	ArrayItems =.. [items|ListItems],

	% Trivial reductions

	length(ListItems,Temp1),
	Temp2 :: 0 .. Temp1,
	Temp2 = MaxSequences,

	Temp3 :: 0 .. Temp1,
	Temp3 = Occurences,

	sequence_1(ArrayItems,MaxSequences,Item,Occurences).

sequence_1(ArrayItems,MaxSequences,Item,Occurences):-
	
	sequences_interface(ArrayItems,MaxSequences,Item,Occurences, List),
	handle_requests(List),
	
	% Resuspend the constraint

	make_suspension(sequence_1(ArrayItems,MaxSequences,Item,Occurences), 4, Susp),
	insert_suspension(ArrayItems-MaxSequences-Occurences, Susp, bound of suspend, suspend),
	insert_suspension(ArrayItems-MaxSequences-Occurences, Susp, min of fd, fd),
	insert_suspension(ArrayItems-MaxSequences-Occurences, Susp, max of fd, fd).

disjunction(Aa, Ad, Ba, Bd,Flag) :-
    (integer(Flag) ->
	( Flag == 1 ->
	    Aa + Ad #<= Ba;
	Flag == 2 ->
	    Ba + Bd #<= Aa;
	fail
	)
    ;
    
    domain_copy(Aa, Aa1),
    domain_copy(Ba, Ba1),
    subcall(Aa1 + Ad #<= Ba1, _) ->
	(domain_copy(Aa, Aa2),
	domain_copy(Ba, Ba2),
	subcall(Ba2 + Bd #<= Aa2, _) ->
	    % Both possibilities are valid
	    dvar_domain(Aa1, DA1),
	    dvar_domain(Aa2, DA2),
	    dvar_domain(Ba1, DB1),
	    dvar_domain(Ba2, DB2),
	    dom_union(DA1, DA2, DAU, _),
	    dom_union(DB1, DB2, DBU, _),
	    dvar_update(Aa, DAU),
	    dvar_update(Ba, DBU),
	    make_suspension(disjunction(Aa, Ad, Ba, Bd,Flag), 4, Susp),
	    insert_suspension(Flag, Susp, inst of suspend, suspend),
	    insert_suspension([Aa|Ba], Susp, min of fd, fd),
	    insert_suspension([Aa|Ba], Susp, max of fd, fd)
	; %%% second case failed
	    Aa + Ad #<= Ba,
	    Flag = 1
	)
    ; %%% first case failed
	Ba + Bd #<= Aa,
	Flag = 2
    ).

domain_copy(Domain, New) :-
	is_domain(Domain),
	!,
	dvar_domain(Domain, D),
	var_fd(New, D).
domain_copy(X, X).


handle_requests([]) :-
    wake.
handle_requests([R|L]) :-
    handle_request(R),
    handle_requests(L).

handle_request(update_min(Val,Var)) :- !,
	update_min(Val, Var),
	true.
handle_request(update_max(Val,Var)):- !,
	update_max(Val, Var),
	true.
handle_request(update_any(Val,Var)):- !,
	update_any(Val, Var),
	true.
handle_request(greatereq(Var1,Var2,Val)):- !,
	Var1 #>= Var2 + Val,
	true.

update_min(_, X):-
	nonvar(X),!.
update_min(Val, Var) :-
	Var #>= Val,
	true.

update_max(_, X) :-
	nonvar(X),!.
update_max(Val, Var) :- 
	Var #<= Val,
	true.

update_any(_, X) :-
	nonvar(X),!.
update_any(_Val, _{fd:Attr}) :- 
	-?->
	Attr = fd with any:Any,
	schedule_woken(Any),
	setarg(any of fd, Attr, []).
