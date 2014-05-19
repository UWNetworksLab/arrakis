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
% Version:	$Id: fd_chip.pl,v 1.1 2008/06/30 17:43:45 jschimpf Exp $
% ----------------------------------------------------------------------

/*
/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * FINITE DOMAINS
 *
 * IDENTIFICATION:      chip.pl 
 *
 * AUTHOR:		Micha Meier
 *
 * DESCRIPTION:         CHIP compatibility package.
 */


:- module(fd_chip).

:- import fd_arith.
:- import fd_util.

:- export
    deletemin/3,

    % CHIP 3:
%    dvar/1,
%    dvarint/2,

%    indomain/2,
%    delete/5,

%    domain_info/6,
    dom/2,

    % CHIP 2:
    deleteff/3,
    deleteffc/3,
    maxdomain/2,
    mindomain/2,

    alldistinct/1.

:- pragma(nodebug).
:- pragma(system).


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Built-In Predicates for CHIP compatibility
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dom(_{fd:(fd with domain:D)}, List) :-
	-?->
	!,
	dom_to_list(D, List).
dom(A, L) :-
	atomic(A),
	!,
	L = [A].
dom(X, List) :-
	error(4, dom(X, List)).

mindomain(Var, Min) :-
    (dvar_range(Var, Min0, _) ->
	Min = Min0
    ;
	error(5, mindomain(Var, Min))
    ).

maxdomain(Var, Max) :-
    (dvar_range(Var, _, Max0) ->
	Max = Max0
    ;
	error(5, maxdomain(Var, Max))
    ).


%
% The predicates for first fail principle
% They are written such that they don't perturb the list order
%

deleteff(Var, List, Rest) :-
    List = [H|T],
    dvar_size(H, Card),
    ( Card == 1 ->
	Rest = T,
    	Var = H
    ;
	find_least_domain(T, List, Card, Chosen, Rest),
	Var = Chosen
    ).

%:- mode find_least_domain(+,+,+,-,-).
find_least_domain([], SoFar, _OldCard, BestVar, Rest) :- !,
    SoFar = [BestVar|Rest].
find_least_domain(List, SoFar, OldCard, BestVar, Rest) :-
    List = [Var|Vars],
    dvar_size(Var, NewCard),
    ( NewCard == 1 ->				% take constant and stop
	BestVar = Var,
    	copy_until(SoFar, List, Rest, Vars)
    ; NewCard >= OldCard ->			% keep old one
	find_least_domain(Vars, SoFar, OldCard, BestVar, Rest)
    ;						% new optimum
    	copy_until(SoFar, List, Rest, Rest0),
	find_least_domain(Vars, List, NewCard, BestVar, Rest0)
    ).


deleteffc(Var, List, Rest) :-
    List = [H|T],
    dvar_size(H, Card),
    (Card == 1 ->
	Rest = T,
	Var = H
    ;
	find_least_domainffc(T, List, Card, _Num, Chosen, Rest),
	Var = Chosen
    ).

%:- mode find_least_domain(+,+,+,?,-,-).
find_least_domainffc([], SoFar, _OldCard, _OldNum, BestVar, Rest) :- !,
    SoFar = [BestVar|Rest].
find_least_domainffc(List, SoFar, OldCard, OldNum, BestVar, Rest) :-
    List = [Var|Vars],
    dvar_size(Var, NewCard),
    ( NewCard == 1 ->				% take constant and stop
	BestVar = Var,
    	copy_until(SoFar, List, Rest, Vars)
    ; NewCard > OldCard ->			% keep old one
	find_least_domainffc(Vars, SoFar, OldCard, OldNum, BestVar, Rest)
    ; NewCard < OldCard ->			% new optimum
	copy_until(SoFar, List, Rest, Rest0),
	find_least_domainffc(Vars, List, NewCard, _Num, BestVar, Rest0)
    ;
	% compute constraints_number of best lazily:
	( var(OldNum) -> constraints_number(BestVar, OldNum) ; true ),
	constraints_number(Var, Num),
	( Num =< OldNum ->			% keep old one
	    find_least_domainffc(Vars, SoFar, OldCard, OldNum, BestVar, Rest)
	;					% new optimum
	    copy_until(SoFar, List, Rest, Rest0),
	    find_least_domainffc(Vars, List, NewCard, Num, BestVar, Rest0)
	)
    ).


% find the List element with the smallest lower bound

deletemin(Var, List, Rest) :-
    List = [H|T],
    dvar_range(H, Min, _),
    find_min_domain(T, List, Min, Chosen, Rest),
    Var = Chosen.

%:- mode find_min_domain(+,+,+,-,-).
find_min_domain([], SoFar, _OldMin, BestVar, Rest) :- !,
    SoFar = [BestVar|Rest].
find_min_domain(List, SoFar, OldMin, BestVar, Rest) :-
    List = [Var|Vars],
    dvar_range(Var, NewMin, _),
    ( NewMin >= OldMin ->			% keep old one
	find_min_domain(Vars, SoFar, OldMin, BestVar, Rest)
    ;						% new optimum
    	copy_until(SoFar, List, Rest, Rest0),
	find_min_domain(Vars, List, NewMin, BestVar, Rest0)
    ).


    % Copy list In until a tail matching Until is reached.
    % Output in difference list Out-Out0
    copy_until(In, Until, Out, Out0) :-
    	( In == Until ->
	    Out = Out0
	;
	    In = [X|In1],
	    Out = [X|Out1],
	    copy_until(In1, Until, Out1, Out0)
	).


alldistinct([]).
alldistinct([H|T]) :-
    outof(H, T),
    alldistinct(T).

