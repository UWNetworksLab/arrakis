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
% Version:	$Id: fd_util.pl,v 1.1 2008/06/30 17:43:45 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * FINITE DOMAINS
 *
 * IDENTIFICATION:      fd_util.pl 
 *
 * AUTHOR:		Micha Meier
 *
 * DESCRIPTION:         Various utility predicates for the finite
			domains library. They usually put together
			several calls of built-in predicates that
			occur frequently enough.

   CONTENTS:
			dvar_domain_list(Var, List)
			dvar_range(Var, Min, Max)
			dvar_size(Var, Size)

			labeling(List)
 *
 */


:- module(fd_util).

:- import fd_arith.


:- export
    # /3,
    dvar_domain_list/2,
    dvar_range/3,
    dvar_size/2,
    labeling/1,
    outof/2.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 	Domain processing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% return the list of elements in the domain of Var
%
dvar_domain_list(_{fd:(fd with domain:D)}, List) :-
    -?->
    !,
    dom_to_list(D, List).
dvar_domain_list(X, List) :-
    nonground(X),
    !,
    error(4, dvar_domain_list(X, List)).
dvar_domain_list(A, [A]).

%
% return the size of a finite term
%
dvar_size(Var, Size) :-
    dvar_domain(Var, Dom),
    dom_size(Dom, Size).

dvar_range(Var, Min, Max) :-
    dvar_domain(Var, Dom),
    dom_range(Dom, Min, Max).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 	Composed Predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

outof(_, []).
outof(X, [H|T]) :-
    X ## H,
    outof(X, T).

%
% Cardinality operator
%
#(Min, ConstrList, Max) :-
    Min #<= Max,
    bool_sum(ConstrList, Sum),
    length(ConstrList, Length),
    Sum #>= Min,
    Sum #<= Max,
    Min #>= 0,
    Max #<= Length.
    
bool_sum([], 0).
bool_sum([C|L], B+S) :-
    B isd C,
    bool_sum(L, S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 	Labeling
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

labeling([]) ?- !.
labeling([X|L]) ?- !,
    indomain(X),
    labeling(L).
labeling(Junk) :-
    error(5, labeling(Junk)).
