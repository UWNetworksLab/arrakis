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
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: varlists.pl,v 1.1 2008/06/30 17:43:51 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(varlists).

:- comment(summary, "Predicates to manipulate lists containing variables").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:51 $").


:- export
	delete/3,
	intersection/3,
	memberchk/2,
	nonmember/2,
	subtract/3,
	union/3.


memberchk(X,[Y|_]) :- X==Y, !.
memberchk(X,[_|T]):- memberchk(X,T).


nonmember(X,[Y|_]) :- X==Y, !,
	fail.
nonmember(Arg,[_|Tail]) :-
	!,
	nonmember(Arg,Tail).
nonmember(_,[]).


% delete (?Element, ?List, ?Result)
% Result is List with Element removed
delete(A, [B|C], C) :- A==B.
delete(A, [B|C], [B|D]) :-
	delete(A, C, D).

% intersection(L1, L2, L3)
% L3 is the intersection of L1 and L2, with arguments ordered as in L1

intersection([], _, []).
intersection([Head|L1tail], L2, L3) :-
	memberchk(Head, L2),
	!,
	L3 = [Head|L3tail],
	intersection(L1tail, L2, L3tail).
intersection([_|L1tail], L2, L3) :-
	intersection(L1tail, L2, L3).


% subtract(L1, L2, L3)
% L3 = L1 - L2

subtract([], _, []).
subtract([Head|L1tail], L2, L3) :-
	memberchk(Head, L2),
	!,
	subtract(L1tail, L2, L3).
subtract([Head|L1tail], L2, [Head|L3tail]) :-
	subtract(L1tail, L2, L3tail).


% union(L1, L2, L3)
% L3 is (L1-L2) + L2

union([], L, L).
union([Head|L1tail], L2, L3) :-
	memberchk(Head, L2),
	!,
	union(L1tail, L2, L3).
union([Head|L1tail], L2, [Head|L3tail]) :-
	union(L1tail, L2, L3tail).

