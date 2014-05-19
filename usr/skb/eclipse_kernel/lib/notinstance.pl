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
% Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: notinstance.pl,v 1.1 2008/06/30 17:43:48 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	notinstance.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		X ~= Y		(X not unifiable with Y)
%			X ~=< Y		(X not instance of Y)
%
% DESCRIPTION:
%

:- module(notinstance).

:- comment(summary, "Constraints for structural equality and subsumption").
:- comment(author, "Joachim Schimpf, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:48 $").

:- export op(700, xfx, (~=<)).

:- export (~=<)/2, (~=)/2.


:- use_module(library(fd)).

:- pragma(nodebug).

%-----------------------------------------------------------------
% Non-unifiability
% Fails if X and Y are non-unifiable, otherwise succeeds or delays.
% Only one delayed goal, explicit wavefront list.
% Failure is detected eagerly.
% Success may be detected late.
%-----------------------------------------------------------------

:- comment((~=)/2, [template:"X ~= Y",
    summary:"Constraints X and Y to be different",
    desc:html("Fails if X and Y are non-unifiable, otherwise succeeds
    or delays.  Unlike the implementation of the same predicate in the
    kernel, this one maintains and explicit wavefront and has only one
    delayed goal.  Failure is detected eagerly.  Success may be
    detected late.")]).

X ~= Y :-
	nu_wf([X~=Y]).


nu_wf([X~=Y|WF0]) :-
	nu3(X, Y, WF1, WF2),
	( WF2 == true ->
	    true
	; WF1 == WF2 ->
	    nu_wf(WF0)
	;
	    % delay on one undecidable pair (could be more precise)
	    WF1 = WF0,
	    WF2 = [First|_],
	    make_suspension(nu_wf(WF2), 2, Susp),
	    insert_suspension(First, Susp, bound of suspend, suspend)
	).


nu3(X, Y, Xr0, Xr) :-
	var(X), var(Y),
	X == Y,
	!,
	Xr = Xr0.		% continue
nu3(X, Y, Xr0, Xr) :-
	(var(X) ; var(Y)),
	!,
	Xr = [X~=Y|Xr0].	% delay
nu3(X, Y, Xr0, Xr) :-
	atomic(X), atomic(Y),
	X = Y,
	!,
	Xr = Xr0.
nu3(X, Y, Xr0, Xr) :-
	compound(X), compound(Y),
	functor(X, F, A),
	functor(Y, F, A),
	!,
	nu3_arg(X, Y, Xr0, Xr, A).
nu3(_X, _Y, _Xr0, true).	% succeed

nu3_arg(_X, _Y, Xr0, Xr, 0) :- !, Xr0 = Xr.
nu3_arg(X, Y, Xr0, Xr, N) :-
	arg(N, X, Xarg),
	arg(N, Y, Yarg),
	nu3(Xarg, Yarg, Xr0, Xr1),
	( Xr1 == true ->
	    Xr = true
	;
	    N1 is N-1,
	    nu3_arg(X, Y, Xr1, Xr, N1)
	).


%-----------------------------------------------------------------
% X ~=< Y	Constrain X not to be an instance of Y.
%
% We assume:
%	- no shared variables between X and Y
%	- X may get more instantiated, but not Y
% Failure is detected eagerly.
% Success may be detected late.
% The binding table is done very naively currently.
%-----------------------------------------------------------------

:- comment((~=<)/2, [template:"X ~=< Y",
    summary:"Constrain X not to be an instance of Y",
    desc:html("We assume:
    <UL>
	<LI> no shared variables between X and Y
	<LI> X may get more instantiated, but not Y
    </UL>
    Failure is detected eagerly. Success may be detected late.")]).

X ~=< Y :-
	ni_wf(X ~=< Y, [], _V).

ni_wf(X ~=< Y, WF0, V) :-
	!,
	ni(X, Y, V, WF1, WF2),
	( WF2 == true ->
	    true			% definitely not an instance
	; WF1 == WF2 ->
	    WF0 = [First|WF3],
	    ni_wf(First, WF3, V)	% continue along the wavefront
	;
	    % delay on one undecidable pair
	    WF1 = WF0,			% prepend
	    WF2 = [First|WF3],
	    make_suspension(ni_wf(First, WF3, V), 2, Susp),
	    ( First = (Lhs ~=< _) ->
		insert_suspension_into_constrained_lists(Lhs, Susp)
	    ; % First = (_ ~= _)
		insert_suspension(First, Susp, bound of suspend, suspend)
	    )
	).
ni_wf(X ~= Y, WF0, V) :-
	nu3(X, Y, WF1, WF2),
	( WF2 == true ->
	    true
	; WF1 == WF2 ->
	    WF0 = [First|WF3],
	    ni_wf(First, WF3, V)
	;
	    % delay on one undecidable pair
	    WF1 = WF0,
	    WF2 = [First|WF3],
	    make_suspension(ni_wf(First, WF3, V), 2, Susp),
	    insert_suspension(First, Susp, bound of suspend, suspend)
	).


ni(X, Y, _V, T0, T) :-
	var(X), nonvar(Y),
	!,
	T = [X~=<Y|T0].
ni(X, Y, _V, T0, T) :-			% nonvar(X);var(Y)
	meta(Y),
	\+ instance(X,Y),
	!,
	( atomic(X) -> T = true ; T = [X~=<Y|T0] ).
ni(X, Y, V, T0, T) :-			% nonvar(X);var(Y)
	var(Y), !,			% free(Y);meta(Y)
	lookup(Y, V, Map),
	( var(Map) ->
	    Map = map(X),		% remember the mapping Y=X
	    T0 = T			% one step closer to failure ...
	;
	    Map = map(Xold),
	    nu3(Xold, X, T0, T)		% add inequality constraint
	).
ni(X, Y, _V, T0, T) :-
	atomic(X),			% atomic(Y),
	X = Y,
	!,
	T0 = T.				% one step closer to failure ...
ni(X, Y, V, T0, T) :-
	compound(X), compound(Y),
	functor(X, F, A),
	functor(Y, F, A),
	!,
	ni_args(X, Y, V, T0, T, A).
ni(_X, _Y, _V, _T0, true).		% not instances: success

ni_args(_X, _Y, _V, T0, T, 0) :- !, T0 = T.
ni_args(X, Y, V, T0, T, N) :-
	arg(N, X, Xarg),
	arg(N, Y, Yarg),
	ni(Xarg, Yarg, V, T0, T1),
	( T1 == true ->
	    T = true
	;
	    N1 is N-1,
	    ni_args(X, Y, V, T1, T, N1)
	).


lookup(Key, List, Entry) :-
	var(List), !,
	List = [Key-Entry|_].
lookup(Key, [Key0-Entry0|Tail], Entry) :-
	Key == Key0 ->
	    Entry = Entry0
	;
	    lookup(Key, Tail, Entry).


%-----------------------------------------------------------------
% insert_suspension_into_constrained_lists
%-----------------------------------------------------------------

insert_suspension_into_constrained_lists(Term, Susp) :-
	term_variables(Term, Vars),
	insert_in_proper_lists(Vars, Susp).

insert_in_proper_lists([], _Susp).
insert_in_proper_lists([Var|Vars], Susp) :-
	( is_domain(Var) ->
	    insert_suspension(Var, Susp, any of fd, fd)
	;
	     true
	),
	insert_suspension(Var, Susp, constrained of suspend, suspend),
	insert_in_proper_lists(Vars, Susp).
