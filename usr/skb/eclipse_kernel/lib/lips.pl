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
% Copyright (C) 1991-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: lips.pl,v 1.2 2008/08/31 23:10:45 jschimpf Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	lips.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		lips/0,1,2
%
% DESCRIPTION:		Measure the system's speed using naive reverse
%

:- module(lips).

:- comment(summary, "Measure the system's speed using the naive reverse benchmark").
:- comment(author, "Joachim Schimpf, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/08/31 23:10:45 $").
:- comment(desc, html("
    Measure the system's speed in logical inferences per second, using
    the infamous naive reverse program. This test does not say very much
    about the quality of a system. All it gives is an indication about
    the speed of list processing.
    <P>
    The executed program is:
    <PRE>
    nreverse([], []).
    nreverse([X|L0],L) :-
	    nreverse(L0, L1),
	    concatenate(L1, [X], L).

    concatenate([], L, L).
    concatenate([X|L1], L2, [X|L3]) :-
	    concatenate(L1, L2, L3).
    </PRE>
    and the standard benchmark is to call nreverse/2 with a 30-element
    list as the first and a variable as the second argument.  This
    instance is assumed to have 496 logical inferences.
    ")).
:- comment(lips/0, [template:"lips",
    summary:"Run the benchmark a reasonable number of times and print the average speed"
    ]).
:- comment(lips/1, [template:"lips(+Count)",
    summary:"Run the benchmark Count times and print the average speed"
    ]).
:- comment(lips/2, [template:"lips(+Count,+Length)",
    summary:"Run the benchmark Count times with lists of length Length"
    ]).

:- export lips/0, lips/1, lips/2.

:- pragma(nodebug).
?- set_flag(gc, off).

lips :-
	lips(10000, 30).

lips(Count) :-
	lips(Count, 30).

lips(Count, Length) :-
	conslist(Length, List),

	cputime(T4),
	compens_loop(Count, List),
	cputime(T5),
	Empty is T5-T4,

	cputime(T0),
	call_loop(Count, List),
	cputime(T1),
	T is T1-T0-Empty,

	printf("%d iterations of nrev(%d)\n", [Count,Length]),

	LI is Length*(Length+3)/2 + 1,
	LIPS is LI * Count / T / 1000,
	printf("%d * %.1f inferences / %.3f seconds / 1000 = %.1f KLIPS\n",
		[Count,LI,T,LIPS]).


compens_loop(N, List) :-
	setval(count, N),
	repeat,
	dummy(List,_),
	decval(count),
	getval(count, 0),
	!.

call_loop(N, List) :-
	setval(count, N),
	repeat,
	nreverse(List,_),
	decval(count),
	getval(count, 0),
	!.

conslist(0, []) :- !.
conslist(N, [N|L]) :-
	N1 is N-1,
	conslist(N1, L).

dummy(_,_).

% This is the benchmark:

nreverse([], []).
nreverse([X|L0],L) :-
	nreverse(L0, L1),
	concatenate(L1, [X], L).

concatenate([], L, L).
concatenate([X|L1], L2, [X|L3]) :-
	concatenate(L1, L2, L3).

:- writeln("Call lips/0 to run the standard naive reverse benchmark.").
