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
% Version:	$Id: mixtus.pl,v 1.1 2008/06/30 17:43:47 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	mixtus.pl
 *
 * DESCRIPTION: 	Package that loads Mixtus to Sepia.
 *
 *
 * CONTENTS:     
 *
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(mixtus, [], quintus).
:- system.		% compiler directive to add the SYSTEM flag
:- make_local_array(flag(2)),
	get_flag(debug_compile, DC),
	setval(flag(0), DC),
	get_flag(variable_names, VN),
	setval(flag(1), VN),
	nodbgcomp.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- local is_dynamic/1.

tracing(X) :-
	var(X),
	!,
	error(4, tracing(X)).
tracing(on) :-
	assert(tracing).
tracing(off) :-
	retract_all(tracing).

frozen(Var, Goals) :-
	delayed_goals(Var, List),
	list_to_comma(List, Goals).

list_to_comma([], true) :- !.
list_to_comma([G], G) :- !.
list_to_comma([H|T], (H,Rest)) :-
	list_to_comma(T, Rest).
	

dif(A, B) :-
	A ~= B.

wait_body(X, _) :-
	var(X),
	!,
	error(4, wait(X)).
wait_body((P, Q), M) :-
	!,
	wait_body(P, M),
	wait_body(Q, M).
wait_body(F/A, M) :-
	!,
	functor(T, F, A),
	arg(1, T, Var),
	compile_term((delay(T) if var(Var))@M.
wait_body(X, _) :-
	error(5, wait(X)).

% call_residue/2 is not quite ok, after executing the call the
% suspended goals should be removed from the suspending variables
call_residue_body(Goal, Delayed, Module) :-
	eclipse_language:call(Goal, Module),
	delayed_goals(Delayed).

:- tool((wait)/1, wait_body/2),
   tool(call_residue/2, call_residue_body/3).

?- skipped
	dif/2,
	frozen/2,
	(wait)/1.

:- unskipped
	call_residue/2,
	call_residue_body/3.

:-
	op(1150, fx, wait).

p(X) :- compile(X).	% To prevent ptags and others to see the .sd file

:- p('/home/lp/sepia/workdir/sepia/mixtus/mixtus/mixtus_load.sd').

:- export
	pconsult/1,
	pe/1,
	pe/2,
	set/1,
	set/2,
	settings/0,
	tracing/1,
	unset/1.


:-
	getval(flag(0), DC),
	set_flag(debug_compile, DC),
	getval(flag(1), VN),
	set_flag(variable_names, VN).
