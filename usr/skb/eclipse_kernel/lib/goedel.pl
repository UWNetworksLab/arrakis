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
% Version:	$Id: goedel.pl,v 1.1 2008/06/30 17:43:46 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

%
% Sepia modifications for SICStus Goedel
%

:- module(user).

:- local
	retractall/1.

:- ensure_loaded(library(lists)).
:- ensure_loaded(library(sorts)).
:- reexport sicstus.


:- set_flag(toplevel_module, user).

:- local
	prolog_flag/3.

:- export
	retractall/1.

:- import
	retract_all_body/2
    from sepia_kernel.

prolog_flag(compiling, _, _).
prolog_flag(A, B, C) :-
	call_explicit(prolog_flag(A, B, C), quintus).

% Sepia fixes
:- tool(retractall/1, retract_all_body/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Goedel compatibility

:- local
	plus/3,
	times/3,
	(mod)/3,
	format/3.

:- [init] -> true.	% because of ptags
:- findall(F, ( (system_file(F); (system_file(sys_modules) -> true; F = sys_modules)),
		     system_directory(D),
	             join_string(D, F, DF),
	             compile(DF)
		   ), _).


is_runtime_system :- fail.		% temporary

display_usage(C) :-
   usage(C, Lists),
   lists_to_strings(Lists, Strings),
   printf('%-25.25s %-5.5s  - %s%n', Strings).

show_usage(Abbrev) :- 
   usage(Abbrev, Lists),
   lists_to_strings(Lists, [U|_]),
   printf(user_error, 'Syntax error, use %s%n%b', U).

lists_to_strings([], []).
lists_to_strings([H|T], [S|R]) :-
	string_list(S, H),
	lists_to_strings(T, R).

my_load(X) :-
	compile(X).

file_exist(File, Suffix) :-
	concat_atoms(File, Suffix, Name),
	exists(Name).

format(S, L, X) :-
	ttyflush,
	call_explicit(format(S, L, X), quintus),
	flush(S).

/*------------------------------------------------------------------------------
 * evaluate_delay_aux
 */

evaluate_delay_aux(Delay, X):-
	goedel_freeze_aux(Delay, [], Var_list, NewDelay),
	( var(NewDelay)
	  -> X = 1
	  ;  prolog:'$disjunctive_geler'(Var_list, 
				call(evaluate_delay_aux(NewDelay, X), user) )
	).

translate_stream('IO.InputStreamDescriptor.F1'(List), Stream) :-
   List = [Stream].
translate_stream('IO.OutputStreamDescriptor.F1'(List), Stream) :-
   List = [Stream].
translate_stream('IO.StdIn.C0', user_input).
translate_stream('IO.StdOut.C0', user_output).
translate_stream('IO.StdErr.C0', user_error).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Simulate non-documented SICStus predicates

:- module(prolog).
:- tool('$disjunctive_geler'/2, disjunctive_geler_body/3).
:- import
	make_suspension/3
    from sepia_kernel.

disjunctive_geler_body(List, Goal, M) :-
	check_vars(List),
	!,
	make_suspension(Goal, S, M),
	insert_suspension(List, S, 1, top).
disjunctive_geler_body(_, Goal, M) :-
	call(Goal, M).

check_vars([]).
check_vars([V|L]) :-
	var(V),
	check_vars(L).

:- begin_module('IO').

translate_stream('IO.InputStreamDescriptor.F1'(List), Stream) :-
   List = [Stream].
translate_stream('IO.OutputStreamDescriptor.F1'(List), Stream) :-
   List = [Stream].
translate_stream('IO.StdIn.C0', user_input).
translate_stream('IO.StdOut.C0', user_output).
translate_stream('IO.StdErr.C0', user_error).

