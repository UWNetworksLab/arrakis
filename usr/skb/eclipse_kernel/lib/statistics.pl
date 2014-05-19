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
% Version:	$Id: statistics.pl,v 1.1 2008/06/30 17:43:49 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	statistics.pl
 * DESCRIPTION: 	
			Some predicates for the profiling package.
 * CONTENTS:     
			print_modes/0
 */

:- module(statistics).

:- export 
	print_modes/0.

:- import
	current_predicate_body/2,
	get_mode_/3
    from sepia_kernel.

print_modes :-
    current_module(M1),
    not(is_locked(M1)),
    current_predicate_body(P, M1),
    get_mode_(P, Mode, M1),
    check_multiple_modules(M1, Mode).

check_multiple_modules(M1, _) :-
    current_module(M2),
    not(is_locked(M2)),
    M1 \== M2,
    current_predicate_body(Q, M2),
    get_mode_(Q, _, M2),
    !,
    (   current_module(M),
	not(is_locked(M)),
	print_multiple_modes(M),
	fail
    ;
	true
    ).
check_multiple_modules(M, Mode) :-
    print_single_modes(M, Mode).
    
print_multiple_modes(M) :-
    once((current_predicate_body(P, M),
	get_mode_(P, Mode, M))),
    printf(':- call((%n    ', []),
    print_modes(M, Mode),
    printf('%n    ), %a).%n%b', [M]).

print_single_modes(M, Mode) :-
    printf(':- ', []),
    print_modes(M, Mode),
    printf('.%n%b', []).

print_modes(M, Mode1) :-
    printf('mode%n%t%w', [Mode1]),
    current_predicate_body(P, M),
    get_mode_(P, Mode, M),
    Mode \== Mode1,
    printf(',%n%t%w', [Mode]),
    fail.
print_modes(_, _).

:- skipped
	print_modes/0.
