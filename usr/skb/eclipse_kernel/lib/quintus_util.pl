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
% Copyright (C) 1990-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: quintus_util.pl,v 1.1 2008/06/30 17:43:49 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 * IDENTIFICATION:	quintus_util.pl 
 *
 * DESCRIPTION: 	Utility predicates, for user convenience.
 *
 * CONTENTS:
 *
 */


:- module(quintus_util).

:- export q_prompt/2, redef_handler/2, end_compile_handler/2.

%
% predicate to obtain a Quintus-like prompt
%

q_prompt(_, Module) :-
    get_flag(debugging, Dbg),
    debug_mode(Dbg, Debug),
    (Debug == nodebug ->
        (Module == eclipse ->
            true
        ;
            printf(toplevel_output, "[%s]\n", Module)
        )
    ;

        put(toplevel_output, 0'[),
        (Module == eclipse ->
            true
        ;
            printf(toplevel_output, "%s ", Module)
        ),
        printf(toplevel_output, "%s]\n", Debug)
    ),
    write(toplevel_output, '| ?- '),
    flush(toplevel_output).

debug_mode(leap, debug).
debug_mode(nodebug, nodebug).
debug_mode(creep, trace).



% A flag to suppress the warnings
:- setval(pflag, 0).

redef_handler(_, (Proc, OldFile, NewFile)) :-
    (getval(pflag, 1) ->
        true
    ;
        printf("Procedure %w is being redefined in a different file\n",
            Proc),
        printf("    Previous file: %s\n    New file:      %s\n",
            [OldFile, NewFile]),
        printf("Do you want to redefine it? (y, n or p) %b", []),
        tyi(X),
        (X == 0'y ->
            writeln(yes)
        ;
        X == 0'p ->
            writeln('suppress warnings'),
            setval(pflag, 1)
        ;
            writeln(no),
            fail
        )
    ).

% At the file end reset the flag
end_compile_handler(A, B) :-
    setval(pflag, 0),
    error(default(A), B).

