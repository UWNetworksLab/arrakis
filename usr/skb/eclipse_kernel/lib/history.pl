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
% Version:	$Id: history.pl,v 1.1 2008/06/30 17:43:46 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	history.pl
 * DESCRIPTION: 	History package for sepia, library file.
 * CONTENTS:     
 * REVISION HISTORY:
 * AUTHOR	VERSION	 DATE	REASON
 * Micha Meier		3.11.89
 */

:- module(history).

% Make sure that the important operators are ok
:-	op(1100, xfy, ;),
	op(1050, xfy, ->).

:-
	make_local_array(dbg),
	get_flag(debug_compile, Dbg),
	get_flag(variable_names, Var),
	setval(dbg, flags(Dbg, Var)),
	nodbgcomp.

:- export h/0.

:- make_local_array(history_command), setval(history_command, 1).
:- setval(history, 20).
:- dynamic history_command/3.

check_goal(_, goal(Term, List, Goal, VarList), M) :-
    (integer(Term) ->
	(Term < 0 ->
	    getval(history_command, Comm),
	    Number is Comm + Term
	;
	    Number = Term
	),
	(history_command(Number, Goal, VarList) ->
	    write(toplevel_output, Goal),
	    writeln(toplevel_output, '.')
	;
	    Goal = fail,
	    VarList = []
	)
    ;
	(Term == h ->
	    true
	;
	history_command(_, OldTerm, _),
	variant(OldTerm, Term) ->
	    true
	;
	    getval(history_command, Comm),
	    getval(history, H),
	    OldComm is Comm - H,
	    (history_command(Oldest, _, _) ->
		(Oldest =< OldComm ->
		    remove_all(Oldest, OldComm)
		;
		    true
		)
	    ;
		true
	    ),
	    incval(history_command),
	    assert(history_command(Comm, Term, List))
	),
	Goal = Term,
	VarList = List
    ).

history_prompt(_, M) :-
    put(toplevel_output, 0'[),
    write(toplevel_output, M),
    put(toplevel_output, 0' ),
    getval(history_command, Comm),
    write(toplevel_output, Comm),
    write(toplevel_output, ']: '),
    flush(toplevel_output).

h :-
    history_command(Comm, Goal, _),
    write(Comm),
    put(0'	),
    writeln(Goal),
    fail.
h.

remove_all(From, To) :-
	From > To,
	!.
remove_all(From, To) :-
	(retract(history_command(From, _, _)) -> true; true),
	Next is From + 1,
	remove_all(Next, To).


:-	set_error_handler(153, history_prompt/2),
	set_error_handler(154, check_goal/3).

:- skipped((h)/0),
   untraceable((h)/0).

:-
	getval(dbg, flags(Dbg, Var)),
	set_flag(debug_compile, Dbg),
	set_flag(variable_names, Var),
	erase_array(dbg).
