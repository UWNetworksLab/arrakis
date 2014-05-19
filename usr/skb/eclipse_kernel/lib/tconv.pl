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
% Version:	$Id: tconv.pl,v 1.2 2008/08/21 18:08:28 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 * IDENTIFICATION:	tconv.pl 
 *
 * DESCRIPTION: 	
 *
 *
 * CONTENTS:
 *
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- system.		% compiler directive to add the SYSTEM flag

:- export
	name/2,
	get_var_info/3,
	term_string/2.

:- skipped term_string/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


name(Const, List) :-
	var(Const),
	!, 
	chk_nmbr_lst(List, name(Const, List)),
	string_list(String, List),
	(	number_string(Const,String) -> true
	;	atom_string(Const,String)
	).
name(Const, List) :-
	number(Const),
	!,
	number_string(Const, String),
	string_list(String, List).
name(Const, List) :-
	atom(Const),
	!,
	atom_string(Const, String),
	string_list(String, List).
name(Const, List) :-
	string(Const),
	!,
	string_list(Const, List).
name(Const, List) :-
	error(5, name(Const, List)).


% The list must be finite and all elements instantiated to valid character codes

chk_nmbr_lst(X, Goal) :-
	var(X), !,
	error(4, Goal).
chk_nmbr_lst([], _) :- !.
chk_nmbr_lst([H|T], Goal) :- !,
	( integer(H) ->
	    ( H < 0 -> error(6, Goal)
	    ; H > 255 -> error(6, Goal)
	    ; chk_nmbr_lst(T, Goal)
	    )
	; var(H) ->
	    error(4, Goal)
	;
	    error(5, Goal)
	).
chk_nmbr_lst(_, Goal) :-
	error(5, Goal).


%
% term_string(?Term, ?String)
%

term_string_body(T, S, Module) :- var(S), !,
	open(string(""), write, Stream),
	writeq_(Stream, T, Module),
	stream_info_(Stream, 0, S),  % = get_stream_info(Stream,name,S)
	close(Stream).
term_string_body(T, S, Module) :- string(S), !,
	( S \== "" ->
	    open(string(S), read, Stream),
	    (
		read_(Stream, T0, Module),
		read_token_(Stream, end_of_file, _, Module)
	    ->
		close(Stream),
		T = T0
	    ;
		close(Stream),
		error(7, term_string(T, S))
	    )
	;
	    error(7, term_string(T, S))
	).
term_string_body(T, S, _Module) :-
	error(5, term_string(T, S)).


%
% get_var_info(?Var, ?InfoName, ?Value)
%

get_var_info(Var, Info, Value) :-
	not(atom(Value); var(Value)),
	!,
	error(5, get_var_info(Var, Info, Value)).
get_var_info(Var, Info, Value) :-
	do_get_var_info(Var, Info, Value).

do_get_var_info(Var, Info, Value) :-
	var(Info),
	var_infos(Info),
	do_get_var_info1(Var, Info, Value).
do_get_var_info(Var, Info, Value) :-
	atom(Info),
	do_get_var_info1(Var, Info, Value).
do_get_var_info(Var, Info, Value) :-
	not(atom(Info); var(Info)),
	error(5, get_var_info(Var, Info, Value)).

do_get_var_info1(Var, name, Value) :-
	!,
	get_var_name(Var, Value).
do_get_var_info1(Var, type, Value) :-
	!,
	get_var_type(Var, Value).
do_get_var_info1(Var, Info, Value) :-
	error(6, get_var_info(Var, Info, Value)).

var_infos(type).
var_infos(name).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
