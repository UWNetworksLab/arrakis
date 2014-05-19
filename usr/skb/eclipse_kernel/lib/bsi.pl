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
% Version:	$Id: bsi.pl,v 1.2 2008/07/27 12:25:05 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	bsi.pl
 *
 * DESCRIPTION: 	
 *
 *
 * CONTENTS:     
 *
 */
:- module(bsi).
:- export
%	syntax_option(no_other_quote),	% no longer supported by the lexer
	syntax_option(no_array_subscripts),
	chtab(0'$, symbol),
	op(1100, xfy, '|'),
	op(1000, xfy, '&'),
	op(500, xfy, (\/)),
	op(500, xfy, (/\)),
	op(0, fy, (nospy)).

:- system.		% compiler directive to add the SYSTEM flag

:- export
	at/2,
	concat/3,
	consult/1,
	device/2,
	display/1,
	e/1,
	open/3,
	pi/1,
	prolog_flag/3,
	reconsult/1,
	seek/2,
	stream/3,
	string_list/2,
	strlength/2.

:- import
	error_handler/2,
	eval/3,
	import_body/2,
	set_default_error_handler/2
   from sepia_kernel.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% to define current_input and current_output

?-
	set_stream(current_input,input),
	set_stream(current_output,output).

% consult and reconsult are not exactly like compile

?-
	tool(consult/1,consult_body/2),
	tool(reconsult/1,reconsult_body/2).

consult_body(X,M) :-
	compile(X,M).

reconsult_body(X,M) :-
	compile(X,M).

'|'(A -> B, _) :-
	call(A), !, call(B).
'|'(_ -> _, C) :-
	!,
	call(C).
'|'(A, _) :-
	call(A).
'|'(_, B) :-
	call(B).

'&'(A, B) :-
	call(A),
	call(B).

strlength(S,L) :-
	string_length(S,L).

concat(X,Y,Z) :-
	var_or_string(X),
	var_or_string(Y),
	var_or_string(Z),
	!,
	concat_chk(X,Y,Z).
concat(X, Y, Z) :-
	error(5, concat(X, Y, Z)).

concat_chk(X,Y,Z) :-
	var(Z),
	!,
	((var(X) ; var(Y))  ->
		error(4, concat(X, Y, Z))
	;
		concat_strings(X, Y, Z)
	).
concat_chk(X,Y,Z) :-
	eclipse_language:string_list(Z, ZL),
	append(XL, YL, ZL),
	eclipse_language:string_list(X, XL),
	eclipse_language:string_list(Y, YL).

var_or_string(X) :-
	var(X),
	!.
var_or_string(X) :-
	string(X).


% in bsi, the result is a list of characters

string_list(S,L) :-
	nonground(S),
	nonground(L),
	!,
	error(4,string_list(S,L)).
string_list(S,L) :-
	nonground(S),
	!,
	convert_to_char(LI,L),
	eclipse_language:string_list(S,LI).
string_list(S,L) :-
	eclipse_language:string_list(S,LI),
	convert_to_char(LI,L).

convert_to_char([],[]) :-
	!.
convert_to_char([HI|TI],[H|T]) :-
	!,
	char_int(H,HI),
	convert_to_char(TI,T).
convert_to_char(S,L) :-
	error(5,string_list(S,L)).

device(_,delete_file(F)) :-
	!,
	delete(F).
device(S,P) :-
	P =.. [F|L],
	append(L,[S],NL),
	NP =.. [F|NL],
	NP.

% display should always output to the terminal in bsi

display(X) :-
	get_stream(output,O),
	set_stream(output, stdout),
	eclipse_language:display(X),
	set_stream(output,O).

% BSI uses a descriptor which is implementation dependent.
% Here, we consider a "sepia like" syntax, but open may be 
% redefined as : open(file(F),M,S) :- open(F,M,S)

open(F,readwrite,S) :-
	!,
	eclipse_language:open(F,update,S).
open(F,M,S) :-
	eclipse_language:open(F,M,S).

% the following predicates have the stream as first argument
% in bsi, and as last argument in sepia

at(S,Pos) :-
	Pos \== end_of_file,
	!,
	eclipse_language:at(S, Pos).
at(S,end_of_file) :-                     % according to bsi, always fails
	fail.

seek(S,Pos) :-
	eclipse_language:seek(S, Pos).

% prolog_flag is not fully implemented here

prolog_flag(error_break,_,_) :-
	!.
prolog_flag(error_number,_,_) :-
	!.
prolog_flag(current_input,Old,New) :-
	New == user,
	!,
	prolog_flag(current_input, Old, stdin).
prolog_flag(current_input,Old,New) :-
	get_stream(current_input,Old),
	set_stream(current_input,New),
	set_stream(input,New).
prolog_flag(current_output,Old,New) :-
	New == user,
	!,
	prolog_flag(current_output, Old, stdout).
prolog_flag(current_output,Old,New) :-
	get_stream(current_output,Old),
	set_stream(current_output,New),
	set_stream(output,New).


% the descriptor is supposed to be the name of the file

stream(Stream, Des, Mode) :-
	current_stream(Des, Smode, Stream),
	(	Smode = update
	->	Mode = readwrite
	;	Mode = Smode
	).


% arithmetic: all arithmetic builtins must evaluate their arguments.
% in sepia only is/2 and the comparisons do it, else +/3 etc would
% have to be tools ...
% This leads to problems here, since the handler is called without
% a module argument. We may therefore be unable to call a
% user-defined arithmetic precidate (if it's not global).

bsi_eval_handler(_, Goal) :-
	arg(1, Goal, X),		% Goals has arity 3
	eval(X, X1, bsi),
	(number(X1) -> true ; var(X1) -> true ; error(5, Goal)),
	arg(2, Goal, Y),
	eval(Y, Y1, bsi),
	(number(Y1) -> true ; var(Y1) -> true ; error(5, Goal)),
	functor(Goal, F, A),
	functor(NewGoal, F, A),
	arg(1, NewGoal, X1),
	arg(2, NewGoal, Y1),
	(A == 3 ->
	    arg(3, Goal, Res),
	    arg(3, NewGoal, Res)
	;
	    true
	),
	call(NewGoal).			% we don't have the caller module!

pi(X) :- X is pi.
e(X) :- X is e.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

?-
	skipped	at/2,
		bsi_eval_handler/2,
		concat/3,
	        consult/1,
		device/2,
		display/1,
		open/3,
		prolog_flag/3,
		reconsult/1,
	        seek/2,
		stream/3,
		string_list/2,
		strlength/2.
?-
	untraceable
		bsi_eval_handler/2.

:-
	set_default_error_handler(198, fail/0),		% fail when past eof
	reset_error_handler(198),
	set_default_error_handler(24, bsi_eval_handler/2),
	reset_error_handler(24).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
