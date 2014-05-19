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
% Version:	$Id: fromonto.pl,v 1.1 2008/06/30 17:43:46 jschimpf Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	fromonto.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		Goal from_stream Stream
%			Goal onto_stream Stream
%			Goal from_string String
%			Goal onto_string String
%			Goal from_file File
%			Goal onto_file File
%
% DESCRIPTION:	Redirect input and output in a convenient way.
%		The idea is stolen from Richard O'Keefe.
%		Alternatives of the goal are discarded.


:- module(fromonto).

:- comment(summary, "Redirect input and output streams in a convenient way").
:- comment(author, "Joachim Schimpf, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:46 $").
:- comment(eg, "
    [eclipse 1]: write(hello) onto_file scratch.
    yes.
    [eclipse 2]: read(X) from_file scratch.
    X = hello
    yes.
    [eclipse 3]: read(X) from_string \"s(a,2,[3])\".
    X = s(a, 2, [3])
    yes.
    [eclipse 4]: (write(hello), put(0' ), write(world)) onto_string S.
    S = \"hello world\"
    yes.
    ").

:- comment((from_stream)/2, [
    summary:"Redirect Goal's standard input stream to Stream",
    template:"+Goal from_stream ++Stream",
    desc:html("Equivalent to once(Goal), with input redirected to Stream.")
    ]).
:- comment((onto_stream)/2, [
    summary:"Redirect Goal's standard output stream to Stream",
    template:"+Goal onto_stream ++Stream",
    desc:html("Equivalent to once(Goal), with output redirected to Stream.")
    ]).
:- comment((from_string)/2, [
    summary:"Redirect Goal's standard input stream to String",
    template:"+Goal from_string ++String",
    desc:html("Equivalent to once(Goal), with input redirected to String.")
    ]).
:- comment((onto_string)/2, [
    summary:"Redirect Goal's standard output stream to String",
    template:"+Goal onto_string ?String",
    desc:html("Equivalent to once(Goal), with output redirected to String.")
    ]).
:- comment((from_file)/2, [
    summary:"Redirect Goal's standard input stream to File",
    template:"+Goal from_file ++File",
    desc:html("Equivalent to once(Goal), with input redirected to File.")
    ]).
:- comment((onto_file)/2, [
    summary:"Redirect Goal's standard output stream to File",
    template:"+Goal onto_file ++File",
    desc:html("Equivalent to once(Goal), with output redirected to File.")
    ]).

:- export
	(from_stream)/2,
	(onto_stream)/2,
	(from_string)/2,
	(onto_string)/2,
	(from_file)/2,
	(onto_file)/2.

:- export op(800, yfx, [from_string, onto_string,
		from_stream, onto_stream,
		from_file, onto_file]).


:- tool((from_string)/2, from_string_body/3).
:- tool((from_file)/2, from_file_body/3).
:- tool((from_stream)/2, from_stream_body/3).
:- tool((onto_string)/2, onto_string_body/3).
:- tool((onto_file)/2, onto_file_body/3).
:- tool((onto_stream)/2, onto_stream_body/3).


from_stream_body(Goal, Stream, Module) :-
	get_stream(input, In),
	set_stream(input, Stream),
	(call(Goal)@Module -> Result = true ; Result = false),
	set_stream(input, In),
	Result = true.

from_file_body(Goal, File, Module) :-
	open(File, read, Stream),
	get_stream(input, In),
	set_stream(input, Stream),
	(call(Goal)@Module -> Result = true ; Result = false),
	set_stream(input, In),
	close(Stream),
	Result = true.

from_string_body(Goal, String, Module) :-
	open(String, string, Stream),
	get_stream(input, In),
	set_stream(input, Stream),
	(call(Goal)@Module -> Result = true ; Result = false),
	set_stream(input, In),
	close(Stream),
	Result = true.


onto_stream_body(Goal, Stream, Module) :-
	get_stream(output, Out),
	set_stream(output, Stream),
	(call(Goal)@Module -> Result = true ; Result = false),
	set_stream(output, Out),
	Result = true.

onto_file_body(Goal, File, Module) :-
	open(File, write, Stream),
	get_stream(output, Out),
	set_stream(output, Stream),
	(call(Goal)@Module -> Result = true ; Result = false),
	set_stream(output, Out),
	close(Stream),
	Result = true.

onto_string_body(Goal, String, Module) :-
	open("", string, Stream),
	get_stream(output, Out),
	set_stream(output, Stream),
	(call(Goal)@Module ->
	    set_stream(output, Out),
	    get_stream_info(Stream, name, String),
	    close(Stream)
	;
	    set_stream(output, Out),
	    close(Stream),
	    fail
	).

