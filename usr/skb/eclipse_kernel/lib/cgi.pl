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
% Version:	$Id: cgi.pl,v 1.1 2008/06/30 17:43:42 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% $Id: cgi.pl,v 1.1 2008/06/30 17:43:42 jschimpf Exp $
%
% IDENTIFICATION:	cgi.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		Some utilities for writing cgi scripts with ECLiPSe
%

:- module(cgi).
:- export
	posted_params/1,
	get_error_output/1,
	get_param_value/3,
	substitute_placeholders/3.

:- comment(summary, "Some utilities for writing cgi scripts with ECLiPSe").
:- comment(author, "Joachim Schimpf, IC-Parc, Imperial College, London").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:42 $").

:- comment(posted_params/1, [
    template:"posted_params(-NameValuePairs)",
    summary:"Returns the parameters posted to the CGI script",
    fail_if:"There was a problem obtaining the parameters from the environment",
    see_also:[get_param_value/3,substitute_placeholders/3,get_error_output/1],
    desc:html("Returns a list of Name=Value pairs, where Name is an atom
    and Value is a string, representing the information that was posted to
    the CGI script. Both POST and GET methods are supported.
    <P>
    If there is a problem, the predicate fails. In that case, the caller should
    retrieve an error message using get_error_output/1 and present it to the
    user, e.g. by embedding it into the generated html page.
    ")
    ]).

:- comment(get_param_value/3, [
    template:"get_param_value(+NameValuePairs, +Name, ?Value)",
    summary:"Look up the value of a posted parameter",
    see_also:[posted_params/1,substitute_placeholders/3],
    desc:html("Look up the value of a posted parameter. Returns an empty string
    if there is no parameter with the given name. Name must be an atom.")
    ]).

:- comment(substitute_placeholders/3, [
    template:"substitute_placeholders(+PageTemplate, +NameValuePairs, -Page)",
    summary:"Substitute placeholders in a html source with a value string",
    desc:html("Takes a string (usually a html-source) with embedded
    placeholders and replaces the placeholders by their value according
    to the NameValuePairs argument. The syntax for placeholders is their
    name enclosed in ^ (up arrow) characters."),
    see_also:[posted_params/1,get_param_value/3]
    ]).

:- comment(get_error_output/1, [
    template:"get_error_output(-Message)",
    summary:"Retrieve error messages explaining failure of posted_params/1",
    see_also:[posted_params/1]
    ]).


% suppress "compiled" messages
:- set_event_handler(139, true/0).

% redirect error output into a string stream
:- open(string(""), write, error).

get_error_output(Messages) :-
	get_stream_info(error, name, Messages).


posted_params(NameValuePairs) :-
	( getenv('REQUEST_METHOD', Method) ->
	    ( Method == "POST" ->
		( getenv('CONTENT_LENGTH', ContentLengthString) ->
		    ( number_string(ContentLength, ContentLengthString),
			( read_string("", ContentLength, String) ->
			    true
			;
			    writeln(error, "Standard input empty"),
			    fail
			)
		    ;
			writeln(error, "CONTENT_LENGTH does not contain a number"),
			fail
		    )
		;
		    writeln(error, "Environment variable CONTENT_LENGTH not set"),
		    fail
		)
	    ; Method == "GET" ->
		( getenv('QUERY_STRING', String) ->
		    true
		;
		    String=""
		)
	    ;
		printf(error, "Illegal method: %q%n", [Method]),
		fail
	    )
	;
	    writeln(error, "Environment variable REQUEST_METHOD not set"),
	    fail
	),
%	log_request(String),
	split_string(String, "&", "", NameEqValueStrings),
	decode_defs(NameEqValueStrings, NameValuePairs).

    log_request(String) :-
    	get_flag(pid, Pid),
	concat_string(["/tmp/timesheetlog",Pid], Logfile),
	open(Logfile, write, S),
	write(S, String),
	close(S).

    decode_defs([], []).
    decode_defs([NameEqValueString|Ins], Outs) :-
	( split_string(NameEqValueString, "=", "", [NameString,RawValue]) ->
	    atom_string(Name, NameString),
	    string_list(RawValue, RawValueList),
	    dequote(RawValueList, ValueList),
	    string_list(Value, ValueList),
	    Outs = [Name=Value|Outs0]
	;
	    Outs = Outs0
	),
	decode_defs(Ins, Outs0).

    :- mode dequote(+,-).
    dequote([], []).
    dequote([0'+|More], [0' |Cs]) :- !,
	dequote(More, Cs).
    dequote([0'%,H,L|More], [C|Cs]) :-
    	hex(H, HX),
    	hex(L, LX),
	!,
	C is HX*16+LX,
	dequote(More, Cs).
    dequote([C|More], [C|Cs]) :-
	dequote(More, Cs).

    :- mode hex(+,-).
    hex(0'0, 0). hex(0'1, 1). hex(0'2, 2). hex(0'3, 3). hex(0'4, 4).
    hex(0'5, 5). hex(0'6, 6). hex(0'7, 7). hex(0'8, 8). hex(0'9, 9).
    hex(0'A, 10). hex(0'B, 11). hex(0'C, 12). hex(0'D, 13). hex(0'E, 14). hex(0'F, 15).
    hex(0'a, 10). hex(0'b, 11). hex(0'c, 12). hex(0'd, 13). hex(0'e, 14). hex(0'f, 15).


get_param_value([], _, "").
get_param_value([NameEqValue|T], Name, Value) :-
	( NameEqValue = (Name=Value) ->
	    true
	;
	    get_param_value(T, Name, Value)
	).


substitute_placeholders(PageTemplate, NameValuePairs0, Page) :-
	NameValuePairs = [''="^"|NameValuePairs0],
	split_string(PageTemplate, "^", "", Parts),
	(
	    fromto(Parts, [Text, ParName|Parts1], Parts1, [Last]),
	    fromto(ExpParts, [Text, ParValue|ExpParts1], ExpParts1, [Last]),
	    param(NameValuePairs)
	do
	    atom_string(ParNameA, ParName),
	    get_param_value(NameValuePairs, ParNameA, ParValue)
	),
	concat_string(ExpParts, Page).

