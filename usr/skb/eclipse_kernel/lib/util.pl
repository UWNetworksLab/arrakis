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
% Version:	$Id: util.pl,v 1.1 2008/06/30 17:43:50 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 * IDENTIFICATION:	util.pl 
 *
 * DESCRIPTION: 	Various utility predicates, for user convenience.
 *
 * CONTENTS:
 *
 */

:- module(util).

:- comment(summary, "Various utility predicates for program development").
:- comment(author, "Various, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:50 $").
:- comment(add_path/1, [template:"add_path(+Directory)",
    summary:"The directory will be added at the beginning of the library path."
    ]).
:- comment(add_suffix/1, [template:"add_suffix(+Suffix)",
    summary:"The Suffix string will be added at the beginning of the prolog_suffix list."
    ]).
:- comment(between/3, [template:"between(+From, +To, ?I)",
    summary:"Generates integers between From and To",
    desc:html("Succeeds if From and To are integers and I unifies with a
	  number between the two.  On backtracking it generates all
	  values for I starting from From onwards.")]).
:- comment(compiled/0, [template:"compiled",
    summary:"List all currently compiled files and indicate if they have been modified since they were compiled."
    ]).
:- comment(list_error/3, [template:"list_error(+String, -ErrNo, -ErrMsg)",
    summary:"Find the event number whose message contains the specified substring"
    ]).
:- comment(read_line/2, [template:"read_line(+Stream, -String)",
    summary:"Defined as read_string(Stream, end_of_line, _Length, String)"
    ]).
:- comment(read_line/1, [template:"read_line(-String)",
    summary:"Defined as read_string(input, end_of_line, _Length, String)"
    ]).
:- comment(stream/1, [template:"stream(+Stream)",
    summary:"List all information about the specified I/O stream"
    ]).
:- comment(streams/0, [template:"streams",
    summary:"List information about the currently opened I/O streams"
    ]).
:- comment(time/1, [template:"time(Goal)",
    summary:"Call the goal Goal, measure its runtime (cputime) and print the result after success or failure"
    ]).
:- comment(edit/1, [template:"edit(+PredSpec)",
    summary:"Invoke an editor on the source of the specified predicate (UNIX only)"
    ]).
:- comment(file_info/1, [template:"file_info(+File)",
    summary:"List all information about the specified File"
    ]).
:- comment(interface/1, [template:"interface(+Module)",
    summary:"List the module interface of the specified Module"
    ]).


:- export
	add_path/1,
	add_suffix/1,
	between/3,
	c_compile_and_load/1,
	compiled/0,
	compile_selection/0,
	edit/1,
	file_info/1,
	list_error/3,
	interface/1,
	stream/1,
	streams/0,
	read_line/1,
	read_line/2,
	time/1.


% add_path(+Path) - prepend a directory to the library path

add_path(Path) :-
	get_flag(library_path, X),
	set_flag(library_path, [Path|X]).

add_suffix(Suffix) :-
	get_flag(prolog_suffix, X),
	set_flag(prolog_suffix, [Suffix|X]).


% streams - print a table of currently opened streams

streams :-
	current_stream(S),
	get_stream_info(S, device, D),
	get_stream_info(S, mode, M),
	get_stream_info(S, name, N),
	printf("%3d   %6s   %6s   %q ( ", [S,D,M,N]),
	(
	    current_atom(Alias),
	    current_stream(Alias),
	    get_stream(Alias, S),
	    printf("%s ", [Alias]),
	    fail
	;
	    true
	),
	writeln(')'),
	fail.
streams.

stream(Stream) :-
	get_stream_info(Stream, _, _),	% so that it fails if not open
	!,
	atom_string('%-20s%w%n', Format),	% to avoid problems in Q mode
	(   get_stream_info(Stream, F, V),
	    printf(Format, [F, V]),
	    fail
	;
	    current_atom(Alias),
	    current_stream(Alias),
	    get_stream(Alias, Stream),
	    printf(Format, [alias, Alias]),
	    fail
	;
	    true
	).

file_info(File) :-
	atom_string('%-20s%w%n', Format),	% to avoid problems in Q mode
	(   get_file_info(File, F, V),
	    printf(Format, [F, V]),
	    fail
	;
	    true
	).

% read_line([+Stream, ] ?String) - read a line of input into String

read_line(Stream, String) :-
	read_string(Stream, end_of_line, _, String).

read_line(String) :-
	read_string(input, end_of_line, _, String).

between(From, To, I) :-
	between(From, To, 1, I).


% time(+Goal) - like call(Goal), but print cputime used

:- tool(time/1, time_body/2).

time_body(Goal, Module) :-
	cputime(T0),
	(
	    call(Goal)@Module,
	    true
	->
	    T is cputime - T0,
	    write('\nSuccess, time = '),
	    writeln(T)
	;
	    T is cputime - T0,
	    write('\nFailure, time = '),
	    writeln(T),
	    fail
	).


% print a list of compiled files and if they were modified since

compiled :-
	current_compiled_file(File, Time, _),
	write(File),
	(get_file_info(File, mtime) =\= Time ->
		writeln(" (modified)")
	;
		nl
	),
	fail.
compiled.

% List all the errors whose message contains a specified text

list_error(String, N, Message) :-
	current_error(N),
	error_id(N, Message),
	substring(Message, String, _).


% Compile selected text (OpenWindow)

:- tool(compile_selection/0, compile_selection/1).

compile_selection(Module) :-
    exec(xv_get_sel, [null, S], Pid),
    compile_stream(S)@Module,
    wait(Pid, _).


% invoke an editor on the source of a predicate

:- tool(edit/1, edit/2).

edit(Pred0, Module0) :-
        ( get_flag(Pred0, tool, on)@Module0 ->
		tool_body(Pred0, Pred, Module)	% edit the tool body instead
	;
		Pred = Pred0,
		Module = Module0
	),
        get_flag(Pred, source_file, File)@Module,
        get_flag(Pred, source_line, Line)@Module,
	( getenv('EDITOR', Editor) -> true ; Editor = "vi"),
        concat_string([Editor, " +", Line, " ", File], Cmd),
	get_file_info(File, mtime, TimeBefore),
        sh(Cmd),
	( get_file_info(File, mtime) =\= TimeBefore ->
                compile(File, Module)		% recompile if changed
        ;
                true
        ).


% Invoke the proper C compiler on File.c and load the result dynamically

c_compile_and_load(File) :-
        get_flag(installation_directory, Inst),
        get_flag(hostarch, Arch),
        get_flag(object_suffix, O),
        concat_string([Inst,"/lib/",Arch,"/Makefile.external"], Makefile),
        concat_string([File,.,O], Ofile),
        concat_string(["sh -c \"ECLIPSEDIR=",Inst,";export ECLIPSEDIR;",
                       "make -f ",Makefile," ",Ofile,"\""], Make),
	writeln(Make),
        exec(Make, []),
	load(Ofile).



% print a module's interface

interface(Module) :-
	write(:- module(Module)),
	get_module_info(Module, locked, Locked),
	( Locked == on -> writeln(".\t% (locked)") ; writeln(.) ),
	get_module_info(Module, interface, List),
	(
	    member(Directive, List), 
	    write(:- Directive), write(.), nl,
	    fail
	;
	    true
	).

