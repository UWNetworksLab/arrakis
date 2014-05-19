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
% Copyright (C) 1999-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London and ICL
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: t_all.pl,v 1.2 2008/07/24 16:26:31 jschimpf Exp $
% ----------------------------------------------------------------------

%-----------------------------------------------------------------------
% ECLiPSe II generic test program tool
%
% Author:    Owen Evans, IC-Parc
% OVDE IC-parc	26 Feb 1999/03
% Modified Kish Shen 15 July 1999
%          Kish Shen 21 March 20001:
%              Output to error stream sent to output file: tests may
%              need to provoke error messages as part of the test
%-----------------------------------------------------------------------

:- module(t_all).

:-
	import
	       set_default_error_handler/2,
	       trimcore/0
	from sepia_kernel.

:- export
	test1/2,
	similar_files/2,
	cputime/1,
	statistics/2.


%-------------------------------------------------------------------------

:- lib(time_log).

:- local variable(error_file).


% Dummy definitions for cputime/1 and statistics/2
% to achieve reproducable test outputs

cputime(1).
	
statistics(runtime, [0, 0]).


:-      tool(test1/2, test1/3).


my_halt :-
%	writeln('Test complete'),
	exit(1).

user_start :-
	writeln(test_log_output, '***** PROBLEM : trying to restart'),
	write_error,
	nl(test_log_output),
	my_halt.

ab_big(Int) :-
	write(test_log_output, '***** BIG PROBLEM - interrupt '),
	writeln(test_log_output, Int),
	write_error,
	nl(test_log_output),
	my_halt.

ab_loop :-
	writeln(test_log_output, '***** PROBLEM : too long (loop ?)'),
	write_error,
	nl(test_log_output),
	my_halt.

ab_pgm(X) :-
	write(test_log_output, '***** PROBLEM : program aborts : exit_block('),
	write(test_log_output, X),
	writeln(test_log_output, ')'),
	write_error,
	nl(test_log_output),
	my_halt.

ab_test(X) :-
	write(test_log_output, '***** PROBLEM : test aborts : exit_block('),
	write(test_log_output, X),
	writeln(test_log_output, ')'),
	write_error,
	nl(test_log_output),
	my_halt.

ab_query_fail :-
	writeln(test_log_output, '***** PROBLEM : a query fails'),
	write_error,
	nl(test_log_output),
	my_halt.

end_test :-
	writeln(test_log_output, '***** PROBLEM IN THE RECOVERY PROCEDURE'),
	nl(test_log_output),
	my_halt.

write_error :-
	getval(error_file, FileErr),
	string(FileErr),
	exists(FileErr),
	!,
	file_print(test_log_output, FileErr).
write_error.

?-
	set_error_handler(76, true/0),		% turn off new warning messages
	set_error_handler(77, true/0),
	set_default_error_handler(75,true/0),	% preliminary
	set_default_error_handler(133,true/0),
	set_default_error_handler(139,true/0),
%	set_default_error_handler(143,ab_query_fail/0),
	set_default_error_handler(151, user_start/0),
	reset_error_handlers.

:- export ab_big/1, ab_loop/0.

:-  ( current_interrupt(_, xcpu) ->
    	set_interrupt_handler(xcpu, ab_loop/0)
    ;
	true
    ).



test1(File, Header, M) :-
	block(test2(File, Header, consult, top, M),_,end_test).

test2(File, Header, CompileGoal, RunGoal, M) :-

	set_stream(test_log_output, output),
	printf(test_log_output, "%w%n%b", [Header]),

	concat_string([File,'.rlt'], FileOut),
	concat_string([File,'.err'], FileErr),
	setval(error_file, FileErr),
	concat_string([File,'.ref'], Reference),
	del_if_exists(FileErr),
	del_if_exists(FileOut),
	open(FileErr, write, ErrorStream, [end_of_line(lf)]),
	open(FileOut, write, OutputStream, [end_of_line(lf)]),
	set_stream(error, OutputStream),  % normal error messages to output

	set_flag(variable_names, off),		% prepare for compile

	block(
	    ( call(CompileGoal)@M ->		% compile the test
		CompilationOk = true
	    ;
		writeln(test_log_output, '***** PROBLEM: COMPILATION FAILS')
	    ),
	    ExitTag,
	    (
		write(test_log_output, '***** PROBLEM: COMPILATION ABORTS WITH '),
		writeln(test_log_output, exit_block(ExitTag))
	    )
	),

	( CompilationOk == true ->

	    set_stream(output, OutputStream),	% prepare for run
	    set_stream(error, OutputStream),
	    set_stream(warning_output, OutputStream),
	    set_stream(log_output, OutputStream),
	    set_stream(stdout, OutputStream),
	    set_flag(strip_variables, on),
	    set_flag(variable_names, off),
	    set_flag(print_depth, 100000),
	    cputime(StartTime),
	    get_priority(OldPrio),

	    block(
		(
		    call(RunGoal)@M,		% run test goal
		    garbage_collect,
		    trimcore,
		    check_state(OldPrio)
		->
		    set_flag(prefer_rationals, off),	% set by some tests
		    cputime(EndTime),
		    Time is fix((EndTime - StartTime) * 10)/10,
		    RunOk = true
		;
		    writeln(test_log_output, '***** PROBLEM: TEST FAILS')
		),
		ExitTag,
		(
		    write(test_log_output, '***** PROBLEM: TEST ABORTS WITH '),
		    writeln(test_log_output, exit_block(ExitTag))
		)
	    ),

	    close(stdout),			% undo output redirections
	    close(log_output),
	    close(warning_output),
	    close(output)
	;
	    true
	),

	( RunOk == true ->			% if success, verify output
	    ( similar_files(Reference,FileOut) ->
		printf(test_log_output,"Ok, time = %.2f%n",Time),
		log_time_local(File, Time)
	    ;
		writeln(test_log_output, '***** PROBLEM - INCORRECT OUTPUT')
	    )
	;
	    true
	),
	flush(ErrorStream),
	( at(ErrorStream, 0) ->			% echo errors if any
	    true
	;
	    writeln(test_log_output, '***** PROBLEM - ERROR OUTPUT:'),
	    file_print(test_log_output, FileErr)
	),
	close(ErrorStream),
	flush(test_log_output),
	close(test_log_output).


check_state(OldPrio) :-
	( get_priority(OldPrio) ->
	    true
	;
	    exit_block(test_terminated_with_modified_priority)
	),
	( events_defer ->
	    events_nodefer
	;
	    events_nodefer,
	    exit_block(test_terminated_with_events_deferred)
	).


%----------------------------------------------------------------------

read_argument(X) :-
	argc(N),
	find_t(1, N, X).

find_t(N, N, true) :- !.
find_t(I, N, X) :-
	argv(I, "+t"),
	!,
	I1 is I + 1,
	(I1 < N ->
	    argv(I1, A),
	    open(string(A), read, S), 
	    read(S, X),
	    close(S)
	;
	    X = true
	).
find_t(I, N, X) :-
	I1 is I + 1,
	find_t(I1, N, X).

file_print(Stream, File) :-
	open(File,read,S), 
	(read_string(S, "", 512, StartOfFile) -> 
	    write(Stream, StartOfFile),
	    ( string_length(StartOfFile) < 512 ->
		true
	    ;
		writeln(Stream, " ... (TRUNCATED)")
	    )
	; true
        ),
	close(S).

write_file(X):-
	write_to_eof(X).	
write_file(_).

write_to_eof(X) :-
	get_char(X, C),			% fails on eof
	put_char(test_log_output, C),
	write_to_eof(X).

del_if_exists(File) :-
	(exists(File) ->
	    delete(File)
	;
	    true
	).

reset_int_handler :-
	set_interrupt_handler(int, abort/0).


%----------------------------------------------------------------------
% Auxiliary: check whether two files are similar.
% If not, print first difference and fail
%----------------------------------------------------------------------

similar_files(F1,F2):-
	open(F1,read,S1), open(F2,read,S2),
	( similar_streams(S1, S2) ->
	    close(S1), close(S2)
	;
	    close(S1), close(S2),
	    fail
	).

similar_streams(S1, S2) :-
	at_eof(S1), at_eof(S2), !.
similar_streams(S1, S2) :-
	get_stream_info(S1, line, LineNr),
	( read_string(S1, end_of_line, _, Line1) -> true ; Line1 = "" ),
	( read_string(S2, end_of_line, _, Line2) -> true ; Line2 = "" ),
	string_list(Line1, List1),
	string_list(Line2, List2),
	( similar_lists(List1, List2) ->
	    similar_streams(S1, S2)
	;
	    printf("--- Files differ in line %d:%n", LineNr),
	    writeln(Line1),
	    writeln("---"),
	    writeln(Line2),
	    fail
	).

similar_lists([], []) :- !.		% finished
similar_lists([C1|T1], [C2|T2]) :-	% treat space sequences as equal
	blank_space(C1),
	blank_space(C2),
	!,
	skip_spaces(T1, NextT1),
	skip_spaces(T2, NextT2),
	similar_lists(NextT1, NextT2).
similar_lists([C|T1], [C|T2]) :- !,	% skip identical characters
	similar_lists(T1, T2).
similar_lists([C1|T1], T2) :-		% skip one left
	ignored_char(C1),
	!,
	similar_lists(T1, T2).
similar_lists(T1, [C2|T2]) :-		% skip one right
	ignored_char(C2),
	!,
	similar_lists(T1, T2).

    ignored_char(13).

    blank_space(0' ).
    blank_space(0'	).

    skip_spaces([], []).
    skip_spaces([C|T], L) :-
	( blank_space(C) -> skip_spaces(T, L) ; L = [C|T] ).


:- untraceable reset_int_handler/0.
:- skipped reset_int_handler/0.
