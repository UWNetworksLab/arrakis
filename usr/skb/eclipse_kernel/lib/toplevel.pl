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
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: toplevel.pl,v 1.1.2.1 2009/01/15 13:02:12 jschimpf Exp $
% ----------------------------------------------------------------------

%
% This module contains two variants of the Eclipse toplevel loop,
% a command line version (tty) and a server version (gui).
%
% The tty variant works on the following tty streams:
%
%	toplevel_output		prompts and messages
%	toplevel_input		query and command input
%	answer_output		results and variable bindings
%
%
% The server variant communicates with a user interface via
% 4 queues (toplevel_in, toplevel_out, gui_interrupt_request
% and answer_output) and a string/exdr-based protocol:
%
%	toplevel_out		(fromec,exdr)	query result
%	toplevel_in		(toec,exdr)	toplevel command
%	answer_output		(fromec,text)	results and variable bindings
%	gui_interrupt_request	(fromec,exdr)	query interrupt acknowledge
%
%
% After startup, an initial "Idle" message is sent on toplevel_out.
% The toplevel_in queue then accepts the following command syntax
% in exdr-format:
%
% call(GoalString)
%	Execute the specified goal under control of the toplevel.
%
%	The result will be returned on the toplevel_out queue as a
%	string in <exdr>-format, and is one of:
%
%		"Yes"
%		"More"
%		"No"
%		"Abort"
%
%	Variable bindings will be printed (in no particular format)
%	on the answer_output queue.
%
% profile(GoalString)
% port_profile(GoalString)
%	Like call(GoalString), but wrap the call into profile/1 or
%	port_profile/2.
%
% more
%	Request backtracking into the last goal (which must have
%	succeeded with a "More" result). The result is as for call(...).
%
% end
%	Cut and fail the last goal (only useful if the goal had
%	succeeded with a "More" or "Yes" result).
%	
% exit
%	Terminate the toplevel loop.
%
%
% The Eclipse engine can be thought of as being in one of 3 states
% (idle,running,success) with the following transitions:
%
%    OldState	toplevel_in	toplevel_out	NewState
%
%    (initial)			"Idle"		idle
%    idle	call("...")			running
%    idle	end				idle
%    idle	exit				(final)
%    running			"Yes"		success
%    running			"More"		success
%    running			"No"		idle
%    running			"Abort"		idle
%    success	end				idle
%    success	more				running
%    success	exit				(final)
%

%----------------------------------------------------------------------
:- module(toplevel).
%----------------------------------------------------------------------

:- export
	toplevel_init/1,
	toplevel/0,
	break/0.

:- export			% handlers
	toplevel_prompt/2,
	print_values/3,
	tty_ask_more/2,
	tty_banner/2,
	start_break/3,
	end_break/3,
	delayed_goals_handler/3.


:- comment(summary, "Interactive ECLiPSe toplevel interpreter").
:- comment(date, "$Date: 2009/01/15 13:02:12 $").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(author, "Joachim Schimpf, IC-Parc").
:- comment(desc, html("
	Note that this library is normally loaded automatically
	by the ECLiPSe development environment.
    ")).

%----------------------------------------------------------------------

:- import
	get_cut/1,
	compiled_file_handler/3,
	set_default_error_handler/2,
	printf_with_current_modes/2,
	mutex_one/2,
	stack_overflow_message/1,
	printf_goal/2,
	sepia_version_banner/2,
	call_local/1
   from sepia_kernel.


:- local variable(toplevel_type).	% 'tty' or 'gui'
:- local variable(toplevel_lock).	% for parallelism

:- local variable(history_index).	% query history
:- local variable(history_length).
:- local array(history(30)).


:- comment(toplevel_init/1, [
    summary:"Initialize a toplevel",
    args:["Type":"one of the atoms 'tty' or 'gui'"],
    desc:html("
    	Initializes streams and event handlers in preparation to
	a call to toplevel/0. Two variants of toplevel loop can be
	selected: command line oriented (tty) or server oriented
	(gui). The latter is intended for an ECLiPSe which is
	embedded or remote-interfaced to a GUI-based host program.
    "),
    see_also:[toplevel/0,break/0]
]).

toplevel_init(Type) :-
	getval(toplevel_type, Old),
	( var(Old) ->
	    ( Type = tty -> tty_toplevel_init, setval(toplevel_type, tty)
	    ; Type = gui -> gui_toplevel_init, setval(toplevel_type, gui)
	    ; Type = peer -> gui_toplevel_init, setval(toplevel_type, gui)
	    ; error(6, toplevel_init(Type))
	    )
	; Old = Type ->
	    true
	;
	    printf(error, "%w toplevel already running in %w%n",
		[Old,toplevel_init(Type)])
	).


:- comment(toplevel/0, [
    summary:"Run a toplevel interpreter",
    desc:html("
    	Run a toplevel query interpreter. This is a program that
	allows to input ECLiPSe queries, runs them, and displays
	the results. Which variant of this interpreter is invoked
	depends on how it was initialized in toplevel_init/1.
    "),
    see_also:[toplevel_init/1,break/0]
    ]).

toplevel :-
	getval(toplevel_type, Type),
	( var(Type) ->
	    printf(error, "toplevel not yet initialized%n", [toplevel]),
	    abort
	; Type = tty ->
	    tty_toplevel
	; Type = gui ->
	    gui_toplevel
	;
	    abort
	).


%----------------------------------------------------------------------
% Command line based 'tty' toplevel
%----------------------------------------------------------------------

% A version of tyi/2 which allows an optional newline when used on non-tty
% streams (for pseudo-terminals that don't have raw mode, e.g. inside emacs)
:- local tyi/2.
tyi(S, C) :-
	eclipse_language:tyi(S, C),
	( get_stream_info(S, device, tty) ->
	    true
	; newline(C) ->
	    true
	;
	    eclipse_language:tyi(S, NL),
	    ( newline(NL) -> true ; unget(S) )
	).

newline(10).
newline(13).


tty_toplevel_init :-

	% make the three streams for the toplevel protocol
	( get_stream_info(input, fd, FD) ->
	    open(dup(FD), read, toplevel_input)
	;
	    set_stream(toplevel_input, input)
	),
	set_stream(answer_output, output),
	set_stream(toplevel_output, output),

	% optional: init debugging tools and interrupt handling
	ensure_loaded(library(tracer_tty)),

	% toplevel event handlers
	set_default_error_handler(139, compiled_file_handler/3),
	reset_event_handler(139),
	set_default_error_handler(153, toplevel_prompt/2),
	reset_event_handler(153),
	set_default_error_handler(154, true/0),
	reset_event_handler(154),
	set_default_error_handler(155, print_values/3),
	reset_event_handler(155),
	set_default_error_handler(156, tty_ask_more/2),
	reset_event_handler(156),
	set_default_error_handler(158, start_break/3),
	reset_event_handler(158),
	set_default_error_handler(159, end_break/3),
	reset_event_handler(159),
	set_default_error_handler(164, tty_banner/2),
	reset_event_handler(164),
	set_default_error_handler(273, delayed_goals_handler/3),
	reset_event_handler(273).



tty_toplevel :-
	compile_rc,
	sepia_version_banner(Message, _Date),
	error(164, Message),		% print the banner
	history_init,
	( get_flag(gc, off) ->
		true
	;
		garbage_collect		% to start with small variable numbers
	),
	tty_abort_loop,
	writeln(log_output, "\nbye").


tty_abort_loop :-
	repeat,				% we start here after abort
	    set_stream_property(toplevel_input, prompt, '\t'),
	    set_stream_property(toplevel_input, prompt_stream, toplevel_output),
	    block(tty_fail_loop, Tag, top_abort(Tag)),
	!.


tty_fail_loop :-
	repeat,				% we start here after every query
	    (get_flag(break_level, 0) -> trimcore ; true),
	    (				% flush all tty streams
		current_stream(S),
		get_stream_info(S, device, tty),
		\+ get_stream_info(S, mode, read),
		flush(S),
		fail
	    ;
		true
	    ),
	    get_flag(toplevel_module, M),
	    error(153, M, M),		% extension hook: toplevel prompt
	    set_stream_property(toplevel_input, reprompt_only, on),

	    readvar(toplevel_input, Goal, VL)@M,
	    tty_run_command(Goal, VL, M),
	fail.


% tty_run_command(+Goal, +VarList, +Module)
% this deals with the history and goal expansion

tty_run_command(Var, _, _) :-
	var(Var), !,
	error(4, Var).
tty_run_command(N, _, M) :-
        integer(N),
        !,
        ( history_retrieve(N, Goal-VL) ->
	    printf_goal(toplevel_output, Goal)@M,
	    put(toplevel_output, 0'.),
	    nl(toplevel_output),
            tty_run_command(Goal, VL, M)
        ;
            history_print(M)
        ).
tty_run_command(h, _, M) :- !,
	history_print(M).
tty_run_command(end_of_file, _, _) :- !,
	( get_flag(ignore_eof, on), get_flag(break_level, 0) ->
	    writeln(toplevel_output, "Use \"halt.\" to exit ECLiPSe.")
	;
	    exit_block(end)
	).
tty_run_command(Goal0, VL0, M) :-
        history_append(Goal0-VL0),
	error(154, goal(Goal0, VL0, NewGoal, NewVL), M), % extension hook
	( is_list(NewVL) -> Goal1=NewGoal, VL=NewVL ; Goal1=Goal0,VL=VL0 ),
	( get_flag(goal_expansion,on) ->
	    expand_goal(Goal1, Goal2)@M,
	    tty_run_command1(Goal2, VL, M)
	;
	    tty_run_command1(Goal1, VL, M)
	).


% tty_run_command1(+Goal, +VarList, +Module)
% this interprets some toplevel commands and calls run_goal otherwise

tty_run_command1(Var, _, _) :-
	var(Var), !,
	error(4, Var).
tty_run_command1('?-'(Goal), VL, M) :- !,
	tty_run_command1(Goal, VL, M).
tty_run_command1(':-'(Goal), _, M) :- !,
	call_local(Goal)@M,
	!.
tty_run_command1(notrace, _, _):- !,
	tty_run_command1(nodebug, _, _).
tty_run_command1(nodebug, _, _):- !,
	set_flag(debugging, nodebug),
	writeln(toplevel_output, "Debugger switched off").
tty_run_command1(trace, _, _):- !,
	set_flag(debugging, creep),
	writeln(toplevel_output, "Debugger switched on - creep mode").
tty_run_command1(debug, _, _):- !,
	set_flag(debugging, leap),
	writeln(toplevel_output, "Debugger switched on - leap mode").
tty_run_command1(module(M), _, _):- !,
	set_flag(toplevel_module, M).
tty_run_command1(Goal, VL, M) :-			% execute a general goal
        run_goal(Goal, VL, M, call).


tty_ask_more(_, yes) :- !,
	tty_ask_more(_, more_answers).
tty_ask_more(_, more_answers) :- !,
	write(toplevel_output, ' ? '),
	flush(toplevel_output),
	tyi(toplevel_input, C),
	( C == 0'; ->
		put(toplevel_output, C),
		nl(toplevel_output),
		fail
	; newline(C) ->
		nl(toplevel_output)
	;
		put(toplevel_output, C),
		printf(toplevel_output, "%nType ; for more solutions, otherwise <return>", []),
		tty_ask_more(_, more_answers)
	).
tty_ask_more(_, _).



toplevel_prompt(_, M) :-		% default handler for event 153
	is_predicate(toplevel_prompt/1)@M,
	!,
	M:toplevel_prompt(M).		% for compatibility
toplevel_prompt(_, M) :-
	get_stream_info(toplevel_input, prompt_stream, Out),
	!,
	history_next(I),
	put(Out, 0'[),
	write(Out, M),
	put(Out, 0' ),
	write(Out, I),
	write(Out, "]: "),
	flush(Out).
toplevel_prompt(_, _).



delayed_goals_handler(_, [], _):- !.
delayed_goals_handler(_, List, _):-
	length(List, Num), Num > 5,
	printf(toplevel_output, "\nThere are %d delayed goals. Do you want to see them? (y/n) %b", Num),
	no_typed, !,
	nl(toplevel_output).
delayed_goals_handler(_, List, M):-
	nl(toplevel_output),
	writeln(answer_output, "\nDelayed goals:"),
	get_flag(output_mode, Mode),
	concat_string(['\t%', Mode, 'Gw%n'], Format),
	print_delayed_list(answer_output, List, Format, M),
	flush(answer_output).

    no_typed :-
	tyi(toplevel_input, C),
	( C = 0'n -> true ; C = 0'y -> fail ; no_typed ).

    print_delayed_list(_, [], _, _).
    print_delayed_list(Stream, [Susp|Rest], Format, M) :-
	suspension_to_qualgoal(Susp, Goal, M),
	printf(Stream, Format, [Goal])@M,
	print_delayed_list(Stream, Rest, Format, M).

    suspension_to_qualgoal(Susp, Goal, M) :-
	get_suspension_data(Susp, qualified_goal, LM:UGoal),
	functor(UGoal, N, A),
	( get_flag(N/A, definition_module, DM)@LM,
	  get_flag(N/A, definition_module, DM)@M ->
	    Goal=UGoal
	;
	    Goal=LM:UGoal
	).


tty_banner(_, Message) :-		% default handler for event 164
	write(log_output, Message),
	flush(log_output).


%----------------------------------------------------------------------
% Embedded/Remote toplevel
%----------------------------------------------------------------------

gui_toplevel_init :-

	% make the three streams for the toplevel protocol
	peer_queue_create(toplevel_in, host, sync, toec, ''),
	peer_queue_create(toplevel_out, host, sync, fromec, ''),
	peer_queue_create(answer_output, host, sync, fromec, ''),

	% redirect standard i/o streams to queues if not already done
	( peer_queue_get_property(output, peer_type, _) ->
	    % assume we already have those queues (e.g. when the
	    % remote-eclipse was created with the UseQueues option)
	    true
	;
	    peer_queue_create(input, host, sync, toec, ''),
	    peer_queue_create(output, host, sync, fromec, ''),
	    peer_queue_create(error, host, sync, fromec, '')
	),
	set_stream_property(output, flush, end_of_line),
	set_stream_property(error, flush, end_of_line),
	set_stream(log_output, output),

	% separate warning_output from output (for colouring)
	% (otherwise: set_stream(warning_output, output))
	peer_queue_create(warning_output, host, sync, fromec, ''),
	set_stream_property(warning_output, flush, end_of_line),

	% Interrupt handling
	% Protocol:
	%	Eclipse is running and has control
	%	remote:
	%		remote sends exdr 'int' on async gui_pause_request
	%		post_events_from_stream/1 handler posts 'int' event
	%	embedded:
	%		host posts 'int' event asynchronously
	%	user_stop_request_handler/1 replies 'int' on sync gui_interrupt_request
	%	remote optionally sends 'abort' on async gui_pause_request
	%	Once Eclipse has control back, it executes 'abort' handler
	% 
	( peer_get_property(host, type, remote) ->
	    event_create(post_events_from_stream(gui_pause_request), [defers], GIWEvent)@sepia_kernel,
	    peer_queue_create(gui_pause_request, host, async, bidirect, GIWEvent)
	;
	    % int-event is posted directly
	    true
	),
	set_interrupt_handler(int, event/1),
	peer_queue_create(gui_interrupt_request, host, sync, fromec, ''),
	set_event_handler(int, user_stop_request_handler/1),

	% toplevel event handlers
	set_default_error_handler(139, compiled_file_handler/3),
	reset_event_handler(139),
	set_default_error_handler(155, print_values/3),
	reset_event_handler(155),
	set_default_error_handler(156, gui_ask_more/2),
	reset_event_handler(156),
	set_default_error_handler(273, gui_delayed_goals_handler/2),
	reset_event_handler(273),

	% optional: init debugging tools and interrupt handling
	set_event_handler(139, true/0),	% suppress messages
	ensure_loaded(library(tracer_tcl)),
	tracer_tcl:install_guitools,
	reset_event_handler(139).


gui_toplevel :-
	write_exdr(toplevel_out, "Idle"),
	repeat,
	    block(gui_fail_loop, Tag, top_abort(Tag)),
	!.

gui_fail_loop :-
	repeat,
	    (get_flag(break_level, 0) -> trimcore ; true),
	    read_command(Command),
	    state_idle(Command),
	fail.


state_idle(call(GoalString)) :- !,
	exec_goal(GoalString, call).
state_idle(profile(GoalString)) :- !,
	exec_goal(GoalString, profile).
state_idle(port_profile(GoalString)) :- !,
	exec_goal(GoalString, port_profile).
state_idle(end) :- !.
state_idle(exit) :- !,
	exit_block(end).
state_idle(Junk) :- !,
	error(6, state_idle(Junk)).


state_success :-
	read_command(Command),
	state_success(Command).

state_success(more) :- !,
	fail.
state_success(end) :- !.
state_success(exit) :- !,
	exit_block(end).
state_success(Junk) :-
	error(6, state_success(Junk)).
	

read_goal(GoalString,Goal,Vars) :-
	open(GoalString,string,Stream),
	get_flag(toplevel_module, Module),
	( block(readvar(Stream,Goal,Vars)@Module,_,fail) ->
	    close(Stream)
	;
	    close(Stream),
	    fail
	).

% exec_goal(GoalString, RunMode)
% read a goal from toplevel_in, run it, do the more-dialog
% and always succeed in the end
exec_goal(GoalString, RunMode) :-
	read_goal(GoalString,Goal,Vars),	% fails on syntax error
	get_flag(toplevel_module, Module),
	tracer_tcl:register_inspected_term(Goal, Module),
	printf_with_current_modes(answer_output, (?- Goal))@Module,
	writeln(answer_output, "."),
	flush(answer_output),
	( get_flag(goal_expansion,on) ->
	    expand_goal(Goal, TransGoal)@Module	% might fail
	;
	    TransGoal=Goal
	),
	!,
	run_goal(TransGoal, Vars, Module, RunMode).
exec_goal(_, _) :-
	exit_block(abort).


gui_ask_more(_, no) :- !,
	gui_ask_more(_, no_answer).
gui_ask_more(_, no_answer) :- !,
	write_exdr(toplevel_out, "No").
gui_ask_more(_, last_yes) :- !,
	gui_ask_more(_, last_answer).
gui_ask_more(_, last_answer) :- !,
	write_exdr(toplevel_out, "Yes"),
	state_success.			% fail if more requested
gui_ask_more(_, yes) :- !,
	gui_ask_more(_, more_answers).
gui_ask_more(_, more_answers) :-
	write_exdr(toplevel_out, "More"),
	state_success.			% fail if more requested


read_command(C) :-
	flush(output),
	flush(error),
	flush(warning_output),
	flush(log_output),
	flush(toplevel_out),		% send query result
	read_exdr(toplevel_in,C).	% gui interaction happens here


drain_stream(S) :-
	( select([S], 0, []) -> true ; get(S, _), drain_stream(S) ).
	

gui_delayed_goals_handler(_, Susps) :-	% handler for 273
	suspensions(Susps),
	length(Susps, N),
	( N > 1 ->
	    printf(answer_output, "There are %d delayed goals.%n", N)
	; N > 0 ->
	    printf(answer_output, "There is 1 delayed goal.%n", [])
	;
	    true
	).


% The handler which is executed when the user hits the interrupt button.
% This must be untraceable/skipped to prevent internals from being traced
% when selecting the 'continue in creep mode' option.

user_stop_request_handler(Event) :-
	write_exdr(gui_interrupt_request, Event),
	flush(gui_interrupt_request).

:- skipped(user_stop_request_handler/1).
:- untraceable(user_stop_request_handler/1).


%----------------------------------------------------------------------
% Common toplevel predicates
%----------------------------------------------------------------------

% the break/0 predicate - it only works in the tty version

:- comment(break / 0, [
    summary:"A new invocation of a top level loop is called as a subgoal",
    desc:html("
    Used to start a new invocation of the top level loop.  Compiled
    procedures and event handler redefinitions remain valid within the new
    invocation level.  Exiting with a Control-D (or end_of_file) or
    calling exit_block(end) will return the control to the previous level.
    "),
    args:[],
    resat:"   No.",
    fail_if:"   None.\n\n",
    eg:"
[eclipse]: [user].
 go:- writeln(\"**** Enter prolog goals and press ^D\"),
      break,
      writeln(\"**** Execution continues...\").
 user compiled 144 bytes in 0.02 seconds

yes.
[eclipse]: go.
**** Enter prolog goals and press ^D

Entering break level 1
[eclipse]: date(X).

X = \"Wed Aug  9 12:21:48 1989\\n\"
yes.
[eclipse]: ^D

Leaving break level 1
**** Execution continues...

yes.
[eclipse]:
",
	see_also:[toplevel/0, exit_block / 1]]).

break :-
	getval(toplevel_type, tty),
	!,
	get_flag(break_level, Level0),
	Level is Level0+1,
	set_flag(break_level, Level),
	get_flag(toplevel_module, M),
	error(158, Level, M),		% calls start_break/3
	tty_abort_loop,
	set_flag(break_level, Level0),
	error(159, Level, M).		% calls end_break/3
break :-
	error(141, break).


    start_break(_, Level, M) :-
	printf(toplevel_output, "\nEntering break level %d\n", Level)@M.

    end_break(_, Level, M) :-
	set_flag(toplevel_module, M),
	printf(toplevel_output, "\n\nLeaving break level %d\n%b",Level)@M.


print_values(_, Vars, Module) :-		% handler for 155
	Stream = answer_output,
	( get_stream(Stream, output) -> nl(answer_output) ; true ),
	( foreach([Name|Value],Vars),param(Stream,Module) do
	    write(Stream, Name),
	    write(Stream, ' = '),
	    printf_with_current_modes(Stream, Value)@Module,
	    nl(Stream)
	).


run_goal(TransGoal, Vars, Module, RunMode) :-
	shelf_create(count(0), Count),
	mutex_init(toplevel_lock),
	% reset invocation numbers etc. unless in break level
	( debug_reset -> true ; true ),
	cputime(Tstart),
	(
	    get_cut(Before),
	    top_call_local(TransGoal, Module, RunMode),
	    Time is cputime-Tstart,
	    get_cut(After),
	    shelf_get(Count, 1, NSol0),
	    NSol is NSol0+1,
	    shelf_set(Count, 1, NSol),
	    % reporting a solution and prompting for more
	    % is in a mutex region for the parallel system
	    mutex_one(toplevel_lock, (
		error(155, Vars, Module),
		suspensions(Susps),
		error(273, Susps, Module),
		( Before == After ->
		    ( NSol == 1 ->
			printf(answer_output, "Yes (%.2fs cpu)%n%b", [Time])
		    ;
			printf(answer_output, "Yes (%.2fs cpu, solution %d)%n%b", [Time,NSol])
		    )
		;
		    printf(answer_output, "Yes (%.2fs cpu, solution %d, maybe more)", [Time,NSol]),
		    ( getval(toplevel_type, tty) -> true ; nl(answer_output) ),
		    flush(answer_output)
		),
		answer(Vars, Before, After, AnswerCode),
		error(156, AnswerCode)
	    ))
	->
	    true
	;
	    Time is cputime-Tstart,
	    ( getval(toplevel_type, tty) -> nl(answer_output) ; true ),
	    printf(answer_output, "No (%.2fs cpu)%n%b", [Time]),
	    error(156, no)
	).


    top_call_local(Goal, Module, call) :-
	call_local((
	    get_flag(debugging, TraceMode),
	    ( TraceMode = creep ->
		trace(Goal)@Module
	    ; TraceMode = leap ->
		debug(Goal)@Module
	    ;
		call(Goal)@Module
	    )
	)).
    top_call_local(Goal, Module, profile) :-
	call_local((
	    profile:profile(Goal)@Module
	)).
    top_call_local(Goal, Module, port_profile) :-
	call_local((
	    port_profiler:port_profile(Goal,[])@Module
	)).


    :- mode answer(++,++,++,-).
    answer([], Cut, Cut, last_yes    ) :- !.
    answer([], _,   _,   yes         ) :- !.
    answer(_,  Cut, Cut, last_answer ) :- !.
    answer(_,  _,   _,   more_answers).


top_abort(end) ?- !.
top_abort(abort) ?- !,
	writeln(answer_output, "Abort"),
	flush(answer_output),
	( current_stream(toplevel_in) ->
	    drain_stream(toplevel_in),
	    write_exdr(toplevel_out, "Abort")
	;
	    true
	),
	fail.
top_abort(Tag) :-
	stack_overflow_message(Tag), !,
	top_abort(abort).
top_abort(Tag) :-
	block(error(230, exit_block(Tag)), T, true),
	top_abort(T).


%-------------------------------
% compile .rc file
%-------------------------------

compile_rc :-
	initfile(File),
	!,
	get_flag(toplevel_module, M),
	compile(File, M).
compile_rc.

initfile(File) :-
	getenv("ECLIPSEINIT", File),
	!, File \= "".
initfile(File) :-
	RcFile = ".eclipserc",
	( File = RcFile
	; getenv('HOME', Home),
	  concat_string([Home,"/",RcFile], File)
	),
	exists(File),
	!.


%-------------------------------
% history
%-------------------------------

history_next(I) :-
	getval(history_index, I).

history_oldest(I) :-
	I is max(1, getval(history_index) - getval(history_length)).

history_append(Goal) :-
	getval(history_index, N),
	I is N mod getval(history_length),
	setval(history(I), Goal),
	N1 is N + 1,
	setval(history_index, N1).

history_print(M) :-
	getval(history_index, Next),
	First is max(1, Next - getval(history_length)),
	history_print(First, Next, M).

history_print(I, N, M) :-
	I < N ->
	    history_retrieve(I, Goal-_),
	    write(toplevel_output, I),
	    put(toplevel_output, 0'	),
	    printf_goal(toplevel_output, Goal)@M,
	    put(toplevel_output, 0'.),
	    nl(toplevel_output),
	    I1 is I+1,
	    history_print(I1, N, M)
	;
	    true.

history_retrieve(N, Goal) :-
	history_next(Next),
	history_oldest(Oldest),
	( N < 0 ->
	    I is Next + N
	;
	    I = N
	),
	I >= Oldest, I < Next,
	I1 is I mod getval(history_length),
	getval(history(I1), Goal).

history_collect(List) :-
	history_oldest(Oldest),
	history_next(Next),
	history_collect(Oldest, Next, List).

history_collect(N, N, []) :- !.
history_collect(N, Lim, [Goal|List]) :-
	history_retrieve(N, Goal),
	N1 is N+1,
	history_collect(N1, Lim, List).

history_init :-
	current_array(history(Max), _),
	setval(history_index, 1),
	setval(history_length, Max),
	( existing_file(".eclipse_history", [""], [readable], File) ->
	    read_history(File)
	; existing_file("~/.eclipse_history", [""], [readable], File) ->
	    read_history(File)
	;
		true
	).

read_history(File) :-
	open(File, read, S),
	readvar(S, Term, VL),
	read_history(S, Term, VL),
	close(S).

read_history(_, end_of_file, _) :- !.
read_history(Stream, Goal, VL) :-
	history_append(Goal-VL),
	readvar(Stream, NewTerm, NewVL),
	read_history(Stream, NewTerm, NewVL).


:- export write_history/0.
:- comment(write_history/0, [template:"write_history",
    summary:"Writes the current command history into the .eclipse_history file"
    ]).

write_history :-
	history_collect(List),
	open('.eclipse_history', write, Stream),
	write_history(Stream, List).

write_history(Stream, []) :-
	close(Stream).
write_history(Stream, [Goal-_|More]) :-
	printf(Stream, "%OQDw.\n", [Goal]),
	write_history(Stream, More).

