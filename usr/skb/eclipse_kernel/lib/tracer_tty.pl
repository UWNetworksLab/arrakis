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
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: tracer_tty.pl,v 1.4.2.1 2009/01/15 13:02:12 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe II debugger -- TTY Interface
%
% $Id: tracer_tty.pl,v 1.4.2.1 2009/01/15 13:02:12 jschimpf Exp $
%
% Authors:	Joachim Schimpf, IC-Parc
%		Kish Shen, IC-Parc
%

:- module(tracer_tty).

:- pragma(nodebug).
:- pragma(system).

%:- import struct(tf), struct(trace_line) from sepia_kernel.
:- import sepia_kernel.

:- local
	struct(inspect(type,top,path,written,module,goalf)),

	reference(exec_state),

	variable(next_cmd),
	variable(indent_step),
	variable(dbg_format_string),
	variable(dbg_goal_format_string),
	variable(dbg_print_depth),
	variable(show_module).

:- export
	print_trace_line/1.


:- import
	set_default_error_handler/2,
	configure_prefilter/5,
	trace_mode/2,
	get_attribute/3,
	get_tf_prop/3,
	failure_culprit/2,
	find_goal/3,
	meta_attributes/1,
	monitor_term/4,
	new_invoc/1,
	timestamp_older/4,
	current_td/1,
	cut_to_stamp/2
    from sepia_kernel.

:- lib(development_support).

:- local break/0.

%----------------------------------------------------------------------
% Tracer TTY interface
%----------------------------------------------------------------------


% Make a separate file descriptor for the debugger input so that it
% doesn't get mixed up with the standard input of the debugged program.
:- ( get_stream_info(input, fd, FD) -> open(dup(FD), read, debug_input)
	; set_stream(debug_input, input) ).
:- set_stream(debug_output, output).

trace_start_handler_tty :-
	clear_cmd.

trace_line_handler_tty(_, Current) :-
        setval(exec_state, Current),
	print_trace_line(Current),
	interact(Current, Cont),
	call(Cont).	% may cut_to/fail

:- set_default_error_handler(250, trace_start_handler_tty/0),
   reset_event_handler(250).
:- set_default_error_handler(252, trace_line_handler_tty/2),
   reset_event_handler(252).

print_trace_line(trace_line{port:Port, frame:Frame}) :-
        Frame = tf{invoc:Invoc,goal:Goal,depth:Depth,prio:Prio,module:M},
	!,
        % print priority only if not the normal 12
        (Prio == 12 -> PrioS = "" ; concat_string([<,Prio,>], PrioS)),
	( get_tf_prop(Frame, skip, on) -> Prop = 0'S ; Prop = 0'  ),
	( get_tf_prop(Frame, break) =\= 0 -> Spied = 0'#
	; get_tf_prop(Frame, spy, on) -> Spied = 0'+ ; Spied = 0'  ),
	Indent is Depth*getval(indent_step),
	printf(debug_output, "%c%c%*c(%d) %d %A%s  ",
			[Prop, Spied, Indent, 0' , Invoc, Depth, Port, PrioS]),
	( getval(show_module,on) -> MGoal = Goal@M ; MGoal = [Goal] ),
	getval(dbg_goal_format_string, Format),
	printf(debug_output, Format, MGoal)@M.
print_trace_line(inspect{type:Type,module:M,written:[CurrentTerm|_],path:Pos}) :-
	(Pos == [], Type == goal ->
	    getval(dbg_format_string, Format)
	;
	    getval(dbg_goal_format_string, Format)
        ),
	printf(debug_output, Format, [CurrentTerm])@M,
	printf(debug_output, "%n        INSPECT  ", []),
	print_current_summary(debug_output, CurrentTerm, M).


print_suspensions([], _, _) :-
	writeln(debug_output, "\n------------ end ------------").
print_suspensions([S|Ss], Kind, Prio) :-
	( get_suspension_data(S, state, Kind) ->
	    ( Prio = all ->
		print_suspension(S)
	    ; get_suspension_data(S, priority, Prio) ->
		print_suspension(S)
	    ;
	        true
	    )
	;
	    true
	),
	print_suspensions(Ss, Kind, Prio).

    print_suspension(S) :-
	get_suspension_data(S, goal, Goal),
	get_suspension_data(S, module, M),
	get_suspension_data(S, invoc, Invoc),
	get_suspension_data(S, priority, Prio),
	( get_suspension_data(S, spy, on) -> Spied = 0'+ ; Spied = 0'  ),
	printf(debug_output, "%n %c(%d) <%d>  ", [Spied, Invoc, Prio]),
	getval(dbg_goal_format_string, Format),
	printf(debug_output, Format, Goal)@M.


% print ancestor if it exists, otherwise fail

print_ancestor(Stack, Anc) :-
	parent(Stack, Anc),
	Anc = tf{}, 	% may fail
	( timestamp_older(Anc, chp of tf, Stack, chp of tf) ->
	    print_trace_line(trace_line{port:'*....', frame:Anc})
	;
	    print_trace_line(trace_line{port:'....', frame:Anc})
	).

print_ancestors_bottom_up(Stack) :-
	parent(Stack, Anc),
	( Anc = tf{} ->
	    print_ancestors_bottom_up(Anc),
	    print_ancestor(Stack, _),
	    nl(debug_output)
	;
	    true
	).

    parent(0, 0) :- !.
    parent(tf{parent:Parent}, Parent).


%
% Print prompt, read and execute commmands
%
% - Display commands are immediately excecuted and call interact/2 again
% - Continuation commands set the global debugger parameters and succeed
%
interact(Current, Cont) :-
	( getval(next_cmd, Num-Cmd) ->
	    printf(debug_output, "   %%> %d", [Num])
	;
	    write(debug_output, "   %> "), flush(debug_output),
	    tyi_num(debug_input, Num, Cmd)
	),
	( do_tracer_command(Cmd, Current, Num, Cont) ->
	    true
	;
	    printf(error, "%n *** Command doesn't exist, is not applicable here, or was aborted: %c%n%b", [Cmd]),
	    clear_cmd,
	    interact(Current, Cont)
	).


%
% do_tracer_command(Command, CurrentTraceLine, Count, Cont)
%
% Command is a single-character command
% CurrentTraceLine is one of
%	trace_line{...}
%	inspect{...}
% Count is the numeric argument given to the command (default 0)
% Cont is a goal to execute before continuing

:- mode do_tracer_command(+,+,+,-).
do_tracer_command(0'a, _Current, _N, Cont) :- !,
	confirm("abort"),
	getval(exec_state, CurrentPort),
	trace_mode(5, 0),
	( CurrentPort = trace_line{port:leave} ->
	    % don't abort, we may not have any catching block!
	    % just behave like n (nodebug)
	    Cont = true
	;
	    Cont = abort
	).

do_tracer_command(0'b, Current, _, Cont) :- !,
	writeln(debug_output, "break"),
	break,
	print_trace_line(Current),
	interact(Current, Cont).

do_tracer_command(13, Current, 0, Cont) :-
	Current = trace_line{},
	!,
	do_tracer_command(0'c, Current, 0, Cont).
do_tracer_command(0'c, _Current, N, true) :- !,
	writeln(debug_output, "creep"),
	trace_mode(0, []),
	store_cmd(0'c, N).

do_tracer_command(0'd, Current, _, Cont) :- !,
	get_param_default("delayed goals with prio", all, Prio),
	write(debug_output, "------- delayed goals -------"),
	suspensions(Susps),
	print_suspensions(Susps, 0, Prio),
	print_trace_line(Current),
	interact(Current, Cont).

do_tracer_command(0'f, Current, _, Cont) :- !,
	get_goal_stack(Current, Port, Stack),
	get_param_default("fail goal", here, N),
	( N = here ->
	    ( Port = '....' ->
		Cont = (cut_to_stamp(Stack, chp of tf),fail)
	    ; Port = fail ; Port = leave ->
		% already failing: don't fail again, we would miss a choicepoint
		% turn it into a creep instead...
		trace_mode(0, []),
	        Cont = true
	    ;
		Cont = fail
	    )
	;
	    ( find_goal(N, Stack, Frame) ->
		Cont = (cut_to_stamp(Frame, chp of tf),fail)
	    ;
		printf(error, "*** Goal (%d) not available!%b", [N]),
		interact(Current, Cont)
	    )
	).

do_tracer_command(0'g, Current, _, Cont) :- !, 
	get_goal_stack(Current, _, Frame),
	writeln(debug_output, "ancestor"),
	( print_ancestor(Frame, NewFrame) ->
	    interact(trace_line{port:'....', frame:NewFrame}, Cont)
	;
	    interact(Current, Cont)
	).

do_tracer_command(0'G, Current, _N, Cont) :- !,
	get_goal_stack(Current, _, Frame), 
	(confirm("print all ancestors") ->
	    print_ancestors_bottom_up(Frame) ; true
        ),
	print_trace_line(Current),
	interact(Current, Cont).

do_tracer_command(0'i, Current, _, true) :- !,
	get_goal_stack(Current, _, tf{invoc:Invoc}),
	get_param_default("jump to invoc", Invoc, N),
	trace_mode(1, N).

do_tracer_command(0'j, Current, 0, true) :- !,
	get_goal_stack(Current, _, tf{depth:Depth}),
	Depth1 is max(0,Depth-1),
	get_param_default("jump to level", Depth1, N),
	( N < Depth -> trace_mode(3, N) ; trace_mode(4, N) ).

do_tracer_command(0'l, _Current, N, true) :- !,
	writeln(debug_output, "leap"),
	trace_mode(2, []),
	store_cmd(0'l, N).

do_tracer_command(0'm, Current, _N, Cont) :- !,
	( getval(show_module, off) ->
	    writeln(debug_output, "show module"),
	    setval(show_module, on)
	;
	    writeln(debug_output, "don't show module"),
	    setval(show_module, off)
	),
	print_trace_line(Current),
	interact(Current, Cont).

do_tracer_command(0'n, _Current, _N, true) :- !,
	confirm("nodebug"),
	trace_mode(5, 0).

do_tracer_command(0'o, Current, _N, Cont) :- !,
	change_output_mode,
	print_trace_line(Current),
	interact(Current, Cont).

do_tracer_command(0'q, Current, _N, Cont) :- !,
	writeln(debug_output, "query culprit"),
	( failure_culprit(CulpritInvoc, LastInvoc) ->
	    ( CulpritInvoc > LastInvoc ->
		printf(debug_output, "failure culprit was (%d) - ", [CulpritInvoc]),
		get_param_default("jump to invoc", CulpritInvoc, N),
		trace_mode(1, N),
		Cont = true
	    ;
		get_goal_stack(Current, Port, _),
		( CulpritInvoc = CulpritInvoc, nonmember(Port, [fail,leave]) ->
		    printf(debug_output,
			"failure culprit was (%d) - the goal you are currently at",
			[CulpritInvoc])
		;
		    printf(debug_output,
			"failure culprit was (%d) - rerun and type q to jump there",
			[CulpritInvoc])
		),
		interact(Current, Cont)
	    )
	;
	    write(debug_output, "no failure culprit stored yet"),
	    interact(Current, Cont)
	).

do_tracer_command(0'N, _Current, _N, true) :- !,
	confirm("nodebug permanently"),
	trace_mode(5, 0),
	set_flag(debugging, nodebug).

do_tracer_command(0's, Current, N, true) :- !,
	get_goal_stack(Current, _, tf{depth:Depth}),
	writeln(debug_output, "skip"),
	trace_mode(3, Depth),
	store_cmd(0's, N).

do_tracer_command(0'u, Current, _, Cont) :- !,
	get_param_default("scheduled goals with prio", all, Prio),
	write(debug_output, "------ scheduled goals ------"),
	suspensions(Susps),
	print_suspensions(Susps, 1, Prio),
	print_trace_line(Current),
	interact(Current, Cont).

do_tracer_command(0'x, Current, 0, Cont) :- !,
	getval(exec_state, ExecCurrent),
	ExecCurrent = trace_line{frame:Stack},
	Stack = tf{invoc:Invoc},
	get_param_default("examine goal", Invoc, N),
	( find_goal(N, Stack, NewFrame) ->
	    NewCurrent = trace_line{port:'....', frame:NewFrame},
	    print_trace_line(NewCurrent),
	    interact(NewCurrent, Cont)
	;
	    printf(error, "*** Goal (%d) not available!%b", [N]),
	    interact(Current, Cont)
	).

do_tracer_command(0'v, Current, _N, Cont) :- !,
	confirm("var/term spy"),
	current_term(Current, Term, Module),
	new_invoc(I),
	printf(debug_output, "Var/term spy set up with invocation number (%d)", [I]),
	suspend(monitor_term(I, Term, Module, Susp), 1, Term->constrained, Susp),
	interact(Current, Cont).

do_tracer_command(0'w, Current, N0, Cont) :- !,
        writeln(debug_output, "write source lines"),
        (N0 == 0 -> N = 4 ; N = N0), % 4 is default
        Current = trace_line{frame:tf{path:File,line:Line}},
        ( File \== '' ->
            ( write_n_lines_around_current(File, Line, N) ->
                true
            ;
                printf(debug_output, "Unable to find source lines in %w.%n",
                   [File])
            )
        ;
            writeln(debug_output, "No source information.")
        ),
        interact(Current, Cont).

do_tracer_command(0'=, Current, _N0, Cont) :- !,
        Current = trace_line{frame:tf{path:File,line:Line}},
        ( File \== '' ->
            writeln(debug_output, "Source position:"),
	    printf(debug_output, "%w:%w%n", [File,Line])
        ;
            writeln(debug_output, "No source information.")
        ),
        interact(Current, Cont).

do_tracer_command(0'z, Current, _N, true) :- !,
	get_goal_stack(Current, ThisPort, _),
	printf(debug_output, "zap to port: [%w] %b", [~(ThisPort)]),
	block((
	    	read_port_list(debug_input, Ports),
		( var(Ports) -> Ports = ~(ThisPort) ; true ),
		configure_prefilter(_, _, Ports, _, dontcare)
	    ), abort, fail).

do_tracer_command(0'<, Current, _, Cont) :- !,
	getval(dbg_print_depth, N0),
	get_param_default("set print_depth", N0, N),
	N > 0,
%	set_flag(print_depth, N),
	setval(dbg_print_depth, N),
	update_format_strings,
	print_trace_line(Current),
	interact(Current, Cont).

do_tracer_command(0'>, Current, _, Cont) :- !,
	get_param("set indent step width", N),
	setval(indent_step, N),
	print_trace_line(Current),
	interact(Current, Cont).

do_tracer_command(0'+, Current, _N, Cont) :- !,
	writeln(debug_output, "spy"),
	get_goal_stack(Current, _, Frame),
	Frame = tf{goal:Goal},
	functor(Goal, F, A),
	get_tf_prop(Frame, module, DM),
	block(set_flag(F/A, spy, on)@DM, abort, true ) ,
	print_trace_line(Current),
	interact(Current, Cont).

do_tracer_command(0'-, Current, _N, Cont) :- !,
	writeln(debug_output, "nospy"),
	get_goal_stack(Current, _, Frame),
	Frame = tf{goal:Goal},
	functor(Goal, F, A),
	get_tf_prop(Frame, module, DM),
	block(set_flag(F/A, spy, off)@DM, abort, true ) ,
	print_trace_line(Current),
	interact(Current, Cont).

do_tracer_command(0'&, Current, _N, Cont) :- !,
	get_flag(extension, development),
	writeln(debug_output, "Fake stack:"),
	getval(exec_state, trace_line{frame:Stack}),
	print_trace_stack(Stack),
	interact(Current, Cont).

do_tracer_command(0'*, Current, _N, Cont) :- !,
	get_flag(extension, development),
	writeln(debug_output, "True stack:"),
	current_td(Stack),
	print_trace_stack(Stack),
	interact(Current, Cont).

do_tracer_command(0'!, Current, _N, Cont) :- !,
	get_flag(extension, development),
	trace_mode(13, []),	% abstract instruction tracing on/off
	interact(Current, Cont).

do_tracer_command(0'p, Current, _N, Cont) :- !,
	nl(debug_output),
	( Current = inspect{path:Pos, written:Written, module:M} ->
	    reverse(Pos, RPos), reverse(Written, RWritten),
	    print_inspect_path(RPos, RWritten, M),
	    flush(debug_output)
	  ; writeln(debug_output, "Not inspecting subterm.")
        ),
	interact(Current, Cont).

do_tracer_command(0'., Current, _N, Cont) :- 
	Current = inspect{written:[CurrentTerm|_], module:M}, !,
	writeln(debug_output, "structure definition:"),
	(compound(CurrentTerm) ->
	    (named_structure(CurrentTerm, M, Defs, A) -> 
	        print_struct_names(1, A, debug_output, Defs),
		nl(debug_output)
	    ;
		functor(CurrentTerm, F, A),
		printf(debug_output, "No struct definition for term %w/%w@%w.\n", [F,A,M])
            )

          ; writeln(debug_output, "Current subterm not compound term.")
        ),
	interact(Current, Cont).

do_tracer_command(0'., Current, _N, Cont) :- 
	Current = trace_line{frame:Frame}, !,
	Frame = tf{goal:G,module:M},
	nonvar(G),
	functor(G, N, A),
	atom(N),
	nl(debug_output),
	print_source(debug_output, N/A, M),
	interact(Current, Cont).

do_tracer_command(0'B, Frame, N, Cont) :- !, % move down
	N1 is max(1, N), % default is 1
	get_inspect_frame(Frame, Frame1), 
	move_down(N1, Frame1, Frame2),
	interact(Frame2, Cont).

do_tracer_command(0'C, Frame, N, Cont) :-  !, % move right
	writeln(debug_output, "right subterm"),
	N1 is max(1, N), % default is 1
	get_inspect_frame(Frame, Frame1),
	move_right(N1, Frame1, Frame2),
	interact(Frame2, Cont).

do_tracer_command(0'D, Frame, N, Cont) :-  !, % move left
	writeln(debug_output, "left subterm"),
	N1 is max(1, N), % default is 1
	get_inspect_frame(Frame, Frame1),
	move_left(N1, Frame1, Frame2),
	interact(Frame2, Cont).

do_tracer_command(0'A, Frame, N, Cont) :- !,		% move up
	writeln(debug_output, "up subterm"),
	N1 is max(1, N), % default is 1
	get_inspect_frame(Frame, Frame1),
	move_up(N1, Frame1, Frame2),
	interact(Frame2, Cont).

do_tracer_command(13, Frame, N, Cont) :- 
	Frame = inspect{},
	!,
	nl(debug_output),
	get_inspect_frame(Frame, Frame1),
	inspect_subterm(N, Frame1, Frame2),
	interact(Frame2, Cont).

do_tracer_command(0'#, Frame, _, Cont) :- !, 
	get_param("inspect arg #", N),
	get_inspect_frame(Frame, Frame1),
	inspect_subterm(N, Frame1, Frame2),
	interact(Frame2, Cont).

do_tracer_command(0'h, Current, N, Cont) :- !,
	do_tracer_command(0'?, Current, N, Cont).
do_tracer_command(0'?, Current, _N, Cont) :- !,
	writeln(debug_output, "\n\n\
Continue execution:\n\
    [N]c	creep [N times]\n\
       <cr>	creep [once]\n\
       i[N]	jump to invocation number N (default: current)\n\
       j[N]	jump to level N (default: parent)\n\
    [N]l	leap to spypoint [N times]\n\
       n	nodebug (continue with tracer off)\n\
       q	jump to the most recent failure's culprit\n\
    [N]s	skip subgoal [N times]\n\
       v	var (really: term) modification skip\n\
       z	zap to port\n\
\n\
Modify execution:\n\
       a	abort\n\
       f	fail here\n\
       f[N]	fail goal with invocation number N\n\
\n\
Print data:\n\
       d[N]	print delayed goals [of priority N]\n\
       G	print ancestors (call stack)\n\
       u[N]	print scheduled goals [of priority N]\n\
       .	print predicate source or structure definition\n\
       =	print source file name and line number for current goal\n\
    [N]w        print +/-N surrounding source lines for current goal\n\
\n\
Navigate/inspect:\n\
       g   	goto ancestor goal (caller)\n\
       x[N]	examine goal with invoc N (default: back to current port)\n\
       0	move to top of inspected term\n\
       #	move to top of inspected term\n\
       #[N]	move down to Nth argument\n\
       N<cr>	move down to Nth argument\n\
    [N]<up>     move up [N times] (alternative: A)\n\
    [N]<left>   move left [N times] (alternative: D)\n\
    [N]<right>  move right [N times] (alternative: C)\n\
    [N]<down>   move down default arg. [N times] (alternative: B)\n\
       p        show inspection path\n\
\n\
Setting options:\n\
       m	display the caller module\n\
       o	change print options\n\
       <[N]	set print_depth to N\n\
       >[N]	set indentation step width to N\n\
       +	set spy point on displayed predicate\n\
       -	remove spy point from displayed predicate\n\
\n\
Other:\n\
       b	break level\n\
       h,?	help\n\
       N	tracer off permanently\n\
"),
	interact(Current, Cont).

%----------------------------------------------------------------------
% Auxiliary
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


% read a number and the next non-numeric character
% the number get echoed, the terminator not

tyi_num(Stream, Number, Terminator) :-
	tyi_num(Stream, 0, Number, Terminator).

    tyi_num(Stream, Num0, Num, Terminator) :-
	tyi(Stream, Char),
	( char_num(Char, Digit) ->
	    Num1 is 10*Num0 + Digit,
	    tyo(debug_output, Char),
	    tyi_num(Stream, Num1, Num, Terminator)
	; backspace(Char) ->
	    ( Num0 > 0 ->
		Num1 is Num0//10,
		write(debug_output, "\b \b"), flush(debug_output),
		tyi_num(Stream, Num1, Num, Terminator)
	    ;
		tyi_num(Stream, Num0, Num, Terminator)
	    )
	; newline(Char) ->
	    Num = Num0, Terminator = Char
	;
	    Num = Num0, Terminator = Char
	).

    backspace(8).
    backspace(127).

    newline(13).
    newline(10).

    char_num(0'0, 0).
    char_num(0'1, 1).
    char_num(0'2, 2).
    char_num(0'3, 3).
    char_num(0'4, 4).
    char_num(0'5, 5).
    char_num(0'6, 6).
    char_num(0'7, 7).
    char_num(0'8, 8).
    char_num(0'9, 9).


confirm(Prompt) :-
	printf(debug_output, "%s? [y] %b", [Prompt]),
	tyi(debug_input, Char),
	( backspace(Char) -> fail
	; newline(Char) -> nl(debug_output)
	; Char = 0'y -> nl(debug_output)
	; Char = 0'Y -> nl(debug_output)
	; Char = 0'n -> fail
	; Char = 0'N -> fail
	; nl(debug_output), confirm(Prompt) ).

get_param_default(Prompt, Default, N) :-
	printf(debug_output, "%s: [%w]? %b", [Prompt,Default]),
	tyi_num(debug_input, 0, N1, Char),
	newline(Char),				% may fail
	nl(debug_output),
	( N1 = 0 -> N=Default ; N=N1 ).

get_param(Prompt, N) :-
	printf(debug_output, "%s: %b", [Prompt]),
	tyi_num(debug_input, 0, N, Char),
	newline(Char),				% may fail
	nl(debug_output).

clear_cmd :-
	setval(next_cmd, 0).

store_cmd(_Cmd, 0) :- !.
store_cmd(_Cmd, 1) :- !,
	clear_cmd.
store_cmd(Cmd, N) :-
	N1 is N-1,
	setval(next_cmd, N1-Cmd).


current_term(trace_line{frame:
		tf{goal:Term,module:Module}}, Term, Module).
current_term(inspect{written:[Term|_],module:Module}, Term, Module).


print_trace_stack(0).
print_trace_stack(Frame) :-
	Frame = tf{invoc:Invoc,goal:Goal,depth:D,parent:Parent},
	( get_tf_prop(Frame, skip, on) -> Prop = 0'S ; Prop = 0'  ),
	( get_tf_prop(Frame, spy, on) -> Spied = 0'+ ; Spied = 0'  ),
	get_tf_prop(Frame, ?, FF),
	printf(debug_output, ">> [%2r] %c%c(%d) %d ", [FF,Prop,Spied,Invoc,D]),
	getval(dbg_goal_format_string, Format),
	printf(debug_output, Format, Goal),
	nl(debug_output),
	print_trace_stack(Parent).

% returns the goal stack from both trace_line and inspect frames
get_goal_stack(trace_line{port:Port,frame:Stack}, Port, Stack) :- !.
get_goal_stack(inspect{goalf:Stack}, Port, Stack) :- 
	Port = '....'.


break :-
	( current_module(toplevel) ->
	    toplevel:break
	;
	    writeln(warning_output, "No toplevel in this configuration")
	).


%----------------------------------------------------------------------
% Inspect subterms
%----------------------------------------------------------------------

get_inspect_frame(trace_line{frame:Frame}, New) ?- !,
	Frame = tf{goal:Goal,module:Module},
	written_term(Goal, Goal, WGoal, Module),
	New = inspect{top:Goal,path:[],module:Module,written:[WGoal],type:goal,goalf:Frame}.
get_inspect_frame(Frame, Frame). % the default case, placed last

inspect_subterm(0, inspect{top:Top,module:Module,type:Type,goalf:Tf}, Frame) ?- !,
% N == 0 jump to top-level
	written_term(Top, Top, WTop, Module),
	Frame = inspect{top:Top,written:[WTop],path:[],module:Module,type:Type,goalf:Tf},
	print_trace_line(Frame).

inspect_subterm(Choice, inspect{top:Top,type:Type,path:Pos0,module:Module,written:Written0,goalf:Tf}, Frame) :-
	Written0 = [CurrentTerm|_],
	meta(CurrentTerm), Choice> 0, !,
	(block(get_attribute(CurrentTerm,RawAttribute,Choice), _, fail) ->
	    meta_attributes(Atts),
	    member([AttName|Choice], Atts),
	    Pos1 = [AttName-Choice|Pos0],
	    written_term(Top, RawAttribute, Attribute, Module),
	    Written = [Attribute|Written0]

	;   printf(debug_output, "%nInvalid attribute.%n", []),
	    Pos1 = Pos0, Written = Written0
        ),
	Frame = inspect{top:Top,type:Type,module:Module,path:Pos1,written:Written,goalf:Tf},
	print_trace_line(Frame).

 
inspect_subterm(N, inspect{top:Top,type:Type,path:Pos,module:Module,written:Written,goalf:Tf}, Frame) :-
	N > 0, !,  % get Nth arg
	Written = [CurrentTerm|_],
	(nonvar(CurrentTerm),
	functor(CurrentTerm, _F, A),
	N =< A ->
	   arg(N, CurrentTerm, RawNewTerm),
           written_term(Top, RawNewTerm, NewTerm, Module), 
           % print transformed term just in case printf_with_current_mode
           % does not print RawNewTerm as expected
           Pos1 = [N|Pos], Written1 = [NewTerm|Written]  
         ; write(debug_output, 'Out of range.....'),
           nl(debug_output),
           Pos1 = Pos, Written1 = Written
        ),
	Frame = inspect{top:Top,module:Module,path:Pos1,
	   written:Written1,type:Type,goalf:Tf},
	print_trace_line(Frame).


move_down(N, inspect{path:Pos,top:Top,written:Written,module:Module,type:Type,goalf:Tf}, Frame) :-
	current_pos(Pos, CPos),
	traverse_down(N, 0, CPos, Top, Pos, Written, Type, Tf, Frame, Module).


traverse_down(N, N, CPos, Top, Pos, Written, Type, Tf, Frame, Module) :- !,
	printf(debug_output, "down subterm %d for %d levels%n", [CPos,N]),
        Frame = inspect{top:Top,path:Pos,written:Written,module:Module,type:Type,goalf:Tf},
        print_trace_line(Frame).
traverse_down(N, M, CPos, Top, Pos, Written0, Type, Tf, Frame, Module) :-
	M1 is M + 1,
	Written0 = [CurrentTerm|_],
	(nonvar(CurrentTerm),
	 functor(CurrentTerm, _, A),
	 CPos =< A ->
	    arg(CPos, CurrentTerm, RawNewTerm),
	    written_term(Top, RawNewTerm, NewTerm, Module),
	    traverse_down(N, M1, CPos, Top, [CPos|Pos], [NewTerm|Written0], Type, Tf, Frame, Module)
          ; printf(debug_output, "Out of range after traversing down argument %d for %d levels%n", [CPos, M]),
            Frame = inspect{top:Top,module:Module,path:Pos,written:Written0,type:Type,goalf:Tf},
	    print_trace_line(Frame)
        ).


move_up(N, inspect{top:Top,module:Module,written:Written0,path:Pos,
   type:Type,goalf:Gf}, Frame) :-
	port_remove_levels(N, Pos, Pos1, _), 
	reverse(Pos1, RPos), reverse(Written0, RWritten0), 
	port_get_new_subterm(RPos, RWritten0, WrittenFront, []),
	Frame = inspect{top:Top,module:Module,written:WrittenFront,
	  type:Type,path:Pos1,goalf:Gf},
	print_trace_line(Frame).

move_left(M, inspect{top:Top,written:Written0,path:Pos,module:Module,
   type:Type,goalf:Gf}, Frame) :-
	move_path_left(M, Pos, Pos1, N1, Status),
	(Status \== false ->
	      Written0 = [_|Written1],
	      Written1 = [ParentTerm|_],
	      arg(N1, ParentTerm, RawNewTerm),
	      written_term(Top, RawNewTerm, NewTerm, Module),
	      Written0 = [_|Written1], Written2 = [NewTerm|Written1]

	    ; nl(debug_output), writeln(debug_output, 'Out of range.....'),
	      nl(debug_output),
	      Written2 = Written0 
        ),
	Frame = inspect{top:Top,written:Written2,path:Pos1,
           type:Type,module:Module,goalf:Gf},
	print_trace_line(Frame).

move_path_left(M, Pos, Pos1, N1, Status) :-
	(Pos = [N|Pos0], % move to N-M
	 integer(N) ->
	      (N > M -> 
		  N1 is N-M, Status = true
	        ; N1 is 1, Status = out
              ),
	      Pos1 = [N1|Pos0]

	    ; Status = false,
	      Pos1 = Pos
	).

move_right(M, inspect{top:Top,written:Written0,path:Pos,module:Module,
   type:Type,goalf:Gf}, Frame) :-
	(Pos = [N|Pos0], integer(N) -> % move to N+1
	    Written0 = [_|Written1],
	    Written1 = [ParentTerm|_],
	    functor(ParentTerm, _F, A),
	    N1 is min(N+M, A),
	    arg(N1, ParentTerm, RawNewTerm),
	    written_term(Top, RawNewTerm, NewTerm, Module),
	    Written2 = [NewTerm|Written1],
	    Pos1 = [N1|Pos0]

	; % Pos == [] or attributed var.
           nl(debug_output), writeln(debug_output, 'Out of range.....'),
	   nl(debug_output),
	   Pos1 = Pos, Written2 = Written0
        ),
	Frame = inspect{top:Top,written:Written2,path:Pos1,module:Module,
	   type:Type,goalf:Gf},
	print_trace_line(Frame).


/* inspect subterm aux **************************/

port_get_new_subterm([], [Term|_], [Term|Front], Front) :- !.
port_get_new_subterm([_N|Pos], [Term|Written], Acc, Front) :-
    port_get_new_subterm(Pos, Written, Acc, [Term|Front]).


% extracting current position from path
current_pos([], 1) :- !.
current_pos([_-_|_], 1) :- !. % attributed variable
current_pos([CPos|_], CPos).

print_current_summary(Stream, Term, _) :- meta(Term), !,
	printf(Stream, "(attributes  ", []),
	valid_attributes_listing(Term, ValidAttsL),
	(foreach(Spec, ValidAttsL) do 
            printf(debug_output, "%s ", Spec)
	),
	put(Stream, 0')).
print_current_summary(Stream, [_|_], _Module) ?- !,
	printf(Stream, "(list  1-head 2-tail)", []).
print_current_summary(Stream, Term, Module) :- compound(Term), !,
	functor(Term, F, A),
	functor(Defs, F, A),
	( current_struct(F, Defs)@Module ->
	    printf(Stream, "(struct %w/%w)", [F,A])
	;
	    printf(Stream, "(%w/%w)", [F,A])
	).
print_current_summary(Stream, Term, _Module) :-
	type_of(Term, Type),
	( Type = goal -> printf(Stream, "(suspension)", [])
	; printf(Stream, "(%w)", [Type]) ).


print_inspect_path(Path, [Top|Written], Mod) :-
	write(debug_output, "Subterm path: "), 
	print_inspect_path1(Path, Written, Top, Mod).

print_inspect_path1([], [], _Parent, _Mod) :-  !, % at top
	writeln(debug_output, "at toplevel").
print_inspect_path1([Pos|Path], [T|Written], Parent, Mod) :-
	print_one_position(Pos, Parent, Mod),
	(Path \== [] -> 
	    write(debug_output, ","),
	    print_inspect_path1(Path, Written, T, Mod)
	;   nl(debug_output)
        ).

print_one_position(Attr-_, _, _) :-
	printf(debug_output, " attr: %w", [Attr]).
print_one_position(Pos, T, Mod) :-
	(compound(T) ->
	    (named_structure(T, Mod, Def, _) ->
		arg(Pos, Def, Field),
		functor(T, F, _),
		printf(debug_output, " %w of %w (%w)", [Field, F, Pos])
	    ;   printf(debug_output, " %w", [Pos])
	    )

	; printf(" %w", [Pos])
        ).


%----------------------------------------------------------------------
% Print source
%----------------------------------------------------------------------

write_n_lines_around_current(File, CurrentLN, N) :-
        get_file_info(File, readable, on),
        open(File, read, S),
        printf(debug_output, "%w:%n", [File]),
	( 
	    FirstLN is max(CurrentLN - N,1),
	    ( for(_,2,FirstLN), param(S) do
		read_string(S, end_of_line, _, _)
	    ),
	    ( for(I,FirstLN,max(CurrentLN-1,1)), param(S) do
		read_string(S, end_of_line, _, Line),
		printf(debug_output, "%5d  %w%n", [I, Line])
	    ),
	    read_string(S, end_of_line, _, CurrentLine),
	    printf(debug_output, "%5d> %w%n", [CurrentLN, CurrentLine]),
	    ( ( for(I,CurrentLN+1,CurrentLN+N), param(S) do
		% read_string may fail due to end of file
		read_string(S, end_of_line, _, Line),
		printf(debug_output, "%5d  %w%n", [I, Line])
	      ) ->
		true
	    ;
		true
	    ),
	    close(S)
        ;
            close(S),
            fail
	).


%----------------------------------------------------------------------
% Changing output mode
%----------------------------------------------------------------------

change_output_mode :-
	get_flag(output_mode, Mode),
	repeat,
	printf(debug_output, 'current output mode is "%w", toggle char: %b', [Mode]),
	string_list(Mode, ModeList),
	tyi(debug_input, Opt),
	tyo(debug_output, Opt),
	( valid_output_option(Opt, _, ExcludedOpts) ->
	    subtract(ModeList, ExcludedOpts, CleanModeList),
	    ( delete(Opt, CleanModeList, NewModeList) -> true
	    ; NewModeList = [Opt|CleanModeList] ),
	    string_list(NewMode0, NewModeList),
	    set_flag(output_mode, NewMode0),
	    get_flag(output_mode, NewMode)
	; newline(Opt) ->
	    NewMode = Mode
	;
	    printf(debug_output, "%nValid output modes are:%n", []),
	    valid_output_option(Char, Descr, _),
	    printf(debug_output, " %c  %s%n", [Char,Descr]),
	    fail
	),
	!,	% repeat
	printf(debug_output, '%nnew output mode is "%w".%n', [NewMode]),
	update_format_strings.



read_port_list(Stream, Ports) :-
	read_string(Stream, end_of_line, _, String),
	( String = "" ->
	    true
	;
	    ( substring(String,"~",1) -> String1 = String
	    ; concat_string(["[",String,"]"], String1) ),
	    term_string(Ports, String1)
	).

update_format_strings :- 
	get_flag(output_mode, OM),
	getval(dbg_print_depth, PD),
	concat_string(["%",PD,OM,"w"], DF),
	setval(dbg_format_string, DF),
	concat_string(["%",PD,OM,"Gw"], DGF),
	setval(dbg_goal_format_string, DGF).


%--------------------------------------------------------
% Handling of the interruption to abort, debug, exit ...
% This is complicated now because of the parallel case.
%--------------------------------------------------------

:- export interrupt_prolog/0.
:- skipped interrupt_prolog/0.
interrupt_prolog :-
	setval(control_c_option, _),
	mutex(control_c_lock, prompt_for_option(Option, Worker)),
	do_option(Option, Worker).

prompt_for_option(Option, TypeInWorker) :-
	getval(control_c_option, X),
	( var(X) ->
	    nl,
	    ask_option(Option),
	    get_flag(worker, TypeInWorker),
	    setval(control_c_option, Option-TypeInWorker)
	;
	    X = Option-TypeInWorker	% already typed in other worker
	).
	
% move printing of interrupt message to warning_output and receive option
% on input, as debug_input and debug_output may be used differently by
% user's application. Kish Shen 2000-8-11
ask_option(Option) :-
	repeat,
	nl(debug_output),
	write(warning_output, 'interruption: type '),
	write_options,
	write(warning_output, 'or h for help : ? '),
	flush(warning_output),
	tyi(input, AnyCase),
	lower_case(AnyCase, Option),
	option_message(Option, Error, Message),
	writeln(warning_output, Message),
	(Error = (help) -> help_debug ; true),
	Error = valid,	% repeat if 'help' or 'invalid'
	!.		% quit loop if valid

lower_case(Case, LowerCase) :-
	(Case >= 0'a ->
	    LowerCase = Case
	;
	    LowerCase is Case + (0'a - 0'A)
	).

current_option(0'a, valid, abort).
current_option(0'b, valid, 'break level').
current_option(0'c, valid, continue).
current_option(0'd, Error, Message) :-
	( get_flag(worker, 0) ->		% sequential
	    (get_flag(debugging, nodebug) ->
		Error = invalid,
		Message = 'debugger is off'
	    ;
		Error = valid,
		Message = 'switch debugger to creep mode'
	    )
	;
	    Error = invalid,
	    Message = 'not available in parallel execution'
	).
current_option(0'e, valid, exit).
current_option(0'h, help, help).

option_message(Option, Error, Message) :-
	current_option(Option, Error, Message), !.
option_message(_, invalid, 'invalid option').

% Option handling:
%	abort	- on all workers
%	break	- on one worker
%	debug	- sequential only
%	cont	- on all workers
%	exit	- on one worker
do_option(0'a, _) :-
	abort.
do_option(0'b, Worker) :- (get_flag(worker, Worker) -> break ; true).
do_option(0'c, _).
do_option(0'd, 0) :-
        clear_cmd,  % clear any existing command
	trace_mode(0, []).
do_option(0'e, Worker) :- (get_flag(worker, Worker) -> halt ; true).

write_options :-
	current_option(Option, valid, _),
	printf(warning_output, '%c, ', Option),
	fail.
write_options.

help_debug :-
	current_option(Option, valid, Message),
	printf(debug_output, '	%c : %w\n', [Option, Message]),
	fail.
help_debug :-
	writeln(debug_output, '	h : help\n'),
	flush(debug_output).


%----------------------------------------------------------------------
% Init global settings
%----------------------------------------------------------------------

:-	setval(next_cmd, 0),
	setval(indent_step, 0),
	setval(dbg_print_depth, 5),
	setval(show_module, off),
	update_format_strings.

:- local variable(control_c_lock).
:- mutex_init(control_c_lock).
:- local variable(control_c_option).


% Interrupt handlers

try_set_interrupt_handler(I, H) :-
	current_interrupt(_, I) -> set_interrupt_handler(I, H) ; true.

:- import reset/0 from sepia_kernel.

:- export
	it_reset/1,
	it_handler/1,
	it_overflow/0.

it_reset(Sig) :-
	it_handler(Sig),
	reset.

it_handler(Sig):-
	printf(error, "Signal %d%n%b", [Sig]).

it_overflow:-
	write(error, "Segmentation violation - possible reasons are:\n"
	    "- a faulty external C function\n"
	    "- certain operations on circular terms\n"
	    "- machine stack overflow\n"
	    "- an internal error in ECLiPSe\n"
	),
	flush(error),
	reset.

:- get_flag(hostarch, "i386_nt") ->
	% Handle interrupt at least synchronously
	set_interrupt_handler(int, event/1),
	set_event_handler(int, interrupt_prolog/0)
   ;
	% Keyboard interrupt
	set_interrupt_handler(int, interrupt_prolog/0),

	( peer(X), peer_get_property(X,type,embed) ->
	    % If we are embedded, don't touch the handlers
	    true
	;
	    % Standalone: try to catch as much as possible
	    try_set_interrupt_handler(hup, halt/0),
	    try_set_interrupt_handler(quit, halt/0),
	    try_set_interrupt_handler(abrt, halt/0),
	    try_set_interrupt_handler(ill, it_reset/1),
	    try_set_interrupt_handler(trap, it_handler/1),
	    try_set_interrupt_handler(iot, it_handler/1),
	    try_set_interrupt_handler(emt, it_handler/1),
	    try_set_interrupt_handler(fpe, it_reset/1),
	    try_set_interrupt_handler(bus, it_reset/1),
	    try_set_interrupt_handler(segv, it_overflow/0),
	    try_set_interrupt_handler(sys, it_handler/1),
	    try_set_interrupt_handler(pipe, it_handler/1),
	    try_set_interrupt_handler(term, abort/0),
	    try_set_interrupt_handler(urg, it_handler/1),
	    try_set_interrupt_handler(ttou, true/0)
	).

