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
% Version:	$Id: tracer.pl,v 1.2 2008/07/08 22:31:23 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe II debugger -- Port generation
%
% $Id: tracer.pl,v 1.2 2008/07/08 22:31:23 jschimpf Exp $
%
% Author:	Joachim Schimpf, IC-Parc
%

/*
ECLiPSe II debugger

The engine notifies the debugger only at the following points:
    
    call(OldStack, NewStack)
    wake(OldStack, NewStack)
    exit(Stack)
    redo(Stack, FailDrop, RedoLevel, Which, ShowNext)
    delay(256, make_suspension(Goal,P,S,M))
    
These points are synchronous in the execution, so we can easily insert
Prolog execution there.

The handler then generates ports from notifications and calls port/2
for each port. Note that because of the mismatch between notifications
and ports, the artificial ports normally cannot be displayed with
arguments because the engine is already in a different state (e.g
FAIL REDO).

Ports are filtered with of_interest/5 and pre-filtered on engine level:

    ==invoc &&  minlevel=<level=<maxlevel && 
    ( (SPIED|TRACEABLE & tracemode) || tracemode=leap && at_breakpoint)
tracemode,invoc,minlevel,maxlevel can be set via 
trace_mode/2.

*/

:- begin_module(sepia_kernel).
:- pragma(nodebug).
:- pragma(system).
:- pragma(noskip).

:- export
	struct(ports(call,exit,'*exit',redo,fail,	% enum, really
		     resume,leave,delay,next,unify,spyterm,modify,else)),
        % tf must correspond to definition in emu_export.h!
	struct(tf(invoc,goal,depth,chp,parent,proc,prio,path,line,from,to,module)),
	struct(trace_line(port,frame)).

:- export
	spy_var/1,
	spy_term/2.

:- export
	new_invoc/1,	% in C
	current_td/1,	% in C
	failure_culprit/2, % in C
	monitor_term/4,
	trace_mode/2,
	find_goal/3,
	get_tf_prop/3,
	debug_port_names/1,
	configure_prefilter/5.

%diagnostics(N) :- nl, writeln(N).
diagnostics(_).

%----------------------------------------------------------------------
% Port generation from notifications
%----------------------------------------------------------------------

% Call and resume notification handler

ncall(OldStack, NewStack) :-
	call_or_wake(OldStack, NewStack, call of ports).

resume(OldStack, NewStack) :-
	call_or_wake(OldStack, NewStack, resume of ports).

    call_or_wake(OldStack, NewStack, Port) :-
	disable_tracing,
	get_priority(P), % Don't wake anything
	set_priority(1),
	diagnostics(ncall(NewStack, OldStack)),
	CurrentB = chp(_),
	timestamp_update(CurrentB, 1),

	( NewStack = tf{parent:Parent} ->		% call port
	    ( OldStack == Parent ->
		raise_init_event % if necessary
	    ;
		trace_exit(OldStack, CurrentB)
	    ),
	    port(Port, NewStack)
	;						% exit port
	    trace_exit(OldStack, CurrentB)
	),
	diagnostics(ncall-done),
	!, set_priority(P), cont_debug.


% Exit notification handler

nexit(Stack) :-
	disable_tracing,
	get_priority(P), % Don't wake anything
	set_priority(1),
	diagnostics(nexit(Stack)),
	CurrentB = chp(_),
	timestamp_update(CurrentB, 1),

	trace_exit(Stack, CurrentB),
	!, cont_debug, set_priority(P).


    trace_exit(Frame, NewB) :-
	( timestamp_older(Frame, chp of tf, NewB, 1) ->
	    port('*exit' of ports, Frame)
	;
	    port(exit of ports, Frame)
	).


% Redo notification handler, called after the failure happened.
% Stack:	the current (restored) stack after the failure
% FailDrop:	how many levels failed (use get_fail_info/2 to get details)
% RedoLevel:	at which level the failure was caught, ie the youngest
%		common ancestor of the failed and the redone goal.
% FailLeave:	fail port or leave port
% ShowNext:	1 if the predicate with the choice point is debuggable,
%		which means that the NEXT-port should be shown.

redo(Stack, FailDrop, RedoLevel, FailLeave, ShowNext) :-
	disable_tracing,
	get_priority(P), % Don't wake anything
	set_priority(1),
	diagnostics(redo(Stack, FailDrop, RedoLevel, FailLeave, ShowNext)),
	trace_fails_redos(Stack, RedoLevel, FailDrop, ShowNext, FailLeave),
	!, set_priority(P), cont_debug,
	% FAIL port: fail for correct state restoration from choicepoint
	% LEAVE port: succeed for state restoration from aux. environment
	FailLeave == (leave of ports).


trace_fails_redos(0, RedoLevel, FailDrop, _ShowNext, FailLeave) :-
	trace_failures(FailDrop, RedoLevel, 0, FailLeave).
trace_fails_redos(Current, RedoLevel, FailDrop, ShowNext, FailLeave) :-
	Current = tf{depth:Depth},
	( Depth > RedoLevel ->
	    trace_fails_redos1(Current, RedoLevel, FailDrop)
	; Depth = RedoLevel ->
	    trace_failures(FailDrop, RedoLevel, Current, FailLeave),
	    ( ShowNext == 0 -> true ; port(ShowNext, Current) )
	;
	    trace_failures(FailDrop, RedoLevel, Current, FailLeave)
	).

    trace_fails_redos1(0, _RedoLevel, _FailDrop).
    trace_fails_redos1(Current, RedoLevel, FailDrop) :-
	Current = tf{depth:Depth,parent:Parent},
	( Depth > RedoLevel ->
	    trace_fails_redos1(Parent, RedoLevel, FailDrop),
	    port(redo of ports, Current)
	; % Depth = RedoLevel
	    trace_failures(FailDrop, RedoLevel, Current, fail of ports)
	).

    trace_failures(0, _Depth, _Stack, _FailLeave) :- !.
    trace_failures(I, Depth, Stack, FailLeave) :-
	I1 is I-1,
	Depth1 is Depth+1,
	get_fail_info(I1, FakeStack),
	( FakeStack \== [] ->
	    % get_fail_info/2 does not fill in depth and parent
	    FakeStack = tf{depth:Depth1,parent:Stack},
	    trace_failures(I1, Depth1, FakeStack, FailLeave),
	    port(FailLeave, FakeStack)
	; % fail info not recorded, ignore
	    trace_failures(I1, Depth1, Stack, FailLeave)
	).




% Delay notification handler for make_suspension/3,4
% This is currently a bit funny, because it is implemented as an
% error handler for make_suspension/4, and it is raised after the
% suspension has been created, but before it has been unified with S.
% That's why the latter has to be done here in the handler.

ndelay(_, MakeSuspension) :-
	disable_tracing,
	get_priority(P), % Don't wake anything
	set_priority(1),
	current_td(Parent),
	extract_suspension(MakeSuspension, S),
	last_suspension(S),	% unify S
	diagnostics(ndelay(MakeSuspension)),
	Parent = tf{depth:D},
	D1 is D+1,
	trace_delays(Parent, D1, [S]),
	!, set_priority(P), cont_debug.

    extract_suspension(make_suspension(_,_,S), S).
    extract_suspension(make_suspension(_,_,S,_), S).


% Delay notification handler for suspensions created inside externals.
% It is the external predicate's responsibility to raise the DEBUG_SUSP_EVENT
% if any of the suspensions created within it need to be traced.

bip_delay :-
	disable_tracing,
	get_priority(P), % Don't wake anything
	set_priority(1),
	delay_port_susps(Susps), % get a list of new, traceable suspensions
	diagnostics(bip_delay(Susps)),
	current_td(Parent),
	Parent = tf{depth:D},
	D1 is D+1,
	trace_delays(Parent, D1, Susps),
	!, set_priority(P), cont_debug.

    trace_delays(_, _, []).
    trace_delays(Parent, Depth, [S|Susps]) :-
	susp_to_tf(S, Stack),
	Stack = tf{depth:Depth,parent:Parent},
	port((delay) of ports, Stack),
	trace_delays(Parent, Depth, Susps).

:- set_flag(bip_delay/0, invisible, on).


% Tracing of inline-compiled builtins like +/3, arg/3, =/2, ...
% Done via exception-events raised by the debug_call_simple and
% debug_exit_simple instructions.
% These handlers are executed under priority 1 because of exception mechanism.

:- export bip_call/0.
:- set_flag(bip_call/0, invisible, on).
bip_call :-
	% CALL port, frame already pushed
	% If we had the OldStack, we'd call ncall(OldStack,TD)
	current_td(TD),
	( TD = tf{parent:Parent} ->		% call port
	    ncall(Parent,TD)
	;
	    writeln(error, "Illegal state in bip_call handler - ignored"),
	    cont_debug
	).

:- export bip_exit/0.
:- set_flag(bip_exit/0, invisible, on).
bip_exit :-
	disable_tracing,
	current_td(Stack),
	port(exit of ports, Stack),
	pop_tf,
	cont_debug.

/* might be needed if we re-introduce shallow choicepoints
bip_fail :-
	disable_tracing,
	current_td(Stack),
	port(fail of ports, Stack),
	pop_tf,
	!,
	cont_debug.
*/


% Builtins for generating user-defined debugger ports

:- export trace_call_port/3.
:- tool(trace_call_port/3, trace_call_port/4).
:- set_flag(trace_call_port/3, invisible, on).
:- set_flag(trace_call_port/4, invisible, on).
trace_call_port(Port, Invoc, Goal0, M) :-
	( integer(Invoc) ; var(Invoc) ), !,
	( tracing ->
	    disable_tracing,
	    get_priority(P),			% Don't wake anything
	    set_priority(1),
	    lookup_module(Goal0, M, Goal, LM),
	    make_tf(1, Invoc, Goal, M, LM, P, Stack),	% push frame
	    port_name_to_number(Port, PortNr),
	    port(PortNr, Stack),
	    !,
	    set_priority(P),
	    cont_debug
	;
	    true
	).

    :- mode lookup_module(+,+,-,-).
    lookup_module(LM0:G0, _, G, LM) ?- G = G0, LM = LM0.
    lookup_module(G, M, G, M).

:- export trace_point_port/3.
:- tool(trace_point_port/3, trace_point_port/4).
:- set_flag(trace_point_port/3, invisible, on).
:- set_flag(trace_point_port/4, invisible, on).
trace_point_port(Port, Invoc, Goal0, M) :-
	( integer(Invoc) ; var(Invoc) ), !,
	( tracing ->
	    trace_point_port_unchecked(Port, Invoc, Goal0, M)
	;
	    true
	).

trace_point_port_unchecked(Port, Invoc, Goal0, M) :-
	    disable_tracing,
	    get_priority(P),			% Don't wake anything
	    set_priority(1),
	    lookup_module(Goal0, M, Goal, LM),
	    make_tf(0, Invoc, Goal, M, LM, P, Stack),	% temporary frame
	    port_name_to_number(Port, PortNr),
	    port(PortNr, Stack),
	    !,
	    set_priority(P),
	    cont_debug.

:- export trace_exit_port/0.
:- set_flag(trace_exit_port/0, invisible, on).
trace_exit_port :-
	( tracing ->
	    disable_tracing,
	    get_priority(P),			% Don't wake anything
	    set_priority(1),
	    current_td(Stack),
	    ( Stack = tf{} ->
		CurrentB = chp(_),
		timestamp_update(CurrentB, 1),
		trace_exit(Stack, CurrentB),
		pop_tf
	    ;
		true	% no parent to exit
	    ),
	    !,
	    set_priority(P),
	    cont_debug
	;
	    true
	).

:- export trace_parent_port/1.
:- set_flag(trace_parent_port/1, invisible, on).
trace_parent_port(Port) :-
	( tracing ->
	    disable_tracing,
	    get_priority(P),			% Don't wake anything
	    set_priority(1),
	    current_td(Stack),			% use parent frame
	    ( Stack = tf{} ->
		port_name_to_number(Port, PortNr),
		port(PortNr, Stack)
	    ;
		true	% no parent
	    ),
	    !,
	    set_priority(P),
	    cont_debug
	;
	    true
	).


% A simple term-spy implementation

:- tool(spy_var/1, spy_var/2).
:- set_flag(spy_var/1, invisible, on).
:- set_flag(spy_var/2, invisible, on).
spy_var(Var, M) :-
	( tracing ->
	    spy_term(Var, Var->constrained, M)
	;
	    true
	).


:- tool(spy_term/2, spy_term/3).
:- set_flag(spy_term/2, invisible, on).
:- set_flag(spy_term/3, invisible, on).
spy_term(Term, Cond, Module) :-
	( tracing ->
	    disable_tracing,
	    suspend(monitor_term(I, Term, Module, Susp), 1, Cond, Susp),
	    trace_point_port_unchecked(spyterm, I, Term, Module)
	;
	    true
	).

:- demon monitor_term/4.
:- set_flag(monitor_term/4, invisible, on).
monitor_term(Invoc, Term, Module, Susp) :-
	( nonground(Term) -> true ; kill_suspension(Susp) ),
	trace_point_port_unchecked(modify, Invoc, Term, Module).
%monitor_term(Invoc, Term, Module, _Susp) :-
%	trace_point_port_unchecked(unmod, Invoc, Term, Module),
%	fail.


%----------------------------------------------------------------------
% Port filtering
% PortNr can be an integer (index of a built-in port) or an atom
%----------------------------------------------------------------------

port(PortNr, Stack) :-
	Stack = tf{invoc:Invoc,depth:Depth,proc:Proc},
	get_tf_prop(Stack, break, BrkPt),
%	diagnostics( of_interest(PortNr, Invoc, Depth, Proc, BrkPt)),
	( of_interest(PortNr, Invoc, Depth, Proc, BrkPt) ->
	    port_name(PortNr, Port),
	    Current = trace_line{port:Port,frame:Stack},
	    % This handler is allowed to cut_to, fail and abort
	    error(252, Current)		% trace line event
	;
%	    diagnostics(no_interest),
	    true
	).


configure_prefilter(Invoc, Depth, Ports, Preds, Module) :-
	decode_range(Invoc, MinInvoc, MaxInvoc),
	decode_range(Depth, MinDepth, MaxDepth),
	port_spec_to_mask(Ports, 0, PortMask),
	diagnostics(portMask=PortMask),
%	nospy(_),
	( Preds == spied -> LeapFlag = 1
	; Preds == all -> LeapFlag = 0
	; set_spypoints(Preds, Module, LeapFlag)
	),
	!,
	trace_mode(6, MinDepth),
	trace_mode(7, MaxDepth),
	trace_mode(8, MinInvoc),
	trace_mode(9, MaxInvoc),
	trace_mode(5, PortMask),
	trace_mode(11, LeapFlag).
configure_prefilter(Invoc, Depth, Ports, Preds, Module) :-
	error(6, configure_prefilter(Invoc, Depth, Ports, Preds, Module)).

    decode_range(N, 0, Max) :- var(N), !, maxint(Max).
    decode_range(N, N, N) :- integer(N).
    decode_range(=(N), N, N).
    decode_range(..(Min,Max), Min, Max).
    decode_range(Min-Max, Min, Max).
    decode_range(>(L), Min, Max) :- Min is L+1, maxint(Max).
    decode_range(<(H), 0, Max) :- Max is H-1.
    decode_range(=<(Max), 0, Max).
    decode_range(>=(Min), Min, Max) :- maxint(Max).

    :- mode port_spec_to_mask(?, +, -).
    port_spec_to_mask(Var, Mask0, Mask) :- var(Var), !,
	Mask is Mask0 \/ any_port_mask.
    port_spec_to_mask([], Mask, Mask) :- !.
    port_spec_to_mask(List, Mask0, Mask) :- List = [_|_],
	port_list_to_mask(List, Mask0, Mask).
    port_spec_to_mask(~Ps, Mask0, Mask) :-
	Mask is Mask0 \/ any_port_mask /\ \port_spec_to_mask(Ps, 0).
    port_spec_to_mask(P, Mask0, Mask) :- atom(P),
	Mask is Mask0 \/ port_name_to_mask_bit(P).

    :- mode port_list_to_mask(?, +, -).
    port_list_to_mask([], Mask, Mask).
    port_list_to_mask([P|Ps], Mask0, Mask) :-
	atom(P),
	Mask1 is Mask0 \/ port_name_to_mask_bit(P),
	port_list_to_mask(Ps, Mask1, Mask).

    :- mode set_spypoints(?, +, -).
    set_spypoints(Var, _Module, 0) :- var(Var), !.
    set_spypoints([], _Module, 0) :- !.
    set_spypoints([P|Ps], Module, 1) :- !,
	set_spypoint(P, Module),
	set_spypoints(Ps, Module, _).
    set_spypoints(P, Module, 1) :-
	set_spypoint(P, Module).

    set_spypoint(Module:N/A, _) ?-
    	spy(N/A)@Module.
    set_spypoint(N/A, Module) ?-
    	spy(N/A)@Module.


%----------------------------------------------------------------------
% Auxiliary
%----------------------------------------------------------------------

port_name(I, Name) :-
	integer(I),
	arg(I, ports{
	    call:call,
	    exit:exit,
	    '*exit':'*exit',
	    redo:redo,
	    fail:fail,
	    resume:resume,
	    leave:leave,
	    (delay):(delay),
	    next:next,
	    unify:unify,
	    spyterm:spyterm,
	    modify:modify,
	    else:else},
	Name).
port_name(I, Name) :-
	atom(I), I = Name.

:- mode debug_port_names(-).
debug_port_names(Names) :-
	Names = [call,
	         exit,
		 '*exit',
		 redo,
		 fail,
		 resume,
		 leave,
		 (delay),
		 next,
		 unify,
		 spyterm,
		 modify,
		 else].

any_port_mask(2'1111111111111111).

:- mode port_name_to_mask_bit(+,-).
port_name_to_mask_bit(call,	2'0000000000000001) :- !.
port_name_to_mask_bit(exit,	2'0000000000000010) :- !.
port_name_to_mask_bit('*exit',	2'0000000000000100) :- !.
port_name_to_mask_bit(redo,	2'0000000000001000) :- !.
port_name_to_mask_bit(fail,	2'0000000000010000) :- !.
port_name_to_mask_bit(resume,	2'0000000000100000) :- !.
port_name_to_mask_bit(leave,	2'0000000001000000) :- !.
port_name_to_mask_bit((delay),	2'0000000010000000) :- !.
port_name_to_mask_bit(next,	2'0000000100000000) :- !.
port_name_to_mask_bit(unify,	2'0000001000000000) :- !.
port_name_to_mask_bit(spyterm,	2'0000010000000000) :- !.
port_name_to_mask_bit(modify,	2'0000100000000000) :- !.
port_name_to_mask_bit(else,	2'0001000000000000) :- !.
port_name_to_mask_bit(_other,	2'1000000000000000).

:- mode port_name_to_number(+,-).
port_name_to_number(call,	1) :- !.
port_name_to_number(exit,	2) :- !.
port_name_to_number('*exit',	3) :- !.
port_name_to_number(redo,	4) :- !.
port_name_to_number(fail,	5) :- !.
port_name_to_number(resume,	6) :- !.
port_name_to_number(leave,	7) :- !.
port_name_to_number((delay),	8) :- !.
port_name_to_number(next,	9) :- !.
port_name_to_number(unify,	10) :- !.
port_name_to_number(spyterm,	11) :- !.
port_name_to_number(modify,	12) :- !.
port_name_to_number(else,	13) :- !.
port_name_to_number(Other,	Other).


find_goal(Invoc, Stack, Frame) :-
	find_ancestor(Invoc, Stack, Frame), !.
find_goal(Invoc, _Stack, Frame) :-
    	suspensions(Susps),
	find_susp_with_invoc(Invoc, Susps, Frame).

    find_ancestor(Invoc, Frame, Found) :-
    	Frame = tf{invoc:I,parent:Parent},		% may fail
	( I =:= Invoc ->
	    Frame = Found
	;
	    find_ancestor(Invoc, Parent, Found)
	).

    find_susp_with_invoc(Invoc, [S|Susps], Frame) :-
	( get_suspension_data(S, invoc, Invoc) ->
	    susp_to_tf(S, Frame),
	    Frame = tf{depth:0,parent:0}
	;
	    find_susp_with_invoc(Invoc, Susps, Frame)
	).


%----------------------------------------------------------------------
% Settings
%----------------------------------------------------------------------

:- set_default_error_handler(253, ncall/2), reset_error_handler(253).
:- set_default_error_handler(254, nexit/1), reset_error_handler(254).
:- set_default_error_handler(255, redo/5), reset_error_handler(255).
:- set_default_error_handler(256, ndelay/2), reset_error_handler(256).
:- set_default_error_handler(257, resume/2), reset_error_handler(257).
:- set_default_error_handler(258, bip_call/0), reset_error_handler(258).
:- set_default_error_handler(259, bip_exit/0), reset_error_handler(259).
%:- set_default_error_handler(251, bip_fail/0), reset_error_handler(251).
:- set_default_error_handler(249, bip_delay/0), reset_error_handler(249).

