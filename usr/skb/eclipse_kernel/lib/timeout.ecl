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
% Copyright (C) 2003-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
%
% Description:  Timeout library
%
% System:       ECLiPSe Constraint Logic Programming System
% Author/s:     Andrew Cheadle, IC-Parc
%               Joachim Schimpf, IC-Parc
% Version:      $Id: timeout.ecl,v 1.1 2008/06/30 17:43:50 jschimpf Exp $
%
% ----------------------------------------------------------------------

% ----------------------------------------------------------------------
% Module definition, library dependencies and predicate import / export
% ----------------------------------------------------------------------

:- module(timeout).

:- comment(summary, "Impose time limit for goals").
:- comment(author, "Andrew Cheadle, Joachim Schimpf, IC-Parc").
:- comment(date, "$Date: 2008/06/30 17:43:50 $").
:- comment(copyright, "Cisco Systems, Inc").

:- import request_fail_event/3, timestamp_init/2, timestamp_update/2 from sepia_kernel.

:- export timeout/3, timeout/7, call_timeout_safe/1.

:- tool(timeout/3, timeout_body/4).
:- tool(timeout/7, timeout_body/8).
:- tool(call_timeout_safe/1, call_timeout_safe_body/2).


% ----------------------------------------------------------------------
% Module local variables
% ----------------------------------------------------------------------

% Semaphore used for locking of (nested) 'call_timeout_safe' goals
:- local reference(timeout_safe_semaphore, 0).

% Copy of timer handle for event raising
:-local variable(timer_ball).

% ----------------------------------------------------------------------
% Nested Timeout implementation
% ----------------------------------------------------------------------

:- comment(timeout/3, [
    amode:timeout(+, ++, +),
    args:["Goal":"Goal to run", 
          "TimeLimit":"Time limit of Goal in seconds (integer or float)",
          "TimeOutGoal":"Goal to run on expiry of TimeLimit"],
    summary: "Run the goal Goal for a maximum of TimeLimit seconds.",
    resat:   "If Goal is resatisfiable.",
    desc: html("
<P>
    Goal is executed as if called via call(Goal),
    but only for a maximum of TimeLimit seconds. If the goal is
    still executing after TimeLimit, time-out occurs, the execution of the
    goal is terminated (via exit_block/1) and TimeOutGoal is executed.  If
    the value of TimeLimit is 0 or 1.0Inf, no timeout is applied to the Goal.
<P>
    Note that, if Goal is nondeterministic, execution flow may leave the scope
    of timeout/3 on success and re-enter on failure. In this case, only time
    spent within Goal will be counted towards the TimeLimit.
<P>
    The predicate is based on the after event timers, so TimeLimit is 
    measured in the timer currently used by after events. The current time
    used for the associated event timer can be retrieved using
    statistics(event_time, CurrentTime). The timeout predicate can be used 
    with other after events, and can be nested within itself (i.e. embedded
    within Goal or TimeOutGoal).
<P>
    Note that timeout/3 can be defined in terms of timeout/7 as:
<PRE>
    timeout(Goal, TimeLimit, TimeOutGoal) :-
        timeout(Goal, TimeLimit, TimeOutGoal, all_solution, _, _, _).
</PRE>
    "),
    see_also:[timeout/7, call_timeout_safe/1, event_after/2],
    eg: "\
    ?- timeout((repeat,fail), 1.5, writeln(timed-out)). % time-out from infinite loop
    timed - out
    Yes (1.51s cpu)
    "
    ]).

timeout_body(Goal, TimeLimit, TimeOutGoal, Module) :-
        timeout_body(Goal, TimeLimit, 
                     TimeOutGoal, all_solution, _, _, _, Module).


:- comment(timeout/7, [
    amode:timeout(+, ++, +, ++, ?, ?, ?),
    args:["Goal":"Goal to run",
          "TimeLimit":"Time limit of Goal in seconds (integer or float)",
          "TimeOutGoal":"Goal to run on expiry of TimeLimit",
          "SolutionMode":"Time limit applies to all solutions or per solution (atom)",
          "Timer":"After event timer handle", 
          "DueTime":"Time at which TimeLimit expires (float)",
          "TimeRemaining":"Time remaining until goal would have expired (float)"],
    summary: "Run the goal Goal for a maximum of TimeLimit seconds.",
    resat:   "If Goal is resatisfiable.",
    desc: html("
<P>
    Goal is executed for a maximum of TimeLimit seconds. If the goal is still
    executing after TimeLimit, time-out occurs, the execution of the goal is
    terminated (via exit_block/1) and TimeOutGoal is executed.  If the value
    of TimeLimit is 0 or 1.0Inf, no timeout is applied to the Goal.
<P>
    SolutionMode is one of all_solution or per_solution.  If Goal is
    resatisfiable, then, having found a solution, the per_solution
    option reapplies the full value of the originally specified
    TimeLimit to the resuming Goal.  The all_solution option resumes
    the Goal with the unelapsed portion of TimeLimit (i.e.  its
    remainder) as the expiry time, the expiry time therefore remains
    the same.
<P>
    Timer is the after-event handle assigned to this timeout goal. It
    may be used within Goal to force early expiry of the timeout using:
    event(Timer). 
<P>
    DueTime is the time at which TimeLimit expires and the TimeOutGoal is 
    posted. It is instantiated before Goal starts executing and may therefore 
    be used within Goal and TimeOutGoal. DueTime is undefined (and remains
    uninstantiated) if the per_solution SolutionMode is specified.
<P>
    On success, TimeRemaining is the time left until TimeLimit expiry,
    if Goal completes before then, on timeout, TimeRemaining is 0.0.
<P>
    The predicate is based on the after event timers, so TimeLimit is 
    measured in the timer currently used by after events. The current time
    used for the associated event timer can be retrieved using
    statistics(event_time, CurrentTime). The timeout predicate can be used 
    with other after events, and can be nested within itself (i.e. embedded
    within Goal or TimeOutGoal). Within Goal, the remaining time that the Goal
    has left to run before time-out can be computed using:
<PRE>
    RemainingTime is max(0.0, DueTime - statistics(event_time)).
</PRE>
    This may be useful for setting up sub-timeouts as a fraction of the
    total remaining time.
    "),
    see_also:[timeout/3, call_timeout_safe/1, event_after/2, event/1],
    eg: "\
    % time-out from infinite loop
    ?- timeout((repeat,fail), 1.5, writeln(timed-out), 
            all_solution, Timer, Due, Remainder).
    timed - out
    Timer = 'EVENT'(16'ed920978)
    Due = Due
    Remainder = 0.0
    Yes (1.51s cpu)
    "
    ]).

timeout_body(Goal, TimeLimit, TimeOutGoal, 
             Mode, Timer, DueTime, TimeRemaining, Module) :-
        ( Mode == all_solution ->
            DueTime = ActualDueTime
        ; Mode == per_solution ->
	    true
	;
	    error(5, timeout(Goal, TimeLimit, TimeOutGoal, Mode, Timer, DueTime, TimeRemaining))
        ),
	( (TimeLimit =:= 0 ; TimeLimit =:= 1.0Inf) ->
            ActualTimeLimit = 0
        ;
            ActualTimeLimit = TimeLimit
        ),
	event_create(timeout_handler(Timer), [defers], Timer),
        sepia_kernel:block_atomic(call_with_timeout(Goal, ActualTimeLimit, Mode,
			                            Module, Timer, 
                                                    ActualTimeRemaining, ActualDueTime),
                                  Tag,
                                  catch_timeout(Tag, Timer, 
                                                TimeOutGoal, Module, ActualTimeRemaining)
                                 ),
	( ActualTimeLimit = 0 ->
	    % in this case ActualTimeRemaining is wrong (0) because
	    % we didn't actually start a timer.
	    TimeRemaining = 1.0Inf
	;
	    TimeRemaining = ActualTimeRemaining
	).


call_with_timeout(Goal, MaxTime, Mode, Module, Timer, TimeRemaining, ActualDueTime) :- 
	( Mode = per_solution ->
	    Restart = restart_timer(Timer, MaxTime)
	;
	    Restart = restart_timer(Timer, TimeRemaining)
	),
	Stop = stop_timer(Timer, TimeRemaining),
	Start = start_timer(Timer, MaxTime, ActualDueTime),
	sepia_kernel:call_boxed(Goal, Start, Stop, Restart, Stop, Module).


% Here 0 means no timeout!
start_timer(Timer, Timeout, DueTime) :-
	( Timeout =:= 0 ->
	    true		% 0 means no timeout
	;
	    event_after(Timer, Timeout, DueTime)
	).

% Here 0 means timeout immediately!
restart_timer(Timer, Timeout) :-
        event_after(Timer, Timeout).


stop_timer(Timer, TimeRemaining) :-
        cancel_after_event(Timer, CancelledEventList),
	( CancelledEventList = [Timer-TimeRemaining|_] ->
	    true
	;
            TimeRemaining = 0.0
        ).


%
% Discussion of race conditions: Assume two nested timeouts expire almost
% simultaneously. There are two cases:
%
% Inner handled first: 
%	Outer handler is deferred until we are about to call the inner's
%	timeout-recovery goal, which is then preempted by the outer handler.
%	Effect as if the inner timeout hadn't occurred. Ok.
% Outer handled first: 
%	Inner handler is deferred until we are about to call the outer's
%	timeout-recovery goal. But first we go through the inner's catch
%	for foreign timouts, which disables the already posted inner event.
%	Effect as if the inner timeout hadn't occurred. Ok.
%
% We handle the case where a timeout occurs when we have just
% started to process an unrelated exit_block inside the timeout-goal by using
% block_atomic/3 instead of block/3.
% In the case of block/3, the timeout handler may interrupt between immediately
% on entering catch_timeout(other_tag,...) and before event_disable(Timer).
% It will then execute an exit_block(timeout_ball_thrown) which is now
% already outside the scope of its block and therefore not caught.
% This is fixed by using a variant of block/3, block_atomic/3 which defers
% event handling automatically on entering the catch-goal.
%

catch_timeout(timeout_ball_thrown, Timer, TimeOutGoal, Module, TimeRemaining) :-
        !,
        getval(timer_ball, TimerCopy),
        ( Timer = TimerCopy ->
%	    writeln(catch_timeout_final(Timer)),
	    % Timed out, call the TimeOutGoal
	    TimeRemaining = 0.0,
            setval(timer_ball, _),
%	    writeln(calling_events_nodefer),
	    sepia_kernel:events_nodefer,
            call(TimeOutGoal)@Module
        ;
%	    writeln(catch_timeout_pass(Timer)),
	    % Other timer timed out, stop this one
	    stop_timer(Timer, _TimeRemaining),
	    % Disable event in case Timer expired before we could stop it
	    event_disable(Timer),
	    % Exit further, leaving event handling deferred
            exit_block(timeout_ball_thrown)
        ).
catch_timeout(Tag, Timer, _TimeOutGoal, _Module, _TimeRemaining) :-
        % TimeOutGoal aborted for other reasons
        stop_timer(Timer, _TimeRemaining),
	event_disable(Timer),
	sepia_kernel:events_nodefer,
        exit_block(Tag).


% Event handling is deferred when entering this handler
% Make sure it gets re-enabled in all cases!
timeout_handler(Timer) :-
        getval(timeout_safe_semaphore, SemValue),
        ( SemValue = 0 ->
%	    writeln(timeout_handler_0(Timer)),
            setval(timer_ball, Timer),
            exit_block(timeout_ball_thrown)
        ;
%	    writeln(timeout_handler_1),
            recordz(deferred_timeouts, Timer),
	    sepia_kernel:events_nodefer
        ).


% ----------------------------------------------------------------------
% call_timeout_safe metacall implementation
%     Defers execution of a timeout/n TimeoutGoal during the execution
%     of the metacalled goal
% ----------------------------------------------------------------------

:- comment(call_timeout_safe/1, [
    amode:call_timeout_safe(+),
    args:["Goal":"Goal to execute"],
    summary: "Execute Goal, deferring the execution of an interrupting"
             " timeout predicate TimeOutGoal until Goal completes.",
    resat:   "If Goal is resatisfiable.",
    desc: html("
<P>
    Metacall Goal, ensuring that its execution is not interrupted and
    preemptively aborted by a timeout of the timeout/3 or timeout/7 predicate.
    If a timeout would have occurred during the execution of Goal, it is
    effectively deferred until Goal completes.
<P>
    The predicate can be used within a computation which is subject to timeouts,
    to ensure that subgoals which should be executed as an atomic sequence
    preserve this atomicity at the cost of delaying the execution
    of TimeOutGoal.
<P>
    In all other respects the semantics of call_timeout_safe/1 is the same
    as call/1.
    "),
    see_also:[timeout/3, timeout/7],
    eg: "\
    ?- timeout(call_timeout_safe((writeln('Goal started'), 
                               sleep(5), 
                               writeln('Goal completed'))),
            3, writeln('Timeout expired!')).
    Goal started
    Goal completed
    Timeout expired!
    Yes (0.01s cpu)
    "
    ]).


call_timeout_safe_body(Goal, Module) :-
	( getval(timeout_safe_semaphore, 0) ->
	    setval(timeout_safe_semaphore, 1),		% trailed
	    sepia_kernel:call_boxed(Goal,
		    true, true, true, raise_delayed_timeout_events, Module),
	    setval(timeout_safe_semaphore, 0),		% trailed
	    raise_delayed_timeout_events
	;
	    call(Goal)@Module
	).


raise_delayed_timeout_events :-
	( getval(timeout_safe_semaphore, 0) ->
	    recorded_list(deferred_timeouts, Timers),
	    erase_all(deferred_timeouts),
	    event(Timers)
	;
	    % The semaphore may be set!  We could have been failing
	    % out of one call_timeout_safe and into the scope of another!
	    true
	).

