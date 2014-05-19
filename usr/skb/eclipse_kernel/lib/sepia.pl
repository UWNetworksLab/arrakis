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
% Copyright (C) 1992-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: sepia.pl,v 1.3 2008/08/20 17:48:13 jschimpf Exp $
% ----------------------------------------------------------------------

%--------------------------------------------------------------------
%
% SEPIA PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	sepia.pl
%
% DESCRIPTION:		SEPIA backward compatibility package
%
%

:- module(sepia).
:- system.		% compiler directive to add the SYSTEM flag

:- comment(summary, "A number of obsolete SEPIA built-ins").
:- comment(date, "$Date: 2008/08/20 17:48:13 $").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(status, deprecated).

:- export
	(abolish)/2,
	array/3,
	bsi/0,
	compiled_file/2,
	cprolog/0,
	current_array/3,
	define_global_macro/3,
	define_local_macro/3,
	(delay)/2,
	err_msg/2,
	get_error_handler/2,
	get_interrupt_handler/2,
	interrupt/1,
	interval_alarm/1,
	is_module/1,
	is_protected/1,
	load/2,
	local_op/3,
	ls/0,
	ls/1,
	no_delayed_goals/0,
	predicate_property/1,
	quintus/0,
	rtoken/1,
	rtoken/2,
	sicstus/0,
	wm_get/2,
	wm_set/2.

:- export
	abolish_body/3,
	array_body/4,
	bsi/1,
	cprolog/1,
	current_array_body/4,
	define_global_macro_/4,
	is_protected_/2,
	quintus/1,
	rtoken_body/2,
	rtoken_body/3,
	sicstus/1.

:- import
	abolish_body/2,
	current_array_body/3,
	define_macro_/4,
	get_flag_body/4,
	listing_body/1,
	listing_body/2,
	local_op_body/4,
	pred_body/2,
	use_module_body/2,
	read_token_/4
   from sepia_kernel.

:- export
	op(500, fx, -),
	op(500, fx, +),
	op(200, fx, \).

%--------------------------------------------------------------------
% interval_alarm/1 superseded by set_timer/2
%--------------------------------------------------------------------

interval_alarm(Seconds) :-
	set_timer(real, Seconds).


%--------------------------------------------------------------------
% rtoken/1,2 superseded by read_token/3
%--------------------------------------------------------------------

:- tool(rtoken/1, rtoken_body/2).
:- tool(rtoken/2, rtoken_body/3).

rtoken_body(Token, Module) :-
	read_token_(input, Token, _, Module).

rtoken_body(Stream, Token, Module) :-
	read_token_(Stream, Token, _, Module).

%--------------------------------------------------------------------
% compatibility switches: can now be done module-wise with use_module/1
%--------------------------------------------------------------------

:-	tool(bsi/0, bsi/1),
	tool(cprolog/0, cprolog/1),
	tool(sicstus/0, sicstus/1),
	tool(quintus/0, quintus/1).

bsi(M) :-
	use_module_body(library(bsi), M).

cprolog(M) :-
	use_module_body(library(cprolog), M).

quintus(M) :-
	use_module_body(library(quintus), M).

sicstus(M) :-
	use_module_body(library(sicstus), M).

%--------------------------------------------------------------------
% current_array/3 and array/3 superseded by current_array/2
%--------------------------------------------------------------------

:- tool(current_array/3, current_array_body/4).
:- tool(array/3, array_body/4).

current_array_body(Atom, ListOfBounds, Type, Module) :-
	current_array_body(Array, [Type|_], Module),
	Array =.. [Atom|ListOfBounds].

array_body(ArrSpec, ListOfBounds, Type, Module) :-
	var(ArrSpec), !,
	error(4, array(ArrSpec, ListOfBounds, Type), Module).
array_body(ArrSpec, ListOfBounds, Type, Module) :-
	atom(ArrSpec), !,
	ListOfBounds = [],
	current_array_body(ArrSpec, [Type|_], Module).
array_body(N/A, ListOfBounds, Type, Module) :-
	!,
	functor(Array, N, A),
	Array =.. [_|ListOfBounds],
	current_array_body(Array, [Type|_], Module).
array_body(ArrSpec, ListOfBounds, Type, Module) :-
	error(5, array(ArrSpec, ListOfBounds, Type), Module).

%--------------------------------------------------------------------
% define_local_macro/3, define_global_macro/3 superseded by define_macro/3
%--------------------------------------------------------------------

:- tool(define_local_macro/3, define_macro_/4).
:- tool(define_global_macro/3, define_global_macro_/4).

define_global_macro_(Macro, Trafo, Options, Module) :-
	define_macro_(Macro, Trafo, [global|Options], Module).

%--------------------------------------------------------------------
% local_op/3 is a synonym for op/3
%--------------------------------------------------------------------

:- tool(local_op/3, local_op_body/4).

%--------------------------------------------------------------------
% load/2 - cannot do more than load/1
%--------------------------------------------------------------------

load(Files, Libs) :-
	concat_string([Files, ' ', Libs], Opts),
	load(Opts).

%--------------------------------------------------------------------
% no_delayed_goals/0 - the same achieved with delayed_goals/1
%--------------------------------------------------------------------

no_delayed_goals :-
	delayed_goals([]).

%--------------------------------------------------------------------
% abolish/2 - same functionality as abolish/1
%--------------------------------------------------------------------

:- tool((abolish)/2, abolish_body/3).
abolish_body(Name, Arity, Module):-
	abolish_body(Name/Arity, Module).

%--------------------------------------------------------------------
% ls/0,1 - synonyms for listing/0,1
%--------------------------------------------------------------------

:- tool((ls)/0, listing_body/1).
:- tool((ls)/1, listing_body/2).

%--------------------------------------------------------------------
% is_module/1 does less than current_module/1
%--------------------------------------------------------------------

is_module(M) :- current_module(M).

%--------------------------------------------------------------------
% is_protected/1 superseded by get_flag/3
%--------------------------------------------------------------------

:- tool(is_protected/1, is_protected_/2).
is_protected_(P, M) :- get_flag_body(P, protected, on, M).

%--------------------------------------------------------------------
% get_error_handler/2 - use get_error_handler/3
% get_interrupt_handler/2 - use get_interrupt_handler/3
%--------------------------------------------------------------------

get_error_handler(N, H):-
	get_event_handler(N, H, _).

get_interrupt_handler(N, H):-
	get_interrupt_handler(N, H, _).

%--------------------------------------------------------------------
% err_msg/2 - not substantial
% list_error/3 has been moved to library(util)
%--------------------------------------------------------------------

err_msg(X, Where):- 
	error_id(X, Msg), 
	get_flag(output_mode, OM),
	concat_string(["%w in %", OM, "w%n%b"], Format),
	printf(error, Format, [Msg, Where]).

%--------------------------------------------------------------------
% interrupt/1 - about the same as kill/2
%--------------------------------------------------------------------

interrupt(I) :-
	( atom(I) ->
	    current_interrupt(Signal, I)
	;
	    Signal = I
	),
	get_flag(pid, Pid),
	kill(Pid, Signal).

%--------------------------------------------------------------------
% predicate_property/1 - same as pred/1
%--------------------------------------------------------------------

:- tool(predicate_property/1, pred_body/2).

%--------------------------------------------------------------------
% compiled_file/2 - compiled_stream/1 is more general
%--------------------------------------------------------------------

compiled_file(File, Line) :-
        compiled_stream(Stream),
        get_stream_info(Stream, name, File),
        get_stream_info(Stream, line, Line).

%-------------------------------------------------------------------
% absorbed in get_flag/2 and set_flag/2
%-------------------------------------------------------------------
wm_get(worker,W) :- get_flag(worker,W).
wm_get(workers(Host),Awake+_) :- get_flag(workers,Host:Awake).
wm_get(workerids(Host),[Awake,Asleep]) :-
	get_flag(workerids,Host:Awake+Asleep).
wm_get(window,OnOff) :- get_flag(wm_window,OnOff).

wm_set(Flag,Value) :- (var(Flag);var(Value)) -> error(4,wm_set(Flag,Value))
		      ; wm_set0(Flag,Value).
wm_set0(workers(Host),Awake) :- set_flag(workers,Host:Awake).
wm_set0(window,OnOff)        :- set_flag(wm_window,OnOff).

%----------------------------------------------------------------
% explicit suspension
%----------------------------------------------------------------

:- tool((delay)/2, (delay)/3).

delay(Term, Goal, Module) :-
	suspend(Goal, 2, Term->bound)@Module.

:- untraceable (delay)/3.

%--------------------------------------------------------------------
% set all the tools skipped
%--------------------------------------------------------------------

:- skipped
	(abolish)/2,
	array/3,
	bsi/0,
	compiled_file/2,
	cprolog/0,
	current_array/3,
	define_global_macro/3,
	define_local_macro/3,
	is_protected/1,
	local_op/3,
	ls/0,
	ls/1,
	predicate_property/1,
	quintus/0,
	rtoken/1,
	rtoken/2,
	sicstus/0.
