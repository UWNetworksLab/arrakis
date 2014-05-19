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
% Version:	$Id: events.pl,v 1.5 2008/08/21 18:08:28 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	events.pl
 *
 * DESCRIPTION: 	
 *
 *
 * CONTENTS:     	Event-Related Prolog Procedures and Declarations
 *
 */

:- system.
:- pragma(nodebug).

%------------------------------------
% error/event handling builtins
%------------------------------------

get_error_handler(N, H, M) :-
	(
		H = F/A
	->
		(
			get_error_handler(N, F, A, M)
		->
			true
		;
			bip_error(get_error_handler(N, H, M))
		)
	;
		error(5,get_error_handler(N, H, M))
	).

get_event_handler(N, H, M) :-
	(
		H = F/A
	->
		(
			get_event_handler(N, F, A, M)
		->
			true
		;
			bip_error(get_event_handler(N, H, M))
		)
	;
		error(5,get_event_handler(N, H, M))
	).

current_error(N) :-
	(var(N) ->
		max_error(Max),
		gen_valid_errors(1, Max, N)
	;
	integer(N) ->
		error_id(N, _)
	;
		error(5, current_error(N))
	).

gen_valid_errors(Start, _Max, Start) :-
	error_id(Start, _).
gen_valid_errors(Start, Max, N) :-
	Start < Max,
	New is Start+1,
	gen_valid_errors(New, Max, N).


% The user-definable exit from a non-recoverable error.
error_exit :-
	exit_block(abort).

%-------------------------------------
% Here are the default error handlers
%
% Arguments of error handlers:
%   1	Error		integer or atom (identifies the error)
%   2	Culprit		usually a goal (but sometimes a clause, a N/A, etc)
%   3	ContextModule	context module (if not known, same as lookup module)
%   4	LookupModule	lookup module for the culprit (always a valid module,
%			except for error 86 NO_LOOKUP_MODULE)
%-------------------------------------

no_err_handler(X, Where) :-
	write(error, 'no error handler, module has been erased,'),
	nl(error),
	error_message(X, Where).

error_handler(X, Where) :- 
	error_message(X, Where),
	error(157, _).

:- tool(error_handler/3, error_handler/4).

error_handler(X, Where, CM, LM) :- 
	error_message(X, Where, CM, LM),
	error(157, _).


% avoid loops by recursive calls due to macros
call_handler(X, Where, Module, LM) :- 
	atom(Module),		% The context module is not checked yet,
	is_a_module(Module),	% since this is normally done by the callee!
	!,
	error_id(X, Msg), 
	% First remove 'm' or 'M' from the output flags so that we don't
	% hit undefined 'print attribute' predicates
	output_mode(Mode),
	string_list(Mode, ModeL),
	(member(0'm, ModeL) ->
	    delete(0'm, ModeL, NewModeL)
	;
	member(0'M, ModeL) ->
	    delete(0'M, ModeL, NewModeL)
	;
	    NewModeL = ModeL
	),
	string_list(NewMode, NewModeL),
	% And then disable write macros. This unfortunately also disables
	% goal macros which would not loop anyway...
	concat_string(['%w %', NewMode, 'Tw in module %w%n'], Format),
	( Module == LM -> QualWhere = Where ; QualWhere = LM:Where ),
	printf_body(error, Format, [Msg,QualWhere,Module], Module),
	error(157, _).
call_handler(_, Where, Module, _) :- 
	error(80, Where@Module).


% Handler for error 86 - lookup module does not exist.
% Culprit is an ok goal, but LM is an atom but not a module.
% If there is a library called LM, we try to load it.
no_lookup_module_handler(N, Goal, CM, LM) :- !,
	getval(prolog_suffix, ECLs),
	getval(eclipse_object_suffix, ECO),
	( existing_file(library(LM), [ECO|ECLs], [readable], _) ->
	    printf(warning_output,
	    	"WARNING: module '%w' does not exist, loading library...%n",
		[LM]),
	    ensure_loaded(library(LM))@CM,
	    ( is_a_module(LM) ->
		:@(LM, Goal, CM)
	    ;
		error_handler(N, Goal, CM, LM)
	    )
	;
	    error_handler(N, Goal, CM, LM)
	).


    % suppress these warnings until autoloading is done properly
declaration_warning_handler(_N, _Pred, lists) :- !.
declaration_warning_handler(_N, _Pred, profile) :- !.
declaration_warning_handler(75, Pred, Module) :- !,
	get_flag_body(Pred, definition_module, DM, Module),
	get_deprecation_advice(Pred, DM, Advice),
	!,
	warning_handler(75, Pred, Module),
	printf(warning_output, " Advice: %w%n", [Advice]).
    % suppress the warning if there is such a library
declaration_warning_handler(85, BadModule:_, _Module) :-
	known_library(BadModule),
	!.
declaration_warning_handler(N, Pred, Module) :-
	warning_handler(N, Pred, Module).

    % modules for which we raise no warning 85
    known_library(daVinci) :- !.	% because not in runtime system
    known_library(ic_gap_sbds) :- !.	% because not in runtime system
    known_library(ic_gap_sbdd) :- !.	% because not in runtime system
    known_library(Module) :-
	getval(prolog_suffix, ECLs),
	getval(eclipse_object_suffix, ECO),
	once existing_file(library(Module), [ECO|ECLs], [readable], _).


warning_handler(X, Where) :-
	write(warning_output, 'WARNING: '),
	warning_message(X, Where).

warning_handler(X, Where, Module) :-
	write(warning_output, 'WARNING: '),
	warning_message(X, Where, Module).



undef_array_handler(N, setval_body(Name, Value, Module), _) :- !,
	undef_array_handler(N, setval(Name, Value), Module).
undef_array_handler(N, getval_body(Name, Value, Module), _) :- !,
	undef_array_handler(N, getval(Name, Value), Module).
undef_array_handler(_N, setval(Name, Value), Module) :-
	atom(Name),
	!,
    	( current_module(M), not is_locked(M), current_array(Name, _)@M ->
	    % there's one in another module, probably error
	    printf(warning_output,
	    	"WARNING: creating local variable(%w) in %w while there exists one in %w%n",
		[Name, Module, M])
	;
	    true	% create it silently
	),
	make_array_(Name, prolog, local, Module),
	setval_body(Name, Value, Module).
undef_array_handler(N, Goal, Module) :-
	error_handler(N, Goal, Module).


make_array_handler(42, Culprit, Module, LM) :-
	!,
	make_array_args(Culprit, Array, Type, Visibility),
	( current_array(Array, [Type,Visibility])@Module ->
	    true	% it's the same
	;
	    warning_handler(42, Culprit),
	    functor(Array, N, A),
	    erase_array_(N/A, visible, Module),
	    :@(LM,Culprit,Module)
	).
make_array_handler(N, Culprit, Module, LM) :-
	error_handler(default(N), Culprit, Module, LM).

    make_array_args(make_array(Array, Type), Array, Type, global).
    make_array_args(make_local_array(Array, Type), Array, Type, local).
    make_array_args(local(variable(Array)), Array, prolog, local) :- !.
    make_array_args(local(variable(Array,_)), Array, prolog, local) :- !.
    make_array_args(global(variable(Array)), Array, prolog, global) :- !.
    make_array_args(local(reference(Array)), Array, reference, local) :- !.
    make_array_args(global(reference(Array)), Array, reference, global) :- !.
    make_array_args(local(reference(Array,_)), Array, reference, local) :- !.
    make_array_args(local(array(Array, Type)), Array, Type, local) :- !.
    make_array_args(local(array(Array)), Array, prolog, local) :- !.
    make_array_args(global(array(Array, Type)), Array, Type, global) :- !.
    make_array_args(global(array(Array)), Array, prolog, global) :- !.


undef_record_handler(_N, Culprit) :-
	extract_record_key(Culprit, Key, Module),
	!,
	( current_module(M), not is_locked(M), current_record(Key)@M ->
	    printf(warning_output,
	    	"WARNING: creating local record(%w) in %w while there exists one in %w%n",
		[Key, Module, M])
	;
	    true	% create it silently
	),
	functor(Key, K, A),
	local_record_body(K/A, Module),
	call(Culprit).	% Culprit is a kernel tool body, so call/1 is ok
undef_record_handler(N, Culprit) :-
	error_handler(N, Culprit).

    extract_record_key(recorda_body(Key,_,M), Key, M).
    extract_record_key(recordz_body(Key,_,M), Key, M).
    extract_record_key(recorda_body(Key,_,_,M), Key, M).
    extract_record_key(recordz_body(Key,_,_,M), Key, M).

parser_error_handler(N, Goal):- 
	error_id(N, Id), 
	(extract_stream(Goal, Stream) ->
	    print_error(Stream, Id)
	;
	    error_message(N, Goal)
	),
	fail.

print_error(Stream, Id) :-
	stream_info_(Stream, 13, Type),
	stream_info_(Stream, 6, Where),
	stream_info_(Stream, 5, Line),
	(get_context(Stream, Type, Where, Line, From, String) ->
	    printf(error, "%s\n", Id),
	    printf(error, "| %s\n", String),
	    print_arrow(String, From, Where),
	    seek(Stream, Where),
	    set_stream_prop_(Stream, 5, Line),	% reset the line
	    skip_to_eocl(Stream, Type)
	;
	    printf(error, "%s\n%b", Id)		% no context available
	).

get_context(Stream, file, Err, Line, From, String) :-
	!,
	stream_info_(Stream, 0, Name),
	Back is max(Err - 80, 0),
	seek(Stream, Back),
	stream_info_(Stream, 16, BufSize),
	find_string(Stream, Err, Back, BufSize, String, From, LineDecr),
	LL is Line - LineDecr,
	local_file_name(Name, File),
	printf(error, "file %s, line %d: ", [File, LL]).
get_context(Stream, tty, Err, _, From, String) :-
	!,
	stream_info_(Stream, 14, Buf),
	stream_info_(Stream, 16, BufSize),
	seek(Stream, Buf),
	find_string(Stream, Err, Buf, BufSize, String, From, _).
get_context(Stream, socket, Err, Line, From, String) :-
	!,
	stream_info_(Stream, 0, Name),
	stream_info_(Stream, 16, BufSize),
	seek(Stream, Buf),
	find_string(Stream, Err, Buf, BufSize, String, From, LineDecr),
	LL is Line - LineDecr,
	printf(error, "socket %s, line %d: ", [Name, LL]).
get_context(_Stream, queue, _Err, _Line, _From, _String) :-
	!, fail.	% can't seek on a queue
get_context(Stream, Type, Err, _, From, String) :-
	stream_info_(Stream, 14, Buf),
	seek(Stream, Buf),
	stream_info_(Stream, 16, BufSize),
	find_string(Stream, Err, Buf, BufSize, String, From, _),
	printf(error, "%w stream %w: ", [Type, Stream]).

find_string(Stream, Where, Last, BufSize, String, From, LineDecr) :-
	read_string(Stream, end_of_line, BufSize, S),
	string_length(S, Length),
	Now is Last + Length + 1,
	(Now >= Where ->
	    String = S,
	    From = Last,
	    (Now = Where ->
		LineDecr = 1
	    ;
		LineDecr = 0
	    )
	;
	    find_string(Stream, Where, Now, BufSize, String, From, LineDecr)
	).

print_arrow(String, From, Where) :-
	Num is Where - From - 1,
	string_print_length(String, 2, Num, Skip),
	printf(error, "| %*c^ here\n%b", [Skip, 0' ]).

skip_to_eocl(Stream, _) :-
	at_eof(Stream),
	!.
skip_to_eocl(Stream, tty) :-
	!,
	read_string(Stream, end_of_line, _, _),
	skip_to_eocl(Stream, 0).
skip_to_eocl(Stream, _) :-
	read_token(Stream, _, fullstop),
	!.
skip_to_eocl(Stream, Type) :-
	skip_to_eocl(Stream, Type).

singleton_in_loop(N, Occurrence) :-
	( Occurrence = quantified(Name) ->
	    printf(warning_output,
		"*** Warning: Singleton local variable %a in do-loop (not used in loop body)%n",
		[Name])
	; Occurrence = unquantified(Name) ->
	    printf(warning_output,
		"*** Warning: Singleton local variable %a in do-loop, maybe param(%a) missing?%n",
		[Name,Name])
	;
	    error_handler(N, Occurrence)
	),
	( compiled_file(File, Line) ->
	    printf(warning_output, "\tbefore line %d in file %s%n", [Line, File])
	;
	    true
	),
	flush(warning_output).
	
% extract_stream(Goal, Stream)
:- mode extract_stream(+, -).
extract_stream(read(_), input).
extract_stream(read_(_, _), input).
extract_stream(readvar(S, _, _), S).
extract_stream(readvar(S, _, _, _), S).
extract_stream(read_annotated_raw(S, _, _, _), S).
extract_stream(read_string(_, _, _), input).
extract_stream(read_string(S, _, _, _), S).
extract_stream(read(S, _), S).
extract_stream(read_(S, _, _), S).
extract_stream(read_token(S, _, _), S).
extract_stream(read_token_(S, _, _, _), S).
extract_stream(read_exdr(S, _), S).
extract_stream(compile_stream(S), S).
extract_stream(compile_stream_(S, _), S).
extract_stream(get(_), input).
extract_stream(get(S, _), S).
extract_stream(get0(_), input).
extract_stream(get0(S, _), S).
extract_stream(get_char(_), input).
extract_stream(get_char(S, _), S).
extract_stream(getw(S, _), S).
extract_stream(tyi(_), input).
extract_stream(tyi(S, _), S).
extract_stream(tyo(_), output).
extract_stream(tyo(S, _), S).
extract_stream(flush(S), S).
extract_stream(format(_, _), output).
extract_stream(format(S, _, _), S).
extract_stream(format_body(_, _, _), output).
extract_stream(format_body(S, _, _, _), S).
extract_stream(printf(_, _), output).
extract_stream(printf(S, _, _), S).
extract_stream(printf_body(_, _, _), output).
extract_stream(printf_body(S, _, _, _), S).
extract_stream(write(_), output).
extract_stream(write(S, _), S).
extract_stream(write_(_, _), output).
extract_stream(write_(S, _, _), S).
extract_stream(write_term(S,_,_,_,_,_), S).
extract_stream(writeln_body(_,_), output).
extract_stream(writeln_body(S,_,_), S).
extract_stream(writeln(_), output).
extract_stream(writeln(S,_), S).
extract_stream(nl, output).
extract_stream(nl(S), S).
extract_stream(close(S), S).


% eof_handler/4 - take the appropriate action for each culprit
% CARE: eof_handler/4 fails for other culprits

eof_handler(N, Goal, Module, LM) :-
	extract_stream(Goal, Stream),
	( stream_info_(Stream, 19, on) ->	% yield
	    stream_info_(Stream, 4, PhysicalStream),
	    (is_remote_sync_queue(PhysicalStream, _, ControlStream) ->
		remote_input(PhysicalStream, ControlStream)
	    ;
	    	yield(6, PhysicalStream, _)	% 6 == PWAITIO == EC_waitio
	    ),
	    :@(LM, Goal, Module)
	;
	    eof_handler(N, Goal)
	).

:- mode eof_handler(++, +).
eof_handler(_, read(end_of_file)).
eof_handler(_, read_(end_of_file, _)).
eof_handler(_, read(_, end_of_file)).
eof_handler(_, read_(_, end_of_file, _)).
eof_handler(_, read_exdr(_, _)) :- fail.
eof_handler(_, readvar(_, end_of_file, _)).
eof_handler(_, readvar(_, end_of_file, _, _)).
eof_handler(_, read_token(_, end_of_file, end_of_file)).
eof_handler(_, read_token_(_, end_of_file, end_of_file, _)).
eof_handler(_, read_string(_, _, _)) :- fail.
eof_handler(_, read_string(_, _, _, _)) :- fail.
eof_handler(_, compile_stream(_)).
eof_handler(_, compile_stream_(_,_)).
eof_handler(_, get(-1)).
eof_handler(_, get(_, -1)).
eof_handler(_, get0(-1)).
eof_handler(_, get0(_, -1)).
eof_handler(_, tyi(-1)).
eof_handler(_, tyi(_, -1)).
eof_handler(_, get_char(_)) :- fail.
eof_handler(_, get_char(_, _)) :- fail.
eof_handler(_, getw(_, end_of_file)).
eof_handler(_, read_annotated_raw(S,
	    annotated_term(end_of_file,end_of_file,File,Line,End,End), 0, _)) :-
	stream_info_(S, 0 /*name*/, File),
	stream_info_(S, 5 /*line*/, Line),
	at(S, End).


past_eof_handler(N, Goal) :-
	extract_stream(Goal, Stream),
	close(Stream),
	error_handler(N, Goal).


compiler_warning_handler(N, Proc) :-
	( ( nonvar(Proc), Proc=Term@File:Line
	  ; compiled_file(File, Line), Term=Proc) ->
	    write(error, '\n*** '),
	    error_id(N, M), 
	    write(error, M), 
	    write(error, ': '),
	    printf_with_current_modes(error, Term), 
	    (Line > 0 ->
		    printf(error, "\n\tbefore line %d in the file %s",
			[Line, File])
	    ;
		    true
	    ),
	    nl(error),
	    flush(error)
	;
	    error_handler(N, Proc)
	).

compiler_error_handler(N, Proc) :-
	compiler_warning_handler(N, Proc),
	fail.

compiler_abort_handler(N, File, _Module) :-
	error_id(N, M), 
	printf(error, "\n*** %s", M),
	(compiled_file(File, Line) ->
	    (Line > 0 ->
		    printf(error, "\n\tbefore line %d in the file %s",
			[Line, File])
	    ;
		    true
	    )
	;
	    printf(error, " in the file %s\n", File)
	),
	nl(error),
	flush(error).

pragma_handler(148, pragma(Pragma), Module) :-
	record_pragma(Pragma, Module), !.
pragma_handler(N, Proc, _Module) :-
	compiler_error_handler(N, Proc).


compiled_file_handler(N, (File, Size, Time), Module) :- !,
	compiled_file_handler(N, File, Size, Time, Module).
compiled_file_handler(N, Goal, Module) :-
	error_handler(N, Goal, Module).

compiled_file_handler(_, term, _, _, _) :- !.
compiled_file_handler(_, File, Size, Time, _Module) :-
	( File = source(Source) ->
	    true
	;
	    local_file_name(File, Source)
	),
	( Size < 0 ->
	    printf(log_output, "%-10s loaded in %.2f seconds\n%b",
	    	[Source, Time])
	;
	    printf(log_output, "%-10s compiled %d bytes in %.2f seconds\n%b",
		[Source, Size, Time])
	).


% end of loading a code unit: do any finishing up work
unit_loaded_handler(_, Options, Module) :-
	run_stored_goals(initialization_goals, Module),
	( memberchk(check, Options) ->
	    record(compiled_modules, Module)
	;
	    true
	).


record_compiled_file_handler(_, File-Goal, Module) :-
	canonical_path_name(File, CanonicalFile0),
	( string(CanonicalFile0) ->
	    atom_string(CanonicalFile, CanonicalFile0)
	;
	    CanonicalFile = CanonicalFile0
	),
	record_compiled_file(CanonicalFile, Goal, Module).


local_file_name(File:Line, LocalF:Line) :- !,
	local_file_name(File, LocalF).
local_file_name(File, LocalF) :-
	getcwd(Cwd),
	atom_string(File, FileS),
	(substring(FileS, Cwd, 1) ->
	    Pos is string_length(Cwd) + 1,
	    Len is string_length(FileS) - Pos + 1,
	    first_substring(FileS, Pos, Len, LocalF)
	;
	    LocalF = File
	).

:- export redef_other_file_handler/2.
redef_other_file_handler(_, (Pred, OldFile0, NewFile0)) :-
	local_file_name(OldFile0, OldFile),
	local_file_name(NewFile0, NewFile),
	printf(warning_output, "WARNING: %w in file %w replaces previous definition in file %w%n",
		 [Pred,NewFile,OldFile]).


:- mode library_module(++, -).
library_module(library(File), File) :- !.
library_module(File, File).

error_message(X, Where):- 
	error_id(X, M), 
	write(error, M), 
	write(error, ' in '),
	printf_goal(error, Where), 
	nl(error),
	flush(error).


% What's all these different modules?
% 
% 				CM	LM	TrueLM	UsedLM
% :- module(lm).
% ?- lm1:p(X).			lm	lm	lm1	lm1
% prints "error in lm1:p(X)" using lm1's syntax
%
% :- module(lm).
% :- import p/1 from lm1.
% ?- lm1:p(X).			lm	lm	lm1	lm
% prints "error in p(X)" using lm's syntax
% ?- p(X).			lm	lm	lm	lm
% prints "error in p(X)" in lm's syntax
%
% :- module(lm).
% ?- lm1:p(X)@cm.		cm	lm	lm1	lm1
% prints "error in lm1:p(X) in module cm" using lm1's syntax
%
% :- module(lm).
% :- import p/1 from lm1.
% ?- lm1:p(X)@cm.		cm	lm	lm1	lm
% prints "error in p(X) in module cm" using lm's syntax
% ?- p(X)@cm.			cm	lm	lm	lm
% prints "error in p(X) in module cm" using lm's syntax


error_message(X, Goal, CM, LM):- 
	error_id(X, M),
	write(error, M),
	write(error, ' in '),

	% Strip off any module qualifier to find the true lookup module
	unqualify(Goal, LM, TrueLM, PlainGoal),

	% Add back a qualifier only if predicate not anyway visible in LM
	qualify_goal_if_needed(PlainGoal, LM, TrueLM, QualGoal, UsedLM),

	% Print the goal using the syntax from one of the lookup modules,
	% to make sure we have the relevant goal output transformations.
	% We prefer LM to TrueLM because that might have some user's trans-
	% formations in addition, which may be needed for goal's arguments.
	( is_a_module(UsedLM) ->
	    printf_goal_body(error, QualGoal, UsedLM)
	;
	    printf_goal(error, QualGoal)
	),

	% If we have an interesting context module, print it
	( atom(CM), is_a_module(CM), not is_locked(CM), CM \== LM ->
	    write(error, ' in module '),
	    write(error, CM)
	;
	    true
	),
	nl(error),
	flush(error).


warning_message(X, Where):- 
	error_id(X, M), 
	write(warning_output, M), 
	write(warning_output, ' in '),
	printf_goal(warning_output, Where), 
	nl(warning_output),
	flush(warning_output).

warning_message(X, Where, Module):- 
	error_id(X, M),
	write(warning_output, M),
	write(warning_output, ' in '),
	printf_goal_body(warning_output, Where, Module),
	write(warning_output, ' in module '),
	write(warning_output, Module),
	nl(warning_output),
	flush(warning_output).

/* Finally boot_error/2 can be properly redefined. It is used
 * as error handler when no error handler can be found
 */
boot_error(N, Goal) :-
	write(error, 'no error handler: '),
	( error_message(N, Goal) ->
	    compiled_file(File, Line),
	    (Line > 0 ->
		    printf(error, "\n\tbefore line %d in the file %s",
			[Line, File])
	    ;
		    true
	    ),
	    nl(error),
	    exit0(-1)	% to avoids loops in error 152 in exit/1
	;
	    writeln(error, N)
	).


output_error_handler(X, Culprit, CM, LM):-
	( Culprit = close(_) ->
	    true
	;
	    extract_stream(Culprit, S),
	    close(S)
	),
	system_error_handler(X, Culprit, CM, LM).


/* system_stream(SystemStream,DefaultAssignStream)
 * Order important: stdin,stdout,stderr must be first, so they get redirected
 * first, and then output,etc get redirected to their new values.
 */

:- mode system_stream(?, -).
system_stream(stdin, 0).
system_stream(stdout, 1).
system_stream(stderr, 2).
system_stream(input, S) :- get_stream(stdin, S).
system_stream(output, S) :- get_stream(stdout, S).
system_stream(warning_output, S) :- get_stream(stdout, S).
system_stream(log_output, S) :- get_stream(stdout, S).
system_stream(error, S) :- get_stream(stderr, S).
system_stream(null, 3).

close_handler(_,close(Stream)) :-
        !,
	get_stream(Stream, Num),
        (
	    % reset system streams that are redirected to Stream
            system_stream(SysStream,Standard),
            get_stream(SysStream,Num),
            set_stream(SysStream,Standard),
            fail
        ;
            true
        ),
	(system_stream(SysStream, _), get_stream(SysStream,Num) ->
	    true		% still a system stream: don't close
	;
	    close(Num)
	),
	(atom(Stream), fail_if(system_stream(Stream, _)) ->
	    erase_stream_property(Stream)
	;
	    true
	).
close_handler(ErrorNumber,Goal) :-
        error_handler(ErrorNumber,Goal).

% Currently only used for output goals
io_yield_handler(_, Goal) :-
	extract_stream(Goal, Stream),
	stream_info_(Stream, 4, PhysicalStream),
        % recover memory used during yielding by \+\+
        \+ \+ do_stream_yield(PhysicalStream).

do_stream_yield(PhysicalStream) :-
	(is_remote_sync_queue(PhysicalStream, RemoteStream, ControlStream) ->
	    remote_output(PhysicalStream, ControlStream, RemoteStream)
	;   yield(7, PhysicalStream, _)
	    % 7 == PFLUSHIO == EC_flushio
        ).


% This is the handler for all errors from the operating system.  It has
% special treatment for "Interrupted system call" and will restart certain
% builtins in that case.  The advantage of doing this through the handler
% rather than directly in C is that this gives the system a chance to
% run a synchronous interrupt handler before the goal gets restarted.

system_error_handler(E, Goal, CM, LM):-
	errno_id(Msg),
	( Msg = "Interrupted system call", restartable_builtin(Goal) ->
	    :@(LM, Goal, CM)
	;
	    error_id(E, EclMsg),
	    printf(error, "%w: %w in ", [EclMsg, Msg]),
	    printf_goal(error, Goal), 
	    nl(error),
	    flush(error),
	    error(157, _)
	).
	
    % Builtins that can raise EINTR and can be restarted after that
    restartable_builtin(accept(_,_,_)).
    restartable_builtin(cd(_)).
    restartable_builtin(open(_,_,_)).
    restartable_builtin(close(_)).
    restartable_builtin(connect(_,_)).
    restartable_builtin(select(_,_,_)).
    restartable_builtin(wait(_,_,_)).


dynamic_handler(_, dynamic(Name/Arity), Module) :-
	!,
	functor(F, Name, Arity),
	retract_all_body(F, Module).
dynamic_handler(N, Proc, Module) :-
	error_handler(N, Proc, Module).

macro_handler(N, define_macro(T, P, F), M) :- !,
	macro_handler(N, define_macro_(T, P, F, M), _).
macro_handler(N, define_macro_(T, QP, F, M), _) :-
	unqualify(QP, M, LMnew, P),
	(
	    current_macro_body(T, P, F1, LMold, M),
	    same_macro_flags(F, F1),
	    same_trans_pred(P, LMnew, LMold)
	->
	    true	% don't warn, definitions are the same
	;
	    warning_handler(N, define_macro(T, P, F), M),
	    erase_macro_(T, F, M),
	    define_macro_(T, P, F, M)
	).

    same_macro_flags(A, B) :-
	subtract(A, [local,read,term], A1), sort(A1, NormFlags),
	subtract(B, [local,read,term], B1), sort(B1, NormFlags).

    same_trans_pred(_P, M, M) :- !.
    same_trans_pred(P, M1, M2) :-
	get_flag_body(P, definition_module, DM, M1),
	get_flag_body(P, definition_module, DM, M2).


?- make_array_(autoload_lock, prolog, local, sepia_kernel),
   mutex_init(autoload_lock).

autoload_handler(_, Goal, CallerModule, LookupModule) :-
	get_unqualified_goal(Goal, UnQualGoal),
	mutex_lib(UnQualGoal, CallerModule),
	:@(LookupModule, Goal, CallerModule).

mutex_lib(Goal, CallerModule) :-
	mutex(autoload_lock, (
	    get_autoload_info(Goal, CallerModule, File, HomeModule) ->
		ensure_loaded(library(File), HomeModule)
	    ;
		true	% already loaded (maybe by other worker)
	)).

% fails if predicate is defined in the meantime
get_autoload_info(Goal, CallerModule, HomeModule, HomeModule) :-
	functor(Goal, N, A),
	proc_flags(N/A, 14, off, CallerModule),	% get_flag(N/A, defined, off)
	proc_flags(N/A, 0, HomeModule, CallerModule).


integer_overflow_handler(E, Goal) :-
	Goal =.. [F,X|T],
	( bignum(X, BigX) ->		% convert one arg to bignum if possible
	    NewGoal =.. [F,BigX|T],
	    call(NewGoal)		% redo the operation with bignums
	;
	    error_handler(E, Goal)
	).

% This one is called when an argument of a comparison
% is neither a number nor a free variable.
% The arguments are evaluated and the goal is re-called.

compare_handler(_, Goal, CM, LM) :-
	functor(Goal, F, A),
	arg(1, Goal, X),
	arg(2, Goal, Y),
	( A > 2 ->
	    arg(3, Goal, M),		% for >= 6.0 culprit is tool body
	    functor(NewGoal, F, 2),
	    arg(1, NewGoal, X1),
	    arg(2, NewGoal, Y1)
	;
	    functor(NewGoal, F, A),	% up to 5.10 culprit is tool
	    arg(1, NewGoal, X1),
	    arg(2, NewGoal, Y1),
	    M = CM
	),
	eval(X, X1, M),
	eval(Y, Y1, M),
	( number(X1), number(Y1) ->
	    :@(LM,NewGoal,M)
	; var(X1) ->
	    :@(LM,NewGoal,M)
	; var(Y1) ->
	    :@(LM,NewGoal,M)
	;
	    error(24, NewGoal, M)
	).


% allow certain things even if the module is locked

locked_access_handler(_, unskipped PredSpec) :-
	unskipping_allowed(PredSpec),
	!,
	unskipped PredSpec.
locked_access_handler(_, export PredSpec) :-
	exporting_allowed(PredSpec),
	!,
	export PredSpec.
locked_access_handler(E, Goal) :-
	error_handler(E, Goal).

% allow certain kernel predicates to be made unskipped

unskipping_allowed((is)/2).
unskipping_allowed((>)/2).
unskipping_allowed((<)/2).
unskipping_allowed((>=)/2).
unskipping_allowed((=<)/2).
unskipping_allowed((=:=)/2).
unskipping_allowed((=\=)/2).

% and certain not to be global any longer

exporting_allowed(wake/0).


ambiguous_import_resolve(_, Pred, Module) :-
	preferred_predicate(Pred, M),
	get_module_info(Module, imports, Imports),
	memberchk(M, Imports),
	(import Pred from M) @ Module.

ambiguous_import_warn(_, Pred, Module) :-
	get_module_info(Module, imports, Imports),
	findall(M, (member(M,Imports),get_flag(Pred,visibility,E)@M,
	    (E=exported;E=reexported)), ExportingModules),
	printf(warning_output, "Ambiguous import of %w from %w in module %w%n",
	    [Pred, ExportingModules, Module]).

    % These predicates will be preferred over definitions in
    % other modules when they are ambiguously imported.
    preferred_predicate((>)/2, eclipse_language).
    preferred_predicate((<)/2, eclipse_language).
    preferred_predicate((>=)/2, eclipse_language).
    preferred_predicate((=<)/2, eclipse_language).
    preferred_predicate((=:=)/2, eclipse_language).
    preferred_predicate((=\=)/2, eclipse_language).


cost_handler(_, (Cost, _)) :-
	printf("Found a solution with cost %w%n%b", Cost).
cost_handler(_, no(Cost, _)) :-
	printf("Found no solution with cost %w%n%b", Cost).


%-------------------------------------
% Symbolic waking triggers
%-------------------------------------

?- make_array_(trigger_suspensions, global_reference, local, sepia_kernel).

% The postponed list is separate because it is also accessed from C
% Moreover, the postponed list is emptied on waking. This makes a difference
% for demons (which would otherwise stay on the list). This semantics
% seems more useful, because demon predicates are often not aware that
% they have been transferred to the postponed-list and therefore cause
% looping when they stay on the list. Conceptually, every postponed-list
% is woken exactly once, and a fresh postponed list is created at that time.

:- export
	attach_suspensions/2,
	attached_suspensions/2,
	schedule_suspensions/1,
        current_trigger/1,
	trigger/1.

trigger(postponed) :- !,
	trigger_postponed.
trigger(N) :-
	schedule_suspensions(N),
	wake.

trigger_postponed :-
	get_postponed_nonempty(WL),	% and reinitialise
	!,
	schedule_suspensions(2,WL),
	wake,
	trigger_postponed.
trigger_postponed.


attached_suspensions(N, Susps) :-
	atom(N), !,
	( find_trigger(N, WL) ->
	    arg(2, WL, Susps)
	;
	    Susps = []
	).
attached_suspensions(N, Susps) :-
	nonvar(N), !,
	error(5, attached_suspensions(N, Susps)).
attached_suspensions(N, Susps) :-
	error(4, attached_suspensions(N, Susps)).


schedule_suspensions(N) :-
	( find_trigger(N, WL) ->
	    schedule_suspensions(2,WL)
	;
	    true
	).


    find_trigger(postponed, ESusp) :- !,
	get_postponed_nonempty(ESusp).	% and reinitialise
    find_trigger(T, WL) :-
	getval(trigger_suspensions, List),
	find_trigger(List, T, WL).

    :- mode find_trigger(+,+,-).
    find_trigger([ESusp|ESusps], T, WL) :-
	( ESusp = es(T,_) ->
	    WL = ESusp
	;
	    find_trigger(ESusps, T, WL)
	).

    enter_trigger(postponed, ESusp) :- !,
	get_postponed(ESusp).
    enter_trigger(T, WL) :-
	getval(trigger_suspensions, List),
	( find_trigger(List, T, WL) ->	% and reinitialise
	    true
	;
	    WL = es(T,[]),
	    setval(trigger_suspensions,[WL|List])
	).


current_trigger(postponed).
current_trigger(Trigger) :-
        getval(trigger_suspensions, List),
        member(es(Trigger, _), List).


attach_suspensions(postponed, Susp) ?- !,
	postpone_suspensions(Susp).
attach_suspensions(Trigger, Susp) :-
	atom(Trigger), !,
	attach_suspensions1(Trigger, Susp).
attach_suspensions(Trigger, Susp) :-
	nonvar(Trigger), !, 
	error(5, attach_suspensions(Trigger, Susp)).
attach_suspensions(Trigger, Susp) :-
	error(4, attach_suspensions(Trigger, Susp)).

attach_suspensions1(Trigger, Susp) :-
	var(Susp), !,
	error(4, attach_suspensions(Trigger, Susp)).
attach_suspensions1(_Trigger, []) :- !.
attach_suspensions1(Trigger, Susps) :-
	Susps = [_|_], !,
	enter_trigger(Trigger, Entry),
	enter_suspensions_list(Trigger, Entry, Susps).
attach_suspensions1(Trigger, Susp) :-
	atomic(Susp), is_suspension(Susp), !,
	enter_trigger(Trigger, Entry),
	enter_suspension_list(2, Entry, Susp).
attach_suspensions1(Trigger, Susp) :-
	error(5, attach_suspensions(Trigger, Susp)).

    enter_suspensions_list(Trigger, _Entry, Susps) :-
    	var(Susps), !,
	error(4, attach_suspensions(Trigger, Susps)).
    enter_suspensions_list(_, _, []) :- !.
    enter_suspensions_list(Trigger, Entry, [Susp|Susps]) :- !,
	enter_suspension_list(2, Entry, Susp),
	enter_suspensions_list(Trigger, Entry, Susps).
    enter_suspensions_list(Trigger, _Entry, Susps) :-
	error(5, attach_suspensions(Trigger, Susps)).


% Specialised code for attach_suspensions(postponed, Susp):
% This is not strictly necessary, but we can clean up the postponed
% list slightly more eagerly than an arbitrary suspension list.
postpone_suspensions(Susp) :-
	var(Susp), !,
	error(4, attach_suspensions(postponed, Susp)).
postpone_suspensions([]) :- !.
postpone_suspensions(Susps) :-
	Susps = [_|_], !,
	postpone_suspensions(1, s(Susps)).
postpone_suspensions(Susp) :-
	atomic(Susp), is_suspension(Susp), !,
	postpone_suspensions(1, s([Susp])).
postpone_suspensions(Susp) :-
	error(5, attach_suspensions(postponed, Susp)).



%-------------------------------------
% default error handler definitions
%-------------------------------------

?- set_default_error_handler_(1, error_handler/2, sepia_kernel),
   set_default_error_handler_(2, error_handler/2, sepia_kernel),
   set_default_error_handler_(4, error_handler/4, sepia_kernel),
   set_default_error_handler_(5, error_handler/4, sepia_kernel),
   set_default_error_handler_(6, error_handler/4, sepia_kernel),
   set_default_error_handler_(7, error_handler/2, sepia_kernel),
   set_default_error_handler_(8, error_handler/2, sepia_kernel),
   set_default_error_handler_(11, true/0, sepia_kernel), % set in meta.pl
   set_default_error_handler_(15, fail/0, sepia_kernel),
   set_default_error_handler_(16, fail/0, sepia_kernel),
   set_default_error_handler_(17, error_handler/2, sepia_kernel),
   set_default_error_handler_(20, error_handler/2, sepia_kernel),
   set_default_error_handler_(21, error_handler/4, sepia_kernel),
   set_default_error_handler_(23, compare_handler/4, sepia_kernel),
   set_default_error_handler_(24, error_handler/2, sepia_kernel),
   set_default_error_handler_(25, integer_overflow_handler/2, sepia_kernel),
   set_default_error_handler_(30, error_handler/2, sepia_kernel),
   set_default_error_handler_(31, error_handler/2, sepia_kernel),
   set_default_error_handler_(32, warning_handler/2, sepia_kernel),
   set_default_error_handler_(33, error_handler/2, sepia_kernel),
   set_default_error_handler_(40, error_handler/2, sepia_kernel),
   set_default_error_handler_(41, undef_array_handler/3, sepia_kernel),
   set_default_error_handler_(42, make_array_handler/4, sepia_kernel),
   set_default_error_handler_(43, error_handler/2, sepia_kernel),
   set_default_error_handler_(44, error_handler/2, sepia_kernel),
   set_default_error_handler_(45, undef_record_handler/2, sepia_kernel),
   set_default_error_handler_(50, error_handler/2, sepia_kernel),
   set_default_error_handler_(60, error_handler/4, sepia_kernel),
   set_default_error_handler_(61, error_handler/4, sepia_kernel),
   set_default_error_handler_(62, error_handler/4, sepia_kernel), 
   set_default_error_handler_(63, error_handler/4, sepia_kernel), 
   set_default_error_handler_(64, dynamic_handler/3, sepia_kernel), 
   set_default_error_handler_(65, error_handler/4, sepia_kernel), 
   set_default_error_handler_(66, error_handler/4, sepia_kernel), 
   set_default_error_handler_(67, error_handler/4, sepia_kernel),
   set_default_error_handler_(68, call_handler/4, sepia_kernel),
   set_default_error_handler_(69, autoload_handler/4, sepia_kernel),
   set_default_error_handler_(70, undef_dynamic_handler/3, sepia_kernel),
   set_default_error_handler_(71, error_handler/2, sepia_kernel),
   set_default_error_handler_(72, error_handler/2, sepia_kernel),
   set_default_error_handler_(73, true/0, sepia_kernel),
   set_default_error_handler_(74, true/0, sepia_kernel),
   set_default_error_handler_(75, declaration_warning_handler/3, sepia_kernel),
   set_default_error_handler_(76, declaration_warning_handler/3, sepia_kernel),
   set_default_error_handler_(77, declaration_warning_handler/3, sepia_kernel),
   set_default_error_handler_(78, error_handler/2, sepia_kernel),
   set_default_error_handler_(79, call_dynamic_/3, sepia_kernel),
   set_default_error_handler_(80, error_handler/2, sepia_kernel),
   set_default_error_handler_(81, error_handler/2, sepia_kernel),
   set_default_error_handler_(82, locked_access_handler/2, sepia_kernel),
   set_default_error_handler_(83, warning_handler/2, sepia_kernel), 
   set_default_error_handler_(84, declaration_warning_handler/3, sepia_kernel),
   set_default_error_handler_(85, declaration_warning_handler/3, sepia_kernel),
   set_default_error_handler_(86, no_lookup_module_handler/4, sepia_kernel),
   set_default_error_handler_(87, warning_handler/3, sepia_kernel),
   set_default_error_handler_(88, warning_handler/3, sepia_kernel),
   set_default_error_handler_(89, warning_handler/3, sepia_kernel),
   set_default_error_handler_(90, error_handler/4, sepia_kernel),
   set_default_error_handler_(91, error_handler/2, sepia_kernel),
   set_default_error_handler_(92, error_handler/4, sepia_kernel),
   set_default_error_handler_(93, error_handler/4, sepia_kernel),
   set_default_error_handler_(94, error_handler/4, sepia_kernel),
   set_default_error_handler_(96, ambiguous_import_resolve/3, sepia_kernel),
   set_default_error_handler_(97, error_handler/2, sepia_kernel),
   set_default_error_handler_(98, error_handler/2, sepia_kernel),
   set_default_error_handler_(99, ambiguous_import_warn/3, sepia_kernel),
   set_default_error_handler_(100, undef_dynamic_handler/3, sepia_kernel),
   set_default_error_handler_(101, error_handler/2, sepia_kernel),
   set_default_error_handler_(110, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(111, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(112, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(113, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(114, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(115, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(116, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(117, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(118, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(119, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(120, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(121, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(122, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(123, error_handler/4, sepia_kernel),
   set_default_error_handler_(124, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(125, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(126, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(127, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(128, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(129, parser_error_handler/2, sepia_kernel),
   set_default_error_handler_(130, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(131, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(132, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(133, true/0, sepia_kernel),
   set_default_error_handler_(134, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(135, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(136, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(137, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(138, singleton_in_loop/2, sepia_kernel),
   set_default_error_handler_(139, true/0, sepia_kernel),
   set_default_error_handler_(140, error_handler/2, sepia_kernel),
   set_default_error_handler_(141, error_handler/2, sepia_kernel),
   set_default_error_handler_(142, error_handler/2, sepia_kernel),
   set_default_error_handler_(143, compiler_error_handler/2, sepia_kernel),
   set_default_error_handler_(145, redef_other_file_handler/2, sepia_kernel),
   set_default_error_handler_(146, true/0, sepia_kernel),
   set_default_error_handler_(147, compiler_abort_handler/3, sepia_kernel),
   set_default_error_handler_(148, pragma_handler/3, sepia_kernel),
   set_default_error_handler_(149, unit_loaded_handler/3, sepia_kernel),
   set_default_error_handler_(150, true/0, sepia_kernel),
   set_default_error_handler_(151, true/0, sepia_kernel),
   set_default_error_handler_(152, true/0, sepia_kernel),
   set_default_error_handler_(157, error_exit/0, sepia_kernel),
   set_default_error_handler_(160, macro_handler/3, sepia_kernel),
   set_default_error_handler_(161, macro_handler/3, sepia_kernel),
   set_default_error_handler_(162, warning_handler/2, sepia_kernel),
   set_default_error_handler_(163, error_handler/2, sepia_kernel),
   set_default_error_handler_(165, error_handler/2, sepia_kernel),
   set_default_error_handler_(166, record_compiled_file_handler/3, sepia_kernel),
   set_default_error_handler_(167, warning_handler/3, sepia_kernel),
   set_default_error_handler_(170, system_error_handler/4, sepia_kernel),
   set_default_error_handler_(171, error_handler/2, sepia_kernel),
   set_default_error_handler_(172, error_handler/2, sepia_kernel),
   set_default_error_handler_(173, error_handler/2, sepia_kernel),
   set_default_error_handler_(174, error_handler/2, sepia_kernel),
   set_default_error_handler_(175, error_handler/2, sepia_kernel),
   set_default_error_handler_(176, error_handler/2, sepia_kernel),
   set_default_error_handler_(177, error_handler/2, sepia_kernel),
   set_default_error_handler_(190, eof_handler/4, sepia_kernel),
   set_default_error_handler_(191, output_error_handler/4, sepia_kernel),
   set_default_error_handler_(192, error_handler/2, sepia_kernel),
   set_default_error_handler_(193, error_handler/2, sepia_kernel),
   set_default_error_handler_(194, error_handler/2, sepia_kernel),
   set_default_error_handler_(195, io_yield_handler/2, sepia_kernel),
   set_default_error_handler_(196, close_handler/2, sepia_kernel),
   set_default_error_handler_(197, error_handler/2, sepia_kernel),
   set_default_error_handler_(198, past_eof_handler/2, sepia_kernel),
   set_default_error_handler_(210, error_handler/2, sepia_kernel),
   set_default_error_handler_(211, error_handler/2, sepia_kernel),
   set_default_error_handler_(212, error_handler/2, sepia_kernel),
   set_default_error_handler_(213, error_handler/2, sepia_kernel),
   set_default_error_handler_(214, error_handler/2, sepia_kernel),
   set_default_error_handler_(230, error_handler/2, sepia_kernel),
   set_default_error_handler_(231, fail/0, sepia_kernel),
   set_default_error_handler_(250, true/0, sepia_kernel),
   set_default_error_handler_(251, true/0, sepia_kernel),
   set_default_error_handler_(252, true/0, sepia_kernel),
   set_default_error_handler_(253, true/0, sepia_kernel),
   set_default_error_handler_(254, true/0, sepia_kernel),
   set_default_error_handler_(255, true/0, sepia_kernel),
   set_default_error_handler_(256, true/0, sepia_kernel),
   set_default_error_handler_(257, true/0, sepia_kernel),
   set_default_error_handler_(258, true/0, sepia_kernel),
   set_default_error_handler_(259, true/0, sepia_kernel),
   set_default_error_handler_(260, error_handler/2, sepia_kernel),
   set_default_error_handler_(261, error_handler/2, sepia_kernel),
   set_default_error_handler_(262, error_handler/2, sepia_kernel),
   set_default_error_handler_(263, error_handler/2, sepia_kernel),
   set_default_error_handler_(264, compiled_file_handler/3, sepia_kernel),
   set_default_error_handler_(265, compiled_file_handler/3, sepia_kernel),
   set_default_error_handler_(267, error_handler/2, sepia_kernel),
   set_default_error_handler_(268, error_handler/2, sepia_kernel),
   set_default_error_handler_(270, error_handler/2, sepia_kernel),
   set_default_error_handler_(271, error_handler/2, sepia_kernel),
   set_default_error_handler_(272, warning_handler/2, sepia_kernel),
   set_default_error_handler_(274, error_handler/2, sepia_kernel),
   set_default_error_handler_(280, cost_handler/2, sepia_kernel).

/* default error handler for MegaLog errors */

'$transaction_deadlock'(317,_) :- exit_block(abort_transaction).

?- set_default_error_handler_(300, error_handler/2, sepia_kernel),
   set_default_error_handler_(301, error_handler/2, sepia_kernel),
   set_default_error_handler_(302, error_handler/2, sepia_kernel),
   set_default_error_handler_(303, error_handler/2, sepia_kernel),
   set_default_error_handler_(304, error_handler/2, sepia_kernel),
   set_default_error_handler_(305, error_handler/2, sepia_kernel),
   set_default_error_handler_(306, error_handler/2, sepia_kernel),
   set_default_error_handler_(307, error_handler/2, sepia_kernel),
   set_default_error_handler_(308, error_handler/2, sepia_kernel),
   set_default_error_handler_(309, error_handler/2, sepia_kernel),
   set_default_error_handler_(310, error_handler/2, sepia_kernel),
   set_default_error_handler_(311, error_handler/2, sepia_kernel),
   set_default_error_handler_(312, error_handler/2, sepia_kernel),
   set_default_error_handler_(313, error_handler/2, sepia_kernel),
   set_default_error_handler_(314, error_handler/2, sepia_kernel),
   set_default_error_handler_(315, error_handler/2, sepia_kernel),
   set_default_error_handler_(316, error_handler/2, sepia_kernel),
   set_default_error_handler_(317, '$transaction_deadlock'/2, sepia_kernel),
   set_default_error_handler_(318, error_handler/2, sepia_kernel),
   set_default_error_handler_(319, error_handler/2, sepia_kernel),
   set_default_error_handler_(320, error_handler/2, sepia_kernel),
   set_default_error_handler_(321, error_handler/2, sepia_kernel),
   set_default_error_handler_(322, error_handler/2, sepia_kernel),
   set_default_error_handler_(329, warning_handler/2, sepia_kernel),
   set_default_error_handler_(333, warning_handler/2, sepia_kernel).

?- set_event_handler(postponed, trigger/1),
   set_event_handler(requested_fail_event, trigger/1),
   set_event_handler(garbage_collect_dictionary, garbage_collect_dictionary/0),
   set_event_handler(abort, exit_block/1).

reset_error_handlers :-
	current_error(N),
	reset_error_handler(N),
	fail.
reset_error_handlers.

?- reset_error_handlers.		% set up the handlers

%-------------------------------------
% interrupt handling builtins
%-------------------------------------

current_interrupt(N, Name) :-
	var(N), var(Name), !,
	gen_interrupt_id(N, Name, 1).
current_interrupt(N, Name) :-
	(integer(N);var(N)),
	(atom(Name);var(Name)),
	!,
	interrupt_id_det(N, Name),
	Name \== '.'.
current_interrupt(N, Name) :-
	error(5, current_interrupt(N, Name)).

gen_interrupt_id(Number, Name, N) :-
        interrupt_id_det(N, Name) ->
	    Name \== '.',
            Number = N
        ;
            !, fail.
gen_interrupt_id(Number, Name, N) :-
        N1 is N + 1,
        gen_interrupt_id(Number, Name, N1).


set_interrupt_handler_body(N, Proc, M):- 
	(check_interrupt(N, Int) ->
		set_interrupt_handler_nr(Int, Proc, M)
	;
		bip_error(set_interrupt_handler(N, Proc), M)
	).

get_interrupt_handler(N, X, M):-
	(var(X), X = F/A ; X = F/A, atom(F),integer(A)),
	(var(M) ; atom(M)),
	!,
	(check_interrupt(N, Int) ->
		get_interrupt_handler_nr(Int, F, A, M)
	;
		bip_error(get_interrupt_handler(N, X, M))
	).

get_interrupt_handler(N, X, M):-
	error(5, get_interrupt_handler(N, X, M)).



% Check Int for a valid interrupt id and unify N with the interrupt number
check_interrupt(N, Int):-
	(var(N) ->
		set_bip_error(4)
	;
	integer(N) ->
		(interrupt_id_det(N, Name), Name \== '.' ->
			N = Int
		;
			set_bip_error(6)
		)
	;
	atom(N) ->
		(interrupt_id_det(Int, N), N \== '.' ->
			true
		;
			set_bip_error(6)
		)
	;
		set_bip_error(5)
	).
	


%----------------------------------------------------------------------
% Raising events from socket streams
%----------------------------------------------------------------------

io_event_handler :-
	findall(Event, ready_sigio_stream_event(Event), Events),
	event(Events),
	events_nodefer.

    ready_sigio_stream_event(Event) :-
	current_stream(S),
	get_stream_info(S, sigio, on),		% it is a sigio stream
	get_stream_info(S, event, Event),	% it wants an event
	select([S], 0, [_]).			% it has data


?- ( current_interrupt(_, io) -> 
	set_interrupt_handler(io, event/1),
	set_event_handler(io, defers(io_event_handler/0))
%	set_interrupt_handler(io, internal/0)	% if socket events not needed
   ;
	true
   ).

?- ( current_interrupt(_, poll) -> 
	set_interrupt_handler(poll, event/1),
	set_event_handler(poll, defers(io_event_handler/0))
%	set_interrupt_handler(poll, internal/0)	% if socket events not needed
   ;
	true
   ).


%----------------------------------------------------------------------
% An event handler that reads exdr terms (atoms or strings)
% from a stream (typically socket) and posts them as events.
% We expect this handler to be set up with the defers-option.
%----------------------------------------------------------------------

:- export post_events_from_stream/1.

post_events_from_stream(Stream) :-
	( select([Stream], 0, [_]), read_exdr(Stream, EventName) ->
	    ( atom(EventName) ->
		event(EventName)
	    ; string(EventName) ->
		atom_string(EventNameAtom, EventName),
		event(EventNameAtom)
	    ;
		type_of(EventName, BadType),
		printf(warning_output, 
		    "WARNING: ignoring %w on event posting stream %w%n%b",
		    [BadType,Stream])
	    ),
	    post_events_from_stream(Stream)
	;
	    events_nodefer
	).


%----------------------------------------------------------------------
% postpone_exit(+Tag) is called when an exit_block was requested inside
% an interrupt, but the exit_block protection is active (e.g. we were
% interrupting a garbage collection). The exit_block is postponed by
% saving the Tag and setting the WAS_EXIT flag.
%----------------------------------------------------------------------

?- make_array_(postpone_exit, prolog, local, sepia_kernel).

postpone_exit(Tag) :-
	setval(postpone_exit, Tag),
	vm_flags(0, 16'08000000, _),	% set the WAS_EXIT flag
	sys_return(0).

% exit_postponed/0 is called when the exit_block protection
% is dropped and the WAS_EXIT flag is set.

exit_postponed :-
	getval(postpone_exit, Tag),
	vm_flags(16'0c000000, 0, _),	% clear NO_EXIT and WAS_EXIT
	exit_block(Tag).

%----------------------------------------------------------------------
% after
%----------------------------------------------------------------------

% Ordered list of pending events, containing structures of the form:
%
%	ev(PostTime, EventName)
%	ev(every(Interval), EventName)
%
% Only modify this variable while event handling is deferred!
% After modifying the variable, call adjust_after_timer/1
% to make sure the next alarm occurs in time for the next event.
:- local variable(after_events).
?- setval(after_events, []).

% The physical timer used for after events: 'real' or 'virtual'
:- local variable(after_event_timer).


current_after_event(E) :-
	(is_event(E) ->
	    !,
	    getval(after_events, EQ),	% atomic read, no need to defer events
	    memberchk(ev(_,E)-_, EQ)

	; var(E) ->
	    !,
	    getval(after_events, EQ),	% atomic read, no need to defer events
	    findall(X, member(ev(_,X)-_, EQ), E)

	; set_bip_error(5)
        ).
current_after_event(E) :-
	get_bip_error(Err),
	error(Err, current_after_event(E)).

current_after_events(DueEvents) :-
	getval(after_events, Events),	% atomic read, no need to defer events
	get_due_event_list(Events, DueEvents).

get_due_event_list([], []).
get_due_event_list([Event | Events], [DueEvent | DueEvents]) :-
	Event = ev(Type, Name)-DueTime,
	DueEvent = due(Name-Type, DueTime),
	get_due_event_list(Events, DueEvents).


% (Synchronous) handler when after-timer expires
% This handler is called with events deferred, and must invoke events_nodefer
% at the end! It must therefore not fail or throw.
% The handler must not contain any calls to wake/0 (however embedded,
% e.g. inside call_priority/2) because that would interfere with
% the environment's waking state.

after_handler :-
	current_after_time(CurrentTime),

	getval(after_events, EQ0),
	ready_events(EQ0, CurrentTime, RepeatEvents, DuedEvents, EQ1),
	sort(2, =<, RepeatEvents, SortedRepeatEvents),
	merge(2, =<, SortedRepeatEvents, EQ1, EQ2),
	setval(after_events, EQ2),

        event(DuedEvents),	% events are deferred at this point!

	adjust_after_timer(EQ2),

	events_nodefer.


% Default timer is real. 

?-  ( current_interrupt(_, alrm) ->
	set_interrupt_handler(alrm, event/1)
    ;
	true
    ),
    setval(after_event_timer, real),
    set_event_handler(alrm, defers(after_handler/0)).

% Stop timer events before exiting eclipse
?- local finalization((
	get_flag(after_event_timer, Timer),
	stop_timer(Timer, _, _)
    )).

signal_timer(vtalrm, virtual).
signal_timer(alrm, real).

try_set_after_timer(Timer) :-
	% assume here that we can always set timer to 'real'
        % alrm/vtalrm signals both do not exist on Windows!
	signal_timer(Signal, Timer),
	((Signal == alrm ; current_interrupt(_, Signal)) ->
	    get_flag(after_event_timer, Timer0),
	    % reinitialise after_events
	    stop_timer(Timer0, Remain, Interv),  	% stop old timer
	    (block(stop_timer(Timer, _, _), _, fail) ->
		true 
	    ; 
		printf(error, "%w not available on this configuration.%n", [Timer]),
		start_timer(Timer0, Remain, Interv),	% restart old timer
		fail
	    ),
	    signal_timer(Signal0, Timer0),
	    setval(after_events, []),
	    (Signal0 == Signal ->
		true
	    ;
		set_interrupt_handler(Signal, event/1),
		set_event_handler(Signal, defers(after_handler/0)),
		setval(after_event_timer, Timer)
	    )
	;

	    printf(error, "%w not available on this platform%n", [Timer]),
	    fail
	).


% To be called whenever after_events has changed, in order to ajust
% the timer. The argument is the current value of variable(after_events)
% This must be called with events being deferred!

adjust_after_timer(CurrentAfterEventQueue) :-
	get_flag(after_event_timer, Timer),
	stop_timer(Timer, _Remain, _),
	current_after_time(CurrentTime),
	( CurrentAfterEventQueue = [_-NextTime|_] ->
	     Interval is NextTime - CurrentTime,
	     (Interval > 0 ->
		  start_timer(Timer, Interval, 0)
	     ;
		  signal_timer(Signal, Timer),
		  event([Signal])   % events are due, handle them immediately
	     )
	;
	    true
	).
	

%
% event_after(+Event, Interval)
% event_after(+Event, Interval, DueTime)
% event_after_every(+Event, Interval)
% events_after(+List)

event_after(E, Int) :-
	event_after(E, Int, _).


event_after(E, Int, DueTime) :-
	(
	    check_event(E),
	    check_interval(single, Int)
	->
	    current_after_time(CurrentTime),
	    ( events_defer ->
		unchecked_add_after_event(CurrentTime, CurrentTime, E, Int, DueTime),
		events_nodefer
	    ;
		unchecked_add_after_event(CurrentTime, CurrentTime, E, Int, DueTime)
	    )
	;
	    get_bip_error(Id),
	    error(Id, event_after(E, Int))
	).

event_after_every(E, Int) :-
	(
	    check_event(E),
	    check_interval(every, Int)
	->
	    current_after_time(CurrentTime),
	    ( events_defer ->
		unchecked_add_after_event(every(Int), CurrentTime, E, Int, _DueTime),
		events_nodefer
	    ;
		unchecked_add_after_event(every(Int), CurrentTime, E, Int, _DueTime)
	    )
	;
	    get_bip_error(Id),
	    error(Id, event_after_every(E, Int))
	).

events_after(Es) :-
	(
	    check_after_events(Es, Names, Ints, Types)
	->
	    current_after_time(CurrentTime),
	    ( events_defer ->
		unchecked_add_after_events(Names, Ints, Types, CurrentTime),
		events_nodefer
	    ;
		unchecked_add_after_events(Names, Ints, Types, CurrentTime)
	    )
	;    
	    get_bip_error(Id),
	    error(Id, events_after(Es))
	).


% may fail with set_bip_error
:- mode check_after_events(?,-,-,-).
check_after_events(X, _, _, _) :- var(X), !,
	set_bip_error(4).
check_after_events([], [], [], []) :- !.
check_after_events([E|Es], [Name|Names], [Int|Ints], [Type|Types]) :- !,
	check_event_spec(E, Name, Type, Int),
	check_after_events(Es, Names, Ints, Types).
check_after_events(_, _, _, _) :-
	set_bip_error(5).

    check_event_spec(Spec, _Name, _Type, _Interval) :- var(Spec), !,
	set_bip_error(4).
    check_event_spec(Name-Type, Name, Type, Interval) :- !,
	check_event(Name),
	check_event_type(Type, Interval).
    check_event_spec(_Spec, _Name, _Type, _Interval) :-
	set_bip_error(5).

    :- mode check_event_type(?,-).
    check_event_type(Spec, _Interval) :- var(Spec), !,
	set_bip_error(4).
    check_event_type(every(Interval), Interval) :- !,
	check_interval(every, Interval).
    check_event_type(Interval, Interval) :-
	check_interval(single, Interval).

    % check_interval(+Type, ?Interval)
    :- mode check_interval(+,?).
    check_interval(every, Interval) :-		% after-every: > 0
	check_time_type(Interval),
	( Interval > 0 -> true ; set_bip_error(6) ).
    check_interval(single, Interval) :-		% simple after: >= 0
	check_time_type(Interval),
	( Interval >= 0 -> true ; set_bip_error(6) ).
	
    check_time_type(X) :- var(X), !, set_bip_error(4).
    check_time_type(X) :- number(X), \+ breal(X), !.
    check_time_type(_) :- set_bip_error(5).


% Called with events deferred. Must not fail/exit_block!
unchecked_add_after_events([], [], [], _) :-
	getval(after_events, List),
	adjust_after_timer(List).
unchecked_add_after_events([Name|Names], [Int|Ints], [Type|Types], CurrentTime) :-
	unchecked_add_after_event(Type, CurrentTime, Name, Int, _),
	unchecked_add_after_events(Names, Ints, Types, CurrentTime).
	    

unchecked_add_after_event(Type, CurrentTime, E, Int, NewEventTime) :-
	NewEventTime is CurrentTime + Int,
	getval(after_events, EQ0),
	%sort(2, =<, [ev(Type,E)-NewEventTime|EQ0], EQ1),	
	insert_into_after_event_queue(EQ0, NewEventTime, ev(Type,E), EQ1),
	setval(after_events, EQ1),
	adjust_after_timer(EQ1).


insert_into_after_event_queue([], NTime, NEvent, EQ) :- EQ = [NEvent-NTime].
insert_into_after_event_queue([Event-Time|EQ0], NewTime, NewEvent, EQ) :-
	(NewTime < Time ->
	     EQ = [NewEvent-NewTime,Event-Time|EQ0]
	;    EQ = [Event-Time|EQ1],
	     insert_into_after_event_queue(EQ0, NewTime, NewEvent, EQ1)
	).


ready_events([], _CurrentTime, [], [], []).
ready_events(EQ0, CurrentTime, Repeats0, Dued0, EQ) :-
	EQ0 = [EventInfo-EventTime|EQ1],
	( CurrentTime >= EventTime ->
	    EventInfo = ev(Type,Event),
	    Dued0 = [Event|Dued1],
            ( Type = every(Interval) ->
		RepeatTime is CurrentTime + Interval,
		Repeats0 = [EventInfo-RepeatTime|Repeats1]
	    ;
		Repeats0 = Repeats1
	    ),
            ready_events(EQ1, CurrentTime, Repeats1, Dued1, EQ)
	;
	    EQ = EQ0, Dued0 = [], Repeats0 = []
	).


cancel_after_event(Event) :-
	is_event(Event),
	!,
	( events_defer ->
	    cancel_after_event1(Event, Found),
	    events_nodefer
	;
	    cancel_after_event1(Event, Found)
	),
	Found = true.
cancel_after_event(Event) :-
	error(5, cancel_after_event(Event)).

    :-mode cancel_after_event1(+,-).
    cancel_after_event1(Event, Found) :-
	getval(after_events, EQ0),
	subtract_template(EQ0, ev(_,Event)-_, EQ1),
	( EQ1 == EQ0 ->
	    Found = false
	;
	    Found = true,
	    setval(after_events, EQ1)
	),
	adjust_after_timer(EQ1).

cancel_after_event(Event, CancelledEvents) :-
	is_event(Event),
	!,
	( events_defer ->
	    cancel_after_event2(Event, CancelledEvents0),
	    events_nodefer
	;
	    cancel_after_event2(Event, CancelledEvents0)
	),
	CancelledEvents = CancelledEvents0.
cancel_after_event(Event, CancelledEvents) :-
	error(5, cancel_after_event(Event, CancelledEvents)).

    :-mode cancel_after_event2(+,-).
    cancel_after_event2(Event, CancelledEvents) :-
	current_after_time(CurrentTime),
	getval(after_events, EQ0),
	extract_and_subtract_cancelled_events(EQ0, Event, CurrentTime, 
					      EQ1, CancelledEvents),
	(EQ1 == EQ0 ->
	    true
	;   
	    setval(after_events, EQ1)
	),
	adjust_after_timer(EQ1).


% subtract all occurrences of elements matching the template from list
subtract_template([], _, []).
subtract_template([H|T], Template, Subtracted) :-
	(\+(\+(Template = H)) ->
	    Subtracted = Subtracted0 ; Subtracted = [H|Subtracted0]
        ),
	subtract_template(T, Template, Subtracted0).

% subtract all occurrences of elements matching the template from list
% and extract the specified data from the first match
extract_and_subtract_cancelled_events([], _, _, [], []).
extract_and_subtract_cancelled_events([H|T], Event, CurrentTime, 
				      Subtracted, Extracted) :-
	( H = ev(Type, Event)-DueTime ->
	    Subtracted = Subtracted0,
	    ( number(Type) ->
		RemainingTime is max(0.0, DueTime - CurrentTime),
		CancelledEvent = Event-RemainingTime
	    ;
		CancelledEvent = Event-Type
	    ),
	    Extracted = [CancelledEvent|Extracted0]
	; 
	    Subtracted = [H|Subtracted0],
	    Extracted = Extracted0
        ),
	extract_and_subtract_cancelled_events(T, Event, CurrentTime, 
					      Subtracted0, Extracted0).
	


% Get the current time from the clock corresponding to the after-timer in use
current_after_time(T) :-
	get_flag(after_event_timer, Timer),
	(Timer == virtual -> T is cputime ; T is statistics(session_time)).



%-------------------------------------

check_event(E) :- var(E), !, set_bip_error(4).
check_event(E) :- is_event(E), !.
check_event(_) :- set_bip_error(5).

error_(N, G, LM) :-
	error_(N, G, LM, LM).    % the context module for normal errors is not significant


error_(default(N), G, CM, LM) :-
	integer(N),
	!,
	Nneg is -N,
	syserror(Nneg, G, CM, LM).
error_(N, G, CM, LM) :-
	syserror(N, G, CM, LM).


event(Var) :- var(Var), !,
	error(4, event(Var)).
event([]) :- !.
event(Events) :- Events = [_|_], !,
	post_events(Events).
event(N) :- atom(N), !,
	post_events([N]).
event(N) :- is_handle(N), is_event(N), !,
	post_events([N]).
event(Junk) :-
	error(5, event(Junk)).


bip_error_(Goal, LM) :-		% for internal use
	get_bip_error(E),
	syserror(E, Goal, LM, LM).

bip_error_(Goal, CM, LM) :-	% for internal use
	get_bip_error(E),
	syserror(E, Goal, CM, LM).


:- unskipped			% handlers that re-call the culprit
	event/1,
	compare_handler/4.

:- untraceable
	error_exit/0,
	get_autoload_info/4,
	compare_handler/4,
	autoload_handler/4,
	mutex_lib/2,
	call_handler/4.

:- skipped
	autoload_handler/4,
	call_handler/4,
	eof_handler/4,
	error_exit/0,
	error_handler/2,
	error_handler/3,
	error_handler/4,
	output_error_handler/4,
	parser_error_handler/2,
	system_error_handler/4.
