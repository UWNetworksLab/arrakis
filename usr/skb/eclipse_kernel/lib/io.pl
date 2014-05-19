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
% Version:	$Id: io.pl,v 1.2 2008/08/21 18:08:28 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	io.pl
 *
 * DESCRIPTION: 	
 *
 *
 * CONTENTS:     
 *
 */

/*
 * GLOBAL DIRECTIVES
 */
:- begin_module(sepia_kernel).
:- pragma(system).		% compiler directive to add the SYSTEM flag
:- pragma(nodebug).
:- pragma(expand).
:- pragma(skip).

:- export
	current_stream/3,
	get_stream_info/3,
	set_stream_property/3,
	current_stream/1,
	current_compiled_file/3,
	dump_header/1,
	dump_term/3,
	exec/2,
	exec/3,
	exec_group/3,
	make/0,
	open/4,
	sh/1,
	system/1,
	get_file_info/3,
	op/3,
	global_op/3,
	phrase/2,
	phrase/3,
	peer/1,
	peer_get_property/3,
	peer_queue_create/5,
	peer_queue_close/1,
	peer_queue_get_property/3,
        peer_multitask_confirm/0,
        peer_multitask_terminate/0,
        peer_register_multitask/2,
        peer_deregister_multitask/1,
        peer_do_multitask/1.


:- tool(phrase/2, phrase_body/3).
:- tool(phrase/3, phrase_body/4).
:- tool(file_query/2, file_query_body/3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% current_stream(?File, ?Mode, ?Stream)
% if Stream is uninstantiated, the stream number is returned.
% if used for testing, a stream name is accepted as well.

current_stream(File, Mode, Stream) :-
	(
	    (var(File);atom(File);string(File)),
	    (var(Mode);atom(Mode)),
	    (var(Stream);atom(Stream);integer(Stream))
	->
		( var(Stream) ->
		    stream_number(Max),
		    gen_valid_streams(0, Max, Stream)
		;
		    is_open_stream(Stream)	% else fail
		),
		stream_info_(Stream, 0, File),
		stream_info_(Stream, 2, Mode)
	;
	    error(5, current_stream(File, Mode, Stream))
	).

current_stream(Stream) :- var(Stream), !,
	stream_number(Max),
	gen_valid_streams(0, Max, Stream).
current_stream(Stream) :- (atom(Stream);integer(Stream)), !,
	is_open_stream(Stream).
current_stream(Stream) :-
	error(5, current_stream(Stream)).

    % generate integers from Start to Finish, which are existing streams
    gen_valid_streams(Start, _, Start) :-
	is_open_stream(Start).
    gen_valid_streams(Start, Finish, Stream) :-
	Start =< Finish,
	NewStart is Start + 1,
	gen_valid_streams(NewStart, Finish, Stream).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% get_stream_info(+Stream, ?Info, ?Value)
% t a s k : accesss various data in the stream descriptor

get_stream_info(Stream, Info, Value) :-
	( check_valid_stream(Stream) ->
	    (   var(Info) ->
		    stream_info_nr(Info, N),
		    stream_info_wrapper(Stream, N, Value)
	    ;   atom(Info) ->
		(   stream_info_nr(Info, N) ->
			stream_info_wrapper(Stream, N, Value)
		;    error(6, get_stream_info(Stream, Info, Value))
		)
	    ;
		error(5, get_stream_info(Stream, Info, Value))
	    )
	;
	    bip_error(get_stream_info(Stream, Info, Value))
	).

stream_info_wrapper(Stream, N, Value) :-
	( stream_info_nr(output_options, N) ->
	    stream_info_(Stream, N, On),
	    stream_info_nr_hidden(print_depth, N1),
	    stream_info_(Stream, N1, Depth),
	    options_from_format(On, Depth, Value)
	;
	    stream_info_(Stream, N, Value)
	).

:- mode stream_info_nr(?,-).
stream_info_nr(name, 0).
%stream_info_nr(mode, 2).	% old-style mode
stream_info_nr(physical_stream, 4).
stream_info_nr(aliases, 3).
stream_info_nr(system_use, 7).
stream_info_nr(line, 5).
stream_info_nr(offset, 6).
stream_info_nr(prompt, 1).
stream_info_nr(prompt_stream, 8).
stream_info_nr(fd, 9).
stream_info_nr(port, 10).
stream_info_nr(connection, 11).
stream_info_nr(reprompt_only, 12).
stream_info_nr(device, 13).
stream_info_nr(mode, 15).
stream_info_nr(event, 17).
stream_info_nr(flush, 18).
stream_info_nr(yield, 19).
stream_info_nr(end_of_line, 20).
stream_info_nr(scramble, 21).
stream_info_nr(sigio, 22).
stream_info_nr(usable, 23).
stream_info_nr(macro_expansion, 24).
stream_info_nr(output_options, 25).
%stream_info_nr(print_depth, 26).	% hidden
stream_info_nr(compress, 27).
stream_info_nr(last_written, 28).

stream_info_nr_hidden(print_depth, 26).


set_stream_property(Stream, Info, Value) :-
	set_stream_property1(Stream, Info, Value),
	!.
set_stream_property(Stream, Info, Value) :-
	bip_error(set_stream_property(Stream, Info, Value)).

    set_stream_property1(_Stream, Info, _Value) :- var(Info), !,
	set_bip_error(4).
    set_stream_property1(Stream, output_options, Options) :- !,
	options_to_format(Options, 0, _Off, 0, On, Depth),
	stream_info_nr(output_options, I1),
	set_stream_prop_(Stream, I1, On),
	stream_info_nr_hidden(print_depth, I2),
	set_stream_prop_(Stream, I2, Depth).
    set_stream_property1(Stream, Info, Value) :-
	( stream_info_nr(Info, Nr) -> true ; set_bip_error(6) ),
	set_stream_prop_(Stream, Nr, Value).



current_compiled_file(File, Time, Module) :-
	current_compiled_file(File, Time, Module, _Goal).


make :-
	current_compiled_file(File, Time, Module, Goal),
	    get_file_info(File, mtime) =\= Time,
	    Goal@Module,	% normally compile(File)@Module
	fail.
make.



open(File, Mode, Stream, Options) :-
	open(File, Mode, Stream),
	set_stream_options(Options, Stream), !.
open(File, Mode, Stream, Options) :-
	bip_error(open(File, Mode, Stream, Options)).

set_stream_options(Options, _) :- var(Options), !, set_bip_error(4).
set_stream_options([], _) :- !.
set_stream_options([O|Os], Stream) :- !,
	set_stream_option(O, Stream),
	set_stream_options(Os, Stream).
set_stream_options(_, _) :-
	set_bip_error(5).

    set_stream_option(alias(Name), Stream) ?- !,
	set_stream(Name, Stream).
    set_stream_option(output_options(Options), Stream) ?-
	options_to_format(Options, 0, _Off, 0, On, Depth),
	stream_info_nr(output_options, I1),
	set_stream_prop_(Stream, I1, On),
	stream_info_nr_hidden(print_depth, I2),
	set_stream_prop_(Stream, I2, Depth),
	!.
    set_stream_option(Option, Stream) :-
	compound(Option),
        functor(Option, Name, 1),
	arg(1, Option, Value),
	stream_info_nr(Name, Nr),
	!,
	set_stream_prop_(Stream, Nr, Value).
    set_stream_option(_Option, _) :- set_bip_error(6).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% OPERATORS
%

:- tool( op/3,	local_op_body/4).
:- tool( global_op/3,		global_op_body/4).

local_op_body(Preced, Assoc, Op, Module):-
	op_body(local, Preced, Assoc, Op, Module).

global_op_body(Preced, Assoc, Op, Module):-
	op_body(global, Preced, Assoc, Op, Module).

op_body(Visible,Preced,Assoc,Op,Module):-
	var(Op), !,
	op_body1(Visible,Preced,Assoc,Op,Module).
op_body(_,_,_,[],_):-!.
op_body(Visible,Preced,Assoc,[Op|T],Module):-
	!,
	op_body1(Visible,Preced,Assoc,Op,Module),
	op_body(Visible,Preced,Assoc,T,Module).
op_body(Visible,Preced,Assoc,Op,Module):-
	op_body1(Visible,Preced,Assoc,Op,Module).

op_body1(Visible,Preced,Assoc,Op,Module):-
	op_(Visible,Preced,Assoc,Op,Module)
	->
	    true
	;
	(
	    get_bip_error(Err),
	    (
		Visible == local
		->
		    error(Err, op(Preced, Assoc, Op))
		;
		    error(Err, global_op(Preced, Assoc, Op))
	    )
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% read_term/3 and write_term/3 (ISO compatible)
%

:- export
	read_term/2,
	read_term/3.

:- tool(read_term/2, read_term_/3).
:- tool(read_term/3, read_term_/4).

read_term_(Term, Options, Module) :-
	read_term_(input, Term, Options, Module).

read_term_(Stream, Term, Options, Module) :-		% 8.14.1
	readvar(Stream, Term, Vars, Module),
	handle_read_options(Options, Term, Vars),
	!.
read_term_(Stream, Term, Options, Module) :-
	get_bip_error(E),
	error(E, read_term(Stream, Term, Options), Module).

    handle_read_options(Options, _, _) :- var(Options), !,
    	set_bip_error(4).
    handle_read_options([], _, _) :- !.
    handle_read_options([O|Os], Term, Vars) :- !,
	handle_read_option(O, Term, Vars),
	handle_read_options(Os, Term, Vars).
    handle_read_options(_Options, _, _) :-
    	set_bip_error(5).

    handle_read_option(Option, _, _) :- var(Option), !,
	set_bip_error(4).
    handle_read_option(variables(Vs), Term, _Vars) :- !,
	term_variables(Term, RVars),	% returns reverse order
	reverse(RVars, Vs).		% ISO requires left-to-right
    handle_read_option(variable_names(VNs), _Term, Vars) :- !,
    	name_eq_var(Vars, VNs).
    handle_read_option(singletons(NamesSingletons), Term, NsVs) :- !,
	collect_variables(Term, [], Vars),
	( Vars = [] ->
	    NamesSingletons = []
	;
	    sort(0, =<, Vars, SortedVars),
	    SortedVars = [_X|Xs],
	    collect_singletons(_X, Xs, Singletons),
	    add_names(Singletons, NsVs, NamesSingletons)
	).
    handle_read_option(_, _, _) :-
	set_bip_error(6).

    vars_only([], []).
    vars_only([[_|V]|Vars], [V|Vs]) :-
	vars_only(Vars, Vs).

    name_eq_var([], []).
    name_eq_var([[N|V]|VNs], [N=V|Vs]) :-
	name_eq_var(VNs, Vs).

    collect_singletons(_X, [], [_X]).
    collect_singletons(_X, [_Y|Ys], Singletons) :-
	( _X == _Y ->
	     skip_multiples(_Y, Ys, Singletons)
	;
	     Singletons = [_X|Singletons1],
	     collect_singletons(_Y, Ys, Singletons1)
	).

    skip_multiples(_, [], []).
    skip_multiples(_X, [_Y|Ys], Singletons) :-
	( _X == _Y ->
	     skip_multiples(_Y, Ys, Singletons)
	;
	     collect_singletons(_Y, Ys, Singletons)
	).

    add_names([], _, []).
    add_names([S|Ss], NsVs, NsSs) :-
	( varnamelookup(S, NsVs, N) -> NsSs = [N=S|NsSs1] ; NsSs = NsSs1 ),
	add_names(Ss, NsVs, NsSs1).

    varnamelookup(X, [[N|Y]|_], N) :- X==Y, !.
    varnamelookup(X, [_|T], N):- varnamelookup(X, T, N).


:- export
	write_term/2,
	write_term/3.

:- tool(write_term/2, write_term_/3).
:- tool(write_term/3, write_term_/4).

write_term_(Term, Options, Module) :-
	write_term_(output, Term, Options, Module).

write_term_(Stream, Term, Options, Module) :-		% 8.14.2
	options_to_format(Options, 0, Off, 0, On, Depth),
	write_term(Stream, Term, Off, On, Depth, Module),
	!.
write_term_(Stream, Term, Options, Module) :-
	bip_error(write_term(Stream, Term, Options), Module).


% The following auxiliary predicates map symbolic write-options to
% bitmask+depth used on the C level (in write.c) and vice versa

:- mode options_to_format(?,+,-,+,-,-).	% may fail with bip_error
options_to_format(List, _, _, _, _, _) :- var(List), !,
	set_bip_error(4).
options_to_format([], Off, Off, On, On, Depth) :- !,
	( var(Depth) -> Depth = 0 ; true ).
options_to_format([O|Os], Off0, Off, On0, On, Depth) :- !,
	option_to_format(O, ThisOff, ThisOn, Depth),
	Off1 is Off0 \/ ThisOff,
	On1 is On0 \/ ThisOn,
	options_to_format(Os, Off1, Off, On1, On, Depth).
options_to_format(_, _, _, _, _, _) :-
	set_bip_error(5).

    option_to_format(Option, C, S, D) :-
	option_format(Option, C, S, D), !.
    option_to_format(Option, C, S, D) :-
	option_format_compat(Option, C, S, D), !.
    option_to_format(Junk, _, _, _) :- var(Junk), !,
	set_bip_error(4).
    option_to_format(Junk, _, _, _) :- compound(Junk), !,
	set_bip_error(6).
    option_to_format(_, _, _, _) :-
	set_bip_error(5).


options_from_format(On, Depth, Options) :-
	findall(Option, (
		option_format(Option, _, Bits, Depth),
		On /\ Bits =\= 0
	    ), Options0),
	once option_format(depth(full), _, FullDepthBit, _), 
	( On /\ FullDepthBit =:= 0, Depth \== 0 ->
	    Options = [depth(Depth)|Options0]
	;
	    Options = Options0
	).


% Output options
%
% ISO compatible:	ignore_ops, quoted, numbervars
% SICStus compatible:	max_depth, portrayed
% CAUTION: The numeric constants must match the definitions in io.h!

% option_format(?Option, -BitsToClear, -BitsToSet, ?MaxDepth).
:- mode option_format(?,-,-,?).
option_format(variables(anonymous),	16'4030, 16'4000, _).	% VAR_ANON
option_format(variables(default),	16'4030, 16'0000, _).
option_format(variables(raw),		16'4030, 16'0010, _).	% VAR_NUMBERS
option_format(variables(full),		16'4030, 16'0020, _).	% VAR_NAMENUM
option_format(attributes(none),		16'0500, 16'0000, _).
option_format(attributes(pretty),	16'0500, 16'0100, _).	% ATTRIBUTE
option_format(attributes(full),		16'0500, 16'0400, _).	% STD_ATTR
option_format(as(term),			16'1200, 16'0000, _).
option_format(as(clause),		16'1200, 16'1000, _).	% WRITE_CLAUSE
option_format(as(goal),			16'1200, 16'0200, _).	% WRITE_GOAL
option_format(newlines(true),		16'0000, 16'2000, _).	% DONT_QUOTE_NL
option_format(newlines(false),		16'2000, 16'0000, _).
option_format(operators(true),		16'0001, 16'0000, _).
option_format(operators(false),		16'0000, 16'0001, _).	% CANONICAL
option_format(dotlists(true),		16'0000, 16'0004, _).	% DOTLIST
option_format(dotlists(false),		16'0004, 16'0000, _).
option_format(transform(true),		16'0800, 16'0000, _).
option_format(transform(false),		16'0000, 16'0800, _).	% NO_MACROS
option_format(quoted(true),		16'0000, 16'0008, _).	% QUOTED
option_format(quoted(false),		16'0008, 16'0000, _).
option_format(numbervars(true),		16'0000, 16'8000, _).	% OUT_DOLLAR_VAR
option_format(numbervars(false),	16'8000, 16'0000, _).
option_format(portrayed(true),		16'0000, 16'0040, _).	% PRINT_CALL
option_format(portrayed(false),		16'0040, 16'0000, _).
option_format(depth(full),		16'0000, 16'0002, 0).	% FULLDEPTH
option_format(depth(N),			16'0002, 16'0000, N).
option_format(compact(true),		16'0000, 16'0080, _).	% WRITE_COMPACT
option_format(compact(false),		16'0080, 16'0000, _).

option_format_compat(ignore_ops(true),	16'0000, 16'0805, _).	% ISO compat
option_format_compat(ignore_ops(false),	16'0805, 16'0000, _).
option_format_compat(max_depth(0),	16'0000, 16'0002, 0).	% SICS compat
option_format_compat(max_depth(N),	16'0002, 16'0000, N).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/**** REMEMBER TO UPDATE annotated_term used in raw form by expand_macros
 **** and friends when changing the definition here
 **** definition now moved to kernel.pl, update it there
:- export struct(annotated_term(
	term,		% var, atomic or compound
	type,		% atom or var/1
	file,		% atom
	line,		% integer
	from,		% integer
	to		% integer
	% may be extended in future
    )).
****/

:- export read_annotated/2.
:- tool(read_annotated/2, read_annotated_/3).

read_annotated_(Stream, AnnTerm, Module) :-
	read_annotated_raw(Stream, RawAnnTerm, HasMacros, Module),
	( HasMacros == 1 ->
	    unannotate_term(RawAnnTerm, RawTerm),
	    expand_macros_annotated_(RawTerm, RawAnnTerm, _Term, AnnTerm, Module)
	;
	    AnnTerm = RawAnnTerm
	).


:- export read_annotated/3.
:- tool(read_annotated/3, read_annotated_/4).

read_annotated_(Stream, Term, AnnTerm, Module) :-
	read_annotated_raw(Stream, RawAnnTerm, HasMacros, Module),
	unannotate_term(RawAnnTerm, RawTerm),
	( HasMacros == 1 ->
	    expand_macros_annotated_(RawTerm, RawAnnTerm, Term, AnnTerm, Module)
	;
	    Term = RawTerm, AnnTerm = RawAnnTerm
	).


unannotate_term(end_of_file, Term) :- -?->
	Term = end_of_file.
unannotate_term(annotated_term{term:TermAnn}, Term) :- -?->
	( compound(TermAnn) ->
	    functor(TermAnn, F, A),
	    functor(Term, F, A),
	    unannotate_term_args(A, TermAnn, Term)
	;
	    Term = TermAnn
	).

    unannotate_term_args(0, _TermAnn, _Term) :- !.
    unannotate_term_args(I, TermAnn, Term) :-
	    I1 is I-1,
	    arg(I, TermAnn, AnnArg),
	    arg(I, Term, Arg),
	    unannotate_term(AnnArg, Arg),
	    unannotate_term_args(I1, TermAnn, Term).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% write the header for a .eco file

dump_header(Out) :-
	% magic .eco header (see procedure.c)
	put(Out, 16'EC), put(Out, 16'1C), put(Out, 16'29),
	put(Out, 16'16),	% ECO_CURRENT_VERSION, see procedure.c
	% flush before switching to scramble mode
	flush(Out),
	% next line contains key that must be used in the .eco loader
	set_stream_property(Out, scramble, 73540),
	% 8 random bytes to make decryption more difficult
	% (it may be better to have one after every dumped term)
	random(R), get_flag(unix_time, T),
	R1 is R/\255, R2 is R>>8/\255, R3 is R>>16/\255, R4 is R>>24/\255,
	R5 is T/\255, R6 is T>>8/\255, R7 is T>>16/\255, R8 is T>>24/\255,
	put(Out, R1), put(Out, R7), put(Out, R3), put(Out, R5),
	put(Out, R2), put(Out, R8), put(Out, R4), put(Out, R6).


% write a term in .eco format

dump_term(Out, Term, Module) :-
        term_to_bytes_(Term, String, Module),
        string_length(String, Length),
        write_integer(Out, Length),
        printf(Out, "%Tw", String).             % no macros!

write_integer(Out, N) :-
        Byte0 is N /\ 16'ff,
        Byte1 is (N >> 8) /\ 16'ff,
        Byte2 is (N >> 16) /\ 16'ff,
        Byte3 is (N >> 24) /\ 16'ff,
        put(Out, Byte3),
        put(Out, Byte2),
        put(Out, Byte1),
        put(Out, Byte0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode file_query_body(++, +, +).
file_query_body(call(Goal), _, M) :-		% call/1 forces execution
	!,
	call(Goal, M).
file_query_body((A, B), Proc, M) :-
	!,
	file_query_body(A, Proc, M),
	file_query_body(B, Proc, M).
file_query_body((A->B;C), Proc, M) :-
	!,
	(file_query_body(A, Proc, M) ->
	    file_query_body(B, Proc, M)
	;
	    file_query_body(C, Proc, M)
	).
file_query_body((A;B), Proc, M) :-
	!,
	(
	    file_query_body(A, Proc, M)
	;
	    file_query_body(B, Proc, M)
	).
file_query_body([File|L], Proc, M) :-
	!,
	call_proc(Proc, File, M),
	(L == [] ->
	    true
	;
	    file_query_body(L, Proc, M)
	).
file_query_body(compile(File), Proc, M) :-
	!,
	(File = [_|_] -> 
	    file_query_body(File, Proc, M)
	;
	    call_proc(Proc, File, M)
	).
file_query_body(ensure_loaded(Files), Proc, M) :-
	!,
	(Files = [_|_] -> 
	    file_query_body(Files, Proc, M)
	;
	    call_proc(Proc, Files, M)
	).
file_query_body(:-(Goal), Proc, M) :-
        !,
        file_query_body(Goal, Proc, M).
file_query_body(?-(Goal), Proc, M) :-
        !,
        file_query_body(Goal, Proc, M).
file_query_body(meta_attribute(_, _), _, M) :-
        !,
        meta_attribute(M, []).
file_query_body(Goal, _Proc, M) :-
	execute(Goal) ->
	    call(Goal, M)
	;
	    true.

:- mode execute(+).
execute(use_module(_)).
execute(define_struct(_)).	% library(structures)
execute(erase_struct(_)).
execute(op(_, _, _)).
execute(global_op(_, _, _)).
execute(local_op(_, _, _)).
execute(set_flag(A, _)) :- allowed_flag(A).
execute(get_flag(_, _)).
execute(define_global_macro(_, _, _)).
execute(define_local_macro(_, _, _)).
execute(define_macro(_, _, _)).
execute(erase_macro(_)).
execute(set_chtab(_, _)).
execute(asserta(_)).
execute(assert(_)).
execute(assertz(_)).
execute(compile_term(_)).
execute(cprolog).
execute(quintus).
execute(bsi).
execute(sicstus).

:- mode allowed_flag(+).
allowed_flag(library_path).
allowed_flag(macro_expansion).
allowed_flag(prolog_suffix).

call_proc(Proc, File, M) :-
	copy_term(Proc, Copy),
	arg(1, Copy, File),
	call(Copy, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	EXEC
%

exec(Command, Streams) :-
	exec(Command, Streams, Pid, 2),	% fails on error
	!,
	wait(Pid, Code),		% waitpid()
	( Code /\ 8'377 =:= 0 ->	% process exited normally
	    Status is Code >> 8 /\ 8'377,
	    Err is Status - 128,
	    (Err > 0 ->
		set_last_errno(Err),
		error(170, exec(Command, Streams))
	    ;
		true
	    )
	; Code /\ 8'377 =:= 8'177 ->	% process stopped
	    error(175, exec(Command, Streams))
	;				% process died
	    error(174, exec(Command, Streams))
	).
exec(Command, Streams) :-
	bip_error(exec(Command, Streams)).


exec(Command, Streams, Pid) :-
	exec(Command, Streams, Pid, 0), !.
exec(Command, Streams, Pid) :-
	bip_error(exec(Command, Streams, Pid)).

exec_group(Command, Streams, Pid) :-
	exec(Command, Streams, Pid, 1), !.
exec_group(Command, Streams, Pid) :-
	bip_error(exec_group(Command, Streams, Pid)).
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Sh
%

system(X) :-
	( get_flag('_system'/1, defined, on) ->
	    '_system'(X)
	;
	    concat_string(['/bin/sh -c "', X, '"'], Command),
	    exec(Command, [])
	).

sh(X) :-
	system(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	MACROS
%

phrase_body(Grammar, S, R, M) :-
	var(Grammar),
	!,
	error(4, phrase(Grammar, S, R), M).
phrase_body(Grammar, S, R, M) :-
	check_grammar(Grammar, S, R, M, NewGr),
	call(NewGr, M).

check_grammar(Grammar, S, R, M, NewGr) :-
	((number(Grammar) ; string(Grammar)) ->
	    error(5, phrase(Grammar, S, R), M)
	;
	    true
	),
	Grammar =.. [F | L],
	append(L, [S, R], NL),
	NewGr =.. [F | NL].

phrase_body(Grammar, S, M) :-
	phrase_body(Grammar, S, [], M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	FILES
%
get_file_info(File, Name, Value) :-
	var(File), 
	!,
	error(4, get_file_info(File, Name, Value)).
get_file_info(File, Name, Value) :-
	( atom(File) -> true
	; string(File) -> true
	),
	( var(Value) -> true
	; atomic(Value) -> true
	),
	( atom(Name) -> true
	; var(Name) -> true
	),
	!,
	expand_filename(File, ExpandedFile),  % File1 is a string
	do_get_file_info(ExpandedFile, Name, Value).
get_file_info(File, Name, Value) :-
	error(5, get_file_info(File, Name, Value)).


do_get_file_info(File, device, X) :-
	sys_file_flag(File, 9, X).
do_get_file_info(File, inode, X) :-
	sys_file_flag(File, 1, X).
do_get_file_info(File, mode, X) :-
	sys_file_flag(File, 0, X).
do_get_file_info(File, nlink, X) :-
	sys_file_flag(File, 2, X).
do_get_file_info(File, uid, X) :-
	sys_file_flag(File, 3, X).
do_get_file_info(File, uname, X) :-
	sys_file_flag(File, 15, X).
do_get_file_info(File, gid, X) :-
	sys_file_flag(File, 4, X).
do_get_file_info(File, gname, X) :-
	sys_file_flag(File, 16, X).
do_get_file_info(File, size, X) :-
	sys_file_flag(File, 5, X).
do_get_file_info(File, atime, X) :-
	sys_file_flag(File, 6, X).
do_get_file_info(File, adate, X) :-
	sys_file_flag(File, 12, X).
do_get_file_info(File, mtime, X) :-
	sys_file_flag(File, 7, X).
do_get_file_info(File, mdate, X) :-
	sys_file_flag(File, 13, X).
do_get_file_info(File, ctime, X) :-
	sys_file_flag(File, 8, X).
do_get_file_info(File, cdate, X) :-
	sys_file_flag(File, 14, X).
do_get_file_info(File, blksize, X) :-
	sys_file_flag(File, 11, X).
do_get_file_info(File, blocks, X) :-
	sys_file_flag(File, 10, X).
do_get_file_info(File, readable, X) :-
	process_file_permission(readable, N),
	sys_file_flag(File, N, X).
do_get_file_info(File, writable, X) :-
	process_file_permission(writable, N),
	sys_file_flag(File, N, X).
do_get_file_info(File, executable, X) :-
	process_file_permission(executable, N),
	sys_file_flag(File, N, X).
do_get_file_info(File, compiled_time, Time) :-
	current_compiled_file(File, Time, _Module, _Goal).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Remote Socket Interface
%
% Recorded information:
%      recorded_list(peer_info, PeerInfos) - Infotmation on all the peers
%
%  Each PeerInfos is a structure:
%
%         PeerName-peer_info(PeerType, Lang, Key, Connect)
%
%      PeerName: peer name (atom)
%      PeerType: either remote or embed. 
%      Lang:     programming language of the peer (atom).
%      Key:      the key used to access dynamic data associated with the peer.
%      Connect:  connection information. Either:
%                   remote(PeerHost,LocalHost,TimeOut)
%                   embed(PeerHost,LocalHost,TimeOut)
%
%            PeerHost is the hostname for the peer side. Used for verifying
%              that any accepted socket connection comes from the same host.
%            LocalHost is the ECLiPSe side hostname specification. This is
%              used in setting up a server connection to the peer side.
%              What is used has implications for what the remote side can
%              give for the server (ECLiPSe) hostname when making a client
%              connection:
%                   1) actual ECLiPSe side hostname: remote side must also 
%                      use this name for the server.
%                   2) localhost: remote side must used localhost for the
%                      server name. This restricts the connection to the 
%                      local machine.
%                   3) not instantiated: remote side can use either
%                      localhost or the actual server hostname. 
%              We use the same specification for all connections between
%              ECLiPSe and the peer for security reasons. 
%            TimeOut is the time-out (in seconds) for accepting any socket
%              connection, plus waiting time for the initial read of data
%              during attachment. Can be 'block' for no time-outs
%
% Currently, the only dynamic data associated with a peer are the peer queues.
% Each queue is an record item recorded under the Key for the peer as:
%
%         queue(StreamNum)
%
% From StreamNum, a queue key can be derived by calling get_peer_queue_key/2.
% Information for the queue is recorded under this key:
%
%         peer_queue(PeerType, PeerName, QueueType, Direction)
%
%      PeerType: peer type for the queue: Either embed or remote
%      PeerName: peer name for the queue.
%      QueueType: the type of the queue. This is either:
%             sync(SocketName): synchronous remote queue, with socket
%             async           : asynchronous remote queue
%             embed           : queue in a embedded peer
%      Direction: direction of queue. Either fromec, toec or bidirect
%
% The above information is used to clean up a remote side when it is 
% disconnected
%
% Dealing with events:
% To ensure that the remote interface protocol is followed at all times,
% event handling is deferred during most of the code below. We only
% allow events during
%	- running rpcs
%	- running user goals, e.g. remote_init
%	- remote flush (but not if inside ec_waitio)
% but these goals must be safely wrapped in block/3 to make sure the
% events are deferred again even on failure/throw.
% Note that, since we re-enable events temporarily from within events-deferred
% code, we cannot allow nesting, i.e. remote_accept, peer_queue_xxx and
% flush/waitio handlers must be called from non-events-deferred contexts.
%
% First clause is the current version of the remote protocol.
% The version information should not occur anywhere else on the ECLiPSe side.
%
 
remote_version(1).

:- local variable(rpeer_count, 0).
:- local variable(in_ec_waitio, []).
:- local struct(peer_info(type,lang,key,connect)).
:- local struct(peer_queue(ptype,pname,qtype,dir)).


non_interruptable(Goal) :-
	( events_defer ->
	    call(Goal),
	    events_nodefer
	;
	    printf(warning_output, "Warning: Illegal events_defer nesting detected during remote protocol (%w)",[Goal]),
	    call(Goal)
	).


remote_connect(Address, Control, Init, Mod) :-
	remote_connect_setup(Address, Control, Soc), !,
	printf(log_output, "Socket created at address %w%n", [Address]),
        remote_connect_accept(Control, Soc, block, Init, "", _, Mod). 
remote_connect(Address, Control, Init, _Mod) :-
	error(5, remote_connect(Address, Control, Init)).

remote_connect_setup(Host/Port, Control, Soc) :-
	(var(Port) ; integer(Port)),
	(var(Control) ; atom(Control)), 
	var(Soc), !,
        copy_term(Host,OrigHost), % OrigHost can be a variable
	new_socket_server(Soc, Host/Port, 2),
        (var(Control) ->
	    new_remote_peer_name(Control)
        ;   
	    not_existing_peer_name(Control)
	),
	recorda(remote_control_host, Control-OrigHost).
remote_connect_setup(Address, Control, Soc) :-
	error(5, remote_connect_setup(Address, Control, Soc)).

new_remote_peer_name(Name) :-
	repeat,
	incval(rpeer_count),
	getval(rpeer_count, NPeer),
	concat_atom([peer, NPeer], Name),
	not_existing_peer_name(Name), !.

not_existing_peer_name(Name) :-
% fails if Name is either an existing or potential peer
	\+ peer(Name), \+ recorded(remote_control_host, Name-_).


remote_connect_accept(Control, Soc, TimeOut, Init, Pass, Res, Mod) :-
	erase(remote_control_host, Control-Host), 
	get_rpcstream_names(Control, Rpc),
	timed_accept(Soc, TimeOut, RemoteHost, Control),
	check_remote_version(Control),
	timed_read_exdr(Control, TimeOut, Pass0), 
        (Pass == Pass0 -> true ; set_bip_error(1)),
	write_exdr(Control, Control), flush(Control),
	% Host is the host name that will be used in any subsequent connections
        timed_read_exdr(Control, TimeOut, RemoteLang),
	timed_accept(Soc, TimeOut, RemoteHost, Rpc),
	write_exdr(Rpc, Control), flush(Rpc),
	set_peer_property(Control, peer_info{type:remote,lang:RemoteLang,
                                     connect:remote(RemoteHost,Host,TimeOut)}),
	set_event_handler(Control, true/0),
	close(Soc),
	events_defer,	% fail if already deferred (can't handle nesting)
	!,
	block((
		run_remote_init(Init, Res, Mod),
		remote_control_read(Control, Message),
		handle_ec_resume(Message, Control),
		events_nodefer
	    ), Tag, (
		events_nodefer,
		exit_block(Tag)
	    )).
remote_connect_accept(Control, Soc, TimeOut, Init, Pass, Res, Mod) :-
	(nonvar(Soc),current_stream(Soc) -> close(Soc) ; true),
	(nonvar(Control), current_stream(Control) -> close(Control) ; true),
	get_bip_error(Err),
	error(Err, remote_connect_accept(Control, Soc, TimeOut, Init, Pass, Res, Mod)).


check_remote_version(Control) :-
	(timed_read_exdr(Control, 100, RemoteVersion) -> 
             true ; set_bip_error(6)
        ),
	get_flag(remote_protocol_version, Version),
	(RemoteVersion == remote_protocol(Version) ->
	     write_exdr(Control, "yes"), flush(Control)
	;    write_exdr(Control, Version), flush(Control),
	     printf(error, "Incompatible remote protocol on remote side: %w%n",
                 [RemoteVersion]),
	     set_bip_error(141)
	).

timed_read_exdr(Stream, TimeOut, Data) :-
	select([Stream], TimeOut, [Stream]),
	block(read_exdr(Stream, Data), _, fail).

timed_accept(Server, TimeOut, RemoteHost, NewQueue) :-
	select([Server], TimeOut, [Server]),
	accept(Server, RemoteHost0/_, NewQueue), 
	(RemoteHost = RemoteHost0 ->
            true ; close(NewQueue), fail
        ).


% events deferred!
run_remote_init(Init, Res, Mod) :-
	( nonvar(Init), var(Res) ->
	    block((
		     events_nodefer,
		     (call(Init)@Mod -> Res = Init ; Res = fail),
		     events_defer
		 ),
		 _,
		 (events_defer, Res = throw)
            )
	; nonvar(Res) ->
	      printf(warning_output, "Warning: result argument %w for initial goal not a variable in remote_control_accept/5. Initial Goal not executed.", [Res])
	; true
	).

peer_info(Peer, Info) :-
	recorded(peer_info, Peer-Info).

peer(Peer) :- 
	( var(Peer) ->
	     peer_info(Peer, _)
	; atom(Peer) ->
	     peer_info(Peer, _), !
	; error(5, peer(Peer))
	).

peer_get_property(Peer, Property, Value) :-
	(atom(Peer),
	(var(Property) ; atom(Property)) -> 
	    true ; set_bip_error(5)
	), !,
	once(peer_info(Peer, Info)),
	get_a_peer_property(Property, Info, Value).
peer_get_property(Peer, Property, Value) :-
	get_bip_error(Err),
	error(Err, peer_get_property(Peer,Property,Value)).

set_embed_peer(Peer, Lang) :-
	\+peer(Peer),
	get_flag(hostname, Host),
	set_peer_property(Peer, peer_info{type:embed,lang:Lang,connect:embed(Host,Host,block)}).

% all the predicates that access peer_info directly should be put here
get_embed_peer(Peer) :- 
	recorded(peer_info, Peer-(peer_info{type: embed})), !.

set_peer_property(Peer, Info) :-
	get_peer_dyn_info_key(Peer, Key),
        Info = peer_info{key: Key},
	recorda(peer_info, Peer-Info).

get_a_peer_property(type, peer_info{type:Type}, Type).
get_a_peer_property(language, peer_info{lang:Lang}, Lang).
get_a_peer_property(connect, peer_info{connect:Connect}, Connect).
get_a_peer_property(queues, peer_info{key:Key}, Qs) :-
	findall(Queue,recorded(Key,queue(Queue)), Qs).


peer_queue_get_property(Queue, Prop, Value) :- 
	(atom(Queue);integer(Queue)),
	(atom(Prop);var(Prop)), 
	!,
	get_queueinfo_st(Queue, _, QueueInfo),
	get_queueinfo_item(Prop, QueueInfo, Value).
peer_queue_get_property(Queue, Prop, Value) :- 
	error(5, peer_queue_get_property(Queue,Prop,Value)).


get_queue_info(Name, Nr, Peer, QType, Dir) :-
	get_queueinfo_st(Name, Nr, QueueInfo),
	get_queueinfo_item(peer, QueueInfo, Peer),
	get_queueinfo_item(type, QueueInfo, QType),
	get_queueinfo_item(direction, QueueInfo, Dir).

get_queueinfo_st(Name, Nr, QueueInfo) :-
	current_stream(Name),
	get_stream_info(Name, physical_stream, Nr),
	get_peer_queue_key(Nr, Key),
	recorded(Key, QueueInfo), !.

get_queueinfo_item(peer_type,  peer_queue{ptype:PeerType}, PeerType).
get_queueinfo_item(peer_name,  peer_queue{pname:Peer}, Peer).
get_queueinfo_item(type,      peer_queue{qtype:Type}, Type).
get_queueinfo_item(direction, peer_queue{dir:Direction}, Direction).
get_queueinfo_item(peer,  peer_queue{ptype:PeerType,pname:Peer},
                   PInfo) :- % for backwards compatibility
        PInfo =.. [PeerType, Peer].
        

is_remote_sync_queue(PhysicalStream, Socket, ControlStream) :-
	peer_queue_get_property(PhysicalStream, peer, remote(ControlStream)),
	peer_queue_get_property(PhysicalStream, type, sync(Socket)).


deregister_queue(Stream, Control) :-
	get_stream_info(Stream, physical_stream, StreamNum),
	get_peer_queue_key(StreamNum, Key),
	erase_all(Key),
	get_peer_dyn_info_key(Control, ControlKey),
	recorded(ControlKey, queue(StreamNum), Ref), !,
	erase(Ref).

register_remote_queue(Name, Control, Type, Direction) :-
	get_stream_info(Name, physical_stream, Nr),
	get_peer_queue_key(Nr, Key),
	get_peer_dyn_info_key(Control, ControlKey),
	recorda(ControlKey, queue(Nr)),
	recorda(Key, peer_queue{ptype:remote, pname:Control, qtype:Type, dir:Direction}).


register_embed_queue(Name, Peer, Direction) :-
	get_stream_info(Name, physical_stream, Nr),
	get_peer_queue_key(Nr, Key),
	peer_get_property(Peer, type, embed),
	get_peer_dyn_info_key(Peer, PeerKey),
	recorda(PeerKey, queue(Nr)),
	recorda(Key, peer_queue{ptype:embed, pname:Peer, qtype:sync(Nr), dir:Direction}).
	
	
get_peer_dyn_info_key(Control, ControlKey) :-
	concat_atom([peer_dynamic_info, Control], ControlKey).

get_peer_queue_key(N, Key) :-
	concat_atom([peer_queue, N], Key).


new_socket_server(Soc, Address, N) :-
	socket(internet, stream, Soc), 
	block(
          (bind(Soc, Address), listen(Soc, N)
          ), Tag, (close(Soc), exit_block(Tag))
        ).



peer_queue_close(Queue) :-
	(atom(Queue) ; integer(Queue)), !,
	get_queue_info(Queue, StreamNum, Peer, QType, _Direction),
	non_interruptable(
	    close_peer_queue_type(Peer, StreamNum, QType)
	).
peer_queue_close(Queue) :-
	error(5, peer_queue_close(Queue)).


    close_peer_queue_type(remote(Peer), StreamNum, QType) :-
	remote_control_send(Peer, queue_close(StreamNum)),
	remote_control_read(Peer, ResumeMessage),
	close_remote_queue_eclipseside(Peer, StreamNum, QType),
	handle_ec_resume(ResumeMessage, Peer).
	close_peer_queue_type(embed(Peer), StreamNum, _QType) :-
	write_exdr(embed_info, queue_close(StreamNum)),
	flush(embed_info),
	close_embed_queue_eclipseside(Peer, StreamNum).

    close_embed_queue_eclipseside(Peer, StreamNum) :-
	deregister_queue(StreamNum, Peer),
	close(StreamNum).

    close_remote_queue_eclipseside(Control, StreamNum, QType) :-
	deregister_queue(StreamNum, Control),
	close_remote_physical_streams(QType, StreamNum).

    close_remote_physical_streams(sync(Socket), StreamNum) :-
	(current_stream(StreamNum) -> close(StreamNum) ; true),
	(current_stream(Socket) -> close(Socket) ; true).
    close_remote_physical_streams(async, StreamNum) :-
	(current_stream(StreamNum) -> close(StreamNum) ; true).



peer_queue_create(Name, Control, Sync, Direction, Event) :-
	non_interruptable(
	    peer_queue_create1(Name, Control, Sync, Direction, Event)
	).
	

peer_queue_create1(Name, Control, Sync, Direction, Event) :-
	(atom(Name), atom(Control), is_event(Event) -> 
            true ; set_bip_error(5)
        ),
	peer_get_property(Control, connect, Type),
	(Sync == sync ->
	    (Direction == fromec ; Direction == toec ; set_bip_error(6))
	;
	 (Sync == async,  functor(Type,remote,_))
        ;
	 set_bip_error(6)
        ), !,
	create_peer_queue_type(Type, Name, Control, Sync, Direction, Event).
peer_queue_create1(Name, Control, Sync, Direction, Event) :-
	get_bip_error(E),
	error(E, peer_queue_create(Name, Control, Sync, Direction, Event)).


    % events deferred!
    create_peer_queue_type(remote(PeerHost,LocalHost,TimeOut), Name, Control, Sync, Direction, Event) ?-
	new_socket_server(Soc, LocalHost/Port, 1),
	remote_control_send(Control, socket_client(Port, Name, Sync, Direction)),
	remote_control_read(Control, ResumeMessage),
	(is_disconnection(ResumeMessage) ->
	    close(Soc),
	    handle_ec_resume(ResumeMessage, Control)
	; 
	 ResumeMessage = socket_connect(Name,Status) ->
	    connect_remote_queue(Status, Soc, Name, Control, Sync, Direction, Event, TimeOut, PeerHost, Return),
	 Return \== fail % fails if connection failed
	;
	 printf(error, "Unexpected control message %w while creating peer queue %w on remote side %w; disconnecting.%n", [ResumeMessage, Name, Control]),
         close(Soc), 
	 handle_ec_resume(disconnect, Control)
        ).
    create_peer_queue_type(embed(_,_,_), Name, _Peer, _Sync, Direction, Event) ?-
	ecl_create_embed_queue(Name, Direction, Event),
	get_stream_info(Name, physical_stream, Nr),
	write_exdr(embed_info, queue_connect(Name, Nr, Direction)),
	flush(embed_info).


    ecl_create_embed_queue(Name, Direction, Event) :-
	get_embed_peer(Peer),
	(Direction == fromec ->
	    Options = [yield(on)],
	    Mode = write
	;
	    (Event == '' -> Options = [yield(on)] ; Options = [event(Event)]),
	    Mode = read
	), 
	open(queue(""), Mode, Name, Options),
	register_embed_queue(Name, Peer, Direction).


    is_disconnection(disconnect).
    is_disconnection(disconnect_resume).
    is_disconnection(end_of_file).

    % events deferred!
    connect_remote_queue(success, Soc, Name, Control, Sync, Direction, Event, TimeOut, RHost, StreamId) :-
	block(
	      (create_remote_queue(Sync, Direction, Soc, Name, Control, TimeOut, RHost, Event) ->
                   get_stream_info(Name, physical_stream, StreamId)
	      ;    
		  % Timed out or other problem
		  close(Soc),
		  StreamId = fail
              ), _, ((current_stream(Soc) -> close(Soc);true), StreamId = fail)
	),
	remote_control_send(Control, socket_accept(Name,StreamId)),
	remote_control_read(Control, ResumeMessage),
	handle_ec_resume(ResumeMessage, Control).
    connect_remote_queue(fail, Soc, Name, Control, _, _, _, _, _, StreamId) :-
	close(Soc),
	StreamId = fail,
	remote_control_send(Control, socket_accept(Name, fail)),
	remote_control_read(Control, ResumeMessage),
	handle_ec_resume(ResumeMessage, Control).


    create_remote_queue(async, _, Soc, Name, Control, TimeOut, RHost, Event) ?-
	remote_create_async_queue(Soc, Name, Control, TimeOut, RHost, Event).
    create_remote_queue(sync, fromec, Soc, Name, Control, TimeOut, RHost, Event) ?-
	remote_create_fromec_queue(Soc, Name, Control, TimeOut, RHost, Event).
    create_remote_queue(sync, toec, Soc, Name, Control, TimeOut, RHost, Event) ?-
	remote_create_toec_queue(Soc, Name, Control, TimeOut, RHost, Event).

    % memory queue needed to allow eof event to be raised reading empty queue
    remote_create_toec_queue(Soc, Name, Control, TimeOut, RemoteHost, Event) :-
	open(queue(""), update, Name),
	concat_atom([Name, soc], SocName),
	timed_accept(Soc, TimeOut, RemoteHost, SocName),
	close(Soc),
	(Event == '' -> 
	    set_stream_property(Name, yield, on) 
        ;   set_stream_property(Name, event, Event)
        ),
	register_remote_queue(Name, Control, sync(SocName), toec).


    % memory queue needed for buffering output. 
    % Event is dummy for now, to be used for remote side requesting data
    remote_create_fromec_queue(Soc, Name, Control, TimeOut, RemoteHost, _Event) :-
	open(queue(""), update, Name, [yield(on)]),
	concat_atom([Name, soc], SocName),
	timed_accept(Soc, TimeOut, RemoteHost, SocName),
	close(Soc), 
	register_remote_queue(Name, Control, sync(SocName), fromec).

    remote_create_async_queue(Soc, Name, Control, TimeOut, RemoteHost, Event) :-
    % use Control to remember which remote process this stream is connected to
	timed_accept(Soc, TimeOut, RemoteHost, Name),
	(Event == '' -> 
	    true
        ;   set_stream_property(Name, event, Event)
        ),
	close(Soc),
	register_remote_queue(Name, Control, async, bidirect).


% returns end_of_file as a message if something goes wrong
remote_control_read(Control, Message) :-
	block((read_exdr(Control, Message) -> true ; Message = end_of_file),
	           _, Message = end_of_file
	).

% catches any prblems before sending control message
remote_control_send(Control, Message) :-
	(select([Control], 0, [Control]) ->
	    % unexpected message arrived on control stream
	    remote_control_read(Control, InMessage),
	    ((InMessage == disconnect_resume; InMessage == end_of_file) ->
		% unilateral disconnect from remote side; disconnect locally now
		remote_cleanup(Control),
		exit_block(peer_abort_disconnected)
	    ;   printf(error, "Unexpected incoming message %w on remote %w.\n", [InMessage,Control]),
	        exit_block(peer_abort_error)
	    )
	;
	    write_exdr(Control, Message),
	    flush(Control)
	).


:- local finalization(disconnect_remotes).

disconnect_remotes :-
	recorded_list(peer_info, Remotes),
	disconnect_remotes(Remotes).

disconnect_remotes([]).
disconnect_remotes([Control-_|Controls]) :-
	remote_disconnect(Control),
	disconnect_remotes(Controls).


remote_disconnect(Control) :-
	((nonvar(Control), current_stream(Control),
          peer_get_property(Control,type,remote)
         ) ->
	    remote_control_send(Control, disconnect),
	    (read_exdr(Control, disconnect_resume) ->
		remote_cleanup(Control)
	    ;   % if not resume, then problem....
	        true
	    )
	;   true  % Control is not a current remote peer...
	).


% events not deferred!
remote_output(PhysicalStream, ControlStream, RemoteStream) :-
	non_interruptable((
	    read_string(PhysicalStream, end_of_file, Len, Data), 
	    yield_to_remote(ControlStream, ec_flushio(PhysicalStream, Len), RemoteStream, Data)
	)).

    % events deferred!
    yield_to_remote(ControlStream, YieldMessage, DataStream, Data) :-
	remote_control_send(ControlStream, YieldMessage),
	write(DataStream, Data),
	flush(DataStream),
	remote_control_read(ControlStream, ResumeMessage),
	handle_ec_resume(ResumeMessage, ControlStream).


% events not deferred!
remote_input(PhysicalStream, ControlStream) :-
	non_interruptable((
	    remote_control_send(ControlStream, ec_waitio(PhysicalStream)),
	    wait_for_remote_input(PhysicalStream, ControlStream)
	)).

    % wait for remote input to arrive, handle any messages before this,
    % data is then copied from the socket to the queue stream (physical stream)
    % events deferred!
    wait_for_remote_input(PhysicalStream, ControlStream) :-
	% we expect at least one rem_flushio-message and a resume
	setval(in_ec_waitio, PhysicalStream),
	remote_control_read(ControlStream, Message0),
	expect_control(ControlStream,
		[rem_flushio(PhysicalStream, _), rem_flushio(PhysicalStream)],
		Message0, Message1),
	handle_control(Message1, ControlStream, Message2),
	setval(in_ec_waitio, []),
	expect_control(ControlStream, [resume], Message2, _).


remote_rpc_handler(Rpc, Control) :-
	% The socket rpc can only handle a single rpc 
	% the rpc goal corresponding to the control message must eventually
        % arrive on the Rpc socket stream
	select([Rpc], block, [Rpc]), % wait until Rpc stream is ready..
	block(execute_remote_rpc(Rpc, Control), _, handle_remote_rpc_throw(Rpc, Control)).

    execute_remote_rpc(Rpc, Control) :-
	read_exdr(Rpc, Goal),
	events_nodefer,
	execute_rpc(Rpc, Goal, (
		events_defer,
		remote_control_send(Control,yield)
	    )).

    handle_remote_rpc_throw(Rpc, Control) :-
	events_defer,
	remote_control_send(Control, yield),
	write_exdr(Rpc, throw), flush(Rpc).


% Handle initial message Message0 (and possibly further messages on Control)
% until we get one of the messages specified in the list Expected.
% The expected message itself is not handled, but returned as ExpectedMessage.

% events deferred!
expect_control(Control, Expected, Message0, ExpectedMessage) :-
	( nonmember(Message0, Expected) ->
	    ( Message0 = resume ->
		printf(warning_output,
		    "Unexpected resume from remote peer %w while waiting for %w%n%b",
		    [Control, Expected]),
		% yield back and hope for the best
		remote_yield(Control, Message1)
	    ;
		% some other message, try to process it
		handle_control(Message0, Control, Message1)
	    ),
	    expect_control(Control, Expected, Message1, ExpectedMessage)
	;
	    ExpectedMessage = Message0
	).


% Handle initial message Message (and possibly further messages on Control).
% Return as soon as we get a resume message.

% events deferred!
handle_ec_resume(Message, Control) :-
	expect_control(Control, [resume], Message, _Message).


% events deferred!
handle_control(rpc, Control, NextMsg) :- -?->  !, % rpc call
	get_rpcstream_names(Control, Rpc),
	remote_rpc_handler(Rpc, Control),
	remote_control_read(Control, NextMsg).
handle_control(disconnect, Control, _NextMsg) :- -?->  !, % disconnect request
	write_exdr(Control, disconnect_yield), % acknowledge disconnect
	flush(Control),
	remote_cleanup(Control),
	exit_block(peer_abort_disconnected).
handle_control(rem_flushio(Queue), Control, NextMsg) :- -?-> !, 
	get_stream_info(Queue, device, Device),
	deal_with_remote_flush(Device, Queue, unknown),
	remote_yield(Control, NextMsg).
handle_control(rem_flushio(Queue, Len), Control, NextMsg) :- -?-> !,
	get_stream_info(Queue, device, Device),
	deal_with_remote_flush(Device, Queue, Len),
	remote_yield(Control, NextMsg).
handle_control(queue_create(Name,Sync,Direction,Event), Control, NextMsg) :- -?-> !,
	block((
	   peer_queue_create1(Name, Control, Sync, Direction, Event) -> true;true),
           _, true
        ),
	remote_yield(Control, NextMsg).
handle_control(queue_close(Queue), Control, NextMsg) :- -?-> !,
	((current_stream(Queue),get_queue_info(Queue, Queue, remote(Control), QType, _)) ->
	    close_remote_queue_eclipseside(Control, Queue, QType)
	;   % not a remote queue, just ignore
	    true
	), remote_yield(Control, NextMsg).
handle_control(disconnect_resume, Control, _NextMsg) :- -?-> !,
% remote side already disconnected, no acknowledgement
	remote_cleanup(Control),
	exit_block(peer_abort_disconnected).
handle_control(end_of_file, Control, _NextMsg) :- -?-> !,
% Control is disconnected. Assume remote side disconnected unexpectedly
	remote_cleanup(Control),
	exit_block(peer_abort_disconnected).
handle_control(Message, Control, NextMsg) :-
	printf(error, "Unrecognised control signal %w; disconnecting.%n",
            [Message]),
	handle_control(disconnect, Control, NextMsg).


% events deferred!
deal_with_remote_flush(Device, Queue, Len) :-
	( getval(in_ec_waitio, Queue) ->
	    % this flush is the input corresponding to a ec_waitio
	    % don't handle events
	    block((
		deal_with_remote_flush1(Device, Queue, Len) -> true ; true
		), _, true)	% ignore any problems with the handler

	; events_nodefer ->
	    % handle events during remote flush
	    block((
		deal_with_remote_flush1(Device, Queue, Len) -> true ; true
		), _, true),	% ignore any problems with the handler
	    events_defer
	;
	    printf(error, "Unexpected events_nodefer state in remote flush %w%n", [Queue])
	).

    deal_with_remote_flush1(socket, Queue, Len) ?- !,
	% raw socket, is an asyn. queue; user process the data
	get_stream_info(Queue, event, Event),
	error(Event, rem_flushio(Queue, Len)).
    deal_with_remote_flush1(_, Queue, Len) :-
	% non-socket case, read data into a buffer
	peer_queue_get_property(Queue, type, sync(SockName)),
	read_sync_data_to_buffer(Len, Queue, SockName).

    read_sync_data_to_buffer(Len, Queue, SockName) :-
	(integer(Len) ->
	    (read_string(SockName, end_of_file, Len, Data) -> true ; Data = end_of_file),
	    write(Queue, Data)
	;   % Length unknown, read as exdr term	
	    (read_exdr(SockName, Data) -> true ; Data = end_of_file),
	    write_exdr(Queue, Data) 
	).


% make remote_cleanup more robust so that problems will not choke eclipse
% events deferred on entry, undeferred om exit
remote_cleanup(Control) :-
	block(remote_cleanup_raw(Control), _, fail), !.
remote_cleanup(Control) :-
	printf(error, "Problem with cleaning up remote peer %w.%n", [Control]).


remote_cleanup_raw(Control) :-
	events_nodefer,			% to make next line work
	(event(Control) -> true ; true), % user defined cleanup 
	reset_event_handler(Control),
	get_peer_dyn_info_key(Control, ControlKey),
	% get all the socket streams associated with this remote process
	recorded_list(ControlKey, RemoteDynInfo), 
	cleanup_dynamic_infos(RemoteDynInfo, Control),
        cleanup_peer_multitask_infos(Control),
	erase_all(ControlKey),
	get_rpcstream_names(Control, Rpc),
	(erase(peer_info, Control-_) -> true;true), 
	close(Rpc), 
	close(Control).

cleanup_dynamic_infos([Item|Infos], Control) :-
	(Item = queue(Queue) -> 
             get_queue_info(Queue, StreamNum, remote(Peer), QType, _Dir),
	    close_remote_queue_eclipseside(Peer, StreamNum, QType)
	;
	    true
	),
	cleanup_dynamic_infos(Infos, Control).
cleanup_dynamic_infos([], _).


% events deferred!
remote_yield(Control, ResumeMessage) :-
	nonvar(Control), 
        peer(Control), 
	current_stream(Control),
	remote_control_send(Control, yield),
	remote_control_read(Control, ResumeMessage).

% events deferred!
remote_yield(Control) :-
	remote_yield(Control, ResumeMessage),
	handle_ec_resume(ResumeMessage, Control).


get_rpcstream_names(Control, Rpc) :-
	concat_atom([Control, '_rpc'], Rpc).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Peer Multitasking

:- local struct(mt_peer(peer,msgq)).
:- local variable(peer_mt_status).
:- setval(peer_mt_status, off).

peers_are_multitasking :-
        \+getval(peer_mt_status, off).

peers_should_cycle :-
        getval(peer_mt_status, mt_set).


peer_register_multitask(Peer, MsgQ) :-
        (peer(Peer) ->
             \+ recorded(multitask_peers, mt_peer{peer:Peer}),
             concat_atom([Peer, multifrom], MsgQ),
             peer_queue_create(MsgQ, Peer, sync, fromec, ''),
             record(multitask_peers, mt_peer{peer:Peer,msgq:MsgQ})
        ;
             error(6, peer_register_multitask(Peer, MsgQ))
        ).

peer_deregister_multitask(Peer) :-
        (peer(Peer) ->
             recorded(multitask_peers, mt_peer{peer:Peer,msgq:MsgQ}),
             cleanup_peer_multitask_infos(Peer),
             peer_queue_close(MsgQ)
        ;
             error(6, peer_deregister_multitask(Peer))
        ).
             
peer_do_multitask(Type) :-
        \+peers_are_multitasking,
        /* multitasking will terminate if peers do not confirm multitasking */
        block(( (peer_multitask_terminate,
                 peer_multitask_phase(Type, Err)
                )-> true
              ;     peer_end_multitask(Err)
              ), 
              Tag, (peer_end_multitask(_Err2), Tag = Err)
        ),
        (nonvar(Err) -> exit_block(Err) ; true).

    peer_multitask_phase(Type, Err) :-
        peers_mt_broadcast_with_cleanup(start_multitask(Type), Err),
        (nonvar(Err) -> true ; peers_mt_cycle(Err)),
        peer_end_multitask(Err).

/* ensure that multitask phase is ended properly: if failure or 
   exit_block occurs, broadcast end_multitask again */
peer_end_multitask(Err) :-
        block(( (peers_mt_broadcast_with_cleanup(end_multitask, Err),
                 peer_multitask_off
                 ) -> true
                ;     peer_end_multitask(Err)
              ), _, peer_end_multitask(_)).


peer_multitask_terminate :-    setval(peer_mt_status, mt_reset).
peer_multitask_confirm   :-    setval(peer_mt_status, mt_set).
peer_multitask_off   :-    setval(peer_mt_status, off).

% avoids pushing witness pword onto global stack by avoiding a CP here
% all peer_mt_status state must be given by the clauses
do_peers_mt_cycle(mt_set, Err) ?-
        sleep(0.01), 
        peers_mt_broadcast_with_cleanup(interact, Err),
        peers_mt_cycle(Err).
do_peers_mt_cycle(mt_reset, _Err) ?- true.
do_peers_mt_cycle(off, _Err) ?- true.

peers_mt_cycle(Err) :-
        getval(peer_mt_status, Status),
        do_peers_mt_cycle(Status, Err).

peers_mt_broadcast_with_cleanup(Msg, Err) :-
        % rollback the garbage generated by peers_mt_broadcast/2
        % if no error occurred
        (peers_mt_broadcast(Msg, Err), nonvar(Err) -> true ; true).


peers_mt_broadcast(Msg, Err) :- 
        recorded_list(multitask_peers, Ps),
        (Ps \== [] ->
             peers_mt_broadcast1(Ps, Msg, Err)
        ;
             peer_multitask_terminate,
             (Err = peer_multitask_empty -> true; true)
        ).

peers_mt_broadcast1([], _, _).
peers_mt_broadcast1([mt_peer{peer:Peer,msgq:MQ}|Ps], Msg, Err) :-
        block(send_mt_message(MQ, Msg), Tag,
              peer_mt_error_recover(Tag,Peer,Err)),
        peers_mt_broadcast1(Ps, Msg, Err).


send_mt_message(ToPQ, Msg) :-
        % ignore failure (invalid terms substituted by _)
        (write_exdr(ToPQ, Msg) -> true;true),
        flush(ToPQ).

% First case happens if a remote peer has disconnected. In this case, the
% remote peer code should have cleaned up already
peer_mt_error_recover(peer_abort_disconnected, _, _) :- !. 
peer_mt_error_recover(abort, _Peer, Err) :- !,
        % abort raised. Stop multitasking and allow abort to continue
        peer_multitask_terminate,
        (Err = abort -> true ; true).
peer_mt_error_recover(Tag, Peer, Err) :- 
        % something went wrong, remove problematic peer from multitasking
        % list and end multitask, follow by aborting with first error
        peer(Peer), 
        cleanup_peer_multitask_infos(Peer),
        peer_multitask_terminate,
        (Tag = Err -> true ; true).

cleanup_peer_multitask_infos(Peer) :-
        (erase(multitask_peers, mt_peer{peer:Peer}) -> true ; true).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Allow users to trace grammar rules through phrase/2/3
?- unskipped
	phrase_body/3,
	phrase_body/4.

% tool interfaces must be set to skipped explicitely
:- skipped
	file_query/2,
	global_op/3,
	op/3,
	read_token/2.

% Set all output predicates to skipped in order not to trace the
% flush event handler (io_yield_handler) when it happens.
:- skipped
	flush/1,
	display/1,
	display/2,
	nl/0,
	nl/1,
	put/1,
	put/2,
	print/1,
	print/2,
	printf/2,
	printf/3,
	tyo/1,
	tyo/2,
	write/1,
	write/2,
	write_canonical/1,
	write_canonical/2,
	write_exdr/2,
	write_term/2,
	write_term/3,
	writeln/1,
	writeln/2,
	writeq/1,
	writeq/2.

:- untraceable
	make/0.

:- export
	file_query/2.
