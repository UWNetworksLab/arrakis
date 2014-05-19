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
% Version:	$Id: environment.pl,v 1.6 2008/08/22 15:04:46 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 * IDENTIFICATION:	environment.pl 
 *
 * DESCRIPTION: 	
 *
 *
 * CONTENTS:
 *			op/3
 *			local_op/3
 *			get_flag/2
 *			set_flag/2
 *			env/0
 *			statistics/0
 *			statistics/1
 *			statistics/2
 *			help/0
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- system.

:- export
	(help)/1,
	statistics/0,
	statistics/1,
	statistics/2,
	env/0,
	get_flag/2,
	set_flag/2.

%----------------------------------------
% get_flag(+Name, ?Value)
%----------------------------------------

:- skipped(get_flag/2).

get_flag_body(Name, Value, M) :-
	atom(Name) ->
	    do_get_flag(Name, Value, M)	% range check missing here !
	;
	var(Name) ->
	    do_get_flag(Name, Value, M)
	;
	    error(5, get_flag(Name, Value)).


do_get_flag(all_dynamic, off, _).
do_get_flag(break_level, X, _) :- getval(break_level, X).
do_get_flag(coroutine,X, _) :-
	global_flags(0,0,F),
	(F /\ 16'00000100 =:= 0 -> X=off ; X=on).
do_get_flag(cwd,X, _) :- getcwd(X).
do_get_flag(debugging, X, _) :-
	getval(toplevel_trace_mode, X).
do_get_flag(debug_compile,X, _) :-
	global_flags(0,0,F),
	(F /\ 16'00000080 =:= 0 -> X=off ; X=on).
do_get_flag(default_language, Language, _) :-
	getval(default_language, Language).
%do_get_flag(debugger_warnings,X, _) :-
%	sys_flags(4, Y), (Y = 0 -> X = off; X = on).
do_get_flag(dfid_compile,X, _) :-
	get_flag(extension, dfid),
	global_flags(0,0,F),
	(F /\ 16'01000000 =:= 0 -> X=off ; X=on).
do_get_flag(enable_interrupts, X, _) :-
	vm_flags(0,0,F),
	(F /\ 16'00000002 =:= 0 -> X=on ; X=off).
do_get_flag(breal_exceptions, X, _) :-
	global_flags(0,0,F),
	(F /\ 16'00000001 =:= 1 -> X=on ; X=off).
do_get_flag(float_precision, double, _).
do_get_flag(gc,X, _) :-
	global_flags(0,0,F),
	( F /\ 16'00000050 =:= 16'00000010 -> X=on
	; (F /\ 16'00000050 =:= 16'00000050 -> X=verbose ; X=off)
	).
do_get_flag(gc_policy,X, _) :-
	global_flags(0,0,F),
	(F /\ 16'00004000 =\= 0 -> X=adaptive
	; X=fixed).
%do_get_flag(gc_incr,X, _) :-
%	global_flags(0,0,F),
%	(F /\ 16'00000200 =:= 0 -> X=on ; X=off).
do_get_flag(gc_interval,X, _) :-	gc_interval(Y), X=Y.
do_get_flag(gc_interval_dict,X, _) :-	dict_param(7,Y), X=Y.
do_get_flag(goal_expansion,X, _) :-
	global_flags(0,0,F),
	(F /\ 16'00000800 =:= 0 -> X=off ; X=on).
do_get_flag(hostid,X, _) :-	get_sys_flag(1, X).
do_get_flag(hostname,X, _) :-	get_sys_flag(2, X).
do_get_flag(hostarch,X, _) :-	get_sys_flag(8, X).
do_get_flag(ignore_eof,X, _) :-	getval(ignore_eof, X).
do_get_flag(after_event_timer,X, _) :- getval(after_event_timer, X).
do_get_flag(installation_directory,X, _) :-	getval(sepiadir, X).
do_get_flag(last_errno, X, _) :-	get_last_errno(X).
do_get_flag(loaded_library, LibName, _) :-
	getval(library_path, LibPath),
	member(LibDir, LibPath),
	canonical_path_name(LibDir, CanonicalLibDir),
	current_compiled_file(File, _,  _, _),
	pathname(File, CanonicalLibDir, LibNameS, _),
	atom_string(LibName, LibNameS).
do_get_flag(tmp_dir, TmpDir, _) :-
        getval(eclipse_tmp_dir, TmpDir).
do_get_flag(macro_expansion,X, _) :-
	global_flags(0,0,F),
	(F /\ 16'00000400 =:= 0 -> X=off ; X=on).
do_get_flag(max_global_trail,X, _) :- gc_stat(15,X).
do_get_flag(max_local_control,X, _) :- gc_stat(23,X).
do_get_flag(max_predicate_arity,255, _).	% keep consistent with MAX_ARITY !
do_get_flag(object_suffix,X, _) :-	get_sys_flag(9, X).
do_get_flag(occur_check,X, _) :-
	get_flag(extension, occur_check),
	global_flags(0,0,F),
	(F /\ 16'02000000 =:= 0 -> X=off ; X=on).
do_get_flag(output_mode, X, _) :-	output_mode(X).
do_get_flag(output_options, X, _) :-
	output_mode_mask(On),
	sys_flags(1, Depth),
	options_from_format(On, Depth, X).
do_get_flag(pid,X, _) :-		get_sys_flag(3, X).
do_get_flag(ppid,X, _) :-		get_sys_flag(4, X).
do_get_flag(prefer_rationals, X, _) :-
	global_flags(0,0,F),
	(F /\ 16'00000002 =:= 0 -> X=off ; X=on).
do_get_flag(print_depth,X, _) :-	sys_flags(1, Y), X=Y.
do_get_flag(prolog_suffix, X, _) :- getval(prolog_suffix, X).
do_get_flag(eclipse_object_suffix, X, _) :- getval(eclipse_object_suffix, X).
do_get_flag(eclipse_info_suffix, X, _) :- getval(eclipse_info_suffix, X).
do_get_flag(remote_protocol_version, X, _) :- remote_version(X).
%do_get_flag(statistics, mode, _) :- getval(mode, on).	% must be first
%do_get_flag(statistics, X, _) :-	getval(statistics, X).
%do_get_flag(strip_variables, X, _) :-
%	global_flags(0,0,F),
%	(F /\ 16'10000000 =:= 0 -> X=off ; X=on).
do_get_flag(syntax_option, X, M) :- get_syntax_(X, 0, M).
do_get_flag(toplevel_module, X, _) :-	getval(toplevel_module, X).
do_get_flag(unix_time, X, _) :-	get_sys_flag(5, X).
do_get_flag(variable_names, X, _) :-
	global_flags(0,0,F),
	(F /\ 16'08000000 =\= 0 -> X = check_singletons ;
	 F /\ 16'04000000 =\= 0 -> X = on ; X = off).
do_get_flag(version,Version, _) :-	get_sys_flag(11, Version).
do_get_flag(version_as_list, List, _) :-
	sepia_version(List, _Patch, _Date).
do_get_flag(extension,X, _) :-	extension(X, 0).
do_get_flag(library_path,X, _) :-	getval(library_path,X).
/* Parallel ECLiPSe extentions */
do_get_flag(worker,X, _) :-	get_sys_flag(10, X).
do_get_flag(workers,(Host:Awake),_) :-
	( do_get_flag(worker,0,_) ->
	  Awake = 1,
	  do_get_flag(hostname,Host,_) 
	; wm_get(List),
	  ( var(Host) -> member([_,Awake,Host], List)
	  ; stringify(Host,HostS),
	    memberchk([_,Awake0,HostS], List),
	    Awake=Awake0
	  )
	). 
do_get_flag(workerids,(Host:Awake+Asleep),_) :-
	( do_get_flag(worker,0,_) ->
	  Awake = [0], Asleep = [],
	  do_get_flag(hostname,Host,_) 
	; wm_get(List),
	  ( var(Host) -> member([_,_,Host],List)
	  ; stringify(Host,HostS),
	    memberchk([_,_,HostS],List)
	  ),
	  wm_get_ids(Host,[Awake,Asleep])
	).
do_get_flag(wm_window,X,_) :-
	( do_get_flag(worker,0,_) -> X = off
	; getval(wm_window,X)
	).

stringify(String, String) :- string(String).
stringify(Atom, String) :- atom(Atom), atom_string(Atom, String).

%----------------------------------------
% set_flag(+Name, +Value)
%----------------------------------------

:- tool(set_flag/2, set_flag_body/3),
   skipped(set_flag/2).

set_flag_body(Name, Value, M) :-
	nonvar(Name), nonvar(Value) ->
	    (
		do_set_flag(Name, Value, M) ->
		    true
		;
		    bip_error(set_flag(Name,Value))
	    )
	;
	    error(4, set_flag(Name,Value)).


% When adding new clauses to do_set_flag/2 be careful to keep it
% deterministic by using the template:
%	do_set_flag(atom, Variable) :- !, ...

:- mode do_set_flag(++, +, ++).

do_set_flag(extension, _, _) :-	!, set_bip_error(30).
do_set_flag(eclipse_info_suffix, _, _) :- !, set_bip_error(30).
do_set_flag(eclipse_object_suffix, _, _) :- !, set_bip_error(30).
do_set_flag(hostid, _, _) :-	!, set_bip_error(30).
do_set_flag(hostname, _, _) :-	!, set_bip_error(30).
do_set_flag(hostarch, _, _) :-	!, set_bip_error(30).
do_set_flag(max_global_trail, _, _) :- !, set_bip_error(30).
do_set_flag(max_local_control, _, _) :- !, set_bip_error(30).
do_set_flag(max_predicate_arity, _, _) :- !, set_bip_error(30).
do_set_flag(object_suffix, _, _) :- !, set_bip_error(30).
do_set_flag(pid, _, _) :-		!, set_bip_error(30).
do_set_flag(ppid, _, _) :-		!, set_bip_error(30).
do_set_flag(remote_protocol_version, _, _) :- !, set_bip_error(30).
do_set_flag(unix_time, _, _) :-	!, set_bip_error(30).
do_set_flag(version, _, _) :-	!, set_bip_error(30).
do_set_flag(version_as_list, _, _) :-	!, set_bip_error(30).
do_set_flag(installation_directory, _, _) :- !, set_bip_error(30).
do_set_flag(last_errno, X, _) :-	!, set_last_errno(X).
do_set_flag(loaded_library,X, _) :- !,
	(
	    (string(X) ; atom(X)) ->
		ensure_loaded(library(X))
	    ;
		set_bip_error(5)
	).
do_set_flag(parser_size,_, _) :- !.			% obsolete
do_set_flag(max_vars_per_clause,_, _) :- !.		% obsolete
do_set_flag(print_depth,X, _) :- !,
	(integer(X) -> sys_flags(1, X) ; set_bip_error(5)).
do_set_flag(load_release_delay,X, _) :- !,
	(integer(X) -> sys_flags(2, X) ; set_bip_error(5)).
do_set_flag(publishing_parameter,X, _) :- !,
	(integer(X) -> sys_flags(3, X) ; set_bip_error(5)).
do_set_flag(break_level, X, _) :- !,
	(integer(X) -> setval(break_level, X) ; set_bip_error(5)).
do_set_flag(all_dynamic, X, _) :- !,
	( X == off -> true ; set_bip_error(141) ).	% unimplemented
do_set_flag(after_event_timer, T, _) :-
	(
	    try_set_after_timer(T) -> true ;
	    wrong_atom(T)
	).
do_set_flag(ignore_eof, X, _) :- !,
	(
	    X == off ->	setval(ignore_eof, off) ;
	    X == on ->	setval(ignore_eof, on) ;
	    wrong_atom(X)
	).
do_set_flag(coroutine,X, _) :- !,
	(
	    X == off -> global_flags(16'00000100, 0, _) ;
	    X == on ->	global_flags(0, 16'00000100, _) ;
	    wrong_atom(X)
	).
do_set_flag(prolog_suffix, List, _) :- !,
	(
	    list_of_strings(List) ->
		setval(prolog_suffix, List)
	    ;
		set_bip_error(5)
	).
do_set_flag(debug_compile,X, _) :- !,
	(
	    X == off ->	global_flags(16'00000080, 0, _) ;
	    X == on ->	global_flags(0, 16'00000080, _) ;
	    wrong_atom(X)
	).
do_set_flag(debugging, X, _) :-	!,
	( valid_debug_mode(X) -> setval(toplevel_trace_mode, X)
	; wrong_atom(X) ).
do_set_flag(default_language, Language, M) :-
	ensure_loaded(library(Language))@M,
	setval(default_language, Language).
do_set_flag(user_options, _, _) :- !.			% obsolete
do_set_flag(cwd,X, _) :- !,
	(
	    (string(X) ; atom(X)) ->
		cd(X)
	    ;
		set_bip_error(5)
	).
do_set_flag(library_path, List, _) :- !,
	(
	    list_of_strings(List) ->
		setval(library_path, List)
	    ;
		set_bip_error(5)
	).
do_set_flag(tmp_dir,X, _) :- !, 
        ((atom(X) ; string(X)) ->
             concat_string([X], Dir), 
             existing_path(Dir, dir),
             check_permissions([writable], Dir),
	     canonical_path_name(Dir, CanonicalDir),
             setval(eclipse_tmp_dir, CanonicalDir)
        ;
             set_bip_error(5)
        ).
do_set_flag(macro_expansion,X, _) :- !,
	(
	    X == on ->	global_flags(0,16'00000400,_) ;
	    X == off ->	global_flags(16'00000400,0,_) ;
	    wrong_atom(X)
	).
do_set_flag(goal_expansion,X, _) :- !,
	(
	    X == on ->	global_flags(0,16'00000800,_) ;
	    X == off ->	global_flags(16'00000800,0,_) ;
	    wrong_atom(X)
	).
do_set_flag(occur_check,X, _) :-
	get_flag(extension, occur_check),
	!,
	(
	    X == on ->  set_bip_error(141) ;
%	    X == on ->	global_flags(0,16'02000000,_) ;
	    X == off ->	global_flags(16'02000000,0,_) ;
	    wrong_atom(X)
	).
do_set_flag(dfid_compile,X, _) :-
	get_flag(extension, dfid),
	!,
	(
	    X == on ->  set_bip_error(141) ;
%	    X == on ->	global_flags(0,16'01000000,_) ;
	    X == off ->	global_flags(16'01000000,0,_) ;
	    wrong_atom(X)
	).
do_set_flag(float_precision, X, _) :-
	( X == double -> true ; wrong_atom(X) ).
do_set_flag(breal_exceptions, X, _) :-
	(
	    X == on ->	global_flags(0,16'00000001,_) ;
	    X == off ->	global_flags(16'00000001,0,_) ;
	    wrong_atom(X)
	).
do_set_flag(prefer_rationals, X, _) :-
	(
	    X == on ->	global_flags(0,16'00000002,_) ;
	    X == off ->	global_flags(16'00000002,0,_) ;
	    wrong_atom(X)
	).
do_set_flag(gc,X, _) :- !,
	(
	    X == on ->	global_flags(16'00000040,16'00000010,_) ;
	    X == off ->	global_flags(16'00000050,0,_) ;
	    X == verbose ->	global_flags(0,16'00000050,_) ;
	    wrong_atom(X)
	).
%do_set_flag(gc_incr,X, _) :- !,
%	(
%	    % bit has opposite meaning!
%	    X == off ->	global_flags(0,16'00000200,_) ;
%	    X == on ->	global_flags(16'00000200,0,_) ;
%	    wrong_atom(X)
%	).
do_set_flag(gc_policy,X, _) :- !,
	(
	    % bit has opposite meaning!
	    X == adaptive ->	global_flags(0,16'00004000,_) ;
	    X == fixed ->	global_flags(16'00004000,0,_) ;
	    wrong_atom(X)
	).
do_set_flag(gc_interval,X, _) :- !,
	( integer(X) -> gc_interval(X) ; set_bip_error(5) ).
do_set_flag(gc_interval_dict,X, _) :- !,
	( integer(X) -> dict_param(7,X) ; set_bip_error(5) ).

do_set_flag(enable_interrupts, X, _) :- !,
	(
	    X == off ->	vm_flags(0, 16'00000002, _) ;
	    X == on ->	vm_flags(16'00000002, 0, _) ;
	    wrong_atom(X)
	).
do_set_flag(debugger_warnings, _X, _) :- !,
%	(
%	    X == off ->	sys_flags(4, 0) ;
%	    X == on ->	sys_flags(4, 1) ;
%	    wrong_atom(X)
%	),
	set_bip_error(141).	% unimplemented
do_set_flag(variable_names, X, _) :- !,
	(X == on ->
	    global_flags(16'08000000,16'04000000,_)
	;
	X == off ->
	    global_flags(16'0c000000,0,_)
	;
	X == check_singletons ->
	    global_flags(0,16'0c000000,_)
	;
	    wrong_atom(X)
	).
do_set_flag(toplevel_module, M, _) :- !,
	set_toplevel_module(M).
do_set_flag(strip_variables, X, _) :- !,
	(X == on ->
	    global_flags(0,16'10000000,_)
	;
	X == off ->
	    global_flags(16'10000000,0,_)
	;
	    wrong_atom(X)
	).
do_set_flag(output_mode,X, _) :- !,
	check_string(X),
	output_mode(X).
do_set_flag(output_options,Options, _) :- !,
	options_to_format(Options, 0, _Off, 0, On, Depth),
	output_mode_mask(On),
	% don't change depth when 0 or not specified
	( Depth = 0 -> true ; sys_flags(1, Depth) ).
do_set_flag(statistics, _X, _) :- !,
	set_bip_error(141).	% obsolete
do_set_flag(syntax_option, X, M) :- !,
	( X = not(Flag) -> Switch = off
	; Switch = on, Flag = X
	),
	set_syntax_(Flag, Switch, M).	% fails on error
/* Parallel ECLiPSe extentions */
do_set_flag(worker, _, _) :- !, set_bip_error(30).
do_set_flag(workerids, _, _) :- !, set_bip_error(30).
do_set_flag(workers,X,_) :- !,
	X = Host:Value,
	(var(Host) -> do_get_flag(hostname,Host,_); true),
	( var(Value) -> error(4, set_flag(workers,X))
	; do_get_flag(worker,0,_) ->
	  do_get_flag(hostname,Host,_),
	  Value = 1
	; Value = -(Sleep) -> wm_set(sleep, Host, Sleep)
	; wm_get(List), stringify(Host, HostS),
	  memberchk([Total,Awake,HostS], List),
	  Asleep is Total-Awake, 
	  ( Value = +(Diff) ->  Create is Diff-Asleep,
				( Create =< 0 -> wm_set(wake, Host, Diff)
				 ; wm_set(wake, Host, Asleep),
				   wm_set(add, Host, Create)
				)
	  ; integer(Value) -> Diff0 is Value-Awake,
			      ( Diff0 == 0 -> true
			      ; Diff0 < 0 -> Sleep is -Diff0,
					     wm_set(sleep, Host, Sleep)
			      ; Diff0 =< Asleep -> wm_set(wake, Host, Diff0)
			      ; wm_set(wake, Host, Asleep),
				Create is Diff0-Asleep,
				wm_set(add, Host, Create)
			      )
	  ; error(5,set_flag(workers,X))
          )
	).
do_set_flag(wm_window,X,_) :- !,
	( do_get_flag(worker,0,_) ->
	   (X = off -> true; error(142, set_flag(wm_window,X)))
        ; var(X) -> error(4, set_flag(wm_window,X))
	; member(X,[on,off]) -> setval(wm_window,X), wm_interface(X)
	; wrong_atom(X)
	).
do_set_flag(InvalidFlag, _, _) :-
	wrong_atom(InvalidFlag).


wrong_atom(X) :-
	atom(X) -> set_bip_error(6) ; set_bip_error(5).

valid_debug_mode(nodebug) ?- true.
valid_debug_mode(creep) ?- true.
valid_debug_mode(leap) ?- true.


%---------------------------------------------------------------------------

?- make_local_array(wm_window), setval(wm_window, off).

%----------------------------------------
% print the current environment settings
%----------------------------------------

:- tool(env/0, env_body/1), skipped(env/0), untraceable(env/0).

env_body(M) :-
	nl(output),
	setof(Flag, V^(get_flag(Flag, V), \+long_flag(Flag), \+obsolete_flag(Flag)), FList),
	Half is (length(FList) + 1) // 2,
	halve(Half, FList, L1, L2),
	splice(L1, L2, NewList),
	open(_, string, env_stream),
	member(Flag, NewList),
	get_flag_body(Flag,Value,M),		% first the deterministic flags
	Fill is 23 - atom_length(Flag),
	printf(env_stream, "%w:%*c%QDvw", [Flag, Fill, 0' , Value]),
	at(env_stream, At),
	(At < 40 ->
	    F is 40 - At,
	    printf(env_stream, "%*c", [F, 0' ])
	;
	    F is 79 - At,
	    (F > 0 -> printf(env_stream, "%*c\n", [F, 0' ]); true),
	    current_stream(S, _, env_stream),
	    write(output, S),
	    seek(env_stream, 0)
	),
	fail.
env_body(M) :-
	at(env_stream, At),
	(At == 40 ->
	    current_stream(S, _, env_stream),
	    printf(output, "%.40s\n", [S])
	;
	    true
	),
	close(env_stream),
	long_flag(Flag),		% then the nondeterministic flags
	\+obsolete_flag(Flag),
	once(get_flag_body(Flag,_,M)),
	Fill is 23 - atom_length(Flag),
	printf(output, "\n%w:%*c", [Flag, Fill, 0' ]),
	get_flag_body(Flag,Value,M),
	(Flag = library_path ->
		write_paths(Value)
	;
		writeq(output, Value),
		write(output, ' ')
	),
	fail.
env_body(_) :-
	nl(output),
	flush(output).

write_paths([H|T]) :-
	write(output, '['),
	writeq(output, H),
	write_list(T).

write_list([]) :-
	write(output, ']').
write_list([H|T]) :-
	printf(output, ",\n%*c", [25, 0' ]),
	writeq(output, H),
	write_list(T).

halve(0, L2, [], L2) :- !.
halve(N, [H|R], [H|Li], L2) :-
	N1 is N - 1,
halve(N1, R, Li, L2).

splice([], L, L) :- !.
splice(L, [], L) :- !.
splice([H1|L1], [H2|L2], [H1, H2|R]) :-
	splice(L1, L2, R).

list_of_strings([]).
list_of_strings([H|T]) :-
	string(H),
	list_of_strings(T).

long_flag(cwd).
long_flag(default_language).
long_flag(extension).		% nondet
long_flag(tmp_dir).
long_flag(hostid).
long_flag(hostname).
long_flag(installation_directory).
long_flag(library_path).	% too long
long_flag(loaded_library).	% nondet
long_flag(output_options).
long_flag(prolog_suffix).
long_flag(syntax_option).	% nondet
long_flag(variable_names).
long_flag(workerids).
long_flag(workers).

obsolete_flag(all_dynamic).
obsolete_flag(dfid_compile).
obsolete_flag(occur_check).
obsolete_flag(worker).
obsolete_flag(workerids).
obsolete_flag(workers).
obsolete_flag(wm_window).

%-------------------------------------
% statistics(+Name, ?Value)
%-------------------------------------

?- make_array_(runtime, prolog, local, sepia_kernel), setval(runtime, 0).

statistics(Name, Value) :-
	var(Name),
	!,
	get_stat(Name, Value, _, _).
statistics(Name, Value) :-
	get_stat(Name, X, _, _),
	!,
	Value = X.
statistics(Name, Value) :-
	(atom(Name) -> Err = 6 ; Err = 5),
	error(Err, statistics(Name, Value)).

:- mode get_stat(?,?,-,-).

get_stat(times, [User, System, Real], seconds, sepia) :-
	all_times(User, System, Real).
get_stat(session_time, Time, seconds, sepia) :-	session_time(Time).
get_stat(event_time, EventTime, seconds, sepia) :-
	( get_flag(after_event_timer, real) ->
	    session_time(EventTime)
	;
	    cputime(EventTime)
	).

get_stat(global_stack_used, X, bytes, sepia) :-		gc_stat(8, X).
get_stat(global_stack_allocated, X, bytes, sepia) :-	gc_stat(9, X).
get_stat(global_stack_peak, X, bytes, sepia) :-		gc_stat(10, X).

get_stat(trail_stack_used, X, bytes, sepia) :-		gc_stat(12, X).
get_stat(trail_stack_allocated, X, bytes, sepia) :-	gc_stat(13, X).
get_stat(trail_stack_peak, X, bytes, sepia) :-		gc_stat(14, X).

get_stat(control_stack_used, X, bytes, sepia) :-	gc_stat(16, X).
get_stat(control_stack_allocated, X, bytes, sepia) :-	gc_stat(17, X).
get_stat(control_stack_peak, X, bytes, sepia) :-	gc_stat(18, X).

get_stat(local_stack_used, X, bytes, sepia) :-		gc_stat(20, X).
get_stat(local_stack_allocated, X, bytes, sepia) :-	gc_stat(21, X).
get_stat(local_stack_peak, X, bytes, sepia) :-		gc_stat(22, X).

get_stat(code_heap_allocated, X, bytes, obsolete) :-	heap_stat(0, X).
get_stat(code_heap_used, X, bytes, obsolete) :-		heap_stat(1, X).
get_stat(general_heap_allocated, X, bytes, obsolete) :-	heap_stat(2, X).
get_stat(general_heap_used, X, bytes, obsolete) :-	heap_stat(3, X).

get_stat(shared_heap_allocated, X, bytes, sepia) :-	heap_stat(0, X).
get_stat(shared_heap_used, X, bytes, sepia) :-		heap_stat(1, X).
get_stat(private_heap_allocated, X, bytes, sepia) :-	heap_stat(2, X).
get_stat(private_heap_used, X, bytes, sepia) :-		heap_stat(3, X).

get_stat(gc_number, X, '', sepia) :-		gc_stat(0, X).
get_stat(gc_collected, X, bytes, sepia) :-	gc_stat(1, X).
get_stat(gc_area, X, bytes, sepia) :-		gc_stat(2, X).
get_stat(gc_ratio, X, '%', sepia) :-		gc_stat(3, X).
get_stat(gc_time, X, seconds, sepia) :-		gc_stat(4, X).
get_stat(dictionary_entries, X, '', sepia) :-	dict_param(0, X).
get_stat(dict_hash_usage, U/T, '', sepia) :-	dict_param(3, U), dict_param(2, T).
get_stat(dict_hash_collisions, C/U, '', sepia) :- dict_param(4, C), dict_param(3, U).
get_stat(dict_gc_number, X, '', sepia) :-	dict_param(5, X).
get_stat(dict_gc_time, X, seconds, sepia) :-	dict_param(6, X).

get_stat(runtime, [Total, Last], '', quintus) :-	% compatibility stuff
% Kish 99-7-2: removed garbage collection time (as done in SICStus)
	gc_stat(4, Gc),
	Total is fix((cputime - Gc) * 1000),
	getval(runtime, Old),
	Last is Total - Old,
	setval(runtime, Total).
get_stat(memory, [Total, 0], '', quintus) :-
	Total is heap_stat(0) + heap_stat(2) + gc_stat(9) + gc_stat(17) +
             gc_stat(13) + gc_stat(21).
get_stat(program, [Used, Free], '', quintus) :-
	heap_stat(1, Used),
	Free is heap_stat(0) - Used.
get_stat(global_stack, [Used, Free], '', quintus) :-
	gc_stat(8, Used),
	Free is gc_stat(9) - Used.
get_stat(local_stack, [Used, Free], '', quintus) :-
	Used is gc_stat(20) + gc_stat(16),
	Free is gc_stat(21) + gc_stat(17) - Used.
get_stat(trail, [Used, Free], '', quintus) :-
	gc_stat(12, Used),
	Free is gc_stat(13) - Used.
get_stat(garbage_collection, [Number, Freed, Time], '', quintus) :-
	gc_stat(0, Number),
	gc_stat(1, Freed),
	gc_stat(4, Time).
get_stat(stack_shifts, [0, 0, 0], '', quintus).
get_stat(core, List, '', quintus) :-
	get_stat(memory, List, '', quintus).
get_stat(heap, List, '', quintus) :-
	get_stat(program, List, '', quintus).

statistics :-
	statistics(log_output).

statistics(Stream) :-
	nl(Stream),
	(is_a_module(quintus) ->
		System = quintus
	;
		System = sepia
	),
	get_stat(What, Value, Unit, System),
	Fill is 23 - atom_length(What),
	printf(Stream, "%w:%*c%w %w\n", [What, Fill, 0' , Value, Unit]),
	fail.
statistics(Stream) :-
	flush(Stream).

%-------------------------------------
% online help
%-------------------------------------

% Find the description of the specified built-in.
:- skipped (help)/1.
:- untraceable (help)/1.

help(Type:Name/Arity) :-
	(var(Type) -> true; atom(Type)),
	(var(Name) -> true; atom(Name)),
	(var(Arity) -> true; integer(Arity)),
	!,
	findall(Bip, find_match(Type, Name, Arity, Bip), List),
	print_help(List, full).
help(Name/Arity) :-
	(var(Name) -> true; atom(Name)),
	(var(Arity) -> true; integer(Arity)),
	!,
	findall(Bip, find_match(_Type, Name, Arity, Bip), List),
	print_help(List, full).
help(lib(Name)) :-
        atom(Name),
        !,
        findall(Bip, find_match(Name, Name, index, Bip), List),
        print_help(List, full).
help(Arity) :-
	integer(Arity),
	!,
	findall(Bip, find_match(_Type, _Name, Arity, Bip), List),
	print_help(List, match).
help(Guess) :-
	atom(Guess),
	!,
	atom_string(Guess, SGuess),
	findall(Pred, find_guess(SGuess, Pred), List),
	print_help(List, match).
help(Guess) :-
	string(Guess),
	!,
	findall(Pred, find_guess(Guess, Pred), List),
	print_help(List, match).
help(Pred) :-
	error(5, help(Pred)).


find_guess(String, Bip) :-		% find bip with substring string
	find_match(_Type, Name, _Arity, Bip),
	atom_string(Name, SName),
	substring(SName, String, _).

find_match(Type, Name, Arity, Bip) :-		% find bip matching Pred
	open_index_file(S),
	b_read(S, Bip),
	Bip = bip(Name, Arity, _, Type, _).

% 2nd arg is either `full' or `match'; `full' is if a `full' specification
% is given: N/A or M:N/A. 
print_help([BipInfo], _) :-
        BipInfo = bip(Name, Arity, System, Type, _),
	!,				% single match, print full description
	getval(sepiadir, Dir),
        get_bip_file_name(desc, BipInfo, File),
	concat_string([Dir,'/doc/bips/',System,/,Type,/,File,'.txt'], HelpFile),
	(existing_file(HelpFile, [""], [], _) ->
	    ( get_stream_info(output, device, tty) ->
		( get_pager(Pager) ->		% only if there is a paging program
		    (get_interrupt_handler(int, Hand, DefMod),
		     set_interrupt_handler(int, true/0),
		     block(((exec([Pager,HelpFile], []) -> true;true),
		            set_interrupt_handler(int,Hand)@DefMod
			   ), Tag, 
		           (set_interrupt_handler(int,Hand)@DefMod,
			    exit_block(Tag)
			   )
		     ))
		;
		    more(HelpFile)
		)
	    ;
	        open(HelpFile, read, S),
		read_string(S, "", _, String),
		close(S),
		writeln(String)
	    )
	;
            printf("In library %w:  %w/%w%n", [Type, Name, Arity]),
            writeln("   No help information available.")
	).
print_help(List0, MatchType) :-			
% multiple matches, more complex handling
	List0 = [_|_],
	((MatchType == full, delete(bip(Name,Arity, kernel, Type, File), List0, List1)) ->
	    % if a built-in exists for the specification, print it in full
	    print_help([bip(Name,Arity, kernel, Type, File)], full)
	;   List1 = List0
        ),
	print_headers(List1).

:- mode print_headers(+).
print_headers([]) :-
	writeln('----\nCall help(<Module:>Name/Arity) for detailed help.').
print_headers([BipInfo|Preds]) :-
        BipInfo = bip(Name, Arity, System, Type, _),
        get_bip_file_name(summary, BipInfo, File),
	getval(sepiadir, Dir),
	concat_string([Dir,'/doc/bips/',System,/,Type,/,File,'.txt'], HelpFile),
	(exists(HelpFile) ->
	    writeln(----),
	    ( System == kernel -> true
	    ; printf("In library %w:  ", [Type]) ),
	    open(HelpFile, read, HelpStream),
	    print_paragraph(HelpStream),
	    print_paragraph(HelpStream),
	    close(HelpStream)
	;
	    ( System == kernel ->
		true
	    ;   writeln(----),
	        printf("In library %w:  %w/%w%n", [Type, Name, Arity]),
	        writeln("   No help information available.")
	    )
	),
	print_headers(Preds).

% checks if the bip description is for a library, in which case
% construct appropriate library info file name (summary or desc)
% Library index has form bip(Name, index, System, Name, '')
get_bip_file_name(InfoType, bip(Name, index, _System, Name, ''), File) ?-
        !,
        concat_string([Name, "_", InfoType], File).
get_bip_file_name(_, bip(_,_,_,_,File0), File) ?- File0 = File.

print_paragraph(S) :-
	repeat,					% skip empty lines
	read_string(S, end_of_line, Len, Line),
	Len > 0,
	writeln(Line),
	repeat,					% print non-empty lines
	read_string(S, end_of_line, Len1, Line1),
	( Len1 == 0 ->
	    !
	;
	    writeln(Line1),
	    fail
	).

get_pager(Pager) :-
	getenv("PAGER", Pager), !. 
get_pager(Pager) :-
	( Pager = "/usr/ucb/more"
	; Pager = "/usr/bin/more"
	; Pager = "/usr/bin/pg"
	),
	exists(Pager), !.

open_index_file(S) :-
	getval(sepiadir, Sepiadir),
	concat_atom([Sepiadir, '/doc/bips/index.pl'], Index),
	(current_stream(Index, read, S) ->
	    seek(S, 0)
	;
	    open(Index, read, S)
	).

% Read the next term from the stream S, leaving a choicepoint
% and an open stream, positioned after the returned term.
% On EOF fail and close the stream.

b_read(S, Bip) :-
	repeat,
	read(S, Bip),
	( Bip == end_of_file ->
	    close(S),
	    !,
	    fail
	;
	    true
	).

% display File in pages

more(File) :-
	open(File, read, S),
	set_error_handler(190, exit_block/1),
	block(more_page(0, S), Tag, more_end(Tag, S)).

more_end(Tag, S) :-
	close(S),
	reset_error_handler(190),
	(Tag \== 190 ->
	    exit_block(Tag)
	;
	    true
	).

more_page(24, S) :-
	!,
	write('More ? '),
	flush(output),
	tyi(X),
	write('\b\b\b\b\b\b\b       \b\b\b\b\b\b\b'),
	(X == 0'q ->
	    exit_block(190)
	;
	X == 0'b ->
	    seek(S, 0),
	    more_page(0, S)
	;
	    more_page(0, S)
	).
more_page(N, S) :-
	get(S, X),
	(X \== 12 -> put(X); true),
	(X == 10 ->
	    N1 is N + 1,
	    more_page(N1, S)
	;
	    more_page(N, S)
	).

:- untraceable
	statistics/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
