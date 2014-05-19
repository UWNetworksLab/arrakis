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
% Version:	$Id: debug.pl,v 1.7 2008/11/17 13:49:18 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	debug.pl
 *
 * DESCRIPTION: 	
 *
 *
 * CONTENTS:     
 *
 */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_module(sepia_kernel).
:- system.		% compiler directive to add the SYSTEM flag

:- export
	set_leash/2,
	get_leash/2,
	(spy)/1,
	(nospy)/1,
	debug_reset/0,
	debugging/0.

% for tracer_*
:- export 
	current_predicate_with_port/4,
        get_portlist_from_file/4,
        find_matching_breakport/6,
        current_files_with_port_lines/1.

:- tool( debugging/0, debugging_body/1).
:- tool( (spy)/1, spy_body/2).
:- tool( (nospy)/1, nospy_body/2).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/********* Conditional definition of GUI debugging predicates ***************/

:- export
	make_display_matrix/2,
	make_display_matrix/5,
	kill_display_matrix/1.

:- tool(make_display_matrix/2, make_display_matrix_body/3).
:- tool(make_display_matrix/5, make_display_matrix_body/6).
:- tool(kill_display_matrix/1, kill_display_matrix_body/2).

make_display_matrix_body(Matrix, Name, Module) :-
	( current_module(remote_tools) ->   
		(remote_tools: attached(_) -> % only when attached
		    tracer_tcl:make_display_matrix_body(Matrix, Name, Module)
		; true
	        )
	; current_module(tracer_tcl) ->
	    tracer_tcl:make_display_matrix_body(Matrix, Name, Module)
	;
	    true
	).

make_display_matrix_body(Matrix, Prio, Type, SList, Name0, Module) :-
	( current_module(remote_tools) ->   
		(remote_tools: attached(_) -> % only when attached
		    tracer_tcl:make_display_matrix_body(Matrix, Prio, Type, SList, Name0, Module)
		; true
	        )
	; current_module(tracer_tcl) ->
	    tracer_tcl:make_display_matrix_body(Matrix, Prio, Type, SList, Name0, Module)
	;
	    true
	).

kill_display_matrix_body(Name0, Module) :-
	( current_module(tracer_tcl) ->
	    tracer_tcl:kill_display_matrix_body(Name0, Module)
	;
	    true
	).

%----------------------------------------------------------------------


debug_reset :-
	trace_mode(12, 0).


debugging_body(Module) :-
	getval(toplevel_trace_mode, Mode),
	debug_info(Mode, Module).


debug_info(nodebug, _) :-
	!,
	writeln(log_output, 'Debugger is switched off').
debug_info(Mode, Module) :-
	write(log_output, 'Debug mode is '),
	writeln(log_output, Mode),
	(
	    current_spied_predicate(F/A, M),
	    write(log_output, "Spying "),
	    ( Module = M -> true ; printf(log_output, "%w:", [M]) ),
	    printf(log_output, "%a/%d%n", [F, A]),
		fail
	;
		true
	),
        (
	    current_predicate_with_port(break_lines, P, DM, File:Line),
	    local_file_name(File, LocalFile),
            printf(log_output, "Breakpoint at line %d in file %w (in predicate %w:%w)%n",
                               [Line,LocalFile,DM,P]),
            fail
        ;
            true
        ).

%
% add spypoint(s)
%
spy_body(Y, _):-
	var(Y), !,
	error(4, 'spy'( Y)).
spy_body(P, M) :-
	set_spypoints(P, M),
	( getval(toplevel_trace_mode, nodebug) ->
	    setval(toplevel_trace_mode, leap),
	    writeln(log_output, "Debugger switched on - leap mode")
	;
	    true	% leave debugger in whatever mode it is
	).

set_spypoints((A, B), M) :-
	!,
	set_spypoints(A, M),
	set_spypoints(B, M).
set_spypoints(F, M) :-
	atom(F),
	!,
	set_spypoints(M:F/_, M).
set_spypoints(F/N, M) ?- !,
	set_spypoints(M:F/N, M).
set_spypoints(PM:F, M) ?- atom(F), !,
	set_spypoints(PM:F/_, M).
set_spypoints(F:L, M) ?-
	integer(L),
	check_atom_string(F),
	!,
        ( find_matching_breakport(F, L, FullName, DMs, PortPreds, PortLine) ->
            ( foreach(DM, DMs), foreach(PortPred, PortPreds),
              param(PortLine,FullName)
            do
                ( get_flag(PortPred, break_lines, PInfo)@DM,
                  member(FullName0:PortLine, PInfo),
                  canonical_path_name(FullName0, FullName)
                ->
                    true
                ;
                    set_proc_flags(PortPred, break, PortLine, DM),
                    printf(log_output, "breakpoint added to line %d of file %w in"
                                   " predicate %w%n", [PortLine,FullName,DM:PortPred])
                )
            )
        ;
            error(6, spy(F:L), M)
        ).
set_spypoints(QualPredSpec, M) :-
	check_partial_qualpredspec(QualPredSpec),
	!,
	QualPredSpec = PM:PredSpec,
	% look first for preds defined locally
	find_matching_predicates(PredSpec, PM, defined, LocalPreds),
	( LocalPreds = [] ->
	    % if none, look for exported preds elsewhere
	    find_matching_predicates(PredSpec, _, exported, ExpElsewherePreds),
	    ( ExpElsewherePreds = [] ->
		% if none, look for local preds elsewhere
		find_matching_predicates(PredSpec, _, defined, Preds),
		( Preds = [] ->
		    error(60, spy(PredSpec), M)
		;
		    true
		)
	    ;
		Preds = ExpElsewherePreds
	    )
	;
	    Preds = LocalPreds
	),
	(
	    member(DM:F/N, Preds),
	    not spied_(F, N, DM),
		( set_proc_flags(F/N, spy, on, DM) ->
		    write(log_output, "spypoint added to "),
		    ( DM = M -> true ; printf(log_output, "%w:", [DM]) ),
		    printf(log_output, "%w/%d.%n", [F,N])
		;
		    !,
		    bip_error(spy(PredSpec), M)
		),
	    fail
	;
	    true
	).
set_spypoints(U, M):-
	bip_error('spy'(U), M).

%
% remove spypoint(s)
%
nospy_body(Y, M):-
	var(Y), !,			% remove all spy points
	nospy_body(_/_, M).
nospy_body((A,B), M) :-
	!,
	nospy_body(A, M),
	nospy_body(B, M).
nospy_body(F, M) :-
	atom(F),
	!,
	nospy_body(M:F/_, M).
nospy_body(F/N, M) ?- !,
	nospy_body(_Any:F/N, M).
nospy_body(PM:F, M) ?- atom(F), !,
	nospy_body(PM:F/_, M).
nospy_body(F:L, _M) ?-
	integer(L),
	check_atom_string(F),
	!,
        ( find_matching_breakport(F, L, FullName, DMs, PortPreds, PortLine) ->
            ( foreach(DM, DMs), foreach(PortPred, PortPreds),
              param(PortLine,FullName)
            do
                ( get_flag(PortPred, break_lines, PInfo)@DM,
                  member(FullName0:PortLine, PInfo),
                  canonical_path_name(FullName0, FullName)
                ->
                    set_proc_flags(PortPred, break, PortLine, DM),
                    printf(log_output, "breakpoint removed from line %d of file %w in"
                                       " predicate %w%n", [PortLine,FullName,DM:PortPred])
                ;
                    true
                )
            )
        ;
            true
        ).
nospy_body(QualPredSpec, M) :-
	check_partial_qualpredspec(QualPredSpec),
	!,
	QualPredSpec = DM:PredSpec,
	(
	    current_spied_predicate(PredSpec, DM),
		(set_proc_flags(PredSpec, spy, off, DM) ->
		    PredSpec = F/N,
		    write(log_output, "spypoint removed from "),
		    ( DM = M -> true ; printf(log_output, "%w:", [DM]) ),
		    printf(log_output, "%w/%d.%n", [F,N])
		;
		    !,
		    bip_error('nospy'(PredSpec), M)
		),
	    fail
	;
	    true
	).
nospy_body(U, M):-
	bip_error('nospy'(U), M).


    find_matching_predicates(P, M, Class, List) :-
	findall(M:P, (
		current_module(M),
		( is_locked(M) ->
		    % only see the exported ones (but not
		    % reexported ones, to avoid duplicates)
		    current_module_predicate(exported, P)@M
		;
		    current_module_predicate(Class, P)@M
		)
	    ), List).

    current_spied_predicate(P, M) :-
	    current_module(M),
	    ( is_locked(M) ->
		current_module_predicate(exported, P)@M
	    ;
		current_module_predicate(defined, P)@M
	    ),
	    P \= trace_body/2,
	    P = F/A,
	    spied_(F, A, M).

    check_partial_qualpredspec(X) :- var(X), !,
	    set_bip_error(4).
    check_partial_qualpredspec(N/A) :- !,
	    check_var_or_atom(N),
	    check_var_or_arity(A).
    check_partial_qualpredspec(M:P) :- !,
	    check_var_or_atom(M),
	    check_partial_predspec(P).
    check_partial_qualpredspec(_) :-
	    set_bip_error(5).


% dummies, no longer supported
set_leash(_Port, _Mode).
get_leash(_Port, stop).

%
% breakpoint related
%
current_predicate_with_port(PortType, PredSpec, Module, PortInfo) :-
        current_module(Module),
        \+ is_locked(Module),
        current_module_predicate(defined, PredSpec)@Module,
        get_flag(PredSpec, debugged, on)@Module,
        get_flag(PredSpec, PortType, PortInfos)@Module,
	member(PortInfo, PortInfos).

% On Windows, canonical_path_name can be really slow!
% We cannot cache more permanently because cwd and file system may change.
cached_canonical_path_name(Path, CanPath, Cache) :-
	( store_get(Cache, Path, CanPath0) ->
	    true
	;
	    canonical_path_name(Path, CanPath0),
	    store_set(Cache, Path, CanPath0)
	),
	CanPath0 = CanPath.

ports_in_file(File, PortType, List) :-
        % use Line-M to allow different modules to reuse the file
	store_create(Cache),
        findall(port(Line-M,P), (
                current_predicate_with_port(PortType, P, M, File0:Line),
                % make sure we are comparing canonical names for this machine
                cached_canonical_path_name(File0, File, Cache)
             ), List0),
        sort(1,<, List0, List).

current_files_with_port_lines(Files) :-
	store_create(Cache),
        findall(OSF, (
                current_predicate_with_port(port_lines, _, _, F0:_),
                cached_canonical_path_name(F0, F, Cache),
                os_file_name(F, OSF)
              ), Files0),
        sort(0, <, Files0, Files).

find_best_port([], _Line, _File, LastPort, PortLine) :-
        LastPort = port(PortLine-_DM,_PortPred).
find_best_port([Port0|PortList0], Line, File, LastPort, PortLine) :-
        Port0 = port(Line0-DM0,Pred0),
        ( Line0 =:= Line ->
            PortLine = Line0
        ; Line0 > Line ->
           /* decide if LastPort or Port0 is the better port */
            ( LastPort = port(LastLine-_LastDM,LastPred) ->
                ( Pred0 \= LastPred, get_flag(Pred0, source_file, File0),
                  canonical_path_name(File0, File)
                ->
                    % start of predicate is in same file as Port0 
                    ( current_module(DM0),
                      get_flag(Pred0, source_line, StartLine0)@DM0,
                      StartLine0 > Line ->
                        /* assume Pred0 starts after Line, use LastPort */
                        PortLine = LastLine
                                                                   
                    ;
                        PortLine = Line0
                    )
                ;
                    PortLine = Line0
                )
            ;
                PortLine = Line0
            )
        ; /* Line0 < Line */
           find_best_port(PortList0, Line, File, Port0, PortLine)
        ).

get_portlist_from_file(F, PortType, File, PortsList) :-
        concat_atom([F], File0),
        canonical_path_name(File0, File1),
        get_flag(prolog_suffix, Ss),
        once(existing_file(File1, Ss, [readable], File)), 
        ports_in_file(File, PortType, PortsList).

find_matching_breakport(File, Line, FullFile, DMs, PortPreds, PortLine) :-
        get_portlist_from_file(File, port_lines, FullFile, PortsList),
        find_best_port(PortsList, Line, FullFile, none, PortLine), 
        find_all_ports_at_line(PortsList, PortLine, DMs, PortPreds).

find_all_ports_at_line([], _Line, [], []) :- !.
find_all_ports_at_line([port(Line0-DM0,Pred0)|Ports], Line, DMs, Preds) :-
        ( Line0 < Line ->
            find_all_ports_at_line(Ports, Line, DMs, Preds)
        ; Line0 == Line ->
            DMs =[DM0|DMs1], 
            Preds = [Pred0|Preds1],
            find_all_ports_at_line(Ports, Line, DMs1, Preds1)
        ; /* Line0 > Line */
            DMs = [], Preds = []
        ).

%--------------------------------------------------------

?- untraceable
	debugging/0,
	set_leash/2,
	get_leash/2,
	(spy)/1,
	(nospy)/1.

?- skipped
	debugging/0,
	set_leash/2,
	set_flag/3,
	get_leash/2,
	(spy)/1,
	(nospy)/1.

