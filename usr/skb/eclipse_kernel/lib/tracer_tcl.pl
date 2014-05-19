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
%
% ECLiPSe II debugger -- Tcl/Tk Interface
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: tracer_tcl.pl,v 1.8 2008/11/17 13:49:19 jschimpf Exp $
% Authors:	Joachim Schimpf, IC-Parc
%		Kish Shen, IC-Parc
%               Josh Singer, Parc Technologies
%
%----------------------------------------------------------------------

:- module(tracer_tcl).

:- local
	struct(inspect(type,top,path,written,module)),
	struct(dg_filter(traceonly, spiedonly, wakeonly)),
	struct(mfile(dir,module,file)),
	struct(minfo(interface,comments)),

	reference(exec_state),
	reference(matdisplaydata),

	variable(observed_count),
        variable(filter_spy_goal),
        variable(filter_status),
        variable(filter_count),
        variable(filter_hits),
        variable(inspect_observed),
	variable(next_cmd),
	variable(indent_step),
	variable(dbg_format_string),
	variable(dbg_goal_format_string),
	variable(dbg_print_depth),
        variable(matdisplayid),
	variable(lbpath_type),
	variable(library_info),
	variable(show_module),
        variable(tracer_command).



:- setval(matdisplayid,0).
:- setval(observed_count,0).
:- setval(inspect_observed, []).
:- setval(filter_spy_goal, none).
:- setval(filter_status, off).
:- setval(filter_count, 1).
:- setval(filter_hits, 0).
:- setval(tracer_command, "c").

% the types for each level of the path used by the library browser
:- setval(lbpath_type, [top,dir,module,interface]).

:- export 
        init_library_info/0,
	init_toplevel_module/0,
	expand_lbnode/2,
	lbnode_display/3,
	lbnode_info/4,
	lbnode_loadmodule/1,
	return_html_root/1,
        report_stats/2,
	stop_report_stats/0,
	start_report_stats_text_summary/1,
	stop_report_stats_text_summary/0,
	change_report_interval/1,
	inspect_command/3,
	inspect_get_children_for_path/5,
	compile_string/1,
	get_tracer_output_modes/1, set_tracer_output_modes/1,
	get_tracer_print_depth/1, set_tracer_print_depth/1,
	compile_os_file/2,
	use_module_os/2,
	list_files/1,
	list_modules/2,
	list_predicates/4,
	set_flag_string/4,
	gui_help_string/2,
	gui_dg/3,
        get_triggers/1,
        get_source_info/4,
	flag_value/4,
	record_source_file/1,
	register_inspected_term/2,
	make_display_matrix_body/6, 
	make_display_matrix_body/3, 
	kill_display_matrix_body/2,
	install_guitools/0,
	uninstall_guitools/0,
	get_ancestors/1,
        get_current_traceline/4,
	get_goal_info_by_invoc/8,
        prepare_filter/1,
        set_usepred_info/5,
        reenable_usepred/0,
        set_tracer_command/1,
	toggle_source_breakpoint/5,
	file_is_readable/1,
	read_file_for_gui/1,
        breakpoints_for_file/4,
	find_exact_callinfo/3,
	find_matching_callinfo/4,
        is_current_goal/2,
	saros_get_library_path/1,
	saros_set_library_path/1,
	saros_compile/1,
	saros_fcompile/2,
	saros_icompile/2,
        saros_eci_to_html/3,
        saros_ecis_to_htmls/4,
	saros_cd/1,
	saros_use_module/1,
	saros_get_goal_info_by_invoc/10.

:- reexport 
        current_files_with_port_lines/1
   from sepia_kernel.


%----------------------------------------------------------------------
:- pragma(system).
:- pragma(nodebug).

%:- import struct(tf), struct(trace_line) from sepia_kernel.
:- import sepia_kernel.

:- import
	set_default_error_handler/2,
	configure_prefilter/5,
	cut_to_stamp/2,
	trace_mode/2,
	find_goal/3,
	get_attribute/3,
	get_tf_prop/3,
	current_predicate_with_port/4,
        get_portlist_from_file/4,
        find_matching_breakport/6
    from sepia_kernel.

:- lib(development_support).

:- setval(dbg_goal_format_string, "%*GQPmw").
:- setval(dbg_print_depth, 5).
:- setval(show_module, off).
:- setval(indent_step, 0).


%----------------------------------------------------------------------
% Tracer/GUI interface
% We use two queues to communicate with the GUI
%----------------------------------------------------------------------

trace_start_handler_tcl :-
	setval(filter_status, off),
        setval(filter_hits, 0),
        setval(filter_counts, 1),
        ( peer_queue_get_property(debug_traceline, peer_name, Name),
          peer_get_property(Name, language, "java") ->
            /* this is a hopefully temporary way to detect we are using Saros
               and avoid making changes to the Java side for now
            */
            true
        ;
            % inform GUI of start of tracing
            write_exdr(debug_traceline, []), 
            flush(debug_traceline)
        ).


trace_line_handler_tcl(_, Current) :-
        % call the goal_filter goal, which represents the user's "conditional
        % breakpoint. Do this FIRST for speed!!!
        getval(filter_status, FilterStatus),
        do_filter(FilterStatus, Current), 
	% make sure the debug_traceline stream is usable, otherwise fail
	(get_stream_info(debug_traceline, usable, on) ->
	    % usable off only applies if stream performs a yield/2, but for
            % remote peer queues, this yeild is not done, and can be handled
	    true ; peer_queue_get_property(debug_traceline, peer_type, remote)
	),
	% store the current trace line in a global reference, where it
	% can be picked up by some of the interactive tools
	setval(exec_state, Current),
	% if the goal_filter goal succeeds, we do not want to use the
        % other clause of trace_line_handler_tcl, which does nothing.
	!,
	flush(debug_output),
	open(string(""), write, SS),
	make_trace_line(SS, Current, Depth, Port, Invoc, Prio, FPath, _Linum, From0, To0),
	get_stream_info(SS, name, Line),
	close(SS),
	port_style(Port, Style),
        check_if_source_should_update(Port, From0, To0, From, To),
	write_exdr(debug_traceline, [Depth, Style, Line, Invoc, Port, Prio,
                                     FPath, From, To]),
	flush(debug_traceline),	% may not work in nested emulator...
        peer_do_multitask(tracer),
        getval(tracer_command, Cmd),
	%writeln(error, got:Cmd),
	%writeln(error, current:Current),
	interpret_command(Cmd, Current, Depth, Cont),
	call(Cont),	% may cut_to/fail
	getval(inspect_observed, Obs),
	setval(inspect_observed, []),  % reset
	create_observed(Obs, Current).
trace_line_handler_tcl(_,_).

% Prepare for the filter command
prepare_filter(Count) :-
        setval(filter_status, on),
        setval(filter_count, Count).

do_filter(off, _).
do_filter(on, _) :-
        filter_count_and_reset(none).
do_filter(goal(SpyStatus), trace_line{frame:tf{goal:Goal, module:Module}}):-
        goal_filter(Goal, Module),
        filter_count_and_reset(SpyStatus).

filter_count_and_reset(SpyStatus) :-
        incval(filter_hits),
        getval(filter_count, FCount),
        (FCount > 1 -> 
             decval(filter_count), 
             fail 
        ;
             reset_usepred_info(SpyStatus)
        ),
        setval(filter_status, off).
        
make_trace_line(Stream, trace_line{port:Port, frame:Frame}, Depth,
                Port, Invoc, Prio, FPath, Linum, From, To) :-
	Frame = tf{invoc:Invoc,goal:Goal,depth:Depth,prio:Prio,module:M,
                         path:Path0, line:Linum, from:From, to:To},
	register_inspected_term(Goal, M),
        % wrapper around pathname to avoid empty string 
        (Path0 == '' -> 
            FPath = no 
        ; 
            os_file_name(Path0, OSPath),
            FPath = p(OSPath)
        ),
        % print priority only if not the normal 12
        (Prio == 12 -> PrioS = "" ; concat_string([<,Prio,>], PrioS)),
	( get_tf_prop(Frame, skip, on) -> Prop = 0'S ; Prop = 0'  ),
	( get_tf_prop(Frame, break) =\= 0 -> Spied = 0'#
	; get_tf_prop(Frame, spy, on) -> Spied = 0'+ ; Spied = 0'  ),
	Indent is Depth*getval(indent_step),
	printf(Stream, "%c%c%*c(%d) %d %A%s  ",
			[Prop, Spied, Indent, 0' , Invoc, Depth, Port, PrioS]),
	( getval(show_module,on) -> MGoal = Goal@M ; MGoal = Goal ),
	getval(dbg_goal_format_string, Format),
	getval(dbg_print_depth, PDepth),
	printf(Stream, Format, [PDepth,MGoal])@M.


:- mode port_style(+,-).
port_style(fail, "fail_style") :- !.
% Next line leads to not printing LEAVE in stack display. Use separate style?
%port_style(leave, "fail_style") :- !.
port_style(exit, "exit_style") :- !.
port_style('*exit', "exit_style") :- !.
port_style(_, "call_style").

% set From/To to -1 if source display should not be updated to new positions
:- mode check_if_source_should_update(+,+,+,-,-).
check_if_source_should_update(next, _, _, -1, -1) :- !.
check_if_source_should_update(else, _, _, -1, -1) :- !.
check_if_source_should_update(_, From, To, From, To).

set_tracer_command(Cmd) :-
        setval(tracer_command, Cmd).

:- mode interpret_command(+,+,+,-).
interpret_command("a", CurrentPort, _, Cont) :- !,
	trace_mode(5, 0),
	( CurrentPort = trace_line{port:leave} ->
	    % don't abort, we may not have any catching block!
	    % turn it into a creep instead...
	    Cont = true
	;
	    Cont = abort
	).
interpret_command("l", _, _, true) :- !, trace_mode(2, []).
interpret_command("filter", _, _, true) :- !. % filter set, just continue
interpret_command("s", _, Depth, true) :- !, trace_mode(3, Depth).
interpret_command("n", _, _, true) :- !, trace_mode(5, 0).
interpret_command("N", _, _, true) :- !, trace_mode(5, 0),
	set_flag(debugging,nodebug).
interpret_command("c", _, _, true) :- !, trace_mode(0, []).
interpret_command("i", _, _, true) :- !.
interpret_command("j", _, _, true) :- !.
interpret_command(f(N), Current, _, Cont) :- !,
	Current = trace_line{port:Port,frame:Stack},
	( Port \== fail, Port \== leave, find_goal(N, Stack, Frame) ->
	    Cont = (cut_to_stamp(Frame, chp of tf),fail)
	;
	    Cont = true		% already failing or frame not found
	).
interpret_command("z", Current, _, true) :- !,	% zap to different port
	Current = trace_line{port:Port},
	configure_prefilter(_, _, ~Port, _, dont_care).
interpret_command("", _, _, true) :- !.	% no command, continue as before


%----------------------------------------------------------------------
% Filter goal setup
%----------------------------------------------------------------------

% initial (empty) breakpoint condition
goal_filter(_,_).

reset_usepred_info(none) :- !.
reset_usepred_info(PreviousSpyStatus) :-
        % there is an active filter goal....
        % set the spy status of the template predicate to its previous 
        % state
        % if no defining module, set on whatever predicate is visible 
        % from here
        getval(filter_spy_goal,DefiningModule:TemplatePredSpec),
        (var(DefiningModule) ->  
             set_flag(TemplatePredSpec, spy, PreviousSpyStatus) 
        ; 
             set_flag(TemplatePredSpec, spy, PreviousSpyStatus)@DefiningModule
        ).


set_usepred_info(PredMatchString, PredModuleString, PredDefModuleString, PredConditionString, Status):-
	!,
	% parse the defining module
	term_string(DefiningModule, PredDefModuleString),
	% construct and compile the condition
        concat_string(["goal_filter((", PredMatchString,
		"),(", PredModuleString,
	        ")) ?- block(\\+(\\+ (", PredConditionString,")), _, fail)"],
	    PredMatchConditionString),
        % DefiningModule cannot be a variable!
	term_string(NewGoalFilter, PredMatchConditionString)@DefiningModule,
	compile_term(NewGoalFilter),
	% Find the template's functor and arity
	NewGoalFilter = (goal_filter(Template,_) ?- _),
	(var(Template) ->
	    Status = none,
            setval(filter_status, goal(none)),
	    setval(filter_spy_goal, none)
	;
	    functor(Template, TemplateFunctor, TemplateArity),
	    % compose its PredSpec out of this.
	    TemplatePredSpec = TemplateFunctor/TemplateArity,
	    % get the spy status of the template predicate
	    % if no defining module, look at whatever is visible from here
	    % fails if module or predicate does not currently exist
            (set_spy_status(TemplatePredSpec, DefiningModule) ->
                 % record the PredSpec, DefiningModule in a local variable
                 setval(filter_spy_goal,DefiningModule:TemplatePredSpec),
                 Status = spy_set	       
            ;
                 setval(filter_spy_goal, none),
                 setval(filter_status, off),
                 Status = not_found
            )
        ).


set_spy_status(TemplatePredSpec, DefiningModule) :-
        find_pred_spyinfo(TemplatePredSpec, DefiningModule, PreviousSpyStatus),
        setval(filter_status, goal(PreviousSpyStatus)),
        % set a spypoint on the template predicate
        % The point of this is that if the template option is 
        % used , and a spypoint is put on the template predicate, 
        % no other predicates need be examined, only spied ones.
        % Therefore an efficiency advantage is gained.
        % if no defining module, set one on whatever is visible from here
        set_flag(TemplatePredSpec, spy, on)@DefiningModule.

reenable_usepred :-
        (getval(filter_spy_goal, DefiningModule:PredSpec) ->
             % set_spy_status/2 should not fail here as PredSpec must exist
             set_spy_status(PredSpec, DefiningModule)
        ;
             setval(filter_status, goal(none))
        ).

find_pred_spyinfo(TemplatePredSpec, DefiningModule, PreviousSpyStatus) :-
	(var(DefiningModule) ->  
	     get_flag(TemplatePredSpec, spy, PreviousSpyStatus) 
        ; 
	     current_module(DefiningModule),
	     get_flag(TemplatePredSpec, spy, PreviousSpyStatus)@DefiningModule
	).

%----------------------------------------------------------------------
% Inspect subterm stuff
%----------------------------------------------------------------------

:- local reference(inspect_object).


% eventually these will allow inspect of more than one term
register_inspected_term(Term, Module) :-
	setval(inspect_object, f(Term, Module)), 
	true.


get_inspected_term(current, Term, Module) :-
	( getval(inspect_object, f(Term,Module)) ->
	    true
	;
	    Term = 'No term registered for inspection',
	    get_flag(toplevel_module, Module)
	).
get_inspected_term(invoc(N), Goal, Module) :-
	find_goal_by_invoc(N, _LookupModule, Goal, Module, _, _, _, _).
get_inspected_term(display(I,R,C), Term, Module) :-
	get_matrix_term(I, R, C, Term, Module).


inspect_command(SourceS, Command, Reply) :-
	term_string(Source, SourceS), 
	get_inspected_term(Source, Term, Module),
	process_inspect_command(Command, Term, Reply, Module).


process_inspect_command(end, _Term, _, _M) ?- !.
% exit inspect term. Nothing to be done for now
process_inspect_command(info(Depth,["1"|Path]), Term, Reply, M) ?- !,
% provides a normal printable version and summary of the term with Path.
% merged previous summary and display commands; this allows more flexibility
% on the Tcl side for how subterms are processed.
	provide_subterm(Path, Depth, Term, Reply, M).
process_inspect_command(record_observed(SSource,["1"|Path],Label), _, _Reply, _M) ?- !,
% make a record of a term that is to be observed
	term_string(Source, SSource),
	getval(inspect_observed, ToBeObs),
	setval(inspect_observed, [o(Source,Path,Label)|ToBeObs]).
process_inspect_command(movepath(up,N,["1"|Path]), _Term, Reply, _M) ?- !,
% moves the current subterm up
	reverse(Path, RPath),
	move_up(N, RPath, Reply).
process_inspect_command(movepath(Dir,N,["1"|Path]), Term, Reply, M) ?- !,
% moves the current subterm to the left or right
        move_sideways(Dir, N, Path, Term, Reply, M).
process_inspect_command(select(SourceS), _, Reply, _) ?- !,
% change the inspected item (in Tcl; here just checks that item is valid)
	term_string(Source, SourceS),
	(get_inspected_term(Source, _, _) -> 
	    Reply = "ok" ; Reply = "failed"
        ).
process_inspect_command(childnodes(Type,Arity,LSize,["1"|Path]), Term, Reply, Module) ?- !,
/* returns  `position' information for the children of all inspector's nodes
  These nodes are of different ECLiPSe types, and the `positions'
  can be in a special format which can then be used both by the 
  Tcl and Prolog sides to take special actions in interpreting the path 
  and presenting the children.
  Type + Arity are the type and arity of the node; LSize is the current
  threshold length for treating list specially
*/
	provide_childnodes(Type, Path, Term, Arity, LSize, Reply, Module).
process_inspect_command(modify(PositionS), _, Modifier, _) ?- !,
/* checks to see if the name of an item needs to be modified because of its
   position
*/
        term_string(Position, PositionS),
	position_modifier(Position, Modifier).
process_inspect_command(translate(PositionS), _, Reply, _) ?-
/* translate a special position in the internal path format (e.g. 1=foo) to
   a more readable format for Tcl to print out (e.g. 1 (filedname: foo)
*/
	term_string(Pos, PositionS),
	translate_pos(Pos, Reply).


translate_pos(N=Field, Out) ?-
	integer(N), !,
	(integer(Field) ->
	    concat_string(["structure arg#", N], Out)
	;   concat_string(["Named structure arg, fieldname:", Field], Out)
        ).
translate_pos(N-Attr, Out) ?-
	integer(N), !,
        concat_string(["attribute name:", Attr], Out).
translate_pos(list(N), Out) ?-
	integer(N), !,
	concat_string(["List element (pos: ", N, ")"], Out).
translate_pos(tail(N), Out) ?-
	integer(N), !,
	concat_string(["List tail (pos: ", N, ")"], Out).
translate_pos(N, Out) :- 
	integer(N), !,
	concat_string(["structure arg#", N], Out).
translate_pos(Pos, Out) :-
	open("", string, S),
	printf(S, "unknown position type: %w", [Pos]),
	get_stream_info(S, name, Out),
	close(S).
	

provide_childnodes(attributed, Path, Term, _, _, Reply, Module) ?- !,
	get_subterm_from_path(Path, Term, Term, AVar, Module),
	valid_attributes_listing(AVar, Reply).
provide_childnodes(ncompound, Path, Term, Arity, _, Reply, Module) ?- !,
	get_subterm_from_path(Path, Term, Term, Sub, Module),
	named_structure(Sub, Module, Defs, Arity),
	(foreacharg(Name, Defs), count(I, 1, _), foreach(NameSpec, Reply) do
            term_string(I=Name, NameSpec)
	).
provide_childnodes(list, Path, Term, _, LSize, Reply, Module) ?- !,
	get_subterm_from_path(Path, Term, Term, Sub, Module),
	provide_listnodes(Sub, 1, LSize, Reply, Module).
provide_childnodes(compound, _Path, _Term, Arity, LSize, Reply, _Module) ?- !,
	% just use arity given to avoid cost of finding subterm
	(Arity > LSize ->  % add argument position if > LSize
	    (for(I, 1, Arity), foreach(PosSpec, Reply) do 
		term_string(I=I, PosSpec)
	    )
	;   (for(I, 1, Arity), foreach(I, Reply) do true)
        ).
provide_childnodes(scheduled, _, _, _, _, Reply, _) ?- !,
	Reply = [1].
provide_childnodes(suspended, _, _, _, _, Reply, _) ?- !,
	Reply = [1].
provide_childnodes(exphandle, _, _, _, _, Reply, _) ?- !,
        Reply = [1].
provide_childnodes(_Others, _, _, _, _, Reply, _) :-
	Reply = [].


provide_listnodes(List, ListPos0, LSize, Reply, Module) :- 
	List = [_|Tail],
	ListPos1 is ListPos0 + 1,
	term_string(list(ListPos0), Pos0S),
	get_type(Tail, TType, Module),
	((TType == list, ListPos0 < LSize) ->
	    Reply = [Pos0S|Reply1],
	    provide_listnodes(Tail, ListPos1, LSize, Reply1, Module)
	;   term_string(tail(ListPos1), Pos1S),   
	    Reply = [Pos0S,Pos1S]
        ).

move_sideways(Dir, N, Path, Term, Reply, Mod) :- 
	(get_parent_path(Path, PPath, Pos, ArgNo0) ->
	    get_subterm_from_path(PPath, Term, Term, Parent, Mod),
	    (Dir == right -> ArgNo is ArgNo0 + N ; ArgNo is ArgNo0 - N),
	    get_sibling_arg(Pos, ArgNo, PPath, Parent, Mod, NewPath, Status) 

	; % can't get parent
	    Status = "false"
	),
	(Status == "false" -> NewPath = Path ; true),
	Reply = [Status,["1"|NewPath]]. % add back the root node


get_sibling_arg(_=_, ArgNo0, PPath, Parent, Module, NewPath, Status) ?-
% named structure
	(named_structure(Parent, Module, Defs, Arity) ->
	    get_arg(ArgNo0, Arity, ArgNo, Status),
	    arg(ArgNo, Defs, FName),
            % path position should always be a string
            term_string(ArgNo=FName, PosSpec),
	    append(PPath, [PosSpec], NewPath)
	;   % structure not named structure as expected
	    Status = "false"
	).
get_sibling_arg(Pos, ArgNo, PPath, Parent, _, NewPath, Status) :-
	integer(Pos), !, % normal structure
	functor(Parent, _, A),
	get_arg(ArgNo, A, N1, Status),
        term_string(N1, PosSpec),
	append(PPath, [PosSpec], NewPath).

get_arg(ArgNo, A, N1, Status) :-
	(ArgNo >  A ->
	    Status = "out",
	    N1 = A
	;ArgNo < 1 ->
	    Status = "out",
	    N1 = 1
	;   N1 = ArgNo,
	    Status = "true"
	).

get_parent_path([PosS], PPath, Full, ArgNo) ?- 
	term_string(Pos, PosS),
	valid_pos(Pos, ArgNo), !,
	PPath = [], Pos = Full.
get_parent_path([N|Path], [N|PPath], Pos, ArgNo) :-
	get_parent_path(Path, PPath, Pos, ArgNo).


move_up(N, RPath, [Status,["1"|NewPath]]) :-
	port_remove_levels(N, RPath, RNewPath, Status),
	reverse(RNewPath, NewPath).


provide_subterm(Path, Depth, Term, Reply, M) :-
	get_flag(output_mode, OM),
	(Path == [] -> %toplevel goal
	    concat_string(["%*",OM,"w"], DF)
	;   concat_string(["%*",OM,"Gw"], DF)
        ),
	get_subterm_from_path(Path, Term, Term, Sub, M),
	open("", string, S),
	printf(S, DF, [Depth, Sub]),
	get_stream_info(S, name, Out),
	close(S),
	get_type(Sub, Type, M),
	get_summary_info(Type, Sub, Arity, Summary),
	Reply = [Out, Summary, Type, Arity].


get_type(Sub, Type, M) :-
	type_of(Sub, Type1),
	refine_type(Type1, Sub, M, Type).

refine_type(var, Var, _, Type) :- !,
	(meta(Var) -> Type = attributed ; Type = var).
refine_type(compound, C, M, Type) :- !,
	( named_structure(C, M, _, _) ->
	    % ncompound is a structure with field names
	    Type = ncompound 
	; C  = [_|_] ->  % a list (may be non-proper) 
	    Type = list
	;   
            Type = compound
	).
refine_type(goal, S, _, Type) :- !,
	get_suspension_data(S, state, State),
	(State == 0 ->
	    Type = suspended 
	; State == 1 ->
	    Type = scheduled
	; Type = dead
	).
refine_type(handle, H, _, Type) :- 
        is_expandable_handle(H, _),
        Type = exphandle.
refine_type(Type, _Var, _, Type).


% check that a particular arg position is valid
valid_pos(N0, N) :- integer(N0), !, N = N0. 
valid_pos(N0=_, N) ?- integer(N0),  N = N0. %named struct
%valid_pos(list(N0), N) ?- integer(N0), !, N = N0.
%valid_pos(tail(N0), N) ?- integer(N0), !, N = N0.


get_summary_info(Type, Term, A, Out) ?-
	get_functorarity(Type, Term, F, A),
	open("", string, S),
	seek(S, end_of_file),
	print_subterm(Type, S, F, A),
	get_stream_info(S, name, Out),
	close(S).


print_subterm(attributed, S, V, _A) :- !,
	write(S, V).
print_subterm(var, S, V, _A) :- !,
	write(S, V).
print_subterm(_T, S, F, A) :-
	writeq(S, F),
	(A \== -1 ->  % a type with valid arity
	   write(S, "/"),
	   writeq(S, A)
         ; true
        ).


get_functorarity(compound, Term, F, A) ?- !,
	functor(Term, F, A).
get_functorarity(ncompound, Term, F, A) ?- !,
	functor(Term, F, A).
get_functorarity(list, _, F, A) ?- !,
	A =  2, F = '.'.
get_functorarity(var, Var, F, A) ?- !, 
	A = -1, F = Var.
get_functorarity(attributed, Var, F, A) ?- !, 
	A = -1, F = Var.
get_functorarity(atom, Atom, F, A) ?- !,
	A = 0, F = Atom.
get_functorarity(integer, I, F, A) ?- !,
	A = -1, F = I.
get_functorarity(float, J, F, A) ?- !,
	A = -1, J = F.
get_functorarity(breal, J, F, A) ?- !,
	A = -1, J = F.
get_functorarity(rational, R, F, A) ?- !, 
	A = -1, R = F.
get_functorarity(string, S, F, A) ?- !,
	A = -1, S = F.
get_functorarity(handle, S, F, A) ?- !,
	A = -1, S = F.
get_functorarity(Susp, S, F, A) :-
	(Susp == suspended ; Susp == scheduled ; Susp == dead), !,
	A = -1, S = F.
get_functorarity(_Unk, S, F, A) :-  
/* unknown type, catch it to avoid failure */
        A = -1, S = F.

% This code should go somewhere else; but it is different from the
% navigate subterm for the tty interface, because you can choose
% somewhere else entirely on the tree. 
% get_subterm_from_path(+Path, +Top, +Current, -SubTerm, +Module)
get_subterm_from_path([], Top, Current, Sub, Mod) ?- !, 
	written_term(Top, Current, Sub, Mod).
get_subterm_from_path([PosS|Path], Top, Current, Sub, M) ?-
	term_string(Pos, PosS),
	(get_subterm_child(Pos, Current, Top, Child, M) ->
	    get_subterm_from_path(Path, Top, Child, Sub, M)
	  ; printf(error, "%n *** can't follow path %w in %w%n%b", [Pos,Current])
        ).

get_subterm_child(Pos-_AttName, AVar, _, Attribute, _Module) ?-
% attribute of an attributed var AVar.
	integer(Pos), !,
	get_attribute(AVar, Attribute, Pos).
get_subterm_child(Pos=_FieldName, Current, Top, Child, Module) ?- !,
% structure with field names
	get_subterm_child(Pos, Current, Top, Child, Module).
get_subterm_child(list(Pos), Current0, Top, Child, Module) ?- !,
	written_term(Top, Current0, Current, Module),
	list_nth(Pos, Current, Child, _).  
get_subterm_child(tail(Pos), Current0, Top, Tail, Module) ?- !,
	Pos0 is Pos - 1,
	written_term(Top, Current0, Current, Module),
	list_nth(Pos0, Current, _, Tail).
get_subterm_child(Pos, Current0, Top, Child, Module) :-
	written_term(Top, Current0, Current, Module),
	( compound(Current) ->
              functor(Current, _, A),
              integer(Pos),
              A >= Pos, Pos > 0,
              arg(Pos, Current, Child)
        ; is_handle(Current), is_expandable_handle(Current, Child) ->
              true
	; is_suspension(Current) -> 
	      get_suspension_data(Current, goal, Child), Pos == 1
	).


is_expandable_handle(H, Exp) :-
        set_event_handler(141, fail/0),
        block(
                   (xget(H, 0, Exp) -> 
                        reset_event_handler(141)
                   ;
                        reset_event_handler(141), fail
                   ), Tag, (reset_event_handler(141), exit_block(Tag)) 
        ).

list_nth(1, [E0|Ls], E, Tail) ?- !,
	E = E0, Tail = Ls.
list_nth(N0, [_|Ls], E, Tail) :-
	N0 > 1, 
	N1 is N0 - 1,
	list_nth(N1, Ls, E, Tail).

position_modifier(_Index-AttName, Modifier) ?- !,
% Is an attribute
	concat_string([AttName, ':  '], Modifier).
position_modifier(_ArgPos=FieldName, Modifier) ?- !,
% structure with named fields
	concat_string([FieldName, ':  '], Modifier).
position_modifier(list(N), Modifier) ?- !,
	(N == 1 ->
	    Modifier = "[   " ; Modifier = ",   "
	).
position_modifier(tail(_), Modifier) ?- !,
	Modifier = "|   ".
position_modifier(_, "").


inspect_get_children_for_path(SourceS, ChildCommand, PrintDepth, 
						ChildPosList, ChildInfoList) :-
	ChildCommand = childnodes(_, _, _, PPath),
	inspect_command(SourceS, ChildCommand, PosList),
	( foreach(Pos, PosList), 
	  foreach(Child, ChildInfoList), 
	  foreach(CPath, ChildPosList), param(SourceS, PrintDepth, PPath) do 
	    ( string(Pos) ->
		PosS = Pos
	    ;
		term_string(Pos, PosS)
	    ),
	    append(PPath, [PosS], CPath),
	    inspect_command(SourceS, info(PrintDepth, CPath), UnmodifiedChild),
	    ( integer(Pos) ->
		Child = UnmodifiedChild
	    ;
		inspect_command(SourceS, modify(PosS), Modifier),
		UnmodifiedChild = [PrintTerm, Summary, Type, Arity],
		concat_strings(Modifier, PrintTerm, ModifiedPrintTerm),
		concat_strings(Modifier, Summary, ModifiedSummary),
		Child = [ModifiedPrintTerm, ModifiedSummary, Type, Arity]
	    )
	).


%----------------------------------------------------------------------
% Output mode setting
%----------------------------------------------------------------------

:- mode get_tracer_output_modes(-).
get_tracer_output_modes(Modes) :-
	getval(dbg_goal_format_string, Format),
	split_string(Format, "G", "%*Gw", ModeList),
	concat_string(ModeList, Modes).

:- mode set_tracer_output_modes(+).
set_tracer_output_modes(Modes) :-
	concat_string(["%*G",Modes,"w"], Format),
	setval(dbg_goal_format_string, Format).


:- mode get_tracer_print_depth(-).
get_tracer_print_depth(Depth) :-
	getval(dbg_print_depth, Depth).


:- mode set_tracer_print_depth(+).
set_tracer_print_depth(Depth) :-
	setval(dbg_print_depth, Depth).


%-------------------------------------------------------------------
%  Grace-like matrix display of variables
%-------------------------------------------------------------------

%:- open(queue(""), update, matrix_out_queue, [yield(on)]).

/*
commands sent by Prolog:
% note Id should always be first arg.

   setup(Id, Name, NRow, NCol, Module)
       setup display matrix (from Module) with Name and Id, of size NRowxNCol

   displ(Id, Row, Col, String, TermState, BackorForward)
       String is the printed representation of Term at matrix Id at Row,Col.
       TermState is the status of the term (for break-points), BackorForward
       is if this value is from backtracking or forward execution

   kill(Id)
       Kill the display matrix Id

   interact(Id)
       interact with user at display matrix Id

Id is used to identify a matrix rather than Name because it is not certain that
a matrix window will dissapear beyond its `logical scope'. Id is monotonically 
increasing number assigned by the system that ensure each new matrix has a
unique number

Need to keep two variable containers:

matdisplayid:     the current value of the id. This is incremented whenever
                  a new matrix is created. Is a variable.

matdisplaydata:   this keeps the actual information associated with all the
                  matricies. Is a reference.
*/

:- local struct(matrix(id,name,module,matrix,suspl)).

convert_list_to_matrix(List, 0, Matrix) :- !,
	Matrix =.. [[]|List].
convert_list_to_matrix(List, N, Matrix) :-
	integer(N), N > 0, !,
	length(List, L),
	(N >= L ->
	    Matrix =.. [[]|List]
	;   divide_list(N, List, LLists, unused, 1, M),
	    dim(Matrix, [M,N]),
	    (foreach(L, LLists), foreacharg(Row, Matrix) do
	         Row =.. [[]|L]
	    )
	).

divide_list(N, List0, LLists0, Fill, M0, M) :-
	make_one_sublist(N, List0, List1, Sub, Fill),
	(List1 == [] ->
	    LLists0 = [Sub],
	    M0 = M
	;
	    LLists0 = [Sub|LLists1],
	    M1 is M0 + 1,
	    divide_list(N, List1, LLists1, Fill, M1, M)
	).

make_one_sublist(I, [], List1, Sub, Fill) ?- !,
	List1 = [],
	(foreach(E, Sub), count(_, 1, I), param(Fill) do
            E = Fill
	).
make_one_sublist(0, List0, List1, Sub, _) ?- !, List1 = List0, Sub = [].
make_one_sublist(I0, List0, List, Sub0, Fill) ?- 
	List0 = [E|List1], Sub0 = [E|Sub1],
	I1 is I0 - 1,
	make_one_sublist(I1, List1, List, Sub1, Fill).


make_display_matrix_body(Matrix, Name, Module) :-
	make_display_matrix_body(Matrix, 1, any, constrained, Name, Module).


/* make_display_matrix_body(+Matrix, +Prio, +Type, +SList, +Name)
     creates a term display matrix. Matrix is either a list or a matrix of
     terms. Prio is the priority the demon would be suspended at, Type is 
     what type of information would be displayed, SList is the suspension
     list the demon's suspension would be added to, and Name is the name
     used for this display matrix
*/
make_display_matrix_body(List/NRow, Prio, Type, SList, Name0, Module) ?- !,
	convert_list_to_matrix(List, NRow, Matrix),
	make_display_matrix_body(Matrix, Prio, Type, SList, Name0, Module).
make_display_matrix_body(List, Prio, Type, SList, Name0, Module) :- 
	List = [_|_], !, % is a list
	convert_list_to_matrix(List, 0, Matrix),
	make_display_matrix_body(Matrix, Prio, Type, SList, Name0, Module).
make_display_matrix_body(Matrix, Prio, Type, SList, Name, Module) :-
	compound(Matrix), \+(Matrix = _/_),
	display_matrix_dim(Matrix, 2, Dims),
	add_matname(Name, Matrix, Module, Id, SL, Module),
	(set_up_matdisplay(Dims, Matrix, Prio, Type, SList, Name, Id, SL, Module) -> 
	    true ; sepia_kernel:set_bip_error(5)
	),
	% kill window on backtracking
	!, (true ; kill_matdisplay(Name,Module,_), fail).
make_display_matrix_body(Matrix, Prio, Type, SList, Name, Module) :-
	sepia_kernel:get_bip_error(Error),
	(Error == 5 -> kill_matdisplay(Name,Module,_) ; true),
	error(Error,  make_display_matrix(Matrix, Prio, Type, SList, Name), Module).

/* display_matrix_dim(+Matrix, +MaxDepth, -Dim) 
   returns the dimensions of a display matrix Matrix. MaxDepth is the max.
   number of dimensions that a matrix will be decomposed to for a 
   display_matrix (cannot be more than 2 as display matrix must be 2d or less)
*/
display_matrix_dim(_Matrix, 0, Dim) :- !, Dim = [].
display_matrix_dim(Matrix, N, [D|Ds]) :-
	compound(Matrix), 
	functor(Matrix, [], D), 
	N1 is N - 1,
        (foreacharg(Row, Matrix), param([Ds,N1]) do
            display_matrix_dim(Row,N1, Ds)
	), !.
display_matrix_dim(_Matrix, _, []).
	
gen_mat_name(Name0, Module, FName) :-
	(atomic(Name0) ->
	    concat_string([Name0], NameS),
	    FName = NameS@Module
	;   sepia_kernel:set_bip_error(5)
        ).

set_up_matdisplay([N], Matrix, Prio, Type, SList, Name, Id, SL, Module) ?- !,
	init_matdisplay(1, N, Name, Id, Module),
	set_up_matrowdis(1, N, Matrix, Prio, Type, SList, Id, SL, []),
	matdisplay_interact(Id).
set_up_matdisplay([M,N], Matrix, Prio, Type, SList, Name, Id, SL, Module) ?-
	init_matdisplay(M, N, Name, Id, Module),
	(foreacharg(Row, Matrix), param(N,Prio,Type,SList,Id), count(I, 1, M),
	 fromto(SL, S0, S1, []) do
            set_up_matrowdis(I, N, Row, Prio, Type, SList, Id, S0, S1)
	), matdisplay_interact(Id).

set_up_matrowdis(CurrentRow, ColSize, Row, Prio, Type, SList, Id, SLIn, SLOut) :-
	(foreacharg(E, Row), count(I, 1, ColSize), fromto(SLIn, SL0, SL1, SLOut), 
	 param(CurrentRow,Prio,Type,Id,SList) do
           set_up_mattermdis(E, CurrentRow, I, Prio, Type, SList, Id, SL0, SL1)
        ).


set_up_mattermdis(E, Row, Col, Prio, Type, SList, Id, [Susp|Out], Out) :-
	(nonground(E) ->
	    get_display_string(E, Type, String, _),
	    suspend(term_display_demon(E,Row,Col,Id,Type,String, Susp), Prio, E->SList, Susp),
	    Type1 = Type
	;   Type1 = none
        ), 
	send_display_elm(E, Row, Col, Id, Type1, _).

matdisplay_interact(Id) :-
	write_exdr(matrix_out_queue, interact(Id)),
	flush(matrix_out_queue).


:- demon term_display_demon/7.
:- set_flag(term_display_demon/7, leash, notrace).
:- set_flag(term_display_demon/7, skip, on).

term_display_demon(Term, Row, Col, Id, Type, _Old, Susp) :-
	send_display_elm(Term, Row, Col, Id, Type, String),
	(nonground(Term) -> 
	  get_suspension_data(Susp, goal, Goal),
	  setarg(6, Goal, String)
        ; kill_suspension(Susp) 
        ).
term_display_demon(Term, Row, Col, Id, _Type, Old, _Susp) :-
	(nonground(Term) -> G = nonground ; G = wasground),
	send_display_string(Id, Row, Col, Old, G, back),
	fail.

	
init_matdisplay(NRow, NCol, Name, Id, Module) :-
	write_exdr(matrix_out_queue, setup(Id, Name, NRow, NCol, Module)),
	flush(matrix_out_queue).

kill_matdisplay(Name, Module, Cond) :-
	(shutdown_mat(Name, Id, Module) ->
	    Cond = yes,
	    write_exdr(matrix_out_queue, kill(Id)),
	    flush(matrix_out_queue)
	;   Cond = no
        ).

send_display_elm(E, Row, Col, Id, Type, String) :-
	get_display_string(E, Type, String, TState),
	send_display_string(Id, Row, Col, String, TState, forward).

send_display_string(Id, Row, Col, String, G, State) :-
	write_exdr(matrix_out_queue, disp(Id, Row, Col, String, G, State)),
	flush(matrix_out_queue).

add_matname(Name0,Matrix,Module,Id,SL, Module) :-
	getval(matdisplaydata, Mats),
	getval(matdisplayid, Id0),
	concat_string([Name0], Name),  % make sure it is a string
	NewMat = matrix{name:Name,module:Module},
	(\+member(NewMat, Mats) -> % \+member because 0 terminates list
	    Id is Id0 + 1,
	    setval(matdisplayid, Id),
	    concat_string([Name0], Name),  % make sure it is a string
	    NewMat = matrix{id:Id,matrix:Matrix,suspl:SL},
	    setval(matdisplaydata, [NewMat|Mats])
	;   sepia_kernel:set_bip_error(6)
        ).

shutdown_mat(Name0, Id, Module) :-
	getval(matdisplaydata, Mats),
	concat_string([Name0], Name),
	M = matrix{id:Id,name:Name,module:Module,suspl:Ss},
	memberchk(M, Mats),
        % stop sending of information to GUI side
	(foreach(S,Ss) do kill_suspension(S)).

get_matrix_term(Id, R, C, Term, Mod) :-
	getval(matdisplaydata, Ms),
	member(matrix{id:Id,module:Mod,matrix:Mat}, Ms),
	dim(Mat, Bounds),
	get_subscripts(Bounds, R, C, Sub),
	subscript(Mat, Sub, Term).

get_subscripts([N], R, C0, [C]) :- !,
	(R == 1, N >= C0 ->  C = C0 ; 
	    writeln(error, "Matrix subscript error"),
	    C = N
	).
get_subscripts([N,M], R0, C0, [R,C]) :- !,
	(N >= R0 -> R = R0 
        ;   writeln(error, "Matrix subscript error"),
	    R = N
        ),
	(M >= C0 -> C = C0 
        ;   writeln(error, "Matrix subscript error"),
            C = M
        ).
get_subscripts(_, _R, _C, []) :- 
	writeln(error, "Matrix subscript error").


kill_display_matrix_body(Name@Module, _) ?- !,  % for compatibility only
	kill_display_matrix_body(Name, Module).
kill_display_matrix_body(Name, Module) :-
	kill_matdisplay(Name, Module, Cond), 
	(Cond == yes ->  ! ; sepia_kernel:set_bip_error(6)).
kill_display_matrix_body(Name, Module) :-
	sepia_kernel:get_bip_error(Error),
	error(Error, kill_display_matrix(Name), Module).

get_display_string(E, _Type, String, G) :-
	(nonground(E) -> G = nonground ; G = ground),
	open("", string, S),
	printf(S, "%mQPw", [E]),
	get_stream_info(S, name, String),
	close(S).


%---------------------------------------------------------------------
% Observed terms
%---------------------------------------------------------------------

create_observed([], _) :- !.
create_observed(Os, trace_line{frame:tf{module:Module}}) :- 
	make_observed_list(Os, OL),
	getval(observed_count, Count),
	incval(observed_count),
	concat_atom(['Observing#', Count], Label),
	make_display_matrix(OL/2, Label)@Module.

make_observed_list([], Out) :- !, Out = [].
make_observed_list([o(Source,Path,Label)|Os], Out) :-
	get_inspected_term(Source, Term, Module),
	get_subterm_from_path(Path, Term, Term, Sub, Module),
	Out = [Label,Sub|Out1],
	make_observed_list(Os, Out1).

%----------------------------------------------------------------------
% Library browser
%----------------------------------------------------------------------

init_library_info :-
% collect the available libraries with valid info files
	collect_library_info(Info),
	setval(library_info, Info).

collect_library_info(Info) :-
	get_flag(library_path, LibPaths),
	(foreach(LP, LibPaths), fromto(UnsortedInfo, InfoIn, InfoOut, []) do
            get_flag(eclipse_info_suffix, ISuf),
	    concat_string(["*", ISuf], IFilter),
            read_directory(LP, IFilter, _, Fs0),
	    (foreach(File, Fs0), fromto(InfoIn, Info1, Info2,  InfoOut), 
             param(LP) do
		 (get_module_from_infofile(LP, File, MFInfo) ->
		      Info1 = [MFInfo|Info2] ; Info1 = Info2
		 )
	    )
	),
	sort(module of mfile, <, UnsortedInfo, Info).

get_module_from_infofile(Path, File, MFile) :-
	concat_string([Path, "/", File], FullName),
	get_file_info(FullName, readable, on),
	open(FullName, read, In),
	(read(In, :-module(Module)) -> % module should be first item in file
	    MFile = mfile{dir:Path,module:Module,file:FullName},
	    close(In)
	;   close(In), fail
        ).

% assumes File is readable and is a valid information file
read_interface_file(File, ILines, CLines) :-
	open(File, read, In), 
	read_interface_file1(In, ILines, CLines),
	close(In).

read_interface_file1(In, ILines0, CLines0) :-
	(at_eof(In)  ->
	    ILines0 = [], CLines0 = []
	;   (read(In, :-Line) -> % all info in interface file should be directives
	        filter_interface_line(Line, ILines0, ILines1, CLines0, CLines1)
	    ;   
		% ignore invalid lines
		ILines1 = ILines0,
		CLines1 = CLines0
	    ),
	    read_interface_file1(In, ILines1, CLines1)
	).

filter_interface_line(module(_), ILines0, ILines, CLines0, CLines) ?- !,
	CLines0 = CLines,
	ILines0 = ILines.
filter_interface_line(comment(T,C), ILines0, ILines, CLines0, CLines) ?- !,
	CLines0 = [comment(T,C)|CLines],
	ILines0 = ILines.
filter_interface_line(Line, ILines0, ILines, CLines, CLines) :-
	ILines0 = [Line|ILines].


% find the exported predicates, sort them into Preds, and place other info
% items into Others in the order they occur in the interface file
sort_minfo(MInfo, Preds, Others) :-
	(foreach(Item, MInfo), fromto(Preds0, P0,P1, []), 
	 fromto(Others, O0, O1, []) do
             (Item = export(Name/Arity) ->
		 P0 = [Name/Arity|P1], O0 = O1
	     ;   P0 = P1, O0 = [Item|O1]
	     )
        ),
	sort(Preds0, Preds).

% support for returning the children and node content for libbrowser widget

extract_lbpath_info(["top"|RestPath], PInfo, Deepest) :-
	getval(lbpath_type, [top|PTypes]),
	lbindex_info(RestPath, PTypes, top, PInfo, Deepest).

lbindex_info([], _PTypes, ParentType, PInfo, Deepest) :-
	Deepest = ParentType, PInfo = [].
lbindex_info([P|Ps], [Type|Types], _, [PInfo|PInfo0], Deepest) :-
	get_onelbindex_info(Type, P, PInfo, ActualType),
	lbindex_info(Ps, Types, ActualType, PInfo0, Deepest).

get_onelbindex_info(dir, Dir, PInfo, ActualType) ?- 
	ActualType = dir,
	PInfo = dir:Dir.
get_onelbindex_info(module, SModule, PInfo, ActualType) ?-
	atom_string(Module, SModule), % SModule is a string
	is_lbmodule(Module),
	ActualType = module,
	PInfo = module:Module.
get_onelbindex_info(interface, IntPath, PInfo, ActualType) ?-
	term_string(Item, IntPath),
	(Item = Name/Arity ->
	    PInfo = interface:Name/Arity,
	    ActualType = interface(predicate)
	;   PInfo = interface:Item,
	    ActualType = interface(nonpredicate)
        ).

expand_lbnode(Path, Children) :-
	extract_lbpath_info(Path, PInfo, Deepest),
	return_lbnode_children(Deepest, PInfo, Children).

lbnode_display(Path, DText, Highlight) :-
	extract_lbpath_info(Path, PInfo, Deepest),
	(Deepest = module -> 
	    memberchk(module:M, PInfo),
	    (get_flag(loaded_library, M) ->
		Highlight = current ; Highlight = highlight
	    )
	;
	    Highlight = none
	),
	return_lbnode_text(Deepest, PInfo, DText).

lbnode_info(Path, IsOpen, NodeInfo, Module) :-
	extract_lbpath_info(Path, PInfo, Deepest),
	return_lbnode_info(Deepest, PInfo, IsOpen, NodeInfo, Module).


% load the module Lib
lbnode_loadmodule(Lib) :-
	get_flag(toplevel_module,Top), 
	lib(Lib)@Top.

% predicates to return information on items

return_html_root(Root) :-
	get_flag(installation_directory, ECDir),
	concat_string([ECDir, "/doc/index.html"], RootInternal),
	os_file_name(RootInternal, Root).

return_lbnode_children(top, _, Dirs) ?-
	return_libdirs(Dirs).
return_lbnode_children(dir, PInfo, Modules) ?-
	memberchk(dir:Dir, PInfo),  
	return_modules_in_dir(Dir, Modules).
return_lbnode_children(module, PInfo, InterNodes) ?-
	memberchk(module:Module, PInfo),
	memberchk(dir:Dir, PInfo),
	return_module_info(Module, Dir, minfo{interface:Interface}),
	sort_minfo(Interface, Preds, Others),
	(foreach(P, Preds), fromto(InterNodes, Nodes0, Nodes1, InterNodes1) do
              term_string(P, PIndex),
              Nodes0 = [PIndex|Nodes1]
	),
	(foreach(O, Others), fromto(InterNodes1, Nodes0, Nodes1, []) do
              term_string(O, OIndex),
              Nodes0 = [OIndex|Nodes1]
	).
return_lbnode_children(interface(predicate), _PInfo, Expanded) ?- !,
	% cannot expand predicates yet
	Expanded = [].
return_lbnode_children(interface(nonpredicate), _PInfo, Expanded) ?-
	Expanded = [].


return_lbnode_text(top, _, DText) ?- DText = "libraries".
return_lbnode_text(dir, PInfo, DText) ?-
	DText = Dir,
	memberchk(dir:Dir, PInfo).
return_lbnode_text(module, PInfo, DText) ?-
	memberchk(module:M, PInfo),
	memberchk(dir:Dir, PInfo),
	(return_module_summary(M, Dir, Summary) ->  
	    concat_string([M, " \n   ", Summary], DText)
        ;   atom_string(M, DText)
        ).
return_lbnode_text(interface(predicate), PInfo, DText) ?- !,
	memberchk(interface:Name/Arity, PInfo),
	memberchk(dir:Dir, PInfo),
	memberchk(module:M, PInfo),
	construct_pred_display(Name, Arity, M, Dir, DText).
return_lbnode_text(interface(nonpredicate), PInfo, DText) ?-
	memberchk(interface:Item, PInfo),
	term_string(Item, DText).

construct_pred_display(Name, Arity, M, Dir, DText) :-
	return_pred_comment(M, Dir, Name, Arity, PredCom),
	(PredCom \== [] ->
	    construct_pred_display_from_comments(Name, Arity, PredCom, DText)
	;   term_string(Name/Arity, DText)  % no comment info for pred
        ).

construct_pred_display_from_comments(Name, Arity, PComs, DText) :-
	get_pred_summary(PComs, Summary),
	construct_pred_template(Name, Arity, PComs, Template),
	concat_string([Template, "\n    ", Summary], DText).

get_pred_summary(PComs, Sum) :-
	(memberchk(summary:Sum, PComs) ; Sum = ""), !.

construct_pred_template(Name, Arity, PComs, Template) :-
	(memberchk(template:Template,PComs) ->
	    true
	;
	    return_pred_modes(Name, Arity, PComs, Modes),
	    generalise_modes(Modes, Mode),
	    construct_onepred_template(Name, Arity, PComs, Mode, Template)
	).

construct_onepred_template(Name, Arity, PComs, Mode, DText) :-
	((memberchk(args:ArgDs, PComs), length(ArgDs, Arity)) ->
	    (foreach(ArgDesc, ArgDs), foreach(Name, ANames) do
	        ((ArgDesc = Name0:_, string(Name0)) -> 
		    Name = Name0 ; Name = ""
		)
	    )
	;   
         (count(_,1,Arity), foreach("", ANames) do true)
        ),
	(foreach(AName, ANames), foreacharg(AMode, Mode), foreach(Arg, ArgsString) do
            concat_string([AMode,AName], Arg)
	),
        construct_pred_template_with_args(Name, Arity, ArgsString, DText).


construct_pred_template_with_args(Name, _, Args, DText) :-
	(Args == [] ->
            concat_string([Name], DText)
	;
	    DTextList = [Name, "("|ArgList],
	    construct_arglist(Args, ArgList),
	    concat_string(DTextList, DText)
	).

construct_arglist([Last], Out) ?- !,
	Out = [Last, ")"].
construct_arglist([Arg|Args], Out) ?-
	Out = [Arg, ", "|Out0],
	construct_arglist(Args, Out0).

return_pred_modes(Name, Arity, PComs, Modes) :-
	findall(Mode, (member(amode:Mode, PComs), functor(Mode, Name, Arity)), 
           Modes0),
	(Modes0 == [] ->
	    % no modes found, generate an all '?' mode.
	    functor(GenMode, Name, Arity),
	    (foreacharg(?,GenMode) do true),
	    Modes = [GenMode]
	;   Modes0 = Modes
        ).

return_lbnode_info(top, _, IsOpen, NInfo, M) ?-
	M = "",
	(IsOpen == 1 ->
	    NInfo = [[normal,"ECLiPSe libraries"]] ; NInfo = []
	).
return_lbnode_info(dir, _PInfo, IsOpen, NInfo, M) ?-
	% could add info on purpose of each directory
	M = "",
	(IsOpen == 1 ->
	    NInfo = [[normal,"An ECLiPSe library directory"]] ; NInfo = []
	).
return_lbnode_info(module, PInfo, IsOpen, NInfo, M) ?-
	memberchk(module:M, PInfo),
	(IsOpen == 1 ->
	    memberchk(dir:Dir, PInfo),
	    return_module_desc(M, Dir, MDesc0),
	    ( string(MDesc0) ->
		NInfo = [[normal, MDesc0]]
	    ; MDesc0 = ascii(MDesc) ->
	        NInfo = [[normal, MDesc]]
	    ; NInfo = [] % don't try to cope with non-plain ascii formats
	    )
	;   
            NInfo = []
        ).
return_lbnode_info(interface(predicate), PInfo, _IsOpen, NInfo, M) ?- !,
	memberchk(dir:_Dir, PInfo),
	memberchk(module:M, PInfo),
	memberchk(interface:Name/Arity, PInfo),
	return_pred_info(M, Name, Arity, NInfo).
return_lbnode_info(interface(nonpredicate), PInfo, _IsOpen, NInfo, M) ?-
	memberchk(module:M, PInfo),
	NInfo = [].


generalise_modes([Mode|Modes], GenM) :-
	generalise_modes(Modes, Mode, GenM).

generalise_modes([], Gen, Gen).
generalise_modes([Mode1|Modes], Mode2, Gen) :-
	functor(Mode1, Name, Arity),
	functor(GenMode1, Name, Arity),
	(foreacharg(M1, Mode1), foreacharg(M2, Mode2), foreacharg(G, GenMode1) do
            lub(M1, M2, G)
	),
	generalise_modes(Modes, GenMode1, Gen).

% lub(PX, PY, PLub)
%
% least upper bound of 2 modes (cf. lattice above)

lub(-, Y, LUB) :- 'lub-'(Y, LUB).
lub(++, Y, LUB) :- 'lub++'(Y, LUB).
lub(+-, Y, LUB) :- 'lub+-'(Y, LUB).
lub(-+, Y, LUB) :- 'lub-+'(Y, LUB).
lub(+, Y, LUB) :- 'lub+'(Y, LUB).
lub(?, _, ?).

'lub-'(-, -).
'lub-'(++, -+).
'lub-'(+-, -+).
'lub-'(-+, -+).
'lub-'(+, ?).
'lub-'(?, ?).

'lub+'(-, ?).
'lub+'(++, +).
'lub+'(+-, +).
'lub+'(-+, ?).
'lub+'(+, +).
'lub+'(?, ?).

'lub++'(-, -+).
'lub++'(++, ++).
'lub++'(+-, +-).
'lub++'(-+, -+).
'lub++'(+, +).
'lub++'(?, ?).

'lub+-'(-, -+).
'lub+-'(++, +-).
'lub+-'(+-, +-).
'lub+-'(-+, -+).
'lub+-'(+, +).
'lub+-'(?, ?).

'lub-+'(-, -+).
'lub-+'(++, -+).
'lub-+'(+-, -+).
'lub-+'(-+, -+).
'lub-+'(+, ?).
'lub-+'(?, ?).


return_module_info(Module, Dir, MInfo) :-
	getval(library_info, Info),
	memberchk(mfile{module:Module,dir:Dir, file:File}, Info),
	read_interface_file(File, InterItems, Comments),
	MInfo = minfo{interface:InterItems, comments:Comments}.

return_module_summary(Module, Dir, Summary) :-
	return_module_info(Module, Dir, minfo{comments:MCom}),
	memberchk(comment(summary, Summary), MCom).

return_module_desc(Module, Dir, Desc) :-
	return_module_info(Module, Dir, minfo{comments:MCom}),
	(memberchk(comment(desc, Desc), MCom) -> 
	    true ; Desc = ""
	).

return_modules_in_dir(Directory, Modules) :-
	getval(library_info, Info),
	findall(M, member(mfile{dir:Directory,module:M}, Info), Modules).

return_libdirs(Dirs) :-
	get_flag(library_path, Dirs).

return_pred_comment(M, Dir, Name, Arity, PCom) :-
	return_module_info(M, Dir, minfo{comments:MCom}),
	(memberchk(comment(Name/Arity, PCom), MCom) ->
	    true ; PCom = []
	).

return_pred_info(M, Name, Arity, PredInfo) :-
% just return what help would return
	term_string(M:Name/Arity, PredSpecS),
	gui_help_string(PredSpecS, Info),
	PredInfo = [[normal, Info]].

construct_args_descr(PredCom, PredInfoIn, PredInfoOut) :-
	(memberchk(args:Args, PredCom) ->
	  length(Args, N),  
	  (N > 0 ->
	      open("", string, ArgsDesc),
	      (foreach(Name:Desc, Args), param(ArgsDesc) do
	          printf(ArgsDesc, "%-20s  %s\n", [Name,Desc])
	      ),
	      get_stream_info(ArgsDesc, name, ArgsString),
	      close(ArgsDesc),
	      PredInfoIn = [[heading, "Arguments"], 
                            [normal, ArgsString],[normal,""]|PredInfoOut]
	  ;
	      PredInfoIn = PredInfoOut
	  )
      ;
	  PredInfoIn = PredInfoOut
      ).

% type checks
is_lbmodule(Module) :-
	atom(Module),
	getval(library_info, Info),
	memberchk(mfile{module:Module}, Info).


%----------------------------------------------------------------------
% Handlers for various GUI requests
% Most are called from the GUI via RPCs
%----------------------------------------------------------------------

:- local record(new_source_files).

:- open(queue(""), update, gui_dg_buffer).
%:- open(queue(""), update, gui_dg_info, [yield(on)]). creation done in gui

compile_os_file(OsFile, Module) :-
	os_file_name(File, OsFile),
	block(compile(File, Module), _Tag, true),
	% flush here, because the flushes in the nested emulator
	% within the compiler are ignored...
	flush(warning_output),
	flush(error),
	flush(output).

use_module_os(OsFile, Module) :-
	os_file_name(File, OsFile),
	block(use_module(File)@Module, _Tag, true),
	% flush here, because the flushes in the nested emulator
	% within the compiler are ignored...
	flush(warning_output),
	flush(error),
	flush(output).

list_predicates(Which, Module, AuxFilter, Sorted) :-
	( Which = exported ->
	    Goal = (current_module_predicate(exported_reexported,P)@Module)
	; Which = local ->
	    Goal = (current_module_predicate(local,P)@Module)
	; Which = defined ->
	    Goal = (current_module_predicate(defined,P)@Module)
	; Which = visible ->
	    Goal = (current_predicate(P)@Module;current_built_in(P)@Module)
	; Which = imported ->
	    Goal = (
		(current_predicate(P)@Module;current_built_in(P)@Module),
	    	get_flag(P,visibility,V)@Module,
		memberchk(V,[imported,reexported])
	    )
	;
	    Goal = fail
	),
	findall(PS,
	    (
		not is_locked(Module),
		Goal,
                filter_auxiliary(AuxFilter, P, Module),
		term_string(P,PS)@Module
	    ),
	    Preds),
	sort(0, =<, Preds, Sorted),
	true.

    filter_auxiliary(1, P, Module) :- get_flag(P,auxiliary,off)@Module.
    filter_auxiliary(0, _, _).

flag_value(PredS,Name,M,String) :-
        % this can be called with an empty selection from Tcl...
        PredS \== "",
	term_string(Pred, PredS),
	get_flag(Pred, Name, Value)@M,
	term_string(Value, String).

set_flag_string(PredS,Name,Value,M) :-
	term_string(Pred, PredS),
	set_flag(Pred, Name, Value)@M.

record_source_file(XFile) :-
	os_file_name(File1, XFile),
	canonical_path_name(File1, File2),
	atom_string(File3, File2),
	( recorded(new_source_files, File3) -> true
	; record(new_source_files, File3) ).

get_source_info(PredS, M, OSFile, Offset) :-
        term_string(N/A, PredS),
        atom(N),
        integer(A),
        current_module(M), % may fail
        % source_line and source_offset are for end of last predicate
        get_flag(N/A, source_file, File)@M,
        get_flag(N/A, source_offset, Offset)@M,
        os_file_name(File, OSFile).
        

% list_files/1 returns a list of lists of strings of the form
% ["filename", "status", "module"] where the filename is in the
% syntax of the operating system

list_files(Files) :-
	findall([F,S,M], source_file_status(F,S,M), Files).

    source_file_status(XFile, State, SModule) :-
	current_compiled_file(File,CompileTime,Module),
	( erase(new_source_files, File),fail ; true ), 
	( get_file_info(File,mtime) =:= CompileTime ->
	    State = "ok"
	; get_file_info(File,mtime,_) ->
	    State = "modified"
	;
	    State = "nonexisting"
	),
	atom_string(Module, SModule),
	atom_string(File, SFile),
	os_file_name(SFile, XFile).
    source_file_status(XFile, "new", "") :-
	recorded(new_source_files, File),
	atom_string(File, SFile),
	os_file_name(SFile, XFile).


list_modules(Modules, ToplevelModule) :-
	findall(Module, current_module(Module), Modules),
        get_flag(toplevel_module, ToplevelModule).

% gui_help(+Stream, +Subject)
% prints help on the string Subject onto Stream

gui_help(Stream,SubjectString) :-
	get_stream(output, S),
	set_stream(output, Stream),
	( block(gui_help1(SubjectString), _, fail) ->
	    true
	;
	    printf("No help available on \"%s\"%n", SubjectString)
	),
	set_stream(output, S),
	flush(Stream).

    gui_help1(SubjectString) :-
    	term_string(Subject,SubjectString),	% for name/arity terms
	( var(Subject) ->
	    help(SubjectString)			% for upper case words
	;
	    help(Subject)
	).

% returns the help info as a string (Info), given the pred. spec. as a string
gui_help_string(PredSpecS, Info) :-
	open(string(""), write, s),
	gui_help(s, PredSpecS),
	get_stream_info(s, name, Info),
	close(s).


% gui_dg(+Which,+Trigger,+Filter)
% send delayed goals to gui, filtering out goals according to filter
% Which specifies if the goals should be obtained from the global
% list (0), or from the symbolic trigger Trigger (1)
% currently Filter is: dg_filter(tracedonly, spiedonly,scheduledonly)
% tracedonly: only send traced goals
% speidonly: only send spied goals
% scheduledonly: only send scheduled goals

gui_dg(Which, Trigger, Filter) :-
	get_suspensions(Which, Trigger, Susps),
	( foreach(Susp, Susps), param(Filter) do
	    ( suspension_info(Susp,Filter) -> true ; true ),
	    % flush before the queue buffers get too large...
	    ( at(gui_dg_info) > 32000 ->
		write_exdr(gui_dg_info, end),
	    	flush(gui_dg_info)
	    ; true )
	),
	write_exdr(gui_dg_info, end),
	flush(gui_dg_info).

    get_suspensions(0, _, Susps) :-   % all suspensions
        suspensions(Susps).
    get_suspensions(1, Trigger, Susps) :-
	( is_list(Trigger) ->
	    ( foreach(T, Trigger), fromto(Susps, ThisNext, Next, [])
	    do 
		attached_suspensions(T, This),
		append(This, Next, ThisNext)
	    )
	;
            attached_suspensions(Trigger, Susps)
	).

    suspension_info(S, Filter) :-
	get_suspension_data(S, goal, Goal),
	get_suspension_data(S, module, M),
	get_suspension_data(S, invoc, Invoc),
	get_suspension_data(S, priority, Prio),
	get_suspension_data(S, state, State),
	( get_suspension_data(S, spy, on) -> Spied = 0'+ ; Spied = 0'  ),
	functor(Goal, F, A),
	filter_dg(F/A, Filter, dg_filter{spiedonly:Spied,wakeonly:State}),
	printf(gui_dg_buffer, "%n %c(%d) <%d>  ", [Spied, Invoc, Prio]),
        % delay goals are printed with format and depth options of tracer
	getval(dbg_goal_format_string, Format),
	getval(dbg_print_depth, PDepth),
	printf(gui_dg_buffer, Format, [PDepth,Goal])@M,
	read_string(gui_dg_buffer, end_of_file, LineLength, DGString),
	write_exdr(gui_dg_info, info(State,Prio,Invoc,LineLength,DGString)).

   % filter_dg fails if the Suspended goal is not to be sent to the gui side
   % filter_dg(+PredSpec, +FilterSettings, +FilterValues)
   % FilterSettings is the settings for the various filters from the gui
   % and FilterValues is any value of the delay goal that may be relevant in 
   % determining if a particular filter should be applied. Both are in the
   % dg_filter structure. PredSpec is the predicate spec. for the delayed goal

   filter_dg(F/A, Filter, Values) :-
	state_filter(Filter, Values),
	spied_filter(Filter, Values),
	traced_filter(Filter, F/A).

      spied_filter(dg_filter{spiedonly:1}, dg_filter{spiedonly:Spied}) ?- !,
	  Spied == 0'+.
      spied_filter(_, _).

      state_filter(dg_filter{wakeonly:1}, dg_filter{wakeonly:State}) ?- !,
	  State == 1.
      state_filter(_, _).

      traced_filter(dg_filter{traceonly: 1}, F/A) ?- 
	  get_flag(F/A, leash, notrace), !,
	  fail.
      traced_filter(_, _).

get_triggers(Ts) :-
        findall(T, current_trigger(T), Ts).

get_goal_info_by_invoc(Invoc, Spec, TSpec, Module, LookupModule, Path, From, To) :-
% TSpec is write transformed goal spec.
	find_goal_by_invoc(Invoc, LookupModule, Goal0, Module0, Path0, _Linum, From, To),
        (Path0 == '' -> 
            Path = no 
        ; 
            os_file_name(Path0, OSPath),
            Path = p(OSPath)
        ),
        check_at_wrapper(Goal0, Module0, Goal, Module),
	getval(dbg_goal_format_string, Mode),
	perform_transformation(Goal, Goal, Mode, TGoal, Module),
	functor(Goal, F,A), !,
	functor(TGoal, TF, TA),
	term_string(F/A,Spec),
	term_string(TF/TA,TSpec).
get_goal_info_by_invoc(_, "unknown", "unknown", "unknown", "unknown","","","").

% this catches any Goal@M calls and returns Goal and M as the goal and module
check_at_wrapper(Goal0@M0, _, Goal, M) ?- !,
	Goal = Goal0, M = M0.
check_at_wrapper(Goal, M, Goal, M).


compile_string(String) :-
	get_flag(toplevel_module, M),
	open(String, string, S),
	block(compile_stream(S)@M, _Tag, true),
	close(S),
	flush(warning_output), % for warnings
	flush(error),	       % for errors
	flush(output).	       % for compiled-messages


find_goal_by_invoc(Invoc, DefModule, Goal, Module, Path, Linum, From, To) :-
	getval(exec_state, Current),
	Current = trace_line{frame:Stack},
	find_goal(Invoc, Stack, Frame),
	Frame = tf{goal:Goal, path:Path, line:Linum, from:From, to:To, module:Module},
	get_tf_prop(Frame, module, DefModule).

get_ancestors(Anc) :-
	getval(exec_state, trace_line{frame:Frame}),
	(Frame = tf{parent:Stack} ->
	    get_ancestors_info(Stack, [], Anc)
	    % Anc are returned with oldest first; printing in GUI can then
	    % be from top to bottom 
	;   Anc = [] % no ancestors, 0'th goal
        ).

get_ancestors_info(Frame0, Anc0, Anc) :-
	(Frame0 == 0 ->  % only at depth 0
	     Anc0 = Anc

	 ;   open(string(""), write, SS),
	     make_trace_line(SS, trace_line{port:'....',frame:Frame0},
                             Depth, _Port, Invoc, Prio, _Path, _Linum, _From, _To),
 	     get_stream_info(SS, name, Line),
	     close(SS),
	     Frame0 = tf{parent: PFrame},
	     get_ancestors_info(PFrame, [a(Depth,Invoc,Prio,Line)|Anc0], Anc)
	 ).

get_current_traceline(Depth, Style, Line, Invoc) :-
        getval(exec_state, Current),
        open(string(""), write, SS),
	make_trace_line(SS, Current, Depth, Port, Invoc, _Prio, _Path, _Linum, _From, _To),
	get_stream_info(SS, name, Line),
	close(SS),
	port_style(Port, Style).

is_current_goal(Invoc, Style) :-
        getval(exec_state, trace_line{frame:Frame,port:Port}),
        Frame = tf{invoc:Invoc},
        port_style(Port, Style).

%-------------------------------------------------------------------
% statistics reporting
%-------------------------------------------------------------------

%:- open(queue(""), update, statistics_out_queue, [yield(on)]).

report_stats(Int, Stats) :-
	get_memory(Stats, Stats1),
	get_times(Stats1),
	set_event_handler(stat_report, reporting/0),
	event_after_every(stat_report, Int).

change_report_interval(New) :-
	cancel_after_event(stat_report, _),
	event_after_every(stat_report, New).

stop_report_stats :-
	cancel_after_event(stat_report, _).

reporting :-
        \+ \+ gen_and_send_stats. % recover memory used when generating stats

gen_and_send_stats :-
	get_memory(Stats, Stats1), 
	get_times(Stats1),
	write_exdr(statistics_out_queue, Stats), 
	flush(statistics_out_queue). 

get_memory(Stats, Tail) :-
	get_flag(max_global_trail, MaxGT),
	statistics(global_stack_allocated, GAlloc),
	statistics(global_stack_used, GUsed),
	statistics(trail_stack_allocated, TAlloc),
	statistics(trail_stack_used, TUsed),
	get_flag(max_local_control, MaxLC),
	statistics(local_stack_allocated, LAlloc),
	statistics(local_stack_used, LUsed),
	statistics(control_stack_allocated, CAlloc),
	statistics(control_stack_used, CUsed),
	statistics(shared_heap_allocated, SHAlloc),
	statistics(shared_heap_used, SHUsed),
	Stats = [[memory, "global and trail stacks", MaxGT, "maximum size of global/trail stacks", stack(global, GAlloc, GUsed), stack(trail, TAlloc, TUsed)], 
           [memory, "local and control stacks", MaxLC, "maximum size of local/control stacks", stack(local, LAlloc, LUsed), stack(control, CAlloc, CUsed)],
	   [memory, "shared heap", SHAlloc, "currently allocated size of shared heap", stack(shared, SHAlloc,SHUsed)]
	   |Tail].

get_times(Stats) :-
	statistics(times, [User, _System, Real]),
	statistics(gc_time, GCTime),
	statistics(gc_number, NGC),
	statistics(gc_collected, Collected),
	statistics(gc_ratio, GCRatio),
	Stats = [[times, User, Real, gc(GCTime, NGC, Collected, GCRatio)]].

start_report_stats_text_summary(Int) :-
        report_stats_text_summary,
	stop_report_stats_text_summary,
	set_event_handler(stat_report_text_summary, report_stats_text_summary/0),
	event_after_every(stat_report_text_summary, Int).

stop_report_stats_text_summary :-
	cancel_after_event(stat_report_text_summary, _).

report_stats_text_summary :-
        \+ \+ statistics(statistics_text_summary_queue).


%----------------------------------------------------------------------
% source debugging/breakpoints
%----------------------------------------------------------------------

file_is_readable(OSFile) :-
        os_file_name(File, OSFile),
        get_file_info(File, readable, on). % may fail

read_file_for_gui(OSFile) :-
        os_file_name(File, OSFile),
        get_file_info(File, readable, on), % may fail
        open(File, read, S),
        repeat,
        ( read_string(S, end_of_file, 32000, Part) ->
            write_exdr(gui_source_file, Part),
            flush(gui_source_file),
            fail
        ;
            !
        ),
        write_exdr(gui_source_file, ""),
        flush(gui_source_file),
        close(S).

toggle_source_breakpoint(OSFile, Line, PortLine, From, To) :-
        os_file_name(File, OSFile),
        find_matching_breakport(File, Line, FullName, DMs, PortPreds, PortLine),
        ( foreach(PortPred, PortPreds), foreach(DM, DMs),
          param(FullName, PortLine, From, To)
        do
            get_flag(PortPred, break_lines, PInfo)@DM,
            ( portline_state(FullName, PortLine, PInfo, From) ->
                (From == on -> To = off ; To = on),
                set_proc_flags(PortPred, break, PortLine, DM)
            ;
                % don't toggle if there is a difference in break status 
                true 
            )
        ).

    portline_state(File,Line, PInfo, From) :-
	store_create(Cache),
        (member(File0:Line, PInfo), cached_canonical_path_name(File0,File,Cache) -> 
            From = on 
        ; 
            From = off
        ).

breakpoints_for_file(OSFile, BreakLines, PortLines, Preds) :-
        os_file_name(File, OSFile),
        get_portlist_from_file(File, port_lines, _, Ports),
        get_portlist_from_file(File, break_lines, _, Breaks),
        ( foreach(port(PL-_,PredSpec), Ports), 
          foreach(PL, PortLines), foreach((PredString,PL), Preds0) 
        do 
            term_string(PredSpec, PredString)
        ),
        ( foreach(port(BL-_,_), Breaks), foreach(BL, BreakLines) do true),
        sort(1, <, Preds0, Preds).

find_matching_callinfo(OSFile, Line, PortPredS, CallSpec) :-
        os_file_name(File, OSFile),
        % ignore problem with possible multiple modules for the same file
        find_matching_breakport(File, Line, FullName, [DM|_], [PortPred|_], PortLine),
        get_flag(PortPred, port_lines, LInfos)@DM,
        get_flag(PortPred, port_calls, CInfos)@DM,
        term_string(PortPred, PortPredS), 
        get_callinfo(FullName:PortLine, LInfos, CInfos, CallSpec).

find_exact_callinfo(OSFile, Line, CallSpec) :-
        % OSFile must be an atom
        os_file_name(File0, OSFile),
	store_create(Cache),
        cached_canonical_path_name(File0, File, Cache),
        current_predicate_with_port(port_lines, PredSpec, Module, File1:Line),
        cached_canonical_path_name(File1, File, Cache),
        !,
        get_flag(PredSpec, port_lines, LInfos)@Module,
        get_flag(PredSpec, port_calls, CInfos)@Module,
        get_callinfo(File:Line, LInfos, CInfos, CallSpec).
                
get_callinfo(File:Line, [File0:Line|_], [CallSpec|_], CallSpec) :- 
        canonical_path_name(File0, File), !.
get_callinfo(PredPos, [_|PInfos], [_|CInfos], CallSpec) :-
        get_callinfo(PredPos, PInfos, CInfos, CallSpec).

% On Windows, canonical_path_name can be really slow!
cached_canonical_path_name(Path, CanPath, Cache) :-
	( store_get(Cache, Path, CanPath0) ->
	    true
	;
	    canonical_path_name(Path, CanPath0),
	    store_set(Cache, Path, CanPath0)
	),
	CanPath0 = CanPath.

%----------------------------------------------------------------------
% Initialise toplevel module
%----------------------------------------------------------------------
init_toplevel_module :-
	get_flag(toplevel_module,  Top), 
	erase_module(Top),
	create_module(Top, [], eclipse_language).

%----------------------------------------------------------------------
% Installation - the part that redefines existing toplevel interface
% Must be called before gui tools can be used
%----------------------------------------------------------------------

install_guitools :-
% if trace_line_handler_tcl already set, don't do it again
	(get_event_handler(252,trace_line_handler_tcl/2,tracer_tcl) ->
	    true
	;
% openning of these queues now done at attachment
%	    open(queue(""), update, debug_output, [yield(on)]),
%	    open(queue(""), update, debug_traceline, [yield(on)]),
	    set_default_error_handler(250, trace_start_handler_tcl/0),
	    reset_event_handler(250),
	    set_default_error_handler(252, trace_line_handler_tcl/2),
	    reset_event_handler(252),

	    ( get_flag(hostarch, "i386_nt") ->
		true
	    ;
		% Catch fatal signals - this is mainly intended for tkeclipse, 
		% to stop the window from disappearing on such a signal.
		% It is important that these are asynchronous handlers which,
		% on Unix, execute on their own sigstack. Otherwise they
		% wouldn't work on C stack overflow.
		( current_interrupt(_, segv) ->
		    set_interrupt_handler(segv, catch_fatal/0) ; true ),
		( current_interrupt(_, bus) ->
		    set_interrupt_handler(bus, catch_fatal/0) ; true )
	    )
	).

%----------------------------------------------------------------------
% Uninstallation - undo the installation of guitools' event handlers
% Does not try to install another debugger
%----------------------------------------------------------------------

uninstall_guitools :-
	set_default_error_handler(250, true/0), 
	reset_event_handler(250),
	set_default_error_handler(252, true/0), 
	reset_event_handler(252).

%----------------------------------------------------------------------
% Interrupt handlers
%----------------------------------------------------------------------

catch_fatal :-
	exit_block(fatal_signal_caught).

%----------------------------------------------------------------------
% Saros Filename involved predicates
%----------------------------------------------------------------------

saros_get_library_path(OSDirs) :-
	get_flag(library_path,Dirs),
	( foreach(Dir, Dirs), foreach(OSDir, OSDirs) do
	    os_file_name(Dir, OSDir)
	).

saros_set_library_path(OSDirs) :-
	( foreach(OSDir, OSDirs), foreach(Dir, Dirs) do
	    os_file_name(Dir, OSDir)
	),
	set_flag(library_path,Dirs).

saros_compile(OSFile) :-
	os_file_name(File, OSFile),
	compile(File).

saros_fcompile(OSFile, OSOutDir) :-
	get_flag(toplevel_module, Module), 
	os_file_name(File, OSFile),
	os_file_name(OutDir, OSOutDir),
	Options = [compile:no, outdir:OutDir],
	fcompile:fcompile(File, Options)@Module.

saros_icompile(OSFile, OSOutDir) :-
	get_flag(toplevel_module, Module), 
	os_file_name(File, OSFile),
	os_file_name(OutDir, OSOutDir),
	document:icompile(File, OutDir)@Module.

saros_eci_to_html(OSFile, OSHtmlTopDir, Header) :-
	os_file_name(File, OSFile),
	os_file_name(HtmlTopDir, OSHtmlTopDir),
	document:eci_to_html(File, HtmlTopDir, Header).
	
saros_ecis_to_htmls(OSDirs, OSHtmlTopDir, LinkBack, SystemName) :-
	( is_list(OSDirs) ->
	    ( foreach(OSDir, OSDirs), foreach(Dir, Dirs) do
		os_file_name(Dir, OSDir)
	    )
	;
		os_file_name(Dirs, OSDirs)
	),
	os_file_name(HtmlTopDir, OSHtmlTopDir),
	document:ecis_to_htmls(Dirs, HtmlTopDir, LinkBack, SystemName).

saros_cd(OSDir) :-
	os_file_name(Dir, OSDir),
	cd(Dir).

saros_use_module(OSFile) :-
	os_file_name(File, OSFile),
	use_module(File).

saros_get_goal_info_by_invoc(Invoc, UseLookupModule, Spec, TSpec, 
                             Module, LookupModule, Path, From, To, Spied) :-
	get_goal_info_by_invoc(Invoc, Spec, TSpec, 
                               Module, LookupModule, Path, From, To),
	( LookupModule == "unknown" ->
	    Spied = "off"
	;
	    ( UseLookupModule = 1 ->
		flag_value(Spec, spy, LookupModule, Spied)
	    ;
		flag_value(Spec, spy, Module, Spied)
	    )
	).

	

