%----------------------------------------------------------------------
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
% Copyright (C) 2002-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
%
% Port Counting Profiler
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: port_profiler.ecl,v 1.1 2008/06/30 17:43:48 jschimpf Exp $
% Authors:	Joachim Schimpf, IC-Parc
%
%----------------------------------------------------------------------

:- module(port_profiler).

%:- pragma(nodebug).

%:- import struct(tf), struct(trace_line) from sepia_kernel.
:- import sepia_kernel.

:- import
	get_tf_prop/3
    from sepia_kernel.


:- lib(module_options).


:- local struct(options(
	border,
	format,
	output,
	ports,
	predicates,
	show_caller,
	show_module,
	width
    )).

valid_option_field(border, border of options).
valid_option_field(format, format of options).
valid_option_field(output, output of options).
valid_option_field(ports, ports of options).
valid_option_field(predicates, predicates of options).
valid_option_field(show_caller, show_caller of options).
valid_option_field(show_module, show_module of options).
valid_option_field(width, width of options).

valid_option_value(border, B) :- !, integer(B).
valid_option_value(format, txt) :- !.
valid_option_value(format, html) :- !.
valid_option_value(format, raw) :- !.
valid_option_value(output, default) :- !.
valid_option_value(output, file(F)) :- !, (atom(F) -> true ; string(F)).
valid_option_value(output, dir(F)) :- !, (atom(F) -> true ; string(F)).
valid_option_value(output, stream(F)) :- !, atomic(F).
valid_option_value(ports, _) :- !.
valid_option_value(predicates, all) :- !.
valid_option_value(predicates, spied_only) :- !.
valid_option_value(width, W) :- !, integer(W).
valid_option_value(_, off) :- !.
valid_option_value(_, on) :- !.


default_options(options{
	border:0,
	format:txt,
	output:default,
	ports:all,
	predicates:all,
	show_caller:on,
	show_module:off,
	width:80
}).



%----------------------------------------------------------------------
% The data is stored in a global hash table
%----------------------------------------------------------------------

:- local store(counters).

:- local variable(last_profile_parameters).

%----------------------------------------------------------------------
% The trace port handler for data collection
%----------------------------------------------------------------------

profiler_start_handler :-
	store_erase(counters).


profiler_port_handler_simple(_, trace_line{port:Port, frame:Frame}) :-
	Frame = tf{goal:Goal},
	functor(Goal, F, A),
	get_tf_prop(Frame, module, DM),

	store_inc(counters, key(Port, DM:F/A)).

profiler_port_handler(_, trace_line{port:Port, frame:Frame}) :-
	Frame = tf{goal:Goal, parent:ParentFrame},
	functor(Goal, F, A),
	get_tf_prop(Frame, module, DM),

	ParentFrame = tf{goal:ParentGoal},
	functor(ParentGoal, ParentF, ParentA),
	get_tf_prop(ParentFrame, module, ParentDM),

	store_inc(counters, key(Port, (DM:F/A from ParentDM:ParentF/ParentA))).


install_handler(options{show_caller:on}) :- !,
	set_event_handler(250, profiler_start_handler/0),
	set_event_handler(252, profiler_port_handler/2).
install_handler(options{show_caller:off}) :- !,
	set_event_handler(250, profiler_start_handler/0),
	set_event_handler(252, profiler_port_handler_simple/2).

uninstall_handler :-
	reset_event_handler(250),
	reset_event_handler(252).


%----------------------------------------------------------------------
% User interface
%----------------------------------------------------------------------

:- export port_profile/2.
:- tool(port_profile/2, port_profile_/3).

port_profile_(Goal, OptionList, Module) :-
	( get_options(OptionList, Options) ->
	    term_string(Goal, GoalString),
	    install_handler(Options),
	    ( Options = options{predicates:spied_only} ->
	    	TraceGoal = debug(Goal)
	    ;
	    	TraceGoal = trace(Goal)
	    ),
	    block(
		( TraceGoal@Module ->
		    Result = true
		;
		    Result = fail
		),
		Exception,
		Result = exit_block(Exception)
	    ),
	    uninstall_handler,
	    list_results(Options, GoalString),
	    setval(last_profile_parameters, GoalString-Options),
	    call(Result)
	;
	    printf(error, "Invalid option list: %w%n", [OptionList]),
	    print_default_options(error),
	    abort
	).

:- export port_profile/1.
port_profile(Goal) :-
	port_profile(Goal,[]).

:- export last_port_profile/1.

last_port_profile(OptionList) :-
	( get_options(OptionList, Options) ->
	    getval(last_profile_parameters, LastProfileParameters),
	    ( var(LastProfileParameters) ->
		printf(error, "No goal profiled, run port_profile/2 first!%n", []),
		abort
	    ;
		LastProfileParameters = GoalString-OldOptions,
		( compatible_options(OldOptions, Options) ->
		    list_results(Options, GoalString)
		;
		    printf(error, "Options not compatible with collected data: %w%n", [OptionList]),
		    abort
		)
	    )
	;
	    printf(error, "Invalid option list: %w%n", [OptionList]),
	    print_default_options(error),
	    abort
	).

compatible_options(options{predicates:P1,show_caller:SP1},
		   options{predicates:P2,show_caller:SP2}) :-
	% predicate selection should be the same
	P1 == P2,
	% cannot show caller if the information wasn't collected
	SP1-SP2 \= off-on.
    	

%----------------------------------------------------------------------
% Result presentation
%----------------------------------------------------------------------


list_results(Options, Goal) :-
	stored_keys(counters, Keys),

	wanted_ports(Options, Keys, Ports),

	% remove duplicate pred specs 
	sort(2, <, Keys, Preds0),
	% annotate them with the sum of call+redo+resume counts
	(
	    foreach(key(_,Spec), Preds0),
	    foreach(EntryCount-Spec, Preds1)
	do
	    ( store_get(counters, key(call,Spec), CallCount) ->
		true
	    ;
		CallCount = 0
	    ), 
	    ( store_get(counters, key(redo,Spec), RedoCount) ->
	    	EntryCount0 is CallCount + RedoCount
	    ;
	    	EntryCount0 = CallCount
	    ),
	    ( store_get(counters, key(resume,Spec), ResumeCount) ->
	    	EntryCount is EntryCount0 + ResumeCount
	    ;
	    	EntryCount = EntryCount0
	    )
	),
	% sort by decreasing (call+redo) count
	sort(1, >=, Preds1, Preds),

	open_output_stream(Options, S, Close),
	( Options = options{format:txt} ->
	    print_table_ascii(Options, S, Ports, Preds)
	; Options = options{format:html} ->
	    print_table_html(Options, S, Goal, Ports, Preds)
	;
	    save_results(Options, S)
	),
	call(Close).


% compute an ordered list of ports, omitting ports that do not occur

wanted_ports(options{ports:Ports0}, Keys, Ports) :-
	( Ports0 == all ->
	    ( setof(Port, Spec^member(key(Port,Spec), Keys), AllPorts) ->
		true
	    ;
		AllPorts = [call]
	    ),
	    ( foreach(P,AllPorts), foreach(Pos-P,KeyedPorts) do
		port_printing_order(P, Pos)
	    ),
	    keysort(KeyedPorts, SortedKeyedPorts),
	    ( foreach(_-P,SortedKeyedPorts), foreach(P,Ports) do
		true
	    )
	;
	    Ports = Ports0
	).

    port_printing_order(call,    1) :- !.
    port_printing_order(exit,    2) :- !.
    port_printing_order(fail,    3) :- !.
    port_printing_order('*exit', 4) :- !.
    port_printing_order(redo,    5) :- !.
    port_printing_order(resume,  6) :- !.
    port_printing_order(delay,   7) :- !.
    port_printing_order(leave,   8) :- !.
    port_printing_order(next,    9) :- !.
    port_printing_order(else,   10) :- !.
    port_printing_order(_,      99).


open_output_stream(options{format:Fmt,output:Output0}, S, Close) :-
	( Output0 \= default ->
	    Output = Output0
	; Fmt = html ->
	    Output = dir(profiler)
	;
	    Output = stream(output)
	),
	( Output = stream(S) ->
	    Close = true
	; 
	    ( Output = file(F) ->
		pathname(F, Dir, _Base, Suffix),
		( Suffix = "" ->
		    concat_string([F,.,Fmt], File)
		;
		    File = F
		),
		( exists(File) ->
		    printf(error, "Output file %w exists already!%n", [File]),
		    abort
		;
		    true
		)
	    ; Output = dir(Dir) ->
		once((
		    between(1, 10000, 1, I),
		    concat_string([Dir,/,I,.,Fmt], File),
		    not exists(File)
		))
	    ;
		printf(error, "Illegal output option %w!%n", [Output]),
		abort
	    ),
	    ( Dir = "" -> true ; exists(Dir) -> true ; mkdir(Dir) ),
	    printf(log_output, "Port profiler: creating result file %w%n", [File]),
	    open(File, write, S),
	    Close = close(S)
	).


%----------------------------------------------------------------------
% ASCII output
%----------------------------------------------------------------------

print_table_ascii(Options, S, Ports, Preds) :-
	% calculate some good column widths
	Width is Options[width of options]-1,
	( Options = options{show_caller:on} -> NPreds = 2 ; NPreds = 1 ),
	length(Ports, NPorts),
	max_name_width(Preds, MaxPredNameWidth, _MaxModWidth),
	MaxPredWidth is MaxPredNameWidth+4,
	MinPredWidth = 16,
	WidthPred is max(MinPredWidth, min(MaxPredWidth, (Width-NPorts*6)//NPreds)),
	WidthCount is max(6, min(9, (Width-NPreds*WidthPred)//NPorts)),

	% print the table header
	( Options = options{show_caller:on} ->
	    printf(S, "%-*s%-*s", [WidthPred,'PREDICATE',WidthPred,'CALLER'])
	;
	    printf(S, "%-*s", [WidthPred,'PREDICATE'])
	),
	(
	    foreach(Port,Ports),
	    param(S, WidthCount)
	do
	    printf(S, "%*s", [WidthCount,Port])
	),
	nl(S),
	% print the table proper
	WidthName is WidthPred-4,
	(
	    foreach(_-Spec, Preds),
	    param(S, Ports,Options,WidthName,WidthCount)
	do
	    ( Options = options{show_caller:on} ->
		Spec = (_:F/A from _:PF/PA),
		print_column(S, F, WidthName),
		write(S, /),
		print_column(S, A, 2),
		write(S, ' '),
		print_column(S, PF, WidthName),
		write(S, /),
		print_column(S, PA, 2),
		write(S, ' ')
	    ;
		Spec = (_:F/A),
		print_column(S, F, WidthName),
		write(S, /),
		print_column(S, A, 2),
		write(S, ' ')
	    ),
	    (
		foreach(Port,Ports),
		param(S, Spec,WidthCount)
	    do
	    	( store_get(counters, key(Port,Spec), Count) ->
		    printf(S, "%*d", [WidthCount,Count])
		;
		    printf(S, "%*s", [WidthCount,.])
		)
	    ),
	    nl(S)
	).


    % print a column with exactly Width characters
    % for some reason, printf can't do that
    print_column(S, Term, Width) :-
	( string(Term) -> String = Term ; term_string(Term, String) ),
	( substring(String, 0, Width, _, TruncString) ->
	    write(S, TruncString)
	;
	    Fill is Width - string_length(String),
	    printf(S, "%s%*c", [String,Fill,0' ])
	).

    max_name_width(Preds, MaxPredNameWidth, MaxModWidth) :-
	(
	    foreach(_-Spec, Preds),
	    fromto(0,MaxP0,MaxP1,MaxPredNameWidth),
	    fromto(0,MaxM0,MaxM1,MaxModWidth)
	do
	    ( Spec = (M:F/_ from _) -> true ; Spec = (M:F/_) ),
	    MaxP1 is max(MaxP0, atom_length(F)),
	    MaxM1 is max(MaxM0, atom_length(M))
	).


%----------------------------------------------------------------------
% HTML output
%----------------------------------------------------------------------

print_table_html(Options, S, Goal, Ports, Preds) :-
	% print the page header
	printf(S, "<HTML><HEAD><TITLE>Port Profiler Results</TITLE></HEAD><BODY>%n", []),
	printf(S, "<H1>Port Profiler Results</H1>", []),
	get_flag(unix_time, Time), local_time_string(Time, "%c", Date),
	printf(S, "<P>Date: %w</P>", [Date]),
	print_html(S, "<P>Goal: ", Goal, "</P>"),

	% print the table header
	Options = options{border:Border},
	printf(S, "<TABLE BORDER=\"%d\">%n", [Border]),
	printf(S, " <TR>%n", []),
	printf(S, "<TH ALIGN=\"LEFT\">PREDICATE</TH>%n", []),
	( Options = options{show_caller:on} ->
	    printf(S, "<TH ALIGN=\"LEFT\">CALLER</TH>%n", [])
	;
	    true
	),
	(
	    foreach(Port,Ports),
	    param(S)
	do
	    printf(S, "<TH ALIGN=\"RIGHT\">%w</TH>%n", [Port])
	),
	printf(S, " </TR>%n", []),
	% print the table
	(
	    foreach(_-Spec, Preds),
	    param(S,Ports,Options)
	do
	    printf(S, " <TR>%n", []),
	    ( Options = options{show_caller:on} ->
		( Options = options{show_module:on} ->
		    Spec = (Pred from Parent)
		;
		    Spec = (_:Pred from _:Parent)
		),
		print_html(S, "<TD>", Pred, "</TD>"),
		print_html(S, "<TD>", Parent, "</TD>")
	    ;
		( Options = options{show_module:on} ->
		    Spec = Pred
		;
		    Spec = _:Pred
		),
		print_html(S, "<TD>", Pred, "</TD>")
	    ),
	    (
		foreach(Port,Ports),
		param(S,Spec)
	    do
	    	( store_get(counters, key(Port,Spec), Count) ->
		    true
		;
		    Count='.'
		),
		printf(S, "  <TD ALIGN=\"RIGHT\">%w</TD>%n", [Count])
	    ),
	    printf(S, " </TR>%n", [])
	),
	% print the footer
	printf(S, "</TABLE>%n", []),
	printf(S, "</BODY></HTML>%n", []).


print_html(S, StartTag, String, EndTag) :-
	write(S, StartTag),
	text_list(String, List),
	( foreach(C,List), param(S) do
	    put_html(S, C)
	),
	write(S, EndTag).

    text_list(S, L) :- string(S), !, string_list(S, L).
    text_list(A, L) :- atom(A), !, atom_string(A, S), string_list(S, L).
    text_list(A, L) :- term_string(A, S), string_list(S, L).

    put_html(S, 0'<) :- !, write(S, '&lt;').
    put_html(S, 0'>) :- !, write(S, '&gt;').
    put_html(S, 0'&) :- !, write(S, '&amp;').
    put_html(S, C) :- put(S, C).


%----------------------------------------------------------------------
% Raw data output
% [M:F/A from PM:PF/PA, Port, Count].	% show_caller:on
% [M:F/A, Port, Count].			% show_caller:off
%----------------------------------------------------------------------

save_results(_Options, S) :-
	stored_keys(counters, Keys),
	sort(1, =<, Keys, KeysSortedByPorts),
	sort(2, =<, KeysSortedByPorts, KeysSortedByPredsThenPorts),
	( foreach(Key, KeysSortedByPredsThenPorts), param(S) do
	    Key = key(Port,Pred),
	    store_get(counters, Key, Count),
	    writeq(S, [Pred,Port,Count]),
	    writeln(S, .)
	).


%----------------------------------------------------------------------
% User documentation
%----------------------------------------------------------------------

:- comment(summary, "Port Counting Profiler").
:- comment(author, "Joachim Schimpf, IC-Parc").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:48 $").

:- comment(desc, html("<P>
    This is a performance analysis tool based on counting of events during
    program execution. The events that are counted are defined in terms
    of the 'box model' of execution (the same model that the debugger uses).
    In this box model, predicates are entered though call, redo or resume
    ports, and exited through exit, fail or leave ports. In addition, other
    interesting events are indicated by ports as well (next, else, delay).
    </P><P>
    The usage is as follows:
    <OL>
    <LI>Compile your program in debug mode, as you would normally do during
    program development, e.g.
    <PRE>
    	?- compile(queen).
    </PRE>
    <LI>Load the port_profiler library
    <PRE>
    	?- lib(port_profiler).
    </PRE>
    <LI>Run the query which you want to examine, using port_profile/2:
    <PRE>
    	?- port_profile(queen([1,2,3,4],Out), []).
    </PRE>
    This will print the results in a table.
    </OL>
    The default output you get looks like this:
    <PRE>
	PREDICATE       CALLER               call     exit     fail    *exit     redo
	store_set   /3  nodiag      /3        106      106        .        .        .
	-           /3  nodiag      /3         46       46        .        .        .
	=\\=         /2  nodiag      /3         46       45        1        .        .
	qperm       /2  qperm       /2         30       28        .       16       14
	qdelete     /4  qperm       /2         20       18        .       12       10
	nodiag      /3  nodiag      /3         17       14        3        .        .
	nodiag      /3  safe        /1         17        7       10        .        .
	+           /3  nodiag      /3         17       17        .        .        .
	qdelete     /4  qdelete     /4         10        9        .        3        2
	qperm       /2  queen       /2          1        .        .       11       10
	safe        /1  queen       /2         11        1       10        .        .
	safe        /1  safe        /1          7        4        3        .        .
	queen       /2  trace_body  /2          1        .        .        1        .
    </PRE>
    The port counts give information about
    <UL>
    <LI>what are the most frequently called predicates (call ports)
    <LI>whether predicates failed unexpectedly (fail ports)
    <LI>whether predicates exited nondeterministically (*exit ports), i.e.
    	whether they left behind any choice-points for backtracking.
    <LI>whether nondeterministically exited predicates were ever re-entered
	to find alternative solutions (redo ports).
    <LI>whether predicates did internal backtracking (next ports) in order
    	to find the right clause. This may indicate suboptimal indexing.
    <LI>how often predicates were delayed and resumed.
    </UL>
    By default, statistics are collected separately for each predicate-caller
    pair, i.e. multiple lines appear for a predicate when it is called from
    different caller predicates. This feature can be disabled so that predicates
    are not distingushed by their caller. It is also possible to restrict
    data collection to predicates with a spy point only (less time consuming).
    </P><P>
    Other options allow output in different formats, e.g. as an html file,
    with a subset or different order of the ports, or with module information.
    For details, see the description of port_profile/2.
    </P><P>
    Related, but different tools are:
    <UL>
    <LI>The sampling profiler (profile/1,2 from lib(profile)): this works
	even on optimized, non-traceable code and gives timing information.
	It does not give information about the caller predicate.
    <LI>The coverage analyzer (see lib(coverage)): this is also based on
	counting, but has counters for every program point and is probably
	less useful for performance analysis.
    </UL>
</P>")).

:- comment(last_port_profile/1, [
    amode:last_port_profile(++),
    args:["Options":"A list of OptionName:OptionValue structures"],
    see_also:[port_profile/2],
    summary:"Output another port profile for the most recently profiled goal",
    desc:html("\
    This allows to output (again) the results of the previous call to
    port_profile/2, possibly in a different format. The options are
    the same as in port_profile/2. Note that the show_caller option can
    only be given if it was on in the corresponding call to port_profile/2,
    otherwise the corresponding information is not available.
    ")
    ]).

:- comment(port_profile/2, [
    amode:port_profile(+,++),
    args:["Goal":"A callable goal (atom or compound term)",
    	"Options":"A list of OptionName:OptionValue structures"],
    see_also:[library(port_profiler),library(coverage),profile/1,last_port_profile/1],
    summary:"Create a (box model) port profile for the given Goal execution",
    desc:html("\
    	Executes Goal and creates a (box model) port profile for this execution.
	The ports are the ports as defined for the debugger's box model and
	include:
	<DL>
	<DT>call</DT> <DD>predicate invocation</DD>
	<TR>
	<DT>exit</DT> <DD>deterministic predicate success</DD>
	</TR>
	<TR>
	<DT>fail</DT> <DD>predicate failure</DD>
	</TR>
	<TR>
	<DT>*exit</DT> <DD>nondeterministic predicate success</DD>
	</TR>
	<TR>
	<DT>redo</DT> <DD>reentering a predicate on backtracking</DD>
	</TR>
	<TR>
	<DT>next</DT> <DD>going to the next clause of a predicate</DD>
	</TR>
	<TR>
	<DT>else</DT> <DD>going to an alternative within a predicate</DD>
	</TR>
	<TR>
	<DT>leave</DT> <DD>leaving a predicate with exit_block/1</DD>
	</TR>
	<TR>
	<DT>delay</DT> <DD>delaying a predicate</DD>
	</TR>
	<TR>
	<DT>resume</DT> <DD>reentering a predicate on waking</DD>
	</TR>
	</DL>
	<P>
    	The available options are:
	<DL>
	<DT>format (default:txt)</DT> <DD>output format, txt or html</DD>
	    <DL>
	    <DT>txt</DT> <DD>prints an ascii table, taking width-option into account</DD>
	    <DT>html</DT> <DD>prints an html table, taking border-option into account</DD>
	    <DT>raw</DT> <DD>prints the raw results as lines of the form
		<PRE>
		[M:F/A from PM:PF/PA, Port, Count].
		</PRE>
		if the show_caller-option is on, and
		<PRE>
		[M:F/A, Port, Count].
		</PRE>
		if the show_caller-option is off. These are valid Prolog
		terms which can be read back using read/2.
	    </DD>
	    </DL>
	<DT>border (default:0)</DT> <DD>table border width for html output</DD>
	<DT>output (default:default)</DT><DD>
	    where to put the result: possible values are
	    <DL>
	    <DT>file(File)</DT> <DD>where File is a file name</DD>
	    <DT>stream(Stream)</DT> <DD>where Stream is an Eclipse stream identifier</DD>
	    <DT>dir(Dir)</DT> <DD>where Dir is a directory in which files with generated names will be placed</DD>
	    <DT>default</DT> <DD>either dir(profiler) for html format, or stream(output) otherwise</DD>
	    </DL>
	    </DD>
	<DT>ports (default:all)</DT> <DD>the atom 'all' or a list of port names</DD>
	<DT>predicates (default:all)</DT> <DD>the atom 'all' or 'spied_only'.
	    The latter means that only predicates with a spy point have their
	    ports counted.</DD>
	<DT>show_caller (default:on)</DT> <DD>whether to show and distinguish
	    predicates by their calling predicate (on or off). This is the
	    only option that affects data collection as well as presentation.</DD>
	<DT>show_module (default:off)</DT> <DD>whether to show the predicate's
	    definition modules in the output table (on or off)</DD>
	<DT>width (default:80)</DT> <DD>page width for txt output</DD>
	</DL>
	Note: Any choicepoints that are left behind by Goal will be cut, i.e.
	port_profile/2 behaves like once/1.
    ")]).

