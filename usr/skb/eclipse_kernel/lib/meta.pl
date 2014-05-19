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
% Version:	$Id: meta.pl,v 1.3.2.2 2008/12/23 02:46:57 jschimpf Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG KERNEL MODULE
%
% IDENTIFICATION:	meta.pl
%
% AUTHOR:		Micha Meier
%
% CONTENTS:		Basic metaterm handling
%

:- begin_module(sepia_kernel).
:- pragma(system).
:- pragma(nodebug).
:- pragma(noskip).

:- export
	copy_term/2,
	copy_term_vars/3,
	delayed_goals/2,
	suspensions/2,
	delayed_goals_number/2,
	instance/2,
	meta_attribute/2,
	get_var_bounds/3,
	set_var_bounds/3,
	not_unify/2,
	variant/2.

:- export			% export tool bodies and handlers
	instance_handler/3,
	meta_attributes/1,
	proper_instance/2,
	unify_attributes/2,
	test_unify_handler/1.

?- make_array_(meta_index, prolog, local, sepia_kernel),
	setval(meta_index, 0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Generic metaterm stuff, meta transformations, multiple extensions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%		MULTIPLE EXTENSIONS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Declaring a new extension
%

:- tool(meta_attribute/2, meta_attribute_body/3).
:- local_record(pre_unify).
:- local_record(unify).
:- local_record(test_unify).
:- local_record(compare_instances).
:- local_record(copy_term).
:- local_record(print).
:- local_record(get_bounds).
:- local_record(set_bounds).
:- local_record(suspensions).
:- local_record(delayed_goals).
:- local_record(delayed_goals_number).

meta_attributes(Atts) :-
	recorded_list(meta_attribute, Atts).


meta_attribute_body(Name, List, Module) :-
    check_atom(Name),
    meta_name_index(Name, Index),
    ( Name == suspend, Index == 1 ->
	% The suspend handlers are handcoded below to avoid use of the
	% compiler during initial booting
	check_handlers(List, Module),
	record_handlers(Index, Name, List, Module)
    ;
	check_handlers(List, Module),
	record_handlers(Index, Name, List, Module),
	recompile_system_handlers
    ),
    !.
meta_attribute_body(Name, List, Module) :-
    bip_error(meta_attribute(Name, List), Module).


meta_name_index(Name, Index) :-
    recordedchk(meta_attribute, [Name|Index]),
    !.
meta_name_index(Name, Index) :-
    incval(meta_index),
    getval(meta_index, Index),
    getval(meta_arity, Max),
    (Index > Max ->
	incval(meta_arity)
    ;
	true
    ),
    recorda(meta_attribute, [Name|Index]).


check_handlers(L, _) :- var(L), !,
    set_bip_error(4).
check_handlers([], _) :- !.
check_handlers([H:P|List], Module) :- !,
    check_predspec(P),
    ( P = _/Arity, is_meta_event(H, Arity) ->
	( get_flag(P, defined, on)@Module ->
	    get_flag(P, visibility, Vis)@Module,
	    ( Vis == local ->
		(export P)@Module
	    ; Vis == imported ->
		get_flag(P, definition_module, DM)@Module,
		(reexport P from DM)@Module
	    ;
		true
	    )
	;
	    % require handler to be defined already
	    set_bip_error(60)
	)
    ; P == true/0, is_meta_event(H,_) ->
    	true
    ;
	fail	% with bip_error set from is_meta_event/2
    ),
    check_handlers(List, Module).
check_handlers(_, _) :-
   set_bip_error(5).


record_handlers(_, _, [], _).
record_handlers(Index, Name, [H:P|List], Module) :-
    (recordedchk(H, t(Index, _, _, _, _), Ref) ->
	erase(Ref)
    ;
	true
    ),
    ( P == true/0 ->
	true	% remove the handler
    ;
	recordz(H, t(Index, Name, H, P, Module))
    ),
    record_handlers(Index, Name, List, Module).


% remove all calls to handlers in the erased module
erase_module_attribute_handlers(suspend) :- !.
erase_module_attribute_handlers(Module) :-
    findall(H, (
	    meta_event(H, _),
	    recorded(H, t(_, _, _, _, Module), Ref),
	    erase(Ref)
	), Erased),
    ( Erased = [_|_] ->
	recompile_system_handlers
    ;
	true
    ).


recompile_system_handlers :-
    recompile_unify_handler,
    recompile_pre_unify_handler,
    recompile_test_unify_handler,
    recompile_compare_instances_handler,
    recompile_copy_term_handler,
    recompile_delayed_goals_handler,
    recompile_suspensions_handler,
    recompile_delayed_goals_number_handler,
    recompile_get_bounds_handler,
    recompile_set_bounds_handler,
    recompile_print_handler.

/*
 *	The handlers have the format
 *		pre_unify_attributes(AttrVar, Term, Pair) :-
 *		    pre_handler1(AttrVar, Term),
 *		    ....
 *		    do_meta_bind(Pair, Term),
 *		    
 *		unify_attributes(Term, meta(Attr1, ...)) :-
 *		    post_handler1(Term, Attr1),
 *		    ...
 *	If there are no pre_unify handlers, their part is omitted.
 */

%------------------------------
:- mode unify_attributes(?,++).
unify_attributes(Term, Meta) :-
	arg(1, Meta, SuspAttr),
    	suspend:unify_suspend(Term , SuspAttr).

recompile_unify_handler :-
    collect_local_handlers(unify, List),
    local_unify_handlers(List, Meta, Term, SuspAttr, Body),
    compile_term((unify_attributes(Term, Meta) :- arg(1,Meta,SuspAttr),Body)).

local_unify_handlers([], _, _, _, untraced_true).
local_unify_handlers([t(I, _, _, N/A, M)], Meta, Term, SuspAttr, Body) :-
    !,
    ( I = 1 ->
	Attr = SuspAttr, Body = M:Goal
    ;
	Body = (arg(I,Meta,Attr), M:Goal)
    ),
    ( A = 3 ->
	Goal =.. [N, Term, Attr, SuspAttr]
    ;
	Goal =.. [N, Term, Attr]
    ).
local_unify_handlers([t(I, _, _, N/A, M)|List], Meta, Term, SuspAttr, Body) :-
    ( I = 1 ->
	Attr = SuspAttr, Body = (M:Goal, NewBody)
    ;
	Body = (arg(I,Meta,Attr), M:Goal, NewBody)
    ),
    ( A = 3 ->
	Goal =.. [N, Term, Attr, SuspAttr]
    ;
	Goal =.. [N, Term, Attr]
    ),
    local_unify_handlers(List, Meta, Term, SuspAttr, NewBody).

%------------------------------
pre_unify_attributes(_AttrVar, _Term, _Pair).

recompile_pre_unify_handler :-
    collect_local_handlers(pre_unify, PreList),
    (PreList = [] ->
	compile_term([pre_unify_attributes(_,_,_)]),
	set_default_error_handler(11, unify_handler/1),
	set_error_handler(11, unify_handler/1)
    ;
	local_pre_unify_handlers(PreList, AttrVar, Term, Pair, Body),
	compile_term([:- pragma(nodebug),
		pre_unify_attributes(AttrVar, Term, Pair) :- Body]),
	set_default_error_handler(11, pre_unify_handler/1),
	set_error_handler(11, pre_unify_handler/1)
    ).

undo_meta_bindings([], []).
undo_meta_bindings([Pair|List], [p(AttrVar, Term, Pair)|PList]) :-
    Pair = [Term|_],
    undo_meta_bind(Pair, AttrVar),
    undo_meta_bindings(List, PList).

local_pre_unify_handlers([t(_, _, _, N/_, M)], AttrVar, Term, Pair, LastCall) :-
    !,
    Goal =.. [N, AttrVar, Term],
    LastCall = (M:Goal, do_meta_bind(Pair, Term)).
local_pre_unify_handlers([t(_, _, _, N/_, M)|List], AttrVar, Term, Pair, Body) :-
    Goal =.. [N, AttrVar, Term],
    Body = (M:Goal, NewBody),
    local_pre_unify_handlers(List, AttrVar, Term, Pair, NewBody).

%------------------------------
:- mode test_unify_attributes(?, ++).
test_unify_attributes(_Term, _Attr).

recompile_test_unify_handler :-
    getval(meta_arity, I),
    functor(Attr, meta, I),
    collect_local_handlers(test_unify, List),
    local_test_unify_handlers(List, Attr, Term, Body),
    compile_term(test_unify_attributes(Term, Attr) :- Body).

local_test_unify_handlers([], _, _, untraced_true).
local_test_unify_handlers([t(I, _, _, N/_, M)], Attr, Term, M:Goal) :-
    !,
    arg(I, Attr, LA),
    Goal =.. [N, Term, LA].
local_test_unify_handlers([t(I, _, _, N/_, M)|List], Attr, Term, Body) :-
    arg(I, Attr, LA),
    Goal =.. [N, Term, LA],
    Body = (M:Goal, NewBody),
    local_test_unify_handlers(List, Attr, Term, NewBody).

%------------------------------
:- mode compare_instances_attributes(-, ?, ?).
compare_instances_attributes(Res, TermL, TermR) :-
	suspend:compare_instances_suspend(Res0, TermL, TermR),
	Res is x_res(Res0).

recompile_compare_instances_handler :-
    collect_local_handlers(compare_instances, List),
    local_compare_instances_handlers(List, Res, TermL, TermR, Body, _),
    compile_term(compare_instances_attributes(Res, TermL, TermR) :- Body).

local_compare_instances_handlers([t(_, _, _, N/_, M)|List], Res, TermL, TermR,
	Body, ResL) :-
    Goal =.. [N, R, TermL, TermR],
    Body = (M:Goal, NewBody),
    (List = [] ->
	(var(ResL) ->
	    NewBody = (Res is x_res(R))
	;
	    NewBody = (Res is x_res(R) /\ ResL)
	)
    ;
	(var(ResL) ->
	    ResR = x_res(R)
	;
	    ResR = x_res(R) /\ ResL
	),
	local_compare_instances_handlers(List, Res, TermL, TermR, NewBody, ResR)
    ).
local_compare_instances_handlers([], RR, _, _, true, _) :-
    x_res(=, RR).

%------------------------------
:- mode copy_term_attributes(?, ?).
copy_term_attributes(_Meta, _Term).

recompile_copy_term_handler :-
	collect_local_handlers(copy_term, List),
	local_copy_term_handlers(List, Meta, Term, Body),
	compile_term(copy_term_attributes(Meta, Term) :- Body).

    local_copy_term_handlers([t(_, _, _, N/_, M)|List], Meta, Term, Body) :-
	Goal =.. [N, Meta, Term],
	(List = [] ->
	    Body = M:Goal
	;
	    Body = (M:Goal, NewBody),
	    local_copy_term_handlers(List, Meta, Term, NewBody)
	).
    local_copy_term_handlers([], _, _, true).

%------------------------------
% Create a handler that computes the minimum range from all bounds handlers.
% The result is always two floats, although the individual handlers may
% return integers.
% The handlers are only called if the attribute exists!

get_meta_bounds(_Meta, Lower, Upper) ?-
	Lower = -1.0Inf, Upper = 1.0Inf.

recompile_get_bounds_handler :-
	collect_local_handlers(get_bounds, List),
	local_get_bounds_handlers(List, Meta, -1.0Inf, 1.0Inf, Lower, Upper, Body),
	compile_term(get_meta_bounds(Meta, Lower, Upper) ?- Body).

    local_get_bounds_handlers([], _Meta, L0, U0, L, U, (L=L0,U=U0)).
    local_get_bounds_handlers([t(AttrSlot, _, _, N/_, M)|List], Meta, L0, U0, L, U, Body) :-
	add_attribute(Meta, Attr, AttrSlot),
	Goal =.. [N, Meta, L1, U1],
	Goal1 = (nonvar(Attr) -> M:Goal,max(L0,L1,L2),min(U0,U1,U2) ; L2=L0,U2=U0),
	(List = [] ->
	    Body = Goal1,
	    U2=U, L2=L
	;
	    Body = (Goal1, NewBody),
	    local_get_bounds_handlers(List, Meta, L2, U2, L, U, NewBody)
	).

%------------------------------
set_meta_bounds(_Meta, _Lwb, _Upb).

recompile_set_bounds_handler :-
	collect_local_handlers(set_bounds, List),
	local_set_bounds_handlers(List, Meta, Lwb, Upb, Body),
	compile_term(set_meta_bounds(Meta, Lwb, Upb) ?- Body).

    :- mode local_set_bounds_handlers(+,?,?,?,-).
    local_set_bounds_handlers([], _, _, _, true).
    local_set_bounds_handlers([t(AttrSlot, _, _, N/_, M)|List], Meta, Lwb, Upb, Body) :-
	add_attribute(Meta, Attr, AttrSlot),
	Goal =.. [N, Meta, Lwb, Upb],
	Goal1 = (nonvar(Attr) -> M:Goal ; true),
	(List = [] ->
	    Body = Goal1
	;
	    Body = (Goal1, NewBody),
	    local_set_bounds_handlers(List, Meta, Lwb, Upb, NewBody)
	).

%------------------------------
% Obsolete delayed_goals handlers
% (modified to work as well on top of new suspensions-handler)
:- mode delayed_goals_attributes(?, ?, ?).
delayed_goals_attributes(Meta, G, G0) :-
	suspend:suspensions_suspend(Meta, ListOfSuspLists, []),
	concat_live_suspensions(ListOfSuspLists, Susps, []),
	suspensions_to_goals(Susps, G, G0).

recompile_delayed_goals_handler :-
    collect_local_handlers(suspensions, ListSH),	% new
    collect_local_handlers(delayed_goals, ListDGH),	% old
    append(ListSH, ListDGH, List0),
    sort(1 /*index of t*/, <, List0, List), % keep only SH if both are there
    local_delayed_goals_handlers(List, Meta, G, G0, Body),
    compile_term(delayed_goals_attributes(Meta, G, G0) :- Body).

local_delayed_goals_handlers([t(_, _, HandlerType, N/_, M)|List], Meta, G, G0, Body) :-
    ( HandlerType == delayed_goals ->
	HGoal =.. [N, Meta, G, G1], Goal = M:HGoal
    ;
	HGoal =.. [N, Meta, ListOfSuspLists, []],
	Goal = (
	    M:HGoal,
	    concat_live_suspensions(ListOfSuspLists, Susps, []),
	    suspensions_to_goals(Susps, G, G1)
	)
    ),
    (List = [] ->
	Body = Goal,
	G0 = G1
    ;
	Body = (Goal, NewBody),
	local_delayed_goals_handlers(List, Meta, G1, G0, NewBody)
    ).
local_delayed_goals_handlers([], _, G, G, true).

%------------------------------
:- mode suspensions_attributes(?, ?, ?).
suspensions_attributes(Meta, S, S0) :-
	suspend:suspensions_suspend(Meta, S, S0).

recompile_suspensions_handler :-
    collect_local_handlers(suspensions, List),
    local_suspensions_handlers(List, Meta, S, S0, Body),
    compile_term(suspensions_attributes(Meta, S, S0) :- Body).

local_suspensions_handlers([t(_, _, _, N/_, M)|List], Meta, S, S0, Body) :-
    Goal =.. [N, Meta, S, S1],
    (List = [] ->
	Body = M:Goal,
	S0 = S1
    ;
	Body = (M:Goal, NewBody),
	local_suspensions_handlers(List, Meta, S1, S0, NewBody)
    ).
local_suspensions_handlers([], _, S, S, true).

%------------------------------
:- mode delayed_goals_number_attributes(?, ?).
delayed_goals_number_attributes(Meta, NG) :-
	suspend:delayed_goals_number_suspend(Meta, NG).

recompile_delayed_goals_number_handler :-
    collect_local_handlers(delayed_goals_number, List),
    local_delayed_goals_number_handlers(List, Meta, NG, Body, 0),
    compile_term(delayed_goals_number_attributes(Meta, NG) :- Body).

local_delayed_goals_number_handlers([t(_, _, _, N/_, M)|List], Meta, NG, Body, NG0) :-
    Goal =.. [N, Meta, NG1],
    (List = [] ->
	( NG0 == 0 ->				% only one
	    Body = M:Goal,
	    NG = NG1
	;
	    Body = (M:Goal, NG is NG0 + NG1)
	)
    ;
	Body = (M:Goal, NewBody),
	( NG0 == 0 ->				% first
	    NG2 = NG1
	;
	    NG2 = NG0 + NG1
	),
	local_delayed_goals_number_handlers(List, Meta, NG, NewBody, NG2)
    ).
local_delayed_goals_number_handlers([], _, 0, true, _).

%------------------------------
print_attribute(_, _) :- fail.

recompile_print_handler :-
    collect_local_handlers(print, List),
    local_print_handlers(List, Var, OL, Body),
    (Body == (_ = []) ->
	compile_term(print_attribute(_, _) :- fail)
    ;
	compile_term(print_attribute(Var, OL) :- Body)
    ).

local_print_handlers([], _, L, L = []).
local_print_handlers([t(_, Name, _, N/_, M)|List], Var, L,
		((M:Goal -> L = [Name:Out|L1]; L = L1), Body1)) :-
    Goal =.. [N, Var, Out],
    local_print_handlers(List, Var, L1, Body1).

%------------------------------
collect_local_handlers(Key, List) :-
    getval(meta_index, I),
    collect_local_handlers(I, Key, List).

collect_local_handlers(I, Key, List) :-
    I > 0,
    !,
    I1 is I - 1,
    (Cont = t(I, _, _, P, _),
    recorded(Key, Cont),
    P \= true/0 ->
	List = [Cont|NewList],
	collect_local_handlers(I1, Key, NewList)
    ;
	collect_local_handlers(I1, Key, List)
    ).
collect_local_handlers(_, _, []).


is_meta_event(Var, _) :-
    var(Var),
    !,
    set_bip_error(4).
is_meta_event(Var, _) :-
    not atom(Var),
    !,
    set_bip_error(5).
is_meta_event(H, A) :-
    meta_event(H, A), !.
is_meta_event(_, _) :-
    set_bip_error(6).

meta_event(pre_unify, 2).
meta_event(unify, 2).
meta_event(unify, 3).
meta_event(test_unify, 2).
meta_event(compare_instances, 3).
meta_event(copy_term, 2).
meta_event(delayed_goals, 3).
meta_event(suspensions, 3).
meta_event(delayed_goals_number, 2).
meta_event(get_bounds, 3).
meta_event(set_bounds, 3).
meta_event(print, 2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Global handlers
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
%%%% unification %%%%
%

:- pragma(debug).
unify_handler([]) :- -?->
    wake.	% we want to trace this call (only)
unify_handler([[Term|Attr]|List]) :-
    -?->
    unify_attributes(Term, Attr),
    unify_handler(List).
:- pragma(nodebug).

pre_unify_handler(List) :-
    undo_meta_bindings(List, NewList),
    pre_unify_pairs(NewList),
    unify_handler(List).

pre_unify_pairs([]).
pre_unify_pairs([p(Var, Term, Pair)|L]) :-
    pre_unify_attributes(Var, Term, Pair),
    pre_unify_pairs(L).



%
%%%% not_unify/2 %%%%
%
not_unify(X, Y) :-
    unify(X, Y, List),		% like =/2 with an explicit list
    test_unify_handler(List),
    !,
    fail.
not_unify(_, _).

test_unify_handler([]).
test_unify_handler([[Term|Attr]|List]) :-
    test_unify_attributes(Term, Attr),
    test_unify_handler(List).

%
%%%% proper_instance/2 %%%%
%
proper_instance(Instance, Term) :-
    compare_instances(Res, Instance, Term, List),
    x_res(Res, R),
    not not instance_handler(R, List, 2).

%
%%%% variant/2 %%%%
%
variant(Term1, Term2) :-
    compare_instances(=, Term1, Term2, List),
    not not instance_handler(3, List, 3).

%
%%%% instance/2 %%%%
%
instance(Term1, Term2) :-
    compare_instances(Res, Term1, Term2, List),
    x_res(Res, R),
    R >= 2,
    not not ((instance_handler(R, List, R1),
	R1 >= 2)).

instance_handler(R, [], R).
instance_handler(Res, [[TermL|TermR]|List], ResL) :-
    % one or both of TermL, TermR are attributed variables!
    compare_instances_attributes(Res1, TermL, TermR),
    Res2 is Res1 /\ Res,
    instance_handler(Res2, List, ResL).


%
%%%% copy_term/2 %%%%
%
copy_term(Term, Copy) :-
    copy_term(Term, Copy, List),
    copy_term_handler(List).

copy_term_vars(Vars, Term, Copy) :-
    copy_term_vars(Vars, Term, Copy, List),
    copy_term_handler(List).

copy_term_handler([]).
copy_term_handler([[Meta|Term]|List]) :-
    copy_term_attributes(Meta, Term),
    copy_term_handler(List).


%
%%%% retrieve current numeric range %%%%
%
get_var_bounds(X, L, U) :-
	free(X), !,
	L = -1.0Inf, U = 1.0Inf.
get_var_bounds(X, L, U) :-
	meta(X), !,
	get_meta_bounds(X, L, U).
get_var_bounds(X, L, U) :-
	breal(X), !,
	breal_bounds(X, L, U).
get_var_bounds(X, L, U) :-
	number(X), !,
	L is float(X), U = L.
get_var_bounds(X, L, U) :-
	error(5, get_var_bounds(X, L, U)).

set_var_bounds(X, _, _) :- free(X), !.
set_var_bounds(X, L, U) :- meta(X), !,
	set_meta_bounds(X, L, U).
set_var_bounds(X, L, U) :- number(X), !,
	L =< X, X =< U.
set_var_bounds(X, L, U) :-
	error(5, set_var_bounds(X, L, U)).


%
%%%% delayed_goals/2 %%%%
%
delayed_goals(Meta, Goals) :-
	meta(Meta),
	!,
	delayed_goals_attributes(Meta, Goals, []).
delayed_goals(_free_or_instantiated, []).


%
%%%% suspensions/2 %%%%
%
suspensions(Meta, Susps) :-
	meta(Meta),
	!,
	suspensions_attributes(Meta, ListOfSuspLists, []),
	( Susps == [] ->
	    % if just testing, we can fail early
	    concat_live_suspensions(ListOfSuspLists, [], [])
	;
	    concat_live_suspensions(ListOfSuspLists, Susps0, []),
	    sort(0, <, Susps0, Susps)	% remove duplicates
	).
suspensions(_free_or_instantiated, []).

    concat_live_suspensions([], Susps, Susps).
    concat_live_suspensions([SuspList|SuspLists], Susps, Susps0) :-
	filter_live_suspensions(SuspList, Susps, Susps1),
	concat_live_suspensions(SuspLists, Susps1, Susps0).

    filter_live_suspensions(Empty, Ls, Ls) :- var(Empty), !.
    filter_live_suspensions([], Ls, Ls).
    filter_live_suspensions([S|Ss], SLs, Ls) :-
	( is_suspension(S) -> SLs = [S|Ls0] ; SLs = Ls0 ),
	filter_live_suspensions(Ss, Ls0, Ls).



%
%%%% delayed_goals_number/2 %%%%
%
delayed_goals_number(Meta, N) :-
	meta(Meta),
	!,
	delayed_goals_number_attributes(Meta, N).
delayed_goals_number(X, N) :-
	var(X),
	!,
	N = 0.
delayed_goals_number(_, 1000000).


%
%%%% print %%%%
%
print_attributes(Attr, {Out}) :-
    print_attribute(Attr, L),
    list_to_attr(L, OT),
    (OT = _:Out ->
	true
    ;
	L = [_|_],
	Out = OT
    ).

list_to_attr([A], A) :- !.
list_to_attr([A|L], (A,B)) :-
    list_to_attr(L, B).

:- mode x_res(++, -).
x_res(>, 1).
x_res(<, 2).
x_res(=, 3).

?- set_default_error_handler(11, unify_handler/1),
   set_error_handler(11, unify_handler/1).
   
:- skipped unify_attributes/2.
:- set_flag(unify_handler/1, invisible, on).

:- unskipped
	test_unify_attributes/2,
	compare_instances_attributes/3,
	copy_term_attributes/2,
	print_attributes/2,
	delayed_goals_attributes/3,
	delayed_goals_number_attributes/2,
	delayed_goals/2,
	delayed_goals_number/2,
	unify_handler/1,
	copy_term_handler/1,
	instance_handler/3,
	test_unify_handler/1.

:- untraceable
	unify_attributes/2,
	pre_unify_attributes/3,
	test_unify_attributes/2,
	compare_instances_attributes/3,
	copy_term_attributes/2,
	print_attribute/2,
	print_attributes/2,
	delayed_goals_attributes/3,
	delayed_goals_number_attributes/2,
	unify_handler/1,
	pre_unify_handler/1,
	undo_meta_bindings/2,
	pre_unify_pairs/1,
	copy_term_handler/1,
	instance_handler/3,
	test_unify_handler/1.

