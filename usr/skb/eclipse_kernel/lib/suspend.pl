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
% Version:	$Id: suspend.pl,v 1.2 2008/08/21 18:08:28 jschimpf Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG MODULE
%
% IDENTIFICATION:	suspend.pl
%
% AUTHOR:		Micha Meier
%			Joachim Schimpf
%
% CONTENTS:		Coroutining with three suspension lists:
%
%			- one for goals to be woken on instantiation only
%			- one for binding with any metaterm
%			- one for general "constrained-ness"
%
%			The code for metaterm handlers should be used
%			as template for writing user-defined extensions;
%			simply replace 'suspend' in the predicate names by
%			the name of the attribute and re-use the code.
%

:- module(suspend, [], sepia_kernel).

% This structure is already defined and exported in the kernel
%:- export struct(suspend(inst, constrained, bound)).

:- comment(summary,
    "Lazy-checking versions of arithmetic primitives, and the suspend-attribute").
:- comment(date, "$Date: 2008/08/21 18:08:28 $").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(author, "Micha Meier, ECRC, Joachim Schimpf, ECRC and IC-Parc").
:- comment(desc, html("\
    This library provides the following:
<UL>
    <LI>the <B>suspend</B> pseudo-solver for general arithmetic
    <LI>the <B>suspend</B> attribute
</UL>
    The suspend pseudo-solver for arithmetic comparisons simply consists
    of suspending (lazy-checking) versions of all arithmetic comparisons
    (&gt;/2, #&gt;/2, etc).  These all suspend until all their variables
    have been instantiated, then they wake up and test the condition,
    succeeding or failing as appropriate. Together with a search routine,
    this provides the means to implement simple test-and-generate algorithms.
<P>
    The suspend-attribute is a basis for the implementation of similar
    user-defined data-driven computations that react to variable instantiation,
    variable binding, or general variable-constraining events.  The suspend-
    attribute defines the following three suspension lists (and thus waking
    conditions) for a variable:
<DL>
    <DT><STRONG>inst</STRONG><DD>
    	woken when variable is instantiated.
    <DT><STRONG>bound</STRONG><DD>
    	woken when variable is bound, even to an other variable.
    <DT><STRONG>constrained</STRONG><DD>
    	woken when variable is (further) constrained.  This is
	triggered by the notify_constrained built-in.
</DL>
    These three lists can be used as waking conditions in the suspend/3,4
    built-in. Variables using the suspend-attribute do not have to be
    declared specially, the attribute is implicitly created when needed.
    ")).

:- import
	(:@)/3,
	eval/3,
	get_flag_body/4,
	insert_suspension/4,
	setarg/3,
	suspensions_to_goals/3
    from sepia_kernel.

%----------------------------------------------------------------
% attribute declaration
%----------------------------------------------------------------

% CAUTION: when changing handlers, update meta.pl accordingly!!!
:- meta_attribute(suspend, [
	unify:			unify_suspend/2,
	compare_instances:	compare_instances_suspend/3,
	suspensions:		suspensions_suspend/3,
	delayed_goals_number:	delayed_goals_number_suspend/2
	]).

%----------------------------------------------------------------
% unify handler
%----------------------------------------------------------------

:- pragma(system).
:- pragma(nodebug).

% unify_suspend(+Term, Attribute)
unify_suspend(_, Attr) :-
    /*** ANY + VAR ***/
    var(Attr).			% Ignore if no attribute for this extension
unify_suspend(Term, Attr) :-
    compound(Attr),
    unify_term_suspend(Term, Attr).


% We wake every time a variable is touched.
:- mode unify_term_suspend(?, +).
unify_term_suspend(Term, AttrX) :-
    atomic(Term),		% The metaterm was instantiated, wake all
    /*** NONVAR + META ***/
    AttrX = suspend{bound:B, inst:I, constrained:C},
    % schedule_woken/1 is faster than schedule_suspensions/2 but can
    % be used only if the lists are no longer needed after waking
    schedule_woken(C),
    schedule_woken(B),
    schedule_woken(I).
unify_term_suspend(Term, AttrX) :-
    compound(Term),		% The metaterm was instantiated, wake all
    /*** NONVAR + META ***/
    schedule_suspensions(constrained of suspend, AttrX),
    AttrX = suspend{bound:B, inst:I, constrained:Cnew},
    schedule_woken(B),
    schedule_woken(I),
    % the constrained list may still contain demons after scheduling,
    % forward them to the variables in the bound term
    ( Cnew = [] ->
    	true
    ;
	term_variables(Term, Vars),
	( Vars = [] -> true ; forward_constrained_list(Cnew, Vars) )
    ).
unify_term_suspend(Y{AttrY}, AttrX) :-
    -?->
    unify_suspend_suspend(Y, AttrX, AttrY).

unify_suspend_suspend(_, AttrX, AttrY) :-
    var(AttrY),			% No attribute for this extension
    /*** VAR + META ***/
    AttrX = AttrY.		% Keep both lists, do not wake
unify_suspend_suspend(_, AttrX, AttrY) :-
    nonvar(AttrY),
    /*** META + META ***/
    AttrX = suspend{inst:XI-YI,  bound:BX, constrained:CX},
    AttrY = suspend{inst:YI-YI0, bound:BY, constrained:CY},
    setarg(inst of suspend, AttrY, XI-YI0),

    % merge the bound-lists and wake those suspensions that were in both lists
    merge_intersect(BX, BY, BXY, BoundSuspsToWake),
    schedule_woken(BoundSuspsToWake),
    setarg(bound of suspend, AttrY, BXY),

    % treat the constrained-lists exactly like the bounds-lists
    merge_intersect(CX, CY, CXY, ConstrSuspsToWake),
    schedule_woken(ConstrSuspsToWake),
    setarg(constrained of suspend, AttrY, CXY).


forward_constrained_list([], _Vars).
forward_constrained_list([Susp|Susps], Vars) :-
    ( is_suspension(Susp) ->
	insert_suspension(Vars, Susp, constrained of suspend)
    ;
	true	% dead suspension
    ),
    forward_constrained_list(Susps, Vars).


:- mode merge_intersect(+,+,-,-).
merge_intersect([], L2, L2, []) :- !.
merge_intersect(L1, [], L1, []) :- !.
merge_intersect(L1, L2, Merged, Common) :-
	% sort and remove duplicates (descending, because the lists are
	% likely to be already in descending order, due to new suspensions
	% always being inserted in front)
	sort(0, >, L1, S1),
	sort(0, >, L2, S2),
	% merge and extract common elements
	merge(0, >=, S1, S2, S12),
	S12 = [S|Ss],
	split_unique_common(S, Ss, Merged, Common).

    split_unique_common(S1, [], [S1], []).
    split_unique_common(S1, [S2|Ss], Merged, Common) :-
    	( S1 == S2 ->
	    Common = [S1|Common0],
	    split_unique_common(S2, Ss, Merged, Common0)
	;
	    Merged = [S1|Merged0],
	    split_unique_common(S2, Ss, Merged0, Common)
	).


%----------------------------------------------------------------
% compare_instances handler
%----------------------------------------------------------------

compare_instances_suspend(Res, T1, T2) :-
    compare_instances(Res, T1, T2).

%----------------------------------------------------------------
% suspensions handler
%----------------------------------------------------------------

suspensions_suspend(_{Attr}, Goals, G0) :-
    -?->
    susp_suspend(Attr, Goals, G0).

susp_suspend(AttrX, Susps, Susps) :- var(AttrX), !.
susp_suspend(suspend{inst:I-_, bound:B, constrained:C}, [I,B,C|Ss], Ss).


%----------------------------------------------------------------
% delayed goals number handler
%----------------------------------------------------------------

delayed_goals_number_suspend(_{AttrX}, N) :-
    -?->
    dgn_suspend(AttrX, N).

dgn_suspend(AttrX, 0) :-
    /*** VAR ***/
    var(AttrX),
    !.
dgn_suspend(suspend{inst:I-_, bound:B, constrained:C}, N) :-
    /*** META ***/
    count_active_suspensions(I, 0, N0),
    count_active_suspensions(B, N0, N1),
    count_active_suspensions(C, N1, N).

count_active_suspensions(Susps, N0, N) :-
    var(Susps),
    !,
    N = N0.
count_active_suspensions([], N, N).
count_active_suspensions([Susp|Susps], N0, N) :-
    ( is_suspension(Susp) ->
	    N1 is N0 + 1
    ;
	    N1 = N0
    ),
    count_active_suspensions(Susps, N1, N).


:- untraceable
	unify_suspend/2,
	compare_instances_suspend/3,
	suspensions_suspend/3,
	delayed_goals_number_suspend/2.


%----------------------------------------------------------------
% Sepia's delay clauses (backward compatibility)
% Source transformation for if/2
%----------------------------------------------------------------

:- export tr_if_suspend/3.
:- export macro((if)/2, tr_if_suspend/3, [clause]).

:- mode tr_if_suspend(+, -, +).
tr_if_suspend(Clause, (Head :- -?-> NewBody, Susp), _M) :-
	Clause = no_macro_expansion(delay Head if Body),
	translate_goal(Body, NewBody, VarList, [], BVarList, []),
	check_varlists(VarList, BVarList, Clause),
	strip_varlist(VarList, VarList1),
	strip_varlist(BVarList, BVarList1),
	handle_suspension(Head, VarList1, BVarList1, Susp).

:- mode translate_goal(?, -, ?, -, ?, -).
translate_goal(Goal,	Goal,	Vars,	Vars, BV, BV) :- var(Goal), !.
translate_goal((Goal, Goals), (NewGoal, NewGoals), Out, In, BOut, BIn) :- !,
	translate_goal(Goal, NewGoal, Mid, In, BMid, BIn),
	translate_goal(Goals, NewGoals, Out, Mid, BOut, BMid).
translate_goal((Goal -> Goals), (NewGoal -> NewGoals), Out, In, BOut, BIn) :- !,
	translate_goal(Goal, NewGoal, Mid, In, BMid, BIn),
	translate_goal(Goals, NewGoals, Out, Mid, BOut, BMid).
translate_goal((Goal ; Goals), (NewGoal, Out=VarsL, BOut = BVarsL ;
				NewGoals, Out=VarsR, BOut = BVarsR),
		[Out], In, [BOut], BIn) :- !,
	translate_goal(Goal, NewGoal, VarsL0, In, BVarsL0, BIn),
	check_varlists(VarsL0, BVarsL0, Goal),
	strip_varlist(VarsL0, VarsL),
	strip_varlist(BVarsL0, BVarsL),
	translate_goal(Goals, NewGoals, VarsR0, In, BVarsR0, BIn),
	check_varlists(VarsR0, BVarsR0, Goal),
	strip_varlist(VarsR0, VarsR),
	strip_varlist(BVarsR0, BVarsR).
translate_goal(var(X),		var(X),			[X|Out],    Out, V,V) :-
	!.
translate_goal(X \== Y,
		(sepia_kernel: \==(X, Y, Vars),
		    ( Vars = [Var] -> Out = [Var|In], BOut = BIn
		    ; BOut = [Vars|BIn], Out = In
		    )
		),
		[Out], In, [BOut], BIn) :- !.
translate_goal(nonground(X),	nonground(X, Var),	[Var|Out],  Out, V,V) :-
	!.
translate_goal(nonground(N, X),	nonground(N, X, Vars),	[Vars|Out], Out, V,V) :-
	!.
translate_goal(Goal,	Goal, 	Vars,	Vars, BVars, BVars).

handle_suspension(Head, List, BL, (!, make_suspension(Head, 0, S), Body)) :-
	handle_i_susp(S, List, BL, Body).

handle_i_susp(Susp, [], BList, Body) :-
	-?->
	!,
	bound_suspension(Susp, BList, Body).
handle_i_susp(Susp, List, [], Body) :-
	-?->
	!,
	inst_suspension(Susp, List, Body).
handle_i_susp(Susp, List, BList, (BI, BB)) :-
	inst_suspension(Susp, List, BI),
	bound_suspension(Susp, BList, BB).

inst_suspension(S, List,
	insert_suspension(List, S, inst of suspend, suspend)).

bound_suspension(S, List,
	insert_suspension(List, S, bound of suspend, suspend)).


:- mode check_varlists(+,+,?).
check_varlists([], [], Culprit) :- !,
	error(272, Culprit).
check_varlists(_, _, _).

:- mode strip_varlist(+,-).
strip_varlist([], []) :- !.
strip_varlist([X], X) :- !.	% avoid list if single element
strip_varlist(L, L).


%----------------------------------------------------------------------
% Delaying arithmetic
%----------------------------------------------------------------------

:- export
	op(750, fx, [neg]),
	op(760, yfx, [and]),
	op(770, yfx, [or]),
	op(780, yfx, [=>]),
	op(700, xfx, [#::,$::,$=,$\=,$>=,$=<,$>,$<]).

:- export
	:: /2,
	$:: /2,
	#:: /2,
	integers/1,
	reals/1,
	=:=  /2,
	=\= /2,
	>= /2,
	=< /2,
	> /2,
	< /2,
	$=  /2,
	$\= /2,
	$>= /2,
	$=< /2,
	$>  /2,
	$<  /2,
	#=  /2,
	#\= /2,
	#>= /2,
	#=< /2,
	#>  /2,
	#<  /2,
	and/2,
	or/2,
	=> /2,
	(neg)/1.


    % integers(X) - X is an integer or a list of integers
integers(Tail) :- var(Tail), !,
	suspend(integers(Tail), 2, Tail->inst).
integers([]) :- !.
integers([X|Xs]) :- !,
	one_integer(X),
	integers(Xs).
integers(X) :- integer(X).

    one_integer(X) :- var(X),
	suspend(integer(X), 2, X->inst).
    one_integer(X) :- integer(X).


    % reals(X) - X is a number or a list of numbers
reals(Tail) :- var(Tail), !,
	suspend(reals(Tail), 2, Tail->inst).
reals([]) :- !.
reals([X|Xs]) :- !,
	one_number(X),
	reals(Xs).
reals(X) :- number(X).

    one_number(X) :- var(X),
	suspend(number(X), 2, X->inst).
    one_number(X) :- number(X).


:- tool((::)/2, '::_body'/3).
:- tool(($::)/2, '$::_body'/3).
:- tool((#::)/2, '#::_body'/3).
:- tool((#=)/2, '#=_body'/3).
:- tool((#\=)/2, '#\\=_body'/3).
:- tool((#=<)/2, '#=<_body'/3).
:- tool((#>=)/2, '#>=_body'/3).
:- tool((#<)/2, '#<_body'/3).
:- tool((#>)/2, '#>_body'/3).
:- tool(($=)/2, '$=_body'/3).
:- tool(($\=)/2, '$\\=_body'/3).
:- tool(($=<)/2, '$=<_body'/3).
:- tool(($>=)/2, '$>=_body'/3).
:- tool(($<)/2, '$<_body'/3).
:- tool(($>)/2, '$>_body'/3).
:- tool((=:=)/2, '=:=_body'/3).
:- tool((=\=)/2, '=\\=_body'/3).
:- tool((=<)/2, '=<_body'/3).
:- tool((>=)/2, '>=_body'/3).
:- tool((<)/2, '<_body'/3).
:- tool((>)/2, '>_body'/3).
:- tool((and)/2, 'and_body'/3).
:- tool((or)/2, 'or_body'/3).
:- tool((=>)/2, '=>_body'/3).
:- tool((neg)/1, 'neg_body'/2).

'::_body'(X, Range, M)	:- delay_until_ground(X :: Range, M).
'$::_body'(X, Range, M)	:- delay_until_ground(X $:: Range, M).
'#::_body'(X, Range, M)	:- delay_until_ground(X #:: Range, M).
'#=_body'(X, Y, M)	:- delay_until_ground(X #= Y, M).
'#\\=_body'(X, Y, M)	:- delay_until_ground(X #\= Y, M).
'#=<_body'(X, Y, M)	:- delay_until_ground(X #=< Y, M).
'#>=_body'(X, Y, M)	:- delay_until_ground(X #>= Y, M).
'#<_body'(X, Y, M)	:- delay_until_ground(X #< Y, M).
'#>_body'(X, Y, M)	:- delay_until_ground(X #> Y, M).
'$=_body'(X, Y, M)	:- delay_until_ground(X =:= Y, M).
'$\\=_body'(X, Y, M)	:- delay_until_ground(X =\= Y, M).
'$=<_body'(X, Y, M)	:- delay_until_ground(X =< Y, M).
'$>=_body'(X, Y, M)	:- delay_until_ground(X >= Y, M).
'$<_body'(X, Y, M)	:- delay_until_ground(X < Y, M).
'$>_body'(X, Y, M)	:- delay_until_ground(X > Y, M).
'=:=_body'(X, Y, M)	:- delay_until_ground(X =:= Y, M).
'=\\=_body'(X, Y, M)	:- delay_until_ground(X =\= Y, M).
'=<_body'(X, Y, M)	:- delay_until_ground(X =< Y, M).
'>=_body'(X, Y, M)	:- delay_until_ground(X >= Y, M).
'<_body'(X, Y, M)	:- delay_until_ground(X < Y, M).
'>_body'(X, Y, M)	:- delay_until_ground(X > Y, M).
'and_body'(X, Y, M)	:- delay_until_ground(X and Y, M).
'or_body'(X, Y, M)	:- delay_until_ground(X or Y, M).
'=>_body'(X, Y, M)	:- delay_until_ground(X => Y, M).
'neg_body'(X, M)	:- delay_until_ground(neg X, M).


    delay_until_ground(Goal, M) :-
	( nonground(Goal, Var) ->
	    suspend(delay_until_ground(Goal, Susp, M), 2, Var->inst, Susp)
	;
	    ground_check(Goal, M)
	).

    :- demon delay_until_ground/3.
    delay_until_ground(Goal, Susp, M) :-
	( nonground(Goal, Var) ->
	    insert_suspension(Var, Susp, inst of suspend)
	;
	    kill_suspension(Susp),
	    ground_check(Goal, M)
	).


% Note on the #-constraints:  Implementation via
%	X #? ...  :-  XI is X, integer(XI), ...
% is not quite right, but almost.  # is supposed to impose integrality on
% all vars in X (except those within user-defined functions?), but we only
% check whether the result is integral. This only breaks down for fix/1,
% sgn/1, numerator/1 and denominator/1 (which seems acceptable), because
% all other functions propagate non-integrality through.

ground_check(Xs :: Range, M) :- 
	eval_range(Xs, Range, From, To, Type, M),
	check_range(Xs, From, To, Type).
ground_check(Xs $:: Range, M) :- 
	eval_range(Xs, Range, From, To, _Type, M),
	check_range(Xs, From, To, number).
ground_check(Xs #:: Range, M) :- 
	eval_range(Xs, Range, From, To, Type, M),
	( Type == integer ->
	    check_range(Xs, From, To, Type)
	;
	    error(5, Xs :: Range, M)
	).
ground_check(integers(Xs), _M)	:- check_integers(Xs).
ground_check(reals(Xs), _M)	:- check_numbers(Xs).
ground_check(X =:= Y, M)	:- :@(sepia_kernel, (X =:= Y), M).
ground_check(X =\= Y, M)	:- :@(sepia_kernel, (X =\= Y), M).
ground_check(X >= Y, M)		:- :@(sepia_kernel, (X >= Y), M).
ground_check(X =< Y, M)		:- :@(sepia_kernel, (X =< Y), M).
ground_check(X > Y, M)		:- :@(sepia_kernel, (X > Y), M).
ground_check(X < Y, M)		:- :@(sepia_kernel, (X < Y), M).
ground_check(X $= Y, M)		:- :@(sepia_kernel, (X =:= Y), M).
ground_check(X $\= Y, M)	:- :@(sepia_kernel, (X =\= Y), M).
ground_check(X $>= Y, M)	:- :@(sepia_kernel, (X >= Y), M).
ground_check(X $=< Y, M)	:- :@(sepia_kernel, (X =< Y), M).
ground_check(X $> Y, M)		:- :@(sepia_kernel, (X > Y), M).
ground_check(X $< Y, M)		:- :@(sepia_kernel, (X < Y), M).
ground_check(X #= Y, M)		:- eval(X,XI,M), integer(XI), eval(Y,YI,M), integer(YI), :@(sepia_kernel, (XI =:= YI), M).
ground_check(X #\= Y, M)	:- eval(X,XI,M), integer(XI), eval(Y,YI,M), integer(YI), :@(sepia_kernel, (XI =\= YI), M).
ground_check(X #>= Y, M)	:- eval(X,XI,M), integer(XI), eval(Y,YI,M), integer(YI), :@(sepia_kernel, (XI >= YI), M).
ground_check(X #=< Y, M)	:- eval(X,XI,M), integer(XI), eval(Y,YI,M), integer(YI), :@(sepia_kernel, (XI =< YI), M).
ground_check(X #> Y, M)		:- eval(X,XI,M), integer(XI), eval(Y,YI,M), integer(YI), :@(sepia_kernel, (XI > YI), M).
ground_check(X #< Y, M)		:- eval(X,XI,M), integer(XI), eval(Y,YI,M), integer(YI), :@(sepia_kernel, (XI < YI), M).
ground_check(X and Y, M)	:- eval(X,XI,M), bool(XI), eval(Y,YI,M), bool(YI), sepia_kernel:(XI+YI > 1).
ground_check(X or Y, M)		:- eval(X,XI,M), bool(XI), eval(Y,YI,M), bool(YI), sepia_kernel:(XI+YI > 0).
ground_check(X => Y, M)		:- eval(X,XI,M), bool(XI), eval(Y,YI,M), bool(YI), sepia_kernel:(YI-XI >= 0).
ground_check(neg X, M)		:- eval(X,0,M).


:- mode eval_range(?,++,-,-,-,+).
eval_range(_X, From0..To0, From, To, Type, M) :- !,
	eval(From0, From, M),
	eval(To0, To, M),
	( integer(From), integer(To) ->
	    Type = integer
	;
	    Type = number
	).
eval_range(X, Range, _From, _To, _Type, M) :-
	error(5, X::Range, M).


:- mode check_range(++,+,+,+).
check_range(X, From, To, Type) :- number(X), !,
	check_range_number(X, From, To, Type).
check_range([], _From, _To, _Type) :- !.
check_range(List, From, To, Type) :- List = [_|_], !,
	check_range_list(List, From, To, Type).
check_range(subscript(Array,Index), From, To, Type) :- !,
	subscript(Array, Index, Elems),
	check_range(Elems, From, To, Type).
check_range(X, From, To, _Type) :- !,
	error(5, X::From..To).

    check_range_list([], _From, _To, _Type) :- !.
    check_range_list([X|Xs], From, To, Type) :- !,
	check_range_number(X, From, To, Type),
	check_range_list(Xs, From, To, Type).
    check_range_list(X, From, To, _Type) :-
    	error(5, X::From..To).

    check_range_number(X, From, To, integer) :-
	sepia_kernel:(From =< X),
	sepia_kernel:(X =< To),
    	integer(X).
    check_range_number(X, From, To, number) :-
	sepia_kernel:(From =< X),
	sepia_kernel:(X =< To).


:- mode check_integers(++).
check_integers([]) :- !.
check_integers([X|Xs]) :- !,
	integer(X),
	check_integers(Xs).
check_integers(X) :-
	error(5, integers(X)).

:- mode check_numbers(++).
check_numbers([]) :- !.
check_numbers([X|Xs]) :- !,
	number(X),
	check_numbers(Xs).
check_numbers(X) :-
	error(5, reals(X)).

bool(0).
bool(1).


%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% Reified versions
%
% Problem: what does #=(X,Y,B) mean?
%
% - The proper reification of #=(X,Y) would be
%
%    #=(X,Y,B) :- integers(X,BX), integers(Y,BY), $=(X,Y,BZ), $=(BX+BY+BZ,3,B).
%   
%   but this is not really what a user expects.
%
% - instead it means
%
%    #=(X,Y,B) :- integers(X), integers(Y), $=(X,Y,B).
%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

:- export
	::  /3,
	$::  /3,
	#::  /3,
	=:=  /3,
	=\= /3,
	>  /3,
	<  /3,
	>= /3,
	=< /3,
	$=  /3,
	$\= /3,
	$>  /3,
	$<  /3,
	$>= /3,
	$=< /3,
	#=  /3,
	#\= /3,
	#>= /3,
	#=< /3,
	#<  /3,
	#>  /3,
	and/3,
	or/3,
	=> /3,
	(neg)/2.

:- tool((::)/3, '::_body'/4).
:- tool(($::)/3, '$::_body'/4).
:- tool((#::)/3, '#::_body'/4).
:- tool((#=)/3, '#=_body'/4).
:- tool((#\=)/3, '#\\=_body'/4).
:- tool((#=<)/3, '#=<_body'/4).
:- tool((#>=)/3, '#>=_body'/4).
:- tool((#<)/3, '#<_body'/4).
:- tool((#>)/3, '#>_body'/4).
:- tool(($=)/3, '$=_body'/4).
:- tool(($\=)/3, '$\\=_body'/4).
:- tool(($=<)/3, '$=<_body'/4).
:- tool(($>=)/3, '$>=_body'/4).
:- tool(($<)/3, '$<_body'/4).
:- tool(($>)/3, '$>_body'/4).
:- tool((=:=)/3, '=:=_body'/4).
:- tool((=\=)/3, '=\\=_body'/4).
:- tool((=<)/3, '=<_body'/4).
:- tool((>=)/3, '>=_body'/4).
:- tool((<)/3, '<_body'/4).
:- tool((>)/3, '>_body'/4).
:- tool((and)/3, 'and_body'/4).
:- tool((or)/3, 'or_body'/4).
:- tool((=>)/3, '=>_body'/4).
:- tool((neg)/2, 'neg_body'/3).


'::_body'(X, Range, B, M) :- one_number(X), reified(X :: Range, B, M).
'$::_body'(X, Range, B, M) :- one_number(X), reified(X $:: Range, B, M).
'#::_body'(X, Range, B, M) :- one_number(X), reified(X #:: Range, B, M).
'#=_body'(X, Y, B, M)	:- reified(X #= Y, B, M).
'#\\=_body'(X, Y, B, M)	:- reified(X #\= Y, B, M).
'#=<_body'(X, Y, B, M)	:- reified(X #=< Y, B, M).
'#>=_body'(X, Y, B, M)	:- reified(X #>= Y, B, M).
'#<_body'(X, Y, B, M)	:- reified(X #< Y, B, M).
'#>_body'(X, Y, B, M)	:- reified(X #> Y, B, M).
'$=_body'(X, Y, B, M)	:- reified(X $= Y, B, M).
'$\\=_body'(X, Y, B, M)	:- reified(X $\= Y, B, M).
'$=<_body'(X, Y, B, M)	:- reified(X $=< Y, B, M).
'$>=_body'(X, Y, B, M)	:- reified(X $>= Y, B, M).
'$<_body'(X, Y, B, M)	:- reified(X $< Y, B, M).
'$>_body'(X, Y, B, M)	:- reified(X $> Y, B, M).
'=:=_body'(X, Y, B, M)	:- reified(X =:= Y, B, M).
'=\\=_body'(X, Y, B, M)	:- reified(X =\= Y, B, M).
'=<_body'(X, Y, B, M)	:- reified(X =< Y, B, M).
'>=_body'(X, Y, B, M)	:- reified(X >= Y, B, M).
'<_body'(X, Y, B, M)	:- reified(X < Y, B, M).
'>_body'(X, Y, B, M)	:- reified(X > Y, B, M).
'and_body'(X, Y, B, M)	:- reified(X and Y, B, M).
'or_body'(X, Y, B, M)	:- reified(X or Y, B, M).
'=>_body'(X, Y, B, M)	:- reified(X => Y, B, M).
'neg_body'(X, B, M)	:- reified(neg X, B, M).


reified(Goal, Bool, M) :-
	( nonground(Goal, Var) ->
	    suspend(delay_reified(Goal, Bool, Susp, M), 2, Var->inst, Susp)
	;
	    ( ground_check(Goal, M) -> Bool=1 ; Bool=0 )	% Goal is ground
	).

    :- demon delay_reified/4.
    delay_reified(Goal, Bool, Susp, M) :-
	( nonground(Goal, Var) ->
	    insert_suspension(Var, Susp, inst of suspend)
	;
	    kill_suspension(Susp),
	    ( ground_check(Goal, M) -> Bool=1 ; Bool=0 )	% Goal is ground
	).


%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
% Pretty printing
%- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

:- export
	portray_delayed_goals/2,
	portray(delay_until_ground/3, portray_delayed_goals/2, [goal]).

portray_delayed_goals(delay_until_ground(Goal, _Susp, _M), Pretty) :- -?-> !,
	Pretty = suspend:Goal.


:- export
	portray_delay_reified/2,
	portray(delay_reified/4, portray_delay_reified/2, [goal]).

portray_delay_reified(delay_reified(Goal, Bool, _Susp, _M), Pretty) :- -?-> !,
	Goal =.. [Op|Args],
	append(Args, [Bool], NewArgs),
	ReifGoal =.. [Op|NewArgs],
	Pretty = suspend:ReifGoal.


%--------------------------------------------------------------------
% Comments doc
%--------------------------------------------------------------------

:- comment((::)/2, [
    summary: "Range constraint with optional integrality constraint",
    template: "?Vars :: ?Range",
    args: ["Vars":  "Variable or number, or a list of variables or numbers",
           "Range": "Variable or Lo..Hi, where Lo and Hi are variables or numeric expressions"
          ],
    see_also:[($::)/2, (#::)/2, (::)/3, integers/1, reals/1],
    fail_if: "Vars contains numbers that do not fall within Range, or violate the optional integrality constraint.",
    eg:"
    % with integrality constraint
    ?- X :: 1 .. 5, X = 3.
    X = 3
    Yes (0.00s cpu)

    ?- X :: 1 .. 5, X = 6.
    No (0.00s cpu)

    ?- X :: 1 .. 5, X = 3.0.
    No (0.00s cpu)

    % without integrality constraint
    ?- X :: 1.0 .. 5.0, X = 3.
    X = 3
    Yes (0.00s cpu)

    ?- X :: 1.0 .. 5.0, X = 3.0.
    X = 3.0
    Yes (0.00s cpu)
    ",
    desc: html("\
   This constraint suspends until its arguments are ground. It then succeeds
   iff all the elements of the list Vars are numbers within the range
   specified by Range.
<P>
   The range must eventually be in the form Lo..Hi, where Lo and Hi are 
   expressions evaluating to numbers. If both are integers, then Vars must
   also be integers (integer or list of integers).")]
).

:- comment((#::)/2, [
    summary: "Range constraint combined with integrality constraint",
    template: "?Vars #:: ?Range",
    see_also:[($::)/2, (::)/2, (#::)/3, integers/1, reals/1],
    args: ["Vars":  "Variable or integer, or a list of variables or integers",
           "Range": "Variable or Lo..Hi, where Lo and Hi are variables or integer expressions"
          ],
    exceptions:[5 : "Range contains non-integers."],
    fail_if: "Vars contains non-integers or integers that do not fall within Range.",
    eg:"
    ?- X #:: 1 .. 5, X = 3.
    X = 3
    Yes (0.00s cpu)

    ?- X #:: 1 .. 5, X = 6.
    No (0.00s cpu)

    ?- X #:: 1 .. 5, X = 3.0.
    No (0.00s cpu)
    ",
    desc: html("\
   This constraint suspends until its arguments are ground. It then succeeds
   iff all the elements of the list Vars are integers within the range
   specified by Range.
<P>
   The range must eventually be in the form Lo..Hi, where Lo and Hi are 
   expressions evaluating to integers.")]
).

:- comment(($::)/2, [
    summary: "Pure range constraint",
    template: "?Vars #:: ?Range",
    args: ["Vars":  "Variable or number, or a list of variables or numbers",
           "Range": "Variable or Lo..Hi, where Lo and Hi are variables or numeric expressions"
          ],
    see_also:[(::)/2, (#::)/2, ($::)/3, integers/1, reals/1],
    fail_if: "Vars contains numbers that do not fall within Range.",
    eg:"
    ?- X $:: 1 .. 5, X = 3.0.
    X = 3.0
    Yes (0.00s cpu)

    ?- X $:: 1 .. 5, X = 3.
    X = 3
    Yes (0.00s cpu)

    ?- X $:: 1.0 .. 5.0, X = 3.
    X = 3
    Yes (0.00s cpu)

    ?- X $:: 1.0 .. 5.0, X = 3.0.
    X = 3.0
    Yes (0.00s cpu)
    ",
    desc: html("\
   This constraint suspends until its arguments are ground. It then succeeds
   iff all the elements of the list Vars are numbers within the range
   specified by Range.
<P>
   The range must eventually be in the form Lo..Hi, where Lo and Hi are 
   expressions evaluating to numbers. The type of these numbers is irrelevant.")]
).

:- comment((::)/3, [
    summary: "Reified range constraint with optional integrality constraint",
    args: ["Var":  "Variable or number",
           "Range": "Variable or Lo..Hi, where Lo and Hi are variables or numeric expressions",
           "Bool": "Variable, 0 or 1"
          ],
    see_also:[(::)/2],
    eg:"
    ?- ::(X, 1 .. 5, B), X = 3.
    B = 1
    X = 3
    Yes (0.00s cpu)

    ?- ::(X, 1.0 .. 5.0, B), X = 3.0.
    B = 1
    X = 3.0
    Yes (0.00s cpu)

    % range violated
    ?- ::(X, 1 .. 5, B), X = 6.
    B = 0
    X = 6
    Yes (0.00s cpu)

    % integrality violated
    ?- ::(X, 1 .. 5, B), X = 3.0.
    B = 0
    X = 3.0
    Yes (0.00s cpu)
    ",
    desc: html("\
    Reified version of ::/2, i.e. the truth value of the range constraint is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    ::/2 constraint.
<P>
    Note: as opposed to ::/2, the first argument cannot be a list.
    ")]
).

:- comment((#::)/3, [
    summary: "Reified range constraint combined with integrality constraint",
    args: ["Vars":  "Variable or integer, or a list of variables or integers",
           "Range": "Variable or Lo..Hi, where Lo and Hi are variables or integer expressions",
           "Bool": "Variable, 0 or 1"
          ],
    see_also:[(#::)/2],
    exceptions:[5 : "Range contains non-integers."],
    eg:"
    ?- #::(X, 1 .. 5, B), X = 3.
    B = 1
    X = 3
    Yes (0.00s cpu)

    % range violated
    ?- #::(X, 1 .. 5, B), X = 6.
    B = 0
    X = 6
    Yes (0.00s cpu)

    % integrality violated
    ?- #::(X, 1 .. 5, B), X = 3.0.
    B = 0
    X = 3.0
    Yes (0.00s cpu)
    ",
    desc: html("\
    Reified version of #::/2, i.e. the truth value of the range constraint is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    #::/2 constraint.
<P>
    Note: as opposed to #::/2, the first argument cannot be a list.
    ")]
).

:- comment(($::)/3, [
    summary: "Reified pure range constraint",
    args: ["Vars":  "Variable or number, or a list of variables or numbers",
           "Range": "Variable or Lo..Hi, where Lo and Hi are variables or numeric expressions",
           "Bool": "Variable, 0 or 1"
          ],
    see_also:[($::)/2],
    eg:"
    ?- $::(X, 1 .. 5, B), X = 3.0.
    B = 1
    X = 3.0
    Yes (0.00s cpu)

    % range violated
    ?- $::(X, 1 .. 5, B), X = 6.
    B = 0
    X = 6
    Yes (0.00s cpu)
    ",
    desc: html("\
    Reified version of $::/2, i.e. the truth value of the range constraint is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    $::/2 constraint.
<P>
    Note: as opposed to $::/2, the first argument cannot be a list.
    ")]
).

:- comment(integers/1, [
    summary: "Constrain Vars to be integers",
    args: ["Vars": "List of variables or integers"],
    see_also:[(::)/2, (#::)/2, reals/1],
    fail_if: "Vars contains non-integers.",
    desc: html("\
    This constraint suspends until its argument is ground. It then succeeds
    iff Vars is an integer or a list of integers.")]
).

:- comment(reals/1, [
    summary: "Constrain Vars to be a number or list of numbers",
    args: ["Vars": "List of variables or numbers"],
    see_also:[(::)/2, ($::)/2, integers/1],
    fail_if: "Vars contains non-numbers.",
    eg:"
    ?- reals(L), L = [3.4, 7].
    L = [3.4, 7]
    Yes (0.00s cpu)

    ",
    desc: html("\
    This constraint suspends until its argument is ground. It then succeeds
    iff Vars is a number or a list of numbers (any type).")]
).

:- comment((=:=)/2, [
	summary: "The value of Expr1 is equal to the value of Expr2.",
        template: "?Expr1 =:= ?Expr2",
	see_also:[(=:=)/3, _:(=:=)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is not equal to the value of Expr2",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff they are equal (beware of 
   rounding errors when comparing reals).
") 
]).

:- comment((=\=)/2, [
	summary: "The value of Expr1 is not equal to the value of Expr2.",
        template: "?Expr1 =\\= ?Expr2",
	see_also:[(=\=)/3, _:(=\=)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is equal to the value of Expr2",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff they are not equal (beware of 
   rounding errors when comparing reals).
") 
]).

:- comment((>=)/2, [
	summary: "The value of Expr1 is greater than or equal to the value of Expr2.",
        template: "?Expr1 >= ?Expr2",
	see_also:[(>=)/3, _:(>=)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is smaller than the value of Expr2",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff Expr1 is greater than or equal to
   Expr2 (beware of rounding errors when comparing reals).
") 
]).

:- comment((=<)/2, [
	summary: "The value of Expr1 is less than or equal to the value of Expr2.",
        template: "?Expr1 =< ?Expr2",
	see_also:[(=<)/3, _:(=<)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is greater than the value of Expr2",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff Expr1 is less than or equal to
   Expr2 (beware of rounding errors when comparing reals).
") 
]).

:- comment((>)/2, [
	summary: "The value of Expr1 is greater than the value of Expr2.",
        template: "?Expr1 > ?Expr2",
	see_also:[(>)/3, _:(>)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is not greater than the value of Expr2",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff Expr1 is greater than Expr2 
   (beware of rounding errors when comparing reals).
") 
]).

:- comment((<)/2, [
	summary: "The value of Expr1 is less than the value of Expr2.",
        template: "?Expr1 < ?Expr2",
	see_also:[(<)/3, _:(<)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is not less than the value of Expr2",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff Expr1 is less than Expr2
   (beware of rounding errors when comparing reals).
") 
]).

:- comment(($=)/2, [
	summary: "The value of Expr1 is equal to the value of Expr2.",
        template: "?Expr1 $= ?Expr2",
	see_also:[($=)/3, suspend:(=:=)/2, _:($=)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is not equal to the value of Expr2",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff they are equal (beware of 
   rounding errors when comparing reals).
") 
]).

:- comment(($\=)/2, [
	summary: "The value of Expr1 is not equal to the value of Expr2.",
        template: "?Expr1 $\\= ?Expr2",
	see_also:[($\=)/3, suspend:(=\=)/2, _:($\=)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is equal to the value of Expr2",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff they are not equal (beware of 
   rounding errors when comparing reals).
") 
]).

:- comment(($>=)/2, [
	summary: "The value of Expr1 is greater than or equal to the value of Expr2.",
        template: "?Expr1 $>= ?Expr2",
	see_also:[($>=)/3, suspend:(>=)/2, _:($>=)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is smaller than the value of Expr2",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff Expr1 is than greater or equal
   to Expr2 (beware of rounding errors when comparing reals).
") 
]).

:- comment(($=<)/2, [
	summary: "The value of Expr1 is less than or equal to the value of Expr2.",
        template: "?Expr1 $=< ?Expr2",
	see_also:[($=<)/3, suspend:(=<)/2, _:($=<)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is greater than the value of Expr2",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff Expr1 is less than or equal to
   Expr2 (beware of rounding errors when comparing reals).
") 
]).

:- comment(($>)/2, [
	summary: "The value of Expr1 is greater than the value of Expr2.",
        template: "?Expr1 $> ?Expr2",
	see_also:[($>)/3, suspend:(>)/2, _:($>)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is not greater than the value of Expr2",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff Expr1 is greater than Expr2 
   (beware of rounding errors when comparing reals).
") 
]).

:- comment(($<)/2, [
	summary: "The value of Expr1 is less than the value of Expr2.",
        template: "?Expr1 $< ?Expr2",
	see_also:[($<)/3, suspend:(<)/2, _:($<)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is not less than the value of Expr2",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff Expr1 is less than Expr2
   (beware of rounding errors when comparing reals).
") 
]).

:- comment((#=)/2, [
	summary: "The integer value of Expr1 is equal to the integer value of Expr2.",
        template: "?Expr1 #= ?Expr2",
	see_also:[(#=)/3, _:(#=)/2],
	args:["Expr1" : "An integer arithmetic expression", 
              "Expr2" : "An integer arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is not equal to the value of Expr2, or if either do not evaluate to an integer.",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff they are both integers and are
   equal.
") 
]).

:- comment((#\=)/2, [
	summary: "The integer value of Expr1 is not equal to the integer value of Expr2.",
        template: "?Expr1 #\\= ?Expr2",
	see_also:[(#\=)/3, _:(#\=)/2],
	args:["Expr1" : "An integer arithmetic expression", 
              "Expr2" : "An integer arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is equal to the value of Expr2, or if either do not evaluate to an integer.",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff they are both integers and are
   not equal.
") 
]).

:- comment((#>=)/2, [
	summary: "The integer value of Expr1 is greater than or equal to the integer value of Expr2.",
        template: "?Expr1 #>= ?Expr2",
	see_also:[(#>=)/3, _:(#>=)/2],
	args:["Expr1" : "An integer arithmetic expression", 
              "Expr2" : "An integer arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is less than the value of Expr2, or if either do not evaluate to an integer.",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff they are both integers and 
   Expr1 is greater than or equal to Expr2.
") 
]).

:- comment((#=<)/2, [
	summary: "The integer value of Expr1 is less than or equal to the integer value of Expr2.",
        template: "?Expr1 #=< ?Expr2",
	see_also:[(#=<)/3, _:(#=<)/2],
	args:["Expr1" : "An integer arithmetic expression", 
              "Expr2" : "An integer arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is greater than the value of Expr2, or if either do not evaluate to an integer.",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff they are both integers and 
   Expr1 is less than or equal to Expr2.
") 
]).

:- comment((#>)/2, [
	summary: "The integer value of Expr1 is greater than the integer value of Expr2.",
        template: "?Expr1 #> ?Expr2",
	see_also:[(#>)/3, _:(#>)/2],
	args:["Expr1" : "An integer arithmetic expression", 
              "Expr2" : "An integer arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is not greater than the value of Expr2, or if either do not evaluate to an integer.",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff they are both integers and 
   Expr1 is greater than Expr2.
") 
]).

:- comment((#<)/2, [
	summary: "The integer value of Expr1 is less than the integer value of Expr2.",
        template: "?Expr1 #< ?Expr2",
	see_also:[(#<)/3, _:(#<)/2],
	args:["Expr1" : "An integer arithmetic expression", 
              "Expr2" : "An integer arithmetic expression"],
	fail_if:"   fails if the value of Expr1 is not less than the value of Expr2, or if either do not evaluate to an integer.",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],

        desc: html("\
   Suspends until both Expr1 and Expr2 are ground, and then both arguments
   are evaluated and compared, succeeding iff they are both integers and 
   Expr1 is less than Expr2.
") 
]).

:- comment((=:=)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[(=:=)/2,($=)/3],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of =:=/2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    =:=/2 constraint.
") 
]).

:- comment(($=)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[($=)/2,(=:=)/3],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of $=/2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    $=/2 constraint.
") 
]).

:- comment((#=)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[(#=)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of #=/2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    #=/2 constraint.
") 
]).

:- comment((=\=)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[(=\=)/2,($\=)/3],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of =\\=/2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    =\\=/2 constraint.
") 
]).

:- comment(($\=)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[($\=)/2,(=\=)/3],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of $\\=/2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    $\\=/2 constraint.
") 
]).

:- comment((#\=)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[(#\=)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of #\\=/2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    #\\=/2 constraint.
") 
]).

:- comment((>=)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[(>=)/2,($>=)/3],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of >=/2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    >=/2 constraint.
") 
]).

:- comment(($>=)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[($>=)/2,(>=)/3],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of $>=/2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    $>=/2 constraint.
") 
]).

:- comment((#>=)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[(#>=)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of #>=/2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    #>=/2 constraint.
") 
]).

:- comment((=<)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[(=<)/2,($=<)/3],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of =</2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    =</2 constraint.
") 
]).

:- comment(($=<)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[($=<)/2,(=<)/3],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of $=</2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    $=</2 constraint.
") 
]).

:- comment((#=<)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[(#=<)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of #=</2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    #=</2 constraint.
") 
]).

:- comment((>)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[(>)/2,($>)/3],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of >/2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    >/2 constraint.
") 
]).

:- comment(($>)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[($>)/2,(>)/3],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of $>/2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    $>/2 constraint.
") 
]).

:- comment((#>)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[(#>)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of #>/2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    #>/2 constraint.
") 
]).

:- comment((<)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[(<)/2,($<)/3],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of </2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    </2 constraint.
") 
]).

:- comment(($<)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[($<)/2,(<)/3],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of $</2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    $</2 constraint.
") 
]).

:- comment((#<)/3, [
	summary: "Reified arithmetic comparison",
	see_also:[(#<)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of #</2, i.e. the truth value of the comparison is
    reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    #</2 constraint.
") 
]).


:- comment((and)/2, [
	summary: "Both Expr1 and Expr2 arithmetically evaluate to 1",
        template: "?Expr1 and ?Expr2",
	see_also:[(and)/3, (or)/2, (=>)/2, (neg)/1, _:(and)/2],
	args:["Expr1" : "A boolean expression", 
              "Expr2" : "A boolean expression"],
	fail_if:"Expr1 or Expr2 do not both evaluate to 1",
	eg:"
	?- B and 1, B = 1.
	B = 1
	Yes (0.00s cpu)

	?- B and 1, B = 0.
	No (0.00s cpu)

	% arguments are typically reifiable expressions:
	?- X > 5 and X < 7, X = 7.
	No (0.00s cpu)

	% the previous example is equivalent to:
	?- >(X,5,B1), <(X,7,B2), B1 and B2, X=7.
	No (0.00s cpu)

	% and/or/=>/neg are themselves reifiable:
	?- X > 5 and neg(X < 7), X = 7.
	X = 7
	Yes (0.00s cpu)
	",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Suspends until both Expr1 and Expr2 are ground, and then both arguments
    are evaluated. Succeeds if both evaluate to 1.
<P>
    Typically, the expressions contains reifiable constraints, in which case
    a corresponding reified constraint is set up, and the expression is
    replaced by the resulting boolean variable.
") 
]).

:- comment((or)/2, [
	summary: "At least one of Expr1 or Expr2 arithmetically evaluate to 1",
        template: "?Expr1 or ?Expr2",
	see_also:[(or)/3, (and)/2, (=>)/2, (neg)/1, _:(or)/2],
	args:["Expr1" : "A boolean expression", 
              "Expr2" : "A boolean expression"],
	fail_if:"Neither Expr1 nor Expr2 evaluates to 1",
	eg:"
	?- B or 1, B = 0.
	B = 0
	Yes (0.00s cpu)

	?- B or 0, B = 0.
	No (0.00s cpu)

	% arguments are typically reifiable expressions:
	?- X > 7 or X < 5, X = 7.
	No (0.00s cpu)

	% the previous example is equivalent to:
	?- >(X,7,B1), <(X,5,B2), B1 or B2, X=7.
	No (0.00s cpu)

	% and/or/=>/neg are themselves reifiable:
	?- X > 7 or neg(X < 5), X = 7.
	X = 7
	Yes (0.00s cpu)
	",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Suspends until both Expr1 and Expr2 are ground, and then both arguments
    are evaluated. Succeeds if at least one evaluates to 1.
<P>
    Typically, the expressions contains reifiable constraints, in which case
    a corresponding reified constraint is set up, and the expression is
    replaced by the resulting boolean variable.
") 
]).

:- comment((=>)/2, [
	summary: "If Expr1 arithmetically evaluates to 1, so does Expr2 (implication)",
        template: "?Expr1 => ?Expr2",
	see_also:[(=>)/3, (and)/2, (or)/2, (neg)/1, _:(=>)/2],
	args:["Expr1" : "A boolean expression", 
              "Expr2" : "A boolean expression"],
	fail_if:"Expr1 evaluates to 1 and Expr2 evaluates to 0",
	eg:"
	?- 0 => B, B = 0.
	B = 0
	Yes (0.00s cpu)

	?- 0 => B, B = 1.
	B = 1
	Yes (0.00s cpu)

	?- 1 => B, B = 0.
	No (0.00s cpu)

	?- 1 => B, B = 1.
	B = 1
	Yes (0.00s cpu)

	% arguments are typically reifiable expressions:
	?- X > Y => X > Y+10, X = 5, Y = 3.
	No (0.00s cpu)

	% the previous example is equivalent to:
	?- >(X,Y,B1), >(X,Y+10,B2), B1 => B2, X = 5, Y = 3.
	No (0.00s cpu)

	% and/or/=>/neg are themselves reifiable:
	?- neg(A => B) or (C => D), A=1, B=0, C=0, D=1.
	A = 1
	B = 0
	C = 0
	D = 1
	Yes (0.00s cpu)
	",
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Suspends until both Expr1 and Expr2 are ground, and then both arguments
    are evaluated. Succeeds if the truth of Expr1 implies Expr2, i.e. if Expr1
    evaluates to 1, Expr2 must evaluate to 1, otherwise Expr2 can evaluate to
    1 or 0. Logically equivalent to
    <PRE>
    	neg(Expr1) or Expr2.
    </PRE>
    Typically, the expressions contains reifiable constraints, in which case
    a corresponding reified constraint is set up, and the expression is
    replaced by the resulting boolean variable.
") 
]).

:- comment((neg)/1, [
	summary: "Expr arithmetically evaluates to 0",
        template: "neg ?Expr",
	see_also:[(neg)/2, (and)/2, (or)/2, (=>)/2, _:(neg)/1],
	args:["Expr" : "A boolean expression"],
	fail_if:"Expr does not evaluate to 0",
	eg:"
	?- neg B, B = 1.
	No (0.00s cpu)

	?- neg B, B = 0.
	B = 0
	Yes (0.00s cpu)

	% arguments are typically reifiable expressions:
	?- neg X > 7, X = 8.
	No (0.00s cpu)

	% the previous example is equivalent to:
	?- >(X,7,B), neg B, X=8.
	No (0.00s cpu)

	% and/or/=>/neg are themselves reifiable:
	?- neg(X > 7 or X < 5), X = 7.
	X = 7
	Yes (0.00s cpu)
	",
	exceptions:[24 : "Expr is not an arithmetic expression."],
        desc: html("\
    Suspends until Expr is ground, and then evaluates it. Succeeds if it
    evaluates to 0.
<P>
    Typically, the expression contains reifiable constraints, in which case
    a corresponding reified constraint is set up, and the expression is
    replaced by the resulting boolean variable.
") 
]).

:- comment((and)/3, [
	summary: "Reified boolean operation",
	see_also:[(and)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of and/2, i.e. the truth value of the boolean operation
    is reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    and/2 constraint.
") 
]).

:- comment((or)/3, [
	summary: "Reified boolean operation",
	see_also:[(or)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of or/2, i.e. the truth value of the boolean operation
    is reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    or/2 constraint.
") 
]).

:- comment((=>)/3, [
	summary: "Reified boolean operation",
	see_also:[(=>)/2],
	args:["Expr1" : "An arithmetic expression", "Expr2" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr1 or Expr2 is not an arithmetic expression."],
        desc: html("\
    Reified version of =>/2, i.e. the truth value of the boolean operation
    is reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first two arguments are ground.
    It then unifies Bool according to the truth value of the corresponding
    =>/2 constraint.
") 
]).

:- comment((neg)/2, [
	summary: "Reified boolean operation",
	see_also:[(neg)/1],
	args:["Expr" : "An arithmetic expression", "Bool":"Variable, 0 or 1"],
	exceptions:[24 : "Expr is not an arithmetic expression."],
        desc: html("\
    Reified version of neg/1, i.e. the truth value of the boolean operation
    is reflected in the value of the 0/1 variable Bool.
<P>
    This constraint suspends until its first argument is ground.
    It then unifies Bool according to the truth value of the corresponding
    neg/1 constraint.
") 
]).

