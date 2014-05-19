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
% Version:	$Id: fd_arith.pl,v 1.2 2008/08/21 17:54:49 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * FINITE DOMAINS
 *
 * IDENTIFICATION:      fd_arith.pl 
 *
 * AUTHOR:		Micha Meier
 *
 * DESCRIPTION:         Arithmetic constraints over linear or polynomial
			terms and structures.
 */


:- module(fd_arith).

:- reexport fd_domain.

:- export op(700, xfx, ##).
:- export op(750, fy, #\+).
:- export op(760, yfx, #/\).
:- export op(770, yfx, #\/).
:- export op(780, yfx, #=>).
:- export op(790, yfx, #<=>).
:- export op(800, xfx, isd).
:- export op(400, yfx, *`).		% to print qeqsquarecheck

% Export transformation routines.
:- export
	check_dom/1,
	fd_eq/1,
	fd_eq/2,
	fd_ge/1,
	fd_ge/2,
	fd_gec/5,
	fd_gec_ent/6,
	fd_ineq/1,
	fd_ineq/2,
	fd_qeq/3,
	fd_re/2,
	fd_eval/1,
	tr_fd_arith_bool/2,
	tr_fd_arith_in/2,
	tr_fd_arith_out/2.

% Output Macros

:- export macro(fd_eq/1, tr_fd_arith_out/2, [write, goal]).
:- export macro(fd_eq/2, tr_fd_arith_out/2, [write, goal]).
:- export macro(fd_abs/2, tr_fd_arith_out/2, [write, goal]).
:- export macro(fd_ge/1, tr_fd_arith_out/2, [write, goal]).
:- export macro(fd_ge/2, tr_fd_arith_out/2, [write, goal]).
:- export macro(fd_gec/5, tr_fd_arith_out/2, [write, goal]).
:- export macro(fd_gec_ent/6, tr_fd_arith_out/2, [write, goal]).
:- export macro(fd_ineq/1, tr_fd_arith_out/2, [write, goal]).
:- export macro(fd_ineq/2, tr_fd_arith_out/2, [write, goal]).
:- export macro(fd_re/2, tr_fd_arith_out/2, [write, goal]).
:- export macro(fd_qeq/3, tr_fd_arith_out/2, [write, goal]).
:- export macro(ge/3, tr_fd_arith_out/2, [write, goal]).
:- export macro(gec/4, tr_fd_arith_out/2, [write, goal]).
:- export macro(gec_ent/6, tr_fd_arith_out/2, [write, goal]).
:- export macro(qeq/3, tr_fd_arith_out/2, [write, goal]).
:- export macro(qeqsquare/3, tr_fd_arith_out/2, [write, goal]).
:- export macro(eq_ent/3, tr_fd_arith_out/2, [write, goal]).
:- export macro(eq/2, tr_fd_arith_out/2, [write, goal]).



% Goal Macros
:- inline((#>=)/2, tr_fd_arith_in/2).
:- inline((#>)/2, tr_fd_arith_in/2).
:- inline((#<=)/2, tr_fd_arith_in/2).
:- inline((#=<)/2, tr_fd_arith_in/2).
:- inline((#<)/2, tr_fd_arith_in/2).
:- inline((#>=)/3, tr_fd_arith_in/2).
:- inline((#>)/3, tr_fd_arith_in/2).
:- inline((#<=)/3, tr_fd_arith_in/2).
:- inline((#=<)/3, tr_fd_arith_in/2).
:- inline((#<)/3, tr_fd_arith_in/2).
:- inline((#=)/2, tr_fd_arith_in/2).
:- inline((#=)/3, tr_fd_arith_in/2).
:- inline((#\=)/2, tr_fd_arith_in/2).
:- inline((#\=)/3, tr_fd_arith_in/2).
:- inline((##)/2, tr_fd_arith_in/2).
:- inline((##)/3, tr_fd_arith_in/2).
:- inline((#/\)/2, tr_fd_arith_in/2).
:- inline((#\/)/2, tr_fd_arith_in/2).
:- inline((#\+)/1, tr_fd_arith_in/2).
:- inline((#=>)/2, tr_fd_arith_in/2).
:- inline((#<=>)/2, tr_fd_arith_in/2).
:- inline((#/\)/3, tr_fd_arith_in/2).
:- inline((#\/)/3, tr_fd_arith_in/2).
:- inline((#\+)/2, tr_fd_arith_in/2).
:- inline((#=>)/3, tr_fd_arith_in/2).
:- inline((#<=>)/3, tr_fd_arith_in/2).
:- inline(fd_eval/1, tr_fd_arith_in/2).
:- inline((isd)/2, tr_fd_arith_in/2).


:- export
				% not skipped because might suspend & wake
    #=  /2,
    #>  /2,
    #<  /2,
    #>= /2,
    #<= /2,
    #=< /2,
    #=  /3,
    #>  /3,
    #<  /3,
    #>= /3,
    #<= /3,
    #=< /3,
    ##  /2,
    ##  /3,
    #/\ /2,
    #\/ /2,
    #=> /2,
    #<=> /2,
    (#\+) /1,
    #/\ /3,
    #\/ /3,
    #=> /3,
    #<=> /3,
    (#\+) /2,
    #\= /2,
    #\= /3,
    (isd)/2,
    default_domain/1.

:- export
	term_to_linear/2,
	linear_term_range/3.

:- import
	% general-purpose predicates
	get_bip_error/1,
	setarg/3,
	set_bip_error/1,
	suspensions_to_goals/3,
	trprotect/2,

	% FD-specific predicates
	attr_instantiate/2,
	lt_test/3,
	make_extreme/2,
	linear_term_range_eq/6,
	linear_term_range_ge/6,
	linear_term_range_only/6,
	ex_insert_suspension/3,
	gec_insert_suspension/4,
	gec_comp/5,
	gec_start/7,
	gec_ent_start/7,
	gec_test/5,
	ineq_test/4,
	peval/4,
	remove_element/3
    from sepia_kernel.

:- pragma(nodebug).
:- pragma(system).


fderror(N, G) :-
	error(N, G, _).

%
% Transformation routines
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Input goal transformation
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This should work, but i don't want to risk hitting compiler bugs now...
%:- local 'C'/3.
%:- inline('C'/3, tr_c/2).
%tr_c('C'(S, Token, Rest), S = [Token|Rest]).
%'C'(S, Token, Rest) :- S = [Token|Rest].

tr_fd_arith_in(fd_eval(Goal), G) :-
    -?->
    !,
    Goal = G.
tr_fd_arith_in(isd(Bool, Expr), G) :-
    -?->
    !,
    conv_bool_expr(Expr, Bool, compile, List, []),
    list_goals(List, G).
tr_fd_arith_in(Pred, Goals) :-
    conv_pred(Pred, compile, List, []),
    list_goals(List, Goals).

tr_fd_arith_bool(Var, Val) :-
    conv_bool_expr(Var, Val, run, Goals, []),
    call_list(Goals).

:- mode conv_pred(+, ?, +, ?).
:- mode conv_expr(+, ?, ?, +, ?).
%
% transform a predicate into its compiled/executable form
%
						% tell arithmetic constraints
conv_pred(A #= B, Mode) -->
    {(compound(A) -> true; compound(B)),	% don't transform simple args
    !},
    conv_expr(A - B, Expr, Mode),
    {linearize(Expr, List)},
    [fd_arith:fd_eq(List)].
conv_pred(A #\= B, Mode) -->
    conv_ne(A, B, Mode).
conv_pred(A ## B, Mode) -->
    (conv_ne(A, B, Mode) ->
	{true}
    ;
	[fd_arith:(A #\= B)]
    ).
conv_pred(A #>= B, Mode) -->
    conv_expr(A - B, Expr, Mode),
    {linearize(Expr, List)},
    conv_ge(List, Mode).
conv_pred(A #> B, Mode) -->
    conv_pred(A - 1 #>= B, Mode).
conv_pred(A #=< B, Mode) -->
    conv_pred(B #>= A, Mode).
conv_pred(A #<= B, Mode) -->
    conv_pred(B #>= A, Mode).
conv_pred(A #< B, Mode) -->
    conv_pred(B - 1 #>= A, Mode).
						% ask arithmetic constraints
conv_pred(#=(A, B, Bool), Mode) -->
    {(compound(A) -> true; compound(B)),	% don't transform simple args
    !},
    conv_expr(A - B, Expr, Mode),
    {linearize(Expr, List)},
    [fd_arith:fd_eq(List, Bool)].
conv_pred(#\=(A, B, Bool), Mode) -->
    conv_ne(A, B, Bool, Mode).
conv_pred(##(A, B, Bool), Mode) -->
    conv_ne(A, B, Bool, Mode).
conv_pred(#>=(A, B, Bool), Mode) -->
    conv_expr(A - B, Expr, Mode),
    {linearize(Expr, List)},
    conv_ge(List, Bool, Mode).
conv_pred(#>(A, B, Bool), Mode) -->
    conv_pred(#>=(A - 1, B, Bool), Mode).
conv_pred(#=<(A, B, Bool), Mode) -->
    conv_pred(#>=(B, A, Bool), Mode).
conv_pred(#<=(A, B, Bool), Mode) -->
    conv_pred(#>=(B, A, Bool), Mode).
conv_pred(#<(A, B, Bool), Mode) -->
    conv_pred(#>=(B - 1, A, Bool), Mode).
						% tell boolean constraints
conv_pred(A #\/ B, Mode) -->
    conv_bool_expr(A, AC, Mode),
    conv_bool_expr(B, BC, Mode),
    conv_sub_pred(AC + BC #>= 1, Mode).
conv_pred(A #/\ B, Mode) -->
    conv_sub_pred(A, Mode),	% make a normal conjunction out of it
    conv_sub_pred(B, Mode).
conv_pred(#\+ A, Mode) -->
    conv_bool_expr(A, 0, Mode).
conv_pred(A #=> B, Mode) -->
    conv_bool_expr(A, AC, Mode),
    conv_bool_expr(B, BC, Mode),
    conv_sub_pred(AC #<= BC, Mode).
conv_pred(A #<=> B, Mode) -->
    conv_bool_expr(A, AC, Mode),
    conv_bool_expr(B, AC, Mode).
						% ask boolean constraints
conv_pred(#\/(A, B, Bool), Mode) -->
    conv_bool_expr(A, AC, Mode),
    conv_bool_expr(B, BC, Mode),
    conv_sub_pred(#>=(AC + BC, 1, Bool), Mode).
conv_pred(#/\(A, B, Bool), Mode) -->
    conv_bool_expr(A, AC, Mode),
    conv_bool_expr(B, BC, Mode),
    conv_sub_pred(#=(AC + BC, 2, Bool), Mode).
conv_pred(#=>(A, B, Bool), Mode) -->
    conv_bool_expr(A, AC, Mode),
    conv_bool_expr(B, BC, Mode),
    conv_sub_pred(#<=(AC, BC, Bool), Mode).
conv_pred(#<=>(A, B, Bool), Mode) -->
    conv_bool_expr(A, AC, Mode),
    conv_bool_expr(B, BC, Mode),
    {linearize(1*AC+(-1)*BC, List)},	% conv_pred(#=(AC, BC, Bool), Mode).
    [fd_arith:fd_eq(List, Bool)].
conv_pred(#\+(A, Bool), Mode) -->
    conv_bool_expr(A, AC, Mode),
    {linearize(1*AC, List)},		% conv_pred(#=(AC, 0, Bool), Mode).
    [fd_arith:fd_eq(List, Bool)].


% like conv_pred, but don't fail

conv_sub_pred(Goal, Mode) -->
    conv_pred(Goal, Mode), !.
conv_sub_pred(Goal, _Mode) -->
    [Goal].

%
% transform a boolean expression into its compiled/executable form
%
conv_bool_expr(Var, Val, Mode) -->
    {var(Var),
    !},
    conv_bool_var(Var, Val, Mode, 1).
conv_bool_expr(Expr, Bool, Mode) -->
    {bool_functor(Expr),
    !,				% by default, add one argument and evaluate as
    Expr =.. [F|Args],		% a predicate (similar to is/2)
    (Mode == run ->
	Bool :: 0..1
    ;
	true
    ),
    append(Args, [Bool], NewArgs),
    BoolExpr =.. [F|NewArgs]},
    conv_sub_pred(BoolExpr, Mode).
conv_bool_expr(Expr, Val, Mode) -->
    conv_expr(Expr, Val, Mode).

conv_bool_var(Var, Var, run, _) -->
    {Var :: 0..1}.
%    {fderror(4, bool_expr(Var))}.
conv_bool_var(Var, Val, compile, _) -->
    [fd_arith:tr_fd_arith_bool(Var, Val)].

%
% transform an arithmetic expression into its compiled/executable form
%
conv_expr(Expr, Val, Mode) -->
    conv_expr(Expr, Val, Mode, 1).

conv_expr(Var, Val, Mode, K) -->
    {var(Var),
    !},
    conv_expr_var(Var, Val, Mode, K).
conv_expr(A + B, AC + BC, Mode, K) -->
    !,
    conv_expr(A, AC, Mode, K),
    conv_expr(B, BC, Mode, K).
conv_expr(A - B, AC + BC, Mode, K) -->
    !,
    conv_expr(A, AC, Mode, K),
    {K1 is -K},
    conv_expr(B, BC, Mode, K1).
conv_expr(-A, AC, Mode, K) -->
    !,
    {K1 is -K},
    conv_expr(A, AC, Mode, K1).
conv_expr(A * B, Val, Mode, K) -->
    !,
    conv_mult(A, B, Val, Mode, K).
conv_expr(A / B, K*X, Mode, K) -->
    !,
    conv_sub_pred(X * B #= A, Mode).
conv_expr(sum(List), Val, Mode, K) -->
    !,
    conv_sum(List, Val, Mode, K).
conv_expr(Expr, Val, Mode, K) -->
    {compound(Expr), arith_functor(Expr)}, !,
    conv_arith(Expr, Elem, Mode),
    conv_expr(Elem, Val, Mode, K).
conv_expr(N, Val, _, K) -->
    {integer(N),
    Val is N * K}.

    conv_arith(Expr, Val, run) -->
	{ Val is Expr }.
    conv_arith(Expr, Val, compile) -->
	peval(Val, Expr).

conv_mult(A, B, K*Val, Mode, K) -->
    {var(A),
    var(B),
    !,
    Mode == run},		% it might not be a good idea to expand *
    register_var(A, Mode),	% as qeq, because one might become an integer
    register_var(B, Mode),
    [fd_arith:fd_qeq(A, B, Val)].
conv_mult(A, B*C, Val, Mode, K) -->
    -?->
    {integer(B)},
    !,
    ({B = 0} ->
	{Val = 0}
    ;
	{K1 is B * K},
	conv_mult(A, C, Val, Mode, K1)
    ).
conv_mult(A, B+C, Val, Mode, K) -->
    -?->
    !,
    {Val = E1 + E2},
    conv_mult(A, B, E1, Mode, K),
    conv_mult(A, C, E2, Mode, K).
conv_mult(A, B-C, Val, Mode, K) -->
    -?->
    !,
    {Val = E1 + E2},
    conv_mult(A, B, E1, Mode, K),
    {K1 is -K},
    conv_mult(A, C, E2, Mode, K1).
conv_mult(A, B, Val, Mode, K) -->
    {integer(B)},
    !,
    ({B = 0} ->
	{Val = 0}
    ;
	{K1 is B * K},
	conv_expr(A, Val, Mode, K1)
    ).
conv_mult([A|As], [B|Bs], Val, Mode, K) -->
    -?->
    !,
    {make_prod_sum(As, Bs, ABs)},
    conv_sum([A*B|ABs], Val, Mode, K).
conv_mult(A, B, Val, Mode, K) -->
    {compound(B)},
    !,
    conv_expr(B, E1, Mode, 1),
    conv_mult(A, E1, Val, Mode, K).
conv_mult(_A, B, _Val, _Mode, _K) -->
    {atomic(B),
    !,
    error(5, fd_arith(B)),
    fail}.
conv_mult(A, B, Val, Mode, K) -->
    conv_mult(B, A, Val, Mode, K).

conv_sum([], Sum, _Mode, _K) --> -?-> !, { Sum=0 }.
conv_sum([A|As], Sum, Mode, K) -->
    -?->
    { Sum = AC+AsC },
    conv_expr(A, AC, Mode, K),
    conv_sum(As, AsC, Mode, K).
conv_sum(subscript(Array,Index), Sum, Mode, K) -->
    -?->
    { Mode==run, subscript(Array,Index,Elem) },
    ( { number(Elem) } -> conv_expr(Elem, Sum, Mode, K)
    ; { var(Elem) } -> conv_expr(Elem, Sum, Mode, K)
    ; conv_sum(Elem, Sum, Mode, K)
    ).

make_prod_sum([], [], Sum) :- -?-> Sum=[].
make_prod_sum([X|Xs], [Y|Ys], Sum) :- -?->
    Sum = [X*Y|XYs],
    make_prod_sum(Xs, Ys, XYs).


conv_expr_var(Var{fd:(fd with [])}, Val, _, K) -->
    -?->
    !,
    {Val = K*Var}.
conv_expr_var(Var, K*Var, compile, K) -->
    !.
conv_expr_var(Var, K*Var, _, K) -->
    {default_domain(Var)}.

register_var(Var, Mode) -->
    conv_expr_var(Var, _, Mode, 1).

list_goals([], true).
list_goals([G|L], Goals) :-
    list_goals(G, L, Goals).

list_goals(G, [], G).
list_goals(G, [Next|L], (G, Goals)) :-
    list_goals(Next, L, Goals).

%
% Convert a linear expression to a list
%
linearize(Expr, [C|L2]) :-
    linearize(Expr, L, [], 0, C),
    sort(2, =<, L, L1),
    var_sort(L1, L2).

linearize(A + B, L, LC, C0, C1) :-
    !,
    linearize(A, L, L1, C0, C2),
    linearize(B, L1, LC, C2, C1).
linearize(C, L, L, C0, C1) :-
    integer(C),
    !,
    C1 is C + C0.
linearize(Prod, [Prod|LC], LC, C, C).

    
var_sort([], []).
var_sort([K*V|T], LT) :-
    var_sort(K, V, T, LT).

var_sort(K, V, [], L) :-
    (K = 0 ->
	L = []
    ;
	L = [K*V]
    ).
var_sort(K, V, [M*V1|T], LT) :-
    (V == V1 ->
	K1 is K + M,
	var_sort(K1, V, T, LT)
    ;
	(K \== 0 ->
	    LT = [K*V|LT1],
	    var_sort(M, V1, T, LT1)
	;
	    var_sort(M, V1, T, LT)
	)
    ).

unlinearize([K], K) :- !.
unlinearize([H|L], H+T) :-
    unlinearize(L, T).

:- mode conv_ge(+, ?, ?, ?).
conv_ge([C], _) -->
    !,
    ({C >= 0} -> {true}; [fail]).
conv_ge([C, K*X], _) -->
    !,
    [fd_arith:fd_gec(0, K, X, C, 0)].
conv_ge([C, 1*X, K*Y], _) -->
    !,
    [fd_arith:fd_gec(X, K, Y, C, 0)].
conv_ge([C, K*X, 1*Y], _) -->
    !,
    [fd_arith:fd_gec(Y, K, X, C, 0)].
conv_ge([0|L], _) -->
    {L = [_, _, _],
    find_coefs(L, Goal)},
    !,
    [Goal].
conv_ge(List, _) -->
    [fd_arith:fd_ge(List)].

find_coefs([-1*Y, 1*X, 1*C], fd_arith:fd_gec(X, -1, Y, C, 0)) :- !.
find_coefs([1*C, -1*Y, 1*X], fd_arith:fd_gec(X, -1, Y, C, 0)) :- !.
find_coefs([1*X, 1*C, -1*Y], fd_arith:fd_gec(X, -1, Y, C, 0)) :- !.
find_coefs([-1*Y, -1*D, 1*X], fd_arith:fd_gec(X, -1, Y, 0, D)) :- !.
find_coefs([1*X, -1*Y, -1*D], fd_arith:fd_gec(X, -1, Y, 0, D)) :- !.
find_coefs([-1*D, 1*X, -1*Y], fd_arith:fd_gec(X, -1, Y, 0, D)) :- !.

:- mode conv_ge(+, -, ?, ?, ?).
conv_ge([C], Bool, _) -->
    !,
    ({C >= 0} -> [Bool = 1]; [Bool = 0]).
conv_ge([C, K*X], Bool, _) -->
    !,
    [fd_arith:fd_gec_ent(0, K, X, C, 0, Bool)].
conv_ge([C, 1*X, K*Y], Bool, _) -->
    !,
    [fd_arith:fd_gec_ent(X, K, Y, C, 0, Bool)].
conv_ge([C, K*X, 1*Y], Bool, _) -->
    !,
    [fd_arith:fd_gec_ent(Y, K, X, C, 0, Bool)].
conv_ge([0|L], Bool, _) -->
    {L = [_, _, _],
    find_coefs(L, Bool, Goal)},
    !,
    [Goal].
conv_ge(List, Bool, _) -->
    [fd_arith:fd_ge(List, Bool)].

find_coefs([-1*Y, 1*X, 1*C], Bool, fd_arith:fd_gec_ent(X, -1, Y, C, 0, Bool)) :- !.
find_coefs([1*C, -1*Y, 1*X], Bool, fd_arith:fd_gec_ent(X, -1, Y, C, 0, Bool)) :- !.
find_coefs([1*X, 1*C, -1*Y], Bool, fd_arith:fd_gec_ent(X, -1, Y, C, 0, Bool)) :- !.
find_coefs([-1*Y, -1*D, 1*X], Bool, fd_arith:fd_gec_ent(X, -1, Y, 0, D, Bool)) :- !.
find_coefs([1*X, -1*Y, -1*D], Bool, fd_arith:fd_gec_ent(X, -1, Y, 0, D, Bool)) :- !.
find_coefs([-1*D, 1*X, -1*Y], Bool, fd_arith:fd_gec_ent(X, -1, Y, 0, D, Bool)) :- !.

conv_ne(A, B, _) -->
    {var(A),
    nonvar(B),
    is_element(B)},
    !,
    [fd_arith:fd_re(A, B)].
conv_ne(A, B, _) -->
    {nonvar(A),
    is_element(A),
    var(B)},
    !,
    [fd_arith:fd_re(B, A)].
conv_ne(A, B, Mode) -->
    {(compound(A) -> true; compound(B))},
    conv_expr(A - B, Expr, Mode),
    {linearize(Expr, List)},
    [fd_arith:fd_ineq(List)].

conv_ne(A, B, Bool, Mode) -->
    {(compound(A) -> true; compound(B))},
    conv_expr(A - B, Expr, Mode),
    {linearize(Expr, List)},
    [fd_arith:fd_ineq(List, Bool)].

is_element(Var) :-
    var(Var).
is_element(A) :-
    atomic(A).
is_element(C) :-
    compound(C),
    ground(C),
    not arith_functor(C).

arith_functor(+_).
arith_functor(-_).
arith_functor(_+_).
arith_functor(_-_).
arith_functor(_*_).
arith_functor(_/_).
arith_functor(sum(_)).
arith_functor(subscript(_,_)).

bool_functor(_::_).
bool_functor(_#=_).
bool_functor(_#<=_).
bool_functor(_#=<_).
bool_functor(_#>=_).
bool_functor(_#<_).
bool_functor(_#>_).
bool_functor(_##_).
bool_functor(_#\=_).
bool_functor(#\+_).
bool_functor(_#/\_).
bool_functor(_#\/_).
bool_functor(_#=>_).
bool_functor(_#<=>_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Output goal transformation
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- mode tr_fd_arith_out(+, -).
tr_fd_arith_out(In, Out) :-
    arg(1, In, Arg),
    ( Arg == ... ->
	tr_fd_arith_out_dummy(In, Out)	% debugger dummy goal with ... args
    ;
	tr_fd_arith_out_(In, Out)
    ).

:- mode tr_fd_arith_out_(+, -).
tr_fd_arith_out_(fd_eq(List), Expr #= 0) :-
    !,
    list_to_expr(List, Expr).
tr_fd_arith_out_(fd_eq(List, B), #=(Expr, 0, B)) :-
    !,
    list_to_expr(List, Expr).
tr_fd_arith_out_(fd_ge(List), Expr #>= 0) :-
    !,
    list_to_expr(List, Expr).
tr_fd_arith_out_(fd_ge(List, B), #>=(Expr, 0, B)) :-
    !,
    list_to_expr(List, Expr).
tr_fd_arith_out_(fd_gec(X, K, Y, C, D), Expr #>= D) :-
    !,
    next_elem(K*Y, X, Expr1), 
    (C == 0 -> Expr = Expr1; Expr = Expr1 + C).
tr_fd_arith_out_(fd_gec_ent(X, K, Y, C, D, Bo), #>=(Expr + C, D, Bo)) :-
    !,
    next_elem(K*Y, X, Expr).
tr_fd_arith_out_(fd_ineq(List), Expr #\= 0) :-
    !,
    list_to_expr(List, Expr).
tr_fd_arith_out_(fd_ineq(List, B), #\=(Expr, 0, B)) :-
    !,
    list_to_expr(List, Expr).
tr_fd_arith_out_(fd_re(X, Y), X #\= Y) :-
    !.
tr_fd_arith_out_(fd_qeq(X, Y, Z), X * Y #= Z) :-
    !.
tr_fd_arith_out_(ge(List, B, _), #>=(Expr, 0, B)) :-
    !,
    list_to_expr(List, Expr).
tr_fd_arith_out_(gec(X, K, Y, C), Expr #>= NC) :-
    !,
    next_elem(K*Y, X, Expr),
    NC is -C.
tr_fd_arith_out_(gec_ent(X, K, Y, C, B, _), #>=(E, N, B)) :-
    !,
    tr_fd_arith_out_(gec(X, K, Y, C), G),
    G = (E #>= N).
tr_fd_arith_out_(qeq(X, Y, Z), X * Y #= Z) :-
    !.
tr_fd_arith_out_(qeqsquare(X, Y,_Susp), X^2 #= Y) :-
    !.
tr_fd_arith_out_(eq_ent(X, Y, B), #=(X, Y, B)) :-
    !.
tr_fd_arith_out_(eq(List, B), #=(Expr, 0, B)) :-
    !,
    list_to_expr(List, Expr).

:- mode tr_fd_arith_out_dummy(+, -).
tr_fd_arith_out_dummy(fd_eq(_List), ... #= 0).
tr_fd_arith_out_dummy(fd_eq(_List, _B), #=(..., 0, ...)).
tr_fd_arith_out_dummy(fd_ge(_List), ... #>= 0).
tr_fd_arith_out_dummy(fd_ge(_List, _B), #>=(..., 0, ...)).
tr_fd_arith_out_dummy(fd_gec(_X, _K, _Y, _C, _D), ... #>= ...).
tr_fd_arith_out_dummy(fd_gec_ent(_X, _K, _Y, _C, _D, _Bo), #>=(... + ..., ..., ...)).
tr_fd_arith_out_dummy(fd_ineq(_List), ... #\= 0).
tr_fd_arith_out_dummy(fd_ineq(_List, _B), #\=(..., 0, ...)).
tr_fd_arith_out_dummy(fd_re(_X, _Y), ... #\= ...).
tr_fd_arith_out_dummy(fd_qeq(_X, _Y, _Z), ... * ... #= ...).
tr_fd_arith_out_dummy(ge(_List, _B, _), #>=(..., 0, ...)).
tr_fd_arith_out_dummy(gec(_X, _K, _Y, _C), ... #>= ...).
tr_fd_arith_out_dummy(gec_ent(_X, _K, _Y, _C, _B, _), #>=(..., ..., ...)).
tr_fd_arith_out_dummy(qeq(_X, _Y, _Z), ... * ... #= ...).
tr_fd_arith_out_dummy(qeqsquare(_X, _Y,_Susp), ... ^2 #= ...).
tr_fd_arith_out_dummy(eq_ent(_X, _Y, _B), #=(..., ..., ...)).
tr_fd_arith_out_dummy(eq(_List, _B), #=(..., 0, ...)).

list_to_expr([F], Expr) :-
    !,
    first_elem(F, Expr).
list_to_expr([F|T], Expr) :-
    first_elem(F, E0),
    next_to_expr(T, E0, Expr).

next_to_expr([H], E0, Expr) :-
    !,
    next_elem(H, E0, Expr).
next_to_expr([H|T], E0, Expr) :-
    next_elem(H, E0, E1),
    next_to_expr(T, E1, Expr).

:- mode first_elem(?, -).
first_elem(1*X, Y) :- -?-> !, Y = X.
first_elem(-1*X, Y) :- -?-> !, Y = -X.
first_elem(V, V).

:- mode next_elem(+, ?, -).
next_elem(1*X, Prev, Prev + X) :- !.
next_elem(-1*X, Prev, Prev - X) :- !.
next_elem(K*X, Prev, Prev + K*X) :-
    K > 0,
    !.
next_elem(K*X, Prev, Prev - K1*X) :-
    K < 0,
    !,
    K1 is -K.
next_elem(K, Prev, G) :-
    nonvar(K),
    !,
    (K > 0 ->
	G = Prev + K
    ;
	K1 is -K,
	G = Prev - K1
    ).
next_elem(V, Prev, Prev + V).


call_list([]).
call_list([Goal|List]) :-
    call(Goal),
    call_list(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Auxiliary Visible Predicates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

linear_term_range(Term, Min, Max) :-
    linear_term_range_only(Term, Res, TMin, TMax, _, _),
    Res = 5,
    TMin = Min,
    TMax = Max.

term_to_linear(Term, List) :-
    conv_expr(Term, Val, linear, _, _),
    linearize(Val, List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Arithmetic Constraints 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A #= B :-
    is_element(A),
    !,
    eq_en(A, B).
A #= B :-
    conv_pred(A #= B, run, List, []),
    !,
    call_list(List).
A #= B :-
    arith_functor(A),
    !,
    fderror(5, A #= B).
A #= B :-
    arith_functor(B),
    !,
    fderror(5, A #= B).
A #= B :-
    fderror(4, A #= B).

eq_en(A, B) :-
    is_element(B),
    !,
    A = B.
eq_en(A, B) :-
    conv_pred(A #= B, run, List, []),
    !,
    call_list(List).
eq_en(A, B) :-
    arith_functor(B),
    !,
    fderror(5, A #= B).
eq_en(A, B) :-
    fderror(4, A #= B).

A ## B :-
    A #\= B.

A #\= B :-
    var(A),
    !,
    ne_vn(A, B).
A #\= B :-
    atomic(A),
    !,
    ne_en(A, B).
A #\= B :-
    compound(A),
    ground(A),
    not arith_functor(A),
    !,
    ne_en(A, B).
A #\= B :-
    conv_pred(A #\= B, run, List, []),
    !,
    A \== B,
    call_list(List).
A #\= B :-
    arith_functor(A),
    !,
    fderror(5, A #\= B).
A #\= B :-
    arith_functor(B),
    !,
    fderror(5, A #\= B).
A #\= B :-
    fderror(4, A #\= B).

ne_vn(A, B) :-
    var(B),
    !,
    A \== B,
    make_suspension(A #\= B, 2, Susp),
    insert_suspension([A|B], Susp, inst of suspend, suspend).
ne_vn(A, B) :-
    atomic(B),
    !,
    fd_re(A, B).
ne_vn(A, B) :-
    compound(B),
    ground(B),
    not arith_functor(B),
    !,
    fd_re(A, B).
ne_vn(A, B) :-
    conv_pred(A #\= B, run, List, []),
    !,
    A \== B,
    call_list(List).
ne_vn(A, B) :-
    arith_functor(B),
    !,
    fderror(5, A #\= B).
ne_vn(A, B) :-
    fderror(4, A #\= B).

ne_en(A, B) :-
    var(B),
    !,
    fd_re(B, A).
ne_en(A, B) :-
    atomic(B),
    !,
    A \== B.
ne_en(A, B) :-
    compound(B),
    ground(B),
    not arith_functor(B),
    !,
    A \== B.
ne_en(A, B) :-
    conv_pred(A #\= B, run, List, []),
    !,
    A \== B,
    call_list(List).
ne_en(A, B) :-
    arith_functor(B),
    !,
    fderror(5, A #\= B).
ne_en(A, B) :-
    fderror(4, A #\= B).

A #>= B :-
    conv_pred(A #>= B, run, List, []),
    call_list(List).

A #> B :-
    conv_pred(A #> B, run, List, []),
    call_list(List).

A #< B :-
    conv_pred(A #< B, run, List, []),
    call_list(List).

A #=< B :-
    conv_pred(B #>= A, run, List, []),
    call_list(List).

A #<= B :-
    conv_pred(B #>= A, run, List, []),
    call_list(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Entailment Arithmetic Constraints 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#=(A, B, Bool) :-
    is_element(A),
    !,
    eq_en(A, B, Bool).
#=(A, B, Bool) :-
    conv_pred(#=(A, B, Bool), run, List, []),
    !,
    call_list(List).
#=(A, B, Bool) :-
    arith_functor(A),
    !,
    fderror(5, #=(A, B, Bool)).
#=(A, B, Bool) :-
    arith_functor(B),
    !,
    fderror(5, #=(A, B, Bool)).
#=(A, B, Bool) :-
    fderror(4, #=(A, B, Bool)).

eq_en(A, B, Bool) :-
    is_element(B),
    !,
    fd_eqe(A, B, Bool).
eq_en(A, B, Bool) :-
    conv_pred(#=(A, B, Bool), run, List, []),
    !,
    call_list(List).
eq_en(A, B, Bool) :-
    arith_functor(B),
    !,
    fderror(5, #=(A, B, Bool)).
eq_en(A, B, Bool) :-
    fderror(4, #=(A, B, Bool)).

##(A, B, Bool) :-
    #\=(A, B, Bool).

#\=(A, B, Bool) :-
    Bool :: 0..1,
    BoolN #= 1 - Bool,
    #=(A, B, BoolN).

#>=(A, B, Bool) :-
    #>=(A, B, Bool).

#>(A, B, Bool) :-
    #>(A, B, Bool).

#=<(A, B, Bool) :-
    #=<(A, B, Bool).

#<=(A, B, Bool) :-
    #<=(A, B, Bool).

#<(A, B, Bool) :-
    #<(A, B, Bool).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Boolean Constraints 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A #/\ B :-
    conv_pred(A #/\ B, run, List, []),
    call_list(List).

A #\/ B :-
    A #\/ B.

A #=> B :-
    A #=> B.

A #<=> B :-
    A #<=> B.

#\+ A :-
    #\+ A.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Boolean Entailment Constraints 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#/\(A, B, Bool) :-
    #/\(A, B, Bool).

#\/(A, B, Bool) :-
    #\/(A, B, Bool).

#=>(A, B, Bool) :-
    #=>(A, B, Bool).

#<=>(A, B, Bool) :-
    #<=>(A, B, Bool).

#\+(A, Bool) :-
    #\+(A, Bool).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Low Level Implementation of Arithmetic Constraints 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fd_eval(Goal) :-
    call(Goal).

isd(Bool, Expr) :-
    conv_bool_expr(Expr, Bool, run, List, []),
    call_list(List).

%
% equality
%
fd_eq(Term) :-
    linear_term_range_eq(Term, Res, Min, Max, NewTerm, _),
    handle_eq(NewTerm, Res, Min, Max).

:- mode handle_eq(+, ++, ++, ++).
handle_eq(Term, 0, _, _) :-			% take maximum
    make_extreme(Term, max).
handle_eq(_, 1, _, _).				% solved
handle_eq(Term, 2, _, _) :-
    make_extreme(Term, min).
handle_eq(_, 3, _, _) :-
    fail.
handle_eq(Term, 4, Min, Max) :-			% only one variable left
    lt_maxmin(Term, Min, Max),
    fd_eq(Term),
    wake.
handle_eq(Term, 5, _, _) :-			% no updates
    make_suspension(fd_eq(Term), 3, Susp),
    ex_insert_suspension(Term, Susp, 1).
handle_eq(Term, 6, _, _) :-			% type error
    tr_fd_arith_out_(fd_eq(Term), G),
    fderror(5, G).
handle_eq(Term, 7, Min, Max) :-			% update
    lt_maxmin(Term, Min, Max),
    wake,
    fd_eq(Term).
handle_eq(Term, 8, Min, Max) :-			% update
    lt_maxmin(Term, Min, Max),
    wake,
    fd_eq(Term).
handle_eq(Term, 9, _, _) :-			% not linearized
    unlinearize(Term, T),
    T #= 0.
handle_eq(_, 10, X, X).				% X #= Y

lt_maxmin([], _, _).
lt_maxmin([H|T], Min, Max) :-
    lt_test(H, Min, Max),
    lt_maxmin(T, Min, Max).

%
% inequality
%
fd_ge(Term) :-
    linear_term_range_ge(Term, Res, K, Max, NewTerm, Offset),
    handle_ge(NewTerm, Res, Max, Offset, K).

:- mode handle_ge(+, ++, ++, ++, ++).
handle_ge(Term, 0, _, _, _) :-			% take maximum
    make_extreme(Term, max).
handle_ge(_, 1, _, _, _).			% solved
handle_ge(_, 2, _, _, _).			% solved
handle_ge(_, 3, _, _, _).			% solved
handle_ge(Term, 4, Max, _, _) :-		% only one variable left
    lt_min(Term, Max),
    wake.
handle_ge(Term, 5, _, _, _) :-			% no updates
    make_suspension(fd_ge(Term), 3, Susp),
    ex_insert_suspension(Term, Susp, 0).
handle_ge(Term, 6, _, _, _) :-			% type error
    tr_fd_arith_out_(fd_ge(Term), G),
    fderror(5, G).
handle_ge(Term, 7, _, _, _) :-			% no updates
    make_suspension(fd_ge(Term), 3, Susp),
    ex_insert_suspension(Term, Susp, 0).
handle_ge(Term, 8, Max, _, _) :-		% update
    lt_min(Term, Max),
    fd_ge(Term).
handle_ge(Term, 9, _, _, _) :-			% not linearized
    unlinearize(Term, T),
    fd_eval(T #>= 0).
handle_ge(X, 10, Y, C, K) :-			% simple relation
    gec(X, K, Y, C).

lt_min([], _).
lt_min([H|T], Max) :-
    lt_test(H, Max, Max),
    lt_min(T, Max).

/* X + K*Y + C >= D */
fd_gec(X, K, Y, C, D) :-
   gec_start(X, K, Y, C, D, E, Res),
   gec_res(X, K, Y, E, Res).

gec(X, K, Y, C) :-
   gec_comp(X, K, Y, C, Res),
   gec_res(X, K, Y, C, Res).

:- mode gec_res(?, ++, ?, ++, ++).
gec_res(X, K, Y, C, 0) :-			% no change
    gec_delay(X, K, Y, C).
gec_res(X, K, Y, C, 1) :-			% change
    gec_delay(X, K, Y, C),
    wake.
gec_res(_, _, _, _, 2) :-			% solved with modifs
    wake.
gec_res(_, _, _, _, 5).				% solved
gec_res(X, K, Y, C, 6) :-			% not simplified
    (var(X), var(Y), var(C) ->			% otherwise it would loop
	fd_ge([1*X, K*Y, 1*C])
    ;
    conv_pred(X+K*Y+C #>= 0, run, List, []) ->
	call_list(List)
    ;
	tr_fd_arith_out_(gec(X, K, Y, C), G),
	fderror(5, G)
    ).
gec_res(X, K, Y, C, 7) :-			% not simplified
    (var(X), var(Y), var(C) ->
	fd_ge([1*X, K*Y, -1*C])
    ;
    conv_pred(X+K*Y-C #>= 0, run, List, []) ->
	call_list(List)
    ;
	tr_fd_arith_out_(gec(X, K, Y, -C), G),
	fderror(5, G)
    ).

gec_delay(X, K, Y, C) :-
    make_suspension(gec(X, K, Y, C), 2, Susp),
    gec_insert_suspension(X, K, Y, Susp).


fd_re(Var{fd:(fd with [])}, Value) :-
    -?->
    !,
    remove_element(Var, Value, Res),
    del_res(Var, Value, Res).
fd_re(Var, Value) :-
    var(Var),
    !,
    make_suspension(Var #\= Value, 3, Susp),
    insert_suspension(Var, Susp, constrained of suspend, suspend).
fd_re(A, B) :-					% may be when expanded
    fd_eval(A #\= B).

:- mode del_res(?, ?, ++).
del_res(_, _, 2) :-				% update
    wake.
del_res(_, _, 5).				% not in the domain or inst
del_res(A, B, 6) :-
    fderror(5, A #\= B).
    

% The real inequality predicate. It is woken when there is at most
% one variable in its arguments.
fd_ineq(L) :-
    ineq_test(L, Res, Var, Val),
    ineq_res(L, Res, Var, Val).

:- mode ineq_res(+, ++, ?, ++).
ineq_res(L, 0, Var1, Var2) :-		% redelay
    make_suspension(fd_ineq(L), 2, Susp),
    insert_suspension([Var1|Var2], Susp, inst of suspend, suspend).
ineq_res(_, 2, _, _) :-			% update
    wake.
ineq_res(_, 5, _, _).			% not in the domain or inst
ineq_res(L, 6, _, _) :-			% bound later
    tr_fd_arith_out_(ineq(L), G),
    fderror(5, G).
ineq_res(L, 9, _, _) :-			% not linearized
    unlinearize(L, Expr),
    fd_eval(Expr #\= 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Low Level Implementation of Entailment Arithmetic Constraints 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% entailment of #=
%
% variables and simple elements
fd_eqe(A{fd:(fd with domain:dom([0, 1], 2))}, B, 1) :-
    -?->
    !,
    A = B.
fd_eqe(A, B, Bool) :-
    Bool :: 0..1,
    eq_ent(A, B, Bool).

eq_ent(A, B, 0) :-
    -?->
    !,
    A #\= B.
eq_ent(A, B, 1) :-
    -?->
    !,
    A #= B.
eq_ent(A, B, Bool) :-
    (A == B ->
	Bool = 1
    ;
	eq_ent_test(A, B, Bool)
    ).

eq_ent_test(A{fd:(fd with domain:D)}, B, Bool) :-
    -?->
    !,
    eq_ent_dom(A, B, Bool, D).
eq_ent_test(A, B, Bool) :-
    var(A),
    !,
    make_suspension(eq_ent(A, B, Bool), 3, Susp),
    insert_suspension(A, Susp, constrained of suspend, suspend).
eq_ent_test(A, B, Bool) :-
    eq_ent_el(A, B, Bool).

eq_ent_dom(A, B{fd:(fd with domain:DB)}, Bool, DA) :-
    -?->
    !,
    (dom_intersection(DA, DB, _, _) ->
	make_suspension(eq_ent(A, B, Bool), 3, Susp),
	insert_suspension([A,B|Bool], Susp, any of fd, fd),
	insert_suspension([A|B], Susp, bound of suspend, suspend)
    ;
	Bool = 0
    ).
eq_ent_dom(A, B, Bool, _) :-
    var(B),
    !,
    make_suspension(eq_ent(A, B, Bool), 3, Susp),
    insert_suspension([A|B], Susp, constrained of suspend, suspend).
eq_ent_dom(A, B, Bool, DA) :-
    (dom_check_in(B, DA) ->
	make_suspension(eq_ent(A, B, Bool), 3, Susp),
	insert_suspension([A|Bool], Susp, any of fd, fd)
    ;
	Bool = 0
    ).

eq_ent_el(A, B{fd:(fd with domain:DB)}, Bool) :-
    -?->
    !,
    (dom_check_in(A, DB) ->
	make_suspension(eq_ent(A, B, Bool), 3, Susp),
	insert_suspension([B|Bool], Susp, any of fd, fd)
    ;
	Bool = 0
    ).
eq_ent_el(A, B, Bool) :-
    var(B),
    !,
    make_suspension(eq_ent(A, B, Bool), 3, Susp),
    insert_suspension(B, Susp, constrained of suspend, suspend).
eq_ent_el(A, B, Bool) :-
    (A == B ->
	Bool = 1
    ;
	Bool = 0
    ).

fd_eq(Term, Bool) :-
    Bool :: 0..1,
    eq(Term, Bool).

eq(Term, 1) :-
    -?->
    fd_eq(Term).
eq(Term, 0) :-
    -?->
    fd_ineq(Term).
eq(Term, Bool) :-
    var(Bool),
    (linear_term_range_eq(Term, Res, Min, Max, NewTerm, _) ->
	handle_eq_ent(NewTerm, Res, Min, Max, Bool)
    ;
	Bool = 0
    ).

:- mode handle_eq_ent(+, ++, ++, ++, ?).
handle_eq_ent(_, 1, _, _, 1) :- !.				% solved
handle_eq_ent(_, 3, _, _, 0) :- !.
handle_eq_ent(Term, 6, _, _, Bool) :-			% type error
    !,
    tr_fd_arith_out_(fd_eq(Term, Bool), G),
    fderror(5, G).
handle_eq_ent(Term, 9, _, _, Bool) :-			% not linearized
    !,
    unlinearize(Term, T),
    #=(T, 0, Bool).
handle_eq_ent(_, 10, X, Y, Bool) :-
    !,
    NewTerm = [1*X,-1*Y],
    handle_eq_ent(NewTerm, 5, 0, 0, Bool).
handle_eq_ent(Term, _, _, _, Bool) :-
    make_suspension(eq(Term, Bool), 3, Susp),
    ex_insert_suspension(Term, Susp, 1),
    insert_suspension(Bool, Susp, inst of suspend, suspend).

% entailment of inequality; just use equality and reverse boolean
fd_ineq(Term, Bool) :-
    Bool :: [0..1],
    Bool + Rev #= 1,
    fd_eq(Term, Rev).

%
% entailment of #>=
%
fd_ge(Term, B) :-
    B :: 0..1,
    ge(Term, B, _Susp).

:- demon ge/3.
ge(Term, 1, Susp) :-
    -?->
    kill_suspension(Susp),
    fd_ge(Term).
ge(Term, 0, Susp) :-
    -?->
    kill_suspension(Susp),
    negate(Term, NewTerm),
    fd_ge([-1|NewTerm]).
ge(Term, B, Susp) :-
    var(B),
    (linear_term_range_ge(Term, Res, K, Max, NewTerm, Offset) ->
	handle_ge_ent(NewTerm, Term, Res, Max, Offset, K, B, Susp)
    ;
	kill_suspension(Susp),
	B = 0
    ).

negate([], []).
negate([K*V|L1], T) :-
    -?->
    !,
    K1 is -K,
    T = [K1*V|T1],
    negate(L1, T1).
negate([C|L], [C1|T]) :-
    C1 is -C,
    negate(L, T).

:- mode handle_ge_ent(+, +, ++, ++, ++, ++, ?, ?).
handle_ge_ent(_, _, 1, _, _, _, 1, Susp) :- !,		% solved
    kill_suspension(Susp).
handle_ge_ent(_, _, 2, _, _, _, 1, Susp) :- !,		% solved
    kill_suspension(Susp).
handle_ge_ent(_, _, 3, _, _, _, 1, Susp) :- !,		% solved
    kill_suspension(Susp).
handle_ge_ent(Term, _, 6, _, _, _, _, Susp) :-		% type error
    !,
    kill_suspension(Susp),
    tr_fd_arith_out_(fd_ge(Term), G),
    fderror(5, G).
handle_ge_ent(Term, _, 9, _, _, _, B, Susp) :-		% not linearized
    !,
    kill_suspension(Susp),
    unlinearize(Term, T),
    fd_eval(#>=(T, 0, B)).
handle_ge_ent(X, _, 10, Y, C, K, B, Susp) :-		% simple relation
    !,
    kill_suspension(Susp),
    gec_ent(X, K, Y, C, B, _Susp).
handle_ge_ent(Term, OldTerm, _, _, _, _, B, Susp) :-		% no updates
    ( var(Susp) ->
	suspend(ge(Term, B, Susp), 3, [Term->min, Term->max, B->inst], Susp)
    ;
       	(Term \== OldTerm ->    % Term has changed, suspension needs update
	    get_suspension_data(Susp, goal, Goal),
	    setarg(1, Goal, Term)
        ; true
        )  
    ).


fd_gec_ent(X, K, Y, C, D, 0) :-
    -?->
    X + K*Y + C #< D.
fd_gec_ent(X, K, Y, C, D, 1) :-
    -?->
    fd_gec(X, K, Y, C, D).
fd_gec_ent(X, K, Y, C, D, Bool) :-
    var(Bool),
    Bool :: 0..1,
    gec_ent_start(X, K, Y, C, D, E, Res),
    gec_ent_res(X, K, Y, E, Bool, Res, _Susp).

:- demon gec_ent/6.
gec_ent(X, K, Y, C, 1, Susp) :-
    -?->
    kill_suspension(Susp),
    gec(X, K, Y, C).
gec_ent(X, K, Y, C, 0, Susp) :-
    -?->
    kill_suspension(Susp),
    C1 is -(C + 1),
    K1 is -K,
    fd_ge([C1, -1*X, K1*Y]).
gec_ent(X, K, Y, C, B, Susp) :-
    var(B),
    gec_test(X, K, Y, C, Res),
    gec_ent_res(X, K, Y, C, B, Res, Susp).

:- mode gec_ent_res(?, ++, ?, ++, ?, ++, ?).
gec_ent_res(X, K, Y, C, B, 0, Susp) :-			% delay
    ( var(Susp) ->
	suspend(gec_ent(X, K, Y, C, B, Susp), 2,
		[X-Y->min, X-Y->max, B->inst], Susp)	%%% what about C?
    ;
    	true
    ).
gec_ent_res(_, _, _, _, B, 5, Susp) :-			% true
    kill_suspension(Susp),
    B = 1.
gec_ent_res(_, _, _, _, B, 11, Susp) :-			% false
    kill_suspension(Susp),
    B = 0.
gec_ent_res(X, K, Y, C, B, 6, Susp) :-			% error
    kill_suspension(Susp),
    (var(X), var(Y), var(C) ->
	fd_ge([1*X, K*Y, 1*C], B)
    ;
    conv_pred(#>=(X+K*Y+C, 0, B), run, List, []) ->
	call_list(List)
    ;
	tr_fd_arith_out_(gec(X, K, Y, C, B), G),
	fderror(5, G)
    ).
gec_ent_res(X, K, Y, C, B, 7, Susp) :-			% error
    kill_suspension(Susp),
    (var(X), var(Y), var(C) ->
	fd_ge([1*X, K*Y, -1*C], B)
    ;
    conv_pred(#>=(X+K*Y-C, 0, B), run, List, []) ->
	call_list(List)
    ;
	tr_fd_arith_out_(gec(X, K, Y, -C, B), G),
	fderror(5, G)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Y * Z = X
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fd_qeq(Y, Z, X) :-
    check_dom(X),
    check_dom(Y),
    check_dom(Z),
    qeq(Y, Z, X).

qeq(Y, Z, X) :-
    dvar_domain(X, DomX),
    dvar_domain(Y, DomY),
    dvar_domain(Z, DomZ),
    dom_range(DomX, MinX, MaxX),
    dom_range(DomY, MinY, MaxY),
    dom_range(DomZ, MinZ, MaxZ),
    % propagate to X
    % update the maximum
    (MinY >= 0 ->
	(MinZ >= 0 ->
	    qequpmax(MaxY, MaxZ, MaxX, X, Upd),
	    qequpmin(MinY, MinZ, MinX, X, Upd)
	;
	MaxZ >= 0 ->
	    qequpmax(MaxY, MaxZ, MaxX, X, Upd),
	    qequpmin(MaxY, MinZ, MinX, X, Upd)
	;
	    qequpmax(MinY, MaxZ, MaxX, X, Upd),
	    qequpmin(MaxY, MinZ, MinX, X, Upd)
	)
    ;
    MaxY >= 0 ->
	(MinZ >= 0 ->
	    qequpmax(MaxY, MaxZ, MaxX, X, Upd),
	    qequpmin(MinY, MaxZ, MinX, X, Upd)
	;
	MaxZ >= 0 ->
	    (MaxY*MaxZ > MinY*MinZ ->
		qequpmax(MaxY, MaxZ, MaxX, X, Upd)
	    ;
		qequpmax(MinY, MinZ, MaxX, X, Upd)
	    ),
	    (MaxY*MinZ < MinY*MaxZ ->
		qequpmin(MaxY, MinZ, MinX, X, Upd)
	    ;
		qequpmin(MinY, MaxZ, MinX, X, Upd)
	    )
	;
	    qequpmax(MinY, MinZ, MaxX, X, Upd),
	    qequpmin(MaxY, MinZ, MinX, X, Upd)
	)
    ;
	(MinZ >= 0 ->
	    qequpmax(MaxY, MinZ, MaxX, X, Upd),
	    qequpmin(MinY, MaxZ, MinX, X, Upd)
	;
	MaxZ >= 0 ->
	    qequpmax(MinY, MinZ, MaxX, X, Upd),
	    qequpmin(MinY, MaxZ, MinX, X, Upd)
	;
	    qequpmax(MinY, MinZ, MaxX, X, Upd),
	    qequpmin(MaxY, MaxZ, MinX, X, Upd)
	)
    ),
    % propagate from X
    qeqdownmin(MinX, MaxX, MinZ, MaxZ, MinY, Y, Upd),
    qeqdownmin(MinX, MaxX, MinY, MaxY, MinZ, Z, Upd),
    qeqdownmax(MinX, MaxX, MinZ, MaxZ, MaxY, Y, Upd),
    qeqdownmax(MinX, MaxX, MinY, MaxY, MaxZ, Z, Upd),
    (nonvar(Upd) ->
	qeq(Y, Z, X)
    ;
	Vars = p(X, Y, Z),
	term_variables(Vars, VL),
	length(VL, N),
	(N = 3 ->
	    make_suspension(qeq(Y, Z, X), 4, Susp),
	    insert_suspension(Vars, Susp, bound of suspend, suspend),
	    insert_suspension(Vars, Susp, min of fd, fd),
	    insert_suspension(Vars, Susp, max of fd, fd)
	;
	N = 2 ->
	    ntimes2(Y, Z, X)
	;
	    ntimes1(Y, Z, X)
	),
	wake
    ).

qequpmin(M1, M2, MinX, X, Upd) :-
    MiX is min(100000,M1*M2),
    (MiX > MinX ->
	dvar_remove_smaller(X, MiX),
	Upd = 1
    ;
	true
    ).

qequpmax(M1, M2, MaxX, X, Upd) :-
    MaX is max(-100000,M1*M2),
    (MaX < MaxX ->
	dvar_remove_greater(X, MaX),
	true,
	Upd = 1
    ;
	true
    ).

qeqdownmax(MinX, MaxX, MinZ, MaxZ, MaxY, Y, Upd) :-
    (MinZ >= 0 ->
	(MaxX >= 0 ->
	    qeqdownmax(MaxX, MinZ, MaxY, Y, Upd)
	;
	    qeqdownmax(MaxX, MaxZ, MaxY, Y, Upd)
	)
    ;
    MaxZ >= 0 ->
	(MinX >= 0 ->
	    qeqdownmax(MaxX, 1, MaxY, Y, Upd)
	;
	MaxX >= 0 ->
	    Mx1 is max(MaxX, -MinX),
	    qeqdownmax(Mx1, 1, MaxY, Y, Upd)
	;
	    qeqdownmax(MinX, -1, MaxY, Y, Upd)
	)
    ;
    MinX >= 0 ->
	qeqdownmax(MinX, MinZ, MaxY, Y, Upd)
    ;
	qeqdownmax(MinX, MaxZ, MaxY, Y, Upd)
    ).

qeqdownmin(MinX, MaxX, MinZ, MaxZ, MinY, Y, Upd) :-
    (MinZ >= 0 ->
	(MinX >= 0 ->
	    qeqdownmin(MinX, MaxZ, MinY, Y, Upd)
	;
	    qeqdownmin(MinX, MinZ, MinY, Y, Upd)
	)
    ;
    MaxZ >= 0 ->
	(MinX >= 0 ->
	    qeqdownmin(MaxX, -1, MinY, Y, Upd)
	;
	MaxX >= 0 ->
	    Mx1 is min(-MaxX, MinX),
	    qeqdownmin(Mx1, -1, MinY, Y, Upd)
	;
	    qeqdownmin(MinX, 1, MinY, Y, Upd)
	)
    ;
    MaxX >= 0 ->
	qeqdownmin(MaxX, MaxZ, MinY, Y, Upd)
    ;
	qeqdownmin(MaxX, MinZ, MinY, Y, Upd)
    ).

qeqdownmax(MaxX, MinY, MaxZ, Z, Upd) :-
    (MaxX < MinY*MaxZ ->
	(sgn(MaxX) =:= sgn(MinY) ->
	    NM is MaxX//MinY
	;
	    NM is (MaxX + MinY - 1)//MinY
	),
	(NM < MaxZ ->
	    dvar_remove_greater(Z, NM),
	    Upd = 1
	;
	    true
	)
    ;
	true
    ).

qeqdownmin(MinX, MaxY, MinZ, Z, Upd) :-
    (MinX > MaxY*MinZ ->
	(sgn(MinX) =:= sgn(MaxY) ->
	    NM is (MinX + MaxY - 1)//MaxY
	;
	    NM is MinX//MaxY
	),
	(NM > MinZ ->
	    dvar_remove_smaller(Z, NM),
	    Upd = 1
	;
	    true
	)
    ;
	true
    ).

% Y*Y = X
qeqsquare(Y, X ) :-
    fd_abs(Y,AY),
    suspend(qeqsquare(AY,X,Susp),4,[[AY|X]->min,[AY|X]->max],Susp),
    qeqsquare(AY,X,Susp).

	% Y is positive
:- demon qeqsquare/3.
qeqsquare(Y, X ,Susp) :-
    ( X==Y ->
    	X::0..1,
    	kill_suspension(Susp)
    ; integer(X) ->
	PY is fix(sqrt(X)),
	X is PY*PY,
	NY is -PY,
	Y:: [NY,PY],
    	kill_suspension(Susp)
    ; integer(Y) ->
	X is Y*Y,
    	kill_suspension(Susp)
    ;
	% propagate to X
	fd_util:dvar_range(Y, MinY, MaxY),
	fd_util:dvar_range(X, MinX, MaxX),
	NewMinX is MinY*MinY, % may have created a bignum
	(NewMinX > MinX ->
	    dvar_remove_smaller(X, NewMinX),
	    Min = NewMinX
	;
	    Min = MinX
	),
	NewMaxX is MaxY*MaxY,
	(NewMaxX < MaxX -> % may have created a bignum
	    dvar_remove_greater(X, NewMaxX),
	    Max = NewMaxX
	;
	    Max = MaxX
	),
	% propagate to Y
	MinS is fix(ceiling(sqrt(Min))),    % round up
	MaxS is fix(sqrt(Max)),		% round down
	dvar_remove_smaller(Y, MinS),
	dvar_remove_greater(Y, MaxS)
    ).



ntimes2(0, _, C) :-
    -?->
    !,
    C = 0.
ntimes2(_, 0, C) :-
    -?->
    !,
    C = 0.
ntimes2(1, B, C) :-
    -?->
    !,
    C = B.
ntimes2(A, 1, C) :-
    -?->
    !,
    C = A.
ntimes2(X, X, Y) :-
    -?->
    !,
    qeqsquare(X,Y).
ntimes2(X, Y, X) :-
    -?->
    !,
    ( X #= 0 #\/ Y #= 1).
ntimes2(Y, Z, X) :-
    integer(Y),
    !,
    Y*Z #= X.
ntimes2(Y, Z, X) :-
    integer(Z),
    !,
    Y*Z #= X.
ntimes2(Y, X, X) :-
    -?->
    !,
    ( X #= 0 #\/ Y #= 1).
ntimes2(Y, Z, X) :-
    Goal = qeq(Y, Z, X),
    make_suspension(Goal, 4, Susp),
    insert_suspension(Goal, Susp, bound of suspend, suspend),
    insert_suspension(Goal, Susp, min of fd, fd),
    insert_suspension(Goal, Susp, max of fd, fd).

% we know there is only one variable
% but some of A,B,C may be the same variable
ntimes1(A, B, C) :-
    ( var(A) ->
	( var(B) ->
	    ( var(C) ->
		A::0..1
	    ; % C is ground
		S is sqrt(C),
		FS is fix(S),
		C is FS * FS,
		MFS is - FS,
		A::[MFS,FS]
    	    )
	; % B is ground
	    ( var(C) ->
	    	( B=1 ->
		    true
		;
		    A=0
		)
	    ;
	    	times(A,B,C)
	    )
	)
    ;
    	( var(B) ->
		( var(C) ->
		    ( A=1 ->
			true
		    ;
			B=0
		    )
		;
		    times(A,B,C)
		)
	;
	    	times(A,B,C)
	)
     ).
	    


check_dom(_{fd:(fd with [])}) :- -?-> !.
check_dom(Var) :- var(Var), !,
    default_domain(Var).
check_dom(_).

default_domain(V) :-
    V :: -10000000..10000000.


% We must skip them explicitly because call/1 is not properly traced (?)
/*
:- skipped
    #=  /2,
    #\= /2,
    ##  /2,
    #=  /3,
    #/\ /3,
    #>= /2,
    #>= /3.
    */

fd_abs(X,A) :-
	A #>= 0,
	suspend(abs(X,A,Susp),3,[X->any,A->min,A->max],Susp),
	abs(X,A,Susp).

:- demon abs/3.
abs(X,A,Susp):-
	fd_util:dvar_range(A,AMin,AMax),
	MAMax is -AMax,
	MAMin is -AMin,
	X::[MAMax..MAMin,AMin..AMax],
	fd_util:dvar_range(X,Xmin,Xmax),
	( Xmin >=0 ->
	    kill_suspension(Susp),
	    X=A
	; Xmax =< 0 ->
	    kill_suspension(Susp),
	    A #= -X
	;
	    dvar_domain(X,XDom),
	    dom_min_abs(XDom,XMinAbs),
	    XMaxAbs is max(-Xmin,Xmax),
	    dvar_remove_smaller(A,XMinAbs),
	    dvar_remove_greater(A,XMaxAbs),
	    (var(A),var(X) ->
	    	true
	    ;
	    	kill_suspension(Susp)
	    )
	).
		
dom_min_abs(dom(List,_), MinAbs) :-
        dom_min_abs(List, -10000000, MaxNeg, MinPos),
        MinAbs is min(-MaxNeg, MinPos).
        
    dom_min_abs([], MaxNeg, MaxNeg, MaxNeg).
    dom_min_abs([I|Is], MaxNeg0, MaxNeg, MinPos) :-
        ( I = From..To ->
            ( From >= 0 ->
                MaxNeg = MaxNeg0, MinPos = From
            ; To < 0 ->
                dom_min_abs(Is, To, MaxNeg, MinPos)
            ;
                MaxNeg = 0, MinPos = 0
            )
        ; I >= 0 ->
            MaxNeg = MaxNeg0, MinPos = I
        ;
            dom_min_abs(Is, I, MaxNeg, MinPos)
        ).





