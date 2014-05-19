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
% Copyright (C) 2001 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Warwick Harvey, Andrew Sadler, Andrew Cheadle, IC-Parc
% 
% END LICENSE BLOCK
%---------------------------------------------------------------------
%
% IC basic constraints module.
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Warwick Harvey, Andrew Sadler, Andrew Cheadle IC-Parc
%
%	Chunks of this module derived from or loosely based on code from
%	the FD module (written by Micha Meier) and the RIA module (written
%	by Joachim Schimpf and Stefano Novello).
%
% This module provides the basic constraints of the IC library, a combined
% finite domain and floating point interval propagation solver.  These
% constraints are $=, $=<, $>=, $<, $>, $\=, #=, #=<, #>=, #<, #>, #\=.
% We have aliases =:=, =<, etc for $=, $=<, etc.
% We still have aliases *=, *=<, as well (for lib(ria) compatibility).
%---------------------------------------------------------------------
%
% TODO:  (from RIA)
%
% - it might be useful to use timeouts for the propagation steps within squash
%
% - use less expensive, weaker squashing in locate/4
%
% - sin, cos, atan do not have reverse functions yet, e.g.
%   Y $= sin(X) does not propagate from Y to X.
%
% - Under some circumstances, piecewise_linear/3 does not propagate as tight
%   bounds as it could.  Specifically, if a Y bound coincides with an "open"
%   point on one side of a discontinuity, then if it has no other support
%   (the bound intersects no other segment, and the segment adjacent to the
%   open point is infeasible with respect to the bound), the bound could be
%   tightened (but is not).
%
%---------------------------------------------------------------------

:- module(ic_constraints).

%---------------------------------------------------------------------
%
% Set some compiler directives.
%

:- pragma(expand).
:- pragma(nodebug).


%---------------------------------------------------------------------
%
% Define the operators for this module.
%

:- export op(500, fx, (+-)).
:- export op(700, xfx, [$::, #::]).
:- export op(700, xfx, [$=, $>=, $=<, $<, $>, $\=]).
:- export op(700, xfx, [*=, *>=, *=<, *<, *>, *\=, iis]).
:- export op(750, fx, [neg]).
:- export op(760, yfx, [and]).
:- export op(770, yfx, [or]).
:- export op(780, yfx, [=>]).

%---------------------------------------------------------------------
%
% Imports and exports.
%

% Load the modules we depend upon.

:- use_module(ic_kernel).
:- use_module(linearize).

%
% We assume the required dynamic library (ic) loaded by ic_kernel, so just
% declare the bits we want.
%

:- external(ic_constraints_init/0, p_ic_constraints_init).
:- external(set_up_ic_con/3, p_set_up_ic_con).
:- external(make_bool/1, p_make_bool).
:- external(indomain_init/2, p_indomain_init).
:- b_external(indomain_try/2, p_indomain_try).
:- external(get_print_info/6, p_get_print_info).
:- external(ac_eq_init/3, p_ac_eq_init).

% Hack to get around not being able to declare external predicates demons.

:- external(dummy_prop_ic_con/1, p_prop_ic_con).
:- demon prop_ic_con/1.
prop_ic_con(Con) :-
	ic_event(ic_lin_prop),
	dummy_prop_ic_con(Con).

:- demon ac_eq_prop/4.
:- external(ac_eq_prop/4, p_ac_eq_prop).


% Don't ask me why this has to be down here instead of just after the
% declaration of ic_constraints_init/0.

:- ic_constraints_init.

%
% Export the user-level constraints.
%

:- export (::)/2, (*=)/2, (*>=)/2, (*=<)/2, (*\=)/2, (*<)/2, (*>)/2.
:- export (::)/3, (*=)/3, (*>=)/3, (*=<)/3, (*<)/3, (*>)/3, (*\=)/3.
:- export ($::)/2, ($=)/2, ($>=)/2, ($=<)/2, ($\=)/2, ($<)/2, ($>)/2.
:- export ($::)/3, ($=)/3, ($>=)/3, ($=<)/3, ($<)/3, ($>)/3, ($\=)/3.
:- export (#::)/3, (#=)/3, (#>=)/3, (#=<)/3, (#<)/3, (#>)/3, (#\=)/3.
:- export (#::)/2, (#=)/2, (#>=)/2, (#=<)/2, (#>)/2, (#<)/2, (#\=)/2.
:- export (neg)/2, (neg)/1.
:- export ('=>')/3, ('=>')/2.
:- export ('or')/3, ('or')/2.
:- export ('and')/3, ('and')/2.
:- export indomain/1, labeling/1, alldifferent/1, element/3.
:- export iis/2, biis/5.
:- export ac_eq/3.

%
% Export some macros which do compile-time processing of the constraints.
%
:- tool(tr_ic_constraint_in/2,tr_ic_constraint_in/3).
:- export tr_ic_constraint_in/2.

:- inline((*=)/2, tr_ic_constraint_in/2).
:- inline((*=)/3, tr_ic_constraint_in/2).
:- inline((*>=)/2, tr_ic_constraint_in/2).
:- inline((*>=)/3, tr_ic_constraint_in/2).
:- inline((*=<)/2, tr_ic_constraint_in/2).
:- inline((*=<)/3, tr_ic_constraint_in/2).
:- inline((*<)/2, tr_ic_constraint_in/2).
:- inline((*<)/3, tr_ic_constraint_in/2).
:- inline((*>)/2, tr_ic_constraint_in/2).
:- inline((*>)/3, tr_ic_constraint_in/2).
:- inline((*\=)/2, tr_ic_constraint_in/2).
:- inline((*\=)/3, tr_ic_constraint_in/2).
:- inline(($=)/2, tr_ic_constraint_in/2).
:- inline(($=)/3, tr_ic_constraint_in/2).
:- inline(($>=)/2, tr_ic_constraint_in/2).
:- inline(($>=)/3, tr_ic_constraint_in/2).
:- inline(($=<)/2, tr_ic_constraint_in/2).
:- inline(($=<)/3, tr_ic_constraint_in/2).
:- inline(($<)/2, tr_ic_constraint_in/2).
:- inline(($<)/3, tr_ic_constraint_in/2).
:- inline(($>)/2, tr_ic_constraint_in/2).
:- inline(($>)/3, tr_ic_constraint_in/2).
:- inline(($\=)/2, tr_ic_constraint_in/2).
:- inline(($\=)/3, tr_ic_constraint_in/2).
:- inline((#=)/2, tr_ic_constraint_in/2).
:- inline((#=)/3, tr_ic_constraint_in/2).
:- inline((#>=)/2, tr_ic_constraint_in/2).
:- inline((#>=)/3, tr_ic_constraint_in/2).
:- inline((#=<)/2, tr_ic_constraint_in/2).
:- inline((#=<)/3, tr_ic_constraint_in/2).
:- inline((#>)/2, tr_ic_constraint_in/2).
:- inline((#>)/3, tr_ic_constraint_in/2).
:- inline((#<)/2, tr_ic_constraint_in/2).
:- inline((#<)/3, tr_ic_constraint_in/2).
:- inline((#\=)/2, tr_ic_constraint_in/2).
:- inline((#\=)/3, tr_ic_constraint_in/2).
:- inline((or)/2, tr_ic_constraint_in/2).
:- inline((and)/2, tr_ic_constraint_in/2).
:- inline((=>)/2, tr_ic_constraint_in/2).
:- inline((neg)/1, tr_ic_constraint_in/2).
:- inline((or)/3, tr_ic_constraint_in/2).
:- inline((and)/3, tr_ic_constraint_in/2).
:- inline((=>)/3, tr_ic_constraint_in/2).
:- inline((neg)/2, tr_ic_constraint_in/2).

% the *... predicates are for ria-compatibility

:-tool('::'/2, '::_body'/3).
:-tool('::'/3, '::_body'/4).
:-tool('$::'/2, '$::_body'/3).
:-tool('$::'/3, '$::_body'/4).
:-tool('#::'/2, '#::_body'/3).
:-tool('#::'/3, '#::_body'/4).  % only allowed reified #:: for ints anyway

:-tool('*='/2, '*=_body'/3).
:-tool('*>='/2,'*>=_body'/3).
:-tool('*=<'/2,'*=<_body'/3).
:-tool('*<'/2, '*<_body'/3).
:-tool('*>'/2, '*>_body'/3).
:-tool('*\\='/2,'*\\=_body'/3).

:-tool('*='/3, '*=_body'/4).
:-tool('*>='/3,'*>=_body'/4).
:-tool('*=<'/3,'*=<_body'/4).
:-tool('*<'/3, '*<_body'/4).
:-tool('*>'/3, '*>_body'/4).
:-tool('*\\='/3,'*\\=_body'/4).

:-tool('$='/2, '*=_body'/3).
:-tool('$>='/2,'*>=_body'/3).
:-tool('$=<'/2,'*=<_body'/3).
:-tool('$<'/2, '*<_body'/3).
:-tool('$>'/2, '*>_body'/3).
:-tool('$\\='/2,'*\\=_body'/3).

:-tool('$='/3, '*=_body'/4).
:-tool('$>='/3,'*>=_body'/4).
:-tool('$=<'/3,'*=<_body'/4).
:-tool('$<'/3, '*<_body'/4).
:-tool('$>'/3, '*>_body'/4).
:-tool('$\\='/3,'*\\=_body'/4).

:-tool('#='/2, '#=_body'/3).
:-tool('#>='/2,'#>=_body'/3).
:-tool('#=<'/2,'#=<_body'/3).
:-tool('#<'/2, '#<_body'/3).
:-tool('#>'/2, '#>_body'/3).
:-tool('#\\='/2,'#\\=_body'/3).

:-tool('#='/3, '#=_body'/4).
:-tool('#>='/3,'#>=_body'/4).
:-tool('#=<'/3,'#=<_body'/4).
:-tool('#<'/3, '#<_body'/4).
:-tool('#>'/3, '#>_body'/4).
:-tool('#\\='/3,'#\\=_body'/4).

:-tool('or'/2,  'or_body'/3).
:-tool('and'/2, 'and_body'/3).
:-tool('=>'/2, '=>_body'/3).
:-tool(/('neg',1), 'neg_body'/2).

:-tool('or'/3,  'or_body'/4).
:-tool('and'/3, 'and_body'/4).
:-tool('=>'/3, '=>_body'/4).
:-tool(/('neg',2), 'neg_body'/3).


%
% Export the low-level constraints generated by compile-time constraint
% processing.
%

:- export make_bool/1.

:- export ic_lin_con/3.

:- export min/2, max/2.
:- export minlist/2, maxlist/2.

%
% Export some macros which cause delayed goals and compile-generated goals
% to be printed in a more user-friendly form.
%

:- export tr_ic_constraint_out/3.

:- export portray(make_bool/1, tr_ic_constraint_out/3, [goal]).
:- export portray(check_domain_demon/4, tr_ic_constraint_out/3, [goal]).

:- export portray(ic_lin_con/3, tr_ic_constraint_out/3, [goal]).
:- export portray(set_up_ic_con/3, tr_ic_constraint_out/3, [goal]).
:- export portray(prop_ic_con/1, tr_ic_constraint_out/3, [goal]).

:- export portray(indomain_init/2, tr_ic_constraint_out/3, [goal]).

:- export portray(element/6, tr_ic_constraint_out/3, [goal]).
:- export portray(ac_eq_prop/4, tr_ic_constraint_out/3, [goal]).


    % Unfortunately these transformations have to be done by ic_kernel, even
    % though it's this module that generates them in compiled code.
%:- export portray(ic_kernel:set_var_type/2, tr_ic_constraint_out/3, [goal]).
%:- export portray(ic_kernel:set_vars_type/2, tr_ic_constraint_out/3, [goal]).

    % These ones also need to be done by ic_kernel; however, it's not clear
    % that they should always be transformed to their ic_constraints
    % parents, and it's fairly clear what they mean...
%:- export portray(exclude/2, tr_ic_constraint_out/3, [goal]).
%:- export portray(exclude_range/3, tr_ic_constraint_out/3, [goal]).


% Piecewise linear constraint
:- export piecewise_linear/3.

% Nonlinear delayed goals stuff
:- export tr_op/2.

:- export portray(sub/3, tr_op/2, [goal]).
:- export portray(unop/4, tr_op/2, [goal]).
:- export portray(unop_function/4, tr_op/2, [goal]).
:- export portray(bi_unop/5, tr_op/2, [goal]).
:- export portray(binop/5, tr_op/2, [goal]).
:- export portray(binop_function/5, tr_op/2, [goal]).
:- export portray(binop_div/5, tr_op/2, [goal]).
:- export portray(ternop/5, tr_op/2, [goal]).
:- export portray(ternop_function/5, tr_op/2, [goal]).
:- export portray(bi_prop_mono_func/5, tr_op/2, [goal]).
:- export portray(bi_prop_sign_mono_func/5, tr_op/2, [goal]).
:- export portray(rpow/6, tr_op/2, [goal]).
:- export portray(interval_ge/3, tr_op/2, [goal]).
:- export portray(sumlist/3, tr_op/2, [goal]).


%
% Define some useful macros for conveniently converting RIA function names
% to numbers.
%

:- export tr_ria_name/2.

tr_ria_name(biis(Type, OpName, RevOpName, X, Y),
		biis(Type, OpNum, RevOpNum, X, Y)) :-
	tr_ria_unop(OpName, OpNum),
	( Type == mono_func ->
	    tr_ria_unop(RevOpName, RevOpNum)
	; Type == sign_mono_func ->
	    tr_ria_binop(RevOpName, RevOpNum)
	;
	    fail
	).
tr_ria_name(biis_prop_int(OpName, X, Z), biis_prop_int(OpNum, X, Z)) :-
	tr_ria_unop(OpName, OpNum).
tr_ria_name(bi_prop_mono_func(OpName, RevOpName, X, Y, S),
		bi_prop_mono_func(OpNum, RevOpNum, X, Y, S)) :-
	tr_ria_unop(OpName, OpNum),
	tr_ria_unop(RevOpName, RevOpNum).
tr_ria_name(bi_prop_sign_mono_func(OpName, RevOpName, X, Y, S),
		bi_prop_sign_mono_func(OpNum, RevOpNum, X, Y, S)) :-
	tr_ria_unop(OpName, OpNum),
	tr_ria_binop(RevOpName, RevOpNum).

:- local macro(biis/5, tr_ria_name/2, []).
:- local macro(biis_prop_int/3, tr_ria_name/2, []).
:- local macro(bi_prop_mono_func/5, tr_ria_name/2, []).
:- local macro(bi_prop_sign_mono_func/5, tr_ria_name/2, []).


% register the propagation stats event
:- ic_stat_register_event(ic_uni_prop,'unary operator propagations').
:- ic_stat_register_event(ic_bin_prop,'binary operator propagations').
:- ic_stat_register_event(ic_tern_prop,'ternary operator propagations').
:- ic_stat_register_event(ic_lin_create,'linear constraint creations').
:- ic_stat_register_event(ic_lin_prop,'linear constraint propagations').

%---------------------------------------------------------------------
%
% Miscellaneous useful predicates.
%

    %
    % convert_constant(A0, A)
    %	Converts a constant A0 to IC's internal representation A. (i.e.
    %	leaves integers as they are, but converts floats and rationals into
    %	breals).
    %
convert_constant(A0, A) :-
	( float(A0) ; rational(A0) ),
	!,
	A is breal(A0).
convert_constant(A0, A) :-
	number(A0),
	!,
	A = A0.
convert_constant(A0, _) :-
	error(5, convert_constant(A0)).


    % 
    % arith_builtin_args(+Expr, -List)
    % arith_builtin_args(+Expr, +List)
    % Maps the expression arguments of a builtin arithmetic function into a
    % list. Note that some functions (e.g. sum/1) already have a list argument.
    % 
arith_builtin_args(Expr, List) :-
	arith_builtin_info(Expr, List, _, _, _, _).


    %
    % arith_builtin_info(Expr,Args,Domain, Range, PromoteIntArgs,PossLin)
    %   Succeeds for all valid expression predicates, with
    %   Domain and Range unified to 'one', 'many' or 'unknown'
    %   depending on the type of the function.
    %   eg. a Domain of 'one' and a Range of 'many' indicates that the
    %   functions is "one-to-many", ie for any single input there may
    %   be many different outputs. eg. (+- 3) $= 3 and (+- 3) $= -3.
    %   PromoteIntArgs implies that integer arguments should be
    %   promoted to breals before being passed to is/2 for
    %   ground evaluation.
    %   PossLin indicates that function may occur in a linear expression.
    %   Args returns the function's expression-valued arguments as a list
    %	(note that some functions (e.g. sum/1) already have a list argument).
    %
    %   This table is used during compile time constant evaulation
    %   and so should be updated whenever new 'builtin' functions are
    %   added to IC (ie new cases added to ic_flatten)
    %
arith_builtin_info([X|Xs],	[X|Xs],	unknown,unknown,no,yes).
arith_builtin_info(+(X),	[X],	one,one,no,yes).
arith_builtin_info(+(X,Y),	[X,Y],	one,one,no,yes).
arith_builtin_info(-(X),	[X],	one,one,no,yes).
arith_builtin_info(-(X,Y),	[X,Y],	one,one,no,yes).
arith_builtin_info(eval(X),	[X],	unknown,unknown,no,no).
arith_builtin_info(*(X,Y),	[X,Y],	one,one,no,yes).
arith_builtin_info(/(X,Y),	[X,Y],	one,one,yes,no).
arith_builtin_info(^(X,Y),	[X,Y],	one,one,no,no).
arith_builtin_info(rpow(X,Y),	[X,Y],	unknown,unknown,no,no). % Integer promotion not required due to implementation
arith_builtin_info(sqr(X),	[X],	many,one,no,no).
arith_builtin_info(rsqr(X),	[X],	one,many,yes,no).
arith_builtin_info(sqrt(X),	[X],	one,one,yes,no).
arith_builtin_info(sin(X),	[X],	many,one,yes,no).
arith_builtin_info(cos(X),	[X],	many,one,yes,no).
arith_builtin_info(exp(X),	[X],	one,one,yes,no).
arith_builtin_info(ln(X),	[X],	one,one,yes,no).
arith_builtin_info(atan(X),	[X],	many,one,yes,no).
arith_builtin_info(abs(X),	[X],	many,one,no,no).
arith_builtin_info(+-(X),	[X],	one,many,no,no).
arith_builtin_info(min(X,Y),	[X,Y],	many,one,no,no).
arith_builtin_info(max(X,Y),	[X,Y],	many,one,no,no).
arith_builtin_info(sum(L),	L,	many,one,no,yes).
arith_builtin_info(min(L),	L,	many,one,no,no).
arith_builtin_info(max(L),	L,	many,one,no,no).
arith_builtin_info(minlist(L),	L,	many,one,no,no).
arith_builtin_info(maxlist(L),	L,	many,one,no,no).


    %
    % numbers(List)
    %   Return true iff List is a list of numbers
    %
numbers([]) ?- true.
numbers([H|T]) ?-
        number(H),
        numbers(T).


    %
    % promote_int_args(Flag, Args, NewArgs)
    %   Promotes all ints to breals for those functions that require it.
    %
promote_int_args(yes, Args, NewArgs) :-
	 (foreach(A0, Args), foreach(A1, NewArgs) do
	      (integer(A0) ->
		   breal(A0,A1)
	      ;
		   A0 = A1
	      )
	 ).
promote_int_args(no, Args, Args).


    %
    % evaluate_ground_term(T0, T)
    %   Attempts to evaluate the given ground term as an arithmentic
    %   expression.  This predicate knows about terms which is/2 does
    %   not.  Furthermore this predicate assumes that floats and
    %   rationals have been converted to breals where necessary to
    %   ensure safe evaluation, though it will promote integers to
    %   breals if the builtin functions requires it (see
    %   arith_builtin_info).
    %
evaluate_ground_term(sqr(T0), Res) :- 
        !,
        ( number(T0) ->
              Res is abs(T0 * T0)
        ;
              Res = sqr(T0)
        ).
evaluate_ground_term(subscript(Array,Index), Res) :-
        !,
        ( var(Array) ->
              Res = subscript(Array, Index)
        ; nonground(Index) ->
              Res = subscript(Array, Index)
        ;
              subscript(Array, Index, Res0),
              convert_term_constants(Res0, Res)
        ).
evaluate_ground_term(Term0, Res) :-
	functor(Term0, Fun, Arity),
        % consider only onto-one functions
	arith_builtin_info(Term0, Args0, _, one, Promote, _),
        numbers(Args0),
        !,
        promote_int_args(Promote,Args0,Args1),
	functor(Term1, Fun, Arity),
	arith_builtin_args(Term1, Args1),
        subcall(Res is Term1,[]).
evaluate_ground_term(Term, Term).
        
        

    %
    % convert_term_constants(?T0, ?T)
    %	Converts all constants appearing in term T0 to IC's internal
    %	representation, yielding T. Also evaluates any builtin
    %   functions which are defined to return a single result. (so
    %   (+-) and rsqr will NOT be evaluated.)
    %
convert_term_constants(T0, T) :-
	number(T0),
	!,
	convert_constant(T0, T).
convert_term_constants(T0, T) :-
	compound(T0),
	!,
        ( arith_builtin_args(T0, Args0) ->
	    functor(T0, F, Arity),
	    functor(T1, F, Arity),
	    arith_builtin_args(T1, Args),
	    convert_term_constants_list(Args0, Args)
        ;
            T0 = T1
        ),
        evaluate_ground_term(T1, T).
convert_term_constants(T0, T) :-
	var(T0),
	!,
	T = T0.
convert_term_constants(pi, T) :-
	!,
	T is breal(pi) + -0.1e-100__0.1e-100.
convert_term_constants(e, T) :-
	!,
	T is breal(e) + -0.1e-100__0.1e-100.
convert_term_constants(inf, T) :-
	!,
	T = 1.0Inf__1.0Inf.
convert_term_constants(T0, T) :-
	T = T0.

	% Iterate over possibly unterminated list
	% If improper list, just copy across
    convert_term_constants_list(Args, Args) :- var(Args), !.
    convert_term_constants_list([], []) :- !.
    convert_term_constants_list([X|Xs], [Y|Ys]) :- !,
	convert_term_constants(X, Y),
	convert_term_constants_list(Xs, Ys).
    convert_term_constants_list(Junk, Junk).


    %
    % list2goals(+List, -Goals)
    %	Converts a list of goals List to a conjunction of goals Goals.
    %
list2goals([], true).
list2goals([G|L], Goals) :-
	list2goals(G, L, Goals).

list2goals(G, [], G).
list2goals(G, [Next|L], (G, Goals)) :-
	list2goals(Next, L, Goals).

    %
    % integer_op(?Op)
    %	Succeeds iff Op is an integer IC operator.
    %
integer_op(#=).
integer_op(#>=).
integer_op(#=<).
integer_op(#>).
integer_op(#<).
integer_op(#\=).

    %
    % div_floor(++X, ++Y, ?Z)
    %	Integer division, rounding down.  Unifies Z with the largest integer
    %	no greater than X/Y.
    %	(Like `Z is fix(floor(X / Y))' where X and Y are ints, only safer).
    %
div_floor(X, Y, Z) :-
	Z is X div Y.

    %
    % div_ceiling(++X, ++Y, ?Z)
    %	Integer division, rounding up.  Unifies Z with the smallest integer
    %	no less than X/Y.
    %	(Like `Z is fix(ceiling(X / Y))' where X and Y are ints, only safer).
    %
div_ceiling(X, Y, Z) :-
	Z is -(-(X) div Y).

    %
    % div_exact(++X, ++Y, ?Z)
    %	Exact integer division.  If X/Y is an integer, unify it with Z;
    %	otherwise fail.
    %
div_exact(X, Y, Z) :-
	X mod Y =:= 0,
	Z is X // Y.

    %
    % schedule_susp(+Susp)
    %	Schedule the specified suspension and propagate (wake).
    %
schedule_susp(Susp) :-
	schedule_suspensions(1, sl([Susp])),
	wake.

    %
    % suspend_eq(++A, ?X, +S, ?Bool)
    %	Add the suspension S to the relevant wake lists of X, assuming the
    %	term A * X appears in an equality constraint with reified value Bool.
    %
suspend_eq(A, X, S, Bool) :-
	( (not A =:= 0, Bool \== 0) ->
	    % A does not overlap with zero and Bool is NOT 0
	    insert_suspension(X, S, min of ic, ic),
	    insert_suspension(X, S, max of ic, ic)
	;
	    % A overlaps zero or Bool is 0
	    insert_suspension(X, S, inst of suspend, suspend)
	).

    %
    % suspend_less(++A, ?X, +S, ?Bool)
    %	Add the suspension S to the relevant wake lists of X, assuming the
    %	term A * X appears on the left hand side of a
    %	strict-less-than or a less-than-or-equal with reified
    %   value of Bool.
    %
suspend_less(A, X, S, Bool) :-
	( var(Bool) ->
	    % Reified status is unknown
	    insert_suspension(X, S, min of ic, ic),
	    insert_suspension(X, S, max of ic, ic)
	; % Bool == 1 ; Bool == 0 ->
	    ( not A =< 0.0 ->
		      %% i.e. (lower bound of) A greater than 0
		      ( Bool == 1 ->
				insert_suspension(X, S, min of ic, ic)
		      ;
				insert_suspension(X, S, max of ic, ic)
		      )
	    ; not A >= 0.0 ->
		      %% i.e. (upper bound of) A less than 0
		      ( Bool == 1 ->
			       insert_suspension(X, S, max of ic, ic)
		      ;
			       insert_suspension(X, S, min of ic, ic)
		      )
	    ;
		      %% A spans zero
		      insert_suspension(X, S, inst of suspend, suspend)
	    )
	).


%---------------------------------------------------------------------
%
% Compile-time manipulation of constraints.
%

	%
	% tr_ic_constraint_in(+Con, -Goals, +Module)
	%	Translates an IC constraint Con into goals to execute Goals.
	%
tr_ic_constraint_in(Con, Goals, Module) :-
	conv_con(Con, compile, Module, List, []),
	list2goals(List, Goals),
	diff_goal(Con, Goals).	% prevent looping

    diff_goal(G, ic:G) ?- !, fail.
    diff_goal(G, (ic:G,_)) ?- !, fail.
    diff_goal(_, _).

    %
    % Flags must match ic.c:
    % 	1 : lower bound
    % 	2 : upper bound
    % 	4 : integer
    %
    % Note the meaning of the bounds:
    %	both bounds: equality (=:=)
    %	just upper:  non-strict inequality (=<)
    %	just lower:  strict inequality (>)
    %	neither:     disequality (=\=)
    %
op_to_flags(=\=, 0, no).
op_to_flags(*\=, 0, no).
op_to_flags($\=, 0, no).
op_to_flags(>,   1, no).
op_to_flags(*>,  1, no).
op_to_flags($>,  1, no).
op_to_flags(<,   1, yes).
op_to_flags(*<,  1, yes).
op_to_flags($<,  1, yes).
op_to_flags(=<,  2, no).
op_to_flags(*=<, 2, no).
op_to_flags($=<, 2, no).
op_to_flags(>=,  2, yes).
op_to_flags(*>=, 2, yes).
op_to_flags($>=, 2, yes).
op_to_flags(=:=, 3, no).
op_to_flags(*=,  3, no).
op_to_flags($=,  3, no).
op_to_flags(#\=, 4, no).
op_to_flags(#>,  5, no).
op_to_flags(#<,  5, yes).
op_to_flags(#=<, 6, no).
op_to_flags(#>=, 6, yes).
op_to_flags(#=,  7, no).

flags_to_op(0, $\=).
flags_to_op(1, $>).
flags_to_op(2, $=<).
flags_to_op(3, $=).
flags_to_op(4, #\=).
flags_to_op(5, #>).
flags_to_op(6, #=<).
flags_to_op(7, #=).

flags_to_op(Flags, 0, Op) :-
	toggle_bool(Flags, Flags1),
	flags_to_op(Flags1, Op).
flags_to_op(Flags, 1, Op) :-
	flags_to_op(Flags, Op).

toggle_bool(Flags0, Flags) :-
	Flags is xor(Flags0, 3).

is_integral_flags(4).
is_integral_flags(5).
is_integral_flags(6).
is_integral_flags(7).

is_equation(3).
is_equation(7).

ic_lin_con(Flags, Bool, Lin) :-
	ic_event(ic_lin_create),
	set_up_ic_con(Flags, Bool, Lin),
	wake.


    %
    % conv_con(+Con, ++Mode, +Module)
    %	DCG rule for converting a user-level IC constraint Con into a list
    %	of low-level predicates to call.  Mode is either the atom `compile'
    %	or the atom `run', indicating whether the transformation is being
    %	done at compile time or run time.
    %
conv_con(A => B, Mode, Module) -->
	!,
	conv_reified_connective_con((=>), A, B, 1, Mode, Module).
conv_con(=>(A, B, Bool), Mode, Module) -->
	!,
	conv_reified_connective_con((=>), A, B, Bool, Mode, Module).
conv_con((A or B), Mode, Module) -->
	!,
	conv_reified_connective_con((or), A, B, 1, Mode, Module).
conv_con(or(A, B, Bool), Mode, Module) -->
	!,
	conv_reified_connective_con((or), A, B, Bool, Mode, Module).
conv_con((A ; B), Mode, Module) -->
	!,
	conv_reified_connective_con((or), A, B, 1, Mode, Module).
conv_con(';'(A, B, Bool), Mode, Module) -->
	!,
	conv_reified_connective_con((or), A, B, Bool, Mode, Module).
conv_con((A and B), Mode, Module) -->
	!,
	conv_reified_connective_con((and), A, B, 1, Mode, Module).
conv_con(and(A, B, Bool), Mode, Module) -->
	!,
	conv_reified_connective_con((and), A, B, Bool, Mode, Module).
conv_con((A , B), Mode, Module) -->
	!,
	conv_reified_connective_con((and), A, B, 1, Mode, Module).
conv_con(','(A, B, Bool), Mode, Module) -->
	!,
	conv_reified_connective_con((and), A, B, Bool, Mode, Module).
conv_con(neg(A), Mode, Module) -->
	!,
	conv_neg_con(A, 1, Mode, Module).
conv_con(neg(A, Bool), Mode, Module) -->
	!,
	conv_neg_con(A, Bool, Mode, Module).
conv_con(:(LookupModule, Goal, Bool), run, _Module) -->
	!,
	{append_arg(Goal, Bool, NewGoal)},
	[LookupModule:NewGoal].
conv_con(Con, Mode, Module) -->
	{ Con =.. [Op, Arg1, Arg2 | Rest] },
	{ op_to_flags(Op, Flags, Swap) },
	!,
	{ rest_to_bool(Rest, Bool) },
	conv_con2(Swap, Flags, Arg1, Arg2, Bool, Mode, Module).
conv_con(Goal, run, _Module) -->
	% only call goals at run-time
	[Goal].


    rest_to_bool([], 1).
    rest_to_bool([Bool], Bool).

conv_con2(no, Flags, Arg1, Arg2, Bool, Mode, Module) -->
	conv_normalised_con(Flags, Arg1, Arg2, Bool, Mode, Module).
conv_con2(yes, Flags, Arg1, Arg2, Bool, Mode, Module) -->
	conv_normalised_con(Flags, Arg2, Arg1, Bool, Mode, Module).


	%
	% append_arg(+Pred, ?Arg, +NewPred)
	%	Append 'Arg' to 'Pred'
append_arg(Pred, Arg, NewPred) :-
	Pred =.. ArgList,
	append(ArgList,[Arg], NewArgList),
	NewPred =.. NewArgList.

make_bool_dcg(Bool) --> 
	{var(Bool)},
	!,
	[ic:make_bool(Bool)].
make_bool_dcg(Bool) --> 
	( {Bool == 0} ->
		[]
	; {Bool == 1} ->
		[]
	;
		{fail}
	).


	%
	% reify_expression(?Arg, ++Mode, +Module, -AppendedBool)
	%	If 'Arg' is a variable, create a reified expression, otherwise
	%	append a 'Bool' variable to the expression.
	%
reify_expression(Arg, _Mode, _Module, AppendedBool) -->
	{var(Arg) ; number(Arg)},
	!,
	make_bool_dcg(Arg),
	{
		AppendedBool = Arg
	}.
reify_expression(:(LookupModule, Arg), Mode, Module, AppendedBool) -->
	!,
	make_bool_dcg(AppendedBool),
	{
		append_arg(Arg, AppendedBool, NewArg),
		Expr = LookupModule:NewArg
	},
	conv_con(Expr, Mode, Module).
reify_expression(Arg, Mode, Module, AppendedBool) -->
	make_bool_dcg(AppendedBool),
	{append_arg(Arg, AppendedBool, NewArg)},
	conv_con(NewArg, Mode, Module).


	%
	% conv_neg_con(+A, ?Bool, +Mode, ++Module)
	%	DCG rule for converting the reified operator 'neg'.
	%	XXX - Should try to push negation into argument's
	%	constraint, if possible, to avoid introducing new constraint.
	%
conv_neg_con(A, Bool, Mode, Module) -->
	reify_expression(A, Mode, Module, ABool),
	%conv_normalised_con((#=), Bool, 1 - ABool, 1, Mode, Module).
	{ op_to_flags(#=, Flags, Swap) },
	conv_con2(Swap, Flags, Bool, 1 - ABool, 1, Mode, Module).


	%
	% conv_reified_connective_con(++Op, +A, +B, ?Bool, +Mode, ++Module)
	%	DCG rule for converting the reified connectives 'or', 'and', '=>'
	%
conv_reified_connective_con((or), A, B, Bool, Mode, Module) -->
	reify_expression(A, Mode, Module, ABool),
	reify_expression(B, Mode, Module, BBool),
	%conv_normalised_con((#=<), 1, ABool + BBool, Bool, Mode, Module).
	{ op_to_flags(#=<, Flags, Swap) },
	conv_con2(Swap, Flags, 1, ABool + BBool, Bool, Mode, Module).
conv_reified_connective_con((and), A, B, Bool, Mode, Module) -->
	reify_expression(A, Mode, Module, ABool),
	reify_expression(B, Mode, Module, BBool),
	%conv_normalised_con((#=), ABool + BBool, 2, Bool, Mode, Module).
	{ op_to_flags(#=, Flags, Swap) },
	conv_con2(Swap, Flags, ABool + BBool, 2, Bool, Mode, Module).
conv_reified_connective_con((=>), A, B, Bool, Mode, Module) -->
	reify_expression(A, Mode, Module, ABool),
	reify_expression(B, Mode, Module, BBool),
	%conv_normalised_con((#=<), ABool, BBool, Bool, Mode, Module).
	{ op_to_flags(#=<, Flags, Swap) },
	conv_con2(Swap, Flags, ABool, BBool, Bool, Mode, Module).


    %
    % conv_normalised_con(++Flags, +A, +B, ?Bool, +Mode, ++Module)
    %	DCG rule for converting the constraint `A Op B' into a list of
    %	low-level predicates to call.  Mode is either the atom `compile' or
    %	the atom `run'. Bool is a reified boolean.
    %
    %	Note that from here on, Op is assumed to be one of
    %		*=, *=<, *\=, #=, #=<, #\=
    %	(i.e. no >=, > or < constraints, and only * and # constraints).
    %
    %	This predicate is responsible for making sure any floating point
    %	constants are converted to intervals.
    %
conv_normalised_con(Flags, A, B, Bool, _, _Module) -->
	{var(A)},
	{number(B)},
	!,
	% Simple one variable constraint.
	make_bool_dcg(Bool),
	{convert_constant(B, B1)},
	[ic_constraints:ic_lin_con(Flags, Bool, [-1 * B1, 1 * A])].
conv_normalised_con(Flags, A, B, Bool, _, _Module) -->
	{number(A)},
	{var(B)},
	!,
	% Simple one variable constraint.
	make_bool_dcg(Bool),
	{convert_constant(A, A1)},
	[ic_constraints:ic_lin_con(Flags, Bool, [1 * A1, -1 * B])].
/*
conv_normalised_con(Flags, A, B, Bool, _, _Module) -->
	{var(A)},
	{var(B)},
	!,
	% Simple two variable constraint --- though good chance one of them
	% will be ground at run-time.
	make_bool_dcg(Bool),
	[ic_constraints:ic_lin_con(Flags, Bool, [1 * A, -1 * B])].
*/
conv_normalised_con(Flags, A0, B0, Bool, Mode, Module) -->
	% Nontrivial constraint.
	% Attempt to replace non-linear subterms with temporary variables so
	% that the expression can be handled more efficiently using a linear
	% propagator.
	make_bool_dcg(Bool),
	{
                (is_integral_flags(Flags) ->
                     Integral=integer
                 ;
                     Integral=real
                ),
                convert_term_constants(A0, A1),
                elim_non_lin_terms(A1, A, [], ResA, [], SubstVarA, Changed)
	},
	ic_flatten_list(Integral, ResA, Mode, SubstVarA),

	{
		convert_term_constants(B0, B1),
		elim_non_lin_terms(B1, B, [], ResB, [], SubstVarB, Changed)
	},
	ic_flatten_list(Integral, ResB, Mode, SubstVarB),

	% After the elimination of non-linear terms:
	% a) If 'A' and 'B' are variables, 
	% b) The operator is an equality constraint and a 
	%    reified test is not being performed,
	% c) If either of the non-linear residues, 'resA' or 'resB' are 
	% non-empty, then one of the variables 'A' or 'B' is a temporary
	% introduced by 'elim_non_lin_terms' and so 'A' and 'B' can be 
	% compile-time unified.
	% Otherwise, let 'linearize' handle the expression.
	( { var(A), var(B), Bool == 1, is_equation(Flags),
            (ResA \= []; ResB \= [])} -> 
	    {A=B, Lin = [], Res = []},
            % constrain the type of this top level temporary, since
            % ic_flatten will NOT do so
            ( {is_integral_flags(Flags)} ->
                [ic_kernel:set_var_type(A,integer)]
            ;
                {true}
            )
	;
	    {linearize(A-B, Lin, Res)@Module}
	),
                 
	% If 'Res' is empty, there's no non-linear portion to worry about,
	% so process the linear portion.
	( {Res = []} ->
	    ( {Lin \= []} ->
		[ic_constraints:ic_lin_con(Flags, Bool, Lin)]
	    ;
		{true}
	    )
	;
	    % If however, there is a non-linear portion:
	    % 	If performing a compile-time transform, fail the DCG if it's
	    % 	potentially looping (no work has been done during the
	    % 	transformation) or the expression is 'potentially linear' at
	    % 	runtime.  Otherwise insert the unprocessed portion of the
	    % 	expression back in to the source.
	    %
	    % 	If performing a run-time transform, flatten the non-linear
	    %  	portion and process the linear portion.
	    ( {Mode == compile} ->
		( {Changed \== true} ->
		    % Transform is doing no work/is looping, or, there are
		    % no definitely non-linear parts to the expression
		    % (Changed is unbound, i.e. not ground to 'true'), there
		    % are some potentially linear parts ('Res' is not
		    % empty), so fail the DCG leaving the original
		    % expression in the source to be handled at runtime.
		    {fail}	
		;
		    % Place the unprocessed portion back into the source.
		    {
			delinearize(Lin, NewExpr),

			( foreach(Elem, Res) do
			    % Unify temporary vars with their expressions
			    Elem = (X = X)
			),

			flags_to_op(Flags, Op),
			NewTerm =.. [Op, NewExpr, 0, Bool]
		    },
		    [ic:NewTerm]
		)		
	    ;
		% Runtime - flatten 'Res' and convert 'Lin'
                {
                    ( is_integral_flags(Flags) ->
                        Integral=integer
                    ;
                        Integral=real
                    )
                },
		( conv_non_lin(Integral, Res, Mode) ->
		    [ic_constraints:ic_lin_con(Flags, Bool, Lin)]
		;
		    {printf(error, "Non-linear expression conversion failed in %q\n", [Res])},
		    {abort}
		)
	    )
	).
			  

    %
    % elim_non_lin_terms(+TermIn, +TermOut, +NonLinTermsIn, +NonLinTermsOut,
    %				 +SubstVarsIn, +SubstVarsOut, ?Changed)
    %	Recursively substitute the sub-terms of an expression with
    %	temporary variables for the 'blatantly' non-linear terms.
    %	'TermOut' is the transformed expression with subterms that
    %	are definitely linear ('+', '-'), or will possibly become
    %	linear ('*', 'eval(<var>)') at runtime, and the non-linear
    %	terms substituted by temporaries.
    %	'NonLinTerms' contains those non-linear terms
    %	which were substituted by temporaries. 'SubstVarsOut'
    %	provides a mapping of temporaries at the corresponding index
    %	to the non-linear terms in 'NonLinTermsOut'. 
    %	'NonLinTermsOut' and 'SubstVarsOut' are then available to be
    %	'ic_flatten'ed in the format required by the auxiliary
    %	'ic_flatten_list'.
    %
    %	'Changed' is unbound (i.e. not ground to 'true'), if:
    %	The transform is doing no work/is looping, or, there are no
    %	definitely non-linear parts to the expression.
    %
elim_non_lin_terms(TermIn, TermOut, NonLinTermsIn, NonLinTermsOut, 
				   SubstVarsIn, SubstVarsOut, Changed) :- 
	compound(TermIn),
	!,
	% If the functor is an 'eval' with a single variable argument
	% then it maybe linear, so leave it alone.
	( TermIn = eval(Arg0) ->
	    ( var(Arg0) ->
		TermIn = TermOut,
		NonLinTermsIn = NonLinTermsOut,
		SubstVarsIn = SubstVarsOut
	    ;
		% Otherwise, drop the 'eval' and attempt subterm 
		% elimination.
		Changed = true,
		elim_non_lin_terms(Arg0, TermOut, NonLinTermsIn,
			NonLinTermsOut, SubstVarsIn, SubstVarsOut, Changed)
	    )
	;
	    % Leave the term in the result and process only its 
	    % subterms if it is defintely linear, or possibly linear 
	    % at runtime
	    ( arith_builtin_info(TermIn, Args0, _, _, _, yes) ->
		functor(TermIn, F, Arity),
		functor(TermOut, F, Arity),
		arith_builtin_args(TermOut, Args),
		elim_non_lin_terms_list(Args0, Args,
			NonLinTermsIn, NonLinTermsOut,
			SubstVarsIn, SubstVarsOut, Changed)
	    ;	
		Changed = true,
		NonLinTermsOut = [TermIn | NonLinTermsIn],
		SubstVarsOut = [TermOut | SubstVarsIn]
	    )
	).
elim_non_lin_terms(Term, Term, NonLinTerms, NonLinTerms, SubstVarsIn,
                   SubstVarsIn, _Changed).

	% Iterate over possibly unterminated list
	% If improper list, just copy across
    elim_non_lin_terms_list(In, In, NLT, NLT, SV, SV, _Changed) :- var(In), !.
    elim_non_lin_terms_list([], [], NLT, NLT, SV, SV, _Changed) :- !.
    elim_non_lin_terms_list([X|Xs], [Y|Ys], NLT0, NLT, SV0, SV, Changed) :- !,
	elim_non_lin_terms(X, Y, NLT0, NLT1, SV0, SV1, Changed),
	elim_non_lin_terms_list(Xs, Ys, NLT1, NLT, SV1, SV, Changed).
    elim_non_lin_terms_list(Junk, Junk, NLT, NLT, SV, SV, _Changed).



	%
	% conv_non_lin( ExprList, ++Mode )
	%	DCG rule to flatten list of non-linear expressions
conv_non_lin( _Integral, [], _Mode ) --> {true}.
conv_non_lin( Integral, [H|T], Mode ) -->
	{H = (Var = Expr)},
	ic_flatten( Integral, Expr, Var, Mode ),
	conv_non_lin( Integral, T, Mode ).


%---------------------------------------------------------------------
%
% Output translation of constraints.
%
% I.e. the following predicates are used for displaying constraints in a
% user-friendly form.
%

tr_ic_constraint_out(In, QualOut, Module) :-
	tr_ic_constraint_out(In, Out),
	% qualify the portrayed goal only when not visible in the caller
	functor(Out, N, A),
	( get_flag(N/A, definition_module, DM)@Module,
	  get_flag(N/A, definition_module, DM)@ic ->
	    QualOut = Out
	;
	    QualOut = ic:Out
	).

    %
    % tr_ic_constraint_out(+Con, -Pretty)
    %	Translates the delayed goal or compile-generated goal Con into a
    %	more user-friendly form Pretty.
    %
tr_ic_constraint_out(make_bool(Bool), Bool :: 0..1).
tr_ic_constraint_out(check_domain_demon(X, Domain, Bool, _Susp),
                     #::(X, Domain, Bool)).
tr_ic_constraint_out(ic_lin_con(Flags0, Bool, Lin0), Term) :-
	( Flags0 == ... ->
	    % In case we're tracing a failure of this predicate.
	    Term = '<ic linear constraint>'
	;
	    ( Lin0 = [NegRHS * One | Lin], One == 1 ->
		RHS is -NegRHS
	    ;
		RHS = 0,
		Lin = Lin0
	    ),
	    tr_lin_terms_out(Lin, Expr),
	    Flags is Flags0 /\ 16'7,
	    ( number(Bool) ->
		flags_to_op(Flags, Bool, Op),
		Constraint =.. [Op, Expr, RHS]
	    ;
		flags_to_op(Flags, Op),
		Constraint =.. [Op, Expr, RHS, Bool]
	    ),
	    Term = Constraint
	).
tr_ic_constraint_out(set_up_ic_con(Flags, Bool, Lin), Term) :-
	tr_ic_constraint_out(ic_lin_con(Flags, Bool, Lin), Term).
tr_ic_constraint_out(prop_ic_con(Con), Term) :-
	( Con == ... ->
	    % In case we're tracing a failure of this predicate.
	    Term = '<ic linear constraint>'
	;
	    get_print_info(Con, Flags0, Bool, Coefs, Vars, RHS),
	    Flags is Flags0 /\ 16'7,
	    tr_lin_vec_out(Coefs, Vars, Expr),
	    % Note this test should not be var/nonvar, since that messes up
	    % the transformation when used in conjunction with the test
	    % framework (which has substituted all the variables with
	    % corresponding atoms).
	    ( number(Bool) ->
		flags_to_op(Flags, Bool, Op),
		Constraint =.. [Op, Expr, RHS]
	    ;
		flags_to_op(Flags, Op),
		Constraint =.. [Op, Expr, RHS, Bool]
	    ),
	    Term = Constraint
	).
tr_ic_constraint_out(indomain_init(Var, _Lo), indomain(Var)).
tr_ic_constraint_out(
		element(Index, Vector, _Hash, Value, _ElementData, _Susp),
		Term) :-
	( Vector == ... ->
	    Term = element(..., ..., ...)
	;
	    Vector =.. [_ | List],
	    Term = element(Index, List, Value)
	).
tr_ic_constraint_out(ac_eq_prop(X, Y, C, _Susp), ac_eq(X, Y, C)).


    %
    % tr_lin_term_out(++A, ?X, -Expr)
    %	Translates the linear term A * X into a more pleasing form Expr.
    %
tr_lin_term_out(1, X, X) :- !.
tr_lin_term_out(-1, X, -X) :- !.
tr_lin_term_out(A, X, A * X).

    %
    % add_to_expr(+Expr0, ++A, ?X, -Expr)
    %	Add the term A * X to the end of the current expression Expr0, to
    %	get a new expression Expr.
    %
add_to_expr(Expr0, 1, X, Expr) :- !,
	Expr = Expr0 + X.
add_to_expr(Expr0, -1, X, Expr) :- !,
	Expr = Expr0 - X.
add_to_expr(Expr0, A, X, Expr) :-
	( not A >= 0 ->
		AbsA is -A,
		Expr = Expr0 - AbsA * X
	;
		Expr = Expr0 + A * X
	).

    %
    % tr_lin_terms_out(+Lin, -Expr)
    %	Translates the list of linear terms Lin into an infix sum Expr.
    %
tr_lin_terms_out([], 0).
tr_lin_terms_out([A0 * X0 | AXs], Expr) :-
	tr_lin_term_out(A0, X0, Expr0),
	(
	    foreach(A * X, AXs),
	    fromto(Expr0, ExprIn, ExprOut, Expr)
	do
	    add_to_expr(ExprIn, A, X, ExprOut)
	).

    %
    % tr_lin_vec_out(++Coefs, +Vars, -Expr)
    %	Translates the coefficient and variable vectors into an infix sum
    %	Expr.
    %
tr_lin_vec_out(Coefs, Vars, Expr) :-
	functor(Coefs, _, N),
	( N = 0 ->
	    Expr = 0
	;
	    subscript(Coefs, [1], A0),
	    subscript(Vars, [1], X0),
	    tr_lin_term_out(A0, X0, Expr0),
	    (
		for(I, 2, N),
		fromto(Expr0, ExprIn, ExprOut, Expr),
		param(Coefs, Vars)
	    do
		subscript(Coefs, [I], A),
		subscript(Vars, [I], X),
		add_to_expr(ExprIn, A, X, ExprOut)
	    )
	).


%---------------------------------------------------------------------

% succeeds given a var or an integer
integer_or_var(X):-
        var(X),!.
integer_or_var(X):-
        integer(X).

% succeeds given a list or sub-array of var/integers
integer_or_var_vars([]).
integer_or_var_vars([H|Tail]):-
        integer_or_var(H),
        integer_or_var_vars(Tail).
integer_or_var_vars(subscript(Array, IndexList)):-
        subscript(Array, IndexList, Elems),
        integer_or_var_vars(Elems).


%
% The `::' (domain) constraint.
%
'::_body'(X, Domain, Module):-
	process_domain_domain(Domain, NormalDomain, Type, Module),
	process_domain_vars(X, Xs),
	!,
	call_priority((
		set_vars_type(Xs, Type),
		assign_domain(Type, Xs, NormalDomain)
	    ), 2).
'::_body'(X, Domain, _Module):-
	domain_error((::), X, Domain).

%
% The `#::' (integral domain) constraint.
%
'#::_body'(X, Domain, Module):-
	process_domain_domain(Domain, NormalDomain, integer, Module),
	process_domain_vars(X, Xs),
	!,
	call_priority((
		set_vars_type(Xs, integer),
		assign_domain_integer(Xs, NormalDomain)
	    ), 2).
'#::_body'(X, Domain, _Module):-
	domain_error((#::), X, Domain).

%
% The `$::' (real domain) constraint.
%
'$::_body'(X, Domain, Module):-
	process_domain_domain(Domain, NormalDomain, real, Module),
	process_domain_vars(X, Xs),
	!,
	call_priority((
		set_vars_type(Xs, real),
		assign_domain_real(Xs, NormalDomain)
	    ), 2).
'$::_body'(X, Domain, _Module):-
	domain_error(($::), X, Domain).


%
% Reified `::'
%
'::_body'(X, Domain, Bool, Module):-
	process_domain_domain(Domain, NormalDomain, Type, Module),
	( var(X) -> true ; number(X) ),
	!,
	make_bool(Bool),
	( NormalDomain == [] ->
	    Bool = 0,
	    set_vars_type(X, real)
	; Type == integer ->
	    % Don't wake anything until we've imposed the type and full domain.
	    call_priority((set_vars_type(X, integer),
			   '::_aux'(X, NormalDomain, Bool)
			  ), 2)
	; NormalDomain = [Lo..Hi] ->
	    % use reified constraint to implement reified domain on
	    % real variables
	    and_body(X $>= Lo, X $=< Hi, Bool, Module)
	;
	    domain_error_reified((::), X, Domain, Bool)
	).
'::_body'(X, Domain, Bool, _Module):-
	domain_error_reified((::), X, Domain, Bool).

%
% Reified `#::'
%
'#::_body'(X, Domain, Bool, Module):-
	process_domain_domain(Domain, NormalDomain, integer, Module),
	( var(X) -> true ; number(X) ),
	!,
	make_bool(Bool),
	% Don't wake anything until we've imposed the type and full domain.
	call_priority((set_vars_type(X, integer),
		       '::_aux'(X, NormalDomain, Bool)
		      ), 2).
'#::_body'(X, Domain, Bool, _Module):-
	domain_error_reified((#::), X, Domain, Bool).

%
% Reified `$::'
%
'$::_body'(X, Domain, Bool, Module):-
	process_domain_domain(Domain, NormalDomain, real, Module),
	( var(X) -> true ; number(X) ),
	!,
	( NormalDomain == [] ->
	    Bool = 0,
	    set_vars_type(X, real)
	;
	    NormalDomain = [Lo..Hi],
	    % use reified constraint to implement reified domain on
	    % real variables
	    and_body(X $>= Lo, X $=< Hi, Bool, Module)
	).
'$::_body'(X, Domain, Bool, _Module):-
	domain_error_reified(($::), X, Domain, Bool).


    % X is a single variable (or number) that has already been constrained
    % to be integral.
'::_aux'(X, Domain, Bool):-
	var(Bool),
	!,
	check_domain(X, Domain, Bool).
'::_aux'(X, Domain, Bool):-
	Bool == 1,
	!,
	assign_domain_integer([X], Domain).
'::_aux'(X, Domain, Bool):-
	Bool == 0,
	remove_domain(X, Domain).


    %
    % domain_error(++Op, ?X, ?Domain)
    %	Flags an error in the domain Domain being applied to X,
    %   subject to the assignment operator Op.
    %
domain_error(Op, X, Domain) :-
	Goal =.. [Op, X, Domain],
	( nonground(Domain) ->
	    % Instantiation fault.
	    error(4, Goal)
	;
	    % Type error.
	    error(5, Goal)
	).

    %
    % domain_error_reified(++Op, ?X, ?Domain, ?Bool)
    %	Flags an error in the domain Domain being applied to X,
    %   subject to the reified truth value Bool by the domain
    %   assignment operator Op.
    %
domain_error_reified(Op, X, Domain, Bool) :-
	Goal =.. [Op, X, Domain, Bool],
	( nonground(Domain) ->
	    % Instantiation fault.
	    error(4, Goal)
	;
	    % Type error.
	    error(5, Goal)
	).


    %
    % process_domain_vars(?VarSpec, -VarList)
    %	Compiles a list of all the variables to be constrained.
    %
process_domain_vars(X, Xs) :-
	( var(X) ; number(X) ),
	!,
	Xs = [X].
process_domain_vars(X, Xs) :-
	nonvar(X),
	collection_to_list(flatten(X), Xs).

    %
    % process_domain_domain(++Domain, -NormalDomain, ?Type, ++Module)
    %   Normalizes the list of sub-domains and checks the domain is of
    %   the given type (which may be a var to indicate don't care).
    %   Returns a sorted list of intervals (no constants).
    %   Binds Type to the appropriate type if not already bound.
    %   Fails if Domain and Type do not match.
    %   May return empty intervals (Lo > Hi) but only if this is the only
    %   interval.
    %
process_domain_domain(Lo..Hi, NormalDomain, Type, Module) :-
	!,
	process_domain_domain([Lo..Hi], NormalDomain, Type, Module).
process_domain_domain([], NormalDomain, Type, _Module) :-
	!,
	% Empty domain.
	NormalDomain = [],
	fix_type(Type).
process_domain_domain(Domain, NormalDomain, Type, Module) :-
	Domain = [H | T],
	!,
	subdomain(H, Lo, Hi, Type, Module),
	% If there's more than one subdomain, the type must be integer.
	( T \== [] ->
	    Type = integer,
	    ( Lo =< Hi ->
		Domain1 = [Lo..Hi | Domain0]
	    ;
		Domain1 = Domain0
	    ),
	    (
		foreach(Sub, T),
		fromto(Domain0, Out, In, []),
		param(Type, Module)
	    do
		subdomain(Sub, Lo, Hi, Type, Module),
		% Filter empty ranges (Lo > Hi).
		( Lo =< Hi ->
		    Out = [Lo..Hi | In]
		;
		    Out = In
		)
	    ),
	    % Order the intervals.
	    number_sort(2, =<, Domain1, SortedUpperBoundsDomain),
	    number_sort(1, =<, SortedUpperBoundsDomain, SortedIntervalDomain),
	    [Lo0..Hi0 | SortedRest] = SortedIntervalDomain,
	    % Collapse zero width intervals to constants and merge
	    % overlapping/adjacent subdomains.  Note that if we need to do this
	    % (i.e. there's more than one subdomain) then we are necessarily
	    % processing an integer domain, so we may assume this in the loop
	    % body.
	    (
		foreach(Lo..Hi, SortedRest),
		fromto(Lo0..Hi0, LoIn..HiIn, LoOut..HiOut, FinalSubDomain),
		fromto(NormalDomain, In, Out, [FinalSubDomain])
	    do
		( HiIn + 1 >= Lo ->
		    % There is no gap between HiIn and Lo so merge
		    In = Out,
		    LoOut = LoIn,
		    HiOut is max(Hi, HiIn)
		;
		    % There is a gap between HiIn and Lo
		    In = [LoIn..HiIn | Out],
		    LoOut = Lo,
		    HiOut = Hi
		)
	    ),
	    % Don't allow first/last intervals to be singleton infinities.
	    NormalDomain = [_ .. FirstHi | _],
	    FirstHi \== -1.0Inf,
	    FinalSubDomain = LastLo .. _,
	    LastLo \== 1.0Inf
	;
	    NormalDomain = [Lo..Hi],
	    fix_type(Type)
	).
process_domain_domain(Expr, [Lo..Hi], Type, Module) :-
	% evaluate any functions/promote any rationals/convert
	% symbolic constants
	subdomain(Expr, Lo, Hi, Type, Module),
	fix_type(Type).


    %
    % assign_domain(++Type, +Xs, ++Domain)
    %	Constrains Xs to have the domain specified by Domain.  Xs must be a
    %	list of variables, which are assumed to already be of the correct
    %	type.
    %
assign_domain(integer, Xs, Domain) :-
	assign_domain_integer(Xs, Domain).
assign_domain(real, Xs, Domain) :-
	assign_domain_real(Xs, Domain).

assign_domain_integer(Xs, Domain) :-
	Domain = [Lo..Hi | Tail],
	( Tail = [] ->
	    ( Lo == Hi ->
		impose_value(Xs, Lo)
	    ;
		vars_impose_bounds(Xs, Lo, Hi)
	    )
	;
	    process_subdomains_include(Xs, Lo, Hi, Tail)
	).

assign_domain_real(Xs, Domain) :-
	Domain = [Lo..Hi],
	( ( breal(Lo) ; breal(Hi) ) ->
	    op_to_flags($=<, Flags, _),	% XXX - should process at compile-time.
	    (
		foreach(X, Xs),
		param(Lo, Hi, Flags)
	    do
		ic_lin_con(Flags, 1, [-1 * X, 1 * Lo]),
		ic_lin_con(Flags, 1, [1 * X, -1 * Hi])
	    )
	;
	    % Don't treat singleton domains differently to ranges (e.g. by
	    % calling impose_value/2) since if Lo and Hi are equal we want
	    % the result to be a zero-width bounded real (assuming the
	    % variable(s) have not already been constrained to be integral),
	    % rather than whatever type Lo and/or Hi are.
	    vars_impose_bounds(Xs, Lo, Hi)
	).

    %
    % remove_domain(?X, ++Domain)
    %	Constrains X to not be contained in the domain specified by
    %   Domain.  X may be a single variable or an integer.
    %   The domain must be an integer, integer
    %   interval or list of integers/integer intervals.
    %
remove_domain(X, Domain) :-
	(
	    foreach(Lo..Hi, Domain),
	    param(X)
	do
	    exclude_var_range(X, Lo, Hi)
	).

    %
    % check_domain(?X, ++Domain, ?Bool)
    %   Checks to see if X can have the domain Domain.  Reflects the 
    %   truth into Bool.  Only works for integer domains.
    %	Instatiating Bool to 0 or 1 results in remove_domain or
    %   assign_domain being called.
    %
check_domain(X, Domain, Bool):-
	% Should use bitmap operations when possible to determin
	% (dis)entailment.
	suspend(check_domain_demon(X, Domain, Bool, Susp),
		4,
		[Bool->inst,X->min, X->max, X->hole],
		Susp),
	schedule_susp(Susp).

    %
    % check_domain_demon(?X, ++Domain, ?Bool, ++Susp)
    %   Constrains the single variable X to have the domain Domain,
    %   subject to the reified boolean Bool.
    %
:- demon(check_domain_demon/4).
check_domain_demon(X, Domain, Bool, Susp):-
	( Bool == 1 ->
	    kill_suspension(Susp),
	    call_priority(assign_domain_integer([X], Domain), 2)
	; Bool == 0 ->
	    kill_suspension(Susp),
	    call_priority(remove_domain(X, Domain), 2)
	;
	    ic_kernel:get_domain(X, VarDomain),
	    % Ensure that the Var domain is normalized
	    ( VarDomain = [_|_] ->
		NormalVarDomain = VarDomain
	    ;
		NormalVarDomain = [VarDomain]
	    ),
	    domain_inclusion_check(NormalVarDomain, Domain, Bool)
	).

    %
    % domain_inclusion_check(++D1, ++D2, -Bool)
    %   Bool = 0 (disentailed) iff the intersection of D1 and D2 is empty.
    %   Bool = 1 (entailed)    iff D1 is wholly contained within D2
    %   Bool = var             otherwise
    %
domain_inclusion_check(D1, D2, Bool):-
	( domain_inclusion_check1(D1, D2, IncludeExclude) ->
	    ( IncludeExclude == 1 ->
		% D1 included in D2.
		Bool = 1
	    ; % IncludeExclude == 0
		% D1 completely outside D2.
		Bool = 0
	    )
	;
	    true
	).

    %
    % domain_inclusion_check1(++Domain1, ++Domain2, -IncludeExclude)
    %   Unifies IncludeExclude with 1 iff there are elements of Domain1 in
    %   Domain2, and unifies it with 0 iff there are elements of Domain1
    %   that are not in Domain2.  Obviously it fails if some elements of
    %   Domain1 are in Domain2 and some aren't.  If we assume that the
    %   variable's domain (Domain1) cannot be empty, IncludeExclude is
    %   guaranteed to be bound to some value upon success.
    %
    %   We assume that Domain1 is a list of integers or non-empty intervals
    %   in ascending order, with adjacent integers/intervals not contiguous.
    %   We make similar assumptions about Domain2, except that integers are
    %   not allowed.
    %
domain_inclusion_check1(D1, D2, IncludeExclude) :-
	( D1 = [] ->
	    true
	; D2 = [] ->
	    % D1 is non-empty, so there are some elements not in D2.
	    IncludeExclude = 0
	;
	    D1 = [Sub1 | Rest1],
	    get_subdomain_bounds(Sub1, L1, H1),
	    D2 = [L2..H2 | Rest2],
	    ( H1 > H2 ->
		% If the intervals overlap we have failure: H2 and H2+1 are
		% both in D1, but only H2 is in D2.  So to proceed we need
		% L1 > H2 as well.
		L1 > H2,
		% Skip to next sub-range in D2.
		domain_inclusion_check1(D1, Rest2, IncludeExclude)
	    ; L1 < L2 ->
		% If the intervals overlap we have failure: L2-1 and L2 are
		% both in D1, but only L2 is in D2.  So to proceed we need
		% H1 < L2 as well.
		H1 < L2,
		% There are some elements of D1 that are not in D2.
		IncludeExclude = 0,
		% Skip to next sub-range of D1.
		domain_inclusion_check1(Rest1, D2, IncludeExclude)
	    ; % L1>=L2 and H1=<H2
		% This sub-range of D1 is entirely contained in this
		% sub-range of D2.
		IncludeExclude = 1,
		% Skip to next sub-range of D1.
		domain_inclusion_check1(Rest1, D2, IncludeExclude)
	    )
	).


get_subdomain_bounds(Min..Max, Min, Max).
get_subdomain_bounds(SubDom, SubDom, SubDom):-
        number(SubDom).


    %
    % process_subdomains_include(+Xs, ++Lo, ++Hi, ++SubDomains, ++Type)
    %	Constrain the list of variables Xs to take their value(s) from one
    %	of the subdomains Lo..Hi, SubDomains.  SubDomains is required to be
    %	a list of elements of the form L..H, sorted in order of increasing
    %	lower bounds and with all overlapping sub-domains merged.
    %
process_subdomains_include(Xs, Lo, Hi, []) :-
	% Xs must be in the range Lo..Hi.
	vars_impose_bounds(Xs, Lo, Hi).
process_subdomains_include(Xs, Lo, Hi, [Lo0..Hi0 | Tail]) :-
	% Recursively process the domain, passing the upperbound of each
	% sub-domain as the new upperbound, then the base case can terminate
	% the recursion by setting the bounds of the variable.
        process_subdomains_include(Xs, Lo, Hi0, Tail),
        % Now exclude this gap.
        GapLo is Hi + 1,
        GapHi is Lo0 - 1,
	(
	    foreach(X, Xs),
	    param(GapLo, GapHi)
	do
	    exclude_var_range(X, GapLo, GapHi)
	).

    %
    % subdomain(++SubDomain, ?Lo, ?Hi, ?Type, ++Module)
    %	Unifies Lo and Hi with the lower and upper bounds of the subdomain
    %	SubDomain.  Also unifies Type with 'real' if SubDomain is of type
    %	real.
    %
subdomain(Lo .. Hi, Lo1, Hi1, Type, Module) :-
        !,
        bound(Lo, TypeLo, Lo1, Module),
	propagate_type(TypeLo, Type),
        bound(Hi, TypeHi, Hi1, Module),
	propagate_type(TypeHi, Type).
subdomain(X, X1, X1, Type, Module) :-
        bound(X, TypeX, X1, Module),
	propagate_type(TypeX, Type).

propagate_type(real, Result) :- -?-> !, Result = real.
propagate_type(_, _).


    %
    % exclude_var_range(?X, ++Lo, ++Hi)
    %	Excludes the values Lo..Hi inclusive from the domain of X.
exclude_var_range(X, Lo, Hi) :-
	( Lo == -1.0Inf ->
	    Hi \== 1.0Inf,  % or we'd be exluding the whole range
	    Min is Hi+1,
	    impose_min(X, Min)
	; Hi == 1.0Inf ->
	    Max is Lo-1,
	    impose_max(X, Max)
	;
	    exclude_range(X, Lo, Hi)
	).


    %
    % bound(++A, ?Type, -B, ++Module)
    %	Takes a bound from an interval domain specification A and returns
    %	its type Type and "adjusted" value B (e.g. rationals are converted to
    %	breals (floats are NOT converted as they must have been
    %   explicitly provided by the caller), and expressions are
    %   evaluated).  Type may be left unbound after the call, if A is
    %   type-neutral (e.g. infinity).
    %
bound(A, _, _, _Module) :-
	var(A), !,
	fail.
bound(A, T, B, _Module) :-
	integer(A), !,
	T = integer,
	B = A.
bound(A, T, B, _Module) :-
	rational(A), !,
	T = real,
	B is breal(A).
bound(-1.0Inf, _, B, _Module) :-
	!,
	B = -1.0Inf.
bound(1.0Inf, _, B, _Module) :-
	!,
	B = 1.0Inf.
bound(A, T, B, _Module) :-
	float(A), !,
	T = real,
	B = A.
bound(A, T, B, _Module) :-
	breal(A), !,
	T = real,
	B = A.
bound(-inf, _, B, _Module) :-
	!,
	B is -1.0Inf.
bound(inf, _, B, _Module) :-
	!,
	B = 1.0Inf.
bound(+inf, _, B, _Module) :-
	!,
	B = 1.0Inf.
bound(A, T, B, Module) :-
	% Allow expressions.
	ground(A),
        % perform safe evaluation of the domain bounds
        convert_term_constants(A,A1),
        (A == A1 ->
             % no conversion happened so just call the function with
             % is/2.  This call is necessary for when user defined
             % functions appear in the bound as convert_term_constants
             % will not call them.
             (A2 is A)@Module
        ;
             A2 = A1
        ),
	bound(A2, T, B, Module).

    %
    % fix_type(?Type)
    %	"Fixes" the type Type.  That is, if Type has not been constrained to
    %	one of `integer' or `real', then it fixes it to be `integer'.
    %
fix_type(Type) :-
	var(Type), !,
	Type = integer.
fix_type(_).

    %
    % vars_impose_bounds(+Xs, ++Lo, ++Hi)
    %	Imposes the bounds Lo..Hi on all the elements of the list Xs.
    %
vars_impose_bounds([], _Lo, _Hi).
vars_impose_bounds([X | Xs], Lo, Hi) :-
	impose_min(X, Lo),
	impose_max(X, Hi),
	vars_impose_bounds(Xs, Lo, Hi).

    %
    % impose_value(List, N)
    %   Unifies the elements of List with the value N.
    %
impose_value([], _N).
impose_value([N | Xs], N) :-
        impose_value(Xs, N).

    %
    % exclude_value(List, N)
    %   Constrains the elements of List to not equal the value N.
    %
exclude_value([], _N).
exclude_value([X | Xs], N) :-
	exclude(X, N),
        exclude_value(Xs, N).


%---------------------------------------------------------------------
%
% Utility routine to call a list of goals sequentialy
%
call_list(Goals, Module):-
	( foreach(Goal, Goals), param(Module) do call(Goal)@Module ).

%---------------------------------------------------------------------
%
% The basic continuous and discrete user-level constraints.
%
% These are normally processed away at compile time; these implementations
% are for cases where this has not occurred (e.g. higher-order calls), and
% simply do the processing at run time.
%
'*=_body'(EX, EY, Module) :-
	%conv_normalised_con((*=), EX, EY, 1, run, Module, Goals, []),
	conv_con(*=(EX, EY, 1), run, Module, Goals, []),
	call_list( Goals, Module ).

'*=_body'(EX, EY, Bool, Module) :-
	%conv_normalised_con((*=), EX, EY, Bool, run, Module, Goals, []),
	conv_con(*=(EX, EY, Bool), run, Module, Goals, []),
	call_list( Goals, Module ).

'*>=_body'(EX, EY, Module) :-
	%conv_normalised_con((*=<), EY, EX, 1, run, Module, Goals, []),
	conv_con(*=<(EY, EX, 1), run, Module, Goals, []),
	call_list( Goals, Module ).

'*>=_body'(EX, EY, Bool, Module) :-
	%conv_normalised_con((*=<), EY, EX, Bool, run, Module, Goals, []),
	conv_con(*=<(EY, EX, Bool), run, Module, Goals, []),
	call_list( Goals, Module ).

'*=<_body'(EX, EY, Module) :-
	%conv_normalised_con((*=<), EX, EY, 1, run, Module, Goals, []),
	conv_con(*=<(EX, EY, 1), run, Module, Goals, []),
	call_list( Goals, Module ).

'*=<_body'(EX, EY, Bool, Module) :-
	%conv_normalised_con((*=<), EX, EY, Bool, run, Module, Goals, []),
	conv_con(*=<(EX, EY, Bool), run, Module, Goals, []),
	call_list( Goals, Module ).

'*<_body'(EX, EY, Module) :-
	%conv_normalised_con((*<), EX, EY, 1, run, Module, Goals, []),
	conv_con(*<(EX, EY, 1), run, Module, Goals, []),
	call_list( Goals, Module ).

'*<_body'(EX, EY, Bool, Module) :-
	%conv_normalised_con((*<), EX, EY, Bool, run, Module, Goals, []),
	conv_con(*<(EX, EY, Bool), run, Module, Goals, []),
	call_list( Goals, Module ).

'*>_body'(EX, EY, Module) :-
	%conv_normalised_con((*<), EY, EX, 1, run, Module, Goals, []),
	conv_con(*<(EY, EX, 1), run, Module, Goals, []),
	call_list( Goals, Module ).

'*>_body'(EX, EY, Bool, Module) :-
	%conv_normalised_con((*<), EY, EX, Bool, run, Module, Goals, []),
	conv_con(*<(EY, EX, Bool), run, Module, Goals, []),
	call_list( Goals, Module ).

'*\\=_body'(EX, EY, Module) :-
	%conv_normalised_con((*\=), EX, EY, 1, run, Module, Goals, []),
	conv_con(*\=(EX, EY, 1), run, Module, Goals, []),
	call_list( Goals, Module ).

'*\\=_body'(EX, EY, Bool, Module) :-
	%conv_normalised_con((*\=), EX, EY, Bool, run, Module, Goals, []),
	conv_con(*\=(EX, EY, Bool), run, Module, Goals, []),
	call_list( Goals, Module ).

'#=_body'(EX, EY, Module) :-
	%conv_normalised_con((#=), EX, EY, 1, run, Module, Goals, []),
	conv_con(#=(EX, EY, 1), run, Module, Goals, []),
	call_list( Goals, Module ).

'#=_body'(EX,EY,Bool,Module) :-
	%conv_normalised_con((#=), EX, EY, Bool, run, Module, Goals, []),
	conv_con(#=(EX, EY, Bool), run, Module, Goals, []),
	call_list( Goals, Module ).

'#>=_body'(EX, EY, Module) :-
	%conv_normalised_con((#=<), EY, EX, 1, run, Module, Goals, []),
	conv_con(#=<(EY, EX, 1), run, Module, Goals, []),
	call_list( Goals, Module ).

'#>=_body'(EX, EY, Bool, Module) :-
	%conv_normalised_con((#=<), EY, EX, Bool, run, Module, Goals, []),
	conv_con(#=<(EY, EX, Bool), run, Module, Goals, []),
	call_list( Goals, Module ).

'#=<_body'(EX, EY, Module) :-
	%conv_normalised_con((#=<), EX, EY, 1, run, Module, Goals, []),
	conv_con(#=<(EX, EY, 1), run, Module, Goals, []),
	call_list( Goals, Module ).

'#=<_body'(EX, EY, Bool, Module) :-
	%conv_normalised_con((#=<), EX, EY, Bool, run, Module, Goals, []),
	conv_con(#=<(EX, EY, Bool), run, Module, Goals, []),
	call_list( Goals, Module ).

'#>_body'(EX, EY, Module) :-
	%conv_normalised_con((#=<), EY, EX - 1, 1, run, Module, Goals, []),
	conv_con(#=<(EY, EX - 1, 1), run, Module, Goals, []),
	call_list( Goals, Module ).

'#>_body'(EX,EY,Bool,Module) :-
	%conv_normalised_con((#=<), EY, EX - 1, Bool, run, Module, Goals, []),
	conv_con(#=<(EY, EX - 1, Bool), run, Module, Goals, []),
	call_list( Goals, Module ).

'#<_body'(EX, EY, Module) :-
	%conv_normalised_con((#=<), EX, EY - 1, 1, run, Module, Goals, []),
	conv_con(#=<(EX, EY - 1, 1), run, Module, Goals, []),
	call_list( Goals, Module ).

'#<_body'(EX,EY,Bool,Module) :-
	%conv_normalised_con((#=<), EX, EY - 1, Bool, run, Module, Goals, []),
	conv_con(#=<(EX, EY - 1, Bool), run, Module, Goals, []),
	call_list( Goals, Module ).

'#\\=_body'(EX, EY, Module) :-
	%conv_normalised_con((#\=), EX, EY, 1, run, Module, Goals, []),
	conv_con(#\=(EX, EY, 1), run, Module, Goals, []),
	call_list( Goals, Module ).

'#\\=_body'(EX, EY, Bool, Module) :-
	%conv_normalised_con((#\=), EX, EY, Bool, run, Module, Goals, []),
	conv_con(#\=(EX, EY, Bool), run, Module, Goals, []),
	call_list( Goals, Module ).

or_body(EX, EY, Module) :-
	conv_reified_connective_con((or), EX, EY, 1, run, Module, Goals, []),
	call_list( Goals, Module ).

or_body(EX, EY, Bool,Module) :-
	conv_reified_connective_con((or), EX, EY, Bool, run, Module, Goals, []),
	call_list( Goals, Module ).

and_body(EX, EY, Module) :-
	conv_reified_connective_con((and), EX, EY, 1, run, Module, Goals, []),
	call_list( Goals, Module ).

and_body(EX, EY, Bool,Module) :-
	conv_reified_connective_con((and), EX, EY, Bool, run, Module, Goals, []),
	call_list( Goals, Module ).

'=>_body'(EX, EY, Module) :-
	conv_reified_connective_con((=>), EX, EY, 1, run, Module, Goals,[]),
	call_list( Goals, Module ).

'=>_body'(EX, EY, Bool, Module) :-
	conv_reified_connective_con((=>), EX, EY, Bool, run, Module, Goals,[]),
	call_list( Goals, Module ).

neg_body(EX, Module) :-
	conv_neg_con(EX, 1, run, Module, Goals, []),
	call_list( Goals, Module ).

neg_body(EX, Bool, Module) :-
	conv_neg_con(EX, Bool, run, Module, Goals, []),
	call_list( Goals, Module ).


%---------------------------------------------------------------------
%
% Miscellaneous constraints.
%

    %
    % alldifferent(+Xs)
    %	Constrains all the elements of the list Xs to be pairwise distinct
    %	(and integral).
    %
alldifferent(Xs) :-
	collection_to_list(Xs, List),
	!,
	set_vars_type(List, integer),
	alldifferent(List, []),
	wake.
alldifferent(Xs) :-
	error(5, alldifferent(Xs)).

alldifferent([], _Left).
alldifferent([X | Right], Left) :-
	outof(X, Left, Right),
	alldifferent(Right, [X | Left]).

outof(X, Left, Right) :-
	var(X), !,
	suspend(outof(X, Left, Right), 3, [X->inst]).
outof(X, Left, Right) :-
	exclude_value(Left, X),
	exclude_value(Right, X).


%---------------------------------------------------------------------
%
% Simple integer labelling predicates.
%

% XXX - perhaps these should go in ic_search?

    %
    % indomain(?X)
    %	Tries to assign an integer value to X, where X is an integer or an
    %	integer variable.
    %
indomain(X) :-
	indomain_init(X, Lo),
	indomain_try(X, Lo),
	wake.


    %
    % labeling(+Xs)
    %	Xs is a list of variables to be labelled using indomain/1.
    %
labeling(Xs) :-
	collection_to_list(Xs, List),
	!,
	labeling2(List).
labeling(Xs) :-
	error(5, labeling(Xs)).

labeling2([]) ?- !.
labeling2([X|L]) ?- !,
	indomain(X),
	labeling2(L).
labeling2(Xs) :-
	error(5, labeling(Xs)).



%---------------------------------------------------------------------
%
% Nonlinear constraints are handled by spliting into simple constraints
% each of which is simple uni-directional propagators.
%


% Result iis SimpleExpr
% This is an interface to the unidirectional constraints.
% SimpleExpr can not be nested and must contain only constants
% or interval variables. iis/2 is not very useful in itself. It exists
% mainly to have nice looking delayed goals that can be called if
% necessary.

Z iis sub(X) :-
	make_suspension(sub(X, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic),
	insert_suspension(X,S,max of ic,ic),
	schedule_susp(S).
Z iis sqr(X) :-
	make_suspension(unop_function(sqr, X, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic),
	insert_suspension(X,S,max of ic,ic),
	schedule_susp(S).
Z iis sqrt(X) :-
	make_suspension(unop(sqrt, X, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic),
	insert_suspension(X,S,max of ic,ic),
	schedule_susp(S).
Z iis sin(X) :-
	make_suspension(unop_function(sin, X, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic),
	insert_suspension(X,S,max of ic,ic),
	schedule_susp(S).
Z iis cos(X) :-
	make_suspension(unop_function(cos, X, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic),
	insert_suspension(X,S,max of ic,ic),
	schedule_susp(S).
Z iis exp(X) :-
	make_suspension(unop_function(exp, X, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic),
	insert_suspension(X,S,max of ic,ic),
	schedule_susp(S).
Z iis ln(X) :-
	make_suspension(unop_function(ln, X, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic),
	insert_suspension(X,S,max of ic,ic),
	schedule_susp(S).
Z iis atan(X) :-
	make_suspension(unop(atan, X, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic),
	insert_suspension(X,S,max of ic,ic),
	schedule_susp(S).
Z iis abs(X) :-
	prop_int(X, Z),
	make_suspension(unop_function(abs, X, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic),
	insert_suspension(X,S,max of ic,ic),
	schedule_susp(S).
Z iis -X :-
	prop_int(X, Z),
	make_suspension(unop_function(neg, X, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic),
	insert_suspension(X,S,max of ic,ic),
	schedule_susp(S).
Z iis X+Y :-
	prop_int(X, Y, Z),
	V = [X|Y],
	make_suspension(binop_function(add, X, Y, Z, S), 3, S),
	insert_suspension(V,S,min of ic,ic),
	insert_suspension(V,S,max of ic,ic),
	schedule_susp(S).
Z iis X-Y :-
	prop_int(X, Y, Z),
	V = [X|Y],
	make_suspension(binop_function(sub, X, Y, Z, S), 3, S),
	insert_suspension(V,S,min of ic,ic),
	insert_suspension(V,S,max of ic,ic),
	schedule_susp(S).
Z iis X*Y :-
	prop_int(X, Y, Z),
	V = [X|Y],
	make_suspension(binop_function(mult, X, Y, Z, S), 3, S),
	insert_suspension(V,S,min of ic,ic),
	insert_suspension(V,S,max of ic,ic),
	schedule_susp(S).
Z iis X/Y :-
	V = [X|Y],
	make_suspension(binop_div(div, X, Y, Z, S), 3, S),
	insert_suspension(V,S,min of ic,ic),
	insert_suspension(V,S,max of ic,ic),
	schedule_susp(S).
Z iis min(X, Y) :-
	prop_int(X, Y, Z),
	V = [X|Y],
	make_suspension(binop_function(min, X, Y, Z, S), 3, S),
	insert_suspension(V,S,min of ic,ic),
	insert_suspension(V,S,max of ic,ic),
	schedule_susp(S).
Z iis max(X, Y) :-
	prop_int(X, Y, Z),
	V = [X|Y],
	make_suspension(binop_function(max, X, Y, Z, S), 3, S),
	insert_suspension(V,S,min of ic,ic),
	insert_suspension(V,S,max of ic,ic),
	schedule_susp(S).
Z iis +-X :-
	prop_int(X, Z),
	V = [X|Z],
	make_suspension(binop(plusminus, X, Z, Z, S), 3, S),
	insert_suspension(V,S,min of ic,ic),
	insert_suspension(V,S,max of ic,ic),
	schedule_susp(S).
Z iis (X;Y) :-
	prop_int(X, Y, Z),
	V = v(X, Y, Z),
	make_suspension(ternop(union, X, Y, Z, S), 3, S),
	insert_suspension(V,S,min of ic,ic),
	insert_suspension(V,S,max of ic,ic),
	schedule_susp(S).
Z iis X^Yi :-
	integer(Yi),
	float(Yi, Y),
	make_suspension(binop_function(pow_int, X, Y, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic),
	insert_suspension(X,S,max of ic,ic),
	schedule_susp(S).
Z iis rpow(X, Yi) :-
	integer(Yi),
	float(Yi, Y),
	ria_binop(div, 1.0, 1.0, Y, Y, YinvL, YinvH), % compute 1/Y safely
	( mod(Yi, 2, 0) -> V = [X|Z] ; V = X ),
	make_suspension(rpow(X, Yi, YinvL, YinvH, Z, S), 3, S),
	insert_suspension(V,S,min of ic,ic),
	insert_suspension(V,S,max of ic,ic),
	schedule_susp(S).
Z iis sum(X) :-
	make_suspension(sumlist(X, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic), insert_suspension(X,S,max of ic,ic),
	schedule_susp(S).


% Bi-directional iis.
biis(mono_func, Op, RevOp, X, Z) :-
	% Set initial bounds based on infinite domains, to handle partial
	% functions etc. neatly.
	ria_unop(Op, -1.0Inf, 1.0Inf, ZL1, ZH1),
	ria_unop(RevOp, -1.0Inf, 1.0Inf, XL1, XH1),
	impose_min(X, XL1),
	impose_min(Z, ZL1),
	impose_max(X, XH1),
	impose_max(Z, ZH1),
	% Set up the propagator and run it.
        make_suspension(bi_prop_mono_func(Op, RevOp, X, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic),
	insert_suspension(X,S,max of ic,ic),
	insert_suspension(Z,S,min of ic,ic),
	insert_suspension(Z,S,max of ic,ic),
	schedule_susp(S).
biis(sign_mono_func, Op, RevOp, X, Z) :-
	% Propagate integrality if appropriate.
	biis_prop_int(Op, X, Z),
	% Set initial bounds based on infinite domains, to handle partial
	% functions etc. neatly.
	ria_unop(Op, -1.0Inf, 1.0Inf, ZL1, ZH1),
	ria_binop(RevOp, -1.0Inf, 1.0Inf, -1.0Inf, 1.0Inf, XL1, XH1),
	impose_min(X, XL1),
	impose_min(Z, ZL1),
	impose_max(X, XH1),
	impose_max(Z, ZH1),
	% Set up the propagator and run it.
        make_suspension(bi_prop_sign_mono_func(Op, RevOp, X, Z, S), 3, S),
	insert_suspension(X,S,min of ic,ic),
	insert_suspension(X,S,max of ic,ic),
	insert_suspension(Z,S,min of ic,ic),
	insert_suspension(Z,S,max of ic,ic),
	schedule_susp(S).

biis_prop_int(abs, X, Z) :-
	!,
	prop_int(X, Z).
biis_prop_int(_, _X, _Z).


prop_int(X, Z) :-
	( get_solver_type(X, integer) ->
		set_var_type(Z, integer)
		% Don't wake here, wake at appropriate point in caller
	;
		true
	).

prop_int(X, Y, Z) :-
	( get_solver_type(X, integer), get_solver_type(Y, integer) ->
		set_var_type(Z, integer)
		% Don't wake here, wake at appropriate point in caller
	;
		true
	).

% Propagates integrality to Z only if all the variables in List are integral
prop_int_list([], Z) :-
		set_var_type(Z, integer).
		% Don't wake here, wake at appropriate point in caller
prop_int_list([X | Xs], Z) :-
	( get_solver_type(X, integer) ->
		prop_int_list(Xs, Z)
	;
		true
	).
	
	
	

% This is the inverse of iis/2, used to transform delayed goals
% back into readable form.

:- mode tr_op(?, -).
tr_op(sub(X, Z, _),			(Z $= sub(X))) :- !.
tr_op(unop_function(sqr, X, Z, _),	(Z $= sqr(X))) :- !.
tr_op(unop(sqrt, X, Z, _),		(Z $= sqrt(X))) :- !.
tr_op(unop_function(sin, X, Z, _),	(Z $= sin(X))) :- !.
tr_op(unop_function(cos, X, Z, _),	(Z $= cos(X))) :- !.
tr_op(unop_function(exp, X, Z, _),	(Z $= exp(X))) :- !.
tr_op(unop_function(ln, X, Z, _),	(Z $= ln(X))) :- !.
tr_op(unop(atan, X, Z, _),		(Z $= atan(X))) :- !.
tr_op(unop_function(abs, X, Z, _),	(Z $= abs(X))) :- !.
tr_op(unop_function(neg, X, Z, _),	(Z $= -X)) :- !.
tr_op(bi_unop(OpX, _OpZ, X, Z, _),	(Z $= EX)) :- !, EX =.. [OpX,X].
tr_op(binop_function(add, X, Y, Z, _),	(Z $= X+Y)) :- !.
tr_op(binop_function(sub, X, Y, Z, _),	(Z $= X-Y)) :- !.
tr_op(binop_function(mult, X, Y, Z, _),	(Z $= X*Y)) :- !.
tr_op(binop(div, X, Y, Z, _),		(Z $= X/Y)) :- !.
tr_op(binop_div(div, X, Y, Z, _),	(Z $= X/Y)) :- !.
tr_op(bi_prop_mono_func(sqr, sqrt, X, Z, _),	(X $= sqrt(Z))) :- !.
tr_op(bi_prop_mono_func(exp, ln, X, Z, _),	(Z $= exp(X))) :- !.
tr_op(bi_prop_sign_mono_func(sqr, rsqr, X, Z, _),	(Z $= sqr(X))) :- !.
tr_op(bi_prop_sign_mono_func(abs, plusminus, X, Z, _),	(Z $= abs(X))) :- !.
tr_op(binop_function(pow_int, X, Y, Z, _),	(Z $= X^Yi)) :- !, fix(Y, Yi).
tr_op(rpow(X, Yi, _, _, Z, _),		(Z $= rpow(X, Yi))) :- !.
tr_op(binop_function(min, X, Y, Z, _),	(Z $= min(X, Y))) :- !.
tr_op(binop_function(max, X, Y, Z, _),	(Z $= max(X, Y))) :- !.
tr_op(binop(plusminus, X, _, Z, _),	(Z $= +-X)) :- !.
tr_op(ternop(union, X, Y, Z, _),	(Z $= (X;Y))) :- !.
tr_op(interval_ge(X, Y, _),		(X >= Y)) :- !.
tr_op(sumlist(List, Z, _),		(Z $= sum(List))) :- !.
tr_op(unop(..., _, _, _),		'<ic nonlinear unary operator>') :- !.
tr_op(unop_function(..., _, _, _),		'<ic nonlinear unary operator>') :- !.
tr_op(bi_unop(..., _, _, _, _),		'<ic nonlinear unary operator>') :- !.
tr_op(binop(..., _, _, _, _),		'<ic nonlinear binary operator>') :- !.
tr_op(binop_function(..., _, _, _, _),		'<ic nonlinear binary operator>') :- !.
tr_op(binop_div(..., _, _, _, _),	'<ic nonlinear binary operator>') :- !.
tr_op(ternop(..., _, _, _, _),		'<ic nonlinear ternary operator>') :- !.
tr_op(ternop_function(..., _, _, _, _),		'<ic nonlinear ternary operator>') :- !.


%
% ic_flatten(++Integral, ?Expr, -Z, ++Mode)
%   Output code to evaluate the expression Z *= Expr or Z #= Expr.
%   Handles all the non-linear cases.  When adding new non-linear
%   functions, be sure to add them into the 'arith_builtin_info'
%   predicate as well so that they can be evaluated at compile time
%   as well.
%

ic_flatten(Integral, X, Z, _Mode) -->
	{var(X)},
	!,
	% make integer variable
	[ic_kernel:set_var_type(X,Integral)],
	{Z = X}.
ic_flatten(integer, X, Z, _Mode) -->
	{integer(X)},
	!,
	% only accept integers
	{Z = X}.
ic_flatten(real, X, Z, _Mode) -->
	{integer(X);breal(X)},
	!,
	% integers and breals are precise
	{Z = X}.
ic_flatten(Integral, X, Z, Mode) -->
	{number(X)},
	!,
	% any other number, convert it to a breal (should never occur?)
	{X2 is breal(X)},
        ic_flatten(Integral, X2, Z, Mode).
ic_flatten(Integral, +EX, Z, Mode) --> !,
	ic_flatten(Integral, EX, Z, Mode).
ic_flatten(Integral, -EX, Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
        /* Need to explicitly add an attribute to since it will have
                           an iis/2 suspended on it */
	[ic_kernel:set_var_type(Z,Integral)],
	[ic_constraints:(Z iis -X)],
	[ic_constraints:(X iis -Z)].
ic_flatten(Integral, eval(EX), Z, Mode) --> !,
	({Mode == compile, var(EX)} ->
            ({Integral=integer} ->
		[ic:(Z #= eval(EX))]
            ;
		[ic:(Z $= eval(EX))]
            )
	;
		ic_flatten(Integral, EX, Z, Mode)
	).
ic_flatten(integer, EX+EY, Z, _Mode) --> !,
	[ic:(Z #= EX+EY)].
ic_flatten(real, EX+EY, Z, _Mode) --> !,
	[ic:(Z $= EX+EY)].
ic_flatten(integer, EX-EY, Z, _Mode) --> !,
	[ic:(Z #= EX-EY)].
ic_flatten(real, EX-EY, Z, _Mode) --> !,
	[ic:(Z $= EX-EY)].
ic_flatten(Integral, EX*EY, Z, Mode) --> {EX \== EY}, !,
	ic_flatten(Integral, EX, X, Mode),
	ic_flatten(Integral, EY, Y, Mode),
        /* Need to explicitly add an attribute to since it will have
                           an iis/2 suspended on it */
	[ic_kernel:set_var_type(Z,Integral)],
	[ic_constraints:(Z iis X*Y)],
	[ic_constraints:(X iis Z/Y)],
	[ic_constraints:(Y iis Z/X)].
ic_flatten(Integral, EX*EY, Z, Mode) --> {EX == EY}, !,
	%% Optimise the squaring case
	ic_flatten(Integral, EX, X, Mode),
        /* Need to explicitly add an attribute to since it will have
                           an iis/2 suspended on it */
	[ic_kernel:set_var_type(Z,Integral)],
        [ic_constraints:biis(sign_mono_func, sqr, rsqr, X, Z)].
ic_flatten(Integral, EX/EY, Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
	ic_flatten(Integral, EY, Y, Mode),
        /* Need to explicitly add an attribute to since it will have
                           an iis/2 suspended on it */
	[ic_kernel:set_var_type(Z,Integral)],
	[ic_constraints:(Z iis X/Y)],
	[ic_constraints:(X iis Z*Y)],
	[ic_constraints:(Y iis X/Z)].
ic_flatten(Integral, EX^EY, Z, Mode) --> !,
	({integer(EY)} ->
		ic_flatten(Integral, EX, X, Mode),
                /* Need to explicitly add an attribute to since it will have
                           an iis/2 suspended on it */
		[ic_kernel:set_var_type(Z,Integral)],
		[ic_constraints:(Z iis X^EY)],
		[ic_constraints:(X iis rpow(Z, EY))]
	;
		ic_flatten(Integral, exp(ln(EX)*EY), Z, Mode)
	).
ic_flatten(Integral, rpow(EX, EY), Z, Mode) --> !,
	({integer(EY)} ->
	    ic_flatten(Integral, EX, X, Mode),
            /* Need to explicitly add an attribute to since it will have
                           an iis/2 suspended on it */
		[ic_kernel:set_var_type(Z,Integral)],
	    [ic_constraints:(Z iis rpow(X, EY))],
	    [ic_constraints:(X iis Z^EY)]
	;
	    ic_flatten(Integral, exp(ln(EX)/EY), Z, Mode)
	).
ic_flatten(Integral, sqr(EX), Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
        /* Need to explicitly add an attribute to since it will have
                           an iis/2 suspended on it */
	[ic_kernel:set_var_type(Z,Integral)],
        [ic_constraints:biis(sign_mono_func, sqr, rsqr, X, Z)].
ic_flatten(Integral, rsqr(EX), Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
        /* Need to explicitly add an attribute to since it will have
                           an iis/2 suspended on it */
	[ic_kernel:set_var_type(Z,Integral)],
        [ic_constraints:biis(sign_mono_func, sqr, rsqr, Z, X)].
ic_flatten(Integral, sqrt(EX), Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
        /* Need to explicitly add an attribute to since it will have
                           an iis/2 suspended on it */
	[ic_kernel:set_var_type(Z,Integral)],
	[ic_constraints:biis(mono_func, sqr, sqrt, Z, X)].
ic_flatten(Integral, sin(EX), Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
	[ic_constraints:(Z iis sin(X))].
ic_flatten(Integral, cos(EX), Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
	[ic_constraints:(Z iis cos(X))].
ic_flatten(Integral, exp(EX), Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
        /* Need to explicitly add an attribute to since it will have
                           an iis/2 suspended on it */
	[ic_kernel:set_var_type(Z,Integral)],
        [ic_constraints:biis(mono_func, exp, ln, X, Z)].
ic_flatten(Integral, ln(EX), Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
        /* Need to explicitly add an attribute to since it will have
                           an iis/2 suspended on it */
	[ic_kernel:set_var_type(Z,Integral)],
        [ic_constraints:biis(mono_func, exp, ln, Z, X)].
ic_flatten(Integral, atan(EX), Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
	[ic_constraints:(Z iis atan(X))].
ic_flatten(Integral, abs(EX), Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
        /* Need to explicitly add an attribute to since it will have
                           an iis/2 suspended on it */
	[ic_kernel:set_var_type(Z,Integral)],
        [ic_constraints:biis(sign_mono_func, abs, plusminus, X, Z)].
ic_flatten(Integral, +-(EX), Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
	/* Need to explicitly add an attribute to since it will have
                           an iis/2 suspended on it */
	[ic_kernel:set_var_type(Z,Integral)],
	[ic_constraints:(Z iis +-X)],
	[ic_constraints:(X iis +-Z)].
ic_flatten(Integral, min(EX, EY), Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
	ic_flatten(Integral, EY, Y, Mode),
	[ic_kernel:set_var_type(Z,Integral)],
	[ic:(Z =< X)],
	[ic:(Z =< Y)],
	[ic_constraints:(Z iis min(X, Y))].
ic_flatten(Integral, max(EX, EY), Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
	ic_flatten(Integral, EY, Y, Mode),
	[ic_kernel:set_var_type(Z,Integral)],
	[ic:(X =< Z)],
	[ic:(Y =< Z)],
	[ic_constraints:(Z iis max(X, Y))].
ic_flatten(Integral, sub(EX), Z, Mode) --> !,
	ic_flatten(Integral, EX, X, Mode),
	[ic_constraints:(Z iis sub(X))].
ic_flatten(Integral, sum(List), Z, Mode) --> !,
	( ic_flatten_list(Integral, List, Mode, List1) ->
            sum_as_minilists(List1, 4, Z)
	; {Mode == compile} ->
            ( {Integral=integer} ->
                [ic:(Z #= sum(List))]	% no change: relies on loop
                                        % detection
            ;
                [ic:(Z $= sum(List))]	% no change: relies on loop
                                        % detection
            )
	;
            {error(4, ic:(Z $= sum(List)))}	% should delay...
	).
ic_flatten(Integral, min(List), Z, Mode) --> !,
	( ic_flatten_list(Integral, List, Mode, List1) ->
		% Note: This not implemented via iis/2 since minlist/2 propagates
		% in both directions while iis/2 is unidirectional.
		{List1 = [X | Tail]},	% Fail if list is empty
		( {Tail = []} ->
			% Only one element, so simply unify it with Z
                    ( {Integral=integer} ->
			[ic:(Z #= X)]
                    ;
			[ic:(Z $= X)]
                    )
		;
			[ic:min(List1, Z)]
		)
	; {Mode == compile} ->
            ( {Integral=integer} ->
                [ic:(Z #= min(List))]	% no change: relies on loop
                                        % detection
            ;
                [ic:(Z $= min(List))]	% no change: relies on loop
                                        % detection
            )
	;
		[ic:min(List, Z)]	% not quite: will not eval expressions!
	).
ic_flatten(Integral, max(List), Z, Mode) --> !,
	( ic_flatten_list(Integral, List, Mode, List1) ->
		% Note: This not implemented via iis/2 since maxlist/2 propagates
		% in both directions while iis/2 is unidirectional.
		{List1 = [X | Tail]},	% Fail if list is empty
		( {Tail = []} ->
                    ( {Integral=integer} ->
			% Only one element, so simply unify it with Z
                        [ic:(Z #= X)]
                    ;
			% Only one element, so simply unify it with Z
                        [ic:(Z $= X)]
                    )
		;
			[ic:max(List1, Z)]
		)
	; {Mode == compile} ->
            ( {Integral=integer} ->
                [ic:(Z #= max(List))]	% no change: relies on loop
                                        % detection
            ;
                [ic:(Z $= max(List))]	% no change: relies on loop
                                        % detection
            )
	;
		[ic:max(List, Z)]	% not quite: will not eval expressions!
	).
ic_flatten(Integral, minlist(List), Z, Mode) --> !,
	ic_flatten(Integral, min(List), Z, Mode).
ic_flatten(Integral, maxlist(List), Z, Mode) --> !,
	ic_flatten(Integral, max(List), Z, Mode).
ic_flatten(_Integral, M:Goal, Z, _Mode) -->
	{var(Goal)},
	!,
	[Z is M:Goal].
ic_flatten(Integral, M:Goal, Z, _Mode) -->
	!,
	{
		Goal =.. ExprList,
		append(ExprList,[Z],NewExprList),
		NewGoal =.. NewExprList
	},
	[ic_kernel:set_var_type(Z,Integral),M:NewGoal].
ic_flatten(_Integral, Goal, Z, _Mode) -->
	{var(Goal)},
	!,
	[Z is Goal].
ic_flatten(Integral, Goal, Z, _Mode) -->
	!,
	{
		Goal =.. ExprList,
		append(ExprList,[Z],NewExprList),
		NewGoal =.. NewExprList
	},
	[ic_kernel:set_var_type(Z,Integral),NewGoal].


%:- mode ic_flatten_list(Integral, ?, +, -).
ic_flatten_list(_Integral, Tail, _Mode, _Flat) --> {var(Tail), !, fail}.
ic_flatten_list(_Integral, [], _Mode, []) --> {true}.
ic_flatten_list(Integral, [EX|EXs], Mode, [X|Xs]) -->
	ic_flatten(Integral, EX, X, Mode),
	ic_flatten_list(Integral, EXs, Mode, Xs).


% We break sums of long lists up into sums of (sums of ...)
% shorter lists (of length N).
sum_as_minilists(List, N, Sum) -->
	( {take_n(List, N, ListN, Remainder), Remainder = [_|_]} ->
		[ic:(Sum1 $= sum(ListN))],
		sum_as_minilists1(Remainder, N, Sums, Remainder1),
		{append(Remainder1, [Sum1 | Sums], SumList)},
		sum_as_minilists(SumList, N, Sum)
	;
		[ic:(Sum $= sum(List))]
	).

sum_as_minilists1(List, N, Sums, Remainder) -->
	( {take_n(List, N, ListN, Remainder1)} ->
		[ic:(Sum $= sum(ListN))],
		{Sums = [Sum | Sums1]},
		sum_as_minilists1(Remainder1, N, Sums1, Remainder)
	;
		{
			Remainder = List,
			Sums = []
		}
	).

take_n(List, N, ListN, Remainder) :-
	(	count(_, 1, N),
		fromto(List, InList, OutList, Remainder),
		foreach(X, ListN)
	do
		InList = [X | OutList]
	).


% The internal unidirectional constraints, generic for each arity.
% They have exactly one output variable (Z), and delay on their
% input variables (X, Y). Redelay is with lower priority when no
% propagation was achieved. That has quite a performance impact!

:- demon(sub/3).
sub(X, Z, S) :-				% Z := sub(X)
	ic_event(ic_uni_prop),
	get_float_bounds(X, L, H),
	check_update(Z, L, H, F, P),
	( var(X) -> set_suspension_data(S, priority, P), Flag=F
	; kill_suspension(S), Flag=4 ),
	do_update(Z, L, H, Flag).

:- demon(unop/4).
unop(Op, X, Z, S) :-			% Z := op(X)
	ic_event(ic_uni_prop),
	get_float_bounds(X, XL, XH),
	tr_ria_unop(Op,RiaOp),
	ria_unop(RiaOp, XL, XH, L, H),
	check_update(Z, L, H, F, P),
	( var(X) -> set_suspension_data(S, priority, P)
        ; var(Z) -> set_suspension_data(S, priority, P)
	; kill_suspension(S)),
	do_update(Z, L, H, F).

:- demon(unop_function/4).
unop_function(Op, X, Z, S) :-			% Z := op(X)
        % grounds Z when X is ground
	ic_event(ic_uni_prop),
	get_float_bounds(X, XL, XH),
	tr_ria_unop(Op,RiaOp),
	ria_unop(RiaOp, XL, XH, L, H),
	check_update(Z, L, H, F, P),
	( var(X) -> set_suspension_data(S, priority, P), Flag=F
	; kill_suspension(S), Flag=4 ),
	do_update(Z, L, H, Flag).


    % bi_prop_mono_func/5:
    %	Bi-directional propagator for a (strictly) monotonic function (e.g.
    %	exp/ln, sqr/sqrt).
    %	i.e. Z := Op(X) and X := RevOp(Z) where RevOp is the inverse of Op.
    %	Op and RevOp are both assumed to be unary functions.
:- demon(bi_prop_mono_func/5).
bi_prop_mono_func(Op, RevOp, X, Z, S) :-
	ic_event(ic_uni_prop),
	( nonvar(X) ->
	    % X ground: Compute ground Z and unify with Z; kill constraint.
	    kill_suspension(S),
	    get_float_bounds(X, XL, XH),
	    ria_unop(Op, XL, XH, ZL, ZH),
	    do_update(Z, ZL, ZH, 4)
	; nonvar(Z) ->
	    % Z ground: Compute ground X and unify with X; kill constraint.
	    kill_suspension(S),
	    get_float_bounds(Z, ZL, ZH),
	    ria_unop(RevOp, ZL, ZH, XL, XH),
	    do_update(X, XL, XH, 4)
	;
	    % Just update bounds of both variables...
	    % XXX - Better propagation if we serialise these?
	    get_float_bounds(X, XL, XH),
	    ria_unop(Op, XL, XH, ZL1, ZH1),
	    get_float_bounds(Z, ZL, ZH),
	    ria_unop(RevOp, ZL, ZH, XL1, XH1),
	    check_update(X, XL1, XH1, XF, XP),
	    check_update(Z, ZL1, ZH1, ZF, ZP),
	    P is min(XP, ZP),
	    set_suspension_data(S, priority, P),
	    do_update(X, XL1, XH1, XF),
	    do_update(Z, ZL1, ZH1, ZF)
	).


    % bi_prop_sign_mono_func/5:
    %	Bi-directional propagator for a function which is (strictly)
    %	monotonic if the sign of X is known (e.g. abs/+-, sqr/rsqr).
    %	i.e. Z := Op(X) and X := RevOp(Z) where RevOp is the inverse of Op.
    %	Op is assumed to be a unary function, while RevOp is assumed to be a
    %	binary function.
:- demon(bi_prop_sign_mono_func/5).
bi_prop_sign_mono_func(Op, RevOp, X, Z, S) :-
	ic_event(ic_uni_prop),
	get_float_bounds(X, XL, XH),
	( nonvar(X) ->
	    % X ground: Compute ground Z and unify with Z; kill constraint.
	    kill_suspension(S),
	    ria_unop(Op, XL, XH, ZL, ZH),
	    do_update(Z, ZL, ZH, 4)
	; nonvar(Z) ->
	    % Z ground: Compute X and see whether sign known.
	    get_float_bounds(Z, ZL, ZH),
	    ria_binop(RevOp, ZL, ZH, XL, XH, XL1, XH1),
	    ( XL1 * XH1 >= 0 ->
		% Sign known: can ground X and kill constraint.
		do_update(X, XL1, XH1, 4),
		kill_suspension(S)
	    ;
		% Sign unknown: just propagate to X.
		check_update(X, XL1, XH1, F, P),
		set_suspension_data(S, priority, P),
		do_update(X, XL1, XH1, F)
	    )
	;
	    % Just update bounds of both variables...
	    % XXX - Better propagation if we serialise these?
	    ria_unop(Op, XL, XH, ZL1, ZH1),
	    get_float_bounds(Z, ZL, ZH),
	    ria_binop(RevOp, ZL, ZH, XL, XH, XL1, XH1),
	    check_update(X, XL1, XH1, XF, XP),
	    check_update(Z, ZL1, ZH1, ZF, ZP),
	    P is min(XP, ZP),
	    set_suspension_data(S, priority, P),
	    do_update(X, XL1, XH1, XF),
	    do_update(Z, ZL1, ZH1, ZF)
	).


:- demon(binop/5).
binop(Op, X, Y, Z, S) :-			% Z := op(X, Y)
	ic_event(ic_bin_prop),
	get_float_bounds(X, XL, XH),
	get_float_bounds(Y, YL, YH),
	tr_ria_binop(Op,RiaOp),
	ria_binop(RiaOp, XL, XH, YL, YH, L, H),
	check_update(Z, L, H, F, P),
	( var(X) -> set_suspension_data(S, priority, P)
	; var(Y) -> set_suspension_data(S, priority, P)
	; var(Z) -> set_suspension_data(S, priority, P)
	; kill_suspension(S)),
	do_update(Z, L, H, F).

:- demon(binop_function/5).
binop_function(Op, X, Y, Z, S) :-		% Z := op(X, Y)
        % grounds Z when X and Y are ground
	ic_event(ic_bin_prop),
	get_float_bounds(X, XL, XH),
	get_float_bounds(Y, YL, YH),
	tr_ria_binop(Op,RiaOp),
	ria_binop(RiaOp, XL, XH, YL, YH, L, H),
	check_update(Z, L, H, F, P),
	( var(X) -> set_suspension_data(S, priority, P), Flag=F
	; var(Y) -> set_suspension_data(S, priority, P), Flag=F
	; kill_suspension(S), Flag=4 ),
	do_update(Z, L, H, Flag).

:- demon(binop_div/5).
binop_div(Op, X, Y, Z, S) :-			% Z := /(X, Y)
	ic_event(ic_bin_prop),
	get_float_bounds(X, XL, XH),
	get_float_bounds(Y, YL, YH),
	tr_ria_binop(Op,RiaOp),
	ria_binop(RiaOp, XL, XH, YL, YH, L, H),
	check_update(Z, L, H, F, P),
	( var(X) -> set_suspension_data(S, priority, P), Flag=F
	; var(Y) -> set_suspension_data(S, priority, P), Flag=F
	; not Y =\= 0 ->
            % delay if Z := X/0, where X is not 0
            ( not X =\= 0 ->                 % X =:= 0
                kill_suspension(S), Flag = F
            ;
                set_suspension_data(S, priority, P), Flag=F
            )
        ; kill_suspension(S), Flag=4 ),
	do_update(Z, L, H, Flag).

:- demon(ternop/5).
ternop(Op, X, Y, Z, S) :-			% Z := op(X, Y, Z)
	ic_event(ic_tern_prop),
	get_float_bounds(X, XL, XH),
	get_float_bounds(Y, YL, YH),
	get_float_bounds(Z, ZL, ZH),
	% delays on X, Y and Z (because of result intersection)
	tr_ria_ternop(Op,RiaOp),
	ria_ternop(RiaOp, XL, XH, YL, YH, ZL, ZH, L, H),
	check_update(Z, L, H, F, P),
	( var(X) -> set_suspension_data(S, priority, P)
	; var(Y) -> set_suspension_data(S, priority, P)
	; var(Z) -> set_suspension_data(S, priority, P)
	; kill_suspension(S) ),
	do_update(Z, L, H, F).

:- demon(ternop_function/5).
ternop_function(Op, X, Y, Z, S) :-		% Z := op(X, Y, Z)
        % grounds Z when X and Y are ground
	ic_event(ic_tern_prop),
	get_float_bounds(X, XL, XH),
	get_float_bounds(Y, YL, YH),
	get_float_bounds(Z, ZL, ZH),
	% delays on X, Y and Z (because of result intersection)
	tr_ria_ternop(Op,RiaOp),
	ria_ternop(RiaOp, XL, XH, YL, YH, ZL, ZH, L, H),
	check_update(Z, L, H, F, P),
	( var(X) -> set_suspension_data(S, priority, P), Flag=F
	; var(Y) -> set_suspension_data(S, priority, P), Flag=F
	; kill_suspension(S), Flag=4 ),
	do_update(Z, L, H, Flag).


:- demon(rpow/6).
rpow(X, N, YL, YH, Z, S) :-		% reverse of X = Z^N, YL..YH = 1/N
	ic_event(ic_tern_prop),
	get_float_bounds(X, XL, XH),
	get_float_bounds(Z, ZL, ZH),
	( mod(N, 2, 0) ->
	    % delays on X and Z (because of result intersection)
	    ria_ternop(rpow_even, XL, XH, YL, YH, ZL, ZH, L, H)
	;
	    % delays on X only
	    ria_binop(rpow_odd, XL, XH, YL, YH, L, H)
	),
	check_update(Z, L, H, F, P),
	( var(X) -> 
            set_suspension_data(S, priority, P), Flag=F
        ; ( mod(N, 2, 0), get_threshold(T), H-L > T) ->
            % do not instantiate to a bounded real if the power is
            % even and the interval is larger than the threshold
            set_suspension_data(S, priority, P), Flag=F
        ;
            kill_suspension(S), Flag=4
        ),
	do_update(Z, L, H, Flag).

:- demon(sumlist/3).
sumlist(List, Z, S) :-			% Z := sum(List)
	ic_event(ic_bin_prop),
	ic_sumlist(List, 0.0, 0.0, L, H, VarFlag),
	check_update(Z, L, H, F, P),
	( nonvar(VarFlag) -> set_suspension_data(S, priority, P), Flag=F
	; kill_suspension(S), Flag=4 ),
	do_update(Z, L, H, Flag).

    :- mode ic_sumlist(+, +, +, -, -, -).
    ic_sumlist([], L, H, L, H, _).
    ic_sumlist([T|Ts], L0, H0, L, H, VarFlag) :-
	sumop(T, Op, X, VarFlag),
	get_float_bounds(X, XL, XH),
	ria_binop(Op, L0, H0, XL, XH, L1, H1),
	ic_sumlist(Ts, L1, H1, L, H, VarFlag).

    :- mode sumop(?, -, -, -).
    sumop(X,    0, X, yes)     :- var(X), !.
    sumop(X,    0, X, _)       :- number(X).
    sumop(-(X), 1, X, VarFlag) :- ( var(X) -> VarFlag = yes ; true ).


	%
	% min(Xs, Min)
	%	Propagates such that 'Min' is the smallest of the elements 
	%	of list 'Xs'.
	%	Does not allow expressions in the argument list -
	%	flatten beforehand!
	%
min(Xs, Min) :-
	var(Xs),
	!,
	suspend(min(Xs,Min), 4, Xs->inst).
min(Xs, Min) :-
	collection_to_list(Xs, List),
	List \== [],
	!,
	min2(List, Min).
min(Xs, Min) :-
	error(5, min(Xs, Min)).

min2(Xs, Min) :-
	get_bounds(Min, MinL, MinH),
	XLMinStart is 1.0Inf,
	(
	    foreach(X, Xs),
	    fromto(XLMinStart, XLmin0, XLmin1, XLmin),
	    fromto(MinH, XHmin0, XHmin1, XHmin),
	    param(MinL),
	    fromto(none, MinXH0, MinXH1, MinXH),
	    fromto(none, MinXL0, MinXL1, MinXL),
	    fromto(Ys, Ys1, Ys0, [])
	do
	    get_bounds(X, XL, XH),
	    ( XL < MinL ->      % Min may constrain variable in list
		impose_min(X, MinL),	% X *>= exact(MinL)
		% Lower bound of X has changed - need to get its new lower
		% bound in case it's greater than MinL - can't just assume
		% that the new minimum is MinL.
		get_min(X, XL1)
	    ;
		XL1 = XL
	    ),
	    ( XL1 < XLmin0 ->
		XLmin1 = XL1,    % it's the new minimum
		MinXL1 = X
	    ;
		XLmin1 = XLmin0,
		MinXL1 = MinXL0
	    ),
	    ( XH < XHmin0 ->    % compute lowest XH
		XHmin1 = XH,
		MinXH1 = X
	    ;
		XHmin1 = XHmin0,
		MinXH1 = MinXH0
	    ),
	    ( XL > XHmin1 ->
		Ys1 = Ys0       % X can be ignored from now on
	    ;
		Ys1 = [X|Ys0]
	    )
	),
	%% Filter out any values greater than the new upper bound
	(
	    foreach(Y,Ys),
	    fromto(YsFiltered,YsFiltered1,YsFiltered0,[]),
	    param(XHmin), param(NoValidVarBound)
	do
	    get_bounds(Y, YL, _YH),
	    ( YL > XHmin ->
		YsFiltered1 = YsFiltered0
	    ;
		( var(Y), YL < XHmin ->
		    NoValidVarBound = false
		;
		    true
		),
		YsFiltered1 = [Y|YsFiltered0]
	    )
	),
	% Only propagate integrality if the target is still a variable (we
	% shouldn't fail just because the target is of the wrong type).
	( var(Min) ->
	    prop_int_list(YsFiltered,Min)
	;
	    true
	),
	impose_min(Min, XLmin),
	impose_max(Min, XHmin),
	( YsFiltered = [LastX] ->
	    LastX *= Min
	; (nonvar(MinXL),nonvar(MinXH), NoValidVarBound \== false) -> % implies nonvar(Min)
	    %% ground Min to its current interval
	    (nonvar(Min) ->
		true
	    ;
		get_float_bounds(Min,ZMin,ZMax),
		Min is breal_from_bounds(ZMin,ZMax)
	    )
	;
	    FMinL is float(MinL),
	    FMinH is float(MinH),
	    check_update(Min, FMinL, FMinH, _Flag, Priority),
	    P2 is Priority+1,
	    make_suspension(min2(YsFiltered, Min), P2, S),
	    insert_suspension(MinXL, S, min of ic, ic),
	    insert_suspension(YsFiltered, S, max of ic, ic),
	    insert_suspension(Min, S, min of ic, ic),
	    insert_suspension(Min, S, max of ic, ic)
	),
	wake.


	%
	% max(Xs, Max)
	%	Propagates such that 'Max' is the largest of the elements 
	%	of list 'Xs'.
	%	Does not allow expressions in the argument list -
	%	flatten beforehand!
	%
max(Xs, Max) :-
	var(Xs),
	!,
	suspend(max(Xs,Max), 4, Xs->inst).
max(Xs, Max) :-
	collection_to_list(Xs, List),
	List \== [],
	!,
	max2(List, Max).
max(Xs, Max) :-
	error(5, max(Xs, Max)).

max2(Xs, Max) :-
	get_bounds(Max, MaxL, MaxH),
	XHMaxStart is -1.0Inf,
	(
	    foreach(X, Xs),
	    fromto(XHMaxStart, XHmax0, XHmax1, XHmax),
	    fromto(MaxL, XLmax0, XLmax1, XLmax),
	    param(MaxH),
	    fromto(none, MaxXH0, MaxXH1, MaxXH),
	    fromto(none, MaxXL0, MaxXL1, MaxXL),
	    fromto(Ys, Ys1, Ys0, [])
	do
	    get_bounds(X, XL, XH),
	    ( XH > MaxH ->      % Max may constrain variable in list
		impose_max(X, MaxH),	% X *=< exact(MaxH)
		% Upper bound of X has changed - need to get its new upper
		% bound in case it's less than MaxH - can't just assume
		% that the new minimum is MaxH.
		get_max(X, XH1)
	    ;
		XH1 = XH
	    ),
	    ( XH1 > XHmax0 ->
		XHmax1 = XH1,    % it's the new maximum
		MaxXH1 = X
	    ;
		XHmax1 = XHmax0,
		MaxXH1 = MaxXH0
	    ),
	    ( XL > XLmax0 ->    % compute highest XL
		XLmax1 = XL,
		MaxXL1 = X
	    ;
		XLmax1 = XLmax0,
		MaxXL1 = MaxXL0
	    ),
	    ( XH < XLmax1 ->
		Ys1 = Ys0       % X can be ignored from now on
	    ;
		Ys1 = [X|Ys0]
	    )
	),
	%% Filter out any values less than the new lower bound
	(
	    foreach(Y,Ys),
	    fromto(YsFiltered,YsFiltered1,YsFiltered0,[]),
	    param(XLmax), param(NoValidVarBound)
	do
	    get_bounds(Y, _YL, YH),
	    ( YH < XLmax ->
		YsFiltered1 = YsFiltered0
	    ;
		( var(Y), YH > XLmax ->
		    NoValidVarBound = false
		;
		    true
		),
		YsFiltered1 = [Y|YsFiltered0]
	    )
	),
	% Only propagate integrality if the target is still a variable (we
	% shouldn't fail just because the target is of the wrong type).
	( var(Max) ->
	    prop_int_list(YsFiltered,Max)
	;
	    true
	),
	impose_min(Max, XLmax),
	impose_max(Max, XHmax),
	( YsFiltered = [LastX] ->
	    LastX *= Max
	;(nonvar(MaxXH),nonvar(MaxXL), NoValidVarBound \== false) -> % implies nonvar(Max)
	    %% ground Max to its current interval
	    (nonvar(Max) ->
		true
	    ;
		get_float_bounds(Max,ZMin,ZMax),
		Max is breal_from_bounds(ZMin,ZMax)
	    )
	;
	    FMaxL is float(MaxL),
	    FMaxH is float(MaxH),
	    check_update(Max, FMaxL, FMaxH, _Flag, Priority),
	    P2 is Priority+1,
	    make_suspension(max2(YsFiltered, Max), P2, S),
	    insert_suspension(MaxXH, S, max of ic, ic),
	    insert_suspension(YsFiltered, S, min of ic, ic),
	    insert_suspension(Max, S, min of ic, ic),
	    insert_suspension(Max, S, max of ic, ic)
	),
	wake.


% Obsolete

minlist(Xs, Min) :-
	min(Xs, Min).

maxlist(Xs, Max) :-
	max(Xs, Max).


% Auxiliaries

:- mode check_update(?, +, +, -, -).
check_update(X, L, H, Flag, Prio) :-
	get_float_bounds(X, XL, XH),
	ria_binop(min_delta, XL, XH, L, H, LD, HD),
	get_threshold(T),
	( LD > T ->
	    ( HD > T ->
		Flag = 3, Prio = 3
	    ;
		Flag = 1, Prio = 3
	    )
	; HD > T ->
		Flag = 2, Prio = 3
	;
		Flag = 0, Prio = 4
	).

:- mode do_update(?, +, +, +).
do_update(_, _, _, 0).
do_update(X, L, _, 1) :- impose_min(X, L), wake.
do_update(X, _, H, 2) :- impose_max(X, H), wake.
do_update(X, L, H, 3) :- impose_min(X, L), impose_max(X, H), wake.
do_update(X, L, H, 4) :-
        Temp is breal_from_bounds(L, H),
	( var(X) ->
	    % Try imposing the bounds.
	    impose_min(X, L), impose_max(X, H),
	    ( var(X) ->
		% If X is still a var, unify it.
		( get_solver_type(X, integer) ->
		    op_to_flags($=, Flags, _),	% XXX - should process at compile-time.
		    ic_lin_con(Flags, 1, [1 * X, -1 * Temp])
		;
		    X = Temp
		)
	    ;
		X =:= Temp
	    ),
	    wake
	;
	    % X is already ground, so just compare it to what it's supposed
	    % to be.
	    X =:= Temp
	).



% ----------------------------------------------------------------------
% Implementation of element/3, originally based on code from Stefano.
%
% For speed, it's probably worth implementing some of this constraint in C,
% like the FD library does, but it would be a non-trivial amount of work to
% understand the FD library's C implementation, let alone adapt it for IC.
%

% Don't load lib(hash) unless element/3 is actually called...
%:- lib(hash).

:- local struct(element_data(index_list, value_list)).

element(Index, ListOrVector, Value):-
	ground(ListOrVector),
	!,
	( ListOrVector = [_|_] ->
	    ListOrVector = List
	;
	    ListOrVector = Vector
	),
	Vector =.. [[] | List],
	arity(Vector, Length),
	Index :: 1..Length,
	sort(List, ValueList),
	Value :: ValueList,
	% Make a hash table that stores the supports Val->SupportingIndexes
	hash:hash_create(Hash),
	% We do everything in reverse here so that the list entries in the
	% hash table end up being sorted in ascending order.
	(
	    for(I, Length, 1, -1),  % Count down so the hash entries are sorted
	    param(Hash,Vector)
	do
	    arg(I, Vector, Val),
	    ( hash:hash_update(Hash, Val, L, L1) ->
		L1 = [I|L]
	    ;
		hash:hash_add(Hash, Val, [I])
	    )
	),
	% Store the initial index and value domains
	length(ValueList, NValues),
	ElementData = element_data{
		    index_list:dom([1..Length],Length),
		    value_list:dom(ValueList,NValues)
		},
	% Call propagator in case Index or Value had a previous domain
	element(Index, Vector, Hash, Value, ElementData, Susp).
element(Index, List, Value):-
	error(4, element(Index, List, Value)).


:- demon(element/6).
element(Index, Vector, Hash, Value, ElementData, Susp) :-
	( integer(Index) ->
	    kill_suspension(Susp),
	    arg(Index, Vector, Value)

	; nonvar(Value) ->
	    kill_suspension(Susp),
	    hash:hash_find(Hash, Value, Possible),
	    Index :: Possible

	;
	    ElementData = element_data{
			index_list:IndexDom0,
			value_list:ValueDom0
		    },

	    % Propagate from Index to Value.
	    get_canonical_domain(Index, IndexDom1),
	    ( sepia_kernel:dom_difference(IndexDom0, IndexDom1, IndexDiffDom, _) ->
		IndexDiffDom = dom(IndexDiff,_),
		(
		    fromto(IndexDiff,[IR|IRs0],IRs,[]),
		    param(Hash, Vector, Value)
		do
		    ( IR = I..IH ->
			I1 is I+1,
			( I1 < IH -> IRs = [I1..IH|IRs0] ; IRs = [IH|IRs0] )
		    ;
			I = IR, IRs = IRs0
		    ),
		    % Value I was removed from the domain of Index
		    % Update the support table
		    arg(I, Vector, Val),
		    hash:hash_update(Hash, Val, Indices, RestIndices),
		    once delete(I, Indices, RestIndices),
		    ( RestIndices == [] ->
			% Val lost all supports, we can prune Value's domain
			exclude(Value, Val)
		    ;
			true
		    )
		)
	    ;
		true
	    ),

	    % Propagate from Value to Index.
	    get_canonical_domain(Value, ValueDom2),
	    ( sepia_kernel:dom_difference(ValueDom0, ValueDom2, ValueDiffDom, _) ->
		ValueDiffDom = dom(ValueDiff,_),
		(
		    fromto(ValueDiff,[VR|VRs0],VRs,[]),
		    param(Hash, Index, IndexReduced)
		do
		    ( VR = Val..VH ->
			V1 is Val+1,
			( V1 < VH -> VRs = [V1..VH|VRs0] ; VRs = [VH|VRs0] )
		    ;
			Val = VR, VRs = VRs0
		    ),
		    % Val was removed from Value's domain
		    hash:hash_find(Hash, Val, NotEq),
		    ( NotEq == [] ->
			% Val was just deleted by the forward pass above!
			true
		    ;
		        % we can prune all the corresponding Index values
			IndexReduced = true,
			(
			    foreach(N, NotEq),
			    param(Index)
			do
			    exclude(Index, N)
			)
		    )
		)
	    ;
		true
	    ),

	    % kill or re-suspend
	    ( nonvar(Index) ->
		kill_suspension(Susp)
	    ; nonvar(Value) ->
		kill_suspension(Susp)
	    ;
		( var(Susp) ->
		    Vars = [Index|Value],
		    suspend(element(Index, Vector, Hash, Value, ElementData, Susp),
			    3, [Vars->min, Vars->max, Vars->hole], Susp)
		;
		    true
		),
		% remember the new current domains
		( var(IndexReduced) ->
		    IndexDom2 = IndexDom1
		;
		    get_canonical_domain(Index, IndexDom2)
		),
		setarg(index_list of element_data, ElementData, IndexDom2),
		setarg(value_list of element_data, ElementData, ValueDom2)
	    ),
	    wake
	).


% return a lib(fd)-compatible domain representation
% for use with dom_difference/4
get_canonical_domain(Value, dom(Dom,Size)) :-
    	get_domain(Value, Dom0),
    	get_domain_size(Value, Size),
	( Dom0 = [_|_] -> Dom = Dom0 ; Dom = [Dom0] ).


%---------------------------------------------------------------------
%
% Arc-consistent equality primitive.
%

    %
    % ac_eq(?X, ?Y, ++C)
    %   Arc-consistent implementation of X #= Y + C where C is a constant.
    %   X and Y are given bitmaps if they don't have them already, and are
    %   given reasonable bounds...
    %   XXX - allocating a bitmap with these "reasonable" bounds will almost
    %   certainly blow the global stack: probably should fail more
    %   gracefully in such a case.
    %
ac_eq(X, Y, C) :-
	integer(C),
	!,
	integers([X, Y]),
	ac_eq1(X, Y, C).
ac_eq(X, Y, C) :-
	var(C),
	!,
	% instantiation fault
	error(4, ac_eq(X, Y, C)).
ac_eq(X, Y, C) :-
	% type error
	error(5, ac_eq(X, Y, C)).

ac_eq1(X, Y, C) :-
	nonvar(X),
	!,
	integer(X),
	Y is X - C.
ac_eq1(X, Y, C) :-
	nonvar(Y),
	!,
	integer(Y),
	X is Y + C.
ac_eq1(X, Y, C) :-
	ac_eq_init(X, Y, C),
	( var(X), var(Y) ->
	    Vars = [X|Y],
	    suspend(ac_eq_prop(X, Y, C, S), 3,
		    [Vars->min, Vars->max, Vars->hole], S)
	;
	    true
	).


% ------------------- Piecewise linear constraint -----------------------

    % The following (non-exported) predicates are used by the testing
    % modules, which may need to be updated if they change:
    %	points_to_chunks/2
    %	unpack_point/3

% The piecewise linear constraint is implemented in two separate layers.
% The lower layer handles only connected "chunks" of the piecewise
% constraint; its knowledge of discontinuities is limited to its endpoints.
% The upper layer splits the original constraint at each discontinuity to
% create a set of such chunks, passes each of them to the lower layer, and
% manages the results.


    % These predicates are used by the convex hull piecewise constraint
    % in ic_eplex_relax.
:- export piecewise_linear_2/4.
:- export fx/7.
:- export interpolate/6.

    % Structure for representing a chunk.
    %
    % points:   the points defining the piecewise linear function
    % left:     properties of the left end of the chunk
    % right:    properties of the right end of the chunk
    %
    % The fields `left' and `right' are one of:
    %
    % infinite: the chunk extends to infinity beyond this end
    % closed:   the chunk includes this end point
    % open:     the chunk excludes this end point
    %
    % Note that points are represented as bounded reals (no integers or
    % floating point numbers), to ensure that any arithmetic done is safe.

:- export struct(chunk(points, left, right)).

%%% --- upper layer --- %%%

    % Converts the list of points to a list of chunks, sets up constraints
    % for each chunk, and then constrains X and Y to have bounds
    % corresponding to the min and max feasible values from the chunks.
piecewise_linear(X, Points, Y) :-
	collection_to_list(Points, List),
	!,
	piecewise_linear_2(X, List, Y, _).
piecewise_linear(X, Points, Y) :-
	error(5, piecewise_linear(X, Points, Y)).

piecewise_linear_2(X, Points, Y, Chunks) :-
	( Points = [_ | _] ->
	    true
	;
	    printf(error, "piecewise_linear: must have at least one input point.\n", []),
	    abort
	),

	points_to_chunks(Points, Chunks),

	(
	    foreach(Chunk, Chunks),
	    foreach(XLo, XLos),
	    foreach(XHi, XHis),
	    foreach(YLo, YLos),
	    foreach(YHi, YHis)
	do
	    process_chunk(Chunk, XLo, XHi, YLo, YHi)
	),

	min(XLos, X),
	max(XHis, X),
	min(YLos, Y),
	max(YHis, Y).


points_to_chunks(Points, Chunks) :-
	basic_points_to_chunks(Points, Chunks0),
	check_sorted_and_trim(Chunks0, Chunks1),
	chunk_points_to_arrays(Chunks1, Chunks).


    % Processes the list of input points into a list of chunks, converting
    % all constants it finds into bounded reals as it goes, and flagging any
    % errors it finds.
basic_points_to_chunks([P | Ps], Chunks) :-
	unpack_point(P, Point, Tag),
	( Tag == '=' ->
	    Point = (X, _),
	    basic_points_to_chunks_2(X, P, Ps, [Point | ChunkTail],
		    ChunkTail, infinite, Chunks)
	;
	    illegal_annotation_error(P)
	).

basic_points_to_chunks_2(_, _, [], ChunkPoints, [], EndLeft,
	    [chunk with [points:ChunkPoints, left:EndLeft, right:infinite]]).
basic_points_to_chunks_2(PrevX, PrevP, [P | Ps], ChunkPoints, ChunkTail,
		EndLeft, Chunks) :-
	unpack_point(P, Point, Tag),
	Point = (X, _),

	( PrevX == X ->
	    ( Tag == > ->
		ChunkTail = [],
		Chunks = [chunk with [
			    points:ChunkPoints, left:EndLeft, right:closed
			] | Chunks1],
		basic_points_to_chunks_2(X, P, Ps, [Point | ChunkTail1],
				ChunkTail1, open, Chunks1)
	    ;
		discontinuity_form_error(PrevP, P)
	    )
	; Tag == > ->
	    ( Ps = [P1 | _], unpack_point(P1, (X1, _), _), X == X1 ->
		discontinuity_form_error(P, P1)
	    ;
		illegal_annotation_error(P)
	    )
	; Tag == < ->
	    ChunkTail = [Point],
	    Chunks = [chunk with [points:ChunkPoints, left:EndLeft, right:open]
		    | Chunks1],
	    (
		Ps = [P1 | Ps1],
		unpack_point(P1, Point1, Tag1),
		Point1 = (X1, _),
		X == X1
	    ->
		( Tag1 = = ->
		    basic_points_to_chunks_2(X, P1, Ps1,
			    [Point1 | ChunkTail1], ChunkTail1, closed,
			    Chunks1)
		;
		    discontinuity_form_error(P, P1)
		)
	    ;
		illegal_annotation_error(P)
	    )
	; % Tag == '='
	    ChunkTail = [Point | ChunkTail1],
	    basic_points_to_chunks_2(X, P, Ps, ChunkPoints, ChunkTail1,
		    EndLeft, Chunks)
	).


    % "Unpacks" a point; that is, separates any annotation from its X and Y
    % coordinates.  It also makes sure all coordinates are integers or
    % bounded reals, and flags any errors.
    % `Tag' is one of `<', `>', or `=' (no tag/point included).
unpack_point(P, (X_out, Y_out), Tag) :-
	(
	    P = (X_in, Y_in),
	    Y_out is breal(Y_in),
	    unpack_x(X_in, X_out, Tag)
	->
	    true
	;
	    printf(error, "piecewise_linear: %q: illegal point specification.\n", [P]),
	    abort
	).

    % Extract the tag from an X coordinate.
unpack_x(X_in, X_out, =) :-
	number(X_in),
	X_out is breal(X_in).
unpack_x(X_in, X_out, <) :-
	X_in = <(X_tmp),
	X_out is breal(X_tmp).
unpack_x(X_in, X_out, >) :-
	X_in = >(X_tmp),
	X_out is breal(X_tmp).


    % Check that the X coordinates are monotonically increasing.  If
    % adjacent coordinates overlap, set up appropriate delayed goals.  Also,
    % trim any X intervals if they contain any regions which obviously
    % violate the sortedness constraint (e.g. if we require X1 < X2 and X2's
    % lower bound is less than X1's, trim X2 so that this is no longer
    % true).
check_sorted_and_trim(Chunks0, Chunks) :-
	(
	    foreach(Chunk0, Chunks0)
	do
	    check_sorted(Chunk0)
	),
	trim_coords(Chunks0, Chunks).

check_sorted(Chunk) :-
	Chunk = chunk with [points:[(First, _) | Rest]],
	(
	    foreach((X, _), Rest),
	    fromto(First, Prev, X, _)
	do
	    Prev < X	% Will set up a delayed goal for overlaps.
	).

trim_coords(Chunks0, Chunks) :-
	trim_lower_bounds(Chunks0, RevChunks),
	trim_upper_bounds(RevChunks, Chunks).

trim_lower_bounds(Chunks, RevChunks) :-
	Chunks = [chunk with [points:[(X0, _) | _]] | _],
	(
	    foreach(Chunk, Chunks),
	    fromto([], RevCTail, [RevChunk | RevCTail], RevChunks),
	    fromto(X0, FirstX, LastX, _)
	do
	    Chunk = chunk with [points:Points],
	    Points = [(_, Y0) | Rest],
	    (
		foreach(Point0, Rest),
		fromto([(FirstX, Y0)], RevPTail, [Point | RevPTail], RevPoints),
		fromto(FirstX, PrevX, NextX, LastX)
	    do
		Point0 = (X0, Y),
		PrevLwb is breal_min(PrevX),
		( PrevLwb > breal_min(X0) ->
		    NextX is breal_from_bounds(PrevLwb, breal_max(X0)),
		    Point = (NextX, Y)
		;
		    Point = Point0,
		    NextX = X0
		)
	    ),
	    update_struct(chunk, [points:RevPoints], Chunk, RevChunk)
	).

trim_upper_bounds(RevChunks, Chunks) :-
	RevChunks = [chunk with [points:[(X0, _) | _]] | _],
	(
	    foreach(RevChunk, RevChunks),
	    fromto([], CTail, [Chunk | CTail], Chunks),
	    fromto(X0, LastX, FirstX, _)
	do
	    RevChunk = chunk with [points:RevPoints],
	    RevPoints = [(_, Y0) | Rest],
	    (
		foreach(Point0, Rest),
		fromto([(LastX, Y0)], PTail, [Point | PTail], Points),
		fromto(LastX, PrevX, NextX, FirstX)
	    do
		Point0 = (X0, Y),
		PrevUpb is breal_max(PrevX),
		( PrevUpb < breal_max(X0) ->
		    NextX is breal_from_bounds(breal_min(X0), PrevUpb),
		    Point = (NextX, Y)
		;
		    Point = Point0,
		    NextX = X0
		)
	    ),
	    update_struct(chunk, [points:Points], RevChunk, Chunk)
	).


illegal_annotation_error(P) :-
	printf(error, "piecewise_linear: (%q):\n"
		"\tmust not annotate X coordinates of points "
		"not involved in a discontinuity.\n", [P]),
	abort.

discontinuity_form_error(P1, P2) :-
	printf(error, "piecewise_linear: (%q), (%q):\n"
		"\tdiscontinuities must be of one of the following forms:\n"
		"\t(<(X), Y1), (X, Y2)\n"
		"\t(X, Y1), (>(X), Y2) or\n"
		"\t(<(X), Y1), (X, Y2), (>(X), Y3).\n", [P1, P2]),
	abort.


    % Converts the points: field in the chunks from a list of points to an
    % array of points.
chunk_points_to_arrays(Chunks0, Chunks) :-
	(
	    foreach(Chunk0, Chunks0),
	    foreach(Chunk, Chunks)
	do
	    Chunk0 = chunk with [points:PointList],
	    PointArray =.. [[] | PointList],
	    update_struct(chunk, [points:PointArray], Chunk0, Chunk)
	).

    % "Processes" a chunk; that is, sets up ic variables corresponding to
    % the upper and lower bounds of X and Y for this chunk and passes it to
    % the chunk solver.
process_chunk(chunk with [points:Points, left:EndLeft, right:EndRight],
		XLo, XHi, YLo, YHi) :-
	reals([XLo, XHi, YLo, YHi]),
	piecewise_chunk(Points, EndLeft, EndRight, XLo, XHi, YLo, YHi).


%%% --- lower layer --- %%%

    % The lower layer of the piecewise linear constraint implements the
    % constraint for a connected chunk.

    % Points are numbered from 1 to NumPoints.
    % Segments are numbered from 1 to NumPoints-1:
    %   segment I is the segment between point I and point I+1;
    %   segments 1 and NumPoints-1 may also extend infinitely to the
    %       left and right, respectively, if the relevant flags have
    %       been passed to the constraint.


    % Structures for storing information needed by the propagation demon
    % used to implement the piecewise linear constraint for a chunk:
    %
    % xinfo:
    %   xlo:     The lower bound on X in the previous call
    %   xlo_seg: The segment corresponding to xlo
    %   xhi:     The upper bound on X in the previous call
    %   xhi_seg: The segment corresponding to xhi
    %
    % yinfo:
    %   ylo:     The lower bound on Y in the previous call
    %   ylos:    A list of the "valleys", in increasing Y order
    %   yhi:     The upper bound on Y in the previous call
    %   yhis:    A list of the "peaks", in decreasing Y order

:- local struct(xinfo(xlo, xlo_seg, xhi, xhi_seg)).
:- local struct(yinfo(ylo, ylos, yhi, yhis)).


    % Sets up the propagator for a piecewise linear chunk.
    % Points is the list of points defining the chunk;
    % EndLeft and EndRight specify whether the corresponding end of the
    % chunk is open, closed, or infinite;
    % XL and YL are variables whose lower bounds correspond to the lower
    % bounds of X and Y, respectively.
    % XH and YH are variables whose upper bounds correspond to the upper
    % bounds of X and Y, respectively.
piecewise_chunk(Points, EndLeft, EndRight, XL, XH, YL, YH) :-
	functor(Points, _, N),
	( N =< 0 ->
	    printf(error, "piecewise_linear: internal error: piecewise_chunk: no points.%n"),
	    abort
	; N = 1 ->
	    (X, Y) is Points[1],
	    % Note: we're surreptitiously assuming
	    % EndLeft = EndRight = closed.  While it should never be called
	    % with an open end point, it can get called with infinite ones,
	    % which we proceed to truncate.  This seems unlikely to ever be
	    % the wrong thing to do.
	    % (Note that the user docs for this predicate explicitly say
	    % that the function is undefined in such a truncated region.)
	    impose_min(XL, X),
	    impose_max(XH, X),
	    impose_min(YL, Y),
	    impose_max(YH, Y),
	    % Monitor XL, XH, etc. to detect when (X, Y) is no longer
	    % feasible.
	    suspend(singularity_lo(XL, X, XL, XH, YL, YH, Susps), 3,
		    [XL->min], SuspXL),
	    suspend(singularity_hi(XH, X, XL, XH, YL, YH, Susps), 3,
		    [XH->max], SuspXH),
	    suspend(singularity_lo(YL, Y, XL, XH, YL, YH, Susps), 3,
		    [YL->min], SuspYL),
	    suspend(singularity_hi(YH, Y, XL, XH, YL, YH, Susps), 3,
		    [YH->max], SuspYH),
	    Susps = [SuspXL, SuspXH, SuspYL, SuspYH]
	;
	    Points =.. [_ | PointList],
	    extract_peaks_valleys(PointList, Peaks, Valleys),
	    N_1 is N - 1,

	    % Set up initial chunk bounds.
	    % This is necessary since if all the bounds are infinite,
	    % the propagator assumes it's a self-wake, and does nothing,
	    % even when there is propagation to be done (e.g. Y bound).
	    ( EndLeft = infinite ->
		fx(Points, 1, 1, -1.0Inf, YLeft, _, _)
	    ;
		(XLo, YLeft) is Points[1],
		impose_min(XL, XLo)
	    ),
	    ( EndRight = infinite ->
		fx(Points, N_1, N_1, 1.0Inf, YRight, _, _)
	    ;
		(XHi, YRight) is Points[N],
		impose_max(XH, XHi)
	    ),
	    YBoundLo is min(YLeft, YRight),
	    YBoundHi is max(YLeft, YRight),
	    ( Valleys = [(_, ValleyY) | _] ->
		YLo is min(YBoundLo, ValleyY)
	    ;
		YLo = YBoundLo
	    ),
	    ( Peaks = [(_, PeakY) | _] ->
		YHi is max(YBoundHi, PeakY)
	    ;
		YHi = YBoundHi
	    ),
	    impose_min(YL, YLo),
	    impose_max(YH, YHi),

	    % Should we be setting x/y lo/hi here to the bounds computed
	    % above?
	    XInfo = xinfo with
		    [xlo: -1.0Inf, xlo_seg:1, xhi:1.0Inf, xhi_seg:N_1],
	    YInfo = yinfo with
		    [ylo: -1.0Inf, ylos:Valleys, yhi:1.0Inf, yhis:Peaks],
	    suspend(propagate_chunk(XL, XH, YL, YH, Points, N,
			    EndLeft, EndRight, XInfo, YInfo, Susp),
		    4, [XL->min, XH->max, YL->min, YH->max], Susp),
	    propagate_chunk(XL, XH, YL, YH, Points, N,
		    EndLeft, EndRight, XInfo, YInfo, Susp)
	).


    % Find all the "peaks" (local maxima) and "valleys" (local minima)
    % of the set of points (excluding end points).  If a horizontal
    % segment is a peak or valley, then any point on the segment will do.
    % The peaks should be returned in decreasing Y order; the valleys in
    % increasing Y order.
    % Note that since Y coordinates may be bounded reals, the
    % determination of what is a peak is based on the upper bound of any
    % such interval, while for valleys it is based on the lower bound.
extract_peaks_valleys(Points, Peaks, Valleys) :-
	Points = [P1 | Rest],
	extract_peaks_valleys_2(down, up, P1, Rest, Peaks0, Valleys0),
	sort_by_upper_y_bound(Peaks0, Peaks),
	sort_by_lower_y_bound(Valleys0, Valleys).

sort_by_upper_y_bound(CoordList, Sorted) :-
	sort(2, >=, CoordList, Sorted).

sort_by_lower_y_bound(CoordList, Sorted) :-
	negate_y_coords(CoordList, NegCoordList),
	sort_by_upper_y_bound(NegCoordList, NegSorted),
	negate_y_coords(NegSorted, Sorted).

negate_y_coords(CoordList, NegCoordList) :-
	(
	    foreach((X, Y), CoordList),
	    foreach((X, NegY), NegCoordList)
	do
	    NegY is -Y
	).

extract_peaks_valleys_2(_HiDir, _LoDir, _Prev, [], [], []).
extract_peaks_valleys_2(HiDir, LoDir, Prev, [P | Rest], Peaks, Valleys) :-
	true,
	Prev = (_, PrevY),
	breal_bounds(PrevY, PrevYLo, PrevYHi),
	P = (_, Y),
	breal_bounds(Y, YLo, YHi),
	update_direction(HiDir, PrevYHi, YHi, NewHiDir),
	update_direction(LoDir, PrevYLo, YLo, NewLoDir),
	update_peaks(HiDir, NewHiDir, Prev, Peaks, Peaks1),
	update_valleys(LoDir, NewLoDir, Prev, Valleys, Valleys1),
	extract_peaks_valleys_2(NewHiDir, NewLoDir, P, Rest, Peaks1, Valleys1).

update_direction(up, Prev, Curr, Dir) :-
	( Curr < Prev ->
	    Dir = down
	;
	    Dir = up
	).
update_direction(down, Prev, Curr, Dir) :-
	( Curr > Prev ->
	    Dir = up
	;
	    Dir = down
	).

update_peaks(up, down, P, Peaks, Peaks1) :-
	!,
	Peaks = [P | Peaks1].
update_peaks(_, _, _, Peaks, Peaks).

update_valleys(down, up, P, Valleys, Valleys1) :-
	!,
	Valleys = [P | Valleys1].
update_valleys(_, _, _, Valleys, Valleys).


:- demon(singularity_lo/7).
singularity_lo(Var, Val, XL, XH, YL, YH, Susps) :-
	get_bounds(Var, Min, _),
	( not Min =< Val ->
	    ( foreach(Susp, Susps) do kill_suspension(Susp) ),
	    infeasible_chunk(XL, XH, YL, YH)
	;
	    true
	).

:- demon(singularity_hi/7).
singularity_hi(Var, Val, XL, XH, YL, YH, Susps) :-
	get_bounds(Var, _, Max),
	( not Max >= Val ->
	    ( foreach(Susp, Susps) do kill_suspension(Susp) ),
	    infeasible_chunk(XL, XH, YL, YH)
	;
	    true
	).


    % Make sure that the chunk with the given bound variables is never again
    % considered by the meta-level constraint.
infeasible_chunk(XL, XH, YL, YH) :-
	impose_min(XL, 1.0Inf),
	impose_max(XH, -1.0Inf),
	impose_min(YL, 1.0Inf),
	impose_max(YH, -1.0Inf).


    % The main propagator for a piecewise linear chunk.
    % XXX - should document algorithm.
    % XXX - should check for X (and possibly Y) becoming ground.
:- demon(propagate_chunk/11).
propagate_chunk(XL, XH, YL, YH, Points, N, EndLeft, EndRight,
		XInfo, YInfo, Susp) :-
	XInfo = xinfo with [xlo:XLo, xlo_seg:XLoSeg, xhi:XHi, xhi_seg:XHiSeg],
	YInfo = yinfo with [ylo:YLo, ylos:YLos, yhi:YHi, yhis:YHis],

	get_bounds(XL, XMin, _),
	get_bounds(XH, _, XMax),
	get_bounds(YL, YMin, _),
	get_bounds(YH, _, YMax),

	% Don't do anything if no bounds have changed (self-wake).
	( XMin == XLo, XMax == XHi, YMin == YLo, YMax == YHi ->
	    true
	;
	    %
	    % X -> Y propagation
	    %

	    % Compute the Y values corresponding to X's bounds.
	    fx(Points, XLoSeg, XHiSeg, XMin, YLeft, XMinSeg, _),
	    fx(Points, XLoSeg, XHiSeg, XMax, YRight, _, XMaxSeg),
	    breal_bounds(YLeft, YLeftMin, YLeftMax),
	    breal_bounds(YRight, YRightMin, YRightMax),

	    ( ( XMin > XLo ; XMax < XHi ) ->
		drop_y_hi_los(XMin, XMax, YLos, NewYLos),
		drop_y_hi_los(XMin, XMax, YHis, NewYHis),

		setarg(ylos of yinfo, YInfo, NewYLos),
		setarg(yhis of yinfo, YInfo, NewYHis),

		% Work out possibly new bounds on Y
		YBorderMin is min(YLeftMin, YRightMin),
		YBorderMax is max(YLeftMax, YRightMax),
		( NewYLos = [(_, Lo) | _] ->
		    NewYMin is min(breal_min(Lo), YBorderMin)
		;
		    NewYMin = YBorderMin
		),
		( NewYHis = [(_, Hi) | _] ->
		    NewYMax is max(breal_max(Hi), YBorderMax)
		;
		    NewYMax = YBorderMax
		)
	    ;
		NewYMin = YMin,
		NewYMax = YMax
	    ),

	    %
	    % Y -> X propagation
	    % "X -> X" prop (skip holes in X's domain due to old Y bounds)
	    %

	    %
	    % The new Y bounds derived from X's old bounds above cannot
	    % yield new X bounds here.  So we only need to propagate if
	    % the old Y bounds would cause some pruning of X's domain.
	    %
	    % NewYMin (NewYMax) should be no greater than (less than)
	    % YMin (YMax) if any "scanning" is done here, because the point
	    % we're trying to find provides a support for the current bound
	    % (YMin/YMax).  Thus it's safe to propagate the current bounds
	    % rather than the new ones.
	    %
	    % Note we don't bother propagating a Y bound to X unless the
	    % fuzzy Y interval corresponding to the existing X bound lies
	    % entirely outside the Y bound; propagating an interval which
	    % merely overlaps the bound is unlikely to yield much benefit
	    % (if any).
	    %

	    ( YLeftMax < YMin ->
		scan_right_min(Points, YMin, XMinSeg, XMaxSeg, NewXMinSeg),
		inverse_fx(Points, YMin, NewXMinSeg, right, TmpXMinFuzzy),
		NewXMin is breal_min(TmpXMinFuzzy)
	    ; YLeftMin > YMax ->
		scan_right_max(Points, YMax, XMinSeg, XMaxSeg, NewXMinSeg),
		inverse_fx(Points, YMax, NewXMinSeg, right, TmpXMinFuzzy),
		NewXMin is breal_min(TmpXMinFuzzy)
	    ;
		NewXMin = XMin,
		NewXMinSeg = XMinSeg
	    ),

	    ( YRightMax < YMin ->
		scan_left_min(Points, YMin, XMinSeg, XMaxSeg, NewXMaxSeg),
		inverse_fx(Points, YMin, NewXMaxSeg, left, TmpXMaxFuzzy),
		NewXMax is breal_max(TmpXMaxFuzzy)
	    ; YRightMin > YMax ->
		scan_left_max(Points, YMax, XMinSeg, XMaxSeg, NewXMaxSeg),
		inverse_fx(Points, YMax, NewXMaxSeg, left, TmpXMaxFuzzy),
		NewXMax is breal_max(TmpXMaxFuzzy)
	    ;
		NewXMax = XMax,
		NewXMaxSeg = XMaxSeg
	    ),


	    % Update any new bounds...

	    ( NewXMin > XMin ->
		impose_min(XL, NewXMin),	% XL *>= exact(NewXMin)
		NewXLo = NewXMin,
		NewXLoSeg = NewXMinSeg
	    ;
		NewXLo = XMin,
		NewXLoSeg = XMinSeg
	    ),
	    ( NewXMax < XMax ->
		impose_max(XH, NewXMax),	% XL *=< exact(NewXMax)
		NewXHi = NewXMax,
		NewXHiSeg = NewXMaxSeg
	    ;
		NewXHi = XMax,
		NewXHiSeg = XMaxSeg
	    ),

	    ( NewYMin > YMin ->
		impose_min(YL, NewYMin),	% YL *>= exact(NewYMin),
		NewYLo = NewYMin
	    ;
		NewYLo = YMin
	    ),
	    ( NewYMax < YMax ->
		impose_max(YH, NewYMax),	% YH *=< exact(NewYMax),
		NewYHi = NewYMax
	    ;
		NewYHi = YMax
	    ),

	    % Check some "satisfiability" conditions

	    (
		(
		    % Y domain empty
		    NewYLo > NewYHi
		;
		    % X domain empty
		    NewXLo > NewXHi
		;
		    % X domain is single point at left end, which is open
		    EndLeft = open,
		    (P1X, _) is Points[1],
		    NewXHi is breal_min(P1X)
		;
		    % X domain is single point at right end, which is open
		    EndRight = open,
		    (PNX, _) is Points[N],
		    NewXLo is breal_max(PNX)
		)
	    ->
		% Make sure that this chunk is never again considered by the
		% meta-level constraint.
		kill_suspension(Susp),
		infeasible_chunk(XL, XH, YL, YH)
	    ;
		% Update the data structures for next time we're woken
		setarg(xlo of xinfo, XInfo, NewXLo),
		setarg(xlo_seg of xinfo, XInfo, NewXLoSeg),
		setarg(xhi of xinfo, XInfo, NewXHi),
		setarg(xhi_seg of xinfo, XInfo, NewXHiSeg),

		setarg(ylo of yinfo, YInfo, NewYLo),
		setarg(yhi of yinfo, YInfo, NewYHi)
	    ),

	    wake
	).
	% Check for Y ground resulting in "unique" X?  (i.e. X segment known)


    %
    % Evaluate the function at the point X.
    % XLoSeg and XHiSeg give upper and lower bounds on the segment number in
    % which to find X.  XSeg returns the segment in which X was found, and Y
    % is the function result.  In the case where it cannot be determined
    % precisely which segment X lies in, SegSelect indicates which one
    % should be returned in XSeg (left or right).
    %
    % Note that an (accurate) X coordinate can only have an ambiguous
    % segment if it "overlaps" one or more of the points.  In such cases,
    % the Y bounds of the overlapping points must be taken into account, as
    % well as the segments to the left and the right of this set of points.
    % The segments between the points can be ignored because any point on
    % one of these segments cannot have a Y coordinate outside the Y bounds
    % of the points the segment lies between.  Similarly, a lower (upper)
    % bound from an adjacent segment need only be considered if the other
    % (non-overlapping) end point has a lower (upper) bound which is smaller
    % (larger) than the smallest (largest) lower (upper) bound of the
    % overlapping points.  Note that since the inaccuracy in a line segment
    % is not independent of the inaccuracy of its end points, not excluding
    % the above cases can result in weaker bounds being derived.
    %
fx(Points, XLoSeg, XHiSeg, X, Y, XSegLeft, XSegRight) :-
	fx2(Points, XLoSeg, XHiSeg, XHiSeg, X, Y, XSegLeft, XSegRight).

fx2(Points, XLoSeg, XHiSeg, XMaxSeg, X, Y, XSegLeft, XSegRight) :-
	( XLoSeg < XHiSeg ->
	    % Do a binary chop, keeping left.
	    Point is (XLoSeg + XHiSeg + 1) // 2,
	    (Xp, _Yp) is Points[Point],
	    ( not X =< Xp ->
		% No way X can be =< the point, so go right.
		NewLoSeg = Point,
		fx2(Points, NewLoSeg, XHiSeg, XMaxSeg, X, Y, XSegLeft,
			XSegRight)
	    ;
		% X potentially =< the point, so go left.
		NewHiSeg is Point - 1,
		fx2(Points, XLoSeg, NewHiSeg, XMaxSeg, X, Y, XSegLeft,
			XSegRight)
	    )
	; XLoSeg = XHiSeg ->
	    XSegLeft = XLoSeg,
	    Pa = XLoSeg,
	    (Xa, Ya) is Points[Pa],
	    breal_bounds(Ya, YaMin, YaMax),
	    ( not X =:= Xa ->
		% X does not overlap point a.
		LeftInterp = yes,
		YMin0 = 1.0Inf,
		YMax0 = -1.0Inf
	    ;
		% X overlaps point a.
		LeftInterp = no,
		YMin0 = YaMin,
		YMax0 = YaMax
	    ),
	    fx_overlapping(Points, XLoSeg, XMaxSeg, X, YMin0, YMax0,
		    RightSeg, YMin1, YMax1),
	    ( LeftInterp = yes ->
		% Interpolate between points Pa and Pa + 1.
		Pb is Pa + 1,
		(Xb, Yb) is Points[Pb],
		interpolate(Xb, Yb, Xa, Ya, X, Yab), % Better approx. near Pb
		breal_bounds(Yab, YabMin, YabMax),
		( YaMin < YMin1 ->
		    YMin2 is min(YMin1, YabMin)
		;
		    YMin2 = YMin1
		),
		( YaMax > YMax1 ->
		    YMax2 is max(YMax1, YabMax)
		;
		    YMax2 = YMax1
		)
	    ;
		YMin2 = YMin1,
		YMax2 = YMax1
	    ),
	    ( RightSeg =< XMaxSeg ->	% RightInterp = yes
		% Interpolate between points Pc and Pc + 1.
		XSegRight = RightSeg,
		Pc = RightSeg,
		Pd = Pc + 1,
		(Xc, Yc) is Points[Pc],
		(Xd, Yd) is Points[Pd],
		breal_bounds(Yd, YdMin, YdMax),
		interpolate(Xc, Yc, Xd, Yd, X, Ycd),
		breal_bounds(Ycd, YcdMin, YcdMax),
		( YdMin < YMin2 ->
		    YMin is min(YMin2, YcdMin)
		;
		    YMin = YMin2
		),
		( YdMax > YMax2 ->
		    YMax is max(YMax2, YcdMax)
		;
		    YMax = YMax2
		)
	    ;
		XSegRight = XMaxSeg,
		YMin = YMin2,
		YMax = YMax2
	    ),

	    breal_from_bounds(YMin, YMax, Y)
	;
	    % XLoSeg > XHiSeg
	    % Can only happen when there's only one point in the chunk?
	    (X0, Y) is Points[XLoSeg],
	    not not X =:= X0,
	    XSegLeft = XLoSeg,
	    XSegRight = XLoSeg - 1
	).

fx_overlapping(Points, Seg, MaxSeg, X, YMin0, YMax0, RightSeg, YMin, YMax) :-
	( Seg > MaxSeg ->
	    % Don't go beyond the maximum allowed segment.
	    RightSeg = Seg,
	    YMin = YMin0,
	    YMax = YMax0
	;
	    NextSeg is Seg + 1,
	    subscript(Points, [NextSeg], (Xb, Yb)),
	    ( not X =:= Xb ->
		% X does not overlap the next point.
		RightSeg = Seg,
		YMin = YMin0,
		YMax = YMax0
	    ;
		breal_bounds(Yb, YbMin, YbMax),
		YMin1 is min(YMin0, YbMin),
		YMax1 is max(YMax0, YbMax),
		fx_overlapping(Points, NextSeg, MaxSeg, X, YMin1, YMax1,
			RightSeg, YMin, YMax)
	    )
	).


    % Find X given Y and a segment.
    % Y is a float, returned X is a bounded real (usually?).
inverse_fx(Points, Y, XSeg, LeftRight, X) :-
	Pa = XSeg,
	Pb is XSeg + 1,
	(Xa, Ya) is Points[Pa],
	(Xb, Yb) is Points[Pb],
	( LeftRight = left, not not Y =:= Yb ->
	    % Given Y value intersects right point.
	    X = Xb
	; not not Y =:= Ya ->
	    % Given Y value intersects left point.
	    X = Xa
	; not not Y =:= Yb ->
	    % Given Y value intersects right point.
	    X = Xb
	; not Ya =\= Yb ->
	    % Segment is exactly horizontal.
	    % We know Y is above or below the segment.
	    % XXX - this code should be generalised for the "approximately
	    % horizontal" cases that arise from non-zero-width intervals.
	    ( LeftRight = left ->
		X = -1.0Inf__-1.0Inf
	    ;
		X = 1.0Inf__1.0Inf
	    )
	;
	    interpolate(Ya, Xa, Yb, Xb, Y, X)
	).


    % Find the value of Y corresponding to X on the line passing through the
    % points (Xa, Ya) and (Xb, Yb).
interpolate(Xa, Ya, Xb, Yb, X, Y) :-
	    % Original version:
	% Ratio giis (Xb - X) / (Xb - Xa),
	% Y giis Ratio * Ya + (1 - Ratio) * Yb.
	    % For greater numerical accuracy, should use this instead:
	% Y giis ((Xb - X) * Ya + (X - Xa) * Yb) / (Xb - Xa).
	    % Nope, the above (like the first) does bad things when X is
	    % +/- infinity.
	    % Try this one (requires one more multiplication):
	% Y giis (Xb * Ya - Xa * Yb + X * (Yb - Ya)) / (Xb - Xa).
	    % Damn, much less accurate.  Oh well, back to the *really* basic
	    % version.  This has a higher error at the "b" end of the
	    % interval than the "a", but consistently lower than the above.
	Gradient is (Yb - Ya) / (Xb - Xa),
	Y is Ya + Gradient * (X - Xa).


    % Starting from XMinSeg and and working right, find the first segment
    % with a (right) endpoint no lower than YMin.
scan_right_min(Points, YMin, XMinSeg, XMaxSeg, NewXMinSeg) :-
	( XMinSeg >= XMaxSeg ->
	    % This case mainly to catch infinite extensions
	    NewXMinSeg = XMinSeg
	;
	    XMinSeg1 is XMinSeg + 1,
	    (_, Y) is Points[XMinSeg1],

	    % Note we stop scanning if Y overlaps YMin.
	    ( not Y >= YMin ->
		scan_right_min(Points, YMin, XMinSeg1, XMaxSeg, NewXMinSeg)
	    ;
		NewXMinSeg = XMinSeg
	    )
	).

    % Starting from XMaxSeg and and working left, find the first segment
    % with a (left) endpoint no lower than YMin.
scan_left_min(Points, YMin, XMinSeg, XMaxSeg, NewXMaxSeg) :-
	( XMaxSeg =< XMinSeg ->
	    % This case mainly to catch infinite extensions
	    NewXMaxSeg = XMaxSeg
	;
	    (_, Y) is Points[XMaxSeg],

	    % Note we stop scanning if Y overlaps YMin.
	    ( not Y >= YMin ->
		XMaxSeg_1 is XMaxSeg - 1,
		scan_left_min(Points, YMin, XMinSeg, XMaxSeg_1, NewXMaxSeg)
	    ;
		NewXMaxSeg = XMaxSeg
	    )
	).

    % Starting from XMinSeg and and working right, find the first segment
    % with a (right) endpoint lower than YMax.
scan_right_max(Points, YMax, XMinSeg, XMaxSeg, NewXMinSeg) :-
	( XMinSeg >= XMaxSeg ->
	    % This case mainly to catch infinite extensions
	    NewXMinSeg = XMinSeg
	;
	    XMinSeg1 is XMinSeg + 1,
	    (_, Y) is Points[XMinSeg1],

	    % Note we stop scanning if Y overlaps YMax.
	    ( not Y =< YMax ->
		scan_right_max(Points, YMax, XMinSeg1, XMaxSeg, NewXMinSeg)
	    ;
		NewXMinSeg = XMinSeg
	    )
	).

    % Starting from XMaxSeg and and working left, find the first segment
    % with a (left) endpoint no higher than YMax.
scan_left_max(Points, YMax, XMinSeg, XMaxSeg, NewXMaxSeg) :-
	( XMaxSeg =< XMinSeg ->
	    % This case mainly to catch infinite extensions
	    NewXMaxSeg = XMaxSeg
	;
	    (_, Y) is Points[XMaxSeg],

	    % Note we stop scanning if Y overlaps YMax.
	    ( not Y =< YMax ->
		XMaxSeg_1 is XMaxSeg - 1,
		scan_left_max(Points, YMax, XMinSeg, XMaxSeg_1, NewXMaxSeg)
	    ;
		NewXMaxSeg = XMaxSeg
	    )
	).


    % Drop from the head of a list points any points which lie outside the
    % specified X interval (intended to be called with the lists of high and
    % low Y points, but works generally).
drop_y_hi_los(_XMin, _XMax, [], []).
drop_y_hi_los(XMin, XMax, [Point | Points], NewPoints) :-
	Point = (X, _),
	( not ( X >= XMin, X =< XMax ) ->
	    drop_y_hi_los(XMin, XMax, Points, NewPoints)
	;
	    NewPoints = [Point | Points]
	).

