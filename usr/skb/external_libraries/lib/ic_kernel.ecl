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
% Contributor(s): Warwick Harvey, IC-Parc
% 
% END LICENSE BLOCK
%---------------------------------------------------------------------
%
% IC kernel module.
%
% System:       ECLiPSe Constraint Logic Programming System
% Author/s:     Warwick Harvey, IC-Parc
%
%	Chunks of this module derived from or loosely based on code from
%	the FD module (written by Micha Meier), the Range module (written by
%	Joachim Schimpf) and the RIA module (written by Joachim Schimpf and
%	Stefano Novello).
%
% This module provides the core of the IC library, a combined finite domain
% and floating point interval propagation solver.  This core provides the
% implementation of IC variables: the implementation of their attribute
% structure, predicates for manipulating their domain, event management,
% etc.  It does not provide any constraints per se (unless you count
% unification); this is left to other modules (e.g. ic_constraints).  It is
% intended that this kernel can be used by other constraint solvers in order
% to share a common representation and cooperate with the IC solver.
%
%---------------------------------------------------------------------
%
% TODO:
%
% - Add an integer tolerance?  (cf. the range library)
%
%---------------------------------------------------------------------

:- module(ic_kernel).

%---------------------------------------------------------------------
%
% Set some compiler directives.
%

:- pragma(expand).
:- pragma(nodebug).
:- set_flag(float_precision, double).


%---------------------------------------------------------------------
%
% Define the operators for this module.
%

:- op(600, xfx, [..]).
:- op(700, xfx, [::]).


%---------------------------------------------------------------------
%
% Imports and exports.
%

% First load the dynamic libraries.

:- import symbol_address/2 from sepia_kernel.

%
% Import the (C) bitmap library.
%

:-  ( symbol_address(p_create_bitmap, _) ->
	true
    ;
	get_flag(hostarch, Arch),
	get_flag(object_suffix, O),
	( O = "o" ->
	    concat_string([Arch, '/bitmap.', O], Load)
	; O = "dll" ->
	    getcwd(Cwd),
	    concat_string([Cwd, Arch, '/bitmap.', O], Load)
	;
	    concat_string([Arch, '/bitmap.', O], Load)
	),
	load(Load)
    ).

:- external(create_bitmap/3, p_create_bitmap),
   external(set_bitmap_lwb/4, p_set_bitmap_lwb),
   external(set_bitmap_upb/4, p_set_bitmap_upb),
   external(remove_bitmap_element/4, p_remove_bitmap_element),
   external(remove_bitmap_range/5, p_remove_bitmap_range),
   external(bitmap_intersect_into/4, p_bitmap_intersect_into),
   external(bitmap_shifted_intersect_into/4, p_bitmap_shifted_intersect_into),
   external(bitmaps_have_non_empty_intersection/2, p_bitmaps_have_non_empty_intersection),
   external(bitmap_union/3, p_bitmap_union),
   external(copy_bitmap/2, p_copy_bitmap),
   external(copy_bitmap_shifted/3, p_copy_bitmap_shifted),
   external(bitmap_range/3, p_bitmap_range),
   external(get_bitmap_lwb/2, p_get_bitmap_lwb),
   external(get_bitmap_upb/2, p_get_bitmap_upb),
   external(next_greater_member/3, p_next_greater_member),
   external(next_smaller_member/3, p_next_smaller_member),
   external(next_greater_non_member/3, p_next_greater_non_member),
   external(next_smaller_non_member/3, p_next_smaller_non_member),
   external(bitmap_size/2, p_bitmap_size),
   external(bitmap_contains/2, p_bitmap_contains),
   external(bitmap_contains_range/3, p_bitmap_contains_range),
   external(compare_bitmaps/3, p_compare_bitmaps).


%
% Import the C part of the IC library.
%

:-  ( symbol_address(p_ic_init, _) ->
	true
    ;
	get_flag(hostarch, Arch),
	get_flag(object_suffix, O),
	( O = "o" ->
	    concat_string([Arch, '/ic.', O], Load)
	; O = "dll" ->
	    getcwd(Cwd),
	    concat_string([Cwd, Arch, '/ic.', O], Load)
	;
	    concat_string([Arch, '/ic.', O], Load)
	),
	load(Load)
    ).

:- external(ic_init/0, p_ic_init),
   external(get_ic_attr/2, p_get_ic_attr),
   external(get_bounds/3, p_get_bounds),
   external(get_integer_bounds1/5, p_get_integer_bounds1),
   external(get_domain_size/2, p_get_domain_size),
   external(ic_impose_bounds/3, p_ic_impose_bounds),
   external(ic_impose_min/2, p_ic_impose_min),
   external(ic_impose_max/2, p_ic_impose_max),
   external(ic_exclude/2, p_ic_exclude),
   external(ic_exclude_range/3, p_ic_exclude_range),
   external(set_var_integer/1, p_set_var_integer),
   external(get_threshold/1, p_get_threshold),
   external(set_threshold/1, p_set_threshold),
   external(unify_ic/3, p_unify_ic).


%
% Export predicates intended for general use.
%

:- export
	reals/1,		% reals(?Vars)
	integers/1,		% integers(?Vars)
	is_solver_var/1,	% is_solver_var(?Var)
	is_exact_solver_var/1,	% is_exact_solver_var(?Var)
	is_solver_type/1,	% is_solver_type(?Var)
	get_solver_type/2,	% get_solver_type(?Var, -Type)
	get_bounds/3,		% get_bounds(?Var, -Lwb, -Upb)
	get_min/2,		% get_min(?Var, -Lwb)
	get_max/2,		% get_max(?Var, -Upb)
	get_float_bounds/3,	% get_float_bounds(?Var, -Lwb, -Upb)
	get_integer_bounds/3,	% get_integer_bounds(?Var, -Lwb, -Upb)
	get_finite_integer_bounds/3,% get_finite_integer_bounds(?Var, -Lwb, -Upb)
	get_domain_size/2,	% get_domain_size(?Var, -Size)
	get_domain/2,		% get_domain(?Var, -Domain)
	get_domain_as_list/2,	% get_domain_as_list(?Var, -DomainList)
	get_median/2,		% get_median(?Var, -Median)
	get_delta/2,		% get_delta(?Var, -Width)
	print_solver_var/2,	% print_solver_var(?Var, -Printed)
	set_threshold/1,	% set_threshold(++Threshold)
	set_threshold/2,	% set_threshold(++Threshold, +WakeVars)
	get_threshold/1,	% get_threshold(-Threshold)
	is_in_domain/2,		% is_in_domain(++Val, ?Var)
	is_in_domain/3,		% is_in_domain(++Val, ?Var, ?Result)
	delayed_goals_number/2. % delayed_goals_number(?Var, -N)
%	int_tolerance/1,	% int_tolerance(-Tol)
%	set_int_tolerance/1.	% set_int_tolerance(+Tol)


%
% Export predicates intended for use only within the IC-based solvers.
%

:- export
	get_ic_attr/2,
	set_vars_type/2,	% set_vars_type(?Vars, ++Type)
	set_var_type/2,		% set_var_type(?Var, ++Type)
	impose_min/2,		% impose_min(?Var, ++Bound)
	impose_max/2,		% impose_max(?Var, ++Bound)
	impose_domain/2,	% impose_domain(?X, ?Y)
	impose_bounds/3,	% impose_bounds(?X, ++Lo, ++Hi)
	exclude/2,		% exclude(?Var, ++Val), Var & Val ints
	exclude_range/3,	% exclude_range(?Var, ++Lo, ++Hi), all ints
	msg/3,			% msg(+X, +Y, -Msg)
	ic_event/1,
	ic_stat/1,		% ic_stat(?Stat)
	ic_stat_get/1,		% ic_stat_get(?Stat)
	ic_stat_register_event/2. % ic_stat_register_event(++Event, ++Description)


%
% Reexport some predicates useful for doing low-level manipulation of
% ground intervals.
%

:- reexport
		ria_unop/5,
		ria_binop/7,
		ria_ternop/9
	from sepia_kernel.


%
% Export some macros for conveniently referring to the magic numbers used
% by the IC C functions.
%

:- export macro(no_macro_expansion(ic_status_failed)/0, tr_ic_c_constant/2, []).
:- export macro(no_macro_expansion(ic_status_unknown)/0, tr_ic_c_constant/2, []).
:- export macro(no_macro_expansion(ic_status_entailed)/0, tr_ic_c_constant/2, []).

:- export macro(no_macro_expansion(ic_unify_bound_implied)/0, tr_ic_c_constant/2, []).
:- export macro(no_macro_expansion(ic_unify_bound_tight)/0, tr_ic_c_constant/2, []).
:- export macro(no_macro_expansion(ic_unify_bound_fuzzy)/0, tr_ic_c_constant/2, []).

:- export tr_ic_c_constant/2.


%
% Export some macros for conveniently referring to the magic numbers used
% by the RIA C functions.
%

:- export macro(ria_unop/1, tr_ria_unop1/2, []).
:- export macro(ria_unop/5, tr_ria_unop5/2, []).
:- export tr_ria_unop1/2, tr_ria_unop5/2.
:- export tr_ria_unop/2.

:- export macro(ria_binop/1, tr_ria_binop1/2, []).
:- export macro(ria_binop/7, tr_ria_binop7/2, []).
:- export tr_ria_binop1/2, tr_ria_binop7/2.
:- export tr_ria_binop/2.

:- export macro(ria_ternop/1, tr_ria_ternop1/2, []).
:- export macro(ria_ternop/9, tr_ria_ternop9/2, []).
:- export tr_ria_ternop1/2, tr_ria_ternop9/2.
:- export tr_ria_ternop/2.


%
% Export some macros which cause some compile-generated goals to be printed
% in a more user-friendly form.
%

:- export tr_ic_kernel_out/2.

:- export portray(set_var_type/2, tr_ic_kernel_out/2, [goal]).
:- export portray(set_vars_type/2, tr_ic_kernel_out/2, [goal]).


%---------------------------------------------------------------------
%
% Declare the IC attribute structure.
%
% If you change this, don't forget to update ic.c!
%

:- export struct(
	ic(
	    var_type,	% atom: 'integer' or 'real' [or 'expression'?]
	    lo,		% float: lower bound
	    hi,		% float: upper bound
	    bitmap,	% 'undefined' or bitmap of integer domain
	%   subst,	% substitution
	%   subst_occ,	% list of substitution occurrences?
% XXX	    variable,	% the variable this attribute belongs to
	    min,	% suspensions: wake on update of lo
	    max,	% suspensions: wake on update of hi
	    hole,	% suspensions: wake on new hole in domain
	%   any		% suspensions: wake on any domain change
	%   subst	% suspensions: wake on substitution
	    type	% suspensions: wake on type change (float -> int)
	)
).

:- meta_attribute(ic, [
	unify:			unify_ic/3,
	test_unify:		test_unify_ic/2,
	compare_instances:	compare_ic_instances/3,
	copy_term:		copy_ic_term/2,
	suspensions:		ic_suspensions/3,
	delayed_goals_number:	delayed_goals_number/2,
	get_bounds:		get_float_bounds/3,
	set_bounds:		impose_bounds/3,
	print:			print_solver_var/2
	]).

% Init IC library - Must follow ic meta_attribute declaration
:- ic_init.

%---------------------------------------------------------------------
%
% Check for whether an incompatible (FD or range) library has been loaded.
%

check_for_conflicting_module :-
	member(Module, [fd, range]),
	current_module(Module),
	printf(warning_output, "Warning: The ic and %w modules use different domain representations and%nshould not be mixed.%n", [Module]),
	fail.
check_for_conflicting_module.

:- check_for_conflicting_module.


%---------------------------------------------------------------------
%
% Declare the IC constraint structure.
%
% If you change this, don't forget to update ic.c!
%
% Not needed at the ECLiPSe level?
%

/*
    % This version assumes the linear terms stored in vector form, with
    % separate vectors for each bound of the coefficients (may be shared)
    % and the variables.
:- local struct(
	ic_con(
	    data,	% flags, RHS constant, etc. in buffer struct
	    boolean,	% reification boolean
	    lo_vector,	% vector containing lower bounds of term coefficients
	    hi_vector,	% vector containing upper bounds of term coefficients
	    var_vector,	% vector containing term variables
	    susp
	)
    ).
*/


%---------------------------------------------------------------------
%
% Implementation of the macro for conveniently referring to the magic numbers
% used by the IC C functions.
%

tr_ic_c_constant(no_macro_expansion(ic_status_failed),		-1).
tr_ic_c_constant(no_macro_expansion(ic_status_unknown),	0).
tr_ic_c_constant(no_macro_expansion(ic_status_entailed),	1).

tr_ic_c_constant(no_macro_expansion(ic_unify_bound_implied),	0).
tr_ic_c_constant(no_macro_expansion(ic_unify_bound_tight),	1).
tr_ic_c_constant(no_macro_expansion(ic_unify_bound_fuzzy),	2).


%---------------------------------------------------------------------
%
% Implementation of macros for conveniently referring to the magic numbers
% used by the RIA C functions.
%
% Example usages:
%	ria_unop(roundout, XL, XU, ZL, ZU)
%	Op = ria_unop(roundout), ria_unop(Op, XL, XU, ZL, ZU)
%

% Unary operations.

tr_ria_unop(X, _) :-
	var(X),
	!,
	% Don't try to translate variables!
	fail.
tr_ria_unop(sqr, 0).
tr_ria_unop(sqrt, 1).
tr_ria_unop(sin, 2).
tr_ria_unop(cos, 3).
tr_ria_unop(exp, 4).
tr_ria_unop(ln, 5).
tr_ria_unop(atan, 6).
tr_ria_unop(pi, 7).
tr_ria_unop(abs, 8).
tr_ria_unop(roundout, 10).
tr_ria_unop(neg, 11).
tr_ria_unop(width, 12).

no_macro_expansion(
    tr_ria_unop1(ria_unop(OpIn), OpOut) :-
	    tr_ria_unop(OpIn, OpOut)
).

no_macro_expansion(
    tr_ria_unop5(ria_unop(OpIn, XL, XU, ZL, ZU),
		    ria_unop(OpOut, XL, XU, ZL, ZU)) :-
	    tr_ria_unop(OpIn, OpOut)
).


% Binary operations.

tr_ria_binop(X, _) :-
	var(X),
	!,
	% Don't try to translate variables!
	fail.
tr_ria_binop(add, 0).
tr_ria_binop(sub, 1).
tr_ria_binop(mult, 2).
tr_ria_binop(div, 3).
tr_ria_binop(rsqr, 4).
tr_ria_binop(pow_int, 5).
tr_ria_binop(rpow_odd, 6).
tr_ria_binop(relax, 8).
tr_ria_binop(min, 9).
tr_ria_binop(max, 10).
tr_ria_binop(logsplit, 11).
tr_ria_binop(plusminus, 12).
tr_ria_binop(min_delta, 13).
tr_ria_binop(linsplit, 14).
tr_ria_binop(linsplit_upper, 15).
tr_ria_binop(logsplit_upper, 16).

no_macro_expansion(
    tr_ria_binop1(ria_binop(OpIn), OpOut) :-
	    tr_ria_binop(OpIn, OpOut)
).

no_macro_expansion(
    tr_ria_binop7(ria_binop(OpIn, XL, XU, YL, YU, ZL, ZU),
		    ria_binop(OpOut, XL, XU, YL, YU, ZL, ZU)) :-
	    tr_ria_binop(OpIn, OpOut)
).


% Ternary operations.

tr_ria_ternop(X, _) :-
	var(X),
	!,
	% Don't try to translate variables!
	fail.
tr_ria_ternop(rpow_even, 0).
tr_ria_ternop(union, 1).
tr_ria_ternop(div, 2).

no_macro_expansion(
    tr_ria_ternop1(ria_ternop(OpIn), OpOut) :-
	    tr_ria_ternop(OpIn, OpOut)
).

no_macro_expansion(
    tr_ria_ternop9(ria_ternop(OpIn, XL, XU, YL, YU, ZL, ZU, RL, RU),
		    ria_ternop(OpOut, XL, XU, YL, YU, ZL, ZU, RL, RU)) :-
	    tr_ria_ternop(OpIn, OpOut)
).


%---------------------------------------------------------------------
%
% Implementation of macros which cause some compile-generated goals to be
% printed in a more user-friendly form.
%

tr_ic_kernel_out(set_var_type(Var, Type), Term) :-
	tr_ic_kernel_out(set_vars_type([Var], Type), Term).
tr_ic_kernel_out(set_vars_type(Vars, Type), Term) :-
	( Type == integer ->
		Term = integers(Vars)
	;
		Type == real,
		Term = reals(Vars)
	).


%---------------------------------------------------------------------
%
% Set or update type.
%


    %
    % reals(?X)
    %	Declares X to be an IC variable of type `real'.
    %	If X is a list, the declaration is applied recursively to all
    %	members of the list.
    %	If X is a matrix subscript specification, the declaration is applied
    %	recursively to all elements of the matrix matching the subscript
    %	specification.
    %
reals(X) :-
	set_vars_type(X, real),
	wake.

    %
    % integers(?X)
    %	Declares X to be an IC variable of type `integer'.
    %	If X is a list, the declaration is applied recursively to all
    %	members of the list.
    %	If X is a matrix subscript specification, the declaration is applied
    %	recursively to all elements of the matrix matching the subscript
    %	specification.
    %
integers(X) :-
	set_vars_type(X, integer),
	wake.

    %
    % valid_type(++Type)
    %	Succeeds iff Type is a valid IC type (integer or real).
    %
valid_type(integer).
valid_type(real).

    %
    % set_vars_type(?X, ++Type)
    %	Sets the type of the var/list/matrix/etc. X to be T.
    %
set_vars_type(X, Type) :-
        var(Type),!,
        error(4, set_vars_type(X, Type)).
set_vars_type(X, Type) :-
	valid_type(Type),
	!,
	set_vars_type1(X, Type).
set_vars_type(X, Type) :-
	error(6, set_vars_type(X, Type)).

set_vars_type1(X, Type) :-
	var(X),
	!,
	set_var_type_var(X, Type).
set_vars_type1(X, _) :-
	integer(X),
	!.
set_vars_type1(X, Type) :-
	( float(X) ; rational(X) ; breal(X) ),
	!,
	Type = real.
set_vars_type1([], _) :-
	!.
set_vars_type1([X|Xs], Type) :-
	!,
	set_vars_type1(X, Type),
	set_vars_type1(Xs, Type).
set_vars_type1(subscript(Array, Index), Type) :-
	!,
	subscript(Array, Index, Elem),
	set_vars_type1(Elem, Type).
set_vars_type1(X, Type) :-
	error(5, set_vars_type(X, Type)).

    %
    % set_var_type(?X, ++Type)
    %	Sets the type of the variable X to be Type.
    %
set_var_type(X, Type) :-
	var(Type), !,
        error(4, set_var_type(X, Type)).        
set_var_type(X, _) :-
	integer(X),
	!.
set_var_type(X, Type) :-
	( float(X) ; rational(X) ; breal(X) ),
	!,
	Type = real.
set_var_type(X, Type) :-
	set_var_type_var(X, Type).

set_var_type_var(X, real) :-
	!,
	get_ic_attr(X, _).	% Make sure attribute exists
set_var_type_var(X, integer) :-
	!,
	set_var_integer(X).
set_var_type_var(X, Type) :-
	error(6, set_var_type(X, Type)).



%---------------------------------------------------------------------
%
% Update domain.
%

    %
    % impose_min(?X, ++Lo)
    %	Low-level primitive for updating the (IC) lower bound of the
    %	variable X to be at least Lo.
    %
    %	Note that this predicate assumes Lo is exact (i.e. floats are not
    %	widened), and if it's a bounded real, only its lower bound is
    %	considered.  Also, woken goals are merely scheduled for execution;
    %	if you want them to be executed, you must call wake/0.
    %
    %	Usually you will want to call X *>= Lo instead.
    %
impose_min(X, NewLo) :-
	var(X), !,
	ic_impose_min(X, NewLo).
impose_min(X, NewLo) :-
	breal(NewLo), !,
	X >= breal_min(NewLo).
impose_min(X, NewLo) :-
	X >= NewLo.

    %
    % impose_max(?X, ++Hi)
    %	Low-level primitive for updating the (IC) upper bound of the
    %	variable X to be at most Hi.
    %
    %	Note that this predicate assumes Hi is exact (i.e. floats are not
    %	widened), and if it's a bounded real, only its upper bound is
    %	considered.  Also, woken goals are merely scheduled for execution;
    %	if you want them to be executed, you must call wake/0.
    %
    %	Usually you will want to call X *=< Hi instead.
    %
impose_max(X, NewHi) :-
	var(X), !,
	ic_impose_max(X, NewHi).
impose_max(X, NewHi) :-
	breal(NewHi), !,
	X =< breal_max(NewHi).
impose_max(X, NewHi) :-
	X =< NewHi.

    %
    % exclude(?X, ++E)
    %	Low-level primitive for excluding the element E from the (IC) domain
    %	of the variable	X.  Assumes X is an integer IC variable and that E
    %	is an integer (a type error is generated otherwise).
    %
    %	Note that woken goals are merely scheduled for execution by this
    %	predicate; if you want them to be executed, you must call wake/0.
    %
    %	Usually you will want to call X #\= E instead.
    %
exclude(X, Excl) :-
	var(X), !,
	ic_exclude(X, Excl).
exclude(X, Excl) :-
	integer(X),
	integer(Excl),
	!,
	X =\= Excl.
exclude(X, Excl) :-
	error(6, X =\= Excl).

    %
    % exclude_range(?X, ++Lo, ++Hi)
    %	Low-level primitive for excluding the elements Lo through Hi from
    %	the (IC) domain of the variable	X.  Assumes X is an integer IC
    %	variable and that Lo and Hi are integers (a type error is generated
    %	otherwise).
    %
    %	Note that woken goals are merely scheduled for execution by this
    %	predicate; if you want them to be executed, you must call wake/0.
    %
exclude_range(X, Lo, Hi) :-
	var(X), !,
	ic_exclude_range(X, Lo, Hi).
exclude_range(X, Lo, Hi) :-
	integer(X),
	integer(Lo),
	integer(Hi),
	!,
	\+ ( Lo =< X, X =< Hi).
exclude_range(X, Lo, Hi) :-
	error(6, exclude_range(X, Lo, Hi)).


    %
    % impose_domain(?X, ?Y)
    %	Imposes the domain of Y onto X.  Like X=Y, but Y is unaffected.
    %   Lazy person's implementation, basically doing copy_term(Y,T),X=T.
    %
    %   This does not call wake/0.  If that is wanted, one can simply
    %   use normal unification, i.e. X=T instead of unify_any_ic(X, T)
    %
impose_domain(X, Y{ic:AttrY}) ?- !,
	impose_domain(X, Y, AttrY).
impose_domain(_X, Y) :-
	var(Y).
impose_domain(X, Y) :-
	nonvar(Y),
	X = Y.

    impose_domain(_X, _Y, AttrY) :-
    	var(AttrY).
    impose_domain(X, Y, AttrY) :-
    	nonvar(AttrY),
	copy_ic_term(Y, T, AttrY),
	unify_any_ic(X, T).
    	
    % Specialised unification:
    % - T is assumed to be a domain-only ic variable (no alias, no delays)
    % - we don't call wake/0
    unify_any_ic(X, T) :-
    	free(X), !, X = T.
    unify_any_ic(X, _T{ic:Attr}) ?-
	unify_ic(X, Attr, _SuspAttr).	% cannot handle free(X) !


    %
    % impose_bounds(?X, ++Lo, ++Hi)
    %	The set_bounds/3 handler for the IC attribute.  Simply imposes the
    %	bounds Lo and Hi on the IC variable X.
    %
    %	Note that this predicate assumes Lo and Hi are exact (i.e. floats
    %	are not widened), and if they're bounded reals, only the lower bound
    %	of Lo and the upper bound of Hi are considered.
    %
    %	Unlike the above predicates, impose_bounds/3 does call wake/0 after
    %	imposing the bounds, so that any woken goals of higher than the
    %	current priority are executed before this predicate returns.
    %
impose_bounds(X, Lo, Hi) :-
	var(X), !,
	ic_impose_bounds(X, Lo, Hi),
	wake.
impose_bounds(X, Lo, Hi) :-
	X >= Lo, X =< Hi.


%---------------------------------------------------------------------
%
% Attribute access.
%
%	Miscellaneous predicates for accessing properties of an IC variable,
%	which also work when the variable has become ground (where
%	appropriate).
%

    %
    % is_solver_var(?X)
    %	Succeeds iff X is a variable with the IC attribute.
    %
is_solver_var(_{ic:(ic with [])}) :- -?-> true.

    %
    % is_exact_solver_var(?X)
    %	Succeeds iff X is a variable with the IC attribute,
    %	has integer type and sufficiently small bounds to be precise.
    %
is_exact_solver_var(_{ic:(ic with [var_type:integer,lo:L,hi:H])}) :- -?->
    -9.0e15 =< L, H =< 9.0e15.	% about -2^52..2^52 double precision

    %
    % is_solver_type(?X)
    %	Succeeds iff X is a variable with the IC attribute or a ground
    %	number.
    %
is_solver_type(_{ic:(ic with [])}) :- -?-> true.
is_solver_type(X) :- number(X).

    %
    % get_solver_type(?X, ?Type)
    %	Unifies Type with the IC type (integer or real) of the variable X.
    %	Also works if X is ground and of an appropriate type.
    %
get_solver_type(X, Type) :-
	var(X), !,
	get_ic_attr(X, Attr),
	Attr = ic with [var_type:Type].
get_solver_type(X, integer) :-
	integer(X).
get_solver_type(X, real) :-
	( float(X) ; rational(X) ; breal(X) ).

    %
    % get_min(?X, -Lo)
    %	Unifies Lo with the lower IC bound of the variable X.  If X is an
    %	integer variable then the bound returned will be integer (unless
    %	it is infinite).
    %	Also works if X is a ground number.
    %
get_min(X, Lo) :-
	get_bounds(X, Lo, _).

    %
    % get_max(?X, -Hi)
    %	Unifies Hi with the upper IC bound of the variable X.  If X is an
    %	integer variable then the bound returned will be integer (unless
    %	it is infinite).
    %	Also works if X is a ground number.
    %
get_max(X, Hi) :-
	get_bounds(X, _, Hi).

    %
    % get_float_bounds(?X, -Lo, -Hi)
    %	Unifies Lo and Hi with the "native" (float) lower and upper (resp.)
    %	IC bounds of the variable X.
    %	Also works if X is a ground number.
    %
get_float_bounds(X, Lo, Hi) :-
	var(X), !,
	get_ic_attr(X, Attr),
	Attr = ic with [lo:Lo, hi:Hi].
get_float_bounds(X, Lo, Hi) :-
	float(X), !,
	Lo = X, Hi = X.
get_float_bounds(X, Lo, Hi) :-
	number(X), !,
	B is breal(X),
	breal_bounds(B, Lo, Hi).
get_float_bounds(X, Lo, Hi) :-
	error(5, get_float_bounds(X, Lo, Hi)).

    %
    % get_integer_bounds(?X, -Lo, -Hi)
    %	Unifies Lo and Hi with the lower and upper (resp.) IC bounds of the
    %	variable X as integers.
    %	If X is a variable, it is constrained to be an integral IC variable.
    %	If X is a ground integer, returns that integer as the bounds.
    %
get_integer_bounds(X, Lo, Hi) :-
	var(X), !,
	get_integer_bounds1(X, 0, Lo0, Hi0, Wake),
	wake_and_reget_integer_bounds_if_needed(Wake, X, Lo0, Hi0, Lo, Hi).
get_integer_bounds(X, Lo, Hi) :-
	integer(X), !,
	Lo = X, Hi = X.
get_integer_bounds(X, Lo, Hi) :-
	error(5, get_integer_bounds(X, Lo, Hi)).

    %
    % get_finite_integer_bounds(?X, -Lo, -Hi)
    %	Unifies Lo and Hi with the lower and upper (resp.) IC bounds of the
    %	variable X as integers.
    %	If X is a variable, it is constrained to be an integral IC variable.
    %	If X is a ground integer, returns that integer as the bounds.
    %	A neater / more efficient implementation of this deferred until it
    %	is rewritten in C.
    %
get_finite_integer_bounds(X, Lo, Hi) :-
	var(X), !,
	get_integer_bounds1(X, 1, Lo0, Hi0, Wake),
	wake_and_reget_integer_bounds_if_needed(Wake, X, Lo0, Hi0, Lo, Hi).
get_finite_integer_bounds(X, Lo, Hi) :-
	integer(X), !,
	Lo = X, Hi = X.
get_finite_integer_bounds(X, Lo, Hi) :-
	error(5, get_finite_integer_bounds(X, Lo, Hi)).

    %
    % wake_and_reget_integer_bounds_if_needed(++Wake, ++Lo0, ++Hi0, -Lo, -Hi)
    %	Call wake/0 and re-get integer bounds if Wake = 1; just return given
    %	bounds if Wake = 0.
    %
wake_and_reget_integer_bounds_if_needed(0, _, Lo, Hi, Lo, Hi).
wake_and_reget_integer_bounds_if_needed(1, X, _, _, Lo, Hi) :-
	wake,
	% Bounds will already be finite if required, and no need to re-wake
	% (the call should not change anything this time anyway).
	get_integer_bounds1(X, 0, Lo, Hi, _).

    %
    % get_median(?X, -Median)
    %	Returns the (logarithmic) median of the bounds of X.
    %
get_median(X, Median) :-
	get_float_bounds(X, Lo, Hi),
	ria_binop(logsplit, Lo, Hi, 0.0, 0.5, Median, _).

    %
    % get_delta(?X, -Width)
    %	Returns the width of the interval X.
    %
get_delta(X, Width) :-
	get_bounds(X, Lo, Hi),
	ria_unop(width, Lo, Hi, _, Width).


%---------------------------------------------------------------------
%
% Accessing meta information 
%

    %
    % get_domain(Var, Domain)
    %
    %    Returns a compact ground representation of the domain of Var.
    %    Gives a type error if Var is not an IC variable or a number.
    %
get_domain(Var, Domain) :-
	( get_compact_domain(Var, Domain) ->
	    true
	; number(Var) ->
            Domain = Var
	;
	    error(5, get_domain(Var, Domain))
	).

    %
    % get_compact_domain(Var, Domain)
    %
    %    Returns a compact representation of the domain of Var.
    %
get_compact_domain(_{ic:Attr}, Dom):-
        -?->
        get_compact_domain(Attr, Dom).
get_compact_domain(ic with [lo:Lo0, hi:Hi0, var_type:Type, bitmap:Bitmap],
                   Domain) :-
	-?->
	( Bitmap = undefined ->
	    ( Type = real ->
		Lo = Lo0, Hi = Hi0,
		Domain = Lo..Hi
	    ;
		( Lo0 =:= -1.0Inf -> Lo=Lo0 ; Lo is fix(Lo0) ),
		( Hi0 =:= 1.0Inf -> Hi=Hi0 ; Hi is fix(Hi0) ),
		( Hi =:= Lo + 1 ->
		    Domain = [Lo, Hi]
		;
		    Domain = Lo..Hi
		)
	    )
	;
	    Lo is fix(Lo0),
	    get_bitmap_domain(Lo, Bitmap, Domain0),
	    ( Domain0 = [R] ->
	    	Domain = R
	    ;
	    	Domain = Domain0
	    )
	).

    %
    % get_bitmap_domain(++Lo, ++Bitmap, ?Domain)
    %	Unifies Domain with a printable representation of the contents of
    %	the bitmap Bitmap, starting at the element Lo.
    %
get_bitmap_domain(Lo, Bitmap, Domain) :-
	next_greater_non_member(Bitmap, Lo, Missing),
	End is Missing - 1,
	( End =:= Lo ->
	    Domain = [Lo | Domain1]
	; End =:= Lo + 1 ->
	    Domain = [Lo, End | Domain1]
	;
	    Domain = [Lo..End | Domain1]
	),
	( next_greater_member(Bitmap, Missing, NextLo) ->
              get_bitmap_domain(NextLo, Bitmap, Domain1)
	;
	    Domain1 = []
	).
        
        
        
    %
    % get_domain_as_list(Var, List)
    %
    %    Returns the domain of Var as an enumerated list of elements.
    %    Only works for integer variables and constant numbers.
    %
get_domain_as_list(Var, List) :-
        domain_to_list(Var, List0),
	(
	    foreach(SubRange, List0),
	    fromto(List, List, ListTail, []), param(Var)
	do
	    ( SubRange = L..U ->
                  ( (float(L);float(U)) ->
                        %% bounds are floats, (ie Var is a 'real' type,
                        %% or one of the bounds is infinite
                        error(6, get_domain_as_list(Var, List))
                  ;
                        true
                  ),
                  (
                      for(X, L, U),
                      fromto(List, [X | ListTail], ListTail, ListTail)
                  do
                      true
                  )
	    ;
		% SubRange is a single number
		List = [SubRange | ListTail]
	    )
	).

domain_to_list(Var, List) :-
	( get_compact_domain(Var, List0) ->
	     (List0 = _.._ -> List = [List0] ; List0 = List)
	; number(Var) ->
              List = [Var]
	; error(5, get_domain_as_list(Var, List))
	).


%---------------------------------------------------------------------
%
% Printing.
%



    %
    % print_solver_var(?X, ?Printed)
    %	If X is a variable with an IC attribute, or an IC attribute itself,
    %	then unifies Printed with a printable representation of the domain.
    %
print_solver_var(Var, Printed) :-
        get_compact_domain(Var, Printed).

%---------------------------------------------------------------------
%
% Unify.
%

% Fully implemented in C now.

%---------------------------------------------------------------------
%
% Test unification.
%

    %
    % test_unify_ic(?Term, ?Attr)
    %	The test_unify/2 handler for IC variables.
    %	Tests whether Term can be unified with the IC attribute Attr (which
    %	may be unbound if the corresponding variable was not an IC variable).
    %
test_unify_ic(_, Attr) :-
	/*** ANY + VAR ***/
	var(Attr).		% Ignore if no attribute for this extension
test_unify_ic(Term, Attr) :-
	compound(Attr),
	test_unify_term_ic(Term, Attr).

:- mode test_unify_term_ic(?, +).
test_unify_term_ic(Term,
		ic with [var_type:Type, lo:Lo, hi:Hi, bitmap:Bitmap]) :-
	nonvar(Term),
	/*** NONVAR + META ***/
	compatible_type(Term, Type),
	( Bitmap == undefined ->
	    Lo =< Term, Term =< Hi
	;
	    bitmap_contains(Bitmap, Term)
	).
test_unify_term_ic(Y{ic:AttrY}, AttrX) :-
	-?->
	test_unify_ic_ic(Y, AttrX, AttrY).

test_unify_ic_ic(_, _AttrX, AttrY) :-
	/*** VAR + META ***/
	var(AttrY).			% No attribute for this extension
test_unify_ic_ic(_Y, AttrX, AttrY) :-
	nonvar(AttrY),
	/*** META + META ***/
	AttrY = ic with [lo:LoY, hi:HiY, bitmap:BitmapY],
	AttrX = ic with [lo:LoX, hi:HiX, bitmap:BitmapX],
	LoX =< HiY, LoY =< HiX,		% Intervals overlap.
	( BitmapX == undefined ->
	    ( BitmapY == undefined ->
	    	true
	    ;
		% Make sure Y's bitmap contains an element within X's
		% bounds.
		IntLoX_1 is fix(ceiling(LoX)) - 1,
	    	next_greater_member(BitmapY, IntLoX_1, Member),
		Member =< HiX
	    )
	;
	    ( BitmapY == undefined ->
		% Make sure X's bitmap contains an element within Y's
		% bounds.
		IntLoY_1 is fix(ceiling(LoY)) - 1,
	    	next_greater_member(BitmapX, IntLoY_1, Member),
		Member =< HiY
	    ;
	    	% Make sure bitmap intersection is not empty.
		bitmaps_have_non_empty_intersection(BitmapY, BitmapX)
	    )
	).


%---------------------------------------------------------------------
%
% Instance comparison.
%

    %
    % compare_ic_instances(?Res, ?X, ?Y)
    %	If X is an instance of Y, vice-versa, or both, then Res is unified
    %	with <, >, or =, respectively.  Fails if none of these relationships
    %	holds.
    %

	% At least one of TermL or TermR is meta.
	% Metas with empty attribute slot are treated like free.
compare_ic_instances(Res, _X{ic:AttrX}, Y) :- -?->
	compare_instances_meta_any(Res, AttrX, Y).
compare_ic_instances(Res, X, Y) :- free(X),
	compare_instances_free_meta(Res, X, Y).		% Y must be meta!
compare_ic_instances(Res, X, Y) :- nonvar(X),
	compare_instances_const_meta(Res, X, Y).	% Y must be meta!

compare_instances_free_meta(Res, _X, _Y{ic:AttrY}) :- -?->
	compare_instances_free_ic(Res, AttrY).

compare_instances_meta_any(Res, AttrX, _Y{ic:AttrY}) :- -?->
	compare_instances_meta_meta(Res, AttrX, AttrY).
compare_instances_meta_any(Res, AttrX, Y) :- free(Y),
	compare_ic_instances_free(Res, AttrX).
compare_instances_meta_any(Res, AttrX, Y) :- nonvar(Y),
	compare_instances_meta_const(Res, AttrX, Y).

compare_instances_const_meta(Res, X, _Y{ic:AttrY}) :- -?->
	compare_instances_const_ic(Res, X, AttrY).

compare_instances_meta_const(Res, _X{ic:AttrX}, Y) :- -?->
	compare_ic_instances_const(Res, AttrX, Y).

compare_instances_free_ic(Res, AttrY) :- var(AttrY),
	Res = (=).
compare_instances_free_ic(Res, AttrY) :- nonvar(AttrY),
	Res = (>).

compare_ic_instances_free(Res, AttrX) :- var(AttrX),
	Res = (=).
compare_ic_instances_free(Res, AttrX) :- nonvar(AttrX),
	Res = (<).

compare_instances_meta_meta(Res, AttrX, AttrY) :- var(AttrX),
	compare_instances_free_meta(Res, AttrY).
compare_instances_meta_meta(Res, AttrX, AttrY) :- nonvar(AttrX),
	compare_ic_instances_meta(Res, AttrX, AttrY).

compare_instances_free_meta(Res, AttrY) :- var(AttrY),
	Res = (=).
compare_instances_free_meta(Res, AttrY) :- nonvar(AttrY),
	compare_instances_free_ic(Res, AttrY).


compare_instances_const_ic(Res, _X, AttrY) :- var(AttrY), !,
	Res = (<).
compare_instances_const_ic(Res, X,
		ic with [var_type:TY, lo:LoY, hi:HiY, bitmap:BitmapY]) :-
	compatible_type(X, TY),		% may fail
	LoY =< X, X =< HiY,		% may fail
	( BitmapY = undefined ->
	    true
	;
	    IntX is fix(X),
	    bitmap_contains(BitmapY, IntX)	% may fail
	),
	Res = (<).

compare_ic_instances_const(Res, AttrX, _Y) :- var(AttrX), !,
	Res = (>).
compare_ic_instances_const(Res,
		ic with [var_type:TX, lo:LoX, hi:HiX, bitmap:BitmapX], Y) :-
	compatible_type(Y, TX),		% may fail
	LoX =< Y, Y =< HiX,		% may fail
	( BitmapX = undefined ->
	    true
	;
	    IntY is fix(Y),
	    bitmap_contains(BitmapX, IntY)	% may fail
	),
	Res = (>).

compare_ic_instances_meta(Res, _AttrX, AttrY) :- var(AttrY),
	Res = (<).
compare_ic_instances_meta(Res,
		ic with [var_type:TX, lo:LoX, hi:HiX, bitmap:BitmapX],
		ic with [var_type:TY, lo:LoY, hi:HiY, bitmap:BitmapY]) :- -?->
	( BitmapX \== undefined, BitmapY \== undefined ->
	    % If both bitmaps are defined, simply compare them.
	    compare_bitmaps(Res, BitmapX, BitmapY)
	;
	    % At least one bitmap is undefined, so start with comparing bounds.
	    ( LoX == LoY, HiX == HiY ->
		( TX == TY -> Res0 = (=)
		; TX == integer -> Res0 = (<)
		; Res0 = (>) )
	    ; LoX =< LoY, HiY =< HiX ->
		( TX == integer -> TY == integer	% may fail
		; true ),
		Res0 = (>)
	    ; LoY =< LoX, HiX =< HiY ->
		( TY == integer -> TX == integer	% may fail
		; true ),
		Res0 = (<)
	    ;
		fail
	    ),

	    % Now see if any defined bitmaps are compatible with the result so far.
	    ( Res0 = (=) ->
		( BitmapX \== undefined ->
		    IntLoY is fix(LoY),
		    IntHiY is fix(HiY),
		    ( bitmap_contains_range(BitmapX, IntLoY, IntHiY) ->
			Res = (=)
		    ;
			Res = (<)
		    )
		; BitmapY \== undefined ->
		    IntLoX is fix(LoX),
		    IntHiX is fix(HiX),
		    ( bitmap_contains_range(BitmapY, IntLoX, IntHiX) ->
			Res = (=)
		    ;
			Res = (>)
		    )
		;
		    Res = (=)
		)
	    ; Res0 = (<) ->
		( BitmapY \== undefined ->
		    IntLoX is fix(LoX),
		    IntHiX is fix(HiX),
		    bitmap_contains_range(BitmapY, IntLoX, IntHiX)	% may fail
		;
		    true
		),
		Res = (<)
	    ; Res0 = (>) ->
		( BitmapX \== undefined ->
		    IntLoY is fix(LoY),
		    IntHiY is fix(HiY),
		    bitmap_contains_range(BitmapX, IntLoY, IntHiY)	% may fail
		;
		    true
		),
		Res = (>)
	    )
	).


%---------------------------------------------------------------------
%
% Term copying.
%

    %
    % copy_ic_term(?X, ?Copy)
    %	Unifies Copy with a copy of the IC variable X.
    %	Essentially, it makes a copy of X's IC attribute and adds it to Copy.
    %
copy_ic_term(X{ic:AttrX}, Copy) :-
	-?->
	copy_ic_term(X, Copy, AttrX).

copy_ic_term(_, _, AttrX) :-
	/*** VAR ***/
	var(AttrX).
copy_ic_term(_, Copy,
		ic with [var_type:Type, lo:Lo, hi:Hi, bitmap:Bitmap]) :-
	-?->
	/*** META ***/
	( Bitmap == undefined ->
	    NewBitmap = undefined
	;
	    copy_bitmap(Bitmap, NewBitmap)
	),
	Attr = ic with [%variable: Copy,
			var_type:Type, lo:Lo, hi:Hi, bitmap:NewBitmap,
		    min:[], max:[], hole:[], type:[]],
	add_attribute(Copy, Attr, ic).


%---------------------------------------------------------------------
%
% Delayed goal access.
%

    %
    % ic_suspensions(?X, -Susps, ?Susps0)
    %	Susps is unified with the result of appending Susps0 to the list of
    %	all the IC suspension lists attached to X.
    %
ic_suspensions(_{ic:Attr}, Susps, Susps0) ?-
	( var(Attr) ->
	    Susps=Susps0
	;
	    Attr = ic with [min:Min, max:Max, hole:Hole, type:Type],
	    Susps = [Min, Max, Hole, Type | Susps0]
	).


    %
    % delayed_goals_number(?X, -N)
    %	Unifies N with the number of goals currently delayed in X's IC
    %	suspension lists.
    %	Also returns suitable values for non-IC variables and non-variables.
    %
delayed_goals_number(_{ic:AttrX}, N) :-
	-?->
	!,
	dgn_ic(AttrX, N).
delayed_goals_number(X, N) :-
	var(X),
	N = 0.
delayed_goals_number(X, N) :-
	nonvar(X),
	N = 1.0Inf.

dgn_ic(AttrX, 0) :-
	/*** VAR ***/
	var(AttrX).
dgn_ic(ic with [min:Min, max:Max, hole:Hole, type:Type], N) :-
	-?->
	/*** META ***/
	count_active_suspensions(Min, 0, N1),
	count_active_suspensions(Max, N1, N2),
	count_active_suspensions(Hole, N2, N3),
	count_active_suspensions(Type, N3, N).

count_active_suspensions([Susp|Susps], N0, N) :-
        -?->
        !,
        ( is_suspension(Susp) -> N1 is N0 + 1 ; N1 = N0 ),
        count_active_suspensions(Susps, N1, N).
count_active_suspensions(_, N, N).


%---------------------------------------------------------------------
%
% Most specific generalisation.
%

ic_over(X, Type, Lo, Hi) :-
	impose_min(X, Lo),
	impose_max(X, Hi),
	set_vars_type(X, Type).



msg(X{ic:AttrX},Y,Msg):-  % Meta + Any
	-?->
	!,
	msg_meta_any(X,AttrX,Y,Msg).
msg(X,Y{ic:_AttrY},Msg):-  % (Const/Var) + Meta
	-?->
	!,
	%% compiler bug - should be able to extract the Attribute of Y in the
	%% clause head, but the compiler gets confused, so we do it here
	%% explicitly
	true,                  % compiler bug
	get_ic_attr(Y, AttrY), % compiler bug
	true,                  % compiler bug
	msg_meta_any(Y,AttrY,X,Msg).
msg(X,Y,Msg):-   % Const + (Const/Var)
	number(X),!,
	msg_const_any(X,Y,Msg).
msg(_X, _Y, _Msg).    % Var + (Const/Var)


msg_const_any(X,Y,Msg):-     % NumConst + NumConst
	number(Y),!,
	get_solver_type(X, XT),
	get_solver_type(Y, YT),
	get_bounds(X, XL, XH),
	get_bounds(Y, YL, YH),
	Lo is min(XL, YL),
	Hi is max(XH, YH),
	( (XT == integer, YT == integer) ->
		  ( X == Y ->
			    Msg = X
		  ;
			    ic_over(Msg, integer, Lo, Hi),
			    get_ic_attr(Msg, AttrMsg),
			    %% Make a bitmap with only X and Y
			    LoI is fix(Lo),
			    HiI is fix(Hi),
			    (HiI > LoI+1 ->
				%% There is a hole, so we need a bitmap
				create_bitmap(LoI, HiI, Bitmap),
				GapLo is LoI+1,
				GapHi is HiI-1,
				remove_bitmap_range(Bitmap, GapLo, GapHi,
				                    _Result,
						    BitmapUnion),
				setarg(bitmap of ic, AttrMsg, BitmapUnion),
				schedule_suspensions(hole of ic, AttrMsg)
			    ;
			        %% There is no hole so we need no bitmap
			        true
			    )
		  )
	;
		  ic_over(Msg, real, Lo, Hi)
	).
msg_const_any(_X, _Y, _Msg).    % Const + Free/NonNum


msg_non_overlap(XL, XH, YL, YH, Lo, Hi, Msg):-
	%% X and Y ranges do not overlap
	%% so create a bitmap containing the complete range
	%% and then remove the gap
	LoI is fix(Lo),
	HiI is fix(Hi),
	create_bitmap(LoI, HiI, Bitmap),
	GapLo is fix(min(XH, YH))+1,
	GapHi is fix(max(XL, YL))-1,
	remove_bitmap_range(Bitmap,GapLo,GapHi,_Result,
	BitmapUnion),
	get_ic_attr(Msg, AttrMsg),
	setarg(bitmap of ic, AttrMsg, BitmapUnion),
	schedule_suspensions(hole of ic, AttrMsg).


msg_meta_any(X,AttrX,Y{ic:AttrY},Msg):-  % Meta + Meta
	-?->
	!,
	get_solver_type(X, XT),
	get_solver_type(Y, YT),
	get_bounds(X, XL, XH),
	get_bounds(Y, YL, YH),
	Lo is min(XL, YL),
	Hi is max(XH, YH),
	( XT == integer, YT == integer ->
		  ic_over(Msg, integer, Lo, Hi),
		  AttrX = ic with [bitmap:BitmapX],
		  AttrY = ic with [bitmap:BitmapY],
		  ( (Lo = -1.0Inf; Hi = +1.0Inf) ->
			    %% one of the bounds is infinte, so nothing more
			    %% to do
			    true
		  ; (BitmapX \= undefined, BitmapY \= undefined) ->
			    %% Both domains have bitmaps so take the union
			    get_ic_attr(Msg, AttrMsg),
			    %AttrMsg = ic with [bitmap:BitmapMsg],
			    bitmap_union(BitmapX, BitmapY,
					 BitmapUnion),
			    setarg(bitmap of ic, AttrMsg, BitmapUnion),
			    schedule_suspensions(hole of ic, AttrMsg)
		  ; (BitmapX \= undefined) ->
			    % X has bitmap Y does not so create a bitmap for Y
			    LoI is fix(YL),
			    HiI is fix(YH),
			    create_bitmap(LoI, HiI, Bitmap),
			    get_ic_attr(Msg, AttrMsg),
			    bitmap_union(BitmapX, Bitmap,
					 BitmapUnion),
			    setarg(bitmap of ic, AttrMsg, BitmapUnion),
			    schedule_suspensions(hole of ic, AttrMsg)
		  ; (BitmapY \= undefined) ->
			    % Y has bitmap X does not so create a bitmap for X
			    LoI is fix(XL),
			    HiI is fix(XH),
			    create_bitmap(LoI, HiI, Bitmap),
			    get_ic_attr(Msg, AttrMsg),
			    bitmap_union(BitmapY, Bitmap,
					 BitmapUnion),
			    setarg(bitmap of ic, AttrMsg, BitmapUnion),
			    schedule_suspensions(hole of ic, AttrMsg)
		  ; (XH < YL; YH < XL) ->
		            msg_non_overlap(XL, XH, YL, YH, Lo, Hi, Msg)
			    %% X and Y ranges do not overlap
			    %% so create a bitmap containing the complete range
			    %% and then remove the gap
			    %LoI is fix(Lo),
			    %HiI is fix(Hi),
			    %create_bitmap(LoI, HiI, Bitmap),
			    %GapLo is fix(min(XH, YH))+1,
			    %GapHi is fix(max(XL, YL))-1,
			    %remove_bitmap_range(Bitmap,GapLo,GapHi,_Result,
				%		BitmapUnion),
			    %get_ic_attr(Msg, AttrMsg),
			    %setarg(bitmap of ic, AttrMsg, BitmapUnion),
			    %schedule_suspensions(hole of ic, AttrMsg)
		  ; 
			    %% The ranges of X and Y overlap so nothing more
			    %% to do
			    true
		  )
	;
	          ic_over(Msg, real, Lo, Hi)
	).
msg_meta_any(X,AttrX,Y,Msg):-  % Meta + NumConst
	number(Y),!,
	get_solver_type(X, XT),
	get_solver_type(Y, YT),
	get_bounds(X, XL, XH),
	get_bounds(Y, YL, YH),
	Lo is min(XL, YL),
	Hi is max(XH, YH),
	( XT == integer, YT == integer ->
		  ic_over(Msg, integer, Lo, Hi),
		  AttrX = ic with [bitmap:BitmapX],
		  ( (BitmapX \= undefined) ->
			    %% X has a bitmap, so add the constant Y to it
			    get_ic_attr(Msg, AttrMsg),
			    %AttrMsg = ic with [bitmap:BitmapMsg],
			    create_bitmap(Y, Y, BitmapY),
			    bitmap_union(BitmapX, BitmapY,
					 BitmapUnion),
			    setarg(bitmap of ic, AttrMsg, BitmapUnion),
			    schedule_suspensions(hole of ic, AttrMsg)
		  ; (Lo = -1.0Inf ; Hi = +1.0Inf) ->
			    %% One of the bounds is infinite, so cannot 
			    %% create a bitmap
			    true
		  ; (XH < Y ; Y < XL) ->
		            msg_non_overlap(XL, XH, Y, Y, Lo, Hi, Msg)
			    %% The constant Y is not in the range of X, 
			    %% so create a bitmap containing the range of X and
			    %% the single value Y
			    %LoI is fix(Lo),
			    %HiI is fix(Hi),
			    %create_bitmap(LoI, HiI, Bitmap),
			    %GapLo is fix(min(XH, Y))+1,
			    %GapHi is fix(max(XL, Y))-1,
			    %remove_bitmap_range(Bitmap,GapLo,GapHi,_Result,
				%		BitmapUnion),
			    %get_ic_attr(Msg, AttrMsg),
			    %setarg(bitmap of ic, AttrMsg, BitmapUnion),
			    %schedule_suspensions(hole of ic, AttrMsg)
		  ; 
			    %% The constant Y is in the range of X, 
			    %% so nothing more to do
			    true
		  )
	;
		  ic_over(Msg, real, Lo, Hi)
	).
msg_meta_any(_X,_AttrX,_Y,_Msg).  % Meta + Free/NonNum



%---------------------------------------------------------------------
%
% is_in_domain(++Val,?Var)
% is_in_domain(++Val,?Var,?Result)
%
% Determines if Val is contained in the domain of Var. An exception is
% thrown if Val is a bounded real and either...
%   1) Var is a bounded real overlapping Val
%   2) Var is an ic variable and the bounded real Val contains either bound
%
is_in_domain(Val, Var) :-
	var(Val),
	!,
	 % instantiation fault
	error(4, is_in_domain(Val, Var)).
is_in_domain(Val, Var) :-
	number(Val),
	( var(Var) -> true ; number(Var) ),
	!,
	is_in_domain(Val, Var, Result),
        is_in_domain_check_result(Result).
is_in_domain(Val, Var) :-
	error(5, is_in_domain(Val, Var)).


is_in_domain_check_result(yes).
is_in_domain_check_result(maybe) :-
	exit_block('undecidable comparison of bounded reals').
%is_in_domain_check_result(no) :-
%        fail.
        


%---------------------------------------------------------------------
%
% is_in_domain(++Val,?Var,?Result)
%
% Determines if Val is contained in the domain of Var. Unifies Result
% to 'yes' if Val is in the domain of Var, unifies to 'false' if it is
% not and unifies Result to 'maybe' if
%   1) Var is a bounded real overlapping Val
%   2) Var is an ic variable and the bounded real Val contains either bound
%
is_in_domain(Val, Var, Result):-
	get_ic_attr(Var, Attr),
        !,
	Attr = ic with [var_type:Type, lo:Lo, hi:Hi, bitmap:Bitmap],
        is_in_domain_var(Val, Var, Type, Lo, Hi, Bitmap, Result).
is_in_domain(Val, Var, Result):-
        number(Val),
        !,
	( not Val =:= Var ->
	    Result = no
	; not Val =\= Var ->
	    Result = yes
	;
	    Result = maybe
	).
is_in_domain(Val, Var, Result) :-
	var(Val),
        !,
        % instantiation fault
	error(4, is_in_domain(Val, Var, Result)).
is_in_domain(Val, Var, Result):-
	error(5, is_in_domain(Val, Var, Result)).






%---------------------------------------------------------------------
%
% is_in_domain_var(++Val, ?Var, ++Type, ++Lo, ++Hi, +Bitmap, ?Result)
%
% Determines whether Val and Var would unify given that Var is an IC
% variable with its attribute expanded to the arguments Type, Lo, Hi,
% Bitmap.
%
is_in_domain_var(Val, _Var, _Type, Lo, Hi, Bitmap, Result) :-
	integer(Val),
        !,
        % independant of the variables type
        (Bitmap == undefined ->
             ( (Lo =< Val,Val =< Hi) ->
                   Result = yes
             ;
                   Result = no
             )
        ;
             ( bitmap_contains(Bitmap, Val) ->
                   Result = yes
             ;
                   Result = no
             )
        ).
is_in_domain_var(Val, _Var, Type, Lo, Hi, _Bitmap, Result) :-
	float(Val),
        !,
        ((Type == real, Lo =< Val, Val =< Hi) ->
             Result = yes
        ; % Type == integer or  Val < Lo  or Val > Hi
             Result = no
	).
is_in_domain_var(Val, _Var, Type, Lo, Hi, _Bitmap, Result) :-
	(breal(Val);rational(Val)),
        !,
        X is breal(Val),
        ( Type == real ->
              breal_bounds(X, ValLo, ValHi),
              ( (Lo =< ValLo, ValHi =< Hi) ->
                    Result = yes
              ; (ValLo > Hi) ->
                    Result = no
              ; (ValHi < Lo) ->
                    Result = no
              ;
                    Result = maybe
              )
        ;
              Result = no
	).
is_in_domain_var(Val, Var, _Type, _Lo, _Hi, _Bitmap, Result) :-
	var(Val),
        !,
        % instantiation fault
	error(4, is_in_domain(Val, Var, Result)).
is_in_domain_var(Val, Var, _Type, _Lo, _Hi, _Bitmap, Result) :-
        % type error
        error(5, is_in_domain(Val, Var, Result)).



%---------------------------------------------------------------------
%
% Threshold adjustment.
%
% The threshold is a global setting specifying at what point bounds changes
% are considered too small to be worth making.  Basically, for non-integer
% variables, bounds are only changed if the absolute and relative changes of
% the bound exceed the threshold.  This means that constraints over real
% variables are only guaranteed to be consistent up to the current threshold
% (over and above any normal widening which occurs).
%
% We provide two predicates for setting the threshold.  The first,
% set_threshold/1, merely sets the threshold.  This is appropriate if the
% threshold is being increased, or if the caller does not mind that some
% constraints may not be consistent to the level specified by the new
% threshold.  The second, set_threshold/2, allows the user to specify a list
% of "variables of interest" when setting the threshold.  In this case, if
% the threshold has been decreased, all goals suspended on the bounds of the
% variables in this list with an IC attribute are woken.  Please see
% ic_design for a discussion of the rationale for this approach and the
% issues involved.
%
% Note that get_threshold/1 and set_threshold/1 are defined in ic.c.
%

set_threshold(Threshold, WakeVars) :-
	get_threshold(OldThreshold),
	set_threshold(Threshold),
	( OldThreshold > Threshold ->
	    ( collection_to_list(WakeVars, WakeList) ->
		wake_bounds(WakeList)
	    ;
		error(5, set_threshold(Threshold, WakeVars))
	    )
	;
	    true
	).

wake_bounds(WakeVars) :-
	(
	    foreach(Var, WakeVars)
	do
	    schedule_bounds(Var)
	),
	wake.

schedule_bounds(_X{ic:Attr}) :-
	-?->
	Attr = ic with [min:Lo, max:Hi],
	!,
	schedule_suspensions(1, sl(Lo)),
	schedule_suspensions(1, sl(Hi)).
schedule_bounds(_).


%---------------------------------------------------------------------
%
% Statistics collection.
%

:- local variable(ic_stat_event_list).

ic_stat(on) :-
	stat(on) -> true ; compile_term([ic_event(C) :- incval(C), stat(on)]).
ic_stat(off) :-
	stat(off) -> true ; compile_term([ic_event(_), stat(off)]).
ic_stat(reset) :-
	getval(ic_stat_event_list,EL),
	(foreach(Event-_,EL) do
	    setval(Event,0)
	).
ic_stat(print) :-
	ic_stat_get(StatsList),
	(
	    foreach(Stat, StatsList)
	do
	    printf("%w%n", [Stat])
	).

ic_stat_get(StatList) :-
	getval(ic_stat_event_list,EL),
	(foreach(Event-Desc,EL), foreach(Desc=Count,StatList) do
	    getval(Event,Count)
	).

ic_stat_register_event(Event,Desc):-
	getval(ic_stat_event_list,EL),
	(memberchk(Event-Desc,EL) -> NewEL=EL ; setval(Event,0),NewEL=[Event-Desc|EL]),
	setval(ic_stat_event_list,NewEL).

ic_event(_).
stat(off).
:- setval(ic_stat_event_list,[]).

%---------------------------------------------------------------------
%
% Miscellaneous.
%

    %
    % compatible_type(+X, +Type)
    %	Succeeds iff the ground value X is compatible with the IC type
    %	Type.
    %
compatible_type(X, _) :-
	% An integer is compatible with either type.
	integer(X).
compatible_type(X, real) :-
	% Floats, rationals and bounded reals are only compatible with real.
	( float(X) ; rational(X) ; breal(X) ).


% XXX - The following should perhaps go...?

result_is_empty(Result) :-
	Result /\ 4 =:= 4.
result_is_non_empty(Result) :-
	Result /\ 4 =\= 4.
result_is_change(Result) :-
	Result /\ 1 =:= 1.
result_is_slack(Result) :-
	Result /\ 2 =:= 2.

%---------------------------------------------------------------------
%
% User documentation for the IC kernel module.
%

:- comment(summary, "Low-level interface to the common kernel of the IC solver").
:- comment(author, "Warwick Harvey").

:- comment(desc, html("<P>
This module provides a low-level interface to the core of the IC solver;
that is, a set of predicates for accessing and manipulating IC variables.
It is not intended for use by general user code (use the interface provided
by the `ic' module for that), but rather for those wishing to implement
their own constraints which use IC variables.  This could either be to
augment IC by providing new constraints or functionality, or to build a
complete constraint solver capable of cooperating with any other solver
which also uses the common numeric variable format provided by IC.</P>
")).

%---------------------------------------------------------------------

:- comment(get_ic_attr/2, [
    amode: get_ic_attr(?,-),
    args: [
    	"X": "A variable",
	"Attr": "The IC attribute of <TT>X</TT>"
    ],
    summary: "Returns the IC attribute of a variable, creating a new attribute if necessary.",
    fail_if: "X is not a variable",
    desc: html("<P>
   If X is a variable and does not already have an IC attribute, it is given
   a new one.  Attr is then bound to this attribute (new or old).  Attr must
   be a fresh variable.  Fails if X is not a variable.
")
]).

%---------------------------------------------------------------------

:- comment(impose_min/2, [
    amode: impose_min(?, ++),
    args: [
    	"Var":   "Variable or number",
	"Bound": "Lower bound (number)"
    ],
    summary: "Update (if required) the lower bound of Var.",
    see_also: [impose_max/2, impose_bounds/3, impose_domain/2, exclude/2, exclude_range/3],
    desc: html("<P>
   Primitive for updating the lower bound of Var so that it is at least
   Bound.  A bound update on a variable may fail (when the update empties
   the domain), succeed (possibly updating the variable's bounds), or
   instantiate the variable (in the case where the domain gets restricted to
   a singleton value).  Note that if the variable's type is integer, its
   bounds will always be adjusted to integral values.</P><P>

   Note that this predicate is intended for use only in implementing
   constraint propagators, and should not be called from ordinary user code
   (use ic:(Var >= Bound) instead).  It differs from the usual constraint
   predicates in several ways.  First, Bound is assumed to be exact (i.e.
   if it's a float, it's not widened).  Second, if Bound is a bounded real,
   only its lower bound is significant, and it is this which is imposed on
   Var.</P><P>

   The final and most important difference relates to the execution of
   delayed goals.  If the call to impose_min/2 results in a bound change,
   any delayed goals suspended on that bound will be scheduled for
   execution, as normal.  However, impose_min/2 does not call the woken goal
   scheduler (wake/0), so these goals may not be executed immediately.  (It
   is possible that under some circumstances the goals will be executed, if
   wake/0 is called indirectly - one example would be by the unify handler
   if the variable becomes ground - but this should not be relied upon.) To
   ensure that the goals are eventually executed, the caller should arrange
   for wake/0 to be called at some appropriate point in the subsequent
   execution.  Please see the \"Advanced Control Features\" section of the
   User Manual for more information about woken goal management.</P>
"),
    eg: "\
[eclipse 2]: X $:: 0..10, impose_min(X, 4.5).
X = X{4.5 .. 10.0}
Yes (0.00s cpu)

[eclipse 3]: X $:: 0..10, impose_min(X, 4.5), integers([X]).
X = X{5 .. 10}
Yes (0.00s cpu)

[eclipse 4]: X $:: 0..10, impose_min(X, 4.5), integers([X]), impose_max(X, 5.9).
X = 5
Yes (0.00s cpu)

[eclipse 5]: X $:: 0..10, impose_min(X, 4.5), impose_max(X, 4.3).
No (0.00s cpu)
"
]).

%---------------------------------------------------------------------

:- comment(impose_max/2, [
    amode: impose_max(?, ++),
    args: [
    	"Var":   "Variable or number",
	"Bound": "Upper bound (number)"
    ],
    summary: "Update (if required) the upper bound of Var.",
    see_also: [impose_min/2, impose_bounds/3, impose_domain/2, exclude/2, exclude_range/3],
    desc: html("<P>
   Primitive for updating the upper bound of Var so that it is at most
   Bound.  A bound update on a variable may fail (when the update empties
   the domain), succeed (possibly updating the variable's bounds), or
   instantiate the variable (in the case where the domain gets restricted to
   a singleton value).  Note that if the variable's type is integer, its
   bounds will always be adjusted to integral values.</P><P>

   Note that this predicate is intended for use only in implementing
   constraint propagators, and should not be called from ordinary user code
   (use ic:(Var =< Bound) instead).  It differs from the usual constraint
   predicates in several ways.  First, Bound is assumed to be exact (i.e. if
   it's a float, it's not widened).  Second, if Bound is a bounded real,
   only its upper bound is significant, and it is this which is imposed on
   Var.</P><P>

   The final and most important difference relates to the execution of
   delayed goals.  If the call to impose_max/2 results in a bound change,
   any delayed goals suspended on that bound will be scheduled for
   execution, as normal.  However, impose_max/2 does not call the woken goal
   scheduler (wake/0), so these goals may not be executed immediately.  (It
   is possible that under some circumstances the goals will be executed, if
   wake/0 is called indirectly - one example would be by the unify handler
   if the variable becomes ground - but this should not be relied upon.)  To
   ensure that the goals are eventually executed, the caller should arrange
   for wake/0 to be called at some appropriate point in the subsequent
   execution.  Please see the \"Advanced Control Features\" section of the
   User Manual for more information about woken goal management.</P>
"),
    eg: "\
[eclipse 3]: X $:: 0..10, impose_min(X, 4.5).
X = X{4.5 .. 10.0}
Yes (0.00s cpu)

[eclipse 4]: X $:: 0..10, impose_min(X, 4.5), integers([X]).
X = X{5 .. 10}
Yes (0.00s cpu)

[eclipse 5]: X $:: 0..10, impose_min(X, 4.5), integers([X]), impose_max(X, 5.9).
X = 5
Yes (0.00s cpu)

[eclipse 6]: X $:: 0..10, impose_min(X, 4.5), impose_max(X, 4.3).
No (0.00s cpu)
"
]).

%---------------------------------------------------------------------

:- comment(impose_domain/2, [
    amode: impose_domain(?, ?),
    args: [
    	"Var":   "Variable or number",
	"DomVar": "Variable or number"
    ],
    summary: "Restrict (if required) the domain of Var t othe domain of DomVar.",
    see_also: [impose_min/2, impose_max/2, impose_bounds/3, exclude/2, exclude_range/3],
    desc: html("<P>
   Primitive for restricting the domain of Var to the domain of DomVar.
   Any values in the domain of Var, which are not also in the domain of
   DomVar, are removed.  DomVar remains unaffected.  
   The domain update on Var may fail (when the update empties the domain),
   succeed (possibly updating the variable's domain), or instantiate the
   variable (in the case where the domain gets restricted to a singleton
   value).  Note that if DomVar's type is integer, the integrality will
   be imposed on Var as well as the domain values.</P><P>

   Note that this predicate is intended for use only in implementing
   constraint propagators, and should not be called from ordinary user code.
   The waking behaviour is the same as discussed for impose_min/2 and
   impose_max/2.  Apart from this, the effect is similar to unifying
   Var with a copy of DomVar.
   </P>
"),
    eg: "\
    ?- X::1..9, Y::5..7, impose_domain(X, Y).
    X = X{5 .. 7}
    Y = Y{5 .. 7}
    Yes (0.00s cpu)

    ?- X::1..9, impose_domain(X, 7).
    X = 7
    Yes (0.00s cpu)

    ?- X::1..9, Y::4.1..7.5, impose_domain(X, Y).
    X = X{5 .. 7}
    Y = Y{4.1 .. 7.5}
    Yes (0.00s cpu)

    ?- X::1.0..9.0, Y::5..7, impose_domain(X, Y).
    X = X{5 .. 7}
    Y = Y{5 .. 7}
    Yes (0.00s cpu)

    ?- X::1..3, Y::5..7, impose_domain(X, Y).
    No (0.00s cpu)

    ?- Y::1..5, impose_domain(3, Y).
    Y = Y{1 .. 5}
    Yes (0.00s cpu)

    ?- Y::1..5, impose_domain(6, Y).
    No (0.00s cpu)

    ?- Y::1..5, impose_domain(X, Y).
    Y = Y{1 .. 5}
    X = X{1 .. 5}
    Yes (0.00s cpu)
"
]).

%---------------------------------------------------------------------

:- comment(impose_bounds/3, [
    amode: impose_bounds(?, ++, ++),
    args: [
    	"Var": "Variable or number",
	"Lo":  "Lower bound",
	"Hi":  "Upper bound"
    ],
    summary: "Update (if required) the bounds of Var.",
    see_also: [impose_min/2, impose_max/2, ic:($::)/2, ic:(::)/2],
    desc: html("<P>
   Primitive for updating the upper and lower bounds of Var, also used as
   the set_bounds handler for the IC attribute.  As with impose_min/2 and
   impose_max/2, it is intended for use in implementing constraint
   propagators, and should not be called from ordinary user code (use $::/2
   or ::/2 instead).  Its semantics is essentially:
<PRE>
       impose_min(Var, Lo), impose_max(Var, Hi), wake.
</PRE>
   Please see the documentation for impose_min/2 and impose_max/2 for more
   details.</P>
")
]).

%---------------------------------------------------------------------

:- comment(exclude/2, [
    amode: exclude(?, ++),
    args: [
    	"Var":  "Integer variable or integer",
	"Excl": "Integer value to exclude"
    ],
    summary: "Exclude the element Excl from the domain of Var.",
    see_also: [exclude_range/3, impose_min/2, impose_max/2, impose_domain/2],
    desc: html("<P>
   Primitive for excluding an element from the domain of an integer
   variable.  The call may fail (when Var is the same integer as Excl),
   succeed (possibly updating the variable's domain), or instantiate the
   variable (when Excl was one of only two domain elements left).</P><P>

   Note that this predicate is intended for use only in implementing
   constraint propagators, and should not be called from ordinary user code
   (use ic:(Var =\\= Excl) instead).  It differs from the usual constraint
   predicates with respect to the execution of delayed goals.  If the call
   to exclude/2 results in a domain change, any delayed goals suspended on
   that change will be scheduled for execution, as normal.  However,
   exclude/2 does not call the woken goal scheduler (wake/0), so these goals
   may not be executed immediately.  (It is possible that under some
   circumstances the goals will be executed, if wake/0 is called indirectly
   - one example would be by the unify handler if the variable becomes
   ground - but this should not be relied upon.)  To ensure that the goals
   are eventually executed, the caller should arrange for wake/0 to be
   called at some appropriate point in the subsequent execution.  Please see
   the \"Advanced Control Features\" section of the User Manual for more
   information about woken goal management.</P>
"),
    eg: "\
[eclipse 3]: X :: 0..10, exclude(X, 4).
X = X{[0 .. 3, 5 .. 10]}
Yes (0.00s cpu)

[eclipse 4]: X :: [4, 6], exclude(X, 4).
X = 6
Yes (0.00s cpu)
"
]).

%---------------------------------------------------------------------

:- comment(exclude_range/3, [
    amode: exclude_range(?, ++, ++),
    args: [
    	"Var": "Integer variable or integer",
	"Lo":  "Integer lower bound of range to exclude",
	"Hi":  "Integer upper bound of range to exclude"
    ],
    summary: "Exclude the elements Lo..Hi from the domain of Var.",
    see_also: [exclude/2, impose_min/2, impose_max/2, impose_domain/2],
    desc: html("<P>
   Primitive for excluding the integers between Lo and Hi (inclusive) from
   the domain of an integer variable.  The call may fail (when the domain of
   Var has no elements outside the range Lo..Hi), succeed (possibly updating
   the variable's domain), or instantiate the variable (in the case where
   the domain gets restricted to a singleton value).</P><P>

   Note that this predicate is intended for use only in implementing
   constraint propagators, and should not be called from ordinary user code.
   It differs from the usual constraint predicates with respect to the
   execution of delayed goals.  If the call to exclude_range/3 results in a
   domain change, any delayed goals suspended on that change will be
   scheduled for execution, as normal.  However, exclude_range/3 does not
   call the woken goal scheduler (wake/0), so these goals may not be
   executed immediately.  (It is possible that under some circumstances the
   goals will be executed, if wake/0 is called indirectly - one example
   would be by the unify handler if the variable becomes ground - but this
   should not be relied upon.)  To ensure that the goals are eventually
   executed, the caller should arrange for wake/0 to be called at some
   appropriate point in the subsequent execution.  Please see the \"Advanced
   Control Features\" section of the User Manual for more information about
   woken goal management.</P>
"),
    eg: "\
[eclipse 3]: X :: 0..10, exclude_range(X, 4, 7).
X = X{[0 .. 3, 8 .. 10]}
Yes (0.00s cpu)

[eclipse 4]: X :: 0..10, exclude_range(X, 9, 20).
X = X{0 .. 8}
Yes (0.00s cpu)

[eclipse 5]: X :: [2, 4, 6 .. 10], exclude_range(X, 4, 12).
X = 2
Yes (0.00s cpu)
"
]).

%---------------------------------------------------------------------

:- comment(msg/3, [
amode: msg(-,-,?), amode:msg(++,++,?),
args:  ["Var1": "A variable or number",
	"Var2": "A variable or number",
	"MSG": "Most specific generalisation (variable)"
       ],
summary: "Computes the most specific generalisation of Var1 and Var2 that is expressible with ic variables.",
desc: html("\
<P>
   The most specific generalisation of two intervals is computed and
   returned as MSG.  MSG will interval over the smallest interval enclosing
   the input intervals, and have the more general type of the input types.
   If either Var1 or Var2 are domain-less, or have values that cannot be
   expressed as ic-domains, MSG remains unbound.
</P>")
]).

%---------------------------------------------------------------------
%---------------------------------------------------------------------
:- comment(ic_stat/1, [
    amode: ic_stat(++),
    args: [
    	"Command": "Statistics related command (eg. turn on, print)"
    ],
    summary: "Enables/disables/resets/prints stats gathering information",
    see_also: [ic_stat_register_event/2, ic_stat_get/1, ic_event/1],
    desc: html("<P>
   The available commands for controlling statistics gathering are...
<DL>
	<DT>on    <DD>Enables the recording of stats events.
	<DT>off   <DD>Disables the recording of stats events.
	<DT>reset <DD>Zero's all stats event counters.
	<DT>print <DD>Prints, on standard output, the value of all registered
	      stats event counters.
</DL><P>
   User programs may register their own events to be recorded by calling
   <TT>ic_stat_register_event/2</TT>.
")
]).

%---------------------------------------------------------------------


:- comment(ic_stat_get/1, [
    amode: ic_stat_get(-),
    args: [
    	"StatsList": "The list of stats events description=value pairs"
    ],
    summary: "Returns the list of stats events description=value pairs",
    see_also: [ic_stat/1, ic_stat_get/1, ic_event/1],
    desc: html("<P>
   Returns the list of stats events description=value pairs.
<P>
   IC records the following statistics by default.  Please note that since
   these depend on the internals of IC, they are subject to change without
   notice, and should be used as a guide only.
<DL>
	<DT>ic_lin_create
	    <DD>Number of linear constraints set up.
	<DT>ic_lin_prop
	    <DD>Number of times a linear constraint is propagated.
	<DT>ic_uni_prop/ic_bin_prop/ic_tern_prop
	    <DD>Number of times a non-linear (unary/binary/ternary) operator is
	    propagated.
	<DT>ic_split
	    <DD>Number of domain splits in locate/2,3,4.
	<DT>ic_squash
	    <DD>Number of squash attempts in squash/3 or locate/4.
</DL>
")
]).

%---------------------------------------------------------------------

:- comment(ic_stat_register_event/2, [
    amode: ic_stat_register_event(++,++),
    args: [
    	"Event": "The short name of the event",
    	"Description": "The long name of the event"
    ],
    summary: "Registers a new stats event to be collected",
    see_also: [ic_stat/1, ic_stat_get/1, ic_event/1],
    desc: html("<P>
   When a program wishes to record events of interest throughout its runtime
   it can do so by first registering the event type with the stats logger at
   the beginning of the program.  E.g.
<PRE>
	ic_stat_register_event('myevent','My really important event').
</PRE>
   Calls to ic_event/1 can then be inserted in the appropriate places
   throughout the code.  E.g.
<PRE>
	foo:-
		ic_event('myevent'),
		bar(X).	
</PRE>
   While stats gathering is not enabled (i.e. without the program explicitly
   calling <TT>ic_stat(on)</TT>) these events are processed very efficiently, and so
   can be left in code without incurring any significant overhead.
")
]).

%---------------------------------------------------------------------
:- comment(ic_event/1, [
    amode: ic_event(++),
    args: [
    	"Event": "The short name of the event to record"
    ],
    summary: "Indicates that the specified event has occured.",
    see_also: [ic_stat/1, ic_stat_get/1, ic_stat_register_event/2],
    desc: html("<P>
   Indicates that the specified event has occured.  Can be left in
   production code as the call succeeds very quickly when stats gathering is
   not turned on.
")
]).
%---------------------------------------------------------------------
:- comment(set_vars_type/2, [
    amode: set_vars_type(?, ++),
    args: [
    	"Vars": "Var/list/matrix",
	"Type": "Type to be set 'real' or 'integer'"
    ],
    summary: "Sets the type of the given variables to the given type.",
    see_also: [set_var_type/2,reals/1,integers/1],
    desc: html("<P>
   Typically not called directly but indirectly through <TT>reals/1</TT> and
   <TT>integers/1</TT>.</P>
"),
   exceptions: [
        4: "Type is un-instantiated."
   ]    
]).
%---------------------------------------------------------------------
:- comment(set_var_type/2, [
    amode: set_var_type(?, ++),
    args: [
    	"Var":  "Variable to set type of",
	"Type": "Type to be set 'real' or 'integer'"
    ],
    summary: "Sets the type of the given variable to the given type.",
    see_also: [set_vars_type/2,reals/1,integers/1],
    desc: html("<P>
   Typically not called directly but indirectly through <TT>reals/1</TT> and
   <TT>integers/1</TT>.</P>
")
]).

