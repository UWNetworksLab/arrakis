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
% Version:	$Id: fd_domain.pl,v 1.2 2008/08/20 18:04:18 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * FINITE DOMAINS
 *
 * IDENTIFICATION:      fd_domain.pl 
 *
 * AUTHOR:		Micha Meier
 *
 * DESCRIPTION:         Finite domain data type and the handling of
			the 'fd' attribute.
 */


:- module(fd_domain).

:- export syntax_option(dense_output).

:- reexport
	% domain access
	dom_range/3,
	dom_check_in/2,

	% domain modification
	dvar_replace/2,
	dvar_remove_element/2,
	dvar_remove_greater/2,
	dvar_remove_smaller/2,

	% domain processing
	dom_compare/3,
	dom_intersection/4,
	dom_union/4,
	dom_difference/4,

	integer_list_to_dom/2
    from sepia_kernel.

    /*****************************************************************
     * A domain variable that apears in some constraints is represented
     * by a metaterm.
     * The metaterm is represented by
     */

:- export struct(fd(domain, min, max, any)).
    /*
     *
     *	where
     *
     *		min - goals to be woken if the domain minimum changes
     *		max - goals to be woken if the domain maximum changes
     *		any - the delayed goals woken if the domain is changed
     *
     *		domain - the representation of the domain itself dom(List, Size)
     *
     * A structure declaration is used so that e.g.
     *
     *		fd with domain:D
     *
     * represents
     *
     *		fd(_, _, _, D)
     * and
     *		min of fd
     * is 3. All operations on the fd/4 structure should be done
     * with these macros so that the access is independent of the
     * actual structure.
     */

%----------------------------------------------------------------
% Attribute definition
%----------------------------------------------------------------

:- meta_attribute(fd, [
	unify:			unify_domain/3,
	test_unify:		test_unify_domain/2,
	compare_instances:	compare_instances_domain/3,
	copy_term:		copy_term_domain/2,
	suspensions:		suspensions_domain/3,
	delayed_goals_number:	delayed_goals_number_domain/2,
	get_bounds:		get_fd_bounds/3,
	set_bounds:		set_fd_bounds/3,
	print:			tr_fd_domain_out/2
	]).

% Export transformation routines.
:- export
	fd_dom_simple/2,
	fd_dom_simple/3,
	tr_fd_domain_in/2,
	tr_fd_domain_out/2.

% Output Macros
% Hide the attribute structure on output
% print the metaterm alone as [Domain]

:- export macro(property(functor) of fd, tr_fd_domain_out/2, [write, protect_arg]).
:- export macro(dom/2, tr_fd_domain_out/2, [write, protect_arg]).
:- export macro(dom_ent/3, tr_fd_domain_out/2, [write, goal]).
:- export macro(fd_dom_simple/2, tr_fd_domain_out/2, [write, goal]).
:- export macro(fd_dom_simple/3, tr_fd_domain_out/2, [write, goal]).

:- export op(700, xfx, #::). 

% Goal Macros
:- inline((::)/2, tr_fd_domain_in/2).
:- inline((::)/3, tr_fd_domain_in/2).
:- inline((#::)/2, tr_fd_domain_in/2).
:- inline((#::)/3, tr_fd_domain_in/2).

:- export
    :: /2,
    :: /3,
    #:: /2,
    #:: /3,
    indomain/1,
    is_domain/1,
    is_integer_domain/1,
    par_indomain/1,

    % domain access
    dom_member/2,
    dom_size/2,
    new_domain_var/1,

    % domain processing
    dom_copy/2,
    dom_to_list/2,
    list_to_dom/2,
    sorted_list_to_dom/2,

    % various
    var_fd/2,
    dvar_attribute/2,
    dvar_domain/2,
    constraints_number/2,

    % var modification
    dvar_update/2,
    dvar_update_nocheck/3,

    dvar_msg/3.

:-  local get_attribute/2.

:- import
	check_dom/1	% should really be defined here...
    from fd_arith.

:- import
	% general-purpose predicates
	add_attribute/3,
	fd_init/0,
	get_bip_error/1,
	remove_element/3,
	setarg/3,
	set_bip_error/1,
	suspensions_to_goals/3,
	trprotect/2,

	% FD-specific predicates
	attr_instantiate/2
    from sepia_kernel.


% Initialize the C variables
:- fd_init.

:- pragma(nodebug).
:- pragma(system).

fderror(N, G) :-
	error(N, G, _).

%
% Transformation routines
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Input goal transformation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Goal Source Transformation
tr_fd_domain_in(V #:: D, G) :-
    tr_fd_domain_in(V :: D, G).
tr_fd_domain_in(#::(V, D, B), G) :-
    tr_fd_domain_in(::(V, D, B), G).
tr_fd_domain_in(V :: D, G) :-
    -?->
    !,
    varset(V),
    ground(D),
    make_domain(D, Dom, _),
    G = fd_domain:fd_dom_simple(V, Dom).
tr_fd_domain_in(::(V, D, B), G) :-
    -?->
    !,
    var(V),
    ground(D),
    make_domain(D, Dom, _),
    G = fd_domain:fd_dom_simple(V, Dom, B).
    
    varset(V) :- var(V), !.
    varset([_|_]).
    varset(subscript(_,_)).

% Domain Output Transformation
tr_fd_domain_out(_{fd:(fd with domain:dom(D, _))}, T) :-
	-?->
	T = D.
tr_fd_domain_out(fd with domain:dom(D, _), T) :-
	-?->
	T = D.
tr_fd_domain_out(dom(D, S), T) :-
	-?->
	(is_finite(D) ->
	    T = dom(D, S)
	;
	D = [T] ->
	    true
	;
	    T = D
	).
tr_fd_domain_out(dom_ent(X, Dom, B), T) :-
	-?->
	T = ::(X, Dom, B).
tr_fd_domain_out(fd_dom_simple(X, Dom), T) :-
	-?->
	T = ::(X, Dom).
tr_fd_domain_out(fd_dom_simple(X, Dom, B), T) :-
	-?->
	T = ::(X, Dom, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 	THE FD EXTENSION
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%----------------------------------------------------------------
% unification
%----------------------------------------------------------------

% unify_domain(+Term, ?Attribute, ?XSuspAttr)
unify_domain(Term, Attr, XSuspAttr) :-
    /*** ANY + VAR ***/
    var(Attr),
    % Nothing to do unless there's a constrained list for X and Term is an
    % FD variable.
    ( nonvar(XSuspAttr), is_domain(Term) ->
	schedule_suspensions(constrained of suspend, XSuspAttr)
    ;
	true
    ).
unify_domain(Term, Attr, XSuspAttr) :-
    compound(Attr),
    unify_term_domain(Term, Attr, XSuspAttr).

% We wake every time a variable is touched.
:- mode unify_term_domain(?, +, ?).
unify_term_domain(Term, Attr, _XSuspAttr) :-
    nonvar(Term),		% The metaterm was instantiated, wake all
    /*** NONVAR + META ***/
    Attr = fd with [],
    attr_instantiate(Attr, Term).
unify_term_domain(Y{fd:AttrY}, AttrX, XSuspAttr) :-
    -?->
    unify_domain_domain(Y, AttrX, AttrY, XSuspAttr).

unify_domain_domain(Y, AttrX, AttrY, _XSuspAttr) :-
    var(AttrY),				% no attribute for this extension
    /*** VAR + META ***/
    AttrY = AttrX,			% share the attribute
    notify_constrained(Y).
unify_domain_domain(Y, AttrX, AttrY, XSuspAttr) :-
    nonvar(AttrY),
    /*** META + META ***/
    AttrY = fd with domain:DomY,
    AttrX = fd with domain:DomX,
    dom_intersection(DomX, DomY, NewDom, Size),
    (Size = 1 ->
	NewDom = dom([Y|_], _),		% bind Y, wake inst,bound,constrained
	attr_instantiate(AttrX, Y)	% wake the fd lists
    ;
	attr_bind(AttrX, NewDom, _, XSuspAttr),	% empties the woken lists
	attr_bind(AttrY, NewDom, Y, _),
	dvar_replace(Y, NewDom),
	merge_suspension_lists(min of fd, AttrX, min of fd, AttrY),
	merge_suspension_lists(max of fd, AttrX, max of fd, AttrY),
	merge_suspension_lists(any of fd, AttrX, any of fd, AttrY)
    ).


% Do the wakings that result from changing Attr's domain to NewDom
attr_bind(Attr, NewDom, Var, SuspAttr) :-
    Attr = fd with [domain:D],
    dom_size(D, S),
    dom_size(NewDom, NewS),
    (NewS < S ->
	( nonvar(SuspAttr) ->
	    schedule_suspensions(constrained of suspend, SuspAttr)
	;
	    notify_constrained(Var)
	),
	schedule_suspensions(any of fd, Attr),
	(dom_range(D, Min, Max),
	dom_range(NewDom, NewMin, NewMax) ->
	    (NewMin > Min ->
		schedule_suspensions(min of fd, Attr)
	    ;
		true
	    ),
	    (NewMax < Max ->
		schedule_suspensions(max of fd, Attr)
	    ;
		true
	    )
	;
	    true
	)
    ;
	true
    ).


%----------------------------------------------------------------
% unification test
%----------------------------------------------------------------

% test_unify_domain(+Term, Attribute)
test_unify_domain(_, Attr) :-
    /*** ANY + VAR ***/
    var(Attr).			% Ignore if no attribute for this extension
test_unify_domain(Term, Attr) :-
    nonvar(Attr),
    test_unify_term_domain(Term, Attr).

% We wake every time a variable is touched.
:- mode test_unify_term_domain(?, +).
test_unify_term_domain(Term, fd with domain:D) :-
    -?->
    /*** NONVAR + META ***/
    nonvar(Term),		% Check here if the instantiation is accepted.
    dom_check_in(Term, D).
test_unify_term_domain(Y{fd:AttrY}, AttrX) :-
    -?->
    test_unify_domain_domain(Y, AttrX, AttrY).

test_unify_domain_domain(_, _, AttrY) :-
    /*** VAR + META ***/
    var(AttrY).				% no attribute for this extension
test_unify_domain_domain(Y, fd with domain:DomX, fd with domain:DomY) :-
    -?->
    /*** META + META ***/
    dom_intersection(DomX, DomY, NewDom, _),
    dvar_replace(Y, NewDom).		% may create a singleton domain;
					% but there is no easy way to bind
					% and invoke other handlers

%----------------------------------------------------------------
% instances
%----------------------------------------------------------------

% compare_instances_domain(-Res, ?TermL, ?TermR)
% One or both Terms are attributed variables
compare_instances_domain(Res, X{fd:AttrX}, Y) :- -?->
    compare_instances_meta_any(Res, X, Y, AttrX).
compare_instances_domain(Res, X, Y{fd:AttrY}) :- -?-> not meta(X),
    compare_instances_term_meta(Res, X, Y, AttrY).

compare_instances_meta_any(Res, X, Y{fd:AttrY}, AttrX) :- -?->
    compare_instances_meta_meta(Res, X, Y, AttrX, AttrY).
compare_instances_meta_any(Res, _X, Y, AttrX) :- not meta(Y),
    /*** META + ANY ***/
    /* can succeed only if Y is a variable */
    var(Y),
    (var(AttrX) ->
	Res = (=)
    ;
	Res = (<)
    ).

compare_instances_meta_meta(Res, X, Y, AttrX, AttrY) :-
    var(AttrX),		% No attribute
    /*** VAR + META or VAR ***/
    compare_instances_term_meta(Res, X, Y, AttrY).
compare_instances_meta_meta(Res, X, Y, AttrX, AttrY) :-
    nonvar(AttrX),
    compare_instances_domain_meta(Res, X, Y, AttrX, AttrY).

compare_instances_domain_meta(Res, _, _, _, AttrY) :-
    var(AttrY),
    /*** META + VAR ***/
    Res = (<).
compare_instances_domain_meta(Res, _, _, fd with domain:D1,
	fd with domain:D2) :-
    -?->
    /*** META + META ***/
    dom_compare(Res, D1, D2).

compare_instances_term_meta(Res, TermL, Y, AttrY) :-
    var(AttrY),
    /*** TERM + VAR ***/
    compare_instances(Res, TermL, Y).
compare_instances_term_meta(Res, TermL, _, fd with domain:D) :-
    -?->
    /*** TERM + META ***/
    nonvar(TermL),	% otherwise Res = >
    dom_check_in(TermL, D),
    (dom_size(D, 1) ->
	Res = (=)
    ;
	Res = (<)
    ).

%----------------------------------------------------------------
% copy_term
%----------------------------------------------------------------

copy_term_domain(X{fd:AttrX}, Copy) :-
    -?->
    copy_term_domain(X, Copy, AttrX).


copy_term_domain(_, _, AttrX) :-
    /*** VAR ***/
    var(AttrX).
copy_term_domain(_, Copy, fd with domain:dom(D, S)) :-
    -?->
    /*** META ***/
    empty_domain(dom(D, S), ND),
    add_attribute(Copy, ND, fd).

empty_domain(D, fd with [domain:D, any:[], min:[], max:[]]).

%----------------------------------------------------------------
% suspensions
%----------------------------------------------------------------

suspensions_domain(_{fd:AttrX}, Susps, Susps0) :-
    -?->
    susp_domain(AttrX, Susps, Susps0).

susp_domain(AttrX, Susps, Susps) :- var(AttrX), !.
susp_domain(fd with [min:Mi, max:Ma, any:B], [Mi,Ma,B|Susps], Susps).


%----------------------------------------------------------------
% delayed goals number
%----------------------------------------------------------------

delayed_goals_number_domain(_{fd:AttrX}, N) :-
    -?->
    dgn_domain(AttrX, N).

dgn_domain(AttrX, 0) :-
    /*** VAR ***/
    var(AttrX).
dgn_domain(fd with [any:B, min:Mi, max:Ma], N) :-
    -?->
    /*** META ***/
    count_active_suspensions(B, 0, N0),
    count_active_suspensions(Mi, N0, N1),
    count_active_suspensions(Ma, N1, N).


count_active_suspensions([Susp|Susps], N0, N) :-
	-?->
	!,
	( is_suspension(Susp) ->
		N1 is N0 + 1
	;
		N1 = N0
	),
	count_active_suspensions(Susps, N1, N).
count_active_suspensions(_, N, N).

% Due to the implementation, it may happen that a metaterm
% occurs in a predicate even if it should not, namely in the case that
% the metaterm is instantiated and a simple goal follows; then
% the domain_unify/1 handler is called only *after* the simple goal.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Attaching and querying the domain
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ?Vars :: ?Domain 
%	The variable(s) Vars have the domain Domain
Varset #:: Domain :-
    Varset :: Domain.
Varset :: Domain :-
    var(Domain),
    get_domain(Varset, Domain).
Varset :: Domain :-
    nonvar(Domain),
    make_domain(Domain, DomRep, Varset),
    fd_dom_simple(Varset, DomRep).

:- mode get_domain(?,-).
get_domain(X, Domain) :- var(X), !,
    (dvar_domain(X, dom(Domain, _)) -> true; fderror(4, X::Domain)).
get_domain(X, Domain) :-
    varset(X), !,
    fderror(5, X::Domain).
get_domain(X, [X]).

% fd_dom_simple(+varset, +domain)
fd_dom_simple(Var, Dom) :- var(Var), !,
    var_fd(Var, Dom).
fd_dom_simple([], _) :- !.
fd_dom_simple([X|Xs], Dom) :- !,
    fd_dom_simple(X, Dom),
    fd_dom_simple(Xs, Dom).
fd_dom_simple(subscript(Array,Index), Dom) :- !,
    subscript(Array, Index, Varset),
    fd_dom_simple(Varset, Dom).
fd_dom_simple(Val, Dom) :-
    dom_check_in(Val, Dom).

var_fd(Var, Domain) :-
    dom_size(Domain, Size),
    ( Size > 1 ->
      dom_copy(Domain, D),
      empty_domain(D, Dom),
      set_domain_var1(Var, Dom)
    ;
	singleton_dom(Var, Domain)
    ).

set_domain_var1(Var{fd:(fd with [])}, Dom) :-
    -?->
    !,
    add_attribute(Var, Dom, fd).	% will be notified in the handler
set_domain_var1(Var, Dom) :-
    add_attribute(Var, Dom, fd),
    new_domain_var(Var),
    notify_constrained(Var),
    wake.

new_domain_var(_).			% primitive hook for extensions

%
% entailment
%
#::(Var, Int, B) :-
    ::(Var, Int, B).
::(Var, Int, B) :-
    nonvar(Int),
    make_domain(Int, DomEnt, Var),
    fd_dom_simple(Var, DomEnt, B).
::(Var, Int, B) :-
    var(Int),
    fderror(4, ::(Var, Int, B)).

dom_ent(Var, DomEnt, B) :-
    dvar_domain(Var, Dom),
    dom_ent(Var, DomEnt, B, Dom).

dom_ent(Var, DomEnt, 0, Dom) :-
    -?->
    dom_difference(Dom, DomEnt, NewDom, _),
    dvar_update(Var, NewDom).
dom_ent(Var, DomEnt, 1, Dom) :-
    -?->
    dom_intersection(Dom, DomEnt, NewDom, _),
    dvar_update(Var, NewDom).
dom_ent(Var, DomEnt, B, Dom) :-
    var(B),
    dom_size(Dom, Size),
    (dom_intersection(Dom, DomEnt, _, SizeInt) ->
	(Size = SizeInt ->
	    B = 1
	;
	    make_suspension(dom_ent(Var, DomEnt, B), 3, Susp),
	    insert_suspension(Var, Susp, any of fd, fd),
	    insert_suspension(B, Susp, inst of suspend, suspend)
	)
    ;
	B = 0
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Conversion to the internal representation
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Create a representation of the domain.
make_domain([H|T], Domain, Var) :-
    !,
    make_dom([H|T], Domain, Var).
% Only for Chip compatibility 
make_domain(Start:End, Domain, Var) :-
    !,
    make_dom([Start..End], Domain, Var).
make_domain([], Domain, _Var) :-
    !,
    empty_dom(Domain).
make_domain(Value, Domain, Var) :-
    make_dom([Value], Domain, Var).

make_dom(List, dom(Domain, Size), _) :-
    make_ground_dom(List, Domain, Size),
    !,
    Size > 0.
make_dom(List, _, Var) :-
    get_bip_error(Err),
    fderror(Err, Var :: List).

empty_dom(dom([], 0)).
singleton_dom(Value, dom([Value], 1)).

sorted_list_to_dom(List, dom(D, _)) :-
    -?->
    List = D.
sorted_list_to_dom(List, Dom) :-
    var(Dom),
    Dom = dom(List, Size),
    list_size(List, 0, Size).

list_size([], S, S).
list_size([H|T], S0, S) :-
    el_size(H, S1),
    S2 is S0 + S1,
    list_size(T, S2, S).

el_size(M..N, S) :-
    !,
    S is N - M + 1.
el_size(_, 1).

list_to_dom(List, dom(Domain, Size)) :-
    make_ground_dom(List, Domain, Size),
    !,
    Size > 0.
list_to_dom(List, Domain) :-
    get_bip_error(Err),
    fderror(Err, list_to_dom(List, Domain)).

make_ground_dom(List, Domain, Size) :-
    sort(List, SList),
    domain_types(SList, Domain, FL, Integers, Intervals, AfterInt, 0, S),
    make_integer_subdom(Integers, Intervals, DI, SU),
    append(DI, AfterInt, FL),
    Size is S + SU,
    (Size > 16'7fffffff ->
	set_bip_error(6)
    ;
	true
    ).

%
% domain_types(List, Floats, FC, Integers, Intervals, Atomic, Ac, ASize)
% Split the sorted input list into a list of different types:
%	Floats (they are smaller than any other atomic type)
%	Integers and intervals
%	Other atomic types (greater than integers and floats)
% FC is the tail of the Floats list, it is used for appending the integers
% and the rest.

:- mode domain_types(+,-,-,-,-,-,+,-).
domain_types([], F, F, [], [], [], N, N) :- !.
domain_types([H|T], F, F0, I, S, A, N0, N) :-
    domain_types1(H, F, F0, I, S, A, N0, N, T).

/*    domain_types1(-,-,-,-,-,-,+,-,+) */
  
    domain_types1(Var, _, _, _, _, _, _, _, _) :-
    	var(Var),
	!,
	set_bip_error(4).
    domain_types1(H, [H|F1], F0, I, S, A, N0, N, T) :-
    	float(H),
	!,
	N1 is N0 + 1,
	domain_types(T, F1, F0, I, S, A, N1, N).
    domain_types1(H, F, F0, [H|I], S, A, N0, N, T) :-
    	integer(H),
	!,
	domain_types(T, F, F0, I, S, A, N0, N).
    domain_types1(K..M, F, F0, I, S, A, N0, N, T) :-
	!,
	( integer(K) -> N1 = K
	; nonvar(K) -> N1 is K, ( integer(N1) -> true ; set_bip_error(5) )
	; set_bip_error(4) ),
	( integer(M) -> N2 = M
	; nonvar(M) -> N2 is M, ( integer(N2) -> true ; set_bip_error(5) )
	; set_bip_error(4) ),
	( N1 =< N2 ->
	    S = [N1..N2 | S1]
	;
	    S1 = S
	),
	domain_types(T, F, F0, I, S1, A, N0, N).
    domain_types1(Str, _, _, _, _, _, _, _, _) :-
	nonground(Str),
	!,
	set_bip_error(4).
    domain_types1(H, F, F0, I, S, [H|A], N0, N, T) :-
	N1 is N0 + 1,
	domain_types(T, F, F0, I, S, A, N1, N).


% Make an integer domain out of sorted integer and interval lists.
make_integer_subdom(Integers, Intervals, Dom, SU) :-
    integer_list_to_dom(Intervals, DS),
    integer_list_to_dom(Integers, DI),
    (dom_union(DS, DI, dom(Dom, SU), SU) ->
	true
    ;
	Dom = [],
	SU = 0
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Domain querying and updates
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_domain(_{fd:(fd with [])}) :- -?-> true.

is_finite(_{fd:(fd with [])}) :- -?-> true.
is_finite(I) :-
    integer(I).

is_integer_domain(_{fd:(fd with domain:D)}) :-
    -?->
    dom_range(D, _, _).

% Var is guaranteed to have an fd attribute!
get_fd_bounds(_{fd:(fd with [domain:D])}, L, H) :- -?-> !,
	dom_range(D, L, H).

% Var is guaranteed to have an fd attribute!
set_fd_bounds(Var, Lwb, Upb) :-
	L is fix(ceiling(Lwb)),
	U is fix(floor(Upb)),
	dvar_remove_smaller(Var, L),
	dvar_remove_greater(Var, U),
	wake.


dvar_domain(_{fd:(fd with [domain:D])}, Domain) :-
    -?->
    !,
    Domain = D.
dvar_domain(Var, D) :-
    nonvar(Var),
    singleton_dom(Var, D).

get_attribute(_{fd:Attr}, Meta) :-
    -?->
    compound(Attr),
    Attr = Meta.

dvar_attribute(_{fd:Attr}, DS) :-
    -?->
    !,
    nonvar(Attr),
    Attr = DS.
dvar_attribute(Var, _) :- var(Var), !, fail.
dvar_attribute(Value, Dom) :-
    nonvar(Value),
    singleton_dom(Value, D),
    empty_domain(D, Dom).

% Replace the domain by another one, do all checks
:- mode dvar_update(?, ++).
dvar_update(A, Dom) :-
    nonvar(A),
    Dom = dom([A], 1).
dvar_update(Var{fd:DS}, NewDom) :-
    -?->
    NewDom = dom(_, Size),
    dvar_update(Var, NewDom, DS, Size).

:- mode dvar_update(?, ++, ++, ++).
dvar_update(Var, dom([Var|_], _), _, 1) :- !.
dvar_update(Var, NewDom, DS, Size) :-
%    integer(Size),
%    Size > 1,
    DS = fd with domain:dom(_, OldSize),
    (Size < OldSize ->
	attr_bind(DS, NewDom, Var, _),
	dvar_replace(Var, NewDom)
    ;
    Size = OldSize ->
	true
    ;
	error(6, dvar_update(Var, NewDom))
    ).

:- mode dvar_update_nocheck(?, ++, ++).
dvar_update_nocheck(Var, [Var|_], 1) :- !.
dvar_update_nocheck(Var{fd:DS}, ND, Size) :-
    -?->
%    integer(Size),
%    Size > 1,
    NewDom = dom(ND, Size),
    attr_bind(DS, NewDom, Var, _),
    dvar_replace(Var, NewDom).

constraints_number(Var, Number) :-
    delayed_goals_number(Var, Number).

:- mode dvar_msg(?, ?, -).
dvar_msg(_A{fd:fd{domain:DA}}, B, M) ?- !,
	msg_domain(DA, B, M).
dvar_msg(A, _B{fd:fd{domain:DB}}, M) ?- !,
	msg_domain(DB, A, M).
dvar_msg(A, B, M) :-
	ground(A), !,
	msg_atomic(A, B, M).
dvar_msg(_A, _B, _M).

% even if B is an atomic term, A is neither an atomic term nor a domain variable

% A is a domain variable
msg_domain(DA, _B{fd:fd{domain:DB}}, M) ?- !,
	dom_union(DA, DB, DM, _),
	empty_domain(DM, Dom),
	add_attribute(M, Dom, fd).
msg_domain(DA, B, M) :-
	ground(B), !,
	( dom_check_in(B, DA) ->
	    empty_domain(DA, Dom),
	    add_attribute(M, Dom, fd)
	; singleton_dom(B, DB),
	  dom_union(DA, DB, DM, _),
	  empty_domain(DM, Dom),
	  add_attribute(M, Dom, fd)
        ).
msg_domain(_DA, _B, _M).

% A is a nonvar term
% B is not a domain variable
msg_atomic(A, B, M) :-
	ground(B), !,
	( A = B ->
	    M = A
	; sort([A, B], D),
	  M :: D
        ).
msg_atomic(_A, _B, _M).

indomain(Var{fd:(fd with domain:D)}) :-
    -?->
    !,
    dom_member(Var, D).
indomain(Val) :-
    nonvar(Val).
indomain(Var) :-
    var(Var),
    error(4, indomain(Var)).

par_indomain(Var{fd:(fd with domain:D)}) :-
    -?->
    !,
    par_dom_member(Var, D).
par_indomain(Val) :-
    nonvar(Val).
par_indomain(Var) :-
    var(Var),
    error(4, par_indomain(Var)).

% Enumerate the elements of a domain.
:- mode dom_member(?, ++).
dom_member(Val, dom([H|T], _)) :-
	dom_member(Val, H, T).

:- mode dom_member(?, ++, ++).
dom_member(Val, Start..End, T) :- !,
	interv_member(Val, Start, End, T).
dom_member(Val, Elem, T) :-
	elem_member(Val, Elem, T).

:- mode elem_member(?, ++, ++).
elem_member(Val, Val, _).
elem_member(Var, Val, [H|T]) :-
	remove_element(Var, Val, _),
	dom_member(Var, H, T).

:- mode interv_member(?, ++, ++, ++).
interv_member(Val, Start, End, _) :-
	between(Start, End, 1, Val).
interv_member(Var, _, _, [H|T]) :-
	dom_member(Var, H, T).


% Enumerate the elements of a domain (in parallel).
:- mode par_dom_member(?, ++).
par_dom_member(Val, dom([H|T], _)) :-
	par_dom_member(Val, H, T).

:- mode par_dom_member(?, ++, ++).
par_dom_member(Val, Start..End, T) :- !,
	par_interv_member(Val, Start, End, T).
par_dom_member(Val, Elem, T) :-
	par_elem_member(Val, Elem, T).

:- parallel par_elem_member/3.
:- mode par_elem_member(?, ++, ++).
par_elem_member(Val, Val, _).
par_elem_member(Var, Val, [H|T]) :-
	remove_element(Var, Val, _),
	par_dom_member(Var, H, T).

:- parallel par_interv_member/4.
:- mode par_interv_member(?, ++, ++, ++).
par_interv_member(Val, Start, End, _) :-
	End1 is End+1,
	N is End1-Start,
	fork(N, I),
	Val is End1-I.
par_interv_member(Var, _, _, [H|T]) :-
	par_dom_member(Var, H, T).

% must be after the make_domain/3 definition which is needed to expand ::
fd_dom_simple(Var, Dom, B) :-
    check_dom(Var),
    B :: 0..1,
    dom_ent(Var, Dom, B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%     Operations on domains (others are written in C)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Convert a domain to a list of its elements.
dom_to_list(dom(D, _), L) :-
	dom_to_list2(D, L).

dom_to_list2([], []).
dom_to_list2([Inter|Intervs], List) :-
        inter1_to_list(Inter, List, Last),
        dom_to_list2(Intervs, Last).
 
:- mode inter1_to_list(++, ?, ?).
inter1_to_list(Low..Up, List, Last) :-
	!,
        gen_list(Low, Up, List, Last). 
inter1_to_list(One, [One|Last], Last). 
 
% Make a partial list of integers from M to N
gen_list(Up, Up, [Up|Last], Last) :- !.  
gen_list(Low, Up, [Low|Next], Last) :- 
        NextLow is Low + 1,
        gen_list(NextLow, Up, Next, Last).



dom_size(dom(_, Size), Size).

dom_copy(dom(Dom, Size), dom(Dom, Size)).


:- untraceable
	unify_domain/3,
	unify_term_domain/3,
	test_unify_domain/2,
	compare_instances_domain/3,
	copy_term_domain/2,
	suspensions_domain/3,
	delayed_goals_number_domain/2,
	tr_fd_domain_in/2,
	tr_fd_domain_out/2.
