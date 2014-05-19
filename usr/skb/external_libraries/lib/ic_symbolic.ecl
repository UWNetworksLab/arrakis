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
% Copyright (C) 2002 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
%
% Solver for constraints over ordered symbolic domains
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc
% Version:	$Id: ic_symbolic.ecl,v 1.3 2008/06/20 13:41:14 jschimpf Exp $
%
% Todo:
%  -	compile time transformation could do all symbolic->int mapping
%	mapping at compile time. E.g.
%	:- local domain(colour(r,g,b)).
%	main :-
%		X &:: colour,
%		X &\= r.
%	would be preprocessed into
%	main :-
%		has_domain_internal(X, colour, 1..3),
%		symbol_domain_index(X, _, X_ic), ic:(X_ic =\= 1).
% ----------------------------------------------------------------------


:- module(ic_symbolic).

:- comment(summary, "Solver for constraints over ordered symbolic domains").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2008/06/20 13:41:14 $").
:- comment(copyright, "Cisco Systems, Inc.").


:- lib(ic).

:- export get_domain_as_list/2.	% hides ic predicate of the same name

:- meta_attribute(ic_symbolic, [
	print:get_domain_as_list/2,
	unify:unify_ic_symbolic/3,
	test_unify:test_unify_ic_symbolic/2,
	copy_term:copy_term_ic_symbolic/2,
	compare_instances:compare_instances_ic_symbolic/3
    ]).

:- local struct(ic_symbolic(
	    ic_var,		% corresponding ic variable
	    dom			% module:domain_name
    	)).


%----------------------------------------------------------------------
% variable declaration
%----------------------------------------------------------------------

:- export (&::)/2.
:- export op(700,xfx, &::).
:- tool((&::)/2, have_domain/3).
have_domain(X, DomSpec, Module) :- ( var(X) ; atomic(X) ), !,
	has_domain(X, DomSpec, Module).
have_domain(Xs, DomSpec, Module) :- Xs = [_|_], !,
	( domain_or_subdomain(DomSpec, Domain, IntRange, Module) ->
	    list_have_domain_range(Xs, DomSpec, Domain, IntRange, Module)
	;
	    printf(error, "Illegal right hand side in %w%n", [(Xs &:: DomSpec)]),
	    abort
	).
have_domain(subscript(Array,Index), DomSpec, Module) :- !,
	subscript(Array, Index, Elems),
	have_domain(Elems, DomSpec, Module).
have_domain(Xs, DomSpec, _Module) :-
	printf(error, "Illegal left hand side in %w%n", [(Xs &:: DomSpec)]),
	abort.


    has_domain(X, DomSpec, Module) :-
	( domain_or_subdomain(DomSpec, Domain, IntRange, Module) ->
	    has_domain_range(X, DomSpec, Domain, IntRange, Module)
	;
	    printf(error, "Illegal right hand side in %w%n", [X &:: DomSpec]),
	    abort
	).


    domain_or_subdomain(DomSpec, _Domain, _IntRange, _Module) :-
	var(DomSpec), !,
	fail.
    domain_or_subdomain(DomSpec, DefModule:DomSpec, IntRange, Module) :-
	atom(DomSpec),
	current_domain(DomSpec, DefModule, ValueArray)@Module,
	functor(ValueArray, _, DomSize),
	IntRange = 1..DomSize.
    domain_or_subdomain(Symbols, Domain, IntRange, Module) :-
	Symbols = [_|_],
	symbols_to_ints(Symbols, Domain, IntRange, [], [], [], [], Module).
    domain_or_subdomain([], nodomain, 1..0, _Module).
    	% 1..0 will cause failure later


    list_have_domain_range([], _DomSpec, _Domain, _IntRange, _Module) ?- !,
    	true.
    list_have_domain_range([X|Xs], DomSpec, Domain, IntRange, Module) ?- !,
	have_domain_range(X, DomSpec, Domain, IntRange, Module),
	list_have_domain_range(Xs, DomSpec, Domain, IntRange, Module).
    list_have_domain_range(X, DomSpec, _Domain, _IntRange, _Module) :-
	printf(error, "Illegal left hand side (improper list) in %w%n", [(X &:: DomSpec)]),
	abort.

    have_domain_range(X, DomSpec, Domain, IntRange, Module) :-
    	( var(X) ; atomic(X) ), !,
	has_domain_range(X, DomSpec, Domain, IntRange, Module).
    have_domain_range(X, DomSpec, Domain, IntRange, Module) :-
    	X = [_|_], !,
	list_have_domain_range(X, DomSpec, Domain, IntRange, Module).
    have_domain_range(X, DomSpec, _Domain, _IntRange, _Module) :-
	printf(error, "Illegal left hand side in %w%n", [(X &:: DomSpec)]),
	abort.

    has_domain_range(X, DomSpec, Domain, IntRange, Module) :-
	( symbol_domain_index_(X, OldDom, Old_ic, Module) ->
	    % X is a symbolic variable or domain constant:
	    % check whether same domain and in range
	    OldDom = Domain,
	    ic:(Old_ic :: IntRange)
	; var(X) ->
	    var_has_domain_range(X, Domain, _, IntRange)
	;
	    % X is non-domain constant
	    printf(error, "Variable or domain value expected in %w%n",
		[X &:: DomSpec]),
	    abort
	).


:- export indomain/1.
indomain(_{ic_symbolic with [ic_var:X_ic]}) ?- !,
	ic:indomain(X_ic).
indomain(X) :-
	var(X), !,
	error(4, indomain(X)).
indomain(X) :-
	% could check here for valid domain value, but that would
	% be expensive and require indomain/1 to be a tool.
	atomic(X).


%----------------------------------------------------------------------
% Auxiliaries
%----------------------------------------------------------------------


% X is domainless: create and add a new symbolic attribute
var_has_domain_range(X, Domain, X_ic, IntRange) :-
	ic:(X_ic :: IntRange),
	( nonvar(X_ic) ->
	    % Imposing the domain actually made the variable ground.
	    Domain = Module:Name,
	    current_domain(Name, _DefModule, DomainArray)@Module,
	    arg(X_ic, DomainArray, X)
	;
	    add_new_sic_attribute(X, X_ic, Domain)
	).


% add a new attribute

add_new_sic_attribute(X{Attr}, X_ic, Domain) ?-
	var(Attr),
	add_new_sic_attribute_unchecked(X, X_ic, Domain).
add_new_sic_attribute(X, X_ic, Domain) :-
	free(X),
	add_new_sic_attribute_unchecked(X, X_ic, Domain).
	
add_new_sic_attribute_unchecked(X, X_ic, Domain) :-
	new_sic_attribute(X, X_ic, Domain, Attr),
	add_attribute(X, Attr),
	suspend(sync_ic_symbolic(X,Attr), 2, [X_ic->inst]),
	notify_constrained(X),
	wake.

new_sic_attribute(_X, X_ic, Domain, ic_symbolic with [
		ic_var:X_ic,
		dom:Domain
	]).


% demon to forward updates to the ic variable to the symbolic one

/*
:- demon(sync_ic_symbolic/3).
sync_ic_symbolic(_X, ic_symbolic with [ic_var:X_ic, dom:(Module:Name)], Susp) :-
	( nonvar(X_ic) ->
	    kill_suspension(Susp),
	    current_domain(Name, _DefModule, DomainArray)@Module,
	    arg(X_ic, DomainArray, X)
	;
	    % X_ic has been bound to Y_ic
	    % need to unify X and Y, but we don't have Y...
	).
*/
sync_ic_symbolic(X, ic_symbolic with [ic_var:X_ic, dom:(Module:Name)]) :-
	% This synchronisation would be more efficient if we would keep
	% the DomainArray directly in the attribute from the start
	current_domain(Name, _DefModule, DomainArray)@Module,
	arg(X_ic, DomainArray, X).


% Auxiliary predicates for the mapping of symbolic to numeric constraints, i.e.
%	- check for consistent domain
%	- collect domainless variables so they can be given a domain
%	- get the integers/intvars that correspond to the given symbols/symvars
%
% Arguments:
%	Syms	[List of] variables (with or without domain) or constants
%	Dom	resulting/required domain (DefModule:DomName)
%	Int	corresponding IC variable or integer
%	DomLess	[List of] domainless variables (SymVar-IcVar pairs)
%	Clash	[List of] variables/constants with mismatched domain
%
% symbols_to_ints(?Syms,?Dom,-Ints,-DomLessOut,+DomLessIn,-ClashOut,+ClashIn,+Module).
%	If Dom is instantiated, checks that all constants and domain variables
%	in Syms have this domain. Variables or constants with mismatched domain
%	are returned in the difference list Clash. Domainless variables are
%	returned in the difference list DomLess, paired with a free variable
%	for the corresponding integer. A list of integers/intvars corresponding
%	to Syms is returned in Ints. If Dom is uninstantiated at call time,
%	the first domainfull element in Syms determines Dom's value, and the
%	remaining elements get checked for consistency with that.
%
% symbol_to_int(?Sym,?Dom,-Int,-DomLessOut,+DomLessIn,-ClashOut,+ClashIn,+Module).
%	same for a single variable.

symbol_to_int(X{ic_symbolic with [ic_var:X_ic0,dom:DomainX]}, Domain, X_ic, DomLess, DomLess0, Clash, Clash0, _) ?- !,
	( DomainX = Domain ->
	    % first or same domain as first
	    X_ic = X_ic0,
	    DomLess = DomLess0,
	    Clash = Clash0
	;
	    DomLess = DomLess0,
	    Clash = [X|Clash0]
	).
symbol_to_int(X, Domain, Index, DomLess, DomLess0, Clash, Clash0, Module) :-
	atomic(X), !,
	( domain_index(X, Domain, Index)@Module ->
	    % first or same domain as first
	    DomLess = DomLess0,
	    Clash = Clash0
	;
	    DomLess = DomLess0,
	    Clash = [X|Clash0]
	).
symbol_to_int(X, _Domain, X_ic, [X-X_ic|DomLess0], DomLess0, Clash, Clash, _Module).


symbols_to_ints([], _Domain, X_ics, DomLess, DomLess0, Clash, Clash0, _Module) ?-
	X_ics = [], DomLess = DomLess0, Clash = Clash0.
symbols_to_ints([X|Xs], Domain, X_ics, DomLess, DomLess0, Clash, Clash0, Module) ?-
	X_ics = [X_ic|X_ics0],
	symbol_to_int(X, Domain, X_ic, DomLess, DomLess1, Clash, Clash1, Module),
	symbols_to_ints(Xs, Domain, X_ics0, DomLess1, DomLess0, Clash1, Clash0, Module).


% check_domain(XDomain, NoDom, Clash, Goal, Module)
% to be called after symbol_to_int/symbols_to_ints to make error
% message or to give domains to domainless variables.
% The main reason this is inlined is to avoid constructing ErrorGoal
% even when there is no error.

tr_check_domain(check_domain(XDomain, NoDom, Clash, ErrorGoal, Module),
	( var(XDomain) ->
	    printf(error, "Arguments have no domains in %w in module %w%n",
	    	[ErrorGoal,Module]),
	    abort
	; Clash = [Y|_] ->
	    ( symbol_domain_index_(Y, YDomain, _, Module) ->
		true
	    ;
	    	YDomain = Y
	    ),
	    printf(error, "Arguments have different domains (%w, %w) in %w in module %w%n",
	    	[XDomain,YDomain,ErrorGoal,Module]),
	    abort
	;
	    XDomain = DefModule:XDomName,
	    domain_or_subdomain(XDomName, _Domain, IntRange, DefModule),
	    ( foreach(X-X_ic, NoDom), param(XDomain,IntRange) do
		var_has_domain_range(X, XDomain, X_ic, IntRange)
	    )
	)
    ).

:- inline(check_domain/5, tr_check_domain/2).
check_domain(XDomain, NoDom, Clash, ErrorGoal, Module) :-
	check_domain(XDomain, NoDom, Clash, ErrorGoal, Module).



% get domain name and numeric value from variable or constant
% fail if no domain information is available

:- export symbol_domain_index/3.
:- tool(symbol_domain_index/3, symbol_domain_index_/4).
symbol_domain_index_(_{ic_symbolic with [ic_var:X_ic0,dom:Domain0]}, Domain, X_ic, _Module) ?- !,
	Domain = Domain0, X_ic = X_ic0.
symbol_domain_index_(X, Domain, X_ic, Module) :-
	atomic(X),
	domain_index(X, Domain, X_ic)@Module.


% An user-level wrapper for symbols_to_ints/check_domain

:- export symbols_domain_indices/3.
:- tool(symbols_domain_indices/3, symbols_domain_indices_/4).
symbols_domain_indices_(Xs, Domain, Is, Module) :- var(Xs), !,
	error(4, symbols_domain_indices(Xs, Domain, Is), Module).
symbols_domain_indices_([], _Domain, [], _Module).
symbols_domain_indices_(Xs, Domain, Is, Module) :- Xs = [_|_], !,
	symbols_to_ints(Xs, Domain, Is, NoDom, [], Clash, [], Module),
	check_domain(Domain, NoDom, Clash, symbols_domain_indices(Xs, Domain, Is), Module).
symbols_domain_indices_(Xs, Domain, Is, Module) :-
	error(5, symbols_domain_indices(Xs, Domain, Is), Module).


:- export is_solver_var/1.
is_solver_var(_{ic_symbolic{}}) ?- true.

:- export is_exact_solver_var/1.
is_exact_solver_var(_{ic_symbolic{}}) ?- true.


%----------------------------------------------------------------------
% Handlers
%----------------------------------------------------------------------

% copy_term handler

copy_term_ic_symbolic(X{ic_symbolic with [dom:D,ic_var:X_ic]}, Copy) ?- !,
	ic_kernel:copy_ic_term(X_ic, Copy_ic),
	add_new_sic_attribute(Copy, Copy_ic, D).
copy_term_ic_symbolic(_, _).


% unify handler

unify_ic_symbolic(Term, AttrX, SuspAttrX) :-
	var(AttrX),
	( nonvar(SuspAttrX), symbol_domain_index(Term, _, _) ->
	    schedule_suspensions(constrained of suspend, SuspAttrX)
	;
	    true
	).
unify_ic_symbolic(Term, AttrX, _) :-
	compound(AttrX),
	unify_term_sic(Term, AttrX).

unify_term_sic(Y{AttrY}, AttrX) ?-
	unify_sic_sic(Y, AttrX, AttrY).
unify_term_sic(Y, ic_symbolic with [ic_var:X_ic, dom:Domain]) :-
	nonvar(Y),
	% Note: for lack of a better module, we use the definition module of
	% the domain to look up the constant's domain index. This could cause
	% confusion in pathological cases where the variable is declared in
	% a module different from the one where the constant originally occurred,
	% and both modules have conflicting domain declarations visible.
	Domain = Module:_,
	domain_index(Y, Domain, X_ic)@Module.	% same domain and Y_ic = X_ic

unify_sic_sic(Y, AttrX, AttrY) :-
	var(AttrY),
	AttrY = AttrX,			% transfer the attribute
	notify_constrained(Y).
unify_sic_sic(_, ic_symbolic with [dom:D,ic_var:X_ic], ic_symbolic with [dom:D,ic_var:Y_ic]) ?-
	% head matching fails if domains differ
	Y_ic = X_ic.


% test_unify handler

test_unify_ic_symbolic(_Term, AttrX) :-
	var(AttrX).
test_unify_ic_symbolic(Term, AttrX) :-
	compound(AttrX),
	test_unify_term_sic(Term, AttrX).

test_unify_term_sic(_Y{AttrY}, AttrX) ?-
	test_unify_sic_sic(AttrY, AttrX).
test_unify_term_sic(Y, ic_symbolic with [ic_var:X_ic, dom:Domain]) :-
	nonvar(Y),
	Domain = Module:_,
	domain_index(Y, Domain, Y_ic)@Module,
	\+ not_unify(Y_ic, X_ic).

test_unify_sic_sic(AttrY, _AttrX) :-
	var(AttrY).
test_unify_sic_sic(ic_symbolic with [dom:D,ic_var:Y_ic], ic_symbolic with [dom:D,ic_var:X_ic]) ?-
	% head matching fails if domains differ
	\+ not_unify(Y_ic, X_ic).


% get domain (also print handler)

get_domain_as_list(Value, [Value]) :-
	atom(Value).
	% We could check for valid domain value instead (but would need Module):
	% domain_index(Value, _Domain, _Index)@Module.
get_domain_as_list(_{ic_symbolic with [ic_var:X_ic,dom:(Module:Name)]}, Values) ?-
	ic_kernel:get_domain_as_list(X_ic, IcDomain),
	current_domain(Name, _DefModule, DomainArray)@Module,
	(
	    foreach(Index,IcDomain),
	    foreach(Value,Values),
	    param(DomainArray)
	do
	    arg(Index, DomainArray, Value)
	).


% compare_instances
% naming convention for the auxiliary predicates:
%	_const	nonvar
%	_free	variable without any attributes
%	_meta	variable with (possibly empty) attributes
%	_any	free;meta;const
%	_attr	(possibly uninstantiated) attribute

compare_instances_ic_symbolic(Res, _X{AttrX}, Y) :- -?->
	compare_instances_attr_any(Res, AttrX, Y).
compare_instances_ic_symbolic(Res, X, _Y{AttrY}) :- -?-> free(X),
	compare_instances_free_attr(Res, AttrY).	% Y must be meta!
compare_instances_ic_symbolic(Res, X, _Y{AttrY}) :- -?-> nonvar(X),
	compare_instances_const_attr(Res, X, AttrY).	% Y must be meta!

compare_instances_attr_any(Res, AttrX, Y{AttrY}) :- -?->
	compare_instances_attr_attr(Res, AttrX, AttrY).
compare_instances_attr_any(Res, AttrX, Y) :- free(Y),
	compare_instances_attr_free(Res, AttrX).
compare_instances_attr_any(Res, AttrX, Y) :- nonvar(Y),
	compare_instances_attr_const(Res, AttrX, Y).

compare_instances_attr_free(Res, AttrX) :- var(AttrX),
	Res = (=).
compare_instances_attr_free(Res, AttrX) :- nonvar(AttrX),
	Res = (<).

compare_instances_free_attr(Res, AttrY) :- var(AttrY),
	Res = (=).
compare_instances_free_attr(Res, AttrY) :- nonvar(AttrY),
	Res = (>).

compare_instances_attr_attr(Res, AttrX, AttrY) :- var(AttrX),
	compare_instances_free_attr(Res, AttrY).
compare_instances_attr_attr(Res, AttrX, AttrY) :- nonvar(AttrX),
	compare_instances_iattr_attr(Res, AttrX, AttrY).

compare_instances_iattr_attr(Res, _AttrX, AttrY) :- var(AttrY), !,
	Res = (<).
compare_instances_iattr_attr(Res,
		ic_symbolic with [dom:Dom,ic_var:X_ic],
		ic_symbolic with [dom:Dom,ic_var:Y_ic]) ?-
	% head matching fails if different domains
	ic_kernel:compare_ic_instances(Res, X_ic, Y_ic).

compare_instances_const_attr(Res, _X, AttrY) :- var(AttrY), !,
	Res = (<).
compare_instances_const_attr(Res, X, ic_symbolic with [dom:Domain, ic_var:Y_ic]) ?-
	Domain = Module:_,
	domain_index(X, Domain, X_ic)@Module, % may fail (X has no/other domain)
	ic_kernel:compare_ic_instances(Res, X_ic, Y_ic).

compare_instances_attr_const(Res, AttrX, _Y) :- var(AttrX), !,
	Res = (>).
compare_instances_attr_const(Res, ic_symbolic with [dom:Domain, ic_var:X_ic], Y) ?-
	Domain = Module:_,
	domain_index(Y, Domain, Y_ic)@Module, % may fail (Y has no/other domain)
	ic_kernel:compare_ic_instances(Res, X_ic, Y_ic).


% most specific generalisation

:- export(msg/3).
:- tool(msg/3, msg_/4).
msg_(X, _Y, _G, _Module) :- free(X), !.
msg_(X{XAttr}, Y, G, Module) ?- !,
	msg_attr_any(XAttr, Y, G, Module).
msg_(X, Y, G, Module) :- atomic(X), !,
	msg_const_any(X, Y, G, Module).
msg_(_X, _Y, _G, _Module).		% X has no domain (not atomic)

msg_attr_any(_XAttr, Y, _G, _Module) :- free(Y), !.
msg_attr_any(XAttr, Y{YAttr}, G, _Module) ?- !,
	msg_attr_attr(XAttr, YAttr, G).
msg_attr_any(XAttr, Y, G, Module) :- atomic(Y), !,
	msg_const_attr(Y, XAttr, G, Module).
msg_attr_any(_XAttr, _Y, _G, _Module).	% Y has no domain (not atomic)

msg_const_any(_X, Y, _G, _Module) :- free(Y), !.
msg_const_any(X, Y{YAttr}, G, Module) ?- !,
	msg_const_attr(X, YAttr, G, Module).
msg_const_any(X, Y, G, Module) :- atomic(Y), !,
	( X = Y ->
	    G = X
	;
	    domain_index(X, Domain, X_ic)@Module, % may fail (X has no domain)
	    domain_index(Y, Domain, Y_ic)@Module  % may fail (Y has no/other domain)
	->
	    ic_kernel:msg(X_ic, Y_ic, G_ic),
	    add_new_sic_attribute(G, G_ic, Domain)
	;
	    true			% no or different domains
	).
msg_const_any(_X, _Y, _G, _Module).	% Y has no domain (not atomic)

msg_const_attr(_X, YAttr, _G, _Module) :- var(YAttr), !.
msg_const_attr(X, ic_symbolic with [dom:Domain,ic_var:Y_ic], G, Module) ?-
	domain_index(X, Domain, X_ic)@Module, % may fail (X has no/other domain)
	!,
	ic_kernel:msg(X_ic, Y_ic, G_ic),
	add_new_sic_attribute(G, G_ic, Domain).
msg_const_attr(_X, _YAttr, _G, _Module).	% no or different domains

msg_attr_attr(ic_symbolic with [dom:Domain,ic_var:X_ic],
		ic_symbolic with [dom:Domain,ic_var:Y_ic], G) ?- !,
	ic_kernel:msg(X_ic, Y_ic, G_ic),
	add_new_sic_attribute(G, G_ic, Domain).
msg_attr_attr(_XAttr, _YAttr, _G).		% no or different domains


%----------------------------------------------------------------------
% Constraints
%----------------------------------------------------------------------

:- export (&=)/2.
:- export op(700, xfx, (&=)).
:- tool((&=)/2, same_/3).
same_(X, Y, Module) :-
	symbol_to_int(X, Domain, _X_ic, _NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, _Y_ic, NoDom1, [], Clash1, [], Module),
	% Following the pattern for the other constraints, this should be:
	%   check_domain(Domain, NoDom, Clash, X &= Y, Module),
	%   ic:(X_ic =:= Y_ic).
	% That would be inefficient for two reasons:
	% - in case one variable doesn't have a domain, it would be given one
	%   by check_domain/5, only to be unified with the other afterwards.
	%   Instead we just bind the domainless variable to the domain one.
	% - calling ic: X_ic=:=Y_ic would not lead to unifying X and Y
	%   because of the shortcoming of the synchronisation mechanism
	( var(Domain) ->
	    printf(error, "Arguments have no domains in %w in module %w%n",
	    	[X &= Y,Module]),
	    abort
	; Clash = [Z] ->
	    ( symbol_domain_index_(Z, ZDomain, _, Module) ->
		true
	    ;
	    	ZDomain = Z
	    ),
	    printf(error, "Arguments have different domains (%w, %w) in %w in module %w%n",
	    	[Domain,ZDomain, X &= Y,Module]),
	    abort
	;
	    X = Y
	).


:- export (&=)/3.
:- tool((&=)/3, same_/4).
same_(X, Y, Bool, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, &=(X,Y,Bool), Module),
	% should be ac_eq(X_ic, Y_ic, 0, Bool).
	ic: =:=(X_ic, Y_ic, Bool).


:- export (&\=)/2.
:- export op(700, xfx, (&\=)).
:- tool((&\=)/2, differ_/3).
differ_(X, Y, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, X &\= Y, Module),
	ic:(X_ic =\= Y_ic).


:- export (&\=)/3.
:- tool((&\=)/3, differ_/4).
differ_(X, Y, Bool, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, &\=(X,Y,Bool), Module),
	ic: =\=(X_ic, Y_ic, Bool).


:- export (&<)/2.
:- export op(700, xfx, (&<)).
:- tool((&<)/2, lt/3).
lt(X, Y, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, X &< Y, Module),
	ic:(X_ic < Y_ic).


:- export (&<)/3.
:- tool((&<)/3, lt/4).
lt(X, Y, Bool, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, &<(X,Y,Bool), Module),
	ic: <(X_ic, Y_ic, Bool).


:- export (&=<)/2.
:- export op(700, xfx, (&=<)).
:- tool((&=<)/2, le/3).
le(X, Y, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, X &=< Y, Module),
	ic:(X_ic =< Y_ic).


:- export (&=<)/3.
:- tool((&=<)/3, le/4).
le(X, Y, Bool, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, &=<(X,Y,Bool), Module),
	ic: =<(X_ic, Y_ic, Bool).


:- export (&>)/2.
:- export op(700, xfx, (&>)).
:- tool((&>)/2, gt/3).
gt(X, Y, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, X &> Y, Module),
	ic:(X_ic > Y_ic).


:- export (&>)/3.
:- tool((&>)/3, gt/4).
gt(X, Y, Bool, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, &>(X,Y,Bool), Module),
	ic: >(X_ic, Y_ic, Bool).


:- export (&>=)/2.
:- export op(700, xfx, (&>=)).
:- tool((&>=)/2, ge/3).
ge(X, Y, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, X &>= Y, Module),
	ic:(X_ic >= Y_ic).


:- export (&>=)/3.
:- tool((&>=)/3, ge/4).
ge(X, Y, Bool, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, &>=(X,Y,Bool), Module),
	ic: >=(X_ic, Y_ic, Bool).


:- export shift/3.
:- tool(shift/3, shift_/4).
shift_(X, C, Y, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, shift(X,C,Y), Module),
	( integer(C) ->
	    ic:ac_eq(Y_ic, X_ic, C)	% domain consistent
	;
	    ic:integers(C),
	    ic:(X_ic + C =:= Y_ic)	% bound consistent
	).


:- export shift/4.
:- tool(shift/4, shift_/5).
shift_(X, C, Y, Bool, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, shift(X,C,Y,Bool), Module),
	% should be made domain consistent for nonvar(C)
	ic:integers(C),
	ic: =:=(X_ic + C, Y_ic, Bool).


% There are two potentially useful meanings for rotate:
%	ic:integers([C,K]),
%	ic:(Y_ic =:= C + X_ic - K*Size).
% which allows arbitrary large positive or negative offsets C, which
% are taken modulo the domain size (by adding/subtracting K*Size). 
% Or the a priori more restricted form:
%	ic:(C::0..Size-1),
%	ic:(K::0..1),
%	ic:(Y_ic =:= C + X_ic - K*Size).
% but the latter can always be written using the former as
%	rotate1(X,C,Y) :- rotate(X,C,Y), C::0..DomSize-1.

% A domain consistent version of this would probably be more useful
:- export rotate/3.
:- tool(rotate/3, rotate_/4).
rotate_(X, C, Y, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, rotate(X,C,Y), Module),
	Domain = DefModule:Name,
	current_domain(Name, _DefModule, ValueArray)@DefModule,
	functor(ValueArray, _, Size),
	ic:integers([C,K]),
	ic:(Y_ic =:= C + X_ic - K*Size).


:- export rotate/4.
:- tool(rotate/4, rotate_/5).
rotate_(X, C, Y, Bool, Module) :-
	symbol_to_int(X, Domain, X_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Y, Domain, Y_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, rotate(X,C,Y,Bool), Module),
	Domain = DefModule:Name,
	current_domain(Name, _DefModule, ValueArray)@DefModule,
	functor(ValueArray, _, Size),
	ic:integers([C,K]),
	ic: =:=(Y_ic, C + X_ic - K*Size, Bool).


:- export alldifferent/1.
:- tool(alldifferent/1, alldifferent_/2).
alldifferent_(Xs, Module) :-
	symbols_to_ints(Xs, Domain, X_ics, NoDom, [], Clash, [], Module),
	check_domain(Domain, NoDom, Clash, alldifferent(Xs), Module),
	ic_global:alldifferent(X_ics).


:- export alldifferent/2.
:- tool(alldifferent/2, alldifferent_/3).
alldifferent_(Xs, Cap, Module) :-
	symbols_to_ints(Xs, Domain, X_ics, NoDom, [], Clash, [], Module),
	check_domain(Domain, NoDom, Clash, alldifferent(Xs, Cap), Module),
	ic_global:alldifferent(X_ics, Cap).


:- export occurrences/3.
:- tool(occurrences/3, occurrences_/4).
occurrences_(V, Xs, Count, Module) :-
	symbol_to_int(V, Domain, V_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbols_to_ints(Xs, Domain, X_ics, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, occurrences(V, Xs, Count), Module),
	ic_global:occurrences(V_ic, X_ics, Count).


:- export atmost/3.
:- tool(atmost/3, atmost_/4).
atmost_(Count, Xs, V, Module) :-
	symbol_to_int(V, Domain, V_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbols_to_ints(Xs, Domain, X_ics, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, atmost(Count, Xs, V), Module),
	ic_global:atmost(Count, X_ics, V_ic).


:- export element/3.
:- tool(element/3, element_/4).
element_(Index, Xs, Value, Module) :-
	symbols_to_ints(Xs, Domain, Xs_ic, NoDom, NoDom1, Clash, Clash1, Module),
	symbol_to_int(Value, Domain, Value_ic, NoDom1, [], Clash1, [], Module),
	check_domain(Domain, NoDom, Clash, element(Index, Xs, Value), Module),
	ic:element(Index, Xs_ic, Value_ic).


%----------------------------------------------------------------------
% Documentation
%----------------------------------------------------------------------

:- comment(desc, html("
    <H4>Overview</H4>
    <P>
    This library is an add-on to library(ic) and implements variables
    over ordered symbolic domains, and constraints over such variables.
    This is in contrast to the basic library(ic), which implements only
    variables over numeric domains.
    <H4>Domains</H4>
    The library uses the domain feature provided by the ECLiPSe kernel.
    I.e. domains need to be declared. The declaration specifies the domain
    values and their order. For example:
    <PRE>
    	?- local domain(weekday(mo,tu,we,th,fr,sa,su)).
    </PRE>
    declares a domain with name 'weekday' and values 'mo', 'tu' etc.
    The domain values are implicitly ordered, with 'mo' corresponding to 1,
    until 'su' corresponding to 7.
    Domain values must be unique within one ECLiPSe module, i.e. a symbolic
    value can belong to at most one domain.
    <H4>Variables</H4>
    A variable of a declared domain can then be created using
    <PRE>
	?- X &:: weekday.
	X = X{[mo, tu, we, th, fr, sa, su]}
	Yes (0.00s cpu)
    </PRE>
    or multiple variables using &:: /2.
    <H4>Basic Constraints</H4>
    The following constraints implement the basic relationships between
    two domain values. The constraints require their arguments to come from
    identical domains, otherwise an error is raised.
    <DL>
    <DT>X &= Y</DT><DD>X is the same as Y</DD>
    <DT>X &\\= Y</DT><DD>X is different from Y</DD>
    <DT>X &< Y</DT><DD>X is strictly before Y in the domain order</DD>
    <DT>X &> Y</DT><DD>X is strictly after Y in the domain order</DD>
    <DT>X &=< Y</DT><DD>X is the same as Y, or before Y in the domain order</DD>
    <DT>X &>= Y</DT><DD>X is the same as Y, or after Y in the domain order</DD>
    <DT>shift(X,C,Y)</DT><DD>Y is C places after X in the domain order</DD>
    <DT>rotate(X,C,Y)</DT><DD>like shift/3 but wraps at domain boundary</DD>
    <DT>element(Index,List,Value)</DT><DD>Value occurs List at position Index</DD>
    </DL>
    For example
    <PRE>
	?- [X, Y] &:: weekday, X &< Y.
	X = X{[mo, tu, we, th, fr, sa]}
	Y = Y{[tu, we, th, fr, sa, su]}
	Yes (0.00s cpu)

	?- X &:: weekday, X &=< we.
	X = X{[mo, tu, we]}
	Yes (0.00s cpu)
    </PRE>
    <H4>Global Constraints</H4>
    A number of global constraints are available which directly correspond
    (and are in fact implemented via) their counterparts in lib(ic_global):
    <DL>
    <DT>alldifferent(List)</DT><DD>All list elements are different</DD>
    <DT>occurrences(Value,List,N)</DT><DD>Value occurs N times in List</DD>
    <DT>atmost(N,List,Value)</DT><DD>Value occurs at most N times in List</DD>
    </DL>
    </P>
    <H4>Internals</H4>
    <P>
    Internally, symbolic domains are mapped to integer ranges from 1 up to
    the number of domain elements. The first value in the domain declaration
    corresponds to 1, the second to 2 and so on. Similarly, symbolic domain
    variables can be mapped to a corresponding IC integer variable.
    This mapping is accessible through the predicate symbol_domain_index/3:
    <PRE>
    ?- symbol_domain_index(fr, D, I).
    D = weekday
    I = 5
    Yes (0.00s cpu)

    ?- X &:: weekday, symbol_domain_index(X, D, I).
    X = X{[mo, tu, we, th, fr, sa, su]}
    D = weekday
    I = I{1 .. 7}
    Yes (0.00s cpu)

    ?- X &:: weekday, X &\\= we, symbol_domain_index(X, D, I).
    X = X{[mo, tu, th, fr, sa, su]}
    D = weekday
    I = I{[1, 2, 4 .. 7]}
    Yes (0.00s cpu)
    </PRE>
    The integer variable I mirrors the domain of the symbolic variable
    X and vice versa.
    </P>
    <H4>Extending and Interfacing this Library</H4>
    <P>
    Because of the mapping of symbols to integers, new constraints over
    symbolic variables can be implemented simply by placing numeric (IC)
    constraints on the corresponding integer variables.
    </P><P>
    Similarly, the facilities of the ic_search library can be exploited
    when working with symbolic variables. Instead of labeling the symbolic
    variables, one can use the various facilities of ic_search to label
    the corresponding integer variables instead.
    </P>
    <H4>Known Problems</H4>
    <P>
    For efficiency reasons, the 'constrained' suspension list of the symbolic
    variable does not automatically get woken every time the domain changes
    (although it does get woken when the domain is initially attached, and
    when the variable gets instantiated). There are two solutions: (1) instead
    of suspending goals on the constrained-list of the symbolic variable,
    suspend them on the constrained-list of the corresponding integer variable.
    (2) Use a forwarding demon that suspends on the constrained-list of the
    integer variable and wakes the constrained-list of the symbolic variable:
    <PRE>
	symbol_domain_index(X, Domain, X_ic),
    	suspend(notify_constrained(X), 2, X_ic->constrained)
    </PRE>
    </P>
    ")).


:- comment((&::)/2, [
    summary:"All elements of Vars have a value in the domain Domain",
    args:["Vars":"variable or atomic value, list of them, or submatrix of them",
    	"Domain":"domain name (atom) or list of domain elements"],
    template:"?Vars &:: +Domain",
    see_also:[(domain)/1, current_domain/3, domain_index/3],
    eg:"
    ?- X &:: weekday.
    X = X{[mo, tu, we, th, fr, sa, su]}
    Yes (0.00s cpu)

    ?- [X,Y,we] &:: weekday.
    X = X{[mo, tu, we, th, fr, sa, su]}
    Y = Y{[mo, tu, we, th, fr, sa, su]}
    Yes (0.00s cpu)

    ?- dim(M,[3]), M[1..3] &:: weekday.
    M = [](_306{[mo, tu, we, th, fr, sa, su]}, ...)
    Yes (0.00s cpu)

    ?- [X,Y] &:: [we,fr,su].
    X = X{[we, fr, su]}
    Y = Y{[we, fr, su]}

    ?- X &:: [].
    No (0.00s cpu)
    ",
    desc:html("<P>
	Constrains a variable, or a list or submatrix of variables, to be in
	the domain Domain.
	Domain must be the name of a previously declared domain (see domain/1),
	or a sub-domain, expressed as a list of values from a single domain.
	A domain variable can only be instantiated to values within its domain.
	</P><P>
	Note that, on the left hand side of &::/2, the atom [] is not
	interpreted as the empty list but as a potential domain element.
	</P>
")]).

:- comment((&<)/2, [
    summary:"X is before Y in the domain order",
    args:["X":"variable or domain value",
    	"Y":"variable or domain value"],
    template:"?X &< ?Y",
    see_also:[(&<)/3,(&>)/2,(&=<)/2,(&>=)/2,(&=)/2,(&\=)/2,shift/3,rotate/3,(domain)/1],
    eg:"
    ?- [X,Y] &:: weekday, X &< Y.
    X = X{[mo, tu, we, th, fr, sa]}
    Y = Y{[tu, we, th, fr, sa, su]}
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- mo &< th.
    Yes (0.00s cpu)

    ?- X &< th.
    X = X{[mo, tu, we]}
    Yes (0.00s cpu)

    ?- fr &< th.
    No (0.00s cpu)

    ?- X &< Y.
    Arguments have no domains in X &< Y in module eclipse
    Abort

    ?- X &:: weekday, X &< red.
    Arguments have different domains (weekday,colour) in X &< red ...
    Abort
    ",
    desc:html("
	Constrains X and Y such that X is before Y in the domain order.
	X and Y must be variables or values of the same domain. If one of
	them is domain-less, it will be given the same domain as the other.
")]).


:- comment((&=<)/2, [
    summary:"X is before or equal to Y in the domain order",
    args:["X":"variable or domain value",
    	"Y":"variable or domain value"],
    template:"?X &=< ?Y",
    see_also:[(&=<)/3,(&<)/2,(&>)/2,(&>=)/2,(&=)/2,(&\=)/2,shift/3,rotate/3,(domain)/1],
    eg:"
    ?- [X,Y] &:: weekday, X &=< Y.
    X = X{[mo, tu, we, th, fr, sa, su]}
    Y = Y{[mo, tu, we, th, fr, sa, su]}
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- mo &=< th.
    Yes (0.00s cpu)

    ?- X &=< th.
    X = X{[mo, tu, we, th]}
    Yes (0.00s cpu)

    ?- fr &=< th.
    No (0.00s cpu)

    ?- X &=< Y.
    Arguments have no domains in X &=< Y in module eclipse
    Abort

    ?- X &:: weekday, X &=< red.
    Arguments have different domains (weekday,colour) in X &=< red ...
    Abort
    ",
    desc:html("
	Constrains X and Y such that X is before Y or the same as Y in the
	domain order.
	X and Y must be variables or values of the same domain. If one of
	them is domain-less, it will be given the same domain as the other.
")]).


:- comment((&>)/2, [
    summary:"X is after Y in the domain order",
    args:["X":"variable or domain value",
    	"Y":"variable or domain value"],
    template:"?X &> ?Y",
    see_also:[(&>)/3,(&<)/2,(&=<)/2,(&>=)/2,(&=)/2,(&\=)/2,shift/3,rotate/3,(domain)/1],
    eg:"
    ?- [X,Y] &:: weekday, X &> Y.
    X = X{[tu, we, th, fr, sa, su]}
    Y = Y{[mo, tu, we, th, fr, sa]}
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- th &> mo.
    Yes (0.00s cpu)

    ?- X &> th.
    X = X{[fr, sa, su]}
    Yes (0.00s cpu)

    ?- th &> fr.
    No (0.00s cpu)

    ?- X &> Y.
    Arguments have no domains in X &> Y in module eclipse
    Abort

    ?- X &:: weekday, X &> red.
    Arguments have different domains (weekday,colour) in X &> red ...
    Abort
    ",
    desc:html("
	Constrains X and Y such that X is after Y in the domain order.
	X and Y must be variables or values of the same domain. If one of
	them is domain-less, it will be given the same domain as the other.
")]).


:- comment((&>=)/2, [
    summary:"X is after or equal to Y in the domain order",
    args:["X":"variable or domain value",
    	"Y":"variable or domain value"],
    template:"?X &>= ?Y",
    see_also:[(&>=)/3,(&<)/2,(&>)/2,(&=<)/2,(&=)/2,(&\=)/2,shift/3,rotate/3,(domain)/1],
    eg:"
    ?- [X,Y] &:: weekday, X &>= Y.
    X = X{[mo, tu, we, th, fr, sa, su]}
    Y = Y{[mo, tu, we, th, fr, sa, su]}
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- th &>= mo.
    Yes (0.00s cpu)

    ?- X &>= th.
    X = X{[th, fr, sa, su]}
    Yes (0.00s cpu)

    ?- th &>= fr.
    No (0.00s cpu)

    ?- X &>= Y.
    Arguments have no domains in X &>= Y in module eclipse
    Abort

    ?- X &:: weekday, X &>= red.
    Arguments have different domains (weekday,colour) in X &>= red ...
    Abort
    ",
    desc:html("
	Constrains X and Y such that X is after Y or the same as Y in the
	domain order.
	X and Y must be variables or values of the same domain. If one of
	them is domain-less, it will be given the same domain as the other.
")]).


:- comment(shift/3, [
    summary:"Y is C places after X in the domain order",
    args:["X":"variable or domain value",
    	"C":"variable or integer",
    	"Y":"variable or domain value"],
    amode:shift(?,?,?),
    see_also:[shift/4,(&<)/2,(&>)/2,(&=<)/2,(&>=)/2,(&=)/2,(&\=)/2,rotate/3,(domain)/1],
    eg:"
    ?- [X,Y] &:: weekday, shift(X, 1, Y).
    X = X{[mo, tu, we, th, fr, sa]}
    Y = Y{[tu, we, th, fr, sa, su]}
    There is 1 delayed goal.
    Yes (0.00s cpu)

    [eclipse 4]: [X,Y]&::weekday, shift(X,C,Y). 
    X = X{[mo, tu, we, th, fr, sa, su]}
    C = C{-6 .. 6}
    Y = Y{[mo, tu, we, th, fr, sa, su]}
    There are 3 delayed goals.
    Yes (0.00s cpu)

    ?- shift(we, 1, th).
    Yes (0.00s cpu)

    ?- shift(we, 2, fr).
    Yes (0.00s cpu)

    ?- shift(X, -1, th).
    X = fr
    Yes (0.00s cpu)

    ?- shift(tu, X, fr).
    X = 3
    Yes (0.00s cpu)

    ?- shift(tu,X,Y).
    X = X{-1 .. 5}
    Y = Y{[mo, tu, we, th, fr, sa, su]}
    Delayed goals: ...

    ?- shift(tu, 1, th).
    No (0.00s cpu)

    ?- shift(X, 1, Y).
    Arguments have no domains in shift(X, 1, Y) in module eclipse
    Abort

    ?- X &:: weekday, shift(X, 1, red).
    Arguments have different domains (weekday,colour) in shift(X, 1, red) ...
    Abort
    ",
    desc:html("
	Constrains X and Y such that Y is C positions after X in the domain
	order. C must be an integer or integer variable, and its range is
	-(S-1)..S-1 where S is the size of the symbolic domain.
	X and Y must be variables or values of the same domain. If one of
	them is domain-less, it will be given the same domain as the other.
	<P>
	The implementation achieves domain consistency iff C is instantiated
	at call time, otherwise only bounds consistency.
")]).


:- comment((&=)/2, [
    summary:"X is the same domain value as Y",
    args:["X":"variable or domain value",
    	"Y":"variable or domain value"],
    template:"?X &= ?Y",
    see_also:[(&=)/3,(&<)/2,(&>)/2,(&=<)/2,(&>=)/2,(&\=)/2,shift/3,rotate/3],
    eg:"
    ?- [X, Y] &:: weekday, X &= Y.
    X = X{[mo, tu, we, th, fr, sa, su]}
    Y = X{[mo, tu, we, th, fr, sa, su]}
    Yes (0.01s cpu)

    ?- th &= th.
    Yes (0.00s cpu)

    ?- X &= th.
    X = th
    Yes (0.00s cpu)

    ?- fr &= th.
    No (0.00s cpu)

    ?- X &= Y.
    Arguments have no domains in X &= Y in module eclipse
    Abort

    ?- X &:: weekday, X &= red.
    Arguments have different domains (weekday,colour) in X &= red ...
    Abort
    ",
    desc:html("
	Constrains X and Y to be the same.  This is almost the same as
	unifying X and Y, except that &=/2 raises errors when X and
	Y are domain-less (where unification succeeds) or when the
	domains of X and Y are incompatible (where unification fails).
")]).


:- comment((&\=)/2, [
    summary:"X is different from Y in the domain",
    args:["X":"variable or domain value",
    	"Y":"variable or domain value"],
    template:"?X &\\= ?Y",
    see_also:[(&\=)/3,(&<)/2,(&>)/2,(&=<)/2,(&>=)/2,(&=)/2,shift/3,rotate/3],
    eg:"
    ?- [X,Y] &:: weekday, X &\\= Y.
    X = X{[mo, tu, we, th, fr, sa, su]}
    Y = Y{[mo, tu, we, th, fr, sa, su]}
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- mo &\\= th.
    Yes (0.00s cpu)

    ?- X &\\= th.
    X = X{[mo, tu, we, fr, sa, su]}
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- th &\\= th.
    No (0.00s cpu)

    ?- X &\\= Y.
    Arguments have no domains in X &\\= Y in module eclipse
    Abort

    ?- X &:: weekday, X &\\= red.
    Arguments have different domains (weekday,colour) in X &\\= red ...
    Abort
    ",
    desc:html("
	Constrains X and Y to be different.
	X and Y must be variables or values of the same domain. If one of
	them is domain-less, it will be given the same domain as the other.
")]).


:- comment((rotate)/3, [
    summary:"Y is C places after X in the (cyclic) domain order",
    args:["X":"variable or domain value",
    	"C":"variable or integer",
    	"Y":"variable or domain value"],
    amode:rotate(?,?,?),
    see_also:[rotate/4,(&<)/2,(&>)/2,(&=<)/2,(&>=)/2,(&=)/2,(&\=)/2,shift/3,(domain)/1],
    eg:"
    ?- [X,Y] &:: weekday, rotate(X,1,Y).
    X = X{[mo, tu, we, th, fr, sa, su]}
    Y = Y{[mo, tu, we, th, fr, sa, su]}
    There is 1 delayed goal.
    Yes (0.00s cpu)

    ?- rotate(th,1,fr).
    Yes (0.00s cpu)

    ?- rotate(su,2,tu).
    Yes (0.00s cpu)

    ?- rotate(su,9,tu).
    Yes (0.00s cpu)

    ?- rotate(X,1,fr).
    X = th
    Yes (0.00s cpu)

    ?- rotate(su,50,X).
    X = mo
    Yes (0.00s cpu)

    ?- rotate(su,-1,X).
    X = sa
    Yes (0.00s cpu)

    ?- rotate(tu,1,th).
    No (0.00s cpu)

    ?- ic:(X::0..7), rotate(tu,X,fr).
    X = 3
    Yes (0.00s cpu)

    ?- ic:(X::2..4), rotate(tu,X,Y).
    X = X{2 .. 4}
    Y = Y{[th, fr, sa]}
    Delayed goals: ...

    ?- rotate(X,1,Y).
    Arguments have no domains in rotate(X, 1, Y) in module eclipse
    Abort

    ?- X &:: weekday, rotate(X, 1, red).
    Arguments have different domains (weekday,colour) in rotate(X, 1, red) ...
    Abort
    ",
    desc:html("
	Constrains X and Y such that Y is C positions after X in the domain
	order, where the order is considered cyclic, i.e. the first domain
	element follows the last one. Because of the cyclic order, for each
	pair of X and Y there are infinitely many solutions for C (which
	are identical modulo the domain size).
	X and Y must be variables or values of the same domain. If one of
	them is domain-less, it will be given the same domain as the other.
	<P>
	The implementation currently achieves only bounds-consistency.
")]).

:- comment((&=)/3, [
    summary:"Reified version of X &= Y",
    see_also:[(&=)/2],
    args:["X":"variable or domain value",
    	"Y":"variable or domain value",
	"Bool":"0, 1, or boolean variable"]
]).

:- comment((&\=)/3, [
    summary:"Reified version of X &\\= Y",
    see_also:[(&\=)/2],
    args:["X":"variable or domain value",
    	"Y":"variable or domain value",
	"Bool":"0, 1, or boolean variable"]
]).

:- comment((&<)/3, [
    summary:"Reified version of X &< Y",
    see_also:[(&<)/2],
    args:["X":"variable or domain value",
    	"Y":"variable or domain value",
	"Bool":"0, 1, or boolean variable"]
]).

:- comment((&=<)/3, [
    summary:"Reified version of X &=< Y",
    see_also:[(&=<)/2],
    args:["X":"variable or domain value",
    	"Y":"variable or domain value",
	"Bool":"0, 1, or boolean variable"]
]).

:- comment((&>)/3, [
    summary:"Reified version of X &> Y",
    see_also:[(&>)/2],
    args:["X":"variable or domain value",
    	"Y":"variable or domain value",
	"Bool":"0, 1, or boolean variable"]
]).

:- comment((&>=)/3, [
    summary:"Reified version of X &>= Y",
    see_also:[(&>=)/2],
    args:["X":"variable or domain value",
    	"Y":"variable or domain value",
	"Bool":"0, 1, or boolean variable"]
]).

:- comment(shift/4, [
    summary:"Reified version of shift(X,C,Y)",
    see_also:[shift/3],
    args:["X":"variable or domain value",
    	"C":"variable or integer",
    	"Y":"variable or domain value",
	"Bool":"0, 1, or boolean variable"]
]).

:- comment(rotate/4, [
    summary:"Reified version of rotate(X,C,Y)",
    see_also:[rotate/3],
    args:["X":"variable or domain value",
    	"C":"variable or integer",
    	"Y":"variable or domain value",
	"Bool":"0, 1, or boolean variable"]
]).


:- comment(alldifferent/1, [
    summary:"All elements of List are different",
    args:["List":"list of variables or domain values"],
    see_also:[ic_global:alldifferent/1],
    desc:html("
	Constrains all list elements to be different values of the same domain.
	This is implemented by mapping onto ic_global:alldifferent/1.
	All list elements must be variables or values of the same domain.
	If one or more of them are domain-less, they will be given the same
	domain as the others.
")]).

:- comment(alldifferent/2, [
    summary:"No domain value occurs more than Cap times in List",
    args:["List":"list of variables or domain values",
    	"Cap":"integer or integer variable"],
    see_also:[ic_global:alldifferent/2],
    desc:html("
	Constrains the list so that no more than Cap elements can have the
	same value.
	This is implemented by mapping onto ic_global:alldifferent/2.
	All list elements must be variables or values of the same domain.
	If one or more of them are domain-less, they will be given the same
	domain as the others.
")]).

:- comment(occurrences/3, [
    summary:"Value occurs N times in List",
    amode:occurrences(+,+,?),
    args:["Value":"domain value",
	"List":"list of variables or domain values",
	"N":"integer variable or integer"],
    see_also:[ic_global:occurrences/3],
    desc:html("
	Constrains its arguments such that Value occurs exactly N times
	in List.
	This is implemented by mapping onto ic_global:occurrences/3.
	All list elements and Value must be variables or values of the
	same domain.  If one or more of them are domain-less, they will
	be given the same domain as the others.
")]).

:- comment(atmost/3, [
    summary:"Value occurs N times in List",
    amode:atmost(++,+,++),
    args:[
	"N":"integer",
	"List":"list of variables or domain values",
	"Value":"domain value"
	],
    see_also:[ic_global:atmost/3],
    desc:html("
	Constrains its arguments such that Value occurs at most N times
	in List.
	This is implemented by mapping onto ic_global:atmost/3.
	All list elements and Value must be variables or values of the
	same domain.  If one or more of them are domain-less, they will
	be given the same domain as the others.
")]).

:- comment(element/3, [
    summary:"Value is the Index'th element of List",
    amode:element(?,++,?),
    args:["Index":"integer variable or integer",
	"List":"list of domain values",
	"Value":"domain variable or value"],
    see_also:[ic:element/3],
    desc:html("
	Constrains its arguments such that Value is the Index'th member
	of List.  This is implemented by mapping onto ic:element/3.
	All list elements and Value must be variables or values of the
	same domain.  If one or more of them are domain-less, they will
	be given the same domain as the others.
")]).


:- comment(symbol_domain_index/3, [
    summary:"Map a symbolic domain variable/value to integer variable/value",
    amode:symbol_domain_index(?,-,-),
    fail_if:"X is neither a symbolic domain variable nor a domain constant",
    args:["X":"domain variable or value",
	"Domain":"Variable, will be bound to a pair Module:DomainName",
	"Index":"Variable, will be bound to integer or integer variable"],
    see_also:[symbols_domain_indices/3, (&::)/2, (domain)/1, domain_index/3],
    eg:"
    ?- symbol_domain_index(we, D, I).
    D = eclipse:weekday
    I = 3
    Yes (0.00s cpu)

    ?- X &:: weekday, X &\\= we, symbol_domain_index(X, D, I).
    X = X{[mo, tu, th, fr, sa, su]}
    D = eclipse:weekday
    I = I{[1, 2, 4 .. 7]}
    Yes (0.00s cpu)
    ",
    desc:html("
	Low-level primitive:
	For a domain variable or domain value, return the corresponding
	domain name and an integer or integer variable reflecting the
	corresponding integer index within the domain order.
")]).


:- comment(symbols_domain_indices/3, [
    summary:"Map symbolic domain variables/values to integer variables/values",
    amode:symbols_domain_indices(+,?,-),
    fail_if:"X is neither a symbolic domain variable nor a domain constant",
    args:["Xs":"list of domain variables or values",
	"Domain":"Variable or domain name",
	"Is":"Variable, will be bound to list of integers or integer variables"],
    see_also:[symbol_domain_index/3, (&::)/2, (domain)/1],
    eg:"
    ?- symbols_domain_indices([we], D, Is).
    D = eclipse:weekday
    Is = [3]
    Yes (0.00s cpu)
    
    ?- X &:: weekday, symbols_domain_indices([X], D, Is).
    X = X{[mo, tu, we, th, fr, sa, su]}
    D = weekday
    Is = [X_ic{1 .. 7}]
    Yes (0.00s cpu)

    ?- X &:: weekday, symbols_domain_indices([X, Y, we], D, Is).
    X = X{[mo, tu, we, th, fr, sa, su]}
    Y = Y{[mo, tu, we, th, fr, sa, su]}
    D = eclipse:weekday
    Is = [X_ic{1 .. 7}, X_ic{1 .. 7}, 3]
    Yes (0.00s cpu)
    ",
    desc:html("<P>
	Low-level primitive:
	For a list of domain variables or domain values, return the
	domain name and a list of integers or integer variables reflecting
	the corresponding integer index within the domain order.
	If the domain name is instantiated at call time, the list elements
	will be checked to be all from this domain.
	</P><P>
	An error will be reported if not all list elements come from the same
	domain. If the list contained domain-less variables, then, as a side
	effect, these will be given a domain to be compatible with the other
	list elements.
	</P>
")]).


:- comment(indomain/1, [
    summary:"Nondeterministically instantiate to domain values",
    args:["X":"domain variable or value"],
    see_also:[(&::)/2, (domain)/1],
    eg:"
    ?- local domain(weekday(mo, tu, we, th, fr, sa, su)).
    Yes (0.00s cpu)

    ?- X &:: weekday.
    X = X{[mo, tu, th, fr, sa, su]}
    Yes (0.00s cpu)

    ?- X &:: weekday, indomain(X).
    X = mo
    More (0.00s cpu)
    X = tu
    More (0.01s cpu)
    X = we
    More (0.02s cpu)
    X = th
    More (0.03s cpu)
    X = fr
    More (0.05s cpu)
    X = sa
    More (0.06s cpu)
    X = su
    Yes (0.06s cpu)

    ?- indomain(we).
    Yes (0.00s cpu)
    ",
    desc:html("<P>
	Instantiates a domain variable to its domain values. The order of
	enumeration is in increasing domain order.
    </P>")]).


:- comment(msg/3, [
    summary:"MSG is the most specific generalisation of X and Y "
	"representable with ic-symbolic domain variables",
    args:["X":"Any term or variable",
	"Y":"Any term or variable",
	"MSG":"A domain variable or constant (output)"],
    amode:msg(?,?,-),
    fail_if:"None",
    see_also:[_:msg/3, library(propia), (&::)/2, (domain)/1],
    eg:"
    ?- local domain(weekday(mo, tu, we, th, fr, sa, su)).
    Yes (0.00s cpu)

    ?- msg(we, fr, Z).
    Z = Z{[we, fr]}
    Yes (0.00s cpu)

    ?- X &:: [sa, su], msg(X, we, Z).
    X = X{[sa, su]}
    Z = Z{[we, sa, su]}
    Yes (0.00s cpu)

    ?- X &:: [sa, su], Y &:: [mo, tu, we], msg(X, Y, Z).
    X = X{[sa, su]}
    Y = Y{[mo, tu, we]}
    Z = Z{[mo, tu, we, sa, su]}
    Yes (0.00s cpu)

    ?- X &:: [sa, su], msg(X, _, Z).
    X = X{[sa, su]}
    Z = Z
    Yes (0.01s cpu)

    % in the following, the result is not precisely representable
    ?- X &:: [sa, su], msg(X, foo, Z).
    X = X{[sa, su]}
    Z = Z
    Yes (0.01s cpu)
    ",
    desc:html("<P>
	This predicate computes the most specific generalisation of X and Y
	which can be represented using ic-symbolic's domains and domain
	variables.
	</P><P>
	If X and Y are domain variables (or constants) from the same domain,
	then MSG will be unified with a new domain variable whose domain
	consists of the union of the domain elements of X and Y.
	</P><P>
	If X and Y are domain variables or constants with incompatible
	domains, then the result will be a free (unconstrained) variable.
	</P><P>
	If X or Y are free (unconstrained) variables, then the result will
	also be a free (unconstrained) variable.
    </P>")]).

