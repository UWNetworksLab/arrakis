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
% Version:	$Id: pdb.pl,v 1.1 2008/06/30 17:43:48 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	pdb.pl
 *
 * DESCRIPTION: 	(used to be db.pl)
 *
 * CONTENTS:     
 *
 */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_module(sepia_kernel).
:- system.		% compiler directive to add the SYSTEM flag

:- export
	current_atom/1,
	current_functor/1,
	current_module/1,
	current_op/3,
	current_predicate/1,
	current_built_in/1,
	is_built_in/1,
	current_macro/4,
	pred/1,
	trimcore/0,
	abolish_op/2,
	(als)/1,
	(als)/2.

:- tool( current_predicate/1, current_predicate_body/2).
:- tool( current_built_in/1, current_built_in_body/2).
:- tool( is_built_in/1, is_built_in_body/2).
:- tool( current_op/3, current_op_body/4).
:- tool( current_macro/4, current_macro_body/5).
:- tool( abolish_op/2, abolish_op_body/3).
:- tool( pred/1, pred_body/2).
:- tool((als)/1, (als)/2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


current_atom(Atom) :- var(Atom), !, current_functor(Atom, 0, 0, 0).
current_atom(Atom) :- atom(Atom), !.
current_atom(Atom) :- error(5, current_atom(Atom)).

current_functor(Name/Arity) :-
	(   (var(Name) ; atom(Name)),
	    (var(Arity) ; integer(Arity), Arity >= 0 )
	->
	    current_functor(Name, Arity, 0, 0)
	;
	    error(5, current_functor(Name/Arity))
	).

current_module(M) :-
	var(M), !,
	current_functor(M, 0, 1, 0),	% atoms with properties only
	is_a_module(M).
current_module(M) :-
	atom(M), !,
	is_a_module(M).
current_module(M) :-
	error(5, current_module(M)).

current_op_body(Preced, Assoc, Name, Module):-
	legal_current_op(Preced, Assoc, Name, Module)
	->
	    ( var(Name) ->
		current_functor(Name, 0, 1, 0)	% atoms with properties only
	    ;
		true
	    ),
	    (
		is_infix_op(Preced, Assoc, Name, _, Module)
		;
		is_prefix_op(Preced, Assoc, Name, _, Module) 
		; 
		is_postfix_op(Preced, Assoc, Name, _, Module)
	    ),
	    Preced \== 0
	;
	    get_bip_error(Err),
	    error(Err, current_op(Preced, Assoc, Name), Module).


current_macro_body(Functor, Pred, List, PredModule, Module) :-
	check_var_or_partial_macro_spec(Functor),
	check_var_or_partial_qual_predspec(Pred),
	check_var_or_partial_list(List),
	check_var_or_atom(PredModule),
	!,
	current_macro_body1(Functor, Pred, List, PredModule, Module).
current_macro_body(Functor, Pred, List, PredModule, Module) :-
	bip_error(current_macro(Functor, Pred, List, PredModule), Module).

current_macro_body1(Functor, Pred, List, PredModule, Module) :-
	var(Functor),
	!,
	(
	    current_functor(Name, Arity, 1, 0),	% functors with properties only
	    Functor = Name/Arity
	;
	    current_type(T),
	    Functor = type(T)
	),
	is_macro(Functor, Pred, List, PredModule, Module).
current_macro_body1(Functor, Pred, List, PredModule, Module) :-
	Functor = Name/Arity,
	atom(Name),
	integer(Arity),
	!,
	is_macro(Functor, Pred, List, PredModule, Module).
current_macro_body1(Functor, Pred, List, PredModule, Module) :-
	Functor = Name/Arity,
	!,
	current_functor(Name, Arity, 1, 0),
	is_macro(Functor, Pred, List, PredModule, Module).
current_macro_body1(Type, Pred, List, PredModule, Module) :-
	Type = type(T),
	current_type(T),
	is_macro(Type, Pred, List, PredModule, Module).


abolish_op_body(Operator, Assoc, Module) :-
	abolish_op_(Operator, Assoc, Module)
	->
	    true
	;
	    get_bip_error(Error),
	    error(Error, abolish_op(Operator, Assoc), Module).


matches_predspec(N/A) :-
	( var(N) -> true ; atom(N) ),
	( var(A) -> true ; integer(A), A >= 0, A =< 255 ).

current_predicate_body(P, M):-
	illegal_unlocked_module(M, Err),
	!,
	error(Err, current_predicate(P), M).
current_predicate_body(P, M):-
	P = N/A,
	matches_predspec(P),
	!,
	( nonground(P) ->
	    current_functor(N, A, 2, 0)		% functors with predicates only
	;
	    true
	),
	get_flag_body(P, defined, on, M),
	get_flag_body(P, type, user, M).
current_predicate_body(P, M):-
	error(5, current_predicate(P), M).


current_built_in_body(P, M):-
	illegal_unlocked_module(M, Err),
	!,
	error(Err, current_built_in(P), M).
current_built_in_body(P, M):-
	P = N/A,
	matches_predspec(P),
	!,
	( nonground(P) ->
	    current_functor(N, A, 2, 0)		% functors with predicates only
	;
	    true
	),
	get_flag_body(P, defined, on, M),
	get_flag_body(P, type, built_in, M).
current_built_in_body(P, M):-
	error(5, current_built_in(P), M).


is_built_in_body(Functor, Module) :-
	( check_predspec(Functor, Module) ->
		is_built_in_(Functor, Module)
	;
		bip_error(is_built_in(Functor), Module)
	).


als(Proc, Module) :-
	(var(Proc) ->
		error(4, als(Proc))
	;
	atom(Proc) ->
		(current_predicate_body(Proc/Arity, Module)
		;
		current_built_in_body(Proc/Arity, Module)),
		als_(Proc/Arity, Module)
	;
	Proc = _/A, var(A) ->
		(current_predicate_body(Proc, Module)
		;
		current_built_in_body(Proc, Module)),
		als_(Proc, Module)
	;
		als_(Proc, Module)
	).


pred_body(Proc, M) :-
	var(Proc), !,
	error(4, pred(Proc), M).
pred_body(Proc, M) :-
	atom(Proc), !,
	(
	    ( current_predicate_body(Proc/A, M)
	    ; current_built_in_body(Proc/A, M) ),
	    nl,
	    pred_body(Proc/A, M),
	    fail
	;
	    true
	).
pred_body(Proc, M) :-
	Proc = _/_, !,
	get_flag_body(Proc, _, _, M),	% so that it fails if none visible
	!,
	(
	    get_flag_body(Proc, F, V, M),
	    printf('%-20s%w%n', [F, V]),
	    fail
	;
	    true
	).
pred_body(Proc, M) :-
	error(5, pred(Proc), M).


trimcore :-
	% We do a garbage collection first because trimcore0 unmaps unsed
	% parts of the stacks. The gc removes trail entries which point above
	% the stack tops. Such entries could lead to segfaults when untrailing
	% after unmapping the former stack space they point to.
	garbage_collect,
	% Now unmap space above stack tops, free abolished code, etc.
	trimcore0.


:- skipped
	abolish_op/2,
	current_built_in/1,
	current_op/3,
	current_predicate/1,
	is_built_in/1,
	pred/1.

:- untraceable
	(als)/1,
	(als)/2,
	pred/1.

