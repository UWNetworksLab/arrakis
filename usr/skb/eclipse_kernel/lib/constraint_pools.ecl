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
% Copyright (C) 2002-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Kish Shen and Joachim Schimpf, IC-Parc, Imperial College
% Version:	$Id: constraint_pools.ecl,v 1.1 2008/06/30 17:43:44 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(constraint_pools).

:- export
	create_constraint_pool/3,
	is_constraint_pool/1,
	post_typed_pool_constraint/3,
	collect_typed_pool_constraints/3,
	collect_all_pool_constraints/2,
	get_typed_pool_constraints/3,
	set_typed_pool_constraints/3,
	get_all_pool_constraints/2,
	pool_is_empty/1,
	get_pool_item/2,
	set_pool_item/2.

:- import
	getval_body/3,
	setval_body/3
    from sepia_kernel.

:- tool(create_constraint_pool/3, create_constraint_pool/4).

:- local struct(pool_data(store,item)).


:- comment(summary, "Support for the creation of constraint pools").
:- comment(author, "Kish Shen and Joachim Schimpf, IC-Parc, Imperial College").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date").

:- comment(desc, html("<P>
    This library contains support for constraint pools. Constraint
    pools are a way to have several instances of a constraint solver.
</P><P>
    A constraint pool is an Eclipse module. It exports a number of
    predicates (often constraint predicates) which are specified when
    the pool is created. The constraint pool does not implement these
    predicates (constraints) itself, but simply forwards every predicate
    call, or stores the calls, annotated with the pool name.
</P>
")).

:- comment(create_constraint_pool/3, [
    summary:"Create a \"constraint pool\" module",
    amode:create_constraint_pool(+,+,++),
    args:[
	"PoolName":"Atom - the name of the pool to create",
	"NTypes":"Integer - the number of constraint types",
    	"SpecList":"List of terms of the form Atom/Integer->Spec"
    ],
    desc:html("<P>
	Create a special type of module, called a \"constraint pool\".
</P><P>
	The module will contain:
	<UL>
	<LI>simple definitions for the predicates listed in SpecList.
	    These definitions will just store or forward every call.
	<LI>a logical store which can be used to store and retrieve
	    constraints, indexed by pool name and type. NTypes is the number
	    of different constraint types that this pool will support.
	<LI>a logical store for one additional data item, for example
	    a solver handle.
	</UL>
</P><P>
	The possible specifications in SpecList are:
	<DL>
	<DT>N/A -&gt; store_as(Type)</DT><DD>
	    will generate a definition for the predicate N/A such that every
	    call to N/A will be stored in the pool for the given Type.
	    </DD>
	<DT>N/A -&gt; ImplN/ImplA</DT><DD>
	    will generate a definition for the predicate N/A such that every
	    call to N/A gets augmented with an additional argument (the pool
	    name), and mapped into a call to the implementation predicate
	    ImplN/ImplA. The implementation predicate must be visible from
	    where create_constraint_pool/3 is invoked.  The implementation
	    predicate's arity ImplA must be one higher than the the arity A
	    of the newly defined predicate.
	    </DD>
	</DL>
</P><P>
	Since a pool is a module, the pool name should normally not refer
	to an existing module. If it does, the existing module gets augmented
	with the pool predicates and pool stores.
</P>
    "),
    eg:"\
    % We assume the implementation predicate:
    d(Data, Pool) :- writeln(d(Data, Pool)).

    % Create the pool:
    ?- create_constraint_pool(pool, 1, [c/2->store_as(1),d/1->d/2]).
    Yes (0.00s cpu)

    % Call the just created pool constraint d/1,
    % which leads to d/2 being invoked:
    ?- pool:d(hello).
    d(hello, pool)
    Yes (0.00s cpu)

    % Call the just created pool constraint c/2,
    % which will be stored. Then retrieve the store:
    ?- pool:c(a,b), collect_all_pool_constraints(pool, C).
    C = [c(a, b)]
    Yes (0.00s cpu)
    ",
    see_also:[is_constraint_pool/1,pool_is_empty/1,
    	post_typed_pool_constraint/3,
	collect_typed_pool_constraints/3,
	collect_all_pool_constraints/2,
	set_pool_item/2,get_pool_item/2]
    ]).


:- comment(is_constraint_pool/1, [
    summary:"Check whether Pool is a constraint pool",
    amode:is_constraint_pool(+),
    args:[
    	"Pool":"Atom"
    ],
    desc:html("
    Succeeds of Pool is a constraint pool that has been created earlier
    using create_constraint_pool/3.
    "),
    eg:"\
    ",
    see_also:[create_constraint_pool/3]]).

:- comment(pool_is_empty/1, [
    summary:"Check whether Pool is an empty constraint pool",
    amode:pool_is_empty(+),
    args:[
    	"Pool":"Atom"
    ],
    desc:html("
    Succeeds of Pool is an empty constraint pool, i.e. has no constraints
    stored.
    "),
    eg:"\
    ",
    see_also:[create_constraint_pool/3,post_typed_pool_constraint/3,
    	collect_typed_pool_constraints/3,collect_all_pool_constraints/2,
    	get_typed_pool_constraints, set_typed_pool_constraints/3,
	get_all_pool_constraints/2]]).

:- comment(post_typed_pool_constraint/3, [
    summary:"Stores the term Constraint in Pool's constraint store as type Type",
    amode:post_typed_pool_constraint(+,+,+),
    args:[
    	"Pool":"Atom",
    	"Type":"Integer",
    	"Constraint":"Callable Term"
    ],
    desc:html("
    Stores the term Constraint in Pool's constraint store as type Type.
    Constraint is not interpreted in any way, it is just stored in order
    to be retrieved later using one of the retrieval predicates.
    "),
    eg:"\
    ?- create_constraint_pool(p, 3, []).
    Yes (0.09s cpu)

    ?- post_typed_pool_constraint(p, 1, foo(one)),
	    post_typed_pool_constraint(p, 2, bar(two)),
	    post_typed_pool_constraint(p, 1, baz(three)),
	    get_typed_pool_constraints(p, 1, C1),
	    get_typed_pool_constraints(p, 2, C2).
	    get_typed_pool_constraints(p, 3, C3).

    C1 = [baz(three), foo(one)]
    C2 = [bar(two)]
    C3 = []
    Yes (0.00s cpu)
    ",
    see_also:[collect_typed_pool_constraints/3,collect_all_pool_constraints/2,
    	get_typed_pool_constraints, set_typed_pool_constraints/3,
	get_all_pool_constraints/2]]).

:- comment(get_typed_pool_constraints/3, [
    summary:"Get all currently stored constraints of type Type in Pool",
    amode:get_typed_pool_constraints(+,+,-),
    args:[
    	"Pool":"Atom",
    	"Type":"Integer",
    	"Constraints":"Variable, returns list of callable terms"
    ],
    desc:html("
    Constraints is unified with a list of all currently stored constraints
    of type Type in Pool. The pool itself is not changed.
    An empty list is returned if the pool is empty for this type.
    "),
    see_also:[collect_typed_pool_constraints/3,get_all_pool_constraints/2]]).

:- comment(collect_typed_pool_constraints/3, [
    summary:"Collect all currently stored constraints of type Type in Pool",
    amode:collect_typed_pool_constraints(+,+,-),
    args:[
    	"Pool":"Atom",
    	"Type":"Integer",
    	"Constraints":"Variable, returns list of callable terms"
    ],
    desc:html("
    Constraints is unified with a list of all currently stored
    constraints of type Type in Pool.  An empty list is returned if
    the pool was already empty for this type.  The pool store for type
    Type is emptied.  This modification is backtrackable.
    "),
    see_also:[pool_is_empty/1,get_typed_pool_constraints/3,collect_all_pool_constraints/2]]).

:- comment(get_all_pool_constraints/2, [
    summary:"Get all currently stored constraints Pool",
    amode:get_all_pool_constraints(+,-),
    args:[
    	"Pool":"Atom",
    	"Constraints":"Variable, returns list of callable terms"
    ],
    desc:html("
    Constraints is unified with a list of all currently stored constraints
    in Pool, regardless of their type. The pool itself is not changed.
    An empty list is returned if the pool is empty.
    "),
    see_also:[collect_all_pool_constraints/2,get_typed_pool_constraints/3]]).

:- comment(collect_all_pool_constraints/2, [
    summary:"Collect all currently stored constraints in Pool",
    amode:collect_all_pool_constraints(+,-),
    args:[
    	"Pool":"Atom",
    	"Constraints":"Variable, returns list of callable terms"
    ],
    desc:html("
    Constraints is unified with a list of all currently stored constraints
    in Pool, regardless of their type.  An empty list is returned if the
    pool was already empty.  The pool store is emptied completely.
    This modification is backtrackable.
    "),
    see_also:[pool_is_empty/1,get_all_pool_constraints/2,collect_typed_pool_constraints/3]]).

:- comment(set_typed_pool_constraints/3, [
    summary:"Replace the stored constraints for type Type",
    amode:set_typed_pool_constraints(+,+,+),
    args:[
    	"Pool":"Atom",
    	"Type":"Integer",
    	"Constraints":"List of callable Terms"
    ],
    desc:html("
    The constraint store of pool Pool for type Type is forgotten and
    replaced with the list of constraints Constraints.  This
    modification is backtrackable.
    "),
    eg:"\
    ",
    see_also:[collect_typed_pool_constraints/3,get_typed_pool_constraints/3]]).


:- comment(set_pool_item/2, [
    summary:"Associate a term Item with the pool Pool",
    amode:set_pool_item(+,+),
    args:[
    	"Pool":"Atom",
    	"Item":"Arbitrary term"
    ],
    desc:html("
    An arbitrary term (for example a solver handle) is associated with the
    pool Pool, in order to be retrieved later using get_pool_item/2.
    The modification to the pool is backtrackable.
    "),
    eg:"\
    ?- create_constraint_pool(p, 0, []).
    Yes (0.08s cpu)

    ?- set_pool_item(p, sample_item), get_pool_item(p,X).
    X = sample_item
    Yes (0.00s cpu)
    ",
    see_also:[create_constraint_pool/3, get_pool_item/2]]).

:- comment(get_pool_item/2, [
    summary:"Associate a term Item with the pool Pool",
    amode:get_pool_item(+,-),
    args:[
    	"Pool":"Atom",
    	"Item":"Variable, returns term"
    ],
    desc:html("
    Retrieve the 'pool item', i.e. the term associated with the
    pool using set_pool_item/2. This could for example be a solver
    handle.  The pool is not modified.
    "),
    see_also:[create_constraint_pool/3, set_pool_item/2]]).


%----------------------------------------------------------------------
% pool creation
%----------------------------------------------------------------------

create_constraint_pool(Pool, NTypes, Preds, Module) :-
	atom(Pool), integer(NTypes), ( Preds=[_|_] ; Preds=[] ),
	!,
	( current_module(Pool) ->
	    ( is_constraint_pool(Pool) ->
		printf(warning_output, "create_constraint_pool: pool %w was already initialised%n", [Pool])
	    ;
		init_constraint_pool(Pool, NTypes, Preds, Module)
	    )
	;
	    create_module(Pool),
	    init_constraint_pool(Pool, NTypes, Preds, Module)
	).
create_constraint_pool(Pool, NTypes, Preds, Module) :-
	error(5, create_constraint_pool(Pool, NTypes, Preds), Module).


    init_constraint_pool(Pool, NTypes, Preds, Module) :-
	(
	    foreach(Pred, Preds),
	    param(Pool,Module)
	do
	    make_clause(Pred, Pool, Module)
	),
	compile_term(static_pool_data(NTypes))@Pool,
	local(reference(pool_data))@Pool.


    % TODO: generate inline transformations as well
    make_clause(F/N->store_as(Type), Pool, _Module) ?-
	atom(F), integer(N),
	!,
	export(F/N)@Pool,
	functor(Head, F, N),
	compile_term((
	    Head :- constraint_pools:post_typed_pool_constraint(Pool,Type,Head)
	))@Pool.
    make_clause(F/N->store_with_module_as(Type), Pool, _Module) ?-
	atom(F), integer(N),
	!,
	export(F/N)@Pool,
	concat_atoms(F,'_',FB),
	NB is N+1,
	tool(F/N,FB/NB)@Pool,
	functor(Head, FB, NB),
	functor(Goal, F, N),
	arg(NB, Head, CM),
	compile_term((
	    Head :- constraint_pools:post_typed_pool_constraint(Pool,Type,Goal@CM)
	))@Pool.
    make_clause(F/N->F1/N1, Pool, Module) ?-
	atom(F), integer(N),
	atom(F1), integer(N1),
	N1 =:= N+1,
	!,
	( get_flag(F1/N1, definition_module, DM)@Module ->
	    export(F/N)@Pool,
	    ( get_flag(F1/N1, tool, on)@Module ->
		tool_body(F1/N1, F1B/N1B, DMB)@Module,
		concat_atoms(F,'_',FB),
		NB is N+1,
		tool(F/N,FB/NB)@Pool,
		functor(Head, FB, NB),
		functor(Call, F1B, N1B),
		( for(I,1,N), param(Head,Call) do
		    arg(I, Head, X),
		    arg(I, Call, X)
		),
		arg(N1, Head, CM),
		arg(N1B, Call, CM),
		PoolArg is N1B-1,
		arg(PoolArg, Call, Pool),
		export(F1B/N1B)@DMB,
		Clause = (Head :- DMB:Call)
	    ;
		functor(Head, F, N),
		functor(Call, F1, N1),
		( for(I,1,N), param(Head,Call) do
		    arg(I, Head, X),
		    arg(I, Call, X)
		),
		arg(N1, Call, Pool),
		export(F1/N1)@DM,
		Clause = (Head :- DM:Call)
	    ),
%	    writeln(Clause),
	    compile_term(Clause)@Pool
	;
	    printf(error, "init_constraint_pool: No such predicate: %w in module %w%n",
	    	[F1/N1, Module]),
	    abort
	).
    make_clause(Spec, _Pool, _Module) :-
	printf(error, "init_constraint_pool: illegal spec: %w%n", [Spec]),
	abort.


is_constraint_pool(Pool) :-
	atom(Pool), !,
	current_module(Pool), 
%	current_array(pool_data, _)@Pool,
	get_flag(static_pool_data/1, defined, on)@Pool.
is_constraint_pool(Pool) :-
	error(5, is_constraint_pool(Pool)).


%----------------------------------------------------------------------
% the store
%----------------------------------------------------------------------

get_pool_data(Pool, Data) :-
	getval_body(pool_data, Data0, Pool),
	init_data_if_needed(Data0, Data, Pool).

    init_data_if_needed(0, Data, Pool) :- !,
	Data = pool_data{store:Store,item:0},
	call(static_pool_data(NTypes))@Pool,
	functor(Store, [], NTypes),
	( foreacharg([], Store) do true ),
	setval_body(pool_data, Data, Pool).
    init_data_if_needed(Data, Data, _).


pool_is_empty(Pool) :-
	get_pool_store(Pool, Store),
	( foreacharg([], Store) do true ).


get_pool_item(Pool, Item) :-
	get_pool_data(Pool, Data),
	Data = pool_data{item:Item}.


set_pool_item(Pool, Item) :-
	get_pool_data(Pool, Data),
	setarg(item of pool_data, Data, Item).


get_pool_store(Pool, Store) :-
	get_pool_data(Pool, Data),
	Data = pool_data{store:Store}. 


% this is performance-critical, that's why there are no checks done here...
post_typed_pool_constraint(Pool, Type, Const) :-
	get_pool_data(Pool, Data),
	Data = pool_data{store:Store},
	arg(Type, Store, Consts),
	setarg(Type, Store, [Const|Consts]).


collect_typed_pool_constraints(Pool, Type, Consts) :-
	get_pool_store(Pool, Store),
	arg(Type, Store, Consts),
	clear_store(Type, Store, Consts).

    clear_store(_Type, _Store, []) :- !.
    clear_store(Type, Store, _) :-
	setarg(Type, Store, []).


get_typed_pool_constraints(Pool, Type, Consts) :-
	get_pool_store(Pool, Store),
	arg(Type, Store, Consts).


set_typed_pool_constraints(Pool, Type, Consts) :-
	get_pool_store(Pool, Store),
	setarg(Type, Store, Consts).


get_all_pool_constraints(Pool, AllConsts) :-
	get_pool_store(Pool, Store),
	( foreacharg(Consts, Store), fromto(AllConsts, C1, C2, []) do
	    ( foreach(C,Consts), fromto(C1, [C|C3], C3, C2) do true )
	).


collect_all_pool_constraints(Pool, AllConsts) :-
	get_pool_store(Pool, Store),
	functor(Store, _, N),
	( for(I,1,N), fromto(AllConsts, C1, C2, []), param(Store) do
	    arg(I, Store, Consts),
	    setarg(I, Store, []),
	    ( foreach(C,Consts), fromto(C1, [C|C3], C3, C2) do true )
	).

