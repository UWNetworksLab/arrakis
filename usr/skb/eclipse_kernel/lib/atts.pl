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
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: atts.pl,v 1.2 2008/08/21 17:36:33 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * Attribute access compatible with SICStus v3
 */

:- module(atts).

:- comment(summary, "Variable attributes compatible with SICStus v3").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(author, "Micha Meier, ECRC Munich").
:- comment(date, "$Date: 2008/08/21 17:36:33 $").

:- export op(1150, fx, [attribute]).

:- export
    (attribute)/1,
    atts_subset/3,
    get_atts/2,
    map_atts/2,
    put_atts/2.

:- reexport
	get_attributes/4,
	replace_attribute/3
    from sepia_kernel.

% -----------------------------------------------------------------------

:- inline(get_atts/2, expand_get_atts/3).
:- inline(map_atts/2, expand_map_atts/3).
:- inline(put_atts/2, expand_put_atts/3).

:- dynamic
    t2v/4.

:- tool((attribute)/1, attribute_body/2).
:- tool(get_atts/2, get_atts_body/3).
:- tool(map_atts/2, map_atts_body/3).
:- tool(put_atts/2, put_atts_body/3).

attribute_body(Decls, Module) :-
    meta_attribute(Module, [])@Module,
    conj_to_list_functor(Decls, Attrs, 1, Size),
    functor(Vector, v, Size),
    compute_att_mask(Attrs, Module, Vector, ProcList, []),
    compile_term(ProcList).

compute_att_mask([], _, _, _, _) --> [].
compute_att_mask([Att|Atts], Module, Vector, I, M) -->
    [t2v(Att,Vector,M, Module)],
    {functor(Att, _, A),
    J is I+A,
    equate_args(A, Att, J, Vector),
    N is M<<1},
    compute_att_mask(Atts, Module, Vector, J, N).

conj_to_list_functor((N/A,B), [H|Rest], I, K) :- !,
    functor(H, N, A),
    J is I+A,
    conj_to_list_functor(B, Rest, J, K).
conj_to_list_functor(N/A, [H], I, K) :- 
    functor(H, N, A),
    K is I+A.



%
% Definition of the predicates for metacalls and debug compilation.
%
get_atts_body(Var, Spec, Module) :-
    expand_get_atts(get_atts(Var, Spec), Exp, Module),
    call(Exp)@Module.

map_atts_body(Attr, Spec, Module) :-
    expand_map_atts(map_atts(Attr, Spec), Exp, Module),
    call(Exp)@Module.

put_atts_body(Var, Spec, Module) :-
    expand_put_atts(put_atts(Var, Spec), Exp, Module),
    call(Exp)@Module.

%
% Expansion of the attribute predicates.
%
expand_get_atts(Goal, _, Module) :-
    not(t2v(_, _, _, Module)),		% check if any attribute defined
    !,
    error(270, Goal).
expand_get_atts(get_atts(Var, Spec), Exp, Module) :-
    nonvar(Spec), !,
    partition_spec(Spec, SpecP, [], SpecN, []),
    l2v(SpecP, Module, Vector, 0, SpecPM),
    l2v(SpecN, Module, Vector, 0, SpecNM),
    all_mask(Module, SpecAll),
    expand_get_atts(SpecPM, SpecNM, SpecAll, get_atts(Var,Spec), Module,
		    Vector, Exp).
expand_get_atts(get_atts(Var, Spec),
	        (get_attributes(Var,Vector,Mask, Module),
	         '$v2l'(Vector,Atts),
	         atts_subset(Atts,Mask,Spec)), Module).

expand_get_atts(P, N, _, Goal, Module, _, _) :-
	P /\ N =\= 0, !,
	error(6, Goal, Module).
expand_get_atts(0, 0, _, Goal, Module, _, Body) :- !,
	arg(1, Goal, Var),
	Body = (get_attributes(Var,_,_, Module)).
expand_get_atts(0, All, All, Goal, Module, _, Body) :- !,
	arg(1, Goal, Var),
	Body = (get_attributes(Var,_,0, Module)).
expand_get_atts(0, N, _, Goal, Module, _, Body) :- !,
	arg(1, Goal, Var),
	Body = (get_attributes(Var,_,Mask, Module),
		Mask /\ N =:= 0).
expand_get_atts(P, N, All, Goal, Module, Vector, Body) :-
	P\/N =:= All, !,
	arg(1, Goal, Var),
	Body = (get_attributes(Var,Vector0,Mask, Module),
		Mask = P,
		Vector0 = Vector).
expand_get_atts(P, N, _, Goal, Module, Vector, Body) :-
	arg(1, Goal, Var),
	PN is P\/N,
	Body = (get_attributes(Var,Vector0,Mask, Module),
		Mask /\ PN =:= P,
		Vector0=Vector).


expand_map_atts(Goal, _, Module) :-
    not(t2v(_, _, _, Module)),		% check if any attribute defined
    !,
    error(270, Goal).
expand_map_atts(map_atts(Attr, Spec), Exp, Module) :-
    nonvar(Spec), !,
    partition_spec(Spec, SpecP, [], SpecN, []),
    l2v(SpecP, Module, Vector, 0, SpecPM),
    l2v(SpecN, Module, Vector, 0, SpecNM),
    all_mask(Module, SpecAll),
    expand_map_atts(SpecPM, SpecNM, SpecAll, map_atts(Attr,Spec), Module,
		    Vector, Exp).
expand_map_atts(map_atts(Vector, Spec),
		(arg(1, Vector, Mask),
	         '$v2l'(Vector,Atts),
	         atts_subset(Atts,Mask,Spec)), _).

expand_map_atts(P, N, _, Goal, Module, _, _) :-
	P /\ N =\= 0, !,
	error(6, Goal, Module).
expand_map_atts(0, 0, _, Goal, _, _, Body) :- !,
	arg(1, Goal, Attr),
	Body = (compound(Attr)).
expand_map_atts(0, All, All, Goal, _, _, Body) :- !,
	arg(1, Goal, Attr),
	Body = (compound(Attr), arg(1, Attr, 0)).
expand_map_atts(0, N, _, Goal, _, _, Body) :- !,
	arg(1, Goal, Attr),
	Body = (compound(Attr), arg(1, Attr, Mask),
		Mask /\ N =:= 0).
expand_map_atts(P, N, All, Goal, _, Vector, Body) :-
	P\/N =:= All, !,
	arg(1, Goal, Attr),
	Body = (compound(Attr), arg(1, Attr, P),
		Attr = Vector).
expand_map_atts(P, N, _, Goal, _, Vector, Body) :-
	arg(1, Goal, Attr),
	PN is P\/N,
	Body = (compound(Attr), arg(1, Attr, Mask),
		Mask /\ PN =:= P,
		Attr=Vector).



expand_put_atts(Goal, _, Module) :-
    not(t2v(_, _, _, Module)),		% check if any attribute defined
    !,
    error(270, Goal).
expand_put_atts(put_atts(Var, Spec), Exp, Module) :-
	nonvar(Spec), !,
	partition_spec(Spec, SpecP, [], SpecN, []),
	all_atts(Module, All),
	put_exp(All, SpecP, SpecN, VOld, VNew, 0, PM, 0, NM),
	all_mask(Module, SpecAll),
	expand_put_atts(PM, NM, SpecAll, put_atts(Var,Spec), Module, VOld, VNew, Exp).
expand_put_atts(Goal, _, Module) :-
	error(6, Goal, Module).
   
expand_put_atts(P, N, _, Goal, Module, _, _, _) :-
	P /\ N =\= 0, !,
	error(6, Goal, Module).
expand_put_atts(0, 0, _, Goal, Module, _, _, Body) :- !,
	arg(1, Goal, Var),
	Body = get_attributes(Var,_,_, Module).
expand_put_atts(0, All, All, Goal, Module, _, VNew, Body) :- !,
	arg(1, Goal, Var),
	arg(1, VNew, 0),
	Body = (get_attributes(Var,_,OldMask, Module),
		OldMask =\= 0 ->
		replace_attribute(Var, VNew, Module)
	       ;true
	       ).
expand_put_atts(0, N, _, Goal, Module, VOld, VNew, Body) :- !,
	arg(1, Goal, Var),
	arg(1, VNew, NewMask),
	NN is \(N),
	Body = (get_attributes(Var,V,OldMask, Module),
		NewMask is OldMask /\ NN,
		NewMask =\= OldMask ->
		V = VOld,
		replace_attribute(Var, VNew, Module)
	       ;true
	       ).
expand_put_atts(P, N, All, Goal, Module, _, VNew, Body) :-
	P\/N =:= All, !,
	arg(1, Goal, Var),
	arg(1, VNew, P),
	Body = replace_attribute(Var, VNew, Module).
expand_put_atts(P, 0, _, Goal, Module, VOld, VNew, Body) :- !,
	arg(1, Goal, Var),
	arg(1, VNew, NewMask),
	Body = (get_attributes(Var,V,OldMask, Module),
		NewMask is OldMask \/ P,
		V = VOld,
		replace_attribute(Var, VNew, Module)
	       ).
expand_put_atts(P, N, _, Goal, Module, VOld, VNew, Body) :-
	arg(1, Goal, Var),
	arg(1, VNew, NewMask),
	NN is \(N),
	Body = (get_attributes(Var,V,OldMask, Module),
		NewMask is (OldMask \/ P) /\ NN,
		V = VOld,
		replace_attribute(Var, VNew, Module)
	       ).

partition_spec(X, _, _, _, _) :- var(X), !, fail. 
partition_spec(+(X), [X|P], P, N, N) :- !.
partition_spec(-(X), P, P, [Y|N], N) :- !,
	functor(X, F, A),
	functor(Y, F, A).
partition_spec([], P0, P0, N0, N0) :- !.
partition_spec([S|Ss], P2, P0, N2, N0) :- !,
	partition_spec(S, P1, P0, N1, N0),
	partition_spec(Ss, P2, P1, N2, N1).
partition_spec(X, [X|P], P, N, N).


l2v([], _, _, M0, M0).
l2v([T|Ts], Module, V, M0, M2) :-
	t2v(T,V,Mask, Module), !,
	M1 is M0 \/ Mask,
	l2v(Ts, Module, V, M1, M2).

all_mask(Module, Mask) :-
	findall(Bits, t2v(_,_,Bits, Module), All),
	all_mask(All, 0, Mask).

all_mask([], Mask, Mask).
all_mask([Bits|L], Mask0, Mask) :-
	Mask1 is Mask0\/Bits,
	all_mask(L, Mask1, Mask).

all_atts(Module, All) :-
	findall(T2V, is_t2v(Module,T2V), All).

is_t2v(Module, t2v(T,Vect,Mask)) :-
	t2v(T,Vect,Mask, Module),
	Mask =\= 0.

put_exp([], _, _, _, _, PM, PM, NM, NM).
put_exp([t2v(T,Vec,Mask)|Ts], SpecP, SpecN, VOld, VNew, PM0, PM, NM0, NM) :-
	(   memberchk(T,SpecP) ->
	    PM1 is PM0 \/ Mask
	;   PM1 = PM0
	),
	(   memberchk(T,SpecN) ->
	    NM1 is NM0 \/ Mask
	;   NM1 = NM0
	),
	(   (PM1\/NM1)/\Mask =:= Mask -> true
	;   copy_term(T-Vec, T-VOld)
	),
	Vec = VNew,
	put_exp(Ts, SpecP, SpecN, VOld, VNew, PM1, PM, NM1, NM).

:- pragma(nodebug).	% to zap variable names, otherwise we get singleton
			% warnings in the expanded source)

% exported - called by expanded code
atts_subset([], _, []).
atts_subset([Att|Atts1], Mask, Present) :-
	(   Mask/\1 =:= 1 -> Present = [Att|Present1]
	;   Present = Present1
	),
	Mask1 is Mask>>1,
	atts_subset(Atts1, Mask1, Present1).

compute_att_mask(Atts, Module, Vector) -->
    {arg(1, Vector, Mask),
    retractall(t2v(_, _, _, Module))},
    [t2v('$bitv'(Mask),Vector,0, Module)],
    compute_att_mask(Atts, Module, Vector, 1, 1),
    {compile_term('$v2l'(Vector,Atts))@Module}.

equate_args(0, _, _, _) :- !.
equate_args(A, T1, C, T2) :-
    arg(A, T1, X),
    arg(C, T2, X),
    B is A-1, D is C-1,
    equate_args(B, T1, D, T2).

