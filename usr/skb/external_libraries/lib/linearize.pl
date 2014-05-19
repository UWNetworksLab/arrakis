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
% Copyright (C) 1997-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: linearize.pl,v 1.1.1.1 2006/09/23 01:53:50 snovello Exp $
%
% Description:		Expression simplifier
%
% Authors:		J.Schimpf, IC-Parc
%
%
% A linear expression is normalised into a list (sum) of the form
%
%	[C0*1, C1*X1, C2*X2, ...]
%
% where Ci are numbers and Xi are distinct variables.
% The first (constant) term is always present, Ci (i>=1) are nonzero.
%
% linearize/3 converts a general arithmetic expression into a
% normalised linear form. The nonlinear parts are extracted using
% auxiliary variables, and returned as Residue, which is a list of
% Aux=NonLinExpr constraints.
%
% linearize/3 understands all arithmetic expressions plus
%
%	lin(Lin)	embedded normalised linear expression
%
%
% linrenorm/2 re-normalises a linear normal form expression after variable
% bindings, unifications.
%
% delinearize/2 converts back into normal arithmetic expression, but
% that may often not be needed since linearize/3 understands lin(Lin).
%
% TODO:
%	-- E1*E2-clause of linearize/7 always makes a choicepoint...
%
% ----------------------------------------------------------------------


% ----------------------------------------------------------------------
:- module(linearize).
% ----------------------------------------------------------------------

:- comment(summary, "Normalizers for arithmetic expressions").
:- comment(author, "Joachim Schimpf, IC-Parc").
:- comment(date, "$Date: 2006/09/23 01:53:50 $").
:- comment(copyright, "Cisco Systems, Inc.").

:- export
	linearize/3,
	linrenorm/2,
	delinearize/2.

:- comment(linearize/3, [
    summary:"Split and arithmetic expression into linear and nonlinear parts",
    amode:linearize(?,-,-),
    args:["Expression":"Arithmetic expression with constants and variables",
	"Linear":"Normalized lists of monomials",
	"Residue":"Residual nonlinear components in the form AuxVar=Expr"],
    see_also:[polynorm/3,linrenorm/2,delinearize/2],
    desc:html("\
	The linear component of expression is normalised into a list
	(sum) of monomials of the form
<PRE>
		[C0*1, C1*X1, C2*X2, ...]
</PRE>
	where Ci are numbers and Xi are distinct variables.
	The first (constant) term is always present, Ci (i>=1) are nonzero.
<P>
	linearize/3 converts a general arithmetic expression into a
	normalised linear form. The nonlinear parts are extracted using
	auxiliary variables, and returned as Residue, which is a list of
	Aux=NonLinExpr constraints.
<P>
	linearize/3 understands all arithmetic expressions plus
<PRE>
	    lin(Lin)
</PRE>
	where Lin is an already normalised linear expression. All variables
	within Expression (which are free at linearization time) are taken
	to be numerical variables. If you intend to have variables which can
	be bound to symbolic expressions rather than numbers, they must be
	wrapped into an eval/1 functor.
    "),
    eg:"
    ?- linearize(3*X-7*Y+2*(X+Y), L, R).
    X = X
    Y = Y
    L = [0 * 1, 5 * X, -5 * Y]
    R = []
    yes.

    ?- linearize(12*3, L, R).
    L = [36 * 1]
    R = []
    yes.

    ?- linearize(X*(3+7*Y)-X, L, R).
    Y = Y
    X = X
    L = [0 * 1, 1 * _308, 2 * X]
    R = [_308 = X * (7 * Y)]
    yes.
    "
]).

:- comment(linrenorm/2, [
    summary:"Renormalize a linear form",
    amode:linrenorm(+,-),
    args:["LinOld":"Possibly denormal lists of monomials",
	"LinNew":"Normalized lists of monomials"],
    see_also:[linearize/3,delinearize/2],
    desc:html("\
	The normal form of linear expressions is
<PRE>
		[C0*1, C1*X1, C2*X2, ...]
</PRE>
	where Ci are numbers and Xi are distinct variables.
	The first (constant) term is always present, Ci (i>=1) are nonzero.
<P>
	Such a form can become denormalized due to unifications
	(instantiation or variable-variable aliasing). This predicate
	renormalizes it. Note that variables may only become instantiated
	to numbers!
    "),
    eg:"
    ?- linearize(3*X-7*Y+2*(X+Y), L1, R), writeln(L1),
	Y = 3,
	linrenorm(L1,L2), writeln(L2).

    [0 * 1, 5 * X, -5 * Y]
    [-15 * 1, 5 * X]
    "
]).

:- comment(delinearize/2, [
    summary:"Convert a linear form back to a standard arithmetic expression",
    amode:delinearize(?,-),
    args:[
	"Linear":"Normalized lists of monomials",
	"Expression":"Arithmetic expression with constants and variables"],
    see_also:[linrenorm/2,linearize/3],
    eg:"
    ?- linearize(3*X-7*Y+2*(X+Y), L, R), delinearize(L, E).
    X = X
    Y = Y
    R = []
    L = [0 * 1, 5 * X, -5 * Y]
    E = 5 * X - 5 * Y
    yes.
    "
]).

% ----------------------------------------------------------------------

:-tool(linearize/3,linearize_body/4).

linearize_body(Expr, Lin, Res, Mod) :-
	linearize(Expr, 0, C, L, [], Res, [], Mod),
	Lin0 = [C*1|L],
	linrenorm(Lin0, Lin).


% linearize(Expr, CstIn, CstOut, LinOut, LinIn, Res, ResT, Module).
% Split arithmetic expression Expr into constant, linear and residue part.

:- mode linearize(?, +, -, -, +, -, +, +).
linearize(X, C, C, [1*X|L], L, R, R, _Mod) :-
	var(X), !.
linearize(K, C0, C, L, L, R, R, _Mod) :-
	number(K), !,
        ( C0 = 0 ->
              % Avoid adding things to zero as we may lose the sign of
              % a -0.0__-0.0 breal in doing so
              C = K
        ;
              ( K = 0 ->
                    C = C0
              ;
                    C is C0+K
              )
        ).
linearize(E, C0, C, L, L0, R, R0, Mod) :- E = subscript(Array,Index), !,
	( nonground(Index) ->
	    C = C0, L = [1*Aux|L0], R = [Aux=E|R0]
	;
	    subscript(Array,Index,Elem),
	    linearize(Elem, C0, C, L, L0, R, R0, Mod)
	).
linearize(lin(Lin), C0, C, L, L0, R, R, _Mod) :- !,
	Lin = [K*1|L1],
	C is C0+K,
	difflistify(L1, L, L0).
linearize(+E, C0, C, L, L0, R, R0, Mod) :- !,
	linearize(E, C0, C, L, L0, R, R0, Mod).
linearize(-E, C0, C, L, L0, R, R0, Mod) :- !,
	linearize((-1)*E, C0, C, L, L0, R, R0, Mod).
linearize(E1+E2, C0, C, L, L0, R, R0, Mod) :- !,
	linearize(E1, C0, C1, L1, L0, R1, R0, Mod),
	linearize(E2, C1, C, L, L1, R, R1, Mod).
linearize(sum(List), C0, C, L, L0, R, R0, Mod) :- !,
	linearize_sum(List, C0, C, L, L0, R, R0, Mod).
linearize(E1-E2, C0, C, L, L0, R, R0, Mod) :- !,
	linearize(E1, C0, C1, L1, L0, R1, R0, Mod),
	linearize((-1)*E2, C1, C, L, L1, R, R1, Mod).
linearize(E1*E2, C0, C, L, L0, R, R0, Mod) :- !,
	( make_prod_sum(E1, E2, ProdSum) ->
	    linearize_sum(ProdSum, C0, C, L, L0, R, R0,Mod)
	;
	    linearize(E1, 0, E1C, E1Lin, [], ProdR1, ProdR0, Mod),
	    linearize(E2, 0, E2C, E2Lin, [], ProdR, ProdR1, Mod),
            EC is E1C*E2C,
            ( C0 = 0 ->
                  % if the accumulated constant is an integer 0 then
                  % do not add it to the newly calculated sum, doing
                  % this preserves the potential -0.0__-0.0 (negative
                  % zero) result of the multiplication which would
                  % otherwise be lost
                  C is EC
            ;
                  ( EC = 0 ->
                        C = C0
                  ;
                        C is C0 + EC
                  )
            ),
	    lin_mult(E1C, E2Lin, L1, L0),
	    lin_mult(E2C, E1Lin, L2, L1),
	    ( E1Lin==[] ->
		L=L2, R=ProdR, R0=ProdR0
	    ; E2Lin==[] ->
		L=L2, R=ProdR, R0=ProdR0
	    ;
		delinearize(E1Lin,E1LinExpr),
		delinearize(E2Lin,E2LinExpr),
		ProdR0 = [],
		resubstitute_free_nonlinears(ProdR,L2,R0,R1),
%		E1LinExpr = lin([0*1|E1Lin]),
%		E2LinExpr = lin([0*1|E2Lin]),
		L = [1*Aux|L2],
		% E1LinExpr*E2LinExpr could be simplified
		% and possibly a constant factored out.
		R = [Aux=E1LinExpr*E2LinExpr|R1]
	    )
	).
linearize(E, C0, C, L, L0, R, R0, Mod) :- E = eval(Expr), !,
	( var(Expr) ->
	    C = C0, L = [1*Aux|L0], R = [Aux=E|R0]
	;
	    linearize(Expr, C0, C, L, L0, R, R0, Mod)
	).
linearize(E, C, C, [1*Aux|L0], L0, [Aux=E|R0], R0, _Mod) :-
	nonground(E), !.
%linearize(E, C0, C, L, L, R, R, Mod) :-
%	(K is E)@Mod, C is C0+K.	% ground(E)
linearize(E, C0, C, L, L0, R, R0, Mod) :-
	%% ground(E)
	(subcall((K is E)@Mod,[]) ->
		 %% function returned without delaying goals
		 C is C0+K,
		 L=L0,
		 R=R0
	;
		 %% function left delayed goals, so return it to the caller
		 C0=C,
		 L=[1*Aux|L0],
		 R=[Aux=E|R0]
	).

    %% Suceeds when the variable V is present in the linear expression
    var_exists_in([_C*Y | Tail],V):-
        ( Y==V ->
              true
        ;
              var_exists_in(Tail,V)
        ).

    %%
    %% resubstitute([Temp=SubExp|..],+Lin, R0, R)
    %%
    %%   Reconstructs any non-linear expressions which had sub-
    %%   expressions replaced by temporaries (as given in the first
    %%   argument).  The caveat is that these bindings CANNOT be
    %%   replaced when the temporary variable exists in the normailised
    %%   linear expression Lin (as this would break the simple [1*
    %%   Const, Const*Var, Const*Var...] format).
    %%
    resubstitute_free_nonlinears([],_Lin,R,R).
    resubstitute_free_nonlinears([LHS=RHS|More], Lin, R0, R) :-
        (var_exists_in(Lin,LHS) ->
             %% var=expr bindings which refer to variables in the
             %% linear expression must be returned
             R1=[LHS=RHS|R0]
        ;
             %% Only substitue bindings that are NOT present in the
             %% Linear expression
             LHS=RHS,R1=R0
        ),
	resubstitute_free_nonlinears(More,Lin,R1,R).

    :- mode linearize_sum(?, +, -, -, +, -, +, +).
    linearize_sum(X, C, C, [1*Aux|L], L, [Aux=sum(X)|R], R, _Mod) :-
    	var(X), !.
    linearize_sum([], C, C, L, L, R, R, _Mod) :- !.
    linearize_sum([E|Es], C0, C, L, L0, R, R0, Mod) :- !,
	linearize(E, C0, C1, L1, L0, R1, R0, Mod),
	linearize_sum(Es, C1, C, L, L1, R, R1, Mod).
    linearize_sum(List, C0, C, L, L0, R, R0, Mod) :- List = subscript(Array,Index), !,
	( nonground(Index) ->
	    C = C0, L = [1*Aux|L0], R = [Aux=sum(List)|R0]
	;
	    subscript(Array,Index,Elems),
	    linearize_sum(Elems, C0, C, L, L0, R, R0, Mod)
	).
    linearize_sum(X, C, C, L, L, R, R, _Mod) :-
	error(5, _ is sum(X)).


    % lin_mult(+Cst, +LinIn, -LinOut, +LinOutT).
    lin_mult(K, CXs, CKXs, CKXs0) :-
	% `not K =\= 0' is like `K =:= 0' except it fails for bounded reals
	% which span 0 rather than setting up a delayed goal.
	( not K =\= 0 -> CKXs=CKXs0
	; lin_mult1(K, CXs, CKXs, CKXs0) ).

	:- mode lin_mult1(+,+,-,+).
	lin_mult1(_, [], CKXs, CKXs).
	lin_mult1(K, [C*X|CXs], [CK*X|CKXs1], CKXs) :-
	    CK is C*K,
	    lin_mult1(K, CXs, CKXs1, CKXs).

    make_prod_sum([], [], Sum) :- -?-> Sum=[].
    make_prod_sum([X|Xs], [Y|Ys], Sum) :- -?->
	Sum = [X*Y|XYs],
	make_prod_sum(Xs, Ys, XYs).


% ----------------------------------------------------------------------
% (re)normalise a linear term (see above) by collecting constants,
% collapsing multiple occurrences of variables and eliminating zeros.
% ----------------------------------------------------------------------

linrenorm(Lin, LinNorm) :-
	sort(2, >=, Lin, LinSorted),
	collapse_terms(LinSorted, LinNorm),
	( LinNorm=[Rhs*One|_], number(Rhs), One==1 -> true
	; writeln(error, "Error in linrenorm/2"), abort ). 


collapse_terms([], []).
collapse_terms([X|Xs], R) :-
	collapse_terms(X, Xs, R).

collapse_terms(X, Xs, Res) :-
	X = F*V,
	( var(V) ->
	    % `not F =\= 0' is like `F =:= 0' except it fails for bounded reals
	    % which span 0 rather than setting up a delayed goal.
	    ( not F =\= 0 ->
		collapse_terms(Xs, Res)
	    ;
		collapse_terms1(X, Xs, Res)
	    )
	; number(V) ->
	    collapse_terms1(X, Xs, Res)
	;
	    writeln(error, "Non-numeric term in linrenorm/2"),
	    abort 
	).

collapse_terms1(Any, [], [Any]).
collapse_terms1(F1V1, [F2V2|More], Res) :-
	F2V2 = F2*V2,	% writing these 2 unification in the other order
	F1V1 = F1*V1,	% triggers a compiler bug (indexing on arg 1...)
	( number(V1), number(V2) ->
	    F is F1*V1+F2*V2,
	    collapse_terms(F*1, More, Res)
	; V1 == V2 ->
	    F is F1+F2,
	    collapse_terms(F*V1, More, Res)
	;
	    Res = [F1V1|Res0],
	    collapse_terms(F2V2, More, Res0)
	).


% ----------------------------------------------------------------------
% Normal form -> standard expressions
% ----------------------------------------------------------------------

delinearize(Norm, Expr) :-
	delinearize(Norm, 0, Expr).

delinearize([], Expr, Expr).
delinearize([F*X|Ts], Expr0, Expr) :-
	( Expr0 == 0 ->
	    Fact = F,   Expr1 = Term
	; sgn(F, -1) ->
	    Fact is -F, Expr1 = Expr0 - Term
	;
	    Fact = F,   Expr1 = Expr0 + Term
	),
	( nonvar(X) ->
	    Term is Fact * X
	; not Fact =\= 1 ->
	    Term = X
	;
	    Term = Fact*X
	),
	delinearize(Ts, Expr1, Expr).


% ----------------------------------------------------------------------

difflistify([], DXs, DXs).
difflistify([X|Xs], [X|DXs], DXs0) :-
	difflistify(Xs, DXs, DXs0).



% ----------------------------------------------------------------------
% Normalizer for polynomials
%
% A monomial is a list of constants and variables and represents
% the product of all the list elements.
% In a normalised monomial, the list is sorted, the first element is
% the (only) constant and the others are variables.
%
% A normalised polynomial is represented as a list of lists of
% normalised monomials. The sublists represent groups of monomials
% of the same degree in ascending order. If there are no monomials
% for a certain degree, the list element is missing:
%
% [ConstantMonos, LinearMonos, QuadraticMonos, CubicMonos, ...]
%
% In a normalised polynomial, all monomials are normalised and
% all monomials with identical variables are merged.
%
% Non-polynomial components are factored out by introducing an
% auxiliary variable and adding a term Aux=NonPolyExpr to the
% NonPoly result list.
%
% We use an intermediate form called sum_of_prods which is just a
% list (sum) of lists (products of numbers and vars) and not further
% normalised.
% ----------------------------------------------------------------------

:- export polynorm/3.
:- export polyrenorm/2.
:- export polydenorm/2.

:- comment(polynorm/3, [
    summary:"Extracts and normalises the polynomial part of an arithmetic expression",
    amode:polynorm(?,-,-),
    args:["Expression":"Arithmetic expression with constants and variables",
	"NormPoly":"Normalized polynomial",
	"Residue":"Residual nonpolynomial components in the form AuxVar=Expr"],
    see_also:[polydenorm/2,polyrenorm/2,linearize/3],
    desc:html("\
	This predicate converts a general arithmetic expression into a
	normalized polynomial representation and possibly a nonpolynomial
	residue. The normalized polynomial representation is a follows:
<P>
	A <EM>monomial</EM> is a list of constants and variables and represents
	the product of all the list elements.
	In a <EM>normalised monomial</EM>, the list is sorted, the first element is
	the (only) constant and the others are variables.
<P>
	A <EM>normalised polynomial</EM> is represented as a list of lists of
	normalised monomials. The sublists represent groups of monomials
	of the same degree in ascending order. If there are no monomials
	for a certain degree, the list element is missing:
<PRE>
	[ConstantMonos, LinearMonos, QuadraticMonos, CubicMonos, ...]
</PRE>
	In a normalised polynomial, all monomials are normalised and
	all monomials with identical variables are merged.
<P>
	Non-polynomial components are factored out by introducing an
	auxiliary variable and adding a term Aux=NonPolyExpr to the
	Residue result list.  All variables within Expression (which
	are free at normalization time) are taken to be numerical
	variables.  If you intend to have variables which can be bound
	to symbolic expressions rather than number, they must be
	wrapped into an eval/1 functor.
    "),
    eg:"
    ?- polynorm(2*5 + 3*(X+5*Y+7)*Z, Poly, Res).
    X = X
    Y = Y
    Z = Z
    Poly = [[[10]], [[21, Z]], [[3, X, Z], [15, Y, Z]]]
    Res = []
    yes.

    ?- polynorm(3*(X+Y),  Poly, Res).
    X = X
    Y = Y
    Poly = [[[3, X], [3, Y]]]
    Res = []
    yes.

    ?- polynorm(3, Poly, Res).
    Poly = [[[3]]]
    Res = []
    yes.
    "
]).

:- comment(polyrenorm/2, [
    summary:"Renormalize a polynomial form",
    amode:polyrenorm(+,-),
    args:["PolyOld":"Possibly denormal polynomial form",
	"PolyNew":"Normalized polynomial form"],
    see_also:[polynorm/3,polydenorm/2],
    desc:html("\
	See polynorm/3 for the definition of the polynomial form.
	Such a form can become denormalized due to unifications
	(instantiation or variable-variable aliasing). This predicate
	renormalizes it.
    "),
    eg:"
    ?- polynorm(3*(X+Y),  Poly1, []), writeln(Poly1),
	Y = 3,
	polyrenorm(Poly1, Poly2), writeln(Poly2).

    [[[3, X], [3, Y]]]
    [[[9]], [[3, X]]]
    "
]).

:- comment(polydenorm/2, [
    summary:"Convert a polynomial form back to a standard arithmetic expression",
    amode:polydenorm(?,-),
    args:[
	"NormPoly":"Normalized polynomial form",
	"Expression":"Arithmetic expression with constants and variables"],
    see_also:[polyrenorm/2,polynorm/3],
    eg:"
    ?- polynorm(2*5 + 3*(X+5*Y+7)*Z, Poly, []), polydenorm(Poly, Expr).
    X = X
    Y = Y
    Z = Z
    Poly = [[[10]], [[21, Z]], [[3, X, Z], [15, Y, Z]]]
    Expr = 10 + 21 * Z + 3 * X * Z + 15 * Y * Z
    yes.
    "
]).

polynorm(Expr, NormPoly, NonPoly) :-
	sum_of_prods(Expr, SumOfProds, [], NonPoly, []),
	sum_of_prods_to_poly(SumOfProds, NormPoly).


	% renormalise a polynomial that may have become
	% denormal by instantiation and bindings
polyrenorm(FormerNormPoly, NormPoly) :-
	(
	    foreach(Group, FormerNormPoly),
	    fromto(MessyPoly, MessyPoly1, MessyPoly2, [])
	do
	    append(Group, MessyPoly2, MessyPoly1)
	),
	sum_of_prods_to_poly(MessyPoly, NormPoly).

% ----------------------------------------------------------------------

:- export quadnorm/6.

:- comment(quadnorm/6, [
    summary:"Extracts constant, linear and quadratic part of an arithmetic expression",
    amode:quadnorm(?,-,-,-,-,-),
    args:["Expression":"Arithmetic expression with constants and variables",
	"Const":"Variable or number",
	"Linear":"Variable or normalized linear polynomial",
	"Quadratic":"Variable or normalized quadratic polynomial",
	"PolyRes":"Variable or normalized superquadratic polynomial",
	"Residue":"Residual nonpolynomial components in the form AuxVar=Expr"],
    see_also:[polynorm/3,linearize/3],
    desc:html("\
	This predicate is a simplified interface to polynorm/3 for the case
	where one is only interested in linear and quadratic components.
	See polynorm/3 for details.
    "),
    eg:"
    ?- quadnorm(2*5 + 3*(X+5*Y+7)*Z, Const, Lin, Quad, Poly, Res).
    X = X
    Y = Y
    Z = Z
    Const = 10
    Lin = [[21, Z]]
    Quad = [[3, X, Z], [15, Y, Z]]
    Poly = []
    Res = []
    yes.
    "
]).

quadnorm(Expr, Const, Lin, Quad, RestPoly, NonPoly) :-
	polynorm(Expr, Poly, NonPoly),
	extract_const(Poly, Const, Poly1),
	extract_lin(Poly1, Lin, Poly2),
	extract_quad(Poly2, Quad, RestPoly).

    extract_const([[[Const]]|RestPoly], Const, RestPoly) :- !.
    extract_const(RestPoly, 0, RestPoly).

    extract_lin([CXs|RestPoly], CXs, RestPoly) :- CXs = [[_,_]|_], !.
    extract_lin(RestPoly, [], RestPoly).

    extract_quad([CXYs|RestPoly], CXYs, RestPoly) :- CXYs = [[_,_,_]|_], !.
    extract_quad(RestPoly, [], RestPoly).

% ----------------------------------------------------------------------

	% sum_of_prods(?Expr, -SumOfProds, +..., -NonPoly, +...)
	% Flatten an expression into a (additive) list of
	% (multiplicative) lists of numbers and variables.
	% variables and constants are mixed and may be repeated.
	% No arithmetic operations are performed in here!
:- mode sum_of_prods(?, -, +, -, +).
sum_of_prods(X, [[X]|P0], P0, R, R) :- (var(X);number(X)), !.
sum_of_prods(E1+E2, P, P0, R, R0) :- !,
	sum_of_prods(E1, P, P1, R, R1),
	sum_of_prods(E2, P1, P0, R1, R0).
sum_of_prods(E1-E2, P, P0, R, R0) :- !,
	sum_of_prods(E1, P, P1, R, R1),
	sum_of_prods((-1)*E2, P1, P0, R1, R0).
sum_of_prods(+E, P, P0, R, R0) :- !,
	sum_of_prods(E, P, P0, R, R0).
sum_of_prods(-E, P, P0, R, R0) :- !,
	sum_of_prods(-1*E, P, P0, R, R0).
sum_of_prods(sum(Es), P, P0, R, R0) :- !,
	sum_list(Es, P, P0, R, R0).
sum_of_prods(EX*EY, P, P0, R, R0) :- !,
	( list_times_list(EX, EY, P, P0, R, R0) ->
	    true
	;
	    sum_of_prods(EX, PX, [], R, R1),
	    sum_of_prods(EY, PY, [], R1, R0),
	    poly_times_poly(PX, PY, P, P0)
	).
sum_of_prods(EX^N, P, P0, R, R0) :- integer(N), !,
	sum_of_prods(EX, PX, [], R, R0),
	poly_power(PX, N, P, P0).
sum_of_prods(lin(Lin), P, P0, R, R0) :- !,
	R = R0,
	( foreach(C*V, Lin), fromto(P, [[C,V]|P1], P1, P0) do true ).
sum_of_prods(E, P, P0, R, R0) :- E = eval(Expr), !,
	( var(Expr) ->
	    P = [[Aux]|P0], R = [Aux=E|R0]
	;
	    sum_of_prods(Expr, P, P0, R, R0)
	).
sum_of_prods(E, P, P0, R, R0) :- E =  subscript(Array,Index), !,
	( nonground(Index) ->
	    P = [[Aux]|P0], R = [Aux=E|R0]
	;
	    subscript(Array,Index,Elem),
	    sum_of_prods(Elem, P, P0, R, R0)
	).
sum_of_prods(E, [[Aux]|P0], P0, [Aux=E|R0], R0) :-
	nonground(E), !.
sum_of_prods(E, [[C]|P0], P0, R, R) :-
	% ground(E),
	C is E.

    poly_times_poly([], _MonoYs, P, P).
    poly_times_poly([MonoX|MonoXs], MonoYs, P, P0) :-
	(
	    foreach(MonoY, MonoYs),
	    fromto(P, [MonoXY|MonoXYs], MonoXYs, P1),
	    param(MonoX)
	do
	    append(MonoX, MonoY, MonoXY)	% MonoX*MonoY
	),
	poly_times_poly(MonoXs, MonoYs, P1, P0).

    sum_list(List, P, P0, R, R0) :- var(List), !,
    	P = [[Aux]|P0], R = [Aux=sum(List)|R0].
    sum_list([], P, P0, R, R0) :- !, P=P0, R=R0.
    sum_list([X|Xs], P, P0, R, R0) :- !,
	sum_of_prods(X, P, P1, R, R1),
	sum_list(Xs, P1, P0, R1, R0).
    sum_list(List, P, P0, R, R0) :- List = subscript(Array,Index), !,
    	( nonground(Index) ->
	    P = [[Aux]|P0], R = [Aux=sum(List)|R0]
	;
	    subscript(Array, Index, Elems),
	    sum_list(Elems, P, P0, R, R0)
	).
    sum_list(X, _P, _P0, _R, _R0) :-
	error(5, _ is sum(X)).

    list_times_list([], [], P, P0, R, R0) ?- P=P0, R=R0.
    list_times_list([X|Xs], [Y|Ys], P, P0, R, R0) ?-
	sum_of_prods(X, PX, [], R, R2),
	sum_of_prods(Y, PY, [], R2, R1),
	poly_times_poly(PX, PY, P, P1),
	list_times_list(Xs, Ys, P1, P0, R1, R0).

    poly_power(Poly, 1, P, P0) :- !,
	append(Poly, P0, P).
    poly_power(Poly, N, P, P0) :-
    	N1 is N//2, N2 is (N+1)//2,	% N =:= N1+N2
	poly_power(Poly, N1, P1, []),
	poly_power(Poly, N2, P2, []),
	poly_times_poly(P1, P2, P, P0).

%----------------------------------------------------------------------

sum_of_prods_to_poly(MessyPoly, NormPoly) :-
	(
	    foreach(Xs,MessyPoly),
	    foreach([Cnew|VarXs],NormMonos)
	do
	    sort(0, >=, Xs, XsSorted),	% constants first, then variables
	    separate_constants(XsSorted, 1, Cnew, VarXs)
	),
	split_by_degree(NormMonos, NonUniqueMonosByDegree),
	( foreach(NonUniqueMonosSameDegree,NonUniqueMonosByDegree),
	  foreach(NormMonosSameDegree, NormPoly)
	do
	    sort(2, =<, NonUniqueMonosSameDegree, SortedMonos),
	    collapse_monos(SortedMonos, NormMonosSameDegree)
	).


% separate_constants(+XsSorted, +Cin, -Cout, -VarXs)
% XsSorted is a list with numbers at the beginning,
% which are all multiplied and returned as Cout,
% the rest list is returned as VarXs
separate_constants([], C, C, []).
separate_constants(XsSorted, C, Cnew, VarXs) :-
	XsSorted = [X|Xs],
	( number(X) ->
	    C1 is C*X,
	    separate_constants(Xs, C1, Cnew, VarXs)
	;
	    Cnew = C, VarXs = XsSorted
	).
	    
degree_sort(Ms, SMs) :-
	( foreach(M,Ms), foreach(N-M,NMs) do length(M,N) ),
	keysort(NMs, SNMs),
	( foreach(M,SMs), foreach(_-M,SNMs) do true ).

split_by_degree(Ms, SMs) :-
	( foreach(M,Ms), foreach(N-M,NMs) do length(M,N) ),
	keysort(NMs, SNMs),
	group_same_key_values(SNMs, GSNMs),
	( foreach(_-SM,GSNMs), foreach(SM,SMs) do true ).
%	strip_key_and_fill_gaps(GSNMs, SMs, 1).

group_same_key_values([], []).
group_same_key_values([K-V|List], [K-[V|KVs]|GroupedList]) :-
	group_same_key_values(List, K, KVs, GroupedList).

    group_same_key_values([], _, [], []).
    group_same_key_values([K-V|List], K, [V|KVs], GroupedList) :- !,
	group_same_key_values(List, K, KVs, GroupedList).
    group_same_key_values([K-V|List], _K, [], [K-[V|KVs]|GroupedList]) :-
	group_same_key_values(List, K, KVs, GroupedList).

    strip_key_and_fill_gaps([], [], _N).
    strip_key_and_fill_gaps(KVsKVs, SVs, N) :-
	KVsKVs = [K-Vs|KVs],
	N1 is N+1,
    	( K > N ->
	    SVs = [[]|SVs0],	% missing degree - insert a filler
	    strip_key_and_fill_gaps(KVsKVs, SVs0, N1)
	;
	    SVs = [Vs|SVs0],
	    strip_key_and_fill_gaps(KVs, SVs0, N1)
	).


collapse_monos([], []).
collapse_monos([Mono|Monos], NormPoly) :-
	collapse_monos(Mono, Monos, NormPoly).

    collapse_monos(M, [], [M]).
    collapse_monos(Mono1, [Mono2|Monos], NormPoly) :-
	Mono2 = [C2|X2s],
	Mono1 = [C1|X1s],
	( X1s == X2s ->
	    C is C1+C2,
	    collapse_monos([C|X1s], Monos, NormPoly)
	;
	    NormPoly = [Mono1|NormPoly1],
	    collapse_monos(Mono2, Monos, NormPoly1)
	).


%----------------------------------------------------------------------

polydenorm([], 0).
polydenorm([[]|Degrees], Expr) :- !,
	polydenorm(Degrees, Expr).
polydenorm([[[C0|Xs0]|RestDegree0]|Degrees], Expr) :-
	list_to_times(Xs0, C0, Prod0),
	(
	    foreach(Degree, [RestDegree0|Degrees]),
	    fromto(Prod0, Expr1, Expr4, Expr)
	do
	    (
	    	foreach([C|Xs], Degree),
		fromto(Expr1, Expr2, Expr3, Expr4)
	    do
		% The following test avoids a potential delayed goal if C
		% is a bounded real.
		( not C > 0 ->
		    CNeg is -C,
		    list_to_times(Xs, CNeg, Prod),
		    Expr3 = (Expr2 - Prod)
		;
		    list_to_times(Xs, C, Prod),
		    Expr3 = (Expr2 + Prod)
		)
	    )
	).


    list_to_times([], X0, X0).
    list_to_times([X|Xs], X0, Expr) :-
	( X0 == 1 ->
	    list_to_times(Xs, X, Expr)
	;
	    list_to_times(Xs, X0*X, Expr)
	).



% ----------------------------------------------------------------------
% Normalizer for arbitrary arithmetic expressions
%
% Given an expression, returns the generalised normalisation which is defined
% to be a record with the following fields
%
%  Note: There are many possible 'normal' forms of an expression, this function
%        returns just one of them.
% ----------------------------------------------------------------------
:-export normalize/3.
normalize( _Expr, _Lin, _Constraints ) :-
	true.
%----------------------------------------------------------------------
% Simplifies an expression, by reducing the number of variable references
%----------------------------------------------------------------------
:-export simplify/2.
simplify(E,E):-
	var(E),!.
simplify(C0-C1,C2):-
	number(C0),number(C1),C2 is C0-C1,!.
simplify(C0/C1,C2):-
	number(C0),number(C1),C2 is C0/C1,!.

% Addition
simplify(C0+C1,C2):-
	number(C0),number(C1),C2 is C0+C1,!.
simplify(C+V,lin([C*1,1*V])):-
	number(C),var(V),!.
simplify(V+C,lin([C*1,1*V])):-
	number(C),var(V),!.
simplify(V0+V1,lin([0*1,1*V0,1*V1])):-
	var(V0),var(V1),!.
simplify(C+lin([K0*1|Tail]),lin([K1*1|Tail])):-
	number(C),K1 is K0 + C,!.
simplify(V+lin([K0*1|Tail1]),lin([K0*1|Tail2])):-
	var(V),
	simplify_merge([1*V],Tail1,Tail2),
	!.
simplify(lin([K0*1|Tail])+C,lin([K1*1|Tail])):-
	number(C),K1 is K0 + C,!.
simplify(lin([K0*1|Tail1])+V,lin([K0*1|Tail2])):-
	var(V),
	simplify_merge([1*V],Tail1,Tail2),
	!.
simplify(lin([K0*1|Tail0])+lin([K1*1|Tail1]),lin([K2*1|Tail2])):-
	K2 is K0 + K1,
	simplify_merge(Tail0,Tail1,Tail2),
	!.
simplify(E1+E2,ExprOut):-
	simplify(E1,E3),
	simplify(E2,E4),
	simplify(E3+E4,ExprOut),!.

% multiplication
simplify(C0*C1,C2):-
	number(C0),number(C1),C2 is C0*C1,!.
simplify(C*V,lin([0*1,C*V])):-
	number(C),var(V),!.
simplify(V*C,lin([0*1,C*V])):-
	number(C),var(V),!.
simplify(V0*V1,poly([V0,V1])):-
	var(V0),var(V1),!.
simplify(C*lin(Lin0), lin(Lin1)):-
	number(C),
	(foreach(C0*V0,Lin0), param(C), fromto(Lin1,Out,In,[]) do
	    C1 is C0*C,
	    Out = [C1*V0|In]
	),!.
simplify(E1*E2,ExprOut):-
	simplify(E1,E3),
	writeq(E1),write('simp-to->'),writeq(E3),nl,
	simplify(E2,E4),
	writeq(E2),write('simp-to->'),writeq(E4),nl,
	simplify(E3*E4,ExprOut),
	writeq(E3*E4),write('simp-to->'),writeq(ExprOut),nl,
	!.

% catch all
simplify(E, E):-!.


% merge lists of ordered terms together maintaining the order
simplify_merge([],Acc,Acc):-!.
simplify_merge(Acc,[],Acc):-!.
simplify_merge([C0*V0|T0],[C1*V1|T1],[C2*V2|Acc0]):-
	(V0 == V1 ->
	    C2 is C0+C1,
	    V2 = V0,
	    simplify_merge(T0,T1,Acc0)
	;
	    (V0 @< V1 ->
		C2 is C0,
		V2 = V0,
		simplify_merge(T0,[C1*V1|T1],Acc0)
	    ;
		C2 is C1,
		V2 = V1,
		simplify_merge([C0*V0|T0],T1,Acc0)
	    )
	).

%----------------------------------------------------------------------
% Test code
%----------------------------------------------------------------------


%ex(3).
%ex(X).
%ex(3*X).
%ex(X*3).
%ex(4*3).
%ex(3*X+4*Y).
%ex(3*(X+Y)).

%ex(3*(X+5*Y)).
/*
ex(3*(X+5*Y+7)*Z).
ex(2*5 + 3*(X+5*Y+7)*Z).
ex(2*5 + 3*(X+5*(Z+7)/(3*X + 4*Y))*Z).

test :-
	ex(Expr0),
	nl,
	nl,
	writeq(Expr0),nl,
	simplify(Expr0,Expr),
	write('- simplifies to ->'), write(Expr), nl,
	%polynorm(Expr, Norm, _NonPoly),
	%writeq(Norm), nl,
	%polydenorm(Norm, NormExpr),
	%writeq(NormExpr), nl,
	fail.
*/
%:-test.

