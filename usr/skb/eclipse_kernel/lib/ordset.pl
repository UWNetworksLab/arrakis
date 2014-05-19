% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: ordset.pl,v 1.1 2008/06/30 17:43:48 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% $Id: ordset.pl,v 1.1 2008/06/30 17:43:48 jschimpf Exp $
%
% IDENTIFICATION:       ordset.pl
%
% AUTHOR:		Joachim Schimpf, IC-Parc, Imperial College, London
%
% DESCRIPTION:
%
%	This is a drop-in replacement for R.A.O'Keefe's ordset library.
%	The predicates have all been rewritten for optimal indexing
%	and garbage avoidance when used within ECLiPSe. I have kept
%	the original interface and comments.
%
%	Efficiency note: In the cases where a 2 or 3-way comparison is
%	needed, I have used two comparisons (==, @> or @<) instead of
%	a single compare/3 + test. In ECLiPSe (at least up to 5.9) this
%	is significantly more efficient when the set elements are simple
%	(obviously, with sufficiently complex set elements, the double
%	comparison will be slower than a single one, but many common
%	uses are with simple elements).
%	With a new compiler, this observation may no longer hold, so the
%	decision should then be reviewed.


%   File   : ORDSET.PL
%   Author : R.A.O'Keefe
%   Updated: 22 May 1983
%   Purpose: Ordered set manipulation utilities

%   In this module, sets are represented by ordered lists with no
%   duplicates.  Thus {c,r,a,f,t} would be [a,c,f,r,t].  The ordering
%   is defined by the @< family of term comparison predicates, which
%   is the ordering used by sort/2 and setof/3.

%   The benefit of the ordered representation is that the elementary
%   set operations can be done in time proportional to the Sum of the
%   argument sizes rather than their Product.  Some of the unordered
%   set routines, such as member/2, length/2,, select/3 can be used
%   unchanged.  The main difficulty with the ordered representation is
%   remembering to use it!


:- module(ordset).

:- comment(summary, "Ordered set manipulation utilities").
:- comment(author, "R.A.O'Keefe and Joachim Schimpf").
:- comment(copyright, 'This file is in the public domain').
:- comment(date, "$Date: 2008/06/30 17:43:48 $").
:- comment(desc, html("<P>\
	In this module, sets are represented by ordered lists with no
	duplicates.  Thus the set {c,r,a,f,t} would be [a,c,f,r,t].  The
	ordering is defined by the @< family of term comparison predicates,
	which is the ordering used by sort/2 and setof/3.
	</P><P>
	The benefit of the ordered representation is that the elementary
	set operations can be done in time proportional to the Sum of
	the argument sizes rather than their Product.  Some of the
	unordered set routines, such as member/2, length/2, select/3
	can be used unchanged.
	</P><P>
	The implementation allows nonground set elements. The only problem
	with this is that a set can lose its set property as a result of
	variable bindings: unifications can create duplicates or change
	the element's position in the term order. The set can be repaired
	by applying list_to_ord_set/2 again, i.e. re-sorting it.
	</P><P>
	Note that the predicates of this library do no error checking.
	When called with the wrong argument types or modes, the result is
	undefined.
	</P>
    ")).


% Compatibility:
%	P	O'Keefe's public domain library 1983
%	Q	Quintus
%	S	SICStus
%	K	O'Keefe's standard library draft 2005
%		(http://www.cs.otago.ac.nz/staffpriv/ok/pllib.htm)

:- export			% Signature			Compatibility
	is_ordset/1,		%  List ->			- Q S K
	list_to_ord_set/2,	%  List -> Set			P Q S K
	ord_add_element/3,	%  Set x Elem -> Set		- Q S -
	ord_compare/3,		%  Set x Set -> Rel		- - - K
	ord_del_element/3,	%  Set x Elem -> Set		- Q S -
	ord_disjoint/2,		%  Set x Set ->			P Q S K
	ord_disjoint_union/3,	%  Set x Set -> Set		- - - K
	ord_insert/3,		%  Set x Elem -> Set		P - - -
	ord_intersect/2,	%  Set x Set ->			P Q S K
	ord_intersect/3,	%  Set x Set -> Set		P - - -
	ord_intersection/2,	%  Sets -> Set			- Q S K
	ord_intersection/3,	%  Set x Set -> Set		- Q S K
%	ord_intersection/4,	%  Set x Set -> Set x Set	- - S -
	ord_intersection/5,	%  Set x Set -> Set x Set x Set	- - - -
	ord_memberchk/2,	%  Elem x Set ->		- - - K
	ord_nonmember/2,	%  Elem x Set ->		- - - K
	ord_proper_subset/2,	%  Set x Set ->			- - - K
	ord_proper_superset/2,	%  Set x Set ->			- - - K
	ord_selectchk/3,	%  Elem x Set -> Set		- - - K
	ord_seteq/2,		%  Set x Set ->			P Q S -
%	ord_setproduct/3,	%  Set x Set -> List		- P S -
	ord_subset/2,		%  Set x Set ->			P Q S K
	ord_subtract/3,		%  Set x Set -> Set		P Q S K
	ord_superset/2,		%  Set x Set ->			- - - K
	ord_symdiff/3,		%  Set x Set -> Set		P Q S K
	ord_union/2,		%  Sets -> Set			- Q S K
	ord_union/3,		%  Set x Set -> Set		P Q S K
	ord_union/4.		%  Set x Set -> Set x Set	- Q S K



:- comment(list_to_ord_set/2, [
    amode:(list_to_ord_set(+,-) is det),
    args:["List":"A list of terms", "Set":"A set or variable"],
    summary:"Converts a list to a set",
    see_also:[sort/2],
    desc:html("\
	Succeeds when Set is the ordered representation of the set
	represented by the unordered representation List.  The only
	reason for giving it a name at all is that you may not have
	realised that sort/2 could be used this way."
    )]).

:- mode list_to_ord_set(+, ?).
list_to_ord_set(List, Set) :-
	sort(0, <, List, Set).


:- comment(is_ordset/1, [
    amode:(is_ordset(?) is semidet),
    args:["Term":"Any Term"],
    summary:"Checks whether term is an ordered set in the sense of lib(ordset)",
    see_also:[@< /2],
    desc:html("\
	Checks whether term is an ordered set in the sense of lib(ordset),
	i.e. a proper, duplicate-free list with elements in increasing order.
    ")]).

is_ordset([]) ?- true.
is_ordset([X|Xs]) ?-
	is_ordset(X, Xs).

    is_ordset(_, []) ?- true.
    is_ordset(X1, [X2|Xs]) ?-
    	X1 @< X2,
	is_ordset(X2, Xs).


:- comment(ord_memberchk/2, [
    amode:(ord_memberchk(?,+) is semidet),
    args:["Term":"Any Term", "Set":"A set"],
    summary:"Checks whether Term is a member of Set",
    see_also:[member/2,ord_nonmember/2,ord_selectchk/3],
    fail_if:"Term is not a member of Set"
    ]).

:- mode ord_memberchk(?,+).
ord_memberchk(X, [Y|Ys]) :-
	( X @> Y ->
	    ord_memberchk(X, Ys)
	;
	    X == Y	% a la O'Keefe, but different from memberchk/2!
	).


:- comment(ord_del_element/3, [
    amode:(ord_del_element(+,?,-) is det),
    args:["Set":"A set", "Term":"Any Term", "Remainder":"A variable or set"],
    summary:"Remainder is the set Set without the element Term",
    see_also:[ord_selectchk/3],
    desc:html("\
	Remainder is the set Set without the element Term. If Term is not
	in Set, Remainder is identical to Set.")
    ]).

:- mode ord_del_element(+,?,?).
ord_del_element([], _, []).
ord_del_element(YYs, X, Rs) :-
	YYs = [Y|Ys],
	( X @> Y ->
	    Rs = [Y|Rs0],
	    ord_del_element(Ys, X, Rs0)
	; X == Y ->
	    Rs = Ys
	;
	    Rs = YYs
	).


:- comment(ord_selectchk/3, [
    amode:(ord_selectchk(?,+,-) is semidet),
    args:["Term":"Any Term", "Set":"A set", "Remainder":"A variable or set"],
    summary:"Set contains Term, and Remainder is the set Set without Term",
    fail_if:"Term is not a member of Set",
    see_also:[ord_del_element/3, delete/3],
    desc:html("\
	If Set contains Term, Remainder is the set Set without the Term.
	Otherwise the predicate fails.")
    ]).

:- mode ord_selectchk(?,+,?).
ord_selectchk(X, [Y|Ys], Rs) :-
	( X @> Y ->
	    Rs = [Y|Rs0],
	    ord_selectchk(X, Ys, Rs0)
	;
	    X == Y,
	    Rs = Ys
	).


:- comment(ord_nonmember/2, [
    amode:(ord_nonmember(?,+) is semidet),
    args:["Term":"Any Term", "Set":"A set"],
    summary:"Term is not a member of Set",
    see_also:[ord_memberchk/2],
    fail_if:"Term is a member of Set"
    ]).

:- mode ord_nonmember(?,+).
ord_nonmember(_X, []).
ord_nonmember(X, [Y|Ys]) :-
	( X @> Y ->
	    ord_nonmember(X, Ys)
	;
	    X \== Y	% a la O'Keefe, but different from nonmember/2!
	).


:- comment(ord_compare/3, [
    amode:(ord_compare(-,+,+) is semidet),
    args:["Rel":"A variable or an atom", "Set1":"A set", "Set2":"A set"],
    summary:"Rel is the ordering relationship between Set1 and Set2",
    see_also:[@< /2, ord_seteq/2, ord_proper_subset/2, ord_proper_superset/2],
    fail_if:"Fails if the sets are not comparable",
    desc:html("\
	Rel is the ordering relationship between Set1 and Set2.
	Rel is one of the atoms =, &gt; or &lt;
	<PRE>
	=    The sets are identical (in the sense of ==/2)
	&gt;    Set1 is a proper superset of Set2
	&lt;    Set1 is a proper subset of Set2
	</PRE>
	Otherwise the predicate fails.")
    ]).

:- mode ord_compare(?,+,+).
ord_compare(R, Xs, Ys) :-
	ord_compare1(R, Xs, Ys, =).

    :- mode ord_compare1(?, +, +, +).
    ord_compare1(R, [], [], RSofar) :- !,
    	R = RSofar.
    ord_compare1(R, [], [_|_], RSofar) :- !,
	RSofar \== (>), R = (<).
    ord_compare1(R, [_|_], [], RSofar) :- !,
	RSofar \== (<), R = (>).
    ord_compare1(R, XXs, YYs, RSofar) :-
	YYs = [Y|Ys],
	XXs = [X|Xs],
	( X == Y ->
	    ord_compare1(R, Xs, Ys, RSofar)
	; X @< Y ->
	    RSofar \== (<),
	    ord_compare1(R, Xs, YYs, >)
	;
	    RSofar \== (>),
	    ord_compare1(R, XXs, Ys, <)
	).


:- comment(ord_disjoint/2, [
    amode:(ord_disjoint(+,+) is semidet),
    args:["Set1":"A set", "Set2":"A set"],
    summary:"Checks whether two sets are disjoint",
    see_also:[ord_intersect/2],
    desc:html("\
	Succeeds when the two ordered sets have no element in common.
    ")]).

:- mode ord_disjoint(+,+).
ord_disjoint([], _).
ord_disjoint(S1, S2) :-
	S1=[_|_],
	ord_disjoint1(S1, S2).

:- mode	ord_disjoint1(+, +).
ord_disjoint1(_, []).
ord_disjoint1(EL1, EL2) :-
	EL2 = [E2|L2],
	EL1 = [E1|L1],
	( E1 @< E2 ->
	    ord_disjoint(L1, EL2)
	;
	    E1\==E2,
	    ord_disjoint(EL1, L2)
	).



:- comment(ord_insert/3, [
    amode:(ord_insert(+,+,-) is det),
    args:["Set1":"A set", "Element":"A term", "Set2":"A set or variable"],
    summary:"Adds an element to a set",
    see_also:[ord_add_element/3],
    desc:html("\
	Set2 is the set resulting from adding Element to Set1. It should
	give exactly the same result as merge(Set1, [Element], Set2).
	This is a synonym of ord_add_element/3.
    ")]).

:- comment(ord_add_element/3, [
    amode:(ord_add_element(+,+,-) is det),
    args:["Set1":"A set", "Element":"A term", "Set2":"A set or variable"],
    summary:"Adds an element to a set",
    see_also:[ord_insert/3],
    desc:html("\
	Set2 is the set resulting from adding Element to Set1. It should
	give exactly the same result as merge(Set1, [Element], Set2).
	This is a synonym of ord_insert/3.
    ")]).

:- mode	ord_add_element(+, ?, ?).
ord_add_element(S1, E, S2) :-
	ord_insert(S1, E, S2).

:- mode	ord_insert(+, ?, ?).
ord_insert([], E, [E]).
ord_insert(EL1, E, L) :-
	EL1 = [E1|L1],
	( E1 @< E ->
	    L = [E1|L0],
	    ord_insert(L1, E, L0)
	; E1==E ->
	    L = EL1
	;
	    L = [E|EL1]
	).



:- comment(ord_intersect/2, [
    amode:(ord_intersect(+,+) is semidet),
    args:["Set1":"A set", "Set2":"A set"],
    summary:"Checks whether two sets have a non-empty intersection",
    see_also:[ord_disjoint/2],
    desc:html("\
	Succeeds when the two ordered sets have at least one element
	in common.  Note that the test is == rather than = .
    ")]).

:- mode	ord_intersect(+, +).
ord_intersect(EL1, EL2) :-
	EL2 = [E2|L2],
	EL1 = [E1|L1],
	( E1==E2 ->
	    true
	; E1 @< E2 ->
	    ord_intersect(L1, EL2)
	;
	    ord_intersect(EL1, L2)
	).



:- comment(ord_intersect/3, [
    amode:(ord_intersect(+,+,-) is det),
    args:["Set1":"A set", "Set2":"A set", "Intersection":"A set"],
    summary:"Computes the intersection of two sets",
    see_also:[ord_intersection/3],
    desc:html("<P>\
	Succeeds when Intersection is the intersection of Set1 
	and Set2, provided that Set1 and Set2 are ordered sets.
	</P><P>
	The use of ord_intersection/3 is preferred.
	</P>
    ")]).

:- comment(ord_intersection/3, [
    amode:(ord_intersection(+,+,-) is det),
    args:["Set1":"A set", "Set2":"A set", "Intersection":"A set"],
    summary:"Computes the intersection of two sets",
    see_also:[ord_intersect/2, ord_intersection/5],
    desc:html("\
	Succeeds when Intersection is the intersection of Set1 
	and Set2, provided that Set1 and Set2 are ordered sets.
    ")]).

:- mode	ord_intersect(+, +, ?).
ord_intersect(S1, S2, S) :-
	ord_intersection(S1, S2, S).

:- mode	ord_intersection(+, +, ?).
ord_intersection([], _, []).
ord_intersection(S1, S2, L) :-
	S1=[_|_],
	ord_intersection1(S1, S2, L).

:- mode	ord_intersection1(+, +, ?).
ord_intersection1(_, [], []).
ord_intersection1(EL1, EL2, L) :-
	EL2 = [E2|L2],
	EL1 = [E1|L1],
	( E1==E2 ->
	    L = [E1|L0],
	    ord_intersection(L1, L2, L0)
	; E1 @< E2 ->
	    ord_intersection(L1, EL2, L)
	;
	    ord_intersection1(EL1, L2, L)
	).


:- comment(ord_intersection/5, [
    amode:(ord_intersection(+,+,-,-,-) is det),
    args:["Set1":"A set", "Set2":"A set",
    	"Intersection":"A variable or set",
    	"Only1":"A variable or set",
    	"Only2":"A variable or set"
	],
    summary:"Computes the intersection and the differences of two sets",
    see_also:[ord_intersection/3, ord_subtract/3, ord_symdiff/3],
    desc:html("\
	Succeeds when Intersection is the intersection of the ordered
	sets Set1 and Set2, Only1 is the set of elements that are only
	in Set1, and Only2 is the set of elements that are only in Set2.
    ")]).

% ord_intersection(+Xs, +Ys, -CommonElements, -XRemains, -YRemains)
% computes intersection and remainders of two ordered lists.
:- mode ord_intersection(+,+,?,?,?).
ord_intersection([], Ys, [], [], Ys).
ord_intersection(Xs, Ys, Cs, Ls, Rs) :-
	Xs=[_|_],
	ord_intersection1(Xs, Ys, Cs, Ls, Rs).

    ord_intersection1(Xs, [], [], Xs, []).
    ord_intersection1(XXs, YYs, Cs, Ls, Rs) :-
	YYs = [Y|Ys],
	XXs = [X|Xs],
	( X == Y ->
	    Cs = [X|Cs0],
	    ord_intersection(Xs, Ys, Cs0, Ls, Rs)
	; X @< Y ->
	    Ls = [X|Ls0],
	    ord_intersection(Xs, YYs, Cs, Ls0, Rs)
	;
	    Rs = [Y|Rs0],
	    ord_intersection1(XXs, Ys, Cs, Ls, Rs0)
	).


:- comment(ord_seteq/2, [
    amode:(ord_seteq(+,+) is semidet),
    args:["Set1":"A set", "Set2":"A set"],
    summary:"Compares two sets for identity",
    see_also:[== /2, ord_compare/3],
    desc:html("\
	Succeeds when the two arguments represent the same set.  Since they
	are assumed to be ordered representations, they must be identical.
    ")]).

:- mode	ord_seteq(+, +).
ord_seteq(Set1, Set2) :-
	Set1 == Set2.



:- comment(ord_subset/2, [
    amode:(ord_subset(+,+) is semidet),
    args:["Set1":"A set", "Set2":"A set"],
    summary:"Checks whether Set1 is a subset of Set2",
    see_also:[ord_proper_subset/2, subset/2, ord_compare/3],
    desc:html("\
	Succeeds when every element of the ordered set Set1 appears
	in the ordered set Set2.
    ")]).

:- mode	ord_subset(+, +).
ord_subset([], _).
ord_subset(EL1, EL2) :-
	EL1 = [E1|L1],
	EL2 = [E2|L2],
	( E1==E2 ->
	    ord_subset(L1, L2)
	; E1 @> E2 ->
	    ord_subset(EL1, L2)
	;
	    fail
	).


:- comment(ord_superset/2, [
    amode:(ord_superset(+,+) is semidet),
    args:["Set1":"A set", "Set2":"A set"],
    summary:"Checks whether Set1 is a superset of Set2",
    see_also:[ord_proper_superset/2, subset/2, ord_compare/3],
    desc:html("\
	Succeeds when every element of the ordered set Set2 appears
	in the ordered set Set1.
    ")]).

:- mode	ord_superset(+, +).
ord_superset(S1, S2) :-
	ord_subset(S2, S1).


:- comment(ord_proper_subset/2, [
    amode:(ord_proper_subset(+,+) is semidet),
    args:["Set1":"A set", "Set2":"A set"],
    summary:"Checks whether Set1 is a proper subset of Set2",
    see_also:[ord_subset/2, ord_compare/3],
    desc:html("\
	Succeeds when every element of the ordered set Set1 appears
	in the ordered set Set2, and Set2 has at least on element that
	does not occur in Set1.
    ")]).

:- mode	ord_proper_subset(+, +).
ord_proper_subset([], [_|_]).
ord_proper_subset(EL1, EL2) :-
	EL1 = [E1|L1],
	EL2 = [E2|L2],
	( E1==E2 ->
	    ord_proper_subset(L1, L2)
	; E1 @> E2 ->
	    ord_subset(EL1, L2)
	;
	    fail
	).


:- comment(ord_proper_superset/2, [
    amode:(ord_proper_superset(+,+) is semidet),
    args:["Set1":"A set", "Set2":"A set"],
    summary:"Checks whether Set1 is a proper superset of Set2",
    see_also:[ord_superset/2, ord_compare/3],
    desc:html("\
	Succeeds when every element of the ordered set Set2 appears
	in the ordered set Set1, and Set1 has at least on element that
	does not occur in Set2.
    ")]).

:- mode	ord_proper_superset(+, +).
ord_proper_superset(S1, S2) :-
	ord_proper_subset(S2, S1).


:- comment(ord_subtract/3, [
    amode:(ord_subtract(+,+,-) is det),
    args:["Set1":"A set", "Set2":"A set", "Difference":"A set or variable"],
    summary:"Subtracts Set2 from Set1",
    see_also:[ord_symdiff/3],
    desc:html("\
	Succeeds when Difference contains all and only the elements
	of Set1 which are not also in Set2.
    ")]).

:- mode	ord_subtract(+, +, ?).
ord_subtract([], _, []).
ord_subtract(S1, S2, L) :-
	S1=[_|_],
	ord_subtract1(S1, S2, L).

:- mode	ord_subtract1(+, +, ?).
ord_subtract1(L, [], L).
ord_subtract1(EL1, EL2, L) :-
	EL2 = [E2|L2],
	EL1 = [E1|L1],
	( E1==E2 ->
	    ord_subtract(L1, L2, L)
	; E1 @< E2 ->
	    L = [E1|L0],
	    ord_subtract(L1, EL2, L0)
	;
	    ord_subtract(EL1, L2, L)
	).



:- comment(ord_symdiff/3, [
    amode:(ord_symdiff(+,+,-) is det),
    args:["Set1":"A set", "Set2":"A set", "Difference":"A set or variable"],
    summary:"Computes the symmetric difference of Set1 and Set2",
    see_also:[ord_subtract/3],
    desc:html("\
	Succeeds when Difference is the symmetric difference of Set1 and Set2.
	These are the elements that occur either only in Set1 or only in Set2,
	but not both.
    ")]).

:- mode	ord_symdiff(+, +, ?).
ord_symdiff([], L, L).
ord_symdiff(S1, S2, L) :-
	S1=[_|_],
	ord_symdiff1(S1, S2, L).

:- mode	ord_symdiff1(+, +, ?).
ord_symdiff1(L, [], L).
ord_symdiff1(EL1, EL2, L) :-
	EL2 = [E2|L2],
	EL1 = [E1|L1],
	( E1==E2 ->
	    ord_symdiff(L1, L2, L)
	; E1 @< E2 ->
	    L = [E1|L0],
	    ord_symdiff(L1, EL2, L0)
	;
	    L = [E2|L0],
	    ord_symdiff(EL1, L2, L0)
	).



:- comment(ord_union/3, [
    amode:(ord_union(+,+,-) is det),
    args:["Set1":"A set", "Set2":"A set", "Union":"A set or variable"],
    summary:"Computes the union of Set1 and Set2",
    see_also:[ord_disjoint_union/3, ord_union/4, ord_union/2],
    desc:html("\
	Succeeds when Union is the union of Set1 and Set2.  Note that when
	something occurs in both sets, we want to retain only one copy.
    ")]).

:- mode	ord_union(+, +, ?).
ord_union(Set1, Set2, Union) :-
	merge(0, <, Set1, Set2, Union).


:- comment(ord_union/4, [
    amode:(ord_union(+,+,-,-) is det),
    args:["Set1":"A set", "Set2":"A set", "Union":"A set or variable",
    	"New":"A set or variable"],
    summary:"Computes the union and difference of Set2 and Set1",
    see_also:[ord_disjoint_union/3, ord_union/3, ord_union/2],
    desc:html("\
	Succeeds when Union is the union of Set1 and Set2, and New is the
	set of elements that are in Set2 but not in Set1.  This is useful
	in order to know which elements were newly added when incrementally
	merging sets.
    ")]).

:- mode	ord_union(+, +, ?, ?).
ord_union([], Ys, Ys, Ys).
ord_union(Xs, Ys, Us, Rs) :-
	Xs=[_|_],
	ord_union1(Xs, Ys, Us, Rs).

    ord_union1(Xs, [], Xs, []).
    ord_union1(XXs, YYs, Us, Rs) :-
	YYs = [Y|Ys],
	XXs = [X|Xs],
	( X == Y ->
	    Us = [X|Us0],
	    ord_union(Xs, Ys, Us0, Rs)
	; X @< Y ->
	    Us = [X|Us0],
	    ord_union(Xs, YYs, Us0, Rs)
	;
	    Us = [Y|Us0],
	    Rs = [Y|Rs0],
	    ord_union1(XXs, Ys, Us0, Rs0)
	).


:- comment(ord_disjoint_union/3, [
    amode:(ord_disjoint_union(+,+,-) is semidet),
    args:["Set1":"A set", "Set2":"A set", "Union":"A set or variable"],
    summary:"Computes the union of Set1 and Set2 when they are disjoint",
    see_also:[ord_union/4, ord_union/2],
    fail_if:"Fails if the sets are not disjoint",
    desc:html("\
	Succeeds when Union is the union of disjoint sets Set1 and Set2.
	Fails if Set1 and Set2 are not disjoint, i.e. if they have a
	non-empty intersection.
    ")]).

:- mode	ord_disjoint_union(+, +, ?).
ord_disjoint_union([], Ys, Ys).
ord_disjoint_union(Xs, Ys, Us) :-
	Xs=[_|_],
	ord_disjoint_union1(Xs, Ys, Us).

    ord_disjoint_union1(Xs, [], Xs).
    ord_disjoint_union1(XXs, YYs, Us) :-
	YYs = [Y|Ys],
	XXs = [X|Xs],
	( X @< Y ->
	    Us = [X|Us0],
	    ord_disjoint_union(Xs, YYs, Us0)
	;
	    X @> Y,		% fail if not disjoint
	    Us = [Y|Us0],
	    ord_disjoint_union1(XXs, Ys, Us0)
	).


:- comment(ord_union/2, [
    amode:(ord_union(+,-) is det),
    args:["Sets":"A list of sets", "Union":"A set or variable"],
    summary:"Computes the union of all sets in Sets",
    see_also:[ord_union/3, ord_intersection/2],
    desc:html("\
	Succeeds when Union is the union of all sets in the list Sets.
    ")]).

:- mode ord_union(+,?).
ord_union([], []).
ord_union([Set1|Sets], Union) :-
	append(Sets, Tail, Head),
	ord_union_queue(Set1, Head, Tail, Union).

    ord_union_queue(Set, Head, _Tail, Union) :- var(Head), !,
	Union = Set.
    ord_union_queue(Set1, [Set2|Sets], [Set12|Tail], Union) :-
	ord_union(Set1, Set2, Set12),
	Sets = [Set3|Head],
	ord_union_queue(Set3, Head, Tail, Union).


:- comment(ord_intersection/2, [
    amode:(ord_intersection(+,-) is det),
    args:["Sets":"A list of sets", "Intersection":"A set or variable"],
    summary:"Computes the intersection of all sets in Sets",
    see_also:[ord_intersection/3, ord_union/2],
    desc:html("\
	Succeeds when Intersection is the intersection of all sets in
	the list Sets.
    ")]).

:- mode ord_intersection(+,?).
ord_intersection([], []).
ord_intersection([Set|Sets], Common) :-
	ord_intersection_multi(Sets, Set, Common).

    ord_intersection_multi([], Common, Common).
    ord_intersection_multi([Set|Sets], Common0, Common) :-
	( Common0 == [] ->
	    Common = Common0	% finish early
	;
	    ord_intersection(Common0, Set, Common1),
	    ord_intersection_multi(Sets, Common1, Common)
	).

