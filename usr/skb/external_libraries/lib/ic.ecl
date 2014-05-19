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
% Contributor(s): Andrew Sadler, Warwick Harvey, IC-Parc
% 
% END LICENSE BLOCK
%---------------------------------------------------------------------
%
% Imports and exports.
%
%---------------------------------------------------------------------
%
% IC top-level module.
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Andrew Sadler, Warwick Harvey, IC-Parc
%
% This module is the top-level wrapper around the standard parts of the IC
% library, a combined finite domain and floating point interval propagation
% solver.  This wrapper repackages and exports the parts of the ic_kernel,
% ic_constraints and ic_search modules which are intended for use by
% ordinary users (in particular, it does not export the parts of ic_kernel
% intended for use only by those writing constraint solvers built on top of
% IC).  The purpose of the repackaging is to provide a convenient interface
% for the user: they just import the one module (ic), and all module
% qualifications are the same (and short) - the user need not know whether a
% particular predicate is actually implemented in ic_kernel, ic_constraints
% or ic_search.
%
%---------------------------------------------------------------------

:- module(ic).

%---------------------------------------------------------------------
%
% Set some compiler directives.
%

:- pragma(expand).
:- pragma(nodebug).



% Reexport the (public parts of) the other IC modules.

:- use_module(ic_kernel).

:- reexport
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
	msg/3,			% msg(?X,?Y,-Msg)
	print_solver_var/2,	% print_solver_var(?Var, -Printed)
	set_threshold/1,	% set_threshold(++Threshold)
	set_threshold/2,	% set_threshold(++Threshold, +WakeVars)
	get_threshold/1,	% get_threshold(++Threshold)
	is_in_domain/2,		% is_in_domain(++Val, ?Var)
	is_in_domain/3,		% is_in_domain(++Val, ?Var, ?Result)
	delayed_goals_number/2	% delayed_goals_number(?Var, -Number)
from ic_kernel.

:- reexport
	struct(ic with [])	% needed for suspend/3 to work
    from ic_kernel.

%:- reexport tr_ic_unify_goal_out/2 from ic_kernel.
%:- reexport portray(_, tr_ic_unify_goal_out/2, _) from ic_kernel.

:- reexport ic_constraints.
:- reexport ic_search.
:- comment(include, generic_search_comments).

%---------------------------------------------------------------------
% Explicitly import those predicates which have their documentation
% defined in ic, but whose definitions are to be found in ic_search.
%
:- import squash/3 from ic_search.
:- import locate/2 from ic_search.
:- import locate/3 from ic_search.
:- import locate/4 from ic_search.


%---------------------------------------------------------------------
%
% Implementation of the canonical constraint names.
%
% This really belongs in ic_constraints, but it's easier if it's done in a
% different module.
%

:- export (=:=)/2, (>=)/2, (=<)/2, (>)/2, (<)/2, (=\=)/2.
:- export (=:=)/3, (>=)/3, (=<)/3, (>)/3, (<)/3, (=\=)/3.

:-tool('=:='/2, '*=_body'/3).
:-tool('>='/2,  '*>=_body'/3).
:-tool('=<'/2,  '*=<_body'/3).
:-tool('<'/2,   '*<_body'/3).
:-tool('>'/2,   '*>_body'/3).
:-tool('=\\='/2,'*\\=_body'/3).

:-tool('=:='/3, '*=_body'/4).
:-tool('>='/3,  '*>=_body'/4).
:-tool('=<'/3,  '*=<_body'/4).
:-tool('<'/3,   '*<_body'/4).
:-tool('>'/3,   '*>_body'/4).
:-tool('=\\='/3,'*\\=_body'/4).


:- inline((=:=)/2, tr_ic_constraint_in/2).
:- inline((>=)/2, tr_ic_constraint_in/2).
:- inline((=<)/2, tr_ic_constraint_in/2).
:- inline((<)/2, tr_ic_constraint_in/2).
:- inline((>)/2, tr_ic_constraint_in/2).
:- inline((=\=)/2, tr_ic_constraint_in/2).
:- inline((=:=)/3, tr_ic_constraint_in/2).
:- inline((>=)/3, tr_ic_constraint_in/2).
:- inline((=<)/3, tr_ic_constraint_in/2).
:- inline((<)/3, tr_ic_constraint_in/2).
:- inline((>)/3, tr_ic_constraint_in/2).
:- inline((=\=)/3, tr_ic_constraint_in/2).



%---------------------------------------------------------------------
%
% User documentation for the IC family of modules.
%
% We put all the predicate documentation in this module so that the
% generated documents list it as being from `ic' even when it is actually
% defined in `ic_constraints' or `ic_search' or whereever.
%

:- comment(summary, "Hybrid integer/real interval arithmetic constraint solver").
:- comment(author, "Warwick Harvey, Andrew Sadler, Andrew Cheadle").

:- comment(desc, html("<P>
   The IC (Interval Constraint) library is a hybrid integer/real interval
   arithmetic constraint solver.  Its aim is to make it convenient for
   programmers to write hybrid solutions to problems, mixing together
   integer and real constraints and variables.</P><P>

   The integer constraints and variables are similar to those available in
   the old finite domain library `fd'.  The real constraints are similar to
   those that were available in the old real interval arithmetic library
   `ria'.  Constraints which are not specifically integer constraints can be
   applied to either real or integer variables (or a mix) seamlessly, and
   any real variable can be converted to an integer variable at any time by
   imposing an integrality constraint on it.</P><P>

   The IC library replaces the `fd', `ria' and `range' libraries (with a new
   symbolic solver library providing the non-numeric functionality of `fd').
   <P>
   For more information, see the IC section of the constraint library manual
   or the documentation for the individual IC predicates.</P><P>

   The IC library solves constraint problems over the reals.  It is not
   limited to linear constraints, so it can be used to solve general
   problems like:
   <PRE>
   [eclipse 2]: ln(X) $>= sin(X).

   X = X{0.36787944117144228 .. 1.0Inf}
   yes.
   </PRE>
   <P>
   The IC library handles linear and non-linear, reified constraints and
   user defined functions. 
   <P>
   User-defined functions/constraints are treated in a similar manner to
   user defined functions found in expressions handled by is/2.  Note,
   however, that user defined constraints/functions, when used in IC, should
   be (semi-)deterministic.  User defined constraints/functions which leave
   choice points may not behave as expected.
   <P>
   Linear constraints are handled by a single propagator, whereas non-linear
   constraints are broken down into simpler ternary/binary/unary
   propagators.  The value of any constraint found in an expression is its
   reified truth value (0..1).
   <P>
   Variables appearing in arithmetic IC constraints at compile-time are
   assumed to be IC variables unless they are wrapped in an <STRONG>eval/1</STRONG>
   term.  The <STRONG>eval/1</STRONG> wrapper inside arithmetic constraints is used to
   indicate that a variable will be bound to an expression at run-time.
   This feature will only be used by programs which generate their
   constraints dynamically at run-time, for example.
   <PRE>
   broken_sum(Xs,Sum):-
       (
	   foreach(X,Xs),
	   fromto(Expr,S1,S2,0)
       do
	   S1 = X + S2
       ),
       Sum $= Expr.
   </PRE>
   <P>
   The above implementation of a summation constraint will not work as
   intended because the variable <TT>Expr</TT> will be treated like an IC
   variable when it is in fact the term <TT>+(X1,+(X2,+(...)))</TT> which is
   constructed in the for-loop.  In order to get the desired functionality,
   one must wrap the variable <TT>Expr</TT> in an <STRONG>eval/1</STRONG>.
   <P>
   <PRE>
   working_sum(Xs,Sum):-
       (
	   foreach(X,Xs),
	   fromto(Expr,S1,S2,0)
       do
	   S1 = X + S2
       ),
       Sum $= eval(Expr).
   </PRE>
   <P>
   The following arithmetic expression can be used inside the constraints:
   <DL>
   <DT><STRONG>X</STRONG><DD>
	    Variables.  If X is not yet an interval variable, it is turned 
	    into one.

   <DT><STRONG>123</STRONG><DD>
	    Integer constants.

   <DT><STRONG>0.1</STRONG><DD>
	    Floating point constants.  These are assumed to be exact and
	    are converted to zero-width bounded reals.

   <DT><STRONG>0.1__0.2</STRONG><DD>
	    Bounded real constants.

   <DT><STRONG>pi, e</STRONG><DD>
	    Intervals enclosing the constants pi and e respectively.

   <DT><STRONG>inf</STRONG><DD>
	    Floating point infinity.

   <DT><STRONG>+Expr</STRONG><DD>
	    Identity.

   <DT><STRONG>-Expr</STRONG><DD>
	    Sign change.

   <DT><STRONG>+-Expr</STRONG><DD>
	    Expr or -Expr.  The result is an interval enclosing both.

   <DT><STRONG>abs(Expr)</STRONG><DD>
	    The absolute value of Expr.

   <DT><STRONG>E1+E2</STRONG><DD>
	    Addition.

   <DT><STRONG>E1-E2</STRONG><DD>
	    Subtraction.

   <DT><STRONG>E1*E2</STRONG><DD>
	    Multiplication.

   <DT><STRONG>E1/E2</STRONG><DD>
	    Division.

   <DT><STRONG>E1^E2</STRONG><DD>
	    Exponentiation.

   <DT><STRONG>min(E1,E2)</STRONG><DD>
	    Minimum.

   <DT><STRONG>max(E1,E2)</STRONG><DD>
	    Maximum.

   <DT><STRONG>sqr(Expr)</STRONG><DD>
	    Square.  Logically equivalent to Expr*Expr, but with better 
	    operational behaviour.

   <DT><STRONG>sqrt(Expr)</STRONG><DD>
	    Square root (always positive).

   <DT><STRONG>exp(Expr)</STRONG><DD>
	    Same as e^Expr.

   <DT><STRONG>ln(Expr)</STRONG><DD>
	    Natural logarithm, the reverse of the exp function.

   <DT><STRONG>sin(Expr)</STRONG><DD>
	    Sine.

   <DT><STRONG>cos(Expr)</STRONG><DD>
	    Cosine.

   <DT><STRONG>atan(Expr)</STRONG><DD>
	    Arcus tangens.  (Returns value between -pi/2 and pi/2.)

   <DT><STRONG>rsqr(Expr)</STRONG><DD>
	    Reverse of the sqr function.  The same as +-sqrt(Expr).

   <DT><STRONG>rpow(E1,E2)</STRONG><DD>
	    Reverse of exponentiation. i.e. finds X in E1 = X^E2.

   <DT><STRONG>sub(Expr)</STRONG><DD>
	    A subinterval of Expr.

   <DT><STRONG>sum(ExprList)</STRONG><DD>
	    Sum of a list of expressions.

   <DT><STRONG>min(ExprList)</STRONG><DD>
	    Minimum of a list of expressions.

   <DT><STRONG>max(ExprList)</STRONG><DD>
	    Maximum of a list of expressions.

   <DT><STRONG>and</STRONG><DD>
	    Reified constraint conjunction.  e.g. X&gt;3 and Y&lt;8

   <DT><STRONG>or</STRONG><DD>
	    Reified constraint disjunction.  e.g. X&gt;3 or Y&lt;8

   <DT><STRONG>=&gt;</STRONG><DD>
	    Reified constraint implication.  e.g. X&gt;3 =&gt; Y&lt;8

   <DT><STRONG>neg</STRONG><DD>
	    Reified constraint negation.  e.g. neg X&gt;3

   <DT><STRONG>$=, $\\=, $&gt;, $&gt;=, $&lt;, $=&lt;, #=, #\\=, #&gt;, #&gt;=, #&lt;, #=&lt;,
   =:=, =\\=, &gt;, &gt;=, &lt;, =&lt;, and, or, =&gt;, neg</STRONG><DD>
	    Constraints whose value is taken as their reified truth value
	    (0..1).

   <DT><STRONG>foo(Arg1, Arg2 ... ArgN), module:foo(Arg1, Arg2 ...  ArgN)</STRONG></DT>
	    Call user-defined constraint/function foo.

   <DT><STRONG>eval(Var)</STRONG><DD>
	    Var will be an expression at run-time.
   </DL>
")).


%---------------------------------------------------------------------
%
% Documentation for constraints reexported from ic_kernel.
%

:- comment(reals/1, [
    amode: (reals(-) is det),
    amode: (reals(+) is det),
%    template: "reals(?Vars)",
    args: [
	"Vars": "Variable or number, or a list or submatrix of variables/numbers"
    ],
    summary: "Vars' domain is the real numbers.",
    see_also: [integers/1, _:reals/1],
%    fail_if: "variables already a non-number.",
    desc: html("<P>
   Constrain the domain of the variables to be the real numbers.  This is
   the default, so the declaration is optional.</P><P>

   Note that the notion of real numbers is used here in the pure
   mathematical sense, where real numbers subsume the integers.  A variable
   of type real can therefore be instantated to either a real number (floating
   point or bounded real) or an integer number.  If the variables are already
   instantiated, this call checks that the variable is instantiated to a
   number.</P>
")
]).

%---------------------------------------------------------------------

:- comment(integers/1, [
    amode: (integers(-) is det),
    amode: (integers(+) is semidet),
%    template: "integers(?Vars)",
    args: [
	"Vars": "Variable or integer, or a list or submatrix of variables/integers"
    ],
    summary: "Vars' domain is the integer numbers.",
    see_also: [reals/1, _:integers/1],
%    fail_if: "variables already a non-integer.",
    desc: html("<P>
   Constrain the variables to integer values.  Note that this declaration is
   implicit when specifiying an integer interval, e.g. in <TT>Y :: 0..99</TT>.</P>
")
]).

%---------------------------------------------------------------------

:- comment(is_solver_var/1, [
    amode: (is_solver_var(?) is semidet),
    args: [
	"Term": "A Term"
    ],
    summary: "Succeeds iff Term is an IC variable.",
    fail_if: "Var is not an IC variable.",
    desc: html("<P>
   Test if the term Term is an IC variable.  Succeed if it is, fail
   otherwise.</P>
")
]).

%---------------------------------------------------------------------

:- comment(is_exact_solver_var/1, [
    amode: (is_exact_solver_var(?) is semidet),
    args: [
	"Term": "A Term"
    ],
    summary: "Succeeds iff Term is an IC integer variable.",
    fail_if: "Var is not an IC integer variable.",
    desc: html("<P>
   Test if the term Term is an IC integer variable.
   </P>
")
]).

%---------------------------------------------------------------------

:- comment(is_solver_type/1, [
    amode: (is_solver_type(?) is semidet),
    args: [
	"Term": "A Term"
    ],
    summary: "Succeeds iff Term is an IC variable or a number.",
    fail_if: "Var is not an IC variable or a number.",
    desc: html("<P>
   Test if the term Term is an IC variable or a numeric type supported by
   the solver (any ground number).  Succeed if it is, fail otherwise.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_solver_type/2, [
    amode: (get_solver_type(?, -) is semidet),
    args: [
	"Var":  "A variable or a number",
	"Type": "Type of Var (real or integer)"
    ],
    summary: "Retrieve the type of a variable.",
    fail_if: "Var is not a variable or a number.",
    desc: html("<P>
   Retrieve the type (the atom 'real' or the atom 'integer') of a variable
   (or number).  If Var has not been declared before, it will be turned into
   an unrestricted real variable.  If Var is a ground number, the type
   returned will be appropriate for its type (i.e. if it's not an integer,
   the type returned will be real).</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_bounds/3, [
    amode: (get_bounds(?, -, -) is det),
    args: [
	"Var": "A variable or a number",
	"Lo":  "Lower bound",
	"Hi":  "Upper bound"
    ],
    summary: "Retrieve the current bounds of Var.",
%    fail_if: "Var is not a variable or a number.",
    see_also: [get_min/2, get_max/2, get_float_bounds/3,
		get_integer_bounds/3, get_finite_integer_bounds/3,
		get_delta/2, get_median/2],
    desc: html("<P>
   Primitive for retrieving the upper and lower bounds of Var.  Lo and Hi
   return the minimum and maximum (respectively) of the variable's interval.
   If the variable is integer and a bound is finite, then that bound will be
   returned as an integer; in all other cases it will be returned as a
   float.  If Var has not been declared before, it will be turned into an
   unrestricted real variable.  If Var is a ground number, Lo and Hi will
   give appropriate bounds based on the type and value of Var: exact bounds
   for floats, bounded reals and integers, and best safe approximations for
   rationals.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_min/2, [
    amode: (get_min(?, -) is det),
    args: [
	"Var": "A variable or a number",
	"Lo":  "Lower bound"
    ],
    summary: "Retrieve the current lower bound of Var.",
%    fail_if: "Var is not a variable or a number.",
    see_also: [get_bounds/3, get_float_bounds/3,
		get_integer_bounds/3, get_finite_integer_bounds/3],
    desc: html("<P>
   Primitive for retrieving the lower bound of Var.  Lo returns the minimum
   of the variable's interval.  If the variable is integer and the bound is
   finite, then the bound will be returned as an integer; in all other cases
   it will be returned as a float.  If Var has not been declared before, it
   will be turned into an unrestricted real variable.  If Var is a ground
   number, Lo will give an appropriate bound based on the type and value of
   Var: an exact bound for floats, bounded reals and integers, and a best
   safe approximation for rationals.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_max/2, [
    amode: (get_max(?, -) is det),
    args: [
	"Var": "A variable or a number",
	"Hi":  "Upper bound"
    ],
    summary: "Retrieve the current upper bound of Var.",
%    fail_if: "Var is not a variable or a number.",
    see_also: [get_bounds/3, get_float_bounds/3,
		get_integer_bounds/3, get_finite_integer_bounds/3],
    desc: html("<P>
   Primitive for retrieving the upper bound of Var.  Hi returns the maximum
   of the variable's interval.  If the variable is integer and the bound is
   finite, then the bound will be returned as an integer; in all other cases
   it will be returned as a float.  If Var has not been declared before, it
   will be turned into an unrestricted real variable.  If Var is a ground
   number, Hi will give an appropriate bound based on the type and value of
   Var: an exact bound for floats, bounded reals and integers, and a best
   safe approximation for rationals.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_float_bounds/3, [
    amode: (get_float_bounds(?, -, -) is det),
    args: [
	"Var": "A variable or a number",
	"Lo":  "Lower bound",
	"Hi":  "Upper bound"
    ],
    summary: "Retrieve the current bounds of Var as floats.",
%    fail_if: "Var is not a variable or a number.",
    see_also: [get_bounds/3, get_integer_bounds/3,
	    get_finite_integer_bounds/3, get_delta/2, get_median/2],
    desc: html("<P>
   Primitive for retrieving the upper and lower bounds of Var.  Lo and Hi
   return the minimum and maximum (respectively) of the variable's interval
   in floating point format (regardless of the variable's type).  If Var has
   not been declared before, it will be turned into an unrestricted real
   variable.  If Var is a ground number, Lo and Hi will give appropriate
   bounds based on the type and value of Var: exact bounds for floats,
   bounded reals and exactly representable integers, and best safe
   approximations for large integers and rationals.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_integer_bounds/3, [
    amode: (get_integer_bounds(?, -, -) is det),
    args: [
	"Var": "A variable or a number",
	"Lo":  "Lower bound",
	"Hi":  "Upper bound"
    ],
    summary: "Retrieve the current bounds of (integral) Var.",
%    fail_if: "Var is not a variable or a number.",
    see_also: [get_bounds/3, get_finite_integer_bounds/3,
		get_float_bounds/3],
    desc: html("<P>
   Primitive for retrieving the upper and lower bounds of (integral) Var.
   Lo and Hi return the minimum and maximum (respectively) of the variable's
   interval.  If Var has not been declared before, it will be turned into an
   unrestricted integer variable.  If Var is a real IC variable, it will be
   constrained to be integral.  (These changes may cause propagation.)
   Infinite bounds are returned as floating point numbers; all others are
   returned as integers.  If Var is a ground integer, Lo and Hi will unified
   with that integer.  All other cases result in a type error.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_finite_integer_bounds/3, [
    amode: (get_finite_integer_bounds(?, -, -) is det),
    args: [
	"Var": "A variable or a number",
	"Lo":  "Lower bound",
	"Hi":  "Upper bound"
    ],
    summary: "Retrieve the current (finite, integral) bounds of Var.",
%    fail_if: "Var is not a variable or a number.",
    see_also: [get_bounds/3, get_integer_bounds/3,
		get_float_bounds/3],
    desc: html("<P>
   Primitive for retrieving the (finite, integral) upper and lower bounds of
   Var.  Lo and Hi return the minimum and maximum (respectively) of the
   variable's interval.  If Var has not been declared before, it will be
   turned into an integer variable.  If Var is a real IC variable, it will
   be constrained to be integral.  If any bound is infinite, it has a
   default bound imposed to make it finite (-10000000 for lower bounds,
   10000000 for upper bounds).  (These changes may cause propagation.)
   Bounds are always returned as integers.  If Var is a ground integer, Lo
   and Hi will unified with that integer.  All other cases result in a type
   error.</P>
")
]).

%---------------------------------------------------------------------

:- comment(get_domain_size/2, [
    amode: (get_domain_size(?, -) is det),
    args: [
	"Var":   "An IC variable or a number",
        "Size":  "A variable (or integer)"
    ],
    summary: "Size is the number of integer elements in the IC domain for Var",
    see_also: [get_delta/2],
    desc: html("<P>
   If Var is an integer IC variable, Size will be set to the number of 
   integer values in the domain of Var.  If Var is a number, then Size 
   will be set to 1.</P><P>

   Currently, an out of range exception would be raised if Var is an IC 
   variable of type real.  This may change in the future.</P>
"),
   exceptions: [
        5: "Var is neither an IC variable or number.",
	6: "Var is a IC variable of type real."
   ],
   fail_if: "The initial value of Size fails to unify with the returned value."
]).

%---------------------------------------------------------------------
:- comment(get_domain/2, [
    amode: (get_domain(?, -) is det),
    args: [
	"Var":    "An IC variable or a number.",
        "Domain": "A ground representation of the domain of Var."
    ],
    summary: "Returns a ground representation of the current IC domain of a variable.",
    see_also: [get_domain_as_list/2, get_bounds/3],
    desc: html("<P>
   If Var is a number, Domain will be unified with that number.</P><P>

   If Var is a real IC variable, Domain will be unified with the term
   Lo..Hi where Lo and Hi are floats corresponding to the current lower
   and upper bounds of Var, respectively.</P><P>

   If Var is an integer IC variable with no holes in its domain, Domain will
   be unified with the term Lo..Hi where Lo and Hi are integers
   corresponding to the current lower and upper bounds of Var, respectively.</P><P>

   If Var is an integer IC variable with holes in its domain, Domain will
   be unified with an ordered list of integers and/or terms Lo..Hi where Lo
   and Hi are integers; in this case the elements of the domain of Var are
   exactly those integers appearing directly in the list or falling within
   any of the intervals Lo..Hi.</P>
"),
   exceptions: [
        5: "Var is neither an IC variable or number.",
	6: "Var is a IC variable of type real."
   ],
   fail_if: "The initial value of DomainList fails to unify with the returned value."
]).

%---------------------------------------------------------------------
:- comment(get_domain_as_list/2, [
    amode: (get_domain_as_list(?, -) is det),
    args: [
	"Var":   "An IC variable or a number",
        "DomainList":  "The domain of Var as a list of elements."
    ],
    summary: "List of all the elements in the IC domain of Var",
    see_also: [get_domain/2, get_bounds/3],
    desc: html("<P>
   If Var is an integer IC variable, DomainList will be set to an ordered
   list containing each element in the domain of Var.  If Var is a number,
   then DomainList will be set to a singleton list containing the number.</P><P>

   Currently, an out of range exception would be raised if Var is an IC 
   variable of type real.  This may change in the future.</P>
"),
   exceptions: [
        5: "Var is neither an IC variable or number.",
	6: "Var is a IC variable of type real."
   ],
   fail_if: "The initial value of DomainList fails to unify with the returned value."
]).

%---------------------------------------------------------------------

:- comment(get_median/2, [
    amode: (get_median(?, -) is det),
    args: [
	"Var":    "A variable or a number",
	"Median": "The median of the interval (float)"
    ],
    summary: "Returns the median of the interval of the IC variable Var.",
    see_also: [get_delta/2, get_bounds/3, get_float_bounds/3],
    desc: html("<P>
   Returns the median of the interval of Var (usually so that the interval
   can be split) as a float value.  If Var has not been declared before,
   it will be turned into an unrestricted real variable as a side effect.
   If Var is a ground number, the median is a float equal or near that
   number.

   Generally, the median splits the interval logarithmically so that the
   two subintervals have roughly the same number of representable floats.
   Only in the vicinity of zero, splitting is linear to prevent the sub-
   intervals from getting too small.</P>
"),
    eg: "\
[eclipse 2]: X :: 10..1000, get_median(X, M).
X = X{10 .. 1000}
M = 100.0
Yes (0.00s cpu)

[eclipse 3]: X :: -1..1000, get_median(X, M).
X = X{-1 .. 1000}
M = 11.633369384516794
Yes (0.00s cpu)

[eclipse 4]: get_median(3, M).
M = 3.0
Yes (0.00s cpu)
"
]).

%---------------------------------------------------------------------

:- comment(get_delta/2, [
    amode: (get_delta(?, -) is det),
    args: [
	"Var":   "A variable or a number",
	"Width": "Width of the interval"
    ],
    summary: "Returns the width of the interval of Var.",
    see_also: [get_median/2, get_bounds/3],
    desc: html("<P>
   Returns the width (Hi - Lo) of the interval of Var.  If Var has not been
   declared before, it will be turned into an unrestricted real variable.
   If Var is a ground number, the width will be calculated appropriately
   based on its type.</P>
")
]).

%---------------------------------------------------------------------

:- comment(is_in_domain/2, [
    amode: (is_in_domain(++,?) is semidet),
    args: [
        "Val": "A number",
        "Var": "An IC variable or a number"
    ],
    summary: "Succeeds iff Val is in the domain of Var",
    exceptions:[4: "Val is not ground",
                5: "either Val or Var is not a numeric type",
                'undecidable comparison of bounded reals':
               "Val was a bounded real which overlapped a bound of Var"],
    see_also:[is_in_domain/3],
    desc: html("<P>
   Low level predicate which succeeds when Val is in the domain of Var.  If
   Var is a number, this predicate will succeed iff Var $= Val would succeed
   WITHOUT leaving a delayed goal.
")
]).

%---------------------------------------------------------------------

:- comment(is_in_domain/3, [
    amode: (is_in_domain(++,?,-) is det),
    args: [
        "Val": "A number",
    	"Var": "An IC variable or a number",
        "Result": "An atom"
    ],
    summary: "Binds Result to indicate presence of Val in domain of Var",
    exceptions:[4: "Val is not ground",
                5: "either Val or Var is not a numeric type"],
    see_also:[is_in_domain/2],
    desc: html("<P>
   Low level predicate which succeeds when Val is in the domain of Var with
   Result bound to the atom 'yes'.  When Val is not in the domain of Var,
   the predicate succeeds binding Result to the atom 'no'.  Should the
   result be undecidable (as can be the case with bounded reals), the
   predicate will succeed binding Result to the atom 'maybe'.  If Var is a
   number, this predicate will succeed with Result bound to 'yes' iff
   Var $= Val would succeed WITHOUT leaving a delayed goal.  If Var $= Val
   would have left a delayed goal then Result gets bound to 'maybe'.
")
]).

%---------------------------------------------------------------------

:- comment(get_threshold/1, [
    amode: (get_threshold(-) is det),
    args: [
	"Threshold": "Current threshold"
    ],
    summary: "Returns the current propagation threshold.",
    see_also: [set_threshold/1, set_threshold/2],
    desc: html("<P>
   Returns the current threshold.  For non-integer variables, bounds are
   only changed if the absolute and relative changes of the bound exceed the
   threshold.  This means that constraints over real variables are only
   guaranteed to be consistent up to the current threshold (over and above
   any normal widening which occurs).</P><P>

   Limiting the amount of propagation is important for efficiency.  A higher
   threshold speeds up computations, but reduces precision and may in the
   extreme case prevent the system from being able to locate individual
   solutions.</P>
")
]).

%---------------------------------------------------------------------

:- comment(set_threshold/1, [
    amode: (set_threshold(++) is det),
    args: [
	"Threshold": "New threshold (float)"
    ],
    summary: "Sets the propagation threshold.",
    see_also: [get_threshold/1, set_threshold/2],
    desc: html("<P>
   Set the threshold to Threshold, which is a small floating-point number.
   Subsequent to this, bounds updates to non-integer variables are only
   performed if the change in the bounds exceeds this threshold (in both
   absolute and relative terms).</P><P>

   The default threshold is 1e-8.</P><P>

   Limiting the amount of propagation is important for efficiency.  A higher
   threshold speeds up computations, but reduces precision and may in the
   extreme case prevent the system from being able to locate individual
   solutions.</P><P>

   Note that if the threshold is reduced using this predicate (requiring a
   higher level of precision), the current state of the system may not be
   consistent with respect to the new threshold.  If it is important that
   the new level of precision be realised for all or part of the system
   before computation proceeds, use set_threshold/2 instead.</P>
")
]).

%---------------------------------------------------------------------

:- comment(set_threshold/2, [
    amode: (set_threshold(++, +) is semidet),
    args: [
	"Threshold": "New threshold (float)",
	"WakeVars":  "Collection (a la collection_to_list/2) of IC variables to \"wake\""
    ],
    summary: "Sets the propagation threshold with recomputation.",
    see_also: [get_threshold/1, set_threshold/1, collection_to_list/2],
    desc: html("<P>
   Set the threshold to Threshold, which is a small floating-point number,
   and then if the threshold has been reduced, wake all constraints
   suspended on the bounds of the variables in the collection WakeVars.
   Subsequent to the setting of the new threshold, bounds updates to
   non-integer variables are only performed if the change in the bounds
   exceeds this threshold (in both absolute and relative terms).  The waking
   of the constraints when the threshold is reduced allows part or all of
   the system to be re-propagated in order to ensure the higher level of
   precision is achieved before the rest of the computation proceeds.</P><P>

   The default threshold is 1e-8.</P><P>

   Limiting the amount of propagation is important for efficiency.  A higher
   threshold speeds up computations, but reduces precision and may in the
   extreme case prevent the system from being able to locate individual
   solutions.</P>
")
]).

%---------------------------------------------------------------------


:- comment(print_solver_var/2, [
    amode: (print_solver_var(?, -) is det),
    args: [
	"Var":     "An IC variable (or IC attribute)",
	"Printed": "A printable representation of Var"
    ],
    summary: "Returns a representation of the IC variable Var suitable for printing.",
    fail_if: "Var is not an IC variable or attribute",
    desc: html("\
<P>
   The print handler for the IC attribute.  Returns a representation of the
   IC variable (or attribute) Var suitable for printing, encoding the type
   of the variable, the bounds, and any holes in the domain.
</P>")
]).

%---------------------------------------------------------------------

:- comment(delayed_goals_number/2, [
amode:     (delayed_goals_number(?,-) is det),
args:      ["Var":    "Variable or term",
            "Number": "Number of goals delayed on the ic attribute of Var."],
summary:   "Returns the number of goals delayed on the ic attribute of Var.",
desc:	   html("   N is the number of constraints and suspended goals currently attached to
   the IC attribute of the variable Var.  Note that this number may not
   correspond to the exact number of different constraints attached to Var,
   as goals in different suspension lists are counted separately, and a
   constraint may decompose into more than one suspension.  This predicate
   is often used when looking for the most or least constrained variable
   from a set of domain variables.  If Var is not a variable, N is bound to
   1.0Inf; otherwise if Var is not an IC variable, N is zero.
</P><P>
   Note that this predicate only counts goals attached to the IC attribute;
   if you want to count all goals attached to a variable, use the generic
   delayed_goals_number/2.
"),
see_also:[_:delayed_goals_number/2]
]).

/*
%---------------------------------------------------------------------

:- comment(unify_ic/2, [
amode:     (unify_ic(+,?) is semidet),
args:      ["Term": "",
            "Attribute":""
           ],
summary:   "Unification handler for ic attribute.",
desc:      html("\
<P>
     This is the unification handler for the ic attribute.  It is not
     meant to be called directly by the user.

</P><P>
     Unification between two variables amounts to intersecting their intervals
     and taking the more restrictive type as the result type.  If the
     intersection is empty, the unification fails.  Unifying a variable
     with a number involves a check whether the number is within the
     variable's interval and of the proper type, otherwise failure occurs.
<P>")
]).

%---------------------------------------------------------------------

:- comment(test_unify_ic/2, [
amode:     (test_unify_ic(+,?) is semidet),
args:      ["Term": "",
            "Attribute":""
           ],
summary:   "test_unify handler for ic attribute.",
desc:      html("\
<P>
     This is the test_unify handler for the ic attribute.  It is not
     meant to be called directly by the user.

</P><P>
     Unification between two variables amounts to intersecting their intervals
     and taking the more restrictive type as the result type.  If the
     intersection is empty, the unification fails.  Unifying a variable
     with a number involves a check whether the number is within the
     variable's interval and of the proper type, otherwise failure occurs.
<P>")
]).

%---------------------------------------------------------------------

:- comment(compare_ic_instances/3, [
amode:		(compare_ic_instances(-,?,?) is det),
args:           ["Res":   "Result (set to '<','>', or '=')",
                 "TermL": "Term (one must be attributed variable)",
                 "TermR": "Term (one must be attributed variable)"
                ],
summary:        "Compare instance handler for the ic attribute.",
see_also:       [compare_instances/3],
desc:           html("\
<P>
    Handler for compare_instance for the ic attribute.  At least one of
    TermL or TermR should be an attributed variable.

</P><P>
    An IC variable is an instance of another when its interval is subsumed
    by the other interval.
</P>")
]).

%---------------------------------------------------------------------

:- comment(copy_ic_term/2, [
amode:     (copy_ic_term(?,-) is det),
args:      ["Meta": "",
            "Copy":""
           ],
summary:   "copy_term handler for ic attribute.",
see_also:  [copy_term/2],
desc:      html("\
<P>
     This is the copy_term handler for the ic attribute.  It allows
     copy_term/2 to handle the ic attribute, and is not meant to
     be called directly by the user.

</P><P>
     Interval and type are copied, delayed goals are not.
</P>")
]).

%---------------------------------------------------------------------

:- comment(ic_suspensions/3, [
amode:     (ic_suspensions(?,-,-) is det),
args:      ["Meta":     "Attributed variable",
            "SuspLists": "List of suspension lists",
            "SuspListsTail": "Tail of SuspLists"
	   ],
summary:   "Handler to retrieve suspensions from the ic attribute"
]).

*/

%---------------------------------------------------------------------
%
% Documentation for constraints reexported from ic_constraints.
%

:- comment((#::)/2, [
    amode: #::(?, ++),
    template: "?Vars #:: ++Domain",
    args: [
	"Vars":   "Variable or collection (a la collection_to_list/2) of variables",
	"Domain": "Domain specification"
    ],
    summary: "Constrain Vars to be integral and have the domain Domain.",
    see_also: [(::)/2, ($::)/2],
    desc: html("<P>
   Like ::/2, but constrains Vars to take only integer values from the given
   domain.
")]).

:- comment(($::)/2, [
    amode: $::(?, ++),
    template: "?Vars $:: ++Domain",
    args: [
	"Vars":   "Variable or collection (a la collection_to_list/2) of variables",
	"Domain": "Domain specification"
    ],
    summary: "Constrain Vars to have the domain Domain.",
    see_also: [(::)/2, (#::)/2],
    desc: html("<P>
   Like ::/2, but does not perform any type analysis on the domain and does
   not enforce integrality on the variables even if the domain specified has
   integer bounds.  Domain must be a contiguous interval.
")]).

:- comment((::)/2, [
    amode: ::(?, ++),
    template: "?Vars :: ++Domain",
    args: [
	"Vars":   "Variable or collection (a la collection_to_list/2) of variables",
	"Domain": "Domain specification"
    ],
    summary: "Constrain Vars to have the domain Domain.",
    see_also: [integers/1, reals/1, _:(::)/2, (::)/3, (#::)/2, ($::)/2],
    desc: html("<P>
   Constrains Vars to take only integer or real values from the domain
   specified by Domain.  Vars may be a variable or a collection of variables
   (as accepted by collection_to_list/2).  Domain can be specified as a
   simple range Lo .. Hi, or as a list of subranges and/or individual
   elements.  Multiple subranges and/or individual elements are allowed in
   integer domains only.  If all subrange bounds and individual elements are
   integers the domain is considered an integer domain and the variables
   Vars are constrained to be integral; otherwise it is considered a real
   domain and the type of the variables is not constrained.  Note that
   infinities are considered to be untyped for this purpose.
<P>
   For instance:
<PRE>
     X :: 0..1                  % boolean
     X :: -1..5                 % integer between -1 and 5
     X :: 1..inf                % strictly positive integer
     X :: 0.0..10.0             % real between 0.0 and 10.0
     X :: 1.5..3.7              % real between 1.5 and 3.7
     X :: 0.0..5                % real between 0.0 and 5.0
     X :: 1.4__1.6..3.6__3.8    % real where the bounds aren't known exactly
     X :: breal(0)..inf         % nonnegative real
     X :: [0..3, 5, 8..10]      % any integer from 0 to 10 except 4 and 6
     [X, Y, Z] :: 1..8          % apply domain to X, Y and Z
     M[2..4, 5] :: 1..8         % apply to rows 2..4, column 5 of matrix M
     X :: [0.0..5.0, 7.0..9.0]  % Type error
     X :: [a, b, c]             % Type error
</PRE>
"),
    eg: "\
[eclipse 2]: X :: 0..1.
X = X{[0, 1]}
Yes (0.00s cpu)

[eclipse 3]: X :: -1..5.
X = X{-1 .. 5}
Yes (0.00s cpu)

[eclipse 4]: X :: 1..inf.
X = X{1 .. 1.0Inf}
Yes (0.00s cpu)

[eclipse 5]: X :: 0.0..10.0.
X = X{0.0 .. 10.0}
Yes (0.00s cpu)

[eclipse 6]: X :: 1.5..3.7.
X = X{1.5 .. 3.7}
Yes (0.00s cpu)

[eclipse 7]: X :: 1.4__1.6..3.6__3.8.
X = X{1.4 .. 3.8}
Delayed goals:
        ic : (-(X{1.4 .. 3.8}) =< -1.6__-1.4)
        ic : (X{1.4 .. 3.8} =< 3.6__3.8)
Yes (0.00s cpu)

[eclipse 8]: X :: [0..3, 5, 8..10].
X = X{[0 .. 3, 5, 8 .. 10]}
Yes (0.00s cpu)
"
]).

:- comment((::)/3, [
    amode: ::(?, ++, ?),
    template: "::(?Var, ++Domain, ?Bool)",
    args: [
	"Var":    "Variable",
	"Domain": "Domain specification",
        "Bool":   "Reified truth value"
    ],
    summary: "Reflect into Bool the truth of Var having the domain Domain.",
    see_also: [integers/1, reals/1, _:(::)/2, (::)/2],
    desc: html("<P>
   Provides a reified form of the ::/2 domain assignment predicate.  This
   reified ::/3 is defined only to work for one variable and only integer
   variables (unlike ::/2), hence only the Domain formats suitable for
   integers may be supplied to this reified ::/3.
<P>
   For a single variable, V, the Bool will be instantiated to 0 if the
   current domain of V does not intersect with Domain.  It will be
   instantiated to 1 iff the domain of V is wholly contained within Domain.
   Finally the Boolean will remain an integer variable in the range 0..1, if
   neither of the above two conditions hold.
<P>
   Instantiating Bool to 1, will cause the constraint to behave exactly like
   ::/2.  Instatiating Bool to 0 will cause Domain to be excluded from the
   domain of the variable where such an exclusion is representable.  If such
   an integer domain is unrepresentable (eg. -1.0Inf .. -5, 5..1.0Inf), then
   a delayed goal will be setup to exclude values when the bounds become
   sufficiently narrow.
<P>
   Note that calling the reified form of :: will result in the Variable
   becoming constrained to be integral, even if Bool is uninstantiated.
<P>
   Further note that, like other reified predicates, :: can be used infix in
   an IC expression, e.g. B #= (X :: [1..10]) is equivalent to
   ::(X, [1..10], B).
"),
    eg: "\
[eclipse 2]: ::(X, [1..10, 12..30],1).

X = X{[1 .. 10, 12 .. 30]}
Yes (0.00s cpu)

[eclipse 3]: ::(X, [1..10, 12..30],0).
X = X{-1.0Inf .. 1.0Inf}
Delayed goals:
        exclude_range(X{-1.0Inf .. 1.0Inf}, 1, 10)
        exclude_range(X{-1.0Inf .. 1.0Inf}, 12, 30)
Yes (0.00s cpu)

[eclipse 4]: ::(X, [1..10, 12..30],B).
X = X{-1.0Inf .. 1.0Inf}
B = B{[0, 1]}
Delayed goals:
        ic : ::(X{-1.0Inf .. 1.0Inf}, [1 .. 10, 12 .. 30], B{[0, 1]})
Yes (0.00s cpu)

[eclipse 5]: ic:( B =:= (X :: [1..10,12..30])).
B = B{[0, 1]}
X = X{-1.0Inf .. 1.0Inf}
Delayed goals:
        ic : ::(X{-1.0Inf .. 1.0Inf}, [1 .. 10, 12 .. 30], B{[0, 1]})
Yes (0.00s cpu)

"
]).

:- comment(($::)/3, [
    amode: $::(?, ++, ?),
    template: "$::(?Var, ++Domain, ?Bool)",
    args: [
	"Var":    "Variable",
	"Domain": "Domain specification",
        "Bool":   "Reified truth value"
    ],
    summary: "Reflect into Bool the truth of Var having the domain"
             " Domain. Does not impose integrality.",
    see_also: [integers/1, reals/1, _:($::)/2, ($::)/2, (::)/3],
    desc: html("<P>
   Provides a reified form of the $::/2 domain assignment predicate.  This
   reified $::/3 is defined only to work for one variable and real 
   variables (unlike ::/3), hence only the Domain formats suitable for
   reals may be supplied to this reified $::/3.
   <P>
   For a single variable, V, the Bool will be instantiated to 0 if the
   current domain of V does not intersect with Domain.  It will be
   instantiated to 1 iff the domain of V is wholly contained within Domain.
   Finally the Boolean will remain an integer variable in the range 0..1, if
   neither of the above two conditions hold.
   <P>
   Instantiating Bool to 1, will cause the constraint to behave exactly like
   $::/2.  Instatiating Bool to 0 will cause a set of delayed goals to
   excluded the range from the domain of the variable where such an
   exclusion is representable.
   <P>
   Note that calling the reified form of $:: will NOT result in the Variable
   becoming constrained to be integral, even if domain contains only integers.
   <P>
   Further note that, like other reified predicates, $:: can be used infix in
   an IC expression, e.g. B #= (X $:: [1..10]) is equivalent to
   $::(X, [1..10], B).
"),
    eg: "\
[eclipse 2]: $::(X, 1..30,1).

X = X{1.0 .. 30.0}
Yes (0.00s cpu)

[eclipse 3]: $::(X, [1..10, 12..30],0).

Abort
type error in $::(X, [1 .. 10, 12 .. 30], 0)

[eclipse 4]: $::(X, 1.0..30.0,B).

X = X{-1.0Inf .. 1.0Inf}
B = B{[0, 1]}
There are 3 delayed goals.
Yes (0.00s cpu)
"
]).


:- comment((#::)/3, [
    amode: #::(?, ++, ?),
    template: "#::(?Var, ++Domain, ?Bool)",
    args: [
	"Var":    "Variable",
	"Domain": "Domain specification",
        "Bool":   "Reified truth value"
    ],
    summary: "Reflect into Bool the truth of Var having the domain Domain.",
    see_also: [integers/1, reals/1, _:(#::)/2, (#::)/2, (::)/3],
    desc: html("<P>
   Provides a reified form of the #::/2 domain assignment predicate.  This
   reified #::/3 is defined only to work for one variable and only integer
   variables (unlike #::/2), hence only the Domain formats suitable for
   integers may be supplied to this reified #::/3.
   <P>
   For a single variable, V, the Bool will be instantiated to 0 if the
   current domain of V does not intersect with Domain.  It will be
   instantiated to 1 iff the domain of V is wholly contained within Domain.
   Finally the Boolean will remain an integer variable in the range 0..1, if
   neither of the above two conditions hold.
   <P>
   Instantiating Bool to 1, will cause the constraint to behave exactly like
   #::/2.  Instatiating Bool to 0 will cause Domain to be excluded from the
   domain of the variable where such an exclusion is representable.  If such
   an integer domain is unrepresentable (eg. -1.0Inf .. -5, 5..1.0Inf), then
   a delayed goal will be setup to exclude values when the bounds become
   sufficiently narrow.
   <P>
   Calling #::/3 with a range with real bounds eg. #::(X,1.0..10.0,B) will
   result in a type error.
   <P>
   Note that calling the reified form of #:: will result in the Variable
   becoming constrained to be integral, even if Bool is uninstantiated.
   <P>
   Further note that, like other reified predicates, #:: can be used infix in
   an IC expression, e.g. B #= (X #:: [1..10]) is equivalent to
   #::(X, [1..10], B).
"),
    eg: "\
[eclipse 2]: #::(X, [1..10, 12..30],1).

X = X{[1 .. 10, 12 .. 30]}
Yes (0.00s cpu)

[eclipse 3]: #::(X, [1..10, 12..30],0).
X = X{-1.0Inf .. 1.0Inf}
Delayed goals:
        exclude_range(X{-1.0Inf .. 1.0Inf}, 1, 10)
        exclude_range(X{-1.0Inf .. 1.0Inf}, 12, 30)
Yes (0.00s cpu)

[eclipse 4]: #::(X, [1..10, 12..30],B).
X = X{-1.0Inf .. 1.0Inf}
B = B{[0, 1]}
Delayed goals:
        ic : #::(X{-1.0Inf .. 1.0Inf}, [1 .. 10, 12 .. 30], B{[0, 1]})
Yes (0.00s cpu)

[eclipse 5]: ic:( B =:= (X #:: [1..10,12..30])).
B = B{[0, 1]}
X = X{-1.0Inf .. 1.0Inf}
Delayed goals:
        ic : #::(X{-1.0Inf .. 1.0Inf}, [1 .. 10, 12 .. 30], B{[0, 1]})
Yes (0.00s cpu)

"
]).


%---------------------------------------------------------------------
:- comment(($=)/2, [
    amode: $=(?, ?),
    template: "?ExprX $= ?ExprY",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression"
    ],
    summary: "ExprX is equal to ExprY.",
    see_also: [(=:=)/2],
    desc: html("<P>
   Constrains ExprX and ExprY to be equal.  Alternate form of
   ic:(ExprX =:= ExprY).
")]).

:- comment(($=<)/2, [
    amode: $=<(?, ?),
    template: "?ExprX $=< ?ExprY",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression"
    ],
    summary: "ExprX is less than or equal to ExprY.",
    see_also: [(=<)/2],
    desc: html("<P>
   Constrains ExprX to be less than or equal to ExprY.  Alternate form of
   ic:(ExprX =< ExprY).
")]).

:- comment(($<)/2, [
    amode: $<(?, ?),
    template: "?ExprX $< ?ExprY",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression"
    ],
    summary: "ExprX is strictly less than ExprY.",
    see_also: [(<)/2],
    desc: html("<P>
   Constrains ExprX to be strictly less than ExprY.  Alternate form of
   ic:(ExprX < ExprY).
")]).

:- comment(($>=)/2, [
    amode: $>=(?, ?),
    template: "?ExprX $>= ?ExprY",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression"
    ],
    summary: "ExprX is greater than or equal to ExprY.",
    see_also: [(>=)/2],
    desc: html("<P>
   Constrains ExprX to be greater than or equal to ExprY.  Alternate form of
   ic:(ExprX >= ExprY).
")]).

:- comment(($>)/2, [
    amode: $>(?, ?),
    template: "?ExprX $> ?ExprY",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression"
    ],
    summary: "ExprX is strictly greater than ExprY.",
    see_also: [(>)/2],
    desc: html("<P>
   Constrains ExprX to be strictly greater than ExprY.  Alternate form of
   ic:(ExprX > ExprY).
")]).

:- comment(($\=)/2, [
    amode: $\=(?, ?),
    template: "?ExprX $\\= ?ExprY",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression"
    ],
    summary: "ExprX is not equal to ExprY.",
    see_also: [(=\=)/2],
    desc: html("<P>
   Constrains ExprX and ExprY to not be equal.  Alternate form of
   ic:(ExprX =\\= ExprY).
")]).


:- comment((=:=)/2, [
    amode: =:=(?, ?),
    template: "ic:(?ExprX =:= ?ExprY)",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression"
    ],
    summary: "ExprX is equal to ExprY.",
    see_also: [(<)/2, (=<)/2, (>=)/2, (>)/2, (=\=)/2,
               (#=)/2, (=:=)/3, _:(=:=)/2],
    desc: html("<P>
   Constrains ExprX and ExprY to be equal.
")
]).

:- comment((=:=)/3, [
    amode: =:=(?, ?, ?),
    template: "=:=(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is equal to ExprY.",
    see_also: [(<)/3, (=<)/3, (>=)/3, (>)/3, (=\=)/3,
               (#=)/3, (=:=)/2, _:(=:=)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>
")
]).

%---------------------------------------------------------------------

:- comment((>=)/2, [
    amode: >=(?, ?),
    template: "ic:(?ExprX >= ?ExprY)",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression"
    ],
    summary: "ExprX is greater than or equal to ExprY.",
    see_also: [(<)/2, (=<)/2, (=:=)/2, (>)/2, (=\=)/2,
               (#>=)/2, (>=)/3, _:(>=)/2],
    desc: html("<P>
   Constrains ExprX to be greater than or equal to ExprY.</P>
")
]).

:- comment((>=)/3, [
    amode: >=(?, ?, ?),
    template: ">=(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is greater than or equal to ExprY.",
    see_also: [(<)/3, (=<)/3, (=:=)/3, (>)/3, (=\=)/3,
               (#>=)/3, (>=)/2, _:(>=)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>
")
]).

%---------------------------------------------------------------------

:- comment((>)/2, [
    amode: >(?, ?),
    template: "ic:(?ExprX > ?ExprY)",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression"
    ],
    summary: "ExprX is strictly greater than ExprY.",
    see_also: [(<)/2, (=<)/2, (=:=)/2, (>=)/2, (=\=)/2,
               (#>)/2, (>)/3, _:(>)/2],
    desc: html("<P>
   Constrains ExprX to be strictly greater than ExprY.</P>
")
]).

:- comment((>)/3, [
    amode: >(?, ?, ?),
    template: ">(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is strictly greater than ExprY.",
    see_also: [(<)/3, (=<)/3, (=:=)/3, (>=)/3, (=\=)/3,
               (#>)/3, (>)/2, _:(>)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>
")
]).

%---------------------------------------------------------------------

:- comment((=<)/2, [
    amode: =<(?, ?),
    template: "ic:(?ExprX =< ?ExprY)",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression"
    ],
    summary: "ExprX is less than or equal to ExprY.",
    see_also: [(<)/2, (=:=)/2, (>=)/2, (>)/2, (=\=)/2,
               (#=<)/2, (=<)/3, _:(=<)/2],
    desc: html("<P>
   Constrains ExprX to be less than or equal to ExprY.</P>
")
]).

:- comment((=<)/3, [
    amode: =<(?, ?, ?),
    template: "=<(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is less than or equal to ExprY.",
    see_also: [(<)/3, (=:=)/3, (>=)/3, (>)/3, (=\=)/3,
               (#=<)/3, (=<)/2, _:(=<)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>
")
]).

%---------------------------------------------------------------------

:- comment((<)/2, [
    amode: <(?, ?),
    template: "ic:(?ExprX < ?ExprY)",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression"
    ],
    summary: "ExprX is strictly less than ExprY.",
    see_also: [(=<)/2, (=:=)/2, (>=)/2, (>)/2, (=\=)/2,
               (#<)/2, (<)/3, _:(<)/2],
    desc: html("<P>
   Constrains ExprX to be strictly less than ExprY.</P>
")
]).

:- comment((<)/3, [
    amode: <(?, ?, ?),
    template: "<(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is strictly less than ExprY.",
    see_also: [(=<)/3, (=:=)/3, (>=)/3, (>)/3, (=\=)/3,
               (#<)/3, (<)/2, _:(<)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>
")
]).

%---------------------------------------------------------------------

:- comment((=\=)/2, [
    amode: =\=(?, ?),
    template: "ic:(?ExprX =\\= ?ExprY)",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression"
    ],
    summary: "ExprX is not equal to ExprY.",
    see_also: [(<)/2, (=<)/2, (>=)/2, (>)/2, (=:=)/2,
               (#\=)/2, (=\=)/3, _:(=\=)/2],
    desc: html("<P>
   Constrains ExprX and ExprY to be different.
")
]).

:- comment((=\=)/3, [
    amode: =\=(?, ?, ?),
    template: "=\\=(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Arithmetic expression",
	"ExprY": "Arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is not equal to ExprY.",
    see_also: [(<)/3, (=<)/3, (>=)/3, (>)/3, (=:=)/3,
               (#\=)/3, (=\=)/2, _:(=\=)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>
")
]).


%---------------------------------------------------------------------

:- comment((#=)/2, [
    amode: #=(?, ?),
    template: "?ExprX #= ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is equal to ExprY (with integrality constraints).",
    see_also: [(#<)/2, (#=<)/2, (#>=)/2, (#>)/2, (#\=)/2,
               (=:=)/2, (#=)/3, _:(#=)/2],
    desc: html("<P>
   Constrains ExprX and ExprY to be equal.  Also constrains all variables
   appearing in ExprX and ExprY to be integral and checks that all constants
   are integers.</P><P>

   Note that if all variables and constants appearing in this constraint
   are already integral, then the constraint is equivalent to
   ic:(ExprX =:= ExprY).</P>
")
]).
:- comment((#=)/3, [
    amode: #=(?, ?, ?),
    template: "#=(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is equal to ExprY (with integrality constraints).",
    see_also: [(#<)/3, (#=<)/3, (#>=)/3, (#>)/3, (#\=)/3,
               (=:=)/3, (#=)/2, _:(#=)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>
")
]).

%---------------------------------------------------------------------

:- comment((#>=)/2, [
    amode: #>=(?, ?),
    template: "?ExprX #>= ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is greater than or equal to ExprY (with integrality constraints).",
    see_also: [(#<)/2, (#=<)/2, (#=)/2, (#>)/2, (#\=)/2,
               (>=)/2, (#>=)/3, _:(#>=)/2],
    desc: html("<P>
   Constrains ExprX to be greater than or equal to ExprY.  Also constrains
   all variables appearing in ExprX and ExprY to be integral and checks that
   all constants are integers.</P><P>

   Note that if all variables and constants appearing in this constraint
   are already integral, then the constraint is equivalent to
   ic:(ExprX >= ExprY).</P>
")
]).

:- comment((#>=)/3, [
    amode: #>=(?, ?, ?),
    template: "#>=(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is greater than or equal to ExprY (with integrality constraints).",
    see_also: [(#<)/3, (#=<)/3, (#=)/3, (#>)/3, (#\=)/3,
               (>=)/3, (#>=)/2, _:(#>=)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>
")
]).

%---------------------------------------------------------------------

:- comment((#=<)/2, [
    amode: #=<(?, ?),
    template: "?ExprX #=< ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is less than or equal to ExprY (with integrality constraints).",
    see_also: [(#<)/2, (#=)/2, (#>=)/2, (#>)/2, (#\=)/2,
               (=<)/2, (#=<)/3, _:(#=<)/2],
    desc: html("<P>
   Constrains ExprX to be less than or equal to ExprY.  Also constrains all
   variables appearing in ExprX and ExprY to be integral and checks that all
   constants are integers.</P><P>

   Note that if all variables and constants appearing in this constraint
   are already integral, then the constraint is equivalent to
   ic:(ExprX =< ExprY).</P>
")
]).
:- comment((#=<)/3, [
    amode: #=<(?, ?, ?),
    template: "#=<(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is less than or equal to ExprY (with integrality constraints).",
    see_also: [(#<)/3, (#=)/3, (#>=)/3, (#>)/3, (#\=)/3,
               (=<)/3, (#=<)/2, _:(#=<)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>
")
]).

%---------------------------------------------------------------------

:- comment((#>)/2, [
    amode: #>(?, ?),
    template: "?ExprX #> ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is strictly greater than ExprY (with integrality constraints).",
    see_also: [(#<)/2, (#=<)/2, (#=)/2, (#>=)/2, (#\=)/2,
               (>)/2, (#>)/3, _:(#>)/2],
    desc: html("<P>
   Constrains ExprX to be greater than ExprY.  Also constrains all variables
   appearing in ExprX and ExprY to be integral and checks that all constants
   are integers.</P>
")
]).
:- comment((#>)/3, [
    amode: #>(?, ?, ?),
    template: "#>(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is strictly greater than ExprY (with integrality constraints).",
    see_also: [(#<)/3, (#=<)/3, (#=)/3, (#>=)/3, (#\=)/3,
               (>)/3, (#>)/2, _:(#>)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>
")
]).

%---------------------------------------------------------------------

:- comment((#<)/2, [
    amode: #<(?, ?),
    template: "?ExprX #< ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is less than ExprY (with integrality constraints).",
    see_also: [(#=<)/2, (#=)/2, (#>=)/2, (#>)/2, (#\=)/2,
               (<)/2, (#<)/3, _:(#<)/2],
    desc: html("<P>
   Constrains ExprX to be less than ExprY.  Also constrains all variables
   appearing in ExprX and ExprY to be integral and checks that all constants
   are integers.</P>
")
]).

:- comment((#<)/3, [
    amode: #<(?, ?, ?),
    template: "#<(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is less than ExprY (with integrality constraints).",
    see_also: [(#=<)/3, (#=)/3, (#>=)/3, (#>)/3, (#\=)/3,
               (<)/3, (#<)/2, _:(#<)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>
")
]).

%---------------------------------------------------------------------

:- comment((#\=)/2, [
    amode: #\=(?, ?),
    template: "?ExprX #\\= ?ExprY",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression"
    ],
    summary: "ExprX is not equal to ExprY (with integrality constraints).",
    see_also: [(#<)/2, (#=<)/2, (#=)/2, (#>=)/2, (#>)/2,
               (=\=)/2, (#\=)/3, _:(#\=)/2],
    desc: html("<P>
   Constrains ExprX to be not equal to ExprY.  Also constrains all variables
   appearing in ExprX and ExprY to be integral and checks that all constants
   are integers.</P><P>

   Note that if all variables and constants appearing in this constraint
   are already integral, then the constraint is equivalent to
   ic:(ExprX =\\= ExprY).</P>
")
]).

:- comment((#\=)/3, [
    amode: #\=(?, ?, ?),
    template: "#\\=(?ExprX, ?ExprY, ?Bool)",
    args: [
	"ExprX": "Integer arithmetic expression",
	"ExprY": "Integer arithmetic expression",
	"Bool": "Reified truth value of the constraint"
    ],
    summary: "Reified ExprX is not equal to ExprY (with integrality constraints).",
    see_also: [(#<)/3, (#=<)/3, (#=)/3, (#>=)/3, (#>)/3,
               (=\=)/3, (#\=)/2, _:(#\=)/3],
    desc: html("<P>
   This predicate is a reified constraint: it succeeds if and only if the
   truth value of its associated constraint (the constraint with arity one
   less and the same arguments except for B) is equal to B, where the value
   0 means false and 1 true.  This constraint can be used both to test the
   validity of the associated constraint (entailment test) and to impose
   this constraint or its negation.  For the former, B will be instantiated
   as soon as either the associated constraint or its negation is subsumed
   by the current state of its domain variables.  For the latter, when B is
   instantiated, then depending on its value, either the associated
   constraint or its negation will be imposed on its arguments.</P>
")
]).


%---------------------------------------------------------------------

:- comment(indomain/1, [
    amode: indomain(?),
    args: [
    	"Var": "An integer IC variable or an integer"
    ],
    resat: "Yes.",
    summary: "Instantiates an integer IC variable to an element of its domain.",
    see_also: [labeling/1, (::)/2, _:indomain/1],
    desc: html("<P>
   Simple predicate for instantiating an integer IC variable to an element
   of its domain.  It starts with the smallest element, and upon
   backtracking tries successive elements until the entire domain has been
   explored, at which point the predicate fails.</P><P>

   If Var is already a ground integer, then this predicate simply succeeds
   exactly once without leaving a choicepoint.</P>
")
]).

%---------------------------------------------------------------------

:- comment(labeling/1, [
    amode: labeling(+),
    args: [
    	"Vars": "A collection (a la collection_to_list/2) of integer IC variables or integers"
    ],
    resat: "Yes.",
    summary: "Instantiates all variables in a collection to elements of their domains.",
    see_also: [indomain/1, _:labeling/1, collection_to_list/2],
    desc: html("<P>
   Simple predicate for instantiating a collection of integer IC variables
   to elements of their domains.  (Integers are also allowed in the
   collection; they are effectively ignored.)  The variables are
   instantiated in the order in which they appear in the collection; the
   implementation is essentially:
<PRE>
	labeling(Vars) :-
		collection_to_list(Vars, List),
		( foreach(Var,List) do
		    indomain(Var)
		).
</PRE></P>
")
]).

%---------------------------------------------------------------------

:- comment(alldifferent/1, [
    amode: alldifferent(+),
    args: [
    	"Vars": "A collection (a la collection_to_list/2) of variables or integers"
    ],
    summary: "Constrains all elements of a list to be pairwise different.",
    see_also: [(#\=)/2, _:alldifferent/1, collection_to_list/2],
    desc: html("<P>
   Constrains all elements of a collection to be pairwise different (and
   integral).  Effectively imposes #\\= constraints on every pair of
   elements from Vars.</P>
")
]).

%---------------------------------------------------------------------
%
% Documentation for constraints reexported from ic_constraints.
%

:- comment(locate/2, [
    amode: locate(+, ++),
    args: [
	"Vars":      "Collection (a la collection_to_list/2) of variables",
	"Precision": "Minimum required precision (float)"
    ],
    summary: "Locate solution intervals for Vars by splitting and search.",
    see_also: [locate/3, locate/4, squash/3, collection_to_list/2],
    desc: html("<P>
   Locate solution intervals for the given variables with the required
   precision.  This works well if the problem has a finite number of
   solutions.  locate/2,3 work by nondeterministically splitting the
   intervals of the variables until they are narrower than Precision (in
   either absolute or relative terms).</P><P>

   locate/2 is defined in terms of locate/3 as follows:
    <PRE>
    locate(Vars, Precision) :- locate(Vars, Precision, log).
    </PRE></P>
")
]).

%---------------------------------------------------------------------

:- comment(locate/3, [
    amode: locate(+, ++, ++),
    args: [
	"Vars":      "Collection (a la collection_to_list/2) of variables",
	"Precision": "Minimum required precision (float)",
	"LinLog":    "Domain splitting method (lin or log)"
    ],
    summary: "Locate solution intervals for Vars by splitting and search.",
    see_also: [locate/2, locate/4, squash/3, collection_to_list/2],
    desc: html("<P>
   Locate solution intervals for the given variables with the required
   precision.  This works well if the problem has a finite number of
   solutions.  locate/2,3 work by nondeterministically splitting the
   intervals of the variables until they are narrower than Precision (in
   either absolute or relative terms).</P><P>

   The LinLog parameter guides the way domains are split.  If it is set to
   <TT>lin</TT> then the split is linear (i.e. the arithmetic mean of the bounds is
   used).  If it is set to <TT>log</TT>, the split is logarithmic (i.e. the geometric
   mean of the bounds is used).  Note that if <TT>log</TT> is used, there will be
   roughly the same number of representable floating point numbers on either
   side of the split, due to the logarithmic distribution of these numbers.</P><P>

   locate/3 is defined in terms of locate/4 as follows:
    <PRE>
    locate(Vars, Precision, LinLog) :- locate(Vars, [], Precision, LinLog).
    </PRE></P>
")
]).

%---------------------------------------------------------------------

:- comment(locate/4, [
    amode: locate(+, +, ++, ++),
    args: [
	"LocateVars": "Collection (a la collection_to_list/2) of variables",
	"SquashVars": "Collection of variables",
	"Precision":  "Minimum required precision (float)",
	"LinLog":     "Domain splitting method (lin or log)"
    ],
    summary: "Locate solution intervals for LocateVars, interleaving search with squashing.",
    see_also: [locate/2, locate/3, squash/3, collection_to_list/2],
    desc: html("<P>
   A variant of locate/2,3 with interleaved squashing.  The squash algorithm
   is applied once to the SquashVars initially, and then again after each
   splitting step (i.e. each time one of the LocateVars has been split
   nondeterministically during the search).  A variable may occur in both
   LocateVars and SquashVars.</P><P>

   The LinLog parameter guides the way domains are split.  If it is set to
   <TT>lin</TT> then the split is linear (i.e. the arithmetic mean of the bounds is
   used).  If it is set to <TT>log</TT>, the split is logarithmic (i.e. the geometric
   mean of the bounds is used).  Note that if <TT>log</TT> is used, there will be
   roughly the same number of representable floating point numbers on either
   side of the split, due to the logarithmic distribution of these numbers.</P>
")
]).

%---------------------------------------------------------------------

:- comment(squash/3, [
    amode: squash(+, ++, ++),
    args: [
	"Vars":      "Collection (a la collection_to_list/2) of variables",
	"Precision": "Minimum required precision (float)",
	"LinLog":    "Domain splitting method (lin or log)"
    ],
    summary: "Refine the intervals of Vars by the squashing algorithm.",
    see_also: [locate/2, locate/3, locate/4, set_threshold/1,
	    get_threshold/1, collection_to_list/2],
    desc: html("<P>
   Use the squash algorithm on Vars.  This is a deterministic reduction of
   the intervals of variables, done by searching for domain restrictions
   which cause failure, and then reducing the domain to the complement of
   that which caused the failure.  This algorithm is appropriate when the
   problem has continuous solution intervals (where locate would return many
   adjacent solutions).</P><P>

   Precision is the minimum required precision, i.e. the maximum size of the
   resulting intervals (in either absolute or relative terms).  Note that
   the arc-propagation threshold (set by set_threshold/1,2), needs to be one
   or several orders of magnitude smaller than Precision, otherwise the
   solver may not be able to achieve the required precision.</P><P>

   The LinLog parameter guides the way domains are split.  If it is set to
   <TT>lin</TT> then the split is linear (i.e. the arithmetic mean of the bounds is
   used).  If it is set to <TT>log</TT>, the split is logarithmic (i.e. the geometric
   mean of the bounds is used).  Note that if <TT>log</TT> is used, there will be
   roughly the same number of representable floating point numbers on either
   side of the split, due to the logarithmic distribution of these numbers.</P><P>

   If the intervals of variables at the start of the squashing algorithm are
   known not to span several orders of magnitude, the somewhat cheaper
   linear splitting may be used.  In general, log splitting is recommended.</P>
")
]).

%---------------------------------------------------------------------

:- comment(max/2, [
    amode: max(+, ?),
    args: [
	"Vars": "Collection (a la collection_to_list/2) of variables or numbers (NOT arbitrary expressions)",
	"Max":  "Maximum element of Vars"
    ],
    summary: "Constrains Max to be the largest element in Vars.",
    see_also: [min/2, collection_to_list/2],
    desc: html("<P>
   Constrains Max to be the largest element in the collection of variables
   or numbers Vars.</P><P>

   You may find it more convenient to embed <TT>max(Vars)</TT> in a
   constraint.
")
]).

%---------------------------------------------------------------------

:- comment(min/2, [
    amode: min(+, ?),
    args: [
	"Vars": "Collection (a la collection_to_list/2) of variables or numbers (NOT arbitrary expressions)",
	"Min":  "Minimum element of Vars"
    ],
    summary: "Constrains Min to be the smallest element in Vars.",
    see_also: [max/2, collection_to_list/2],
    desc: html("<P>
   Constrains Max to be the smallest element in the collection of variables
   or numbers Vars.</P><P>

   You may find it more convenient to embed <TT>min(Vars)</TT> in a
   constraint.
")
]).

%---------------------------------------------------------------------

:- comment(maxlist/2, [
    amode: maxlist(+, ?),
    args: [
	"Vars": "Collection (a la collection_to_list/2) of variables or numbers (NOT arbitrary expressions)",
	"Max":  "Maximum element of Vars"
    ],
    summary: "Constrains Max to be the largest element in Vars.",
    see_also: [max/2, min/2, collection_to_list/2],
    desc: html("<P>
   This predicate is obsolete.  Use <TT>max/2</TT> instead.
")
]).

%---------------------------------------------------------------------

:- comment(minlist/2, [
    amode: minlist(+, ?),
    args: [
	"Vars": "Collection (a la collection_to_list/2) of variables or numbers (NOT arbitrary expressions)",
	"Min":  "Minimum element of Vars"
    ],
    summary: "Constrains Min to be the smallest element in Vars.",
    see_also: [min/2, max/2, collection_to_list/2],
    desc: html("<P>
   This predicate is obsolete.  Use <TT>min/2</TT> instead.
")
]).

%---------------------------------------------------------------------
:- comment((and)/2, [
    amode: and(+, +),
    template: "+ConX and +ConY",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint"
    ],
    summary: "Constraints ConX and ConY must both be true.",
    see_also: [(and)/3, (neg)/1, (neg)/2, (or)/2, (or)/3, (=>)/2,
               (=>)/3, (=:=)/3, (=<)/3, (=\=)/3, (>=)/3, (>)/3, (<)/3],
    desc: html("<P>
   Equivalent to BX $= (ConX), BY $= (ConY), BX + BY #= 2</P>
   <P>
   The two constraints are reified in such a way that both must be true.
   ConX and ConY must be a constraints that have a corresponding reified
   form.
")
]).

:- comment((and)/3, [
    amode: and(+, +, ?),
    template: "and(+ConX,+ConY,Bool)",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint",
        "Bool": "Reified truth value of the constraint"
    ],
    summary: "Bool is the reified truth of both constraints ConX and ConY being true.",
    see_also: [(and)/2, (neg)/1, (neg)/2, (or)/2, (or)/3, (=>)/2,
               (=>)/3, (=:=)/3, (=<)/3, (=\=)/3, (>=)/3, (>)/3, (<)/3],
    desc: html("<P>
   Equivalent to BX $= (ConX), BY $= (ConY), Bool #= (BX + BY #= 2)</P>
   <P>
   The two constraints are reified in such a way that Bool reflects the
   truth of both being true.  ConX and ConY must be constraints that have a
   corresponding reified form.
")
]).

%---------------------------------------------------------------------
:- comment((or)/2, [
    amode: or(+, +),
    template: "+ConX or +ConY",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint"
    ],
    summary: "At least one of the constraints ConX or ConY must be true.",
    see_also: [(or)/3, (neg)/1, (neg)/2, (and)/2, (and)/3, (=>)/2,
               (=>)/3, (=:=)/3, (=<)/3, (=\=)/3, (>=)/3, (>)/3, (<)/3],
    desc: html("<P>
   Equivalent to BX $= (ConX), BY $= (ConY), BX + BY #>= 1</P>
   <P>
   The two constraints are reified in such a way that at least one must be
   true.  ConX and ConY must be constraints that have a corresponding
   reified form.
")
]).

:- comment((or)/3, [
    amode: or(+, +, ?),
    template: "or(+ConX,+ConY,Bool)",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint",
        "Bool": "Reified truth value of the constraint"
    ],
    summary: "Bool is the reified truth of at least one of the constraints ConX or ConY being true.",
    see_also: [(or)/2, (neg)/1, (neg)/2, (and)/2, (and)/3, (=>)/2,
               (=>)/3, (=:=)/3, (=<)/3, (=\=)/3, (>=)/3, (>)/3, (<)/3],
    desc: html("<P>
   Equivalent to BX $= (ConX), BY $= (ConY), Bool #= (BX + BY #>= 1)</P>
   <P>
   The two constraints are reified in such a way that Bool reflects the
   truth of at least one being true.  ConX and ConY must be constraints that
   have a corresponding reified form.
")
]).

%---------------------------------------------------------------------
:- comment((=>)/2, [
    amode: =>(+, +),
    template: "+ConX => +ConY",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint"
    ],
    summary: "Constraint ConX being true implies ConY must both be true.",
    see_also: [(=>)/3, (neg)/1, (neg)/2, (or)/2, (or)/3, (and)/2,
               (and)/3, (=:=)/3, (=<)/3, (=\=)/3, (>=)/3, (>)/3, (<)/3],
    desc: html("<P>
   Equivalent to BX $= (ConX), BY $= (ConY), BX #=< BY</P>
   <P>
   The two constraints are reified in such a way that ConX being true
   implies that ConY must also be true.  ConX and ConY must be constraints
   that have a corresponding reified form.
")
]).

:- comment((=>)/3, [
    amode: =>(+, +, ?),
    template: "=>(+ConX,+ConY,Bool)",
    args: [
	"ConX": "Constraint",
	"ConY": "Constraint",
        "Bool": "Reified truth value of the constraint"
    ],
    summary: "Bool is the reified truth of constraint ConX implying the truth of ConY.",
    see_also: [(=>)/2, (neg)/1, (neg)/2, (or)/2, (or)/3, (and)/2,
               (and)/3, (=:=)/3, (=<)/3, (=\=)/3, (>=)/3, (>)/3, (<)/3],
    desc: html("<P>
   Equivalent to BX $= (ConX), BY $= (ConY), Bool #= (BX #=< BY)</P>
   <P>
   The two constraints are reified in such a way that Bool is true if ConX
   being true implies that ConY must also be true.  ConX and ConY must be
   constraints that have a corresponding reified form.
")
]).

%---------------------------------------------------------------------

:- comment((neg)/1, [
    amode: neg(+),
    template: "neg(+Con)",
    args: [
	"Con": "Constraint"
    ],
    summary: "Constraints Con is negated.",
    see_also: [(and)/2, (and)/3, (neg)/2, (or)/2, (or)/3, (=>)/2,
               (=>)/3, (=:=)/3, (=<)/3, (=\=)/3, (>=)/3, (>)/3, (<)/3],
    desc: html("<P>
   Equivalent to 0 $= (Con)</P>
   <P>
   The reified constraint Con is negated.  Con must be a constraint that has
   a corresponding reified form.
")
]).

:- comment((neg)/2, [
    amode: neg(+, ?),
    template: "neg(+Con,Bool)",
    args: [
	"Con": "Constraint",
        "Bool": "Reified truth value of the constraint"
    ],
    summary: "Bool is the logical negation of the reified truth constraints Con.",
    see_also: [(and)/2, (and)/3, (neg)/1, (or)/2, (or)/3, (=>)/2,
               (=>)/3, (=:=)/3, (=<)/3, (=\=)/3, (>=)/3, (>)/3, (<)/3],
    desc: html("<P>
   Equivalent to B $= (Con), Bool #= 1-B</P>
   <P>
   Bool is the logical negation of the reified truth constraint Con.  Con
   must be a constraint that has a corresponding reified form.
")
]).

%---------------------------------------------------------------------

:- comment(element/3, [
	summary:"Value is the Index'th element of the integer list List.",
	template:"element(?Index, ++List, ?Value)",
	args:[
	    "?Index" : "A variable or an integer.",
	    "++List" : "A non-empty list or array of integers.",
	    "?Value" : "A variable or an integer."
	],
	resat:"No.",
	fail_if:"Fails if Value is not the Index'th element of List.",
	desc:html("This constraint can be used in a variety of programs to state a
   relation between two domain variables.  List is a list of integers and
   the constraint states that its Index'th element is equal to Value, i.e.
<P>
<PRE>
			     List_Index = Value
</PRE>
   Every time Index or Value is updated, this constraint is activated and
   the domain of the other variable is updated accordingly.
<P>
"),
	eg:"
   [eclipse 13]: element(I, [1,3,6,3], V).
   I = I{1 .. 4}
   V = V{[1, 3, 6]}
   Delayed goals:
   element(I{1 .. 4}, [1, 3, 6, 3], V{[1, 3, 6]})
   yes.
   [eclipse 14]: element(I, [1,3,6,3], V), V #\\= 3.
   I = I{[1, 3]}
   V = V{[1, 6]}
   Delayed goals:
   element(I{[1, 3]}, [1, 3, 6, 3], V{[1, 6]})
   yes.
",
	see_also:[/(::, 2)]
    ]).

%---------------------------------------------------------------------

:- comment(ac_eq/3, [
    amode: ac_eq(?, ?, ++),
    args: [
	"X": "Variable or integer",
	"Y": "Variable or integer",
	"C": "Integer constant"
    ],
    summary: "Arc-consistent implementation of X #= Y + C.",
    see_also: [(#=)/2],
    desc: html("<P>
   This constraint implements an arc-consistent version of X #= Y + C (i.e.
   any \"holes\" in the domain of X are propagated to Y and vice-versa).<P>

   A range error is thrown if X and Y do not have \"reasonable\" bounds
   (roughly +/- 2^30 on a 32-bit machine, roughly +/- 2^52 on a 64-bit
   machine: note that even much smaller bounds than this can lead to
   available memory being exhausted very rapidly, if not immediately).
")
]).

%---------------------------------------------------------------------

:- comment(piecewise_linear/3, [
amode: piecewise_linear(?,++,?),
args:["X":"Parameter/domain of the piecewise function",
      "Points":"Collection (a la collection_to_list/2) of points defining the piecewise function",
      "Y":"Result/interval of piecewise the function"
     ],
summary: "Relates X and Y according to a piecewise linear function.",
see_also: [piecewise_linear_hull/3, collection_to_list/2],
desc: html("<P>
   This predicate imposes the constraint Y = f(X), where f is a piecewise
   linear function defined by Points.  Points must be (after processing by
   collection_to_list/2) a list of elements of the form (X, Y), sorted by
   increasing X values.  The function is constructed by performing linear
   interpolation between consecutive points, and extrapolating the first and
   last linear segments to infinity.
</P><P>
   Discontinuities are allowed, by specifying more than one point with the
   same X coordinate.  However, since the constraint is a function, one must
   specify which of the given Y values is the true value of the function at
   that point.  This is done by annotating which of the adjacent line
   segments is ``open'' at that end (i.e. does not include the end point).
   There are three allowed forms of discontinuity:
</P><P>
	(X1, Y1), (X2, Y2a), (>(X2), Y2b), (X3, Y3) </P><P>
	(X1, Y1), (&lt;(X2), Y2a), (X2, Y2b), (X3, Y3) </P><P>
	(X1, Y1), (&lt;(X2), Y2a), (X2, Y2b), (>(X2), Y2c), (X3, Y3)
</P><P>
   In the first form, the segment specified by the first two points is
   defined over the interval [X1 .. X2] while that specified by the last two
   is defined over the interval (X2 .. X3].  That is, the first interval is
   closed at X2, the second is open at X2 (i.e. only includes points
   strictly greater than X2), and thus the value of the function at X2 is
   Y2a.
</P><P>
   In the second form, the reverse is true.  The first segment is over the
   interval [X1 .. X2) (points less than X2), the second is over the
   interval [X2 .. X3], and the value of the function at X2 is Y2b.
</P><P>
   The final form is a double discontinuity.  Both adjacent segments are
   open - [X1 .. X2), (X2 .. X3] - and at X2 the value of the function is
   Y2b, taken from neither segment.
</P><P>
   Note that a segment can be open at both ends, if it has discontinuities
   at both ends.  For example, if the list of points is
</P><P>
	[(0, 0), (10, 0), (>(10), 1), (&lt;(20), 1), (20, 2), (30, 2)]
</P><P>
   then the function has value 0 for all values of X up to and including 10,
   it has value 2 for all values of X from 20 onwards, and has value 1 in
   between.

</P><P>
   Other notes:
</P><P>
   A piecewise linear constraint is expected to have at least two points
   (though if it only has two, then a standard linear constraint would do,
   and likely be more efficient).  If there is only one point, then there is
   no way to determine the gradient to use to extend the function to other
   values of X.  As a result, the value of the function is not defined for
   all other values of X.  Similarly, if the first or last points in the
   specification of the piecewise constraint are involved in
   discontinuities, then the corresponding extensions to infinity are
   undefined; it is recommended that the user either provide a valid
   extension by supplying an extra point, or exclude those values of X by
   imposing an appropriate bound.  (In any event, the first and last points
   must be closed; an open discontinuity is not permitted at either end.)
</P><P>
   Whilst the piecewise constraint accepts bounded reals in its arguments,
   the current implementation does not fully support bounded reals of
   non-zero width (i.e. those which do not correspond to a single floating
   point value).  As a result, use of such bounded reals is not recommended
   at this time.</P>
")
]).

