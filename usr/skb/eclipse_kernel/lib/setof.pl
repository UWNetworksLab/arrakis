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
% Contributor(s): R.A.O'Keefe and David Warren
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: setof.pl,v 1.2.2.1 2008/12/17 09:43:54 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	setof.pl
 *
 * DESCRIPTION: 	Implements setof features
 *			See comments below.
 *
 * CONTENTS:     
 *
 */

%   File   : SETOF.PL
%   Author : R.A.O'Keefe
%   Updated: 17 November 1983
%   Purpose: define setof/3, bagof/3, findall/3, and findall/4
%   Needs  : Not.Pl
%   Updated: 21 October 1987 by micha, included a part of not.pl
%   Updated: 21 Feb 1990 by joachim, using erase_list/3 and occurs/2
%   Updated: 25 May 1990 by micha, made it invisible to the debugger
%   Updated: 2 Nov 1992 by joachim, made nodbgcomp, added useful modes
%   Updated: 14 Oct 1994 by joachim, modified to use bag_enter/2 etc

/*  This file defines two predicates which act like setof/3 and bagof/3.
    I have seen the code for these routines in Dec-10 and in C-Prolog,
    but I no longer recall it, and this code was independently derived
    in 1982 by me and me alone.

    Most of the complication comes from trying to cope with free variables
    in the Filter; these definitions actually enumerate all the solutions,
    then group together those with the same bindings for the free variables.
    There must be a better way of doing this.  I do not claim any virtue for
    this code other than the virtue of working.  In fact there is a subtle
    bug: if setof/bagof occurs as a data structure in the Generator it will
    be mistaken for a call, and free variables treated wrongly.  Given the
    current nature of Prolog, there is no way of telling a call from a data
    structure, and since nested calls are FAR more likely than use as a
    data structure, we just put up with the latter being wrong.  The same
    applies to negation.

    Would anyone incorporating this in their Prolog system please credit
    both me and David Warren;  he thought up the definitions, and my
    implementation may owe more to subconscious memory of his than I like
    to think.  At least this ought to put a stop to fraudulent claims to
    having bagof, by replacing them with genuine claims.

    Thanks to Dave Bowen for pointing out an amazingly obscure bug: if
    the Template was a variable and the Generator never bound it at all
    you got a very strange answer!  Now fixed, at a price.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- system.

:- export
	findall/3,
	setof/3,
	coverof/3,
	bagof/3,
	(^)/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%   findall(Template, Generator, List)
%   is a special case of bagof, where all free variables in the
%   generator are taken to be existentially quantified.  It is
%   described in Clocksin & Mellish on p152.  The code they give
%   has a bug (which the Dec-10 bagof and setof predicates share)
%   which this has not.

findall_body(Template, Generator, List, Module) :-
	bag_create(Ref),
	recordz_instances(Template, Generator, Module, Ref),
	bag_dissolve(Ref, List).


%   setof(Template, Generator, Set)
%   finds the Set of instances of the Template satisfying the Generator..
%   The set is in ascending order (see compare/3 for a definition of
%   this order) without duplicates, and is non-empty.  If there are
%   no solutions, setof fails.  setof may succeed more than one way,
%   binding free variables in the Generator to different values.  This
%   predicate is defined on p51 of the Dec-10 Prolog manual.

setof_body(Template, Filter, Set, Module) :-
	bagof_body(Template, Filter, Bag, Module),
	sort(0, <, Bag, Set).


%   coverof(Template, Generator, Set)
%   works like setof/3, however the list of solutions is not sorted
%   and only the most general instances are retained

coverof_body(Template, Filter, Set, Module) :-
	bagof_body(Template, Filter, Bag, Module),
	prune_instances(Bag, Set).



%   bagof(Template, Generator, Bag)
%   finds all the instances of the Template produced by the Generator,
%   and returns them in the Bag in they order in which they were found.
%   If the Generator contains free variables which are not bound in the
%   Template, it assumes that this is like any other Prolog question
%   and that you want bindings for those variables.  (You can tell it
%   not to bother by using existential quantifiers.)
%   bagof records two things:
%	terms with no free variables   -Term
%	terms with free variables   Key-Term
%   The second clause is basically just findall, which of course works in
%   the common case when there are no free variables.

bagof_body(Template, Generator, Bag, Module) :-
	free_variables(Generator, Template, [], Vars),
	Vars \== [],
	!,
	Key =.. [.|Vars],
	functor(Key, ., N),
	bag_create(Ref),
	recordz_instances(Key-Template, Generator, Module, Ref),
	collect_instances(Key, N, Ref, Bag).
bagof_body(Template, Generator, Bag, Module) :-
	bag_create(Ref),
	recordz_instances(Template, Generator, Module, Ref),
	bag_dissolve(Ref, Bag),
	Bag \== [].

:- mode collect_instances(+,++,++,?).
collect_instances(Key, N, Ref, Bag) :-
	list_instances(Key, N, Ref, OmniumGatherum),
	keysort(OmniumGatherum, Gamut),
	concordant_subset(Gamut, Key, Answer),
	Bag = Answer.


%    recordz_instances(Template, Generator, Module, Ref)
%    enumerates all provable instances of the Generator and records the
%    associated Template instances.  Neither argument ends up changed.

% :- sequential recordz_instances/4.
recordz_instances(Template, Generator, Module, Ref) :-
	call_local(Generator, Module),
%	true,			% force waking before recording
	bag_enter(Ref, Template),
	fail.
recordz_instances(_, _, _, _).



%   list_instances(Key, NVars, Ref, Bag)
%   pulls all the Key-Template instances out of the data base.
%   Note that asserting something into the data base and pulling it out
%   again renames all the variables; to counteract this we use replace_
%   key_variables to put the old variables back.  Fortunately if we
%   bind X=Y, the newer variable will be bound to the older, and the
%   original key variables are guaranteed to be older than the new ones.
%   This replacement must be done @i<before> the keysort.


list_instances(Key, NVars, Ref, Bag) :-
	bag_dissolve(Ref, Bag),
	replace_all_key_variables(Bag, Key, NVars).

replace_all_key_variables([], _, _).
replace_all_key_variables([NewKey-_Term|Templates], Key, NVars) :-
	replace_key_variables(NVars, Key, NewKey),
	replace_all_key_variables(Templates, Key, NVars).


replace_key_variables(0, _, _) :- !.
replace_key_variables(N, OldKey, NewKey) :-
	arg(N, NewKey, Arg),
	replace_key_variable(Arg, OldKey, N),
	M is N-1,
	replace_key_variables(M, OldKey, NewKey).

replace_key_variable(Arg, OldKey, N) :-
	free(Arg),
	arg(N, OldKey, Arg).
replace_key_variable(Arg, OldKey, N) :-
	meta(Arg),
	arg(N, OldKey, OldArg),
	( variant(Arg, OldArg) ->
	    meta_bind(Arg, OldArg)
	;
	    true
	).
replace_key_variable(Arg, _, _) :-
	nonvar(Arg).

%   concordant_subset([Key-Val list], Key, [Val list]).
%   takes a list of Key-Val pairs which has been keysorted to bring
%   all the identical keys together, and enumerates each different
%   Key and the corresponding lists of values.

concordant_subset([Key-Val|Rest], Clavis, Answer) :-
	concordant_subset(Rest, Key, List, More),
	concordant_subset(More, Key, [Val|List], Clavis, Answer).


%   concordant_subset(Rest, Key, List, More)
%   strips off all the Key-Val pairs from the from of Rest,
%   putting the Val elements into List, and returning the
%   left-over pairs, if any, as More.

:- mode	concordant_subset(+,+,-,-).
concordant_subset([Key-Val|Rest], Clavis, [Val|List], More) :-
	Key == Clavis,
	!,
	concordant_subset(Rest, Clavis, List, More).
concordant_subset(More, _, [], More).


%   concordant_subset/5 tries the current subset, and if that
%   doesn't work if backs up and tries the next subset.  The
%   first clause is there to save a choice point when this is
%   the last possible subset.

:- mode	concordant_subset(+,+,+,+,-).
concordant_subset([],   Key, Subset, Key, Subset) :- !.
concordant_subset(_,    Key, Subset, Key, Subset).
concordant_subset(More, _,   _,   Clavis, Answer) :-
	concordant_subset(More, Clavis, Answer).


%   In order to handle variables properly, we have to find all the 
%   universally quantified variables in the Generator.  All variables
%   as yet unbound are universally quantified, unless
%	a)  they occur in the template
%	b)  they are bound by X^P, setof, or bagof
%   free_variables(Generator, Template, OldList, NewList)
%   finds this set, using OldList as an accumulator.

free_variables(Term, Bound, OldVarList, VarList) :-
	var(Term),
	!,
	(
	occurs(Term, Bound) ->
		VarList = OldVarList
	;
	occurs(Term, OldVarList) ->
		VarList = OldVarList
	;
		VarList = [Term|OldVarList]
	).
free_variables(Term, Bound, OldList, NewList) :-
	explicit_binding(Term, Bound, NewTerm, NewBound),
	!,
	free_variables(NewTerm, NewBound, OldList, NewList).
free_variables(Term, Bound, OldList, NewList) :-
	arity(Term, N),
	free_variables(N, Term, Bound, OldList, NewList).

:- mode	free_variables(+,+,+,+,-).
free_variables(0, _Term, _Bound, VarList, VarList) :- !.
free_variables(N, Term, Bound, OldList, NewList) :-
	arg(N, Term, Argument),
	free_variables(Argument, Bound, OldList, MidList),
	M is N-1, !,
	free_variables(M, Term, Bound, MidList, NewList).

%   explicit_binding checks for goals known to existentially quantify
%   one or more variables.  In particular \+ is quite common.

:- mode	explicit_binding(+,+,-,-).
explicit_binding(\+ _,		       Bound, fail,	Bound      ) :- !.
explicit_binding(not(_),	       Bound, fail,	Bound	   ) :- !.
explicit_binding(fail_if(_),	       Bound, fail,	Bound	   ) :- !.
explicit_binding(Var^Goal,	       Bound, Goal,	Bound+Var) :- !.
explicit_binding(setof(Var,Goal,Set),  Bound, Goal-Set, Bound+Var) :- !.
explicit_binding(bagof(Var,Goal,Bag),  Bound, Goal-Bag, Bound+Var) :- !.
explicit_binding(coverof(Var,Goal,Bag),  Bound, Goal-Bag, Bound+Var) :- !.

% For proper tracing behaviour, this file must be in nodbgcomp
% and the metapredicates must be set to unskipped. This will cause
% only the metacalls of the user goals to show up in the trace.

:- unskipped
	setof_body/4,
	bagof_body/4,
	coverof_body/4,
	findall_body/4.

:- set_flag(setof_body/4, trace_meta, on).
:- set_flag(bagof_body/4, trace_meta, on).
:- set_flag(coverof_body/4, trace_meta, on).
:- set_flag(findall_body/4, trace_meta, on).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% auxiliaries

exquant_body(_, Goal, Module) :-
	untraced_call(Goal, Module).

:- unskipped exquant_body/3.
:- set_flag(exquant_body/3, trace_meta, on).
