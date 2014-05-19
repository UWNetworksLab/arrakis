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
% Version:	$Id: apply_macros.pl,v 1.1 2008/06/30 17:43:41 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(apply_macros).

:- comment(summary, "Utilities to apply a predicate to all elements of a list resp. all subterms of a term").
:- comment(author, "Joachim Schimpf, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:41 $").
:- comment(desc, html("
    Note that this library is largely superseded by the do-loop construct!
    <P>
    A collection of utilities to apply a predicate to
    all elements of a list resp. all subterm of a term.
    To avoid performance degradation due to apply/2,
    they are implemented as macros, i.e. they are specialized
    into auxiliary predicates without metacalls, and the
    calls are translated into calls of the auxiliary predicates.")
    ).

:- export
	fromto/4,
	maplist/3,
	mapstream/3,
	mapargs/3,
	appnodes/2,
	sumargs/4,
	checklist/2,
	applist/2,
	selectlist/3,
	sumlist/4,
	sumnodes/4.


:- comment(fromto/4, [template:"fromto(+From, +To, +Step, +Pred)",
    summary:"Call Pred with the numbers From..To in increments of Step"
    ]).
:- comment(maplist/3, [template:"maplist(+Pred, +ListIn, ?ListOut)",
    summary:"Create new list by applying a predicate to all list elements",
    eg:"maplist(times(3), [1,2,3,4], [3,6,9,12])."
    ]).
:- comment(mapstream/3, [template:"mapstream(+Pred, ?ListIn, ?ListOut)",
    summary:"Like maplist/3, but delays if ListIn is not complete"
    ]).
:- comment(mapargs/3, [template:"mapargs(+Pred, +TermIn, ?TermOut)",
    summary:"Create new term by applying a predicate to all arguments",
    eg:"mapargs(atom_string, s(a,b,c), s(\"a\",\"b\",\"c\"))."
    ]).
:- comment(appnodes/2, [template:"appnodes(+Pred, +Term)",
    summary:"Call Pred on all Subterms of Term (depth-first and left-to-right order)"
    ]).
:- comment(sumargs/4, [template:"sumargs(+Pred, +Term, ?AccIn, ?AccOut)",
    summary:"Call Pred on all arguments of Term and collect a result in Accumulator",
    desc:"The traversal order is unspecified!"
    ]).
:- comment(checklist/2, [template:"checklist(+Pred, +List)",
    summary:"Apply a predicate to all list elements",
    eg:"checklist(<(0), [1,2,3])."
    ]).
:- comment(applist/2, [template:"applist(+Pred, +List)",
    summary:"Apply a predicate to all list elements",
    eg:"applist(<(0), [1,2,3])."
    ]).
:- comment(selectlist/3, [template:"selectlist(+Pred, +ListIn, ?ListOut)",
    summary:"Creates output list of all list elements that pass a given test",
    eg:"selectlist(<(0), [1,0,-2,3], [1,3])."
    ]).
:- comment(sumlist/4, [template:"sumlist(+Pred, +List, ?AccIn, ?AccOut)",
    summary:"Call Pred on all element of List and collect a result in Accumulator",
    eg:"
	sumlist(plus, [1,2,3,4], 1, 10).
	sumlist(times, [1,2,3,4], 1, 24)."
    ]).
:- comment(sumnodes/4, [template:"sumnodes(+Pred, +Term, ?AccIn, ?AccOut)",
    summary:"Call Pred on all Subterms of Term and collect a result in Accumulator",
    desc:"The traversal is depth-first and left-to-right",
    eg:"
	sumnodes(vars, s(1,t(X,2),[Y]), [], [X,Y]).
	where
	    vars(X, Vars, [X|Vars]) :- var(X), !.
	    vars(_, Vars, Vars).

	or even more elegant using grammar rules:

	sumnodes(vars, s(1,t(X,2),[Y]), [X,Y], []).
	where
	    vars(X) --> {var(X)} -> [X];[]."
    ]).


:- inline(fromto/4, t_fromto/3).
:- inline(maplist/3, t_maplist/3).
:- inline(mapstream/3, t_mapstream/3).
:- inline(mapargs/3, t_mapargs/3).
:- inline(appnodes/2, t_appnodes/3).
:- inline(sumargs/4, t_sumargs/3).
:- inline(checklist/2, t_checklist/3).
:- inline(applist/2, t_applist/3).
:- inline(selectlist/3, t_selectlist/3).
:- inline(sumlist/4, t_sumlist/3).
:- inline(sumnodes/4, t_sumnodes/3).


t_maplist(maplist(Pred, In, Out), NewGoal, Module) :-
	callable(Pred),
	!,
	analyse_pred(Pred, VarArgs, GenPred, GenVarArgs, Pattern),
	aux_pred_name(maplist, Pattern, NewName),
	append(VarArgs, [In, Out], NewGoalArgs),	% make new goal
	NewGoal =.. [NewName|NewGoalArgs],
	append_args(NewName, GenVarArgs, HeadPrefix),	% make new predicate
	append_args(HeadPrefix, [[], []], NilClause),
	append_args(HeadPrefix, [[Old|Olds], [New|News]], LoopHead),
	append_args(GenPred, [Old, New], Apply),
	append_args(HeadPrefix, [Olds, News], Recursion),
	compile_aux([
		NilClause,
		(LoopHead :- Apply, Recursion)
	], Module).


t_fromto(fromto(From, To, Step, Pred), NewGoal, Module) :-
	callable(Pred),
	number(Step),
	!,
	analyse_pred(Pred, VarArgs, GenPred, GenVarArgs, Pattern),
	aux_pred_name(fromto, (Step,Pattern), NewName),
	append(VarArgs, [From,To], NewGoalArgs),	% make new goal
	NewGoal =.. [NewName|NewGoalArgs],
	append_args(NewName, GenVarArgs, HeadPrefix),	% make new predicate
	append_args(HeadPrefix, [From0, To0], LoopHead),
	append_args(GenPred, [From0], Apply),
	append_args(HeadPrefix, [From1,To0], Recursion),
	( Step >= 0 -> Cond = (From0=<To0) ; Cond = (From0>=To0) ),
	compile_aux([
		(LoopHead :-
		    ( Cond ->
		    	Apply, From1 is From0+Step, Recursion
		    ;
		    	true
		    )
		)
	], Module).


% similar to maplist, but with a delay clause

t_mapstream(mapstream(Pred, In, Out), NewGoal, Module) :-
	callable(Pred),
	!,
	analyse_pred(Pred, VarArgs, GenPred, GenVarArgs, Pattern),
	aux_pred_name(mapstream, Pattern, NewName),
	append(VarArgs, [In, Out], NewGoalArgs),	% make new goal
	NewGoal =.. [NewName|NewGoalArgs],
	append_args(NewName, GenVarArgs, HeadPrefix),	% make new predicate
	append_args(HeadPrefix, [[], []], NilClause),
	append_args(HeadPrefix, [InStream, _], DelayHead),
	append_args(HeadPrefix, [[Old|Olds], News], LoopHead),
	append_args(GenPred, [Old, New], Apply),
	append_args(HeadPrefix, [Olds, News0], Recursion),
	compile_aux([
		(delay DelayHead if var(InStream)),
		NilClause,
		(LoopHead :- Apply, News = [New|News0], Recursion)
	], Module).



% selectlist/3 creates output list of all list elements that pass a given test
% e.g. selectlist(<(0), [5,-3,0,2,-7], [5, 2])

t_selectlist(selectlist(Pred, In, Out), NewGoal, Module) :-
	callable(Pred),
	!,
	analyse_pred(Pred, VarArgs, GenPred, GenVarArgs, Pattern),
	aux_pred_name(selectlist, Pattern, NewName),
	append(VarArgs, [In, Out], NewGoalArgs),	% make new goal
	NewGoal =.. [NewName|NewGoalArgs],
	append_args(NewName, GenVarArgs, HeadPrefix),	% make new predicate
	append_args(HeadPrefix, [[], []], NilClause),
	append_args(HeadPrefix, [[Old|Olds], News], LoopHead),
	append_args(GenPred, [Old], Apply),
	append_args(HeadPrefix, [Olds, News0], Recursion),
	compile_aux([
		NilClause,
		(LoopHead :-
			(Apply -> News = [Old|News0] ; News = News0),
			Recursion)
	], Module).



t_applist(applist(Pred, List), NewGoal, Module) :-
	callable(Pred),
	!,
	analyse_pred(Pred, VarArgs, GenPred, GenVarArgs, Pattern),
	aux_pred_name(applist, Pattern, NewName),
	append(VarArgs, [List], NewGoalArgs),		% make new goal
	NewGoal =.. [NewName|NewGoalArgs],
	append_args(NewName, GenVarArgs, HeadPrefix),	% make new predicate
	append_args(HeadPrefix, [[]], NilClause),
	append_args(HeadPrefix, [[Old|Olds]], LoopHead),
	append_args(GenPred, [Old], Apply),
	append_args(HeadPrefix, [Olds], Recursion),
	compile_aux([
		NilClause,
		(LoopHead :- Apply, Recursion)
	], Module).


t_checklist(checklist(Pred, List), NewGoal, Module) :-
	t_applist(applist(Pred, List), NewGoal, Module).

t_sumlist(sumlist(Pred, List, AccIn, AccOut), NewGoal, Module) :-
	callable(Pred),
	!,
	analyse_pred(Pred, VarArgs, GenPred, GenVarArgs, Pattern),
	aux_pred_name(sumlist, Pattern, NewName),
	append(VarArgs, [List, AccIn, AccOut], NewGoalArgs),	% make new goal
	NewGoal =.. [NewName|NewGoalArgs],
	append_args(NewName, GenVarArgs, HeadPrefix),	% make new predicate
	append_args(HeadPrefix, [[], Acc0, Acc0], NilClause),
	append_args(HeadPrefix, [[Old|Olds], Acc10, Acc12], LoopHead),
	append_args(GenPred, [Old, Acc10, Acc11], Apply),
	append_args(HeadPrefix, [Olds, Acc11, Acc12], Recursion),
	compile_aux([
		NilClause,
		(LoopHead :- Apply, Recursion)
	], Module).


t_mapargs(mapargs(Pred, In, Out), NewGoal, Module) :-
	callable(Pred),
	!,
	analyse_pred(Pred, VarArgs, GenPred, GenVarArgs, Pattern),
	aux_pred_name(mapargs, Pattern, NewName),
	append(VarArgs, [In, Out], NewGoalArgs),	% make new goal
	NewGoal =.. [NewName|NewGoalArgs],
	append_args(NewName, GenVarArgs, HeadPrefix),	% make new predicate
	append_args(HeadPrefix, [Old, New], TopHead),
	append_args(HeadPrefix, [_, _, 0], NullHead),
	append_args(HeadPrefix, [Old, New, N0], LoopHead),
	append_args(GenPred, [OldArg, NewArg], Apply),
	append_args(HeadPrefix, [Old, New, N], Recursion),
	compile_aux([
		(TopHead :-
			functor(Old, F, N),
			functor(New, F, N),
			Recursion
			),
		(NullHead :- !),
		(LoopHead :-
			arg(N0, Old, OldArg),
			arg(N0, New, NewArg),
			N is N0-1,
			Apply,
			Recursion)
	], Module).


% appnodes(Pred, Term)
% call Pred on all Subterms of Term
% traversal is depth-first and left-to-right

t_appnodes(appnodes(Pred, Term), NewGoal, Module) :-
	callable(Pred),
	!,
	analyse_pred(Pred, VarArgs, GenPred, GenVarArgs, Pattern),
	aux_pred_name(appnodes, Pattern, NewName),
	aux_pred_name(appnodes_list, Pattern, NewNameList),
	append(VarArgs, [Term], NewGoalArgs),	% make new goal
	NewGoal =.. [NewName|NewGoalArgs],
	append_args(NewName, GenVarArgs, HeadPrefix),	% make new predicate
	append_args(NewNameList, GenVarArgs, HeadPrefixList),
	append_args(HeadPrefix, [T], HeadA1),
	append_args(GenPred,    [T], Apply),
	append_args(HeadPrefixList, [[]], HeadL1),
	append_args(HeadPrefixList, [[T|Args]], HeadL2),
	append_args(HeadPrefixList, [T], RecList),
	append_args(HeadPrefixList, [Args], RecListA),
	compile_aux([
		(HeadA1 :-
			(atomic(T); var(T)),
			Apply
			),
		(HeadA1 :-
			compound(T),
			T = [_|_],
			!,
			Apply,
			RecList),
		(HeadA1 :-
			compound(T),
			Apply,
			T =.. [_|Args],
			RecListA),
		HeadL1,
		(HeadL2 :-
			HeadA1,
			RecListA)
	], Module).


% sumnodes(Pred, Term, AccIn, AccOut)
% call Pred on all Subterms of Term and collect a result in Accumulator
% traversal is depth-first and left-to-right

t_sumnodes(sumnodes(Pred, Term, AccIn, AccOut), NewGoal, Module) :-
	callable(Pred),
	!,
	analyse_pred(Pred, VarArgs, GenPred, GenVarArgs, Pattern),
	aux_pred_name(sumnodes, Pattern, NewName),
	append(VarArgs, [Term, AccIn, AccOut], NewGoalArgs),	% make new goal
	NewGoal =.. [NewName|NewGoalArgs],
	append_args(NewName, GenVarArgs, HeadPrefix),	% make new predicate
	append_args(HeadPrefix, [T, A0, A2], Head0),
	append_args(GenPred,    [T, A0, A1], Apply),
	append_args(HeadPrefix, [T, A1, A2, 0, N], Call0),
	append_args(HeadPrefix, [T, A1, A3, N0, Ar], Head1),
	append_args(HeadPrefix, [Arg, A1, A2], Call1),
	append_args(HeadPrefix, [T, A2, A3, N, Ar], Recursion),
	compile_aux([
		(Head0 :-
			Apply,
			(compound(T) ->
			    functor(T, _, N),
			    Call0
			;
			    A1 = A2
			)),
		(Head1 :-
			N0 < Ar ->
			    N is N0 + 1,
			    arg(N, T, Arg),
			    Call1,
			    Recursion
			;
			    A1 = A3)
	], Module).


% sumargs(Pred, Term, AccIn, AccOut)
% call Pred on all arguments of Term and collect a result in Accumulator
% traversal order is unspecified!

t_sumargs(sumargs(Pred, Term, AccIn, AccOut), NewGoal, Module) :-
	callable(Pred),
	!,
	analyse_pred(Pred, VarArgs, GenPred, GenVarArgs, Pattern),
	aux_pred_name(sumargs, Pattern, NewName),
	append(VarArgs, [Term, AccIn, AccOut], NewGoalArgs),	% make new goal
	NewGoal =.. [NewName|NewGoalArgs],
	append_args(NewName, GenVarArgs, HeadPrefix),	% make new predicate
	append_args(HeadPrefix, [T, A0, A1], Head0),
	append_args(HeadPrefix, [T, A0, A1, N], Call0),
	append_args(HeadPrefix, [_, A0, A1, 0], NullHead),
	append_args(HeadPrefix, [T, A1, A3, N], Head1),
	append_args(GenPred,    [Arg, A1, A2], Apply),
	append_args(HeadPrefix, [T, A2, A3, N1], Recursion),
	compile_aux([
		(Head0 :-
			functor(T, _, N),
			Call0),
		(NullHead :-
			!, A0 = A1),
		(Head1 :-
			arg(N, T, Arg),
			N1 is N - 1,
			Apply,
			Recursion)
	], Module).



% utilities

compile_aux(Clauses, Module) :-
	Clauses = [Clause|_],
	( Clause = (Head :- _) -> true
	; Clause = (delay Head if _) -> true
	; Clause = Head ),
	functor(Head, F, N),
	( is_predicate(F/N)@Module ->
%	    printf("*** Reusing auxiliary predicate %Qw\n%b", [F/N]),
	    true
	;
	    printf("*** Creating auxiliary predicate %Qw\n%b", [F/N]),
%	    write_clauses(Clauses),
	    compile_term(Clauses)@Module,
	    set_flag(F/N, auxiliary, on)@Module
	).

write_clauses([]).
write_clauses([C|Cs]) :-
	writeclause(C),
	write_clauses(Cs).


append_args(Term, Args, NewTerm) :-
	atom(Term),
	NewTerm =.. [Term|Args].
append_args(Term, Args, NewTerm) :-
	compound(Term),
	Term =.. OldList,
	append(OldList, Args, NewList),
	NewTerm =.. NewList.


:- mode analyse_pred(+,-,-,-,-).
analyse_pred(Pred, VarArgs, GenPred, GenVarArgs, Pattern) :-
	functor(Pred, F, N),
	functor(Pattern, F, N),
	functor(GenPred, F, N),
	analyse_arg(Pred, VarArgs, GenPred, GenVarArgs, Pattern, 0, N).
	
:- mode analyse_arg(+,-,+,-,+,+,+).
analyse_arg(_Pred, [], _GenPred, [], _Pattern, Ar, Ar) :- !.
analyse_arg(Pred, VarArgs, GenPred, GenVarArgs, Pattern, N0, Ar) :-
	N is N0+1,
	arg(N, Pred, Arg),
	( nonground(Arg) ->
	    arg(N, Pattern, '_'),
	    VarArgs = [Arg|VarArgs0],
	    GenVarArgs = [_Var|GenVarArgs0],
	    arg(N, GenPred, _Var)
	;
	    arg(N, Pattern, Arg),
	    arg(N, GenPred, Arg),
	    VarArgs = VarArgs0,
	    GenVarArgs = GenVarArgs0
	),
	analyse_arg(Pred, VarArgs0, GenPred, GenVarArgs0, Pattern, N, Ar).


aux_pred_name(Name, Pattern, NewName) :-
	open("", string, S),
	printf(S, "%a(%w)", [Name, Pattern]),
	get_stream_info(S, name, NewNameS),
	close(S),
	atom_string(NewName, NewNameS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Definitions for Metacalls
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- local maplist_body/4.	% hide the one from lib(lists)
:- tool(maplist/3, maplist_body/4).

maplist_body(_, [], [], _).
maplist_body(Pred, [In|ListIn], [Out|ListOut], Module) :-
    apply(Pred, [In, Out], Module),
    maplist_body(Pred, ListIn, ListOut, Module).

:- tool(mapstream/3, mapstream_body/4).

delay mapstream_body(_, L, _, _) if var(L).
mapstream_body(_, [], [], _).
mapstream_body(Pred, [In|ListIn], [Out|ListOut], Module) :-
    apply(Pred, [In, Out], Module),
    mapstream_body(Pred, ListIn, ListOut, Module).

:- tool(mapargs/3, mapargs_body/4).

mapargs_body(Pred, TermIn, TermOut, Module) :-
    functor(TermIn, F, N),
    functor(TermOut, F, N),
    mapargs_args(Pred, TermIn, TermOut, N, Module).

mapargs_args(_, _, _, 0, _) :- !.
mapargs_args(Pred, TermIn, TermOut, I, Module) :-
    arg(I, TermIn, InArg),
    arg(I, TermOut, OutArg),
    I1 is I-1,
    apply(Pred, [InArg, OutArg], Module),
    mapargs_args(Pred, TermIn, TermOut, I1, Module).

:- tool(appnodes/2, appnodes_body/3).

appnodes_body(Pred, Term, Module) :-
    (atomic(Term); var(Term)),
    apply(Pred, [Term], Module).
appnodes_body(Pred, Term, Module) :-
    compound(Term),
    Term = [_|_],
    !,
    apply(Pred, [Term], Module),
    appnodes_list(Pred, Term, Module).
appnodes_body(Pred, Term, Module) :-
    compound(Term),
    apply(Pred, [Term], Module),
    Term =.. [_|Args],
    appnodes_list(Pred, Args, Module).

appnodes_list(_, [], _).
appnodes_list(Pred, [Term|Args], Module) :-
    appnodes_body(Pred, Term, Module),
    appnodes_list(Pred, Args, Module).

:- tool(sumargs/4, sumargs_body/5).

sumargs_body(Pred, Term, A0, A1, Module) :-
    functor(Term, _, N),
    sumargs(Pred, Term, A0, A1, N, Module).

sumargs(_, _, A0, A1, 0, _) :-
    !,
    A0 = A1.
sumargs(Pred, Term, A1, A3, N, Module) :-
    arg(N, Term, Arg),
    N1 is N - 1,
    apply(Pred, [Arg, A1, A2], Module),
    sumargs(Pred, Term, A2, A3, N1, Module).

:- tool(applist/2, applist_body/3).
:- tool(checklist/2, applist_body/3).

applist_body(_, [], _).
applist_body(Pred, [In|ListIn], Module) :-
    append_args(Pred, [In], Goal),
    call(Goal)@Module,
    applist_body(Pred, ListIn, Module).

:- tool(selectlist/3, selectlist_body/4).

selectlist_body(_, [], [], _).
selectlist_body(Pred, [In|ListIn], ListOut, Module) :-
    (apply(Pred, [In], Module) ->
	ListOut = [In|NewListOut]
    ;
	ListOut = NewListOut
    ),
    selectlist_body(Pred, ListIn, NewListOut, Module).

:- tool(sumlist/4, sumlist_body/5).

sumlist_body(_, [], Acc, Acc, _).
sumlist_body(Pred, [H|T], AccIn, AccOut, Module) :-
    apply(Pred, [H, AccIn, A1], Module),
    sumlist_body(Pred, T, A1, AccOut, Module).

:- tool(sumnodes/4, sumnodes_body/5).

sumnodes_body(Pred, Term, A0, A2, Module) :-
    apply(Pred, [Term, A0, A1], Module),
    (compound(Term) ->
	functor(Term, _, N),
	sumnodes(Pred, Term, A1, A2, 0, N, Module)
    ;	% simple term or variable
	A1 = A2
    ).

sumnodes(Pred, Term, A1, A3, N0, Ar, Module) :-
    N0 < Ar ->
	N is N0+1,
	arg(N, Term, Arg),
	sumnodes_body(Pred, Arg, A1, A2, Module),
	sumnodes(Pred, Term, A2, A3, N, Ar, Module)
    ;
	A1 = A3.

:- tool(fromto/4, fromto_body/5).

fromto_body(From, To, Step, Pred, Module) :-
	( sgn(Step) =:= sgn(From-To) ->
	    true
	;
	    apply(Pred, [From], Module),
	    From1 is From+Step,
	    fromto_body(From1, To, Step, Pred, Module)
	).

apply(Term, AddArgs, Module) :-
    append_args(Term, AddArgs, Goal),
    call(Goal)@Module.

