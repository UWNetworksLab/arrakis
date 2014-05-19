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
% Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% 
% Solver for constraints over finite sets of integers (ic wrapper)
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc
% Version:	$Id: ic_hybrid_sets.ecl,v 1.1.1.1 2006/09/23 01:53:46 snovello Exp $
%
% ----------------------------------------------------------------------

:- module(ic_hybrid_sets).

:- lib(ic).
:- lib(ic_kernel).

:-pragma(nodebug).

tr_generic_sets(solver_module, ic).
tr_generic_sets(sbds_module, ic_sbds).

:- local macro(solver_module, tr_generic_sets/2, []).
:- local macro(sbds_module, tr_generic_sets/2, []).

:- include(generic_hybrid_sets).

%:-include(search).

:-export safe_set_range/3.  % works for ground list
:-export insetdomain/4.     % set version of indomain
:-export labeling_lex/1.    % simplest labelling strategy
:-export labeling_lex/4.    % simplest labelling strategy
:-export labeling_smallest_glb/1.  % labelling set with smallest glb first
:-export labeling_ff/4.  % labelling set with smallest domain first


:-local(variable(current_node)).
:-setval(current_node,0).

goal_string(Goal, Str):-
    open(string(""), write, Stream),
    % use the flags which you want
    printf(Stream, "%w", [Goal]),
    get_stream_info(Stream, name, Str),
    close(Stream).

search_node(Goal):-
        call(Goal).
xsearch_node(Goal):-
        getval(current_node,OldNode),
        incval(current_node),
        sepia_kernel:get_cut(OldCut),
        goal_string(Goal, GoalStr),
        Goal,
        sepia_kernel:get_cut(NewCut),
        (NewCut \= OldCut ->
            % a choice point was left so leave behind a goal which
            % will increment the node number when backtrack happens
            (
                concat_string([" [","color=\"cyan\" style=\"filled\" ","label=\"",GoalStr,"\"]"], NodeAttrs),
                %concat_string([" [label=\"",GoalStr,"\"]"], EdgeAttrs),
                EdgeAttrs=""
            ;
                (incval(current_node),fail)
            )
        ;
            concat_string([" [","color=\"green\", style=\"filled\" ","label=\"",GoalStr,"\"]"], NodeAttrs),
            EdgeAttrs=[]
        ),
        getval(current_node,NewNode),
        %write(Node),write(OldCut->Node),write(NewCut),writeln(';'),
        write(OldNode),write(NodeAttrs),writeln(';'),
        write(OldNode->NewNode),write(EdgeAttrs),writeln(';'),
        true.

safe_set_range(S,S,S):-
	nonvar(S),!.
safe_set_range(S,GLB,LUB):-
	set_range(S,GLB,LUB).
	

labeling_lex(Ss):-
        labeling_lex(Ss,_,_,_).

labeling_lex([],_,_,_).
labeling_lex([S | LS], CardSel, ElemSel, Order) :-
        %cputime(T0),
        %getval(set_lex_min_count,Min0),
        %getval(set_lex_max_count,Max0),
        insetdomain(S,CardSel,ElemSel,Order),
        %Time is cputime - T0,
        %Mins is getval(set_lex_min_count) - Min0,
        %Maxs is getval(set_lex_max_count) - Max0,
        %writeln(Time - Mins -Maxs),
        labeling_lex(LS, CardSel, ElemSel, Order).

find_smallest_glb(Ss, Smallest):-
        [Start|_] = Ss,
        (foreach(S,Ss),
         fromto(Start, In, Out, Smallest) do
             safe_set_range(S, GLB, _LUB),
             length(GLB,L),
             safe_set_range(In, InGLB, _InLUB),
             length(InGLB,InL),
             (L < InL ->
                  Out = S
             ;
                  Out = In
             )
        ).

remove_ground([],[]).
remove_ground([S|Rest],Filtered) :-
        remove_ground(Rest,RestFiltered),
        (ground(S) ->
             Filtered = RestFiltered
        ;
             Filtered = [S|RestFiltered]
        ).

labeling_smallest_glb([]).
labeling_smallest_glb([H|T]):-
        remove_ground([H|T],FilteredSets),
        (FilteredSets == [] ->
             true
        ;
             find_smallest_glb(FilteredSets,Smallest),
             select_element(Smallest, big_first, Elem),
             label_element(Smallest, in_notin, Elem),
             labeling_smallest_glb(FilteredSets)
        ).

find_smallest_domain(Ss, Smallest):-
        [Start|_] = Ss,
        (foreach(S,Ss),
         fromto(Start, In, Out, Smallest) do
             safe_set_range(S, GLB, LUB),
             Size is length(LUB) - length(GLB),
             safe_set_range(In, InGLB, InLUB),
             InSize is length(InLUB) - length(InGLB),
             (Size < InSize ->
                  Out = S
             ;
                  Out = In
             )
        ).

labeling_ff([],_,_,_).
labeling_ff([H|T], CardSel, ElemSel, Order):-
        remove_ground([H|T],FilteredSets),
        (FilteredSets == [] ->
             true
        ;
             find_smallest_domain(FilteredSets,Smallest),
             insetdomain(Smallest,CardSel,ElemSel,Order),
             labeling_ff(FilteredSets, CardSel, ElemSel, Order)
        ).


% -----------------------------------
% :-include(lexico_less_nobound).
% -----------------------------------
less_lex(S1,S2):-
        lex_set_range(S1,_,S1Lub),
        lex_set_range(S2,_,S2Lub),
        lex_set:lex_union(S1Lub,S2Lub,Union),
        (foreach(I,Union),
         foreach(Bool1,Bools1),
         foreach(Bool2,Bools2),
         param(S1,S2) do
            in(I,S1,Bool1),
            in(I,S2,Bool2)
        ),
        lexico_less(Bools1,Bools2).

leq_lex(S1,S2):-
        lex_set_range(S1,_,S1Lub),
        lex_set_range(S2,_,S2Lub),
        lex_set:lex_union(S1Lub,S2Lub,Union),
        (foreach(I,Union),
         foreach(Bool1,Bools1),
         foreach(Bool2,Bools2),
         param(S1,S2) do
            in(I,S1,Bool1),
            in(I,S2,Bool2)
        ),
        lexico_leq(Bools1,Bools2).
        

lexico_less([], []) ?- !, fail.
lexico_less([], [_|_]) ?- !, true.
lexico_less([X], [Y]) ?- !, X #< Y.
lexico_less([X], [Y|_Ys]) ?- !, X #=< Y.
lexico_less(Xs, Ys) :-
	lexico_less(Xs, Ys, _).
    	
    :- demon lexico_less/3.
    lexico_less([X1|X2Xs], [Y1|Y2Ys], S) ?-
	X1 #=< Y1,
	get_bounds(X1, _X1min, X1max),
	get_bounds(Y1, Y1min, _Y1max),
	( X1max < Y1min ->		% X1 #< Y1 entailed
	    kill_suspension(S)
	; X1 == Y1 ->			% same value or variable
	    kill_suspension(S),
	    lexico_less(X2Xs, Y2Ys)
	; 
	    X2Xs = [X2|_], Y2Ys = [Y2|_],
	    get_bounds(X2, X2min, _X2max),
	    get_bounds(Y2, _Y2min, Y2max),
	    ( X2min > Y2max ->
		kill_suspension(S),
		X1 #< Y1
	    ; var(S) ->
	        suspend(lexico_less([X1|X2Xs], [Y1|Y2Ys], S), 3,
		    [X1->bound,Y1->bound,X1->max,Y1->min,X2->min,Y2->max], S)
	    ;
		true	% resuspend
	    )
	).



lexico_leq([], []) ?- !, true.
lexico_leq([X], [Y|_]) ?- !, X #=< Y.
lexico_leq(Xs, Ys) :-
	lexico_leq(Xs, Ys, _).
    	
    :- demon lexico_leq/3.
    lexico_leq([X1|X2Xs], [Y1|Y2Ys], S) ?-
	X1 #=< Y1,
	get_bounds(X1, _X1min, X1max),
	get_bounds(Y1, Y1min, _Y1max),
	( X1max < Y1min ->		% X1 #< Y1 entailed
	    kill_suspension(S)
	; X1 == Y1 ->			% same value or variable
	    kill_suspension(S),
	    lexico_leq(X2Xs, Y2Ys)
	; 
	    X2Xs = [X2|_], Y2Ys = [Y2|_],
	    get_bounds(X2, X2min, _X2max),
	    get_bounds(Y2, _Y2min, Y2max),
	    ( X2min > Y2max ->
		kill_suspension(S),
		X1 #< Y1
	    ; var(S) ->
	        suspend(lexico_leq([X1|X2Xs], [Y1|Y2Ys], S), 3,
		    [X1->bound,Y1->bound,X1->max,Y1->min,X2->min,Y2->max], S)
	    ;
		true	% resuspend
	    )
	).

