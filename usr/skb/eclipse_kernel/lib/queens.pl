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
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: queens.pl,v 1.1 2008/06/30 17:43:48 jschimpf Exp $
% ----------------------------------------------------------------------

/*

  SEPIA DEMO PROGRAM
 
  IDENTIFICATION:	queens.pl
 
  AUTHOR:		Micha Meier
 
  CONTENTS:		go_queens/0		runs the demo
 
  DESCRIPTION:
 
 	This program shows the difference of various constraint
 	solving approaches. Using the graphics interface it is possible
	to solve the N-queens problem for any N, using four different
	strategies:
		- naive: generate and test
		- constraints: using finite domains, queens are labelled
			strictly left to right
		- first fail: finite domains, the most constrained queen
			is selected for the labeling
		- heuristics: finite domains, start from the centre of
			the board rather than from the left bottom corner,
			us the first fail principle.
	It can be seen that for small N the difference is not very big,
	however for greater N the strategy becomes significant.
 */


:- module(queens).
:- use_module(library(fd)).
:- lib(util).
:- global
	close_queens/0,
	go_queens/0.

:- dynamic
	stop/0.

:- 
   make_local_array(mode),
   setval(mode, naive).

:-      make_local_array(flags),
	get_flag(debug_compile, DC),
	get_flag(variable_names, VN),
	setval(flags, flags(DC, VN)),
	nodbgcomp,
	true.

window_data(0, 0, 500, 500).

go_queens :-
	(open_pce -> true; true),
	global(window_data/4),
	(open_2d(0,0,5000,5000) -> true; clear_view_surface(0)),
	interior_style(1,0),
	line_color(1),
	Nb is fix(5000/8),
	board(8, 0, Nb),
	make_dialog.

close_queens :-
	(close_2d -> true; true),
	object(@map_dialog),
	send(@map_dialog, destroy),
	call(get_flag(window_data/4, definition_module, queens), kegi),
	local(window_data/4),
	!.
close_queens.

place_queens :-
	get(@queens_number, string, Text),
	integer_atom(N, Text),
	getval(mode, Mode),
	queens(N, yes, Mode).

queens(N, Mode) :-
	queens(N, yes, Mode).

queens(N, Gr, Mode) :-
	make_list(N, List),
	(Gr == yes ->
	    send(@queen_setting, selection, '        '),
	    send(@queen_solving, selection, '        '),
	    clear_view_surface(0),
	    interior_style(1,0),
	    line_color(1),
	    Nb is fix(5000/N),
	    board(N, 0, Nb),
	    drawlp(List, 1, Nb)
	;
	    true
	),
	cputime(T0),
	(Mode \== naive ->
	    List :: 1..N, 		% Domain of Xi's
	    queens(List),
	    alldistinct(List)
	;
	    true
	),
	cputime(T1),
	T is round((T1 - T0) * 100)/100,
	(Gr == yes ->
	    term_string(T, String),
	    send(@queen_setting, selection, String)
	;
	    printf("%d queens:\n\tSetting up constraints %.2f sec.\n%b", [N, T])
	),
	labeling(List, N, Mode),
	Te is round((cputime - T1) * 100)/100,
	(Gr == yes ->
	    term_string(Te, Se),
	    send(@queen_solving, selection, Se)
	;
	    printf("\tSolving                %.2f sec.\n%b", [Te])
	).

queens([]).
queens([X|Y]) :-
   safe(X,Y,1),
   queens(Y).

safe(_,[],_).
safe(X,[F|T],Nb) :-
   noattack(X,F,Nb),
   Newnb is Nb + 1,
   safe(X,T,Newnb).

noattack(X,Y,Nb) :-
   Y + Nb ## X,
   X + Nb ## Y.

safe_naive(_,[],_).
safe_naive(X,[F|T],Nb) :-
   X + Nb =\= F,
   F + Nb =\= X,
   X \== F,
   Newnb is Nb + 1,
   safe_naive(X,T,Newnb).

make_list(0, []) :- !.
make_list(N, [_|Rest]) :-
	N1 is N - 1,
	make_list(N1, Rest).

labeling(L, _, ff) :-
	labeling(L).
labeling(L, N, ffh) :-
	Nh is N // 2,
	labeling_h(L, Nh).
labeling(L, _, noff) :-
	labeling_noff(L).
labeling(L, N, naive) :-
	generate(L, N).

labeling([]).
labeling([X|Y]) :-
   deleteffc(Var,[X|Y], Reste),			% Var has least domain
   indomain(Var),				% generates values in domain
   (stop -> exit_block(abort) ; labeling(Reste)).

labeling_h([], _).
labeling_h([X|Y], Nh) :-
   deleteffh(Var,[X|Y],Nh, Reste),			% Var has least domain
   indomainq(Var),				% generates values in domain
   (stop -> exit_block(abort) ; labeling_h(Reste, Nh)).

labeling_noff([]).
labeling_noff([H|T]) :-
	indomain(H),
	(stop -> exit_block(abort) ; labeling_noff(T)).

generate(List, N) :-
	generate(List, N, []).

generate([], _, _).
generate([Var|Vars], N, Alloc) :-
	between(1, N, Var),
	safe_naive(Var, Alloc, 1),
	(stop -> exit_block(abort); generate(Vars, N, [Var|Alloc])).

% Specific predicates, some of them actually copied from domain.pl
deleteffh(Var, [H|T], Nh, Rest) :-
    get_var_domain(H, _, Card),
    Dist is Nh - 1,
    find_least_domain(T, H, Card, 2, Nh, Dist, Chosen, Rest),
    Var = Chosen.

find_least_domain([], Chosen, _, _, _, _, Chosen, []) :- !.
find_least_domain([Var|L], OldVar, OldCard, I, Nh, OldDist, Chosen, [V|Rest]) :-
    get_var_domain(Var, _, NewCard),
    (NewCard == 1 ->
            Chosen = Var,
            Rest = L,
            V = OldVar
    ;
            D is abs(Nh - I),
            ((NewCard > OldCard ; NewCard == OldCard, D > OldDist) ->
                    V = Var, Next = OldVar, Max = OldCard, Dist = OldDist
            ;
                    V = OldVar, Next = Var, Max = NewCard, Dist = D
            ),
            I1 is I + 1,
            find_least_domain(L, Next, Max, I1, Nh, Dist, Chosen, Rest)
    ).

make_queen_domain(Var, Size, NewDomain) :-
	Half is Size // 2,
	dom(Var, List),
	halve(Half, List, [], L1, L2),
	merge(L1, L2, NewDomain).

halve(0, L2, L1, L1, L2) :- !.
halve(N, [H|R], Li, L1, L2) :-
	N1 is N - 1,
	halve(N1, R, [H|Li], L1, L2).

merge([], L, L) :- !.
merge(L, [], L) :- !.
merge([H1|L1], [H2|L2], [H1, H2|R]) :-
	merge(L1, L2, R).

get_var_domain(Var, Domain, Size) :-
    dvar_domain(Var, Domain),
    dom_size(Domain, Size).

indomainq(Var) :-
	get_var_domain(Var, Domain, Size),
        make_queen_domain(Var, Size, NewDomain),
        member(Var, NewDomain).

%% Graphic stuff
drawlp([], _, _).
drawlp([H|T],N,Size):-
	draw(H,N,Size),
	N1 is N+1,
	drawlp(T,N1,Size).

delay draw(L,_,_) if var(L).
draw(L,K,Size):-
	X is K*Size,
	Y is L*Size,
	X1 is X-Size,
	Y1 is Y-Size,
	fill_color(1),
	rectangle(X,Y,X1,Y1).
draw(L,K,Size):-
	X is K*Size,
	Y is L*Size,
	X1 is X-Size,
	Y1 is Y-Size,
	fill_color(0),
	rectangle(X,Y,X1,Y1),
	line(X,Y,X,Y1),
	line(X,Y1,X1,Y1),
	line(X1,Y1,X1,Y),
	line(X1,Y,X,Y),
	fail.

board(N,N,Size):-
	!,
	X is N*Size,
	line(0,X,X,X),
	line(X,0,X,X).
board(Nmax,N,Size):-
	X is N*Size,
	Xmax is Nmax*Size,
	line(0,X,Xmax,X),
	line(X,0,X,Xmax),
	N1 is N+1,
	board(Nmax,N1,Size).

%-------------------------------------------------------
% dialog window
%-------------------------------------------------------

:- make_callback(queen_dialog_pressed/2).
:- make_callback(queen_constraints_selected/2).
:- make_callback(queen_text_item/2).

make_dialog :-
	new_dialog(@map_dialog, 'Queens Set Up', dialog_panel),
	send(@map_dialog, open, point(600, 0)).

dialog_panel(@queen_run, button('Run', queen_dialog_pressed), append, []).
dialog_panel(@queen_stop, button('Abort', queen_dialog_pressed), right, []).
dialog_panel(@queen_quit, button('Quit', queen_dialog_pressed), right, []).
dialog_panel(_, label(none, 'Setting: '), point(180, 10), []).
dialog_panel(@queen_setting, label(none, '       '), point(240, 10), []).
dialog_panel(_, label(none, 'Solving: '), point(180, 30), []).
dialog_panel(@queen_solving, label(none, '       '), point(240, 30), []).
dialog_panel(_, menu('Solving mode', cycle, cascade(0, queen_constraints_selected, 0),
	[naive, constraints, 'first fail', 'heuristics']), below, []).
%dialog_panel(_, label(none, 'Number of queens'), below, []).
dialog_panel(@queens_number, text_item('Number of queens:', 8, queen_text_item), below, []).

queen_constraints_selected(_, naive) :-
	setval(mode, naive).
queen_constraints_selected(_, constraints) :-
	setval(mode, noff).
queen_constraints_selected(_, 'first fail') :-
	setval(mode, ff).
queen_constraints_selected(_, heuristics) :-
	setval(mode, ffh).

queen_text_item(D, _) :-
	queen_dialog_pressed(D, 'Run').

queen_dialog_pressed(_, 'Run') :-
	retract_all(stop),
	send(@pce, async, 0),
	send(@queen_stop, [greyed:off, active:on]),
	send(@queen_run, [greyed:on, active:off]),
	send(@queen_quit, [greyed:on, active:off]),
	block(place_queens, stopped, true),
	send(@queen_quit, [greyed:off, active:on]),
	send(@queen_run, [greyed:off, active:on]),
	send(@queen_stop, [greyed:on, active:off]).
queen_dialog_pressed(_, 'Abort') :-
	send(@queen_run, [greyed:off, active:on]),
	send(@queen_quit, [greyed:off, active:on]),
	send(@queen_stop, [greyed:on, active:off]),
	assert(stop).
queen_dialog_pressed(_, 'Quit') :-
	close_queens,
	abort.

:-      getval(flags, flags(DC, VN)),
	set_flag(debug_compile, DC),
	set_flag(variable_names, VN),
	erase_array(flags).
