%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, 2011, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-dynamic(route_child/2).

:- set_flag(print_depth, 500).

% make groups of numbers to be able to do a breadth-first search
route_listgroups(NrGroups,L,G) :-
    length(L,Len),
    T1 is Len / NrGroups,
    ceiling(T1,T2),
    integer(T2,GLen),
    route_listgroupsimpl(L,GLen,G).

route_listgroupsimpl([],_,[]).
route_listgroupsimpl(L,GLen,[G1|G]) :-
    length(L,Len),
    M is min(Len,GLen),
    M > 0,
    length(G1, M),
    append(G1,Rem,L),
    route_listgroupsimpl(Rem,GLen,G).

% construct the tree
route_tree(Radix, Root, L, T) :-
    route_listgroups(Radix, L, G),
    ( foreach(El, G),
      foreach(ST, SubTrees),
      param(Radix)
      do
        [R|C] = El,
        route_tree(Radix, R, C, ST)
    ),
    T = node(Root, SubTrees).

% construct the routing table out of the tree
route_postorder(Tree, L, CurrRoutes) :-
    node(Root, Children) = Tree,
    ( foreach(C, Children),
      foreach(E, Elements),
      foreach(SubRoute, SubRoutes),
      foreach(Ne, Neighbours)
      do
        route_postorder(C, E, SubRoute),
        node(Ne, _) = C
    ),
    flatten(Elements,FE),
    flatten(SubRoutes, SubRoutesF),
    append(FE,[Root],L),
    append(SubRoutesF, [route(Root,FE, Neighbours)],CurrRoutes).

% construct routing table for all cores
route_allcores_alt(Radix,Root,Routes,L) :-
    subtract(L,[Root],L2),
    route_tree(Radix, Root, L2, T),
    route_postorder(T, _, Routes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% new code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

route_neighbours(S, L) :-
    findall(C, route_child(S, C), L).

route_reach(S, L) :-
    route_neighbours(S, R),
    ( foreach(El, R),
      foreach(REl, SubRoutes)
      do
        route_reach(El, REl)
    ),
    flatten(SubRoutes,SubRoutesF),
    append(R, SubRoutesF, L).


% create the route_child(Source, Dest) facts
route_create_facts(_, _, _, []).
route_create_facts(Radix, 0, [_|NodeList], L) :-
    route_create_facts(Radix, Radix, NodeList, L).
route_create_facts(Radix, N, [Root|NodeList], [H|T]) :-
    N > 0,
    assert(route_child(Root, H)),
    M is N - 1,
    append([Root|NodeList],[H],LL),
    route_create_facts(Radix, M, LL, T).


% root has to be partd of the CoreList
route_create_routing(Radix, Root, CoreList, Routes) :-
    member(Root, CoreList),
    subtract(CoreList, [Root], CoreListWORoot),
    retractall(route_child(_,_)),
    route_create_facts(Radix, Radix, [Root], CoreListWORoot),
    ( foreach(El, [Root|CoreListWORoot]),
      foreach(R, Routes)
      do
        route_reach(El, Reach),
        route_neighbours(El, Neighbours),
        R = route(El, Reach, Neighbours)
    ).

% construct routing table for all cores
route_allcores(Radix,Root,Routes, L) :-
    route_create_routing(Radix, Root, L, Routes).

% construct dot output for pretty diagrams
% to use: eclipse -b route_tree_radix.pl -e 'route_demo(4,16)' | dot -Tpdf -o route.pdf
route_demo(Radix,NCores) :-
    Root is 0,
    (for(I,0,NCores - 1), foreach(I,CoreList) do true),

    route_create_routing(Radix, Root, CoreList, _),
    ( foreach(C,CoreList),
      foreach(L,Links)
      do
        route_neighbours(C, Neighbours),
        L = neigh(C,Neighbours)
    ),

    writeln("digraph G {"),
    writeln("  node [shape=circle,width=.6,fixedsize=true];"),
    ( foreach(L,Links)
      do
        neigh(C,Neighbours) = L,
        ( foreach(N,Neighbours), param(C)
          do
            printf("  %d -> %d;\n", [C, N])
        )
    ),
    writeln("}").
