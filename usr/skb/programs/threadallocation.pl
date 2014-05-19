%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :-include("data.pl").
:-lib(ic).
:-lib(branch_and_bound).

permutation([],[]).
permutation(List,[X|Perm]) :-
    asqselect(X,List,Rest),
    permutation(Rest,Perm).
asqselect(X,[X|Rest],Rest).
asqselect(X,[Head|List],[Head|Rest]) :-
    asqselect(X,List,Rest).

lastelement([H],H) :- !.
lastelement([_|T],El) :-
    lastelement(T,El).


% create a range on which a certain core can operate so that it works on a
% single cache line
% createrange(3,B).
createrange(CoreID,StartAddress,Bucket) :-
    cache(_, CoreID, 1, data, _, _, LineSize, _),
    S is StartAddress mod LineSize,
    S =:= 0,
    E is StartAddress + LineSize,
    Bucket = range(CoreID, StartAddress, E).

% create a list of threads so that every one works on one part of a bigger address
% range and that all the ranges are contiguous. This goal leaves choice points and
% can be called several times to get different results.
% threadlist(0,177,L).
threadlist([],_,_,[]).
threadlist([CH|CT],Start,End,[H|T]) :-
    createrange(CH,Start,H),
    range(_,_,E) = H,
    ( E < End ->
        threadlist(CT,E,End,T);
        T = []
    ).


nextsol(CoreIDs,Start,End,L,C) :-
    permutation(CoreIDs,[CH|CoreIDPerm]),
    cache(_, CH, 1, data, _, _, LineSize, _),
    SMod is Start mod LineSize,
    StartNew is Start - SMod,
    threadlist([CH|CoreIDPerm],StartNew,End,L),
    lastelement(L,range(CL,SL,_)),
    cache(_, CL, 1, data, _, _, LineSizeLast, _),
    SModLast is SL mod LineSizeLast,
    SModLast =:= 0,
    length(L,Len),
    C is 10000 - Len.


allocthread(CoreIDs, Start, End, List) :-
    minimize(nextsol(CoreIDs, Start, End, List, C), C).

