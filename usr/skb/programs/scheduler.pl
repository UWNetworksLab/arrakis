%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, 2011, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :-include("data_sbrinz1.txt").

:-lib(ic).
:-lib(branch_and_bound).

% parallelapp(string name, int max-usable-threads, bool morecores_better?, bool synchronization_intensive).

%parallelapp(a, 10, true, true).
%parallelapp(b, 10, true, false).
%parallelapp(c, 10, true, true).
%parallelapp(d,  2, true, true).

%parallelapp(a, 6, true, true).
%parallelapp(b, 12, true, false).
%parallelapp(c, 8, true, true).
%parallelapp(d,  7, true, false).

:-dynamic(parallelapp/4).

scheduler_add_app(Name, MaxUsableThreads, MoreCoresBetter, SyncIntensive) :-
    assert(parallelapp(Name, MaxUsableThreads, MoreCoresBetter, SyncIntensive)).

scheduler_remove_app(Name) :-
    retract(parallelapp(Name, _, _, _)).


schedule(S) :-
% make sure that the CoreIDs are sorted according to which package they belong
%    findall(CoreID, cpu(CoreID,_), CoreIDs),
    findall(cpu_thread(CPU, Pkg), cpu_thread(CPU, Pkg, _, _), CPUThreads),
    sort(2, =<, CPUThreads, CPUThreadsSorted),
    maplist(get_core_id, CPUThreadsSorted, CoreIDs),
    length(CoreIDs, NrCores),
    findall(Name, parallelapp(Name, _, _, true), SyncThreadNames),
    findall(thread(Name, C, B1, false), parallelapp(Name, C, B1, false), NoSyncThreads),
    % set the max #cores a sync thread can use to the max #hwcores available
    setmaxcores(NrCores, SyncThreadNames, SyncThreads),
    maplist(maxthreads, SyncThreads, SyncNrThreads),
    maplist(maxthreads, NoSyncThreads, NoSyncNrthreads),
    sum(SyncNrThreads,SyncS),
    sum(NoSyncNrthreads, NoSyncS),
    length(SyncThreads, Len1),
    length(NoSyncThreads,Len2),
    NrApps is Len1 + Len2,
    ( NrApps > 0 ->
        NrThreads is SyncS + NoSyncS,
        virtcores(NrCores, NrCores, NrThreads, NrVCores),
        R is NrVCores mod NrApps,
        T1 is NrVCores - R,
        NrVCoresPerThreadF is T1 / NrApps,
        integer(NrVCoresPerThreadF, NrVCoresPerThread),
        nl,write(NrVCoresPerThread),nl,write(R),nl,
        count_to_distr_cores(NrVCoresPerThread, SyncThreads, SyncDistrCores),
        count_to_distr_cores(NrVCoresPerThread, NoSyncThreads, NoSyncDistrCores),
        DistrCores is SyncDistrCores + NoSyncDistrCores,
        adjust_nr_cores(NrVCoresPerThread, DistrCores, SyncThreads, SyncThreadsAdj, NewDistrCores),
        adjust_nr_cores(NrVCoresPerThread, NewDistrCores, NoSyncThreads, NoSyncThreadsAdj, _),
        create_schedule(NrCores, NrVCores, NrThreads, CoreIDs, SyncThreadsAdj, NoSyncThreadsAdj, 0, S);
        S=[]
    ),
    write(S),nl.


get_core_id(cpu_thread(CPU, _), CPU).

setmaxcores(_, [], []).
setmaxcores(NrCores, [N|Threads], [TM|ThreadMax]) :-
    parallelapp(N, M, B1, B2),
    min(NrCores, M, Min),
    TM = thread(N, Min, B1, B2),
    setmaxcores(NrCores, Threads, ThreadMax).

maxthreads(thread(_, M, _, _), M).

virtcores(NrCoresOrig, NrCores, NrThreads, NrVCores) :-
    N is NrCores + NrCoresOrig,
    ( N =< NrThreads ->
        virtcores(NrCoresOrig, N, NrThreads, NrVCores);
        NrVCores = NrCores
    ).


count_to_distr_cores(AvailableCores, Threads, DistrCores) :-
    maplist(to_distr_cores(AvailableCores), Threads, L),
    sum(L,DistrCores).

to_distr_cores(AvailableCores, thread(_, M, _, _), C) :-
    ( M < AvailableCores ->
        C is AvailableCores - M;
        C is 0
    ).

adjust_nr_cores(_, NewDistrCores, [], [], NewDistrCores).
adjust_nr_cores(AvailableCores, DistrCores, [thread(N, M, B1, B2)|Inp], [thread(N, C, B1, B2)|Outp], NewDistrCores) :-
    M =< AvailableCores,
    C = M,
    adjust_nr_cores(AvailableCores, DistrCores, Inp, Outp, NewDistrCores).
adjust_nr_cores(AvailableCores, DistrCores, [thread(N, M, B1, B2)|Inp], [thread(N, C, B1, B2)|Outp], NewDistrCores) :-
    M > AvailableCores,
    Tmp is AvailableCores + DistrCores,
    min(M, Tmp, C),
    RemC is Tmp - C,
    adjust_nr_cores(AvailableCores, RemC, Inp, Outp, NewDistrCores).




create_schedule(_, _, _, _, [], [], _, []).
create_schedule(NrCores, NrVCores, NrThreads, CoreIDs, SyncThreads, NoSyncThreads, Time, S) :-
    minimize(add_sync(SyncThreads, NrCores, Threads, RemSync, RemCores),RemCores),
    construct_plan(Threads, Time, SubPlan),
    write(remocres),write(RemCores),nl,
    add_nosync(NoSyncThreads, RemCores, NoSyncPlan, NoSyncRem),
    construct_plan(NoSyncPlan, Time, NoSyncSubPlan),
    append(SubPlan, NoSyncSubPlan, SP),

%% uncomment one of the next two lines and use append or permutation in
%% assign_coreids/3

%    assign_coreids(SP, CoreIDs, SPC),
    minimize((assign_coreids(SP, CoreIDs, SPC),assigned_coreids_cost(SPC,Cost)), Cost),

    write(Threads),write(RemCores),nl, write(SubPlan),nl,
    NextTime is Time + 1,
    create_schedule(NrCores, NrVCores, NrThreads, CoreIDs, RemSync, NoSyncRem, NextTime, SPNC),
    append(SPC, SPNC, S).


add_sync(SyncThreads,NrCores,L,R,C) :-
    ( not SyncThreads == [] ->
        permutation(SyncThreads,P),
        append(L,R,P),
        maplist(maxthreads, L, MaxT),
        sum(MaxT,SumT),
        SumT =< NrCores,
        C is NrCores - SumT;
        L = [],
        R = [],
        C = NrCores
    ).

permutation([], []).
permutation(SyncThreads, List) :-
    member(T, SyncThreads),
    append([T],M,List),
    subtract(SyncThreads,[T],RemainingSync),
    permutation(RemainingSync, M).

add_nosync(NoSyncThreads, NrCores, L, RemTNext) :-
    ( not NoSyncThreads == [] ->
        minimize(add_nosync_block(NoSyncThreads, NrCores, Threads, RemT, RemCores), RemCores),
        write(nosync),write(Threads),nl,
        add_nosync_part(RemT, RemCores, ThreadsPart, RemTNext),
        append(Threads, ThreadsPart, L);
        L = [],
        RemTNext = []
    ).

add_nosync_block(NoSyncThreads, NrCores, L, R, C) :-
    append(L,R,NoSyncThreads),
    maplist(maxthreads, L, MaxT),
    sum(MaxT,SumT),
    SumT =< NrCores,
    C is NrCores - SumT.

add_nosync_part([T|NoSyncThreads], RemCores, [TP], [PartT|NoSyncThreads]) :-
    thread(N, M, B1, B2) = T,
    TP = thread(N, RemCores, B1, B2),
    MissingThreads is M - RemCores,
    PartT = thread(N, MissingThreads, B1, B2).
add_nosync_part([], _, [], []).


construct_plan([], _, []).
construct_plan([T|Threads], Time, [P|Plan]) :-
    P = exec(Time, T),
    construct_plan(Threads, Time, Plan).


assign_coreids([], _, []).
assign_coreids([exec(T, Thread)|Plan], CoreIDs, [exec(T, Cores, Thread)|Schedule]) :-
    thread(_, M, _, _) = Thread,
    length(Cores, M),

%% either the next line or the two lines after the next
%    append(Cores, Rem, CoreIDs),
%    permutation(CoreIDs, CoreIDsPerm),
%    append(Cores, Rem, CoreIDsPerm),
    rotation(CoreIDs, CoreIDsRot),
    append(Cores, Rem, CoreIDsRot),

    assign_coreids(Plan, Rem, Schedule).

assigned_coreids_cost([], 0).
assigned_coreids_cost([exec(_, _, thread(_, _, _, false))|L], C) :-
    assigned_coreids_cost(L, C).
assigned_coreids_cost([exec(_, CoreIDs, thread(_, _, _, true))|L], C) :-
    maplist(coreid_to_packageid, CoreIDs, PackageIDs),
    no_dups(PackageIDs, Pkgs),
    length(Pkgs, CTmp),
    assigned_coreids_cost(L, CSub),
    C is CTmp + CSub.

coreid_to_packageid(CoreID, PackageID) :-
    cpu_thread(CoreID, PackageID, _, _).


no_dups([], []).
no_dups([H|T1],[H|T2]) :-
    no_dups(T1, T2),
    not member(H, T2).
no_dups([H1|T1],L) :-
    no_dups(T1, L),
    member(H1,L).


rotation(L1,L2) :-
    length(L1, Len),
    rotationimp(L1, L2, Len).

rotationimp([], [], _).
rotationimp([H|T],L, Len) :-
    L = [H|T];
    Len > 1,
    append(T,[H],Tmp),
    LenNew is Len - 1,
    rotationimp(Tmp, L, LenNew).

