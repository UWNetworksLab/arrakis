%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, 2011, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-lib(ic).
:-lib(ic_global).
:-use_module(library(ic_edge_finder)).
:-lib(branch_and_bound).

% :-include("../data/globalthread_testdata.txt").

:- set_flag(print_depth, 200).

% :-dynamic(currentbar/5).

:-dynamic(task_currently_allocated/3).
:-dynamic(task_status/5).
:-dynamic(task_statistics/6).
:-dynamic(task_max_parallel/3).



%CLP: Aus Daten und Information Wissen machen -> Siehe Buch Qin


%Regla
% 1. #Threads <= Nr (z.Bsp. minimale Blockgroesse pro Thread)
% 2. #Threads >= Nr
% 3. Wenn Blockgroesse < Cachelinegroesse => Threads auf HT gruppieren
% 4. Wenn Threads auf Puffer operieren ((de-)codieren, maplist,...)
%    dann HTs benutzen
% 5. Wenn Threads intensive Berechnungen von wenigen Variablen durchfuehren,
%    dann echte Cores benutzen (ev. zweiter HT als Mem-Prefetcher benutzen)
% 6. Global: Sporadische Threads gruppieren, Threads die lange im System
%    bleiben und lange auf gleichem Puffer arbeiten (Netzwerk, Server)
%    als eigene Gruppe auf #Cores gruppieren
% 7. Services bauen: OpenSSL-Service, der AES-Ver- und -Entschluesselung
%    anbietet und dadurch lange im System bleibt und weiss, wo wann welche
%    Threads zum Einsatz kommen
% 8. Allen Services jeweils sagen, dass sie jetzt so und so viele Threads
%    auf diesen und diesen Cores benutzen koennen
% 9. User-Apps, die schnell kommen und gehen auf Cores gruppieren

%10. Applikation soll definieren, welche Threads miteinander kommunizieren
%    oder synchronisieren. Algo hier soll dann diese Threads moeglichst
%    nahe voneinander plazieren.

%11. Applikation soll definieren, dass sie nur auf Core, der bla unterstuetzt,
%    laufen kann (beispielsweise einer, der FPU hat). Algo soll entsprechend
%    nur diese Cores auswaehlen

%12. Algo soll FPU threads moeglichst auf verschiedene Cores setzen, damit das
%    Betriebssystem keinen FPU-Kontext-Switch machen muss. Dieser wird
%    nur on-demand by FPU-Exception (interrupt) gemacht, falls neuer Thread
%    nach Switch das erstemal FPU-Instruktion ausfuehrt. Wenn man nur einen
%    Thread pro Core mit FPU-Instruktionen laufen laesst, muss nie FPU-Exception
%    behandelt werden.


% HW data
% cpu_affinity(APIC, LocalSApicEid, ProximityDomain).

%cpu_affinity(0, 0, 0).
%cpu_affinity(1, 0, 0).
%cpu_affinity(2, 0, 0).
%cpu_affinity(3, 0, 0).
%cpu_affinity(4, 0, 0).
%cpu_affinity(5, 0, 0).

%cpu_affinity(6, 0, 1).
%cpu_affinity(7, 0, 1).
%cpu_affinity(8, 0, 1).
%cpu_affinity(9, 0, 1).
%cpu_affinity(10, 0, 1).
%cpu_affinity(11, 0, 1).

%cpu_affinity(12, 0, 2).
%cpu_affinity(13, 0, 2).
%cpu_affinity(14, 0, 2).
%cpu_affinity(15, 0, 2).
%cpu_affinity(16, 0, 2).
%cpu_affinity(17, 0, 2).

%cpu_affinity(18, 0, 3).
%cpu_affinity(19, 0, 3).
%cpu_affinity(20, 0, 3).
%cpu_affinity(21, 0, 3).
%cpu_affinity(22, 0, 3).
%cpu_affinity(23, 0, 3).

affinity_domain_list(Domains) :-
    findall(D, cpu_affinity(_,_,D),DomainList),
    affinity_domain_list(DomainList, Domains).
affinity_domain_list([H|DomainList], [H|Domains]) :-
    subtract(DomainList, [H], DomainListNew),
    affinity_domain_list(DomainListNew, Domains).
affinity_domain_list([], []).

get_numa_domain_for_core(CoreNumber, NUMADomain, RL, RH, MaxSize) :-
    ( is_predicate(cpu_affinity/3),cpu_affinity(CoreNumber, _, NUMADomain) ->
        findall(L, memory_affinity(L, _, NUMADomain), AfL),
        findall(H, (memory_affinity(L1, S, NUMADomain), H is L1 + S), AfH),
        findall(Sz, memory_affinity(_, Sz, NUMADomain), Sizes),
        eclipse_language:min(AfL, RL),
        eclipse_language:max(AfH, RH),
        sum(Sizes, MaxSize)
        ;
        NUMADomain = 0,
        RL = 0,
        RH = 4294967295,
        MaxSize = 4294967295
    ).

% task(functionaddress, clientsocket, minblocksize(sz)).
%task(t1, 1, minblocksize(1)).
%task(t2, 2, minblocksize(1)).
%task(t3, 3, minblocksize(1)).
%task(t4, 4, minblocksize(1)).

% task_max_parallel(t1, Socket, 4).
%task_max_parallel(t1, 1, 4).
%task_max_parallel(t2, 2, 2).
%task_max_parallel(t3, 3, 3).
%task_max_parallel(t4, 4, 1).


% data entries to define whether the task is compute or memory bound
%task_numa(functionaddress, clientid, memory).
% -> spread threads on different NUMA domains

%task_numa(functionaddress, clientid, compute).
% -> fill one NUMA domain (use all the cores on the same domain), before using
%    cores on other domains




task_get_max_parallel(Name, ClientID, MaxNr) :-
    core_list_id(CoreIDList),
    length(CoreIDList, NrRunningCores), 
    ( task_max_parallel(Name, ClientID, MaxNr) ->
        true
        ;
        MaxNr is NrRunningCores
    ).


nr_running_tasks(Nr) :-
    findall(_, (task(Name,ClientID,_),task_status(Name,ClientID,_,_,running)), TaskList),
    length(TaskList, Nr).

task_list(TaskList) :-
    findall(taskname(Name,ClientID), ( task(Name,ClientID,_), task_running_status(Name, ClientID, running)), TaskNames),
    length(TaskNames, Len),
    ( foreach(N, TaskNames),
      foreach(Task, TaskList),
      for(I, 1, Len)
      do
        Task = task(I, N)
    ).

core_list_hw_id(CoreList):-
    findall(HWID, corename(_,_,apic(HWID)), CoreList).

% use higlevel name, that makes it easier
core_list_id(CoreList):-
    findall(corename(ID), corename(ID,_,_), CoreList).


get_max_parallel(task(_, N), N).

sum_max_parallel_threads(Sum) :-
    findall(S,(task(N,ClientID,_),task_get_max_parallel(N,ClientID,S), task_running_status(N, ClientID,running)),L),
    sum(L, Sum).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create data structure
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%access_nr(_, [], []).
%access_nr(1, [H|_], H).
%access_nr(N, [_|T], El) :-
%    M is N - 1,
%    access_nr(M, T, El).

access_nr(N, [H|T], El) :-
    ( N = 1 ->
        El = H;
      N < 1 ->
        El = [];
      M is N - 1,
      access_nr(M, T, El)
    ).

transpose_matrix(M, CoreList) :-
%    nr_running_cores(NrRunningCores),
    nr_running_tasks(NrRunningTasks),
    core_list_hw_id(CoreIDList),
    length(CoreIDList, NrRunningCores), 
    ( for(I, 1, NrRunningCores),
      foreach(core(I, _, TaskList), CoreList),
      param(NrRunningTasks),
      param(M)
      do
      ( for(J,1,NrRunningTasks),
        foreach(task(J, TaskNama, TaskVar), TaskList),
        param(M),
        param(I)
        do
          access_nr(J, M, TEl),
          task(_, TaskNama, TCoreList) = TEl,
          access_nr(I, TCoreList, CEl),
          core(_, _, TaskVar) = CEl
      )
    ).

thread_alloc_create_datastructure(TaskList, CoreList) :-
%    nr_running_cores(NrRunningCores),
    nr_running_tasks(NrRunningTasks),
    task_list(TLNames),
    core_list_hw_id(CoreIDList),
    length(CoreIDList, NrRunningCores), 
    ( for(I, 1, NrRunningTasks),
      foreach(task(_,TaskNama), TLNames),
      foreach(task(I, TaskNama, CoreList), TaskList),
      param(NrRunningCores),
      param(CoreIDList)
      do
      ( for(J, 1, NrRunningCores),
        foreach(HWID, CoreIDList),
        foreach(core(J, HWID, Core), CoreList)
        do
        Core::[0..1]
      )
    ),
    transpose_matrix(TaskList, CoreList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% instantiate a variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

globalthreadalloc_labelall(TaskList) :-
    ( foreach(Task, TaskList),
      foreach(VarList, VarLists)
      do
        task(_,_, TaskVarList) = Task,
        maplist(corelistvariable, TaskVarList, VarList)
    ),
    flatten(VarLists, VarListsF),
    labeling(VarListsF).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tasklistvariable(task(_, _, Var), Var).

nr_tasks_per_core(MaxNr, CoreList) :-
    ( foreach(Core, CoreList),
      param(MaxNr)
      do
        core(_, _,CoreVarList) = Core,
        maplist(tasklistvariable, CoreVarList, CoreVars),
        ic_global:sumlist(CoreVars, CoreSum),
        CoreSum $=< MaxNr
    ).

corelistvariable(core(_, _, Var), Var).


% explicit min and max nr threads per task
nr_threads_per_task(MinNr, MaxNr, TaskList) :-
    ( foreach(Task, TaskList),
      param(MinNr),
      param(MaxNr)
      do
        task(_, _, TaskVarList) = Task,
        maplist(corelistvariable, TaskVarList, TaskVars),
        ic_global:sumlist(TaskVars, TaskSum),
        TaskSum $>= MinNr,
        TaskSum $=< MaxNr
    ).


% max nr threads set according to stored data:
% task_max_parallel(taskname, maxnr threads).

nr_threads_per_task(TaskList) :-
    ( foreach(Task, TaskList)
      do
        task(_, TaskName, TaskVarList) = Task,
        taskname(TN,TClID) = TaskName,
        task_get_max_parallel(TN, TClID, MaxNr),
        maplist(corelistvariable, TaskVarList, TaskVars),
        ic_global:sumlist(TaskVars, TaskSum),
        TaskSum $=< MaxNr
    ).


% this is the main function handling the per task NUMA properties (no global
% NUMA properties are handled here).
% Actually we cannot specify that one task should evenly spread its threads
% over all NUMA regions and at the same time specify globally that one NUMA
% region can only have one task on it. This would violate that a massively
% parallel task evenly distributes its threads on _all_ NUMA regions. Therefore
% specifying both at the same time is contradicting and would not work.
% If at ll, we would need to make sure that one task only gets a subset of
% all available NUMA regions and therefore only a subset of all cores already
% from the beginning when assigning cores to each task.

numa_properties_per_task(TaskList) :-
    affinity_domain_list(L),
    ( foreach(Task, TaskList),
      param(L)
      do
        task(_, TaskName, TaskVarList) = Task,
        ( foreach(D,L),
          foreach(CList, CLists),
          param(TaskVarList)
          do
%            findall(core(_,corename(CoreNr),_),cpu_affinity(CoreNr,_,D),SubtractList),
            findall(core(_,CoreNr,_),cpu_affinity(CoreNr,_,D),SubtractList),
            subtract(TaskVarList,SubtractList,TmpList),
            subtract(TaskVarList,TmpList,CList)
        ),
        apply_numa_properties(TaskName, CLists)
    ).


% this function decides which NUMA policy applies to which task

apply_numa_properties(taskname(FunctionAddress, ClID), NUMACoreList) :-
    ( is_predicate(task_numa/3),task_numa(FunctionAddress, ClID, memory) ->
        apply_numa_properties_memory(NUMACoreList)
      ;
       is_predicate(task_numa/3),task_numa(FunctionAddress, ClID, compute) ->
        apply_numa_properties_compute(NUMACoreList)
      ;
       is_predicate(task_working_set_size/3), task_working_set_size(FunctionAddress, ClID, WorkingSetSize) ->
        apply_numa_properties_max_numa_size(NUMACoreList, WorkingSetSize)
      ;
      true
    ).


% the concrete NUMA policy implementations

apply_numa_properties_compute(NUMACoreList) :-
    ( foreach(NUMARegion, NUMACoreList),
      foreach(B, NUMASums)
      do
        maplist(corelistvariable, NUMARegion, NUMARegionVars),
        ic_global:sumlist(NUMARegionVars, Sum),
        length(NUMARegionVars, Len),
        and(Sum $\= 0, Sum $\= Len, B)
    ),
    ic_global:sumlist(NUMASums, Sum),
    Sum $=< 1.

apply_numa_properties_memory(NUMACoreList) :-
    ( foreach(NUMARegion, NUMACoreList),
      foreach(Sum, NUMASums)
      do
        maplist(corelistvariable, NUMARegion, NUMARegionVars),
        ic_global:sumlist(NUMARegionVars, Sum)
    ),
    ic:minlist(NUMASums, Min),
    ic:maxlist(NUMASums, Max),
    Diff $= Max - Min,
    Diff $=< 1.

apply_numa_properties_max_numa_size(NUMACoreList, WorkingSetSize) :-
    ( foreach(NUMARegion, NUMACoreList),
      foreach(Sum, NUMASums)
      do
        maplist(corelistvariable, NUMARegion, NUMARegionVars),
        ic_global:sumlist(NUMARegionVars, Sum)
    ),
    findall(A, memory_affinity(_, _, A), AL),
    sort(AL, ALS),
    ( foreach(Af, ALS),
      foreach(Sz, NodeSizes)
      do
        findall(Size, memory_affinity(_ , Size, Af), Sizes),
        sum(Sizes, Sz)
    ),
    ( foreach(N, NodeSizes),
      foreach(NSum, NUMASums),
      param(WorkingSetSize)
      do
        AllocSize $= NSum * WorkingSetSize,
        AllocSize $=< N
    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjust_ax_parallel_threads([], _, _).
adjust_ax_parallel_threads([H|TaskList], NrRunningCores, Sum):-
    task(_, TaskName, TaskVarList) = H,
    taskname(TN, TClID) = TaskName,
    task_get_max_parallel(TN, TClID, MaxNr),
    EffektivNumberFloat is (NrRunningCores / Sum * MaxNr),
    floor(EffektivNumberFloat, EffektivNumberFloor),
    integer(EffektivNumberFloor, EffektivNumberInt),
    ( EffektivNumberInt =:= 0 ->
        EffektivNumber is 1
        ;
        EffektivNumber is EffektivNumberInt
    ),
    min(EffektivNumber, MaxNr, DefNr),
    maplist(corelistvariable, TaskVarList, TaskVars),
    ic_global:sumlist(TaskVars, TaskSum),
    TaskSum $= DefNr,
    NewNumberRunningCores is NrRunningCores - EffektivNumber,
    NewSum is Sum - MaxNr,
    adjust_ax_parallel_threads(TaskList, NewNumberRunningCores, NewSum).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main internal goal: create data structure, apply constraints, instantiate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

haupt(TaskList) :-
    thread_alloc_create_datastructure(TaskList, CoreList),
    nr_tasks_per_core(1, CoreList),
%    nr_running_cores(NrRunningCores),
    core_list_hw_id(CoreIDList),
    length(CoreIDList, NrRunningCores), 
%    nr_threads_per_task(1,NrRunningCores, TaskList),
%    nr_threads_per_task(TaskList),
    sum_max_parallel_threads(Sum),
    adjust_ax_parallel_threads(TaskList, NrRunningCores, Sum),
    numa_properties_per_task(TaskList),
    globalthreadalloc_labelall(TaskList).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% transforming solution to get a nice output
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get all the allocated cores (the ones which have a "1")
% this is a kind of filter function which creates a list of allocated cores
% per task

allocated_cores([], []).
allocated_cores([H|CoreList], AllocatedCoresList) :-
    core(_, _, 0) = H,
    allocated_cores(CoreList, AllocatedCoresList).
allocated_cores([H|CoreList], [HWID|AllocatedCoresList]) :-
    core(_, HWID, 1) = H,
    allocated_cores(CoreList, AllocatedCoresList).



% get all the allocated cores for each task

allocation_output(TaskList, AllocatedCoresList) :-
    ( foreach(Task, TaskList),
      foreach(task(TaskName,AllocatedCores), AllocatedCoresList)
      do
        task(_, TaskName, CoreList) = Task,
        allocated_cores(CoreList, AllocatedCores)
    ).


% Compute the allocated core difference per task. This difference defines
% which cores have to be added and which ones have to be removed according
% to the last allocation. Every element contains the task as well as the
% core name and an operation (1 = add this core to the task, 0 = remove this
% core from this task).

allocation_difference(AllocatedCores, OpsisF) :-
    ( foreach(Task, AllocatedCores),
      foreach(Ops, Opsis)
      do
        task(taskname(Name,TClID), NewCoreList) = Task,
        ( is_predicate(task_currently_allocated/3),task_currently_allocated(Name, TClID, CurrentCoreList) ->
            true
            ;
            CurrentCoreList = []
        ),
        ( is_predicate(task_currently_allocated/3),retract(task_currently_allocated(Name, TClID, CurrentCoreList)) ->
            true;
            true
        ),
        assert(task_currently_allocated(Name, TClID, NewCoreList)),
        subtract(CurrentCoreList, NewCoreList, RemoveCores),
        subtract(NewCoreList, CurrentCoreList, AddCores),
        ( foreach(AddCore, AddCores),
          foreach(El, OperationsList),
          param(Name),
          param(TClID)
          do
%% XXX: WARNING: there is a mess between hardware corenames and core numbers as used in the OS
%%               this only works on Linux where we only see core numbers allocated by the kernel
%%               and no apic IDs
%            corename(CN) = AddCore,
            CN = AddCore,
            get_numa_domain_for_core(CN, NUMADomain, Low, High, Sz),
%            El = task_thread(Name, TClID, corename(AddCore), 1, numa(NUMADomain))
            El = task_thread(Name, TClID, AddCore, 1, numa(NUMADomain, Low, High, Sz))
%            El = task_thread(Name, TClID, corename(AddCore), 1)
        ),
        ( foreach(RemoveCore, RemoveCores),
          foreach(El2, OperationsList2),
          param(Name),
          param(TClID)
          do
%% XXX: WARNING: there is a mess between hardware corenames and core numbers as used in the OS
%%               this only works on Linux where we only see core numbers allocated by the kernel
%%               and no apic IDs
%            corename(CN2) = RemoveCore,
            CN2 = RemoveCore,
            get_numa_domain_for_core(CN2, NUMADomain, Low, High, Sz),
%            El2 = task_thread(Name, TClID, corename(RemoveCore), 0, numa(NUMADomain))
            El2 = task_thread(Name, TClID, RemoveCore, 0, numa(NUMADomain, Low, High, Sz))
%            El2 = task_thread(Name, TClID, corename(RemoveCore), 0)
        ),
        append(OperationsList, OperationsList2, Ops)
    ),
    flatten(Opsis, OpsisF).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this is the main goal called regularly by the resource manager
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

task_allocation_plan(P) :-
    haupt(Plan),
    !,
    allocation_output(Plan, AllocatedCores),
    allocation_difference(AllocatedCores, P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
task_add_statistics(FunctionAddress, ClientSocket, tsc(TSCTicks), nrcores(NrC), startaddr(Start), endaddr(End)) :-
    assert(task_statistics(FunctionAddress, ClientSocket, tsc(TSCTicks), nrcores(NrC), startaddr(Start), endaddr(End))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
task_config(FunctionAddress, ClID, Start, End, running):-
    ( is_predicate(task_status/5),task_status(FunctionAddress, ClID, _, _, _) ->
        retract(task_status(FunctionAddress, ClID, _, _, _))
        ;
        true
    ),
    assert(task_status(FunctionAddress, ClID, Start, End, running)).

task_config(FunctionAddress, ClID, Start, End, stopped):-
    ( is_predicate(task_status/5),task_status(FunctionAddress, ClID, _, _, _) ->
        retract(task_status(FunctionAddress, ClID, _, _, _))
        ;
        true
    ),
    ( is_predicate(task_currently_allocated/3) ->
        retractall(task_currently_allocated(FunctionAddress, ClID, _))
        ;
        true
    ),
    assert(task_status(FunctionAddress, ClID, Start, End, stopped)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Limit the Parellelism that this task can get
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
task_set_max_parallel(FunctionAddress, ClID, MaxNr) :-
    ( is_predicate(task_max_parallel/3) ->
        retractall(task_max_parallel(FunctionAddress, ClID,_))
        ;
        true
    ),
    assert(task_max_parallel(FunctionAddress, ClID, MaxNr)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set the working set size per core
% This helps deciding whether the sum of the working set of all cores
% belonging to the same NUMA node does not exceed the size of the NUMA
% node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
task_set_working_set_size(FunctionAddress, ClID, WorkingSetSize) :-
    ( is_predicate(task_working_set_size/3) ->
        retractall(task_working_set_size(FunctionAddress, ClID, _))
        ;
        true
    ),
    assert(task_working_set_size(FunctionAddress, ClID, WorkingSetSize)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Register a function. Called by the resource manager
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
task_register_function(FunctionAddress, ClID, MinBlockSize):-
    ( is_predicate(task/3) ->
        retractall(task(FunctionAddress, ClID, _))
        ;
        true
    ),
    assert(task(FunctionAddress, ClID, minblocksize(MinBlockSize))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
task_running_status(FunctionAddress, ClID, Running) :-
    ( is_predicate(task_status/5),task_status(FunctionAddress, ClID, _, _, Running) ->
        true
        ;
        Running = nonexistent
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
task_remove_client(ClID):-
    ( is_predicate(task/3),task(_, ClID, _) ->
        retract(task(_, ClID, _))
        ;
        true
    ),
    ( is_predicate(task_max_parallel/3),task_max_parallel(_, ClID, _) ->
      retract(task_max_parallel(_, ClID, _))
      ;
      true
    ),
    ( is_predicate(task_status/5),task_status(_, ClID, _, _, _) ->
        retract(task_status(_, ClID, _, _, _))
        ;
        true
    ),
    ( is_predicate(task_currently_allocated/3),task_currently_allocated(_, ClID, _) ->
        retract(task_currently_allocated(_, ClID, _))
        ;
        true
    ),
    ( is_predicate(task_statistics/6),task_statistics(_, ClID, _, _, _, _) ->
        retract(task_statistics(_, ClID, _, _, _, _))
        ;
        true
    ),
    ( is_predicate(task_working_set_size/3), task_working_set_size(_, ClID, _) ->
        retractall(task_working_set_size(_ , ClID, _))
        ;
        true
    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


