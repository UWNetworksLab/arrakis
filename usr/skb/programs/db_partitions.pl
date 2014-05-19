% :-include("../data/data_sbrinz1.txt").
% :-include("../data/data_nos4.txt").

:-include("globalthreadalloc.pl").


%% asq: If we know the data store size, what should we do?
%%      Two options:
%%         1.: Create NUMANode-sized partitions and place multiple partitions
%%             on same nodes
%%         2.: Ignore and just return the same number of partitions as there are NUMA nodes


partitions_get_optimal_number_of_partitions(_, _, NrPartitions, Partitions) :-
    task_register_function(987654321, 987654321, 0),
    task_config(987654321, 987654321, 0, 0, running),
    haupt(Plan),
    !,
    allocation_output(Plan, AllocatedCores),
    member(task(taskname(987654321,987654321), CoreList), AllocatedCores),
    ( foreach(Core, CoreList),
      foreach(partition(RL, RH, MaxSize, Core), Partitions)
      do
        cpu_affinity(Core, _, AffinityDomain),
        findall(L, memory_affinity(L, _, AffinityDomain), AfL),
        findall(H, (memory_affinity(L1, S, AffinityDomain), H is L1 + S), AfH),
        findall(Sz, memory_affinity(_, Sz, AffinityDomain), Sizes),
        eclipse_language:min(AfL, RL),
        eclipse_language:max(AfH, RH),
        sum(Sizes, MaxSize)
    ),
    length(Partitions, NrPartitions).

    

partitions_get_optimal_resource_allocation(NrPartitions, PartitionSize, independent, Partitions):-
    task_register_function(987654321, 987654321, 0),
    task_config(987654321, 987654321, 0, 0, running),
    task_set_max_parallel(987654321, 987654321, NrPartitions),
    task_set_working_set_size(987654321, 987654321, PartitionSize),
    haupt(Plan),
    !,
    allocation_output(Plan, AllocatedCores),
    member(task(taskname(987654321,987654321), CoreList), AllocatedCores),
    ( foreach(Core, CoreList),
      foreach(partition(RL, RH, MaxSize, Core), Partitions)
      do
        cpu_affinity(Core, _, AffinityDomain),
        findall(L, memory_affinity(L, _, AffinityDomain), AfL),
        findall(H, (memory_affinity(L1, S, AffinityDomain), H is L1 + S), AfH),
        findall(Sz, memory_affinity(_, Sz, AffinityDomain), Sizes),
        eclipse_language:min(AfL, RL),
        eclipse_language:max(AfH, RH),
        sum(Sizes, MaxSize)
    ).


partitions_cleanup :-
    task_remove_client(987654321).



multisplice([[]|_],[]).
multisplice([H|T],L) :-
    [H2|T2] = H,
    append(T, [T2], TN),
    multisplice(TN,M),
    append([H2],M,L).



