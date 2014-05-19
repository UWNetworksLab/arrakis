%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :-include("data.pl").
% :-include("boardlayout.pl").
% test data which are manually created, but can be figured out at runtime
% :-include("testdata.pl").

:- dynamic interrupt_override/4.

% 1. find list of core IDs which are closest to device add(bus, dev, fun)
%    -> that means that the HT-network has to be represented. With this information
%       we can ask this kind of information

cores_close_to_pci_device(Bus, _, _, CoreIDs) :-
    rootbridge(Addr,childbus(CLow,CHigh),_),
    Bus >= CLow,
    Bus =< CHigh,
    rootcomplex_addr(Name, Addr),
    ht_link(CPU,Name),
    findall(X,core(X,CPU),CoreIDs).

% 2. find memory region closest to core ID
%    -> this can also be queried if we have the whole connection between cores
%       memory and rootcomplexes represented

ram_closest_to(CoreID,RetBase,RetLimit) :-
	corename(CoreID,_,apic(ApicID)),
	cpu_affinity(ApicID, _, ProxDomain),
	memory_affinity(AffinityBase, AffinitySize, ProxDomain),
	AffinityLimit is AffinityBase + AffinitySize,
	memory_region(MemBase, _, MemSize, MemType, _),
	mem_region_type(MemType, ram),
	MemLimit is MemBase + MemSize,
	MemBase < AffinityLimit,
	MemLimit > AffinityBase,
	( AffinityBase < MemBase ->
	    RetBase is MemBase
	;
	    RetBase is AffinityBase),
	( AffinityLimit < MemLimit ->
	    RetLimit is AffinityLimit
	;
	    RetLimit is MemLimit).

% The above returns one range, this will return all the ranges by using findall.
ram_findall(CoreID, List) :-
	findall(range(Base,Limit), ram_closest_to(CoreID, Base, Limit), List).

% this is the version which only gives local address range whithout checking
% whether it is actually RAM. This is sufficient, if ram_set_affinity is called,
% because that will only allocate RAM anyway

local_memory_affinity(CoreID, L) :-
    is_predicate(cpu_affinity/3),
    is_predicate(memory_affinity/3),
    findall(range(Base,Limit),
            (
             cpu_affinity(CoreID,_,ProxDomain),
             memory_affinity(Base,Size,ProxDomain),
             Limit is Base + Size
            ),
            L).

extractbase(range(X,_),X).
extractlimit(range(_,X),X).

local_memory_affinity(CoreID, Base, Limit) :-
    local_memory_affinity(CoreID, L),
    ( not L=[] ->
        maplist(extractbase, L, Bases),
        maplist(extractlimit, L, Limits),
        min(Bases, Base),
        max(Limits, Limit)
      ;
        Base=0,
        Limit=0
    ).

% 3. find core ID list which share same L2/L3 cache
%    -> till now it looks like the only way to know that is from data sheets.

% cores_with_cache_sharing(Level,CoreIDs) :-
%    findall([C1,C2],cache_share(C1,C2,Level),CoreIDs).


% 4. find core IDs which do _not_ share caches

%cns(CPL) :-
%    core(X,_),core(Y,_),not X =:= Y,not cache_share(X,Y,_),not cache_share(Y,X,_),
%    CPL = [X,Y].
%
%cores_no_cache_sharing(CoreIDs) :-
%    findall(X,cns(X),CoreIDs).

% 5. find core IDs which are not on the same CPU

%cores_on_different_cpus(CoreIDs) :-
%    core(X,N1),
%    core(Y,N2),
%    not N1 =:= N2,
%    CoreIDs = [X,Y].


% 6. get a sorted list of all available APIC IDs

get_apic_id_list(L) :-
    ( is_predicate(apic/3) ->
        findall(ID, apic(_, ID, 1), TmpL), sort(TmpL, L);
        L = []
    ).

% get a sorted list of all available core ids
get_core_id_list(L) :-
    ( is_predicate(corename/3) ->
        findall(ID, corename(ID, _, _), TmpL), sort(TmpL, L);
        L = []
    ).

% 7. find the available number of cores

available_nr_cores(Nr) :-
    get_core_id_list(L),
    length(L, Nr).

% 8. figure out whether all the available cores found by pci/acpi are added to the
%    skb

is_pci_done(Status) :-
    ( is_predicate(pci_discovery_done/0) ->
        Status=yes;
        Status=no
    ).

% 9. Figure out if boot time perfmon domain is finished

is_boot_perfmon_done(Status) :-
	( is_predicate(boot_perfmon_done/0) ->
	    Status=yes;
	    Status=no
	).


get_cache_size(Level, Type, Size):-
    ( is_predicate(cache/8), cache(_, _, Level, Type, Size, _, _, _) ->
        true;
        Size is 32768
    ).




%10. Figure out whether the datagatherer is done

is_datagatherer_done(Status) :-
    ( is_predicate(datagatherer_done/0) ->
        Status=yes;
        Status=no
    ).
        
