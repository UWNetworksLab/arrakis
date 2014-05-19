%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2013, University of Washington.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This file fakes the machine config for 'bigfish' at UW. I got it
% from running Barrelfish input to ECLiPSe on Linux and dumping the
% result in here.
%
% It does a few checks to see that the machine it's running on is in
% fact the same version of bigfish. It does so by evaluating a few
% facts that should be inserted in the database. I got the facts by
% running:
%
% sed -ne 's/assert(\(.*\))\./        \1,/p' bigfish.cmds > bigfish.facts
%
% And then inserting bigfish.facts into the beginning of
% bridge_programming/2. The first time this actually blew the stack
% of the SKB, so I removed all unneccessary things and kept only
% things relevant to PCI bridge programming.

:-lib(ic).
:-lib(ic_global).
:-use_module(library(ic_edge_finder)).

:- set_flag(print_depth, 200).

:-dynamic(currentbar/5).

% :-include("../data/data_hand.txt").
% :-include("../data/data_qemu_hand.txt").
% :-include("../data/data_qemu.txt").
% :-include("../data/data_nos3.txt").
% :-include("../data/data_nos4.txt").
% :-include("../data/data_nos5.txt").
% :-include("../data/data_nos6.txt").
% :-include("../data/data_gruyere.txt").
% :-include("../data/data_sbrinz1.txt").
% :-include("../data/data_loner.txt").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main goal to be called from outside
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bridge_programming(Plan, NrElements) :-
        ioapic(0,4273995776,0),
        memory_region(4273995776,12,4096, 6,0),
        ioapic(1,4274520064,24),
        memory_region(4274520064,12,4096, 6,0),
        ioapic(2,4274782208,56),
        memory_region(4274782208,12,4096, 6,0),
        fixed_memory(4274585600,4274601984),
        fixed_memory(4274847744,4274864128),
        rootbridge_address_window(addr(0, 0, 0), mem(655360, 786431)),
        rootbridge_address_window(addr(0, 0, 0), mem(4273995776, 4274520063)),
        rootbridge_address_window(addr(0, 0, 0), mem(4274520064, 4274782207)),
        rootbridge_address_window(addr(0, 0, 0), mem(3925868544, 4016046079)),
        rootbridge_address_window(addr(0, 0, 0), mem(3917479936, 3925868543)),
        rootbridge_address_window(addr(0, 0, 0), mem(4275306496, 4275326975)),
        rootbridge(addr(0,0,0),childbus(0,30),mem(3925868544,4016046079)),
        rootbridge_address_window(addr(32, 0, 0), mem(4274782208, 4275044351)),
        rootbridge_address_window(addr(32, 0, 0), mem(3900702720, 3917479935)),
        rootbridge(addr(32,0,0),childbus(32,62),mem(3900702720,3917479935)),
        memory_affinity(0, 655360, 0),
        memory_affinity(1048576, 3220176896, 0),
        memory_affinity(4294967296, 1073741824, 0),
        memory_affinity(5368709120, 4294967296, 1),
        bridge(pcie,addr(0,2,0),4098,23062,6,4,0, secondary(1)),
        bridge(pcie,addr(0,4,0),4098,23064,6,4,0, secondary(3)),
        bridge(pcie,addr(3,0,0),4277,34340,6,4,0, secondary(4)),
        bar(addr(3, 0, 0), 0, 16'ef2e0000, 16'20000, mem, nonprefetchable, 32),
        bridge(pcie,addr(4,0,0),4277,34340,6,4,0, secondary(5)),
        NrElements = 41,
        Plan = [[buselement(device, addr(1, 0, 0), 0, 3925868544, 3959422976, 33554432, mem, nonprefetchable, pcie, 64), buselement(device, addr(1, 0, 1), 0, 3959422976, 3992977408, 33554432, mem, nonprefetchable, pcie, 64), buselement(device, addr(9, 0, 0), 0, 3992977408, 3993501696, 524288, mem, nonprefetchable, pcie, 64), buselement(device, addr(9, 0, 1), 0, 3993501696, 3994025984, 524288, mem, nonprefetchable, pcie, 64), buselement(device, addr(5, 0, 0), 3, 3995074560, 3995336704, 262144, mem, nonprefetchable, pcie, 64), buselement(device, addr(5, 0, 0), 1, 3995336704, 3995402240, 65536, mem, nonprefetchable, pcie, 64), buselement(device, addr(9, 0, 0), 4, 3994025984, 3994042368, 16384, mem, nonprefetchable, pcie, 64), buselement(device, addr(9, 0, 1), 4, 3994042368, 3994058752, 16384, mem, nonprefetchable, pcie, 64), buselement(device, addr(0, 17, 0), 5, 3996123136, 3996127232, 4096, mem, nonprefetchable, pcie, 32), buselement(bridge, addr(0, 20, 4), secondary(10), 3925868544, 3925868544, 0, mem, nonprefetchable, pcie, 0), buselement(bridge, addr(0, 9, 0), secondary(9), 3992977408, 3995074560, 2097152, mem, nonprefetchable, pcie, 0), buselement(bridge, addr(4, 5, 0), secondary(8), 3995074560, 3995074560, 0, mem, nonprefetchable, pcie, 0), buselement(bridge, addr(4, 4, 0), secondary(7), 3995074560, 3995074560, 0, mem, nonprefetchable, pcie, 0), buselement(bridge, addr(4, 1, 0), secondary(6), 3995074560, 3995074560, 0, mem, nonprefetchable, pcie, 0), buselement(bridge, addr(4, 0, 0), secondary(5), 3995074560, 3996123136, 1048576, mem, nonprefetchable, pcie, 0), buselement(bridge, addr(3, 0, 0), secondary(4), 3995074560, 3996123136, 1048576, mem, nonprefetchable, pcie, 0), buselement(bridge, addr(0, 4, 0), secondary(3), 3995074560, 3996123136, 1048576, mem, nonprefetchable, pcie, 0), buselement(bridge, addr(0, 3, 0), secondary(2), 3925868544, 3925868544, 0, mem, nonprefetchable, pcie, 0), buselement(bridge, addr(0, 2, 0), secondary(1), 3925868544, 3992977408, 67108864, mem, nonprefetchable, pcie, 0), buselement(bridge, addr(0, 20, 4), secondary(10), 3925868544, 3925868544, 0, mem, prefetchable, pcie, 0), buselement(bridge, addr(0, 9, 0), secondary(9), 3925868544, 3925868544, 0, mem, prefetchable, pcie, 0), buselement(bridge, addr(4, 5, 0), secondary(8), 3925868544, 3925868544, 0, mem, prefetchable, pcie, 0), buselement(bridge, addr(4, 4, 0), secondary(7), 3925868544, 3925868544, 0, mem, prefetchable, pcie, 0), buselement(bridge, addr(4, 1, 0), secondary(6), 3925868544, 3925868544, 0, mem, prefetchable, pcie, 0), buselement(bridge, addr(4, 0, 0), secondary(5), 3925868544, 3925868544, 0, mem, prefetchable, pcie, 0), buselement(bridge, addr(3, 0, 0), secondary(4), 3925868544, 3925868544, 0, mem, prefetchable, pcie, 0), buselement(bridge, addr(0, 4, 0), secondary(3), 3925868544, 3925868544, 0, mem, prefetchable, pcie, 0), buselement(bridge, addr(0, 3, 0), secondary(2), 3925868544, 3925868544, 0, mem, prefetchable, pcie, 0), buselement(bridge, addr(0, 2, 0), secondary(1), 3925868544, 3925868544, 0, mem, prefetchable, pcie, 0)], [buselement(device, addr(33, 0, 0), 1, 3900702720, 3904897024, 4194304, mem, nonprefetchable, pcie, 32), buselement(device, addr(33, 0, 1), 1, 3904897024, 3909091328, 4194304, mem, nonprefetchable, pcie, 32), buselement(device, addr(33, 0, 0), 0, 3909091328, 3909222400, 131072, mem, nonprefetchable, pcie, 32), buselement(device, addr(33, 0, 1), 0, 3909222400, 3909353472, 131072, mem, nonprefetchable, pcie, 32), buselement(device, addr(33, 0, 0), 3, 3909353472, 3909369856, 16384, mem, nonprefetchable, pcie, 32), buselement(device, addr(33, 0, 1), 3, 3909369856, 3909386240, 16384, mem, nonprefetchable, pcie, 32), buselement(bridge, addr(32, 11, 0), secondary(35), 3900702720, 3900702720, 0, mem, nonprefetchable, pcie, 0), buselement(bridge, addr(32, 3, 0), secondary(34), 3900702720, 3900702720, 0, mem, nonprefetchable, pcie, 0), buselement(bridge, addr(32, 2, 0), secondary(33), 3900702720, 3910139904, 9437184, mem, nonprefetchable, pcie, 0), buselement(bridge, addr(32, 11, 0), secondary(35), 3900702720, 3900702720, 0, mem, prefetchable, pcie, 0), buselement(bridge, addr(32, 3, 0), secondary(34), 3900702720, 3900702720, 0, mem, prefetchable, pcie, 0), buselement(bridge, addr(32, 2, 0), secondary(33), 3900702720, 3900702720, 0, mem, prefetchable, pcie, 0)]].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% small tools
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjust_range(X, buselement(T,A,Sec,B1,H1,S,Tp,PF, PCIe, Bits), buselement(T,A,Sec,B2,H2,S,Tp,PF, PCIe, Bits)) :-
    B2 is B1 + X,
    H2 is H1 + X.

back_to_bytes(Granularity, buselement(T,A,Sec,BP,HP,SP,Tp,PF, PCIe, Bits), buselement(T,A,Sec,B,H,S,Tp,PF, PCIe, Bits)) :-
    B is BP * Granularity,
    H is HP * Granularity,
    S is SP * Granularity.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the main part of the allocation. Called once per root bridge
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bridge_assignment(Plan, Root, Granularity, ExclRanges, IOAPICs) :-
    root(Addr,childbus(MinBus,MaxBus),mem(LMem,HMem)) = Root,
    X is HMem - LMem,
    Type = mem,

% prefetchable
    constrain_bus(Granularity, Type, prefetchable, Addr,MinBus,MaxBus,LMem,HMem,BusElementListP),
    RBaseP::[LMem..HMem],
    RHighP::[LMem..HMem],
    RSizeP::[0..X],
    devicetree(BusElementListP,buselement(bridge,Addr,secondary(MinBus),RBaseP,RHighP,RSizeP, Type, prefetchable, _, _),TP),

% nonprefetchable
    constrain_bus(Granularity, Type, nonprefetchable, Addr,MinBus,MaxBus,LMem,HMem,BusElementListNP),
    RBaseNP::[LMem..HMem],
    RHighNP::[LMem..HMem],
    RSizeNP::[0..X],
    devicetree(BusElementListNP,buselement(bridge,Addr,secondary(MinBus),RBaseNP,RHighNP,RSizeNP, Type, nonprefetchable, _, _),TNP),

% pseudo-root of both trees
    PseudoBase::[LMem..HMem],
    PseudoHigh::[LMem..HMem],
    PseudoSize::[0..X],
    T = t(buselement(bridge, addr(-1, -1, -1), childbus(-1, -1), PseudoBase, PseudoHigh, PseudoSize, _, _, _, _), [TP, TNP]),
    setrange(T,_,_,_),
    nonoverlap(T),
    naturally_aligned(T, 256, LMem, HMem),
    tree2list(T,ListaU),
    sort(6, >=, ListaU, Lista),
    not_overlap_memory_ranges(Lista, ExclRanges),
    keep_orig_addr(Lista, 12, 3, _, _, _, _),
    keep_ioapic_bars(Lista, IOAPICs),
    labelall(Lista),
    subtract(Lista,[buselement(bridge,Addr,_,_,_,_,_,prefetchable,_,_)],Pl3),
    subtract(Pl3,[buselement(bridge,Addr,_,_,_,_,_,nonprefetchable,_,_)],Pl2),
    subtract(Pl2,[buselement(bridge,addr(-1,-1,-1),_,_,_,_,_,_,_,_)],Pl),
    maplist(adjust_range(0),Pl,PR),
    maplist(back_to_bytes(Granularity),PR,Plan).

% dot output:
%    PrBaseBytePref is RBaseP * Granularity,
%    PrHighBytePref is RHighP * Granularity,
%    PrBaseByteNonPref is RBaseNP * Granularity,
%    PrHighByteNonPref is RHighNP * Granularity,
%    plan_to_dot(Granularity, Plan, Root, PrBaseBytePref, PrHighBytePref, PrBaseByteNonPref, PrHighByteNonPref).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% instantiating the variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

base(buselement(_,_,_,Base,_,_,_,_,_,_),Base).
high(buselement(_,_,_,_,High,_,_,_,_,_),High).
size(buselement(_,_,_,_,_,Size,_,_,_,_),Size).

labelall(BusElementList) :-
    maplist(base, BusElementList, Base),
    maplist(high, BusElementList, High),
    maplist(size, BusElementList, Size),
    append(Base, High, L1),
    append(L1, Size, L2),
    labeling(L2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create the list of devices and bridges in form of buselements and create the
% variables.
% we care about the allocation of memory mapped registers here, therefore we only
% look at bar located in "mem", not "io"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constrain_bus(Granularity, Type, Prefetch, RootAddr,Bus,MaxBus,LMem,HMem,OutBusElementList) :-
    constrain_bus_ex(Granularity, Type, Prefetch, RootAddr,Bus,MaxBus,LMem,HMem,[],OutBusElementList).

constrain_bus_ex(_, _, _, _,Bus,MaxBus,_,_,InL,InL) :- Bus > MaxBus.
constrain_bus_ex(Granularity, Type, Prefetch, RootAddr,Bus,MaxBus,LMem,HMem,InBusElementList,OutBusElementList) :-
    Bus =< MaxBus,
    SMax is HMem - LMem,
    ( is_predicate(bridge/8) ->
	    findall(buselement(bridge,addr(Bus,Dev,Fun),secondary(Sec),Base,High,Size,Type,Prefetch, PCIe, 0),
	            ( bridge(PCIe, addr(Bus,Dev,Fun), _, _, _, _, _, secondary(Sec)),
	              not addr(Bus,Dev,Fun) = RootAddr,
	              Base::[LMem..HMem],High::[LMem..HMem],Size::[0..SMax]
	            ),BridgeList);
        BridgeList = []
    ),
    ( is_predicate(device/8) ->
	    findall(buselement(device,addr(Bus,Dev,Fun),BAR,Base,High,SizeP,Type,Prefetch, PCIe, Bits),
	            ( device(PCIe, addr(Bus,Dev,Fun),_,_,_,_,_,_),
	              bar(addr(Bus,Dev,Fun),BAR,_,Size, Type, Prefetch, Bits),
	              Base::[LMem..HMem],High::[LMem..HMem],
	              ST1 is Size / Granularity,
	              ceiling(ST1, ST2),
	              integer(ST2, SizeP)
	            ),DeviceList);
        DeviceList = []
    ),
    append(BridgeList, DeviceList, MyBusElementList),
    append(InBusElementList, MyBusElementList, NewBusElementList),
    NextBus is Bus + 1,
    constrain_bus_ex(Granularity, Type, Prefetch, RootAddr, NextBus, MaxBus, LMem,HMem,NewBusElementList,OutBusElementList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create the PCI(e) device tree from a list of "buselement" and return it in Tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

devicetree(List,CurrRoot,Tree) :-
    buselement(bridge,_,secondary(Sec),_,_,_,_,_,_,_) = CurrRoot,
    findall(X,(
               member(Y,List),
               buselement(_,addr(Sec,_,_),_,_,_,_,_,_,_,_) = Y,
               devicetree(List, Y, X)),Children
           ),
    Tree = t(CurrRoot,Children).
devicetree(_,CurrRoot,Tree) :-
    buselement(device,_,_,_,_,_,_,_,_,_) = CurrRoot,
    Tree = t(CurrRoot, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% convert a tree to a list of buselements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tree2list([],[]).
tree2list(Tree, List) :-
    t(Node,Children) = Tree,
    ( foreach(El,Children),
      foreach(L1,ChildList)
      do
        tree2list(El,L1)
    ),
    flatten(ChildList,L2),
    List = [Node|L2].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% store the new values of the BARs
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
replace_current_BAR_values(L) :-
    delete_current_BAR_values(L),
    store_current_BAR_values(L).

store_current_BAR_values([]).
store_current_BAR_values([H|T]) :-
    ( buselement(device,Addr,BAR,Base,High,Size,_,_,_,_) = H ->
         assert(currentbar(Addr,BAR,Base,High,Size));
        true
    ),
    store_current_BAR_values(T).


delete_current_BAR_values([]).
delete_current_BAR_values([H|T]) :-
    ( buselement(device,Addr,BAR,_,_,_,_,_,_,_) = H ->
        ( currentbar(Addr,BAR,_,_,_) ->
            retract(currentbar(Addr,BAR,_,_,_));
            true
        );
        true
    ),
    delete_current_BAR_values(T).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% add constraints to the tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% make sure that the bridge has a range which includes all the children
setrange(Tree,SubTreeSize,SubTreeMin,SubTreeMax) :-
    t(Node,Children) = Tree,
    ( foreach(El,Children),
      foreach(Sz,SizeList),
      foreach(Mi,MinList),
      foreach(Ma,MaxList)
      do
        setrange(El,Sz,Mi,Ma)
    ),
    ic_global:sumlist(SizeList,Size),
    buselement(_,_,_,Base,High,ElemSize,_,_,_,_) = Node,
    ElemSize $>= Size,
    ( not MinList=[] ->
        ic:minlist(MinList,Min),
        ic:maxlist(MaxList,Max),
        Min $>= Base,
        Max $=< High;
        true
    ),
    High $= Base + ElemSize,
    SubTreeSize $= ElemSize,
    SubTreeMin $= Base,
    SubTreeMax $= High.
setrange([],0,_,_).


% make sure that the children do not overlap
child(t(C,_),C).
nonoverlap(Tree) :-
    t(_ ,Children) = Tree,
    maplist(child,Children,ChildList),
    ( not ChildList=[] ->
        maplist(base,ChildList,Base),
        maplist(size,ChildList,Size),
        disjunctive(Base,Size);
        true
    ),
    ( foreach(El, Children)
      do
        nonoverlap(El)
    ).


naturally_aligned(Tree, BridgeAlignment, LMem, HMem) :-
    t(Node,Children) = Tree,
    ( buselement(device,_,_,Base,High,Size,_,_,_,_) = Node ->
      Divisor is Size
      ;
      buselement(bridge,_,_,Base,High,_,_,_,_,_) = Node ->
      Divisor is BridgeAlignment
    ),

    T1 is (HMem - LMem) / Divisor,
    ceiling(T1, T2),
    integer(T2, Nr),
    N::[0..Nr],
    N2::[0..Nr],
    mod(LMem,Divisor,Remainder),
    ( Remainder =:= 0 ->
        Corr is 0;
        Corr is Divisor - Remainder
    ),
    Base $= N*Divisor + LMem + Corr,
    High $>= Base,
    High $= N2*Divisor + LMem + Corr,
    ( foreach(El, Children),
      param(BridgeAlignment),
      param(LMem),
      param(HMem)
      do
        naturally_aligned(El, BridgeAlignment, LMem, HMem)
    ).


% do not overlap with the given list of memory ranges
not_overlap_memory_ranges([], _).
not_overlap_memory_ranges(_, []).
not_overlap_memory_ranges([buselement(bridge,_,_,_,_,_,_,_,_,_)|PCIList], MemoryRanges) :-
    not_overlap_memory_ranges(PCIList, MemoryRanges).
not_overlap_memory_ranges([H|PCIList], MemoryRanges) :-
    ( foreach(range(RBase,RSize),MemoryRanges),
      param(H)
      do
      buselement(device,_,_,Base,_,Size,_,_,_,_) = H,
      append([Base],[RBase],Bases),
      append([Size],[RSize],Sizes),
      disjunctive(Bases,Sizes)
    ),
    not_overlap_memory_ranges(PCIList, MemoryRanges).


keep_orig_addr([], _, _, _, _, _, _).
keep_orig_addr([H|Buselements], Class, SubClass, ProgIf, Bus, Dev, Fun) :-
    ( buselement(device,addr(Bus,Dev,Fun),BAR,Base,_,_,_,_,_,_) = H,device(_,addr(Bus,Dev,Fun),_,_,Class, SubClass, ProgIf,_),bar(addr(Bus,Dev,Fun),BAR,OrigBase,_,_,_,_) ->
       T1 is OrigBase / 4096,
       floor(T1,T2),
       integer(T2,KeepBase),
        Base $= KeepBase;
        true
    ),
    keep_orig_addr(Buselements, Class, SubClass, ProgIf, Bus, Dev, Fun).

% on some machines (sbrinz1) one of the two IOAPICs appears as a BAR
% on a device which claims to be a RAM memory controller. If this occurs,
% we want to avoid moving this BAR as otherwise the IOAPIC cannot be reached
% anymore.
keep_ioapic_bars(_, []).
keep_ioapic_bars(Buselements, [H|IOAPICList]) :-
    (
    range(B, _) = H,
    bar(addr(Bus,Dev,Fun),_,OrigBase,_,_,_,_),
    T1 is OrigBase / 4096,
    floor(T1,T2),
    integer(T2,KeepBase),
    KeepBase =:= B ->
    keep_orig_addr(Buselements, _, _, _, Bus, Dev, Fun);
    true
    ),
    keep_ioapic_bars(Buselements, IOAPICList).
