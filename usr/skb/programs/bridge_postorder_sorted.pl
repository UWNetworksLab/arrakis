%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, 2011, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% the PCI tree is constructed by adding child devices to the bridge. all the
% children are sorted by the requesting size of the children in descending order.
% on the next level children (which include subordinate bridges) are sorted by
% the biggest requesting child (not the sum of the children under a bridge).
% the bridges Sz variable contains therefore the size of the biggest requesting
% child, not the size under the bridge. like this, we can allocate resources
% for the biggest requesting child first.


%:-dynamic(bridge/8).
%:-dynamic(bar/7).

% :-include("../data/data_nos6.txt").
% :-include("../data/data_qemu_hand.txt").
% :-include("../data/data_qemu.txt").
% :-include("../data/data_nos4.txt").
% :-include("../data/data_nos5.txt").
% :-include("../data/data_hand.txt").

% asq: important: this entry _has_ to be here all the time!!
% bridge(pcie, addr(0,0,0),0,0,6,4,0,secondary(0)).
bar(addr(0,0,0),0,0,5,mem, nonprefetchable,0).
bar(addr(0,0,0),0,0,5,mem, prefetchable,0).

:- set_flag(print_depth, 200).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main goal to be called from outside
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bridge_programming(Plan, NrElements) :-

    Granularity is 4096,
    FixedAddresses=[fixed(12,3,_)],
    reserve_fixed_addresses(FixedAddresses),

    findall(root(Addr,Child,mem(LP,HP)),
            (  rootbridge(Addr,Child,mem(L,H)),
               LT1 is L / Granularity,
               ceiling(LT1, LT2),
               integer(LT2, LP),
               HT1 is H / Granularity,
               ceiling(HT1, HT2),
               integer(HT2, HP)
            ),Roots),
    ( is_predicate(fixed_memory/2) ->
        findall(range(ResLowP,ResSizeP),
          (
            fixed_memory(ResLow,ResHigh), T1 is ResLow / Granularity, floor(T1,T2),
            integer(T2,ResLowP),
            T3 is (ResHigh - ResLow) / Granularity,
            ceiling(T3,T4),
            integer(T4,ResSizeP)
          ), ExclRanges);
        ExclRanges = []
    ),
    ( foreach(Root,Roots),
      foreach(P,Plan),
      foreach(L,Lengths),
      param(Granularity),
      param(ExclRanges),
      param(FixedAddresses)
      do
        bridge_assignment(P,Root, Granularity, ExclRanges, FixedAddresses),
        length(P,L)
    ),
    sum(Lengths,NrElements).






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% construct a tree and do the assignment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bridge_assignment(Plan, Root, Granularity, ExclRanges, FixedAddresses) :-
    root(Addr,childbus(MinBus,MaxBus),mem(LMem,HMem)) = Root,
    X is HMem - LMem,
    Type = mem,
% prefetchable and nonprefetchable
    constrain_bus(Granularity, Type, _, Addr,MinBus,MaxBus,LMem,HMem,BusElementListP),
    devicetree(BusElementListP,buselement(bridge,Addr,secondary(MinBus),RBaseP,RHighP,RSizeP, Type, _, _, _),T),

% prefetchable
%    constrain_bus(Granularity, Type, prefetchable, Addr,MinBus,MaxBus,LMem,HMem,BusElementListP),
%    devicetree(BusElementListP,buselement(bridge,Addr,secondary(MinBus),RBaseP,RHighP,RSizeP, Type, prefetchable, _, _),TP),
    
%% nonprefetchable
%    constrain_bus(Granularity, Type, nonprefetchable, Addr,MinBus,MaxBus,LMem,HMem,BusElementListNP),
%    devicetree(BusElementListNP,buselement(bridge,Addr,secondary(MinBus),RBaseNP,RHighNP,RSizeNP, Type, nonprefetchable, _, _),TNP),
    
%% pseudo-root of both trees
%% sorted
%    T = t(buselement(bridge, addr(-1, -1, -1), childbus(-1, -1), PseudoBase, PseudoHigh, PseudoSize, _, _, _, _), Sz, [TP, TNP]),
%
%% unsorted
%%    T = t(buselement(bridge, addr(-1, -1, -1), childbus(-1, -1), PseudoBase, PseudoHigh, PseudoSize, _, _, _, _), [TP, TNP]),

    nl,nl,nl,writeln(tree),nl,writeln(T),nl,nl,nl,
    pci_postorder(T, LMem, High, Granularity, FixedAddresses),
    
% XXX
%    High =< HMem,

    tree2list(T,Lista),
    
    subtract(Lista,[buselement(bridge,Addr,_,_,_,_,_,_,_,_)],Pl),
    compute_bridge_size(Pl),
    maplist(adjust_range(0),Pl,PR),
    maplist(back_to_bytes(Granularity),PR,Plan).

%    subtract(Lista,[buselement(bridge,Addr,_,_,_,_,_,prefetchable,_,_)],Pl3),
%    subtract(Pl3,[buselement(bridge,Addr,_,_,_,_,_,nonprefetchable,_,_)],Pl2),
%    subtract(Pl2,[buselement(bridge,addr(-1,-1,-1),_,_,_,_,_,_,_,_)],Pl),
%    maplist(adjust_range(0),Pl,PR),
%    maplist(back_to_bytes(Granularity),PR,Plan).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create the list of devices and bridges in form of buselements and create the
% variables.
% we care about the allocation of memory mapped registers here, therefore we only
% look at bar located in "mem", not "io"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

constrain_bus(_, _, _, _,Bus,MaxBus,_,_,[]) :- Bus > MaxBus.
constrain_bus(Granularity, Type, Prefetch, RootAddr,Bus,MaxBus,LMem,HMem,NewBusElementList) :-
    Bus =< MaxBus,
    SMax is HMem - LMem,
    findall(buselement(bridge,addr(Bus,Dev,Fun),secondary(Sec),Base,High,Size,Type,Prefetch, PCIe, 0),
            ( bridge(PCIe, addr(Bus,Dev,Fun), _, _, _, _, _, secondary(Sec)),
              not addr(Bus,Dev,Fun) = RootAddr
            ),BridgeList),
    findall(buselement(device,addr(Bus,Dev,Fun),BAR,Base,High,SizeP,Type,Prefetch, PCIe, Bits),
            ( device(PCIe, addr(Bus,Dev,Fun),_,_,_,_,_,_),
              bar(addr(Bus,Dev,Fun),BAR,_,Size, Type, Prefetch, Bits),
              ST1 is Size / Granularity,
              ceiling(ST1, ST2),
              integer(ST2, SizeP)
            ),DeviceList),
    append(BridgeList, DeviceList, BusElementList),
    NextBus is Bus + 1,
    constrain_bus(Granularity, Type, Prefetch, RootAddr, NextBus, MaxBus, LMem,HMem,List),
    append(List,BusElementList,NewBusElementList).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create the PCI(e) device tree from a list of "buselement" and return it in Tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sorted

devicetree(List,CurrRoot,Tree) :-
    buselement(bridge,_,secondary(Sec),_,_,_,_,_,_,_) = CurrRoot,
    findall(X,(
               member(Y,List),
               buselement(_,addr(Sec,_,_),_,_,_,_,_,_,_,_) = Y,
               devicetree(List, Y, X)),Children
           ),
    ( not Children=[] ->
          sort(2, >=, Children, [H|ChildrenSorted]),
          t(_,Sz,_) = H,
          Tree = t(CurrRoot,Sz,[H|ChildrenSorted]);
          Tree = t(CurrRoot,0,[])
    ).
devicetree(_,CurrRoot,Tree) :-
    buselement(device,_,_,_,_,Size,_,_,_,_) = CurrRoot,
    Tree = t(CurrRoot, Size, []).



% unsorted
%
%devicetree(List,CurrRoot,Tree) :-
%    buselement(bridge,_,secondary(Sec),_,_,_,_,_,_,_) = CurrRoot,
%    findall(X,(
%               member(Y,List),
%               buselement(_,addr(Sec,_,_),_,_,_,_,_,_,_,_) = Y,
%               devicetree(List, Y, X)),Children
%           ),
%    Tree = t(CurrRoot,Children).
%devicetree(_,CurrRoot,Tree) :-
%    buselement(device,_,_,_,_,_,_,_,_,_) = CurrRoot,
%    Tree = t(CurrRoot, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% convert a tree to a list of buselements
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sorted
tree2list([],[]).
tree2list(Tree, List) :-
    t(Node,_,Children) = Tree,
    ( foreach(El,Children),
      foreach(L1,ChildList)
      do
        tree2list(El,L1)
    ),
    flatten(ChildList,L2),
    List = [Node|L2].

% unsorted
%tree2list([],[]).
%tree2list(Tree, List) :-
%    t(Node,Children) = Tree,
%    ( foreach(El,Children),
%      foreach(L1,ChildList)
%      do
%        tree2list(El,L1)
%    ),
%    flatten(ChildList,L2),
%    List = [Node|L2].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Traverse tree in postoder mode and assign addresses to the devices
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pci_postorder([], StartAddr, StartAddr, _, _) :- writeln([]).
pci_postorder(T, StartAddr, NextAddr, Granularity, FixedAddresses) :-
    t(Node, _, Children) = T,
    buselement(Type,Addr,BAR,Base,High,Size,_,_,_,_) = Node,
    MBF is ((1024 * 1024) / Granularity),
    integer(MBF, MB),

% adjust the start address in case it is a device to avoid resource conflicts
    adjust_start_address(Type, StartAddr, Size, Granularity, AllocationStartAddr),

% now do the allocation
    ( Type = device ->
      mod(AllocationStartAddr, Size, Remainder),
      ( Remainder > 0 ->
          Base is AllocationStartAddr + Size - Remainder;
          Base is AllocationStartAddr
      );
      mod(AllocationStartAddr, MB, Remainder2),
      ( Remainder2 > 0 ->
          Base is AllocationStartAddr + MB - Remainder2;
          Base is AllocationStartAddr
      )
   ),

    pci_postorder_children(Children, Base, NextChildAddr, Granularity, FixedAddresses),

    ( Type = device ->
        NextAddr is NextChildAddr + Size;
        mod(NextChildAddr, MB, Remainder3),
        ( Remainder3 > 0 ->
            NextAddr is NextChildAddr + MB - Remainder3;
            NextAddr is NextChildAddr
        )
    ),

    High = NextAddr,
    writeln(Node),
    writeln(NextChildAddr),
    writeln(NextAddr).


pci_postorder_children([], StartAddr, StartAddr, _, _).
pci_postorder_children([H|T], StartAddr, NextAddr, Granularity, FixedAddresses) :-
    pci_postorder(H, StartAddr, Next, Granularity, FixedAddresses),
    pci_postorder_children(T, Next, NextAddr, Granularity, FixedAddresses).



reserve_fixed_addresses([]).
reserve_fixed_addresses([fixed(Class,SubClass,ProgIf)|T]) :-
    findall(m(B,H), (
                      device(_,Addr,_,_,Class, SubClass, ProgIf,_),bar(Addr,BAR,B,H,_,_,_)
                    ),
            FixedList),
    ( foreach(m(B,H),FixedList)
      do
        assert(fixed_memory(B,H))
    ),
    reserve_fixed_addresses(T).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% adjust startaddress: leave reserved regions and fixed addresses of devices out
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
adjust_start_address(bridge, StartAddr, _, _, StartAddr).

adjust_start_address(device, StartAddr, Size, Granularity, AllocStartAddr) :-
    EndAddr is StartAddr + Size,
    ( is_predicate(fixed_memory/2) ->
        findall(r(B,H), (
            fixed_memory(ResLow,ResHigh),
            T1 is ResLow / Granularity,
            floor(T1,T2),
            integer(T2,B),
            T3 is ResHigh / Granularity,
            ceiling(T3, T4),
            integer(T4, H)
                        ),
                        ReservedList)
    ;
    ReservedList=[]
    ),

    IOAPIC_SizeT1 is (4096 / Granularity),
    ceiling(IOAPIC_SizeT1, IOAPIC_SizeT2),
    integer(IOAPIC_SizeT2, IOAPIC_Size),
    findall(r(IOB,IOH),(
                ioapic(_,B,_),
                T1 is B / Granularity,
                floor(T1, T2),
                integer(T2, IOB),
                IOH is IOB + IOAPIC_Size
               ),IOAPIC_reserved),
    append(ReservedList, IOAPIC_reserved, ResList),
    ( foreach(r(B,H), ResList),
      foreach(A, ConflictList),
      param(StartAddr),
      param(EndAddr)
      do
          ( StartAddr >= B, StartAddr =< H ->
                A = H;
            EndAddr >= B, EndAddr =< H ->
                A = H;
            StartAddr =< B, EndAddr >= H ->
                A = H;
                A = 0
          )
    ),
    max(ConflictList,Max),
    max([Max,StartAddr], AllocStartAddrAdjusted),

    mod(AllocStartAddrAdjusted, Size, Remainder),
    ( Remainder > 0 ->
        AllocStartAddr is AllocStartAddrAdjusted + Size - Remainder;
        AllocStartAddr is AllocStartAddrAdjusted
    ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tools
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

adjust_range(X, buselement(T,A,Sec,B1,H1,S,Tp,PF, PCIe, Bits), buselement(T,A,Sec,B2,H2,S,Tp,PF, PCIe, Bits)) :-
    B2 is B1 + X,
    H2 is H1 + X.

back_to_bytes(Granularity, buselement(T,A,Sec,BP,HP,SP,Tp,PF, PCIe, Bits), buselement(T,A,Sec,B,H,S,Tp,PF, PCIe, Bits)) :-
    B is BP * Granularity,
    H is HP * Granularity,
    S is SP * Granularity.

base(buselement(_,_,_,Base,_,_,_,_,_,_),Base).
high(buselement(_,_,_,_,High,_,_,_,_,_),High).
size(buselement(_,_,_,_,_,Size,_,_,_,_),Size).


compute_bridge_size([]).
compute_bridge_size([buselement(device,_,_,_,_,_,_,_,_,_)|T]) :-
    compute_bridge_size(T).
compute_bridge_size([buselement(bridge,_,_,Base,High,Size,_,_,_,_)|T]) :-
    Size is High - Base,
    compute_bridge_size(T).


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



