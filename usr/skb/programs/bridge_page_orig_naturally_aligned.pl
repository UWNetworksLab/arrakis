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
    Granularity is 4096,
% find all the root bridges
    findall(root(Addr,Child,mem(LP,HP)),
            (  rootbridge(Addr,Child,mem(L,H)),
               LT1 is L / Granularity,
               ceiling(LT1, LT2),
               integer(LT2, LP),
               HT1 is H / Granularity,
               ceiling(HT1, HT2),
               integer(HT2, HP)
            ),Roots),
% exclude fixed memory from being allocated to devices
    ( is_predicate(fixed_memory/2) ->
        findall(range(ResLowP,ResSizeP),
          (
            fixed_memory(ResLow,ResHigh), T1 is ResLow / Granularity, floor(T1,T2),
            integer(T2,ResLowP),
            T3 is (ResHigh - ResLow) / Granularity,
            ceiling(T3,T4),
            integer(T4,ResSizeP)
          ), ExclRangesFixed);
        ExclRangesFixed = []
    ),
% exclude IOAPIC regions from being allocated to devices
    ( is_predicate(ioapic/3) ->
      % according to the spec we need 64Bytes in the Intel case. Reserve a page
      % anyway, since currently we cannot query the real requested size
      TSz is (4096 / Granularity),
      ceiling(TSz, TSz2),
      integer(TSz2, IOAPIC_MinSize),
      findall(range(Bs,IOAPIC_MinSize),
               (
                ioapic(_,B,_),
                T1 is B / Granularity,
                floor(T1, T2),
                integer(T2, Bs)
               ),IOAPICs)
      ;
      IOAPICs = []
    ),

%all the regions to avoid
    append(ExclRangesFixed, IOAPICs, ExclRanges),
      
% create an assignment for all PCI buses (all root bridges and their children)
    ( foreach(Root,Roots),
      foreach(P,Plan),
      foreach(L,Lengths),
      param(Granularity),
      param(ExclRanges)
      do
        bridge_assignment(P,Root, Granularity, ExclRanges),
        length(P,L)
    ),
    sum(Lengths,NrElements).


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

bridge_assignment(Plan, Root, Granularity, ExclRanges) :-
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
    keep_orig_addr(Lista, 12, 3, _),
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

constrain_bus(_, _, _, _,Bus,MaxBus,_,_,[]) :- Bus > MaxBus.
constrain_bus(Granularity, Type, Prefetch, RootAddr,Bus,MaxBus,LMem,HMem,NewBusElementList) :-
    Bus =< MaxBus,
    SMax is HMem - LMem,
    findall(buselement(bridge,addr(Bus,Dev,Fun),secondary(Sec),Base,High,Size,Type,Prefetch, PCIe, 0),
            ( bridge(PCIe, addr(Bus,Dev,Fun), _, _, _, _, _, secondary(Sec)),
              not addr(Bus,Dev,Fun) = RootAddr,
              Base::[LMem..HMem],High::[LMem..HMem],Size::[0..SMax]
            ),BridgeList),
    findall(buselement(device,addr(Bus,Dev,Fun),BAR,Base,High,SizeP,Type,Prefetch, PCIe, Bits),
            ( device(PCIe, addr(Bus,Dev,Fun),_,_,_,_,_,_),
              bar(addr(Bus,Dev,Fun),BAR,_,Size, Type, Prefetch, Bits),
              Base::[LMem..HMem],High::[LMem..HMem],
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

    suspend(mod(Base, Divisor, 0), 3, Base->inst),
    ( foreach(El, Children),
      param(BridgeAlignment),
      param(LMem),
      param(HMem)
      do
        naturally_aligned(El, BridgeAlignment, LMem, HMem)
    ).


% do not overlap with the given list of memory ranges
not_overlap_memory_ranges([], _).
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


keep_orig_addr([], _, _, _).
keep_orig_addr([H|Buselements], Class, SubClass, ProgIf) :-
    ( buselement(device,Addr,BAR,Base,_,_,_,_,_,_) = H,device(_,Addr,_,_,Class, SubClass, ProgIf,_),bar(Addr,BAR,OrigBase,_,_,_,_) ->
       T1 is OrigBase / 4096,
       floor(T1,T2),
       integer(T2,KeepBase),
        Base $= KeepBase;
        true
    ),
    keep_orig_addr(Buselements, Class, SubClass, ProgIf).

