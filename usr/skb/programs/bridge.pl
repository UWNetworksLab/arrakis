%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, 2011, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-lib(ic).
:-use_module(library(ic_edge_finder)).

:-dynamic(bridge/4).
:-dynamic(bar/5).

% :-include("../data/data_nos6.txt").
% :-include("../data/data_qemu_hand.txt").
% :-include("../data/data_qemu.txt").
% :-include("../data/data_nos4.txt").
% :-include("../data/data_nos5.txt").
% :-include("../data/data_hand.txt").

% asq: important: this entry _has_ to be here all the time!!
bridge(addr(0,0,0),0,0,secondary(0)).
bar(addr(0,0,0),0,0,5,mem).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% main goal to be called from outside
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bridge_programming(Plan) :-
%    findall(memory_region(Base,Bits,Size,Type,Data),(
%                   memory_region(Base,Bits,Size,Type,Data),
%                   mem_region_type(Type,phyaddr)),
%            PhysList),
%    sort(2, >=, PhysList, PhysListSorted),
%    writeln(PhysListSorted),
    findall(root(Addr,Child,Mem),rootbridge(Addr,Child,Mem),Roots),
    ( foreach(Root,Roots),
      foreach(P,Plan)
      do
        bridge_assignment(P,Root)
    ).

adjust_range(X, buselement(T,A,Sec,B1,H1,S), buselement(T,A,Sec,B2,H2,S)) :-
    B2 is B1 + X,
    H2 is H1 + X.

bridge_assignment(Plan, Root) :-
    root(Addr,childbus(MinBus,MaxBus),mem(LMem,HMem)) = Root,
    X is HMem - LMem,
    constrain_bus(Addr,MinBus,MaxBus,0,X,BusElementList),
    RBase::[0..X],
    RHigh::[0..X],
    RSize::[0..X],
    devicetree(BusElementList,buselement(bridge,Addr,secondary(MinBus),RBase,RHigh,RSize),T),
    writeln(tree),
    writeln(T),
    setrange(T,_,_,_),
    nonoverlap(T),
%    naturally_aligned(T,LMem),
%    naturally_aligned_range(T, LMem, 0, X),
    naturally_aligned_mul(T, LMem, LMem, HMem),
    bridge_min_size(T, 100),
    tree2list(T,Lista),
%    not_overlap_ioapic(Lista, LMem),
%    keep_orig_address(Lista,[addr(0,9,0)],LMem),
    labelall(Lista),
    subtract(Lista,[buselement(bridge,Addr,_,_,_,_)],Pl),
    maplist(adjust_range(LMem),Pl,Plan).
   


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% instantiating the variables
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

base(buselement(_,_,_,Base,_,_),Base).
high(buselement(_,_,_,_,High,_),High).
size(buselement(_,_,_,_,_,Size),Size).

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

constrain_bus(_,Bus,MaxBus,_,_,[]) :- Bus > MaxBus.
constrain_bus(RootAddr,Bus,MaxBus,LMem,HMem,NewBusElementList) :-
    Bus =< MaxBus,
    findall(buselement(bridge,addr(Bus,Dev,Fun),secondary(Sec),Base,High,Size),
            ( bridge(addr(Bus,Dev,Fun), _, _, secondary(Sec)),
              not addr(Bus,Dev,Fun) = RootAddr,
              SMax is HMem - LMem,
              Base::[LMem..HMem],High::[LMem..HMem],Size::[0..SMax]
            ),BridgeList),
    findall(buselement(device,addr(Bus,Dev,Fun),BAR,Base,High,Size),
            ( device(_, addr(Bus,Dev,Fun),_,_,_,_,_,_),
              bar(addr(Bus,Dev,Fun),BAR,_,Size, mem),
              Base::[LMem..HMem],High::[LMem..HMem]
            ),DeviceList),
    append(BridgeList, DeviceList, BusElementList),
    NextBus is Bus + 1,
    constrain_bus(RootAddr, NextBus, MaxBus, LMem,HMem,List),
    append(List,BusElementList,NewBusElementList).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create the PCI(e) device tree from a list of "buselement" and return it in Tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

devicetree(List,CurrRoot,Tree) :-
    buselement(bridge,_,secondary(Sec),_,_,_) = CurrRoot,
    findall(X,(
               member(Y,List),
               buselement(_,addr(Sec,_,_),_,_,_,_) = Y,
               devicetree(List, Y, X)),Children
           ),
    Tree = t(CurrRoot,Children).
devicetree(_,CurrRoot,Tree) :-
    buselement(device,_,_,_,_,_) = CurrRoot,
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
% add constraints to the tree
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% make sure that the bridge has a range which includes all the children
setrange(Tree,SubTreeSize,SubTreeMin,SubTreeMax) :-
    t(Node,Children) = Tree,
    writeln(Node),
    ( foreach(El,Children),
      foreach(Sz,SizeList),
      foreach(Mi,MinList),
      foreach(Ma,MaxList)
      do
        setrange(El,Sz,Mi,Ma)
    ),
    writeln(a),
    ic_global:sumlist(SizeList,Size),
    writeln(b),
    buselement(_,_,_,Base,High,ElemSize) = Node,
    write(elemsize),write(ElemSize),write(size),write(Size),
    writeln(c),
    ElemSize $>= Size,
    writeln(d),
    ( not MinList=[] ->
        writeln(e),
        ic:minlist(MinList,Min),
        writeln(f),
        ic:maxlist(MaxList,Max),
        writeln(g),
        Min $>= Base,
        writeln(h),
        Max $=< High;
        writeln(i),
        true
    ),
    writeln(j),
    High $= Base + ElemSize,
    writeln(k),
    SubTreeSize $= ElemSize,
    writeln(l),
    SubTreeMin $= Base,
    writeln(m),
    SubTreeMax $= High,
    writeln(n).
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


% make sure that device bases are naturally aligned
naturally_aligned(Tree,Shift) :-
    t(Node,Children) = Tree,
    ( buselement(device,_,_,Base,_,Size) = Node ->
      B1 $= Base + Shift,
      suspend(mod(B1,Size,0),3,B1->inst)
      ;
      ( foreach(El, Children),
        param(Shift)
        do
          naturally_aligned(El,Shift)
      )
    ).


naturally_aligned_range(Tree, Shift, Min, Max) :-
    t(Node, Children) = Tree,
    ( buselement(device,_,_,Base,_,Size) = Node ->
      ( for(I,Min,Max,Size),
        foreach(B1,Bases),
        param(Shift)
        do
          B1 is I - Shift
      ),
      Base::Bases
      ;
      ( foreach(El, Children),
        param(Shift),
        param(Min),
        param(Max)
        do
          naturally_aligned_range(El, Shift, Min, Max)
      )
   ).


% N::[0..262000],Size=4096,Shift=1000000000,Base::[0..40000000],Base $= N*Size - Shift,labeling([Base]).
% make sure that device bases are naturally aligned
naturally_aligned_mul(Tree,Shift, Low, High) :-
    t(Node,Children) = Tree,
    ( buselement(device,_,_,Base,_,Size) = Node ->
      T1 is (High - Low) / Size,
      ceiling(T1, T2),
      integer(T2, Nr),
      N::[0..Nr],
      mod(Low,Size,Remainder),
      Corr is Size - Remainder,
      Base $= N*Size + Low + Corr - Shift
      ;
      ( foreach(El, Children),
        param(Shift),
        param(Low),
        param(High)
        do
          naturally_aligned_mul(El,Shift, Low, High)
      )
    ).



% a bridge decodes at least a certain amount of memory (due to the granularity
% in the decoder register
bridge_min_size(Tree, MinAmount) :-
    t(Node,Children) = Tree,
    ( buselement(bridge,_,_,_,_,Size) = Node ->
      Size $>= MinAmount,
      ( foreach(El, Children),
        param(MinAmount)
        do
          bridge_min_size(El, MinAmount)
      )
      ;
      true
    ).

% do not overlap with IOAPIC addresses
not_overlap_ioapic(List, LMem) :-
    findall(Bs,(ioapic(_,B,_), Bs is B - LMem),IOAPIC_Bases),
    ( foreach(_,IOAPIC_Bases),
      foreach(S,IOAPIC_Sizes)
        do
          S $= 100
    ),
    ( foreach(El,List),
      param(IOAPIC_Bases),
      param(IOAPIC_Sizes)
      do
      buselement(_,_,_,Base,_,Size) = El,
      append([Base],IOAPIC_Bases,Bases),
      append([Size],IOAPIC_Sizes,Sizes),
      disjunctive(Bases,Sizes)
    ).

% some devices we do not want to move, like the VGA device, because otherwise we
% loose our console temporarly
keep_orig_addr(Device,DeviceAddresses,Offset) :-
    ( foreach(Addr, DeviceAddresses),
      param(Device),
      param(Offset)
      do
        ( (bar(Addr,_,Base,_,_),
           buselement(_,Addr,_,B,_,_) = Device) ->
           B $= (Base - Offset)
           ;
           true
        )
    ).
keep_orig_address(List,DeviceAddresses,Offset) :-
    ( foreach(D,List),
      param(DeviceAddresses),
      param(Offset)
      do
        keep_orig_addr(D,DeviceAddresses,Offset)
    ).

