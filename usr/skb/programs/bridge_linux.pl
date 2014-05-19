%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, 2011, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bridge_programming(Plan, NrElements) :-
    get_devices(Devices),
    convert_devices(Devices,DeviceElements),
    get_bridges(Bridges),
    convert_bridges(Bridges,BridgeElements),
    append(DeviceElements, BridgeElements, Plan),
    length(Plan, NrElements).

% note: device with addr(-1,-1,-1) will be removed because it has no
%       regions -> nuet. The clean way would be to really remove it which leads
%       to other prolog problems... (two solutions).
convert_devices([], []).
convert_devices([buselement(device, Addr ,Regions)|T], L) :-
    subtract(Regions,[nuet],RegionsClean),
    ( foreach(R,RegionsClean),
      foreach(El,Elements),
      param(Addr)
      do
          region(BAR,Base,Bits,Prefetch,Sz,MulL) = R,
          ( MulL = b ->
              Mul is 1
          ;
            MulL = k ->
              Mul is 1024
          ;
              Mul is 1024 * 1024
          ),
          Size is Sz * Mul,
          High is Base + Size,
          El = buselement(device, Addr, BAR, Base, High, Size, mem, Prefetch, pcie, Bits),
          assert(bar(Addr,BAR,_,Size,_,_,_))
    ),
    convert_devices(T, L2),
    append(L2, Elements, L).

convert_bridges([], []).
convert_bridges([buselement(bridge, _, _)|T], L) :-
    convert_bridges(T, L).
convert_bridges([buselement(bridge, Addr, S, m(B1,H1), p(B2, H2),_)|T], L) :-
    ( H1 >= B1 ->
        S1 is H1 - B1,
        Bridge1 = [buselement(bridge, Addr, S, B1, H1, S1, mem, nonprefetchable, pcie, 0)];
        Bridge1 = []
    ),
    ( H2 >= B2 ->
        S2 is H2 - B2,
        Bridge2 = [buselement(bridge, Addr, S, B2, H2, S2, mem, prefetchable, pcie, 0)];
        Bridge2 = []
    ),
    append(Bridge1,Bridge2,BridgeList),
    convert_bridges(T, L2),
    append(L2, BridgeList, L).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tools
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


base(buselement(_,_,_,Base,_,_,_,_,_,_),Base).
high(buselement(_,_,_,_,High,_,_,_,_,_),High).
size(buselement(_,_,_,_,_,Size,_,_,_,_),Size).


