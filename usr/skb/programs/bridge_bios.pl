%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2013, University of Washington.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This SKB PCI bridge config program just uses the preprogrammed
% values from the BIOS. Use if the regular PCI bridge configurator
% fails.

:- set_flag(print_depth, 600).

:-dynamic(currentbar/5).

bridge_programming(Plan, NrElements) :-
    convert_devices(Plan1),
    get_more(Plan1, Plan2),
    eval_more(Plan2, Plan3),
    % Filter IO BARs because some device drivers aren't able to handle them...
    filter_io_bars(Plan3, Plan),
    % We can leave out bridges for now, they're not programmed anyway
    % and we don't care about their BARs.
    %     DeviceElements = [],
    % convert_bridges(BridgeElements1),
    % get_more_bridge(BridgeElements1, BridgeElements),
    % eval_more(, DeviceElements),
    % append(DeviceElements, BridgeElements, Plan),
    length(Plan, NrElements).

convert_devices(L) :-
        findall(buselement(device, addr(Bus, Device, Function), _, _, _,
                                  _, _, _, PCIe, _),
                device(PCIe, addr(Bus, Device, Function), _, _, _, _,
                       _, _), L).

% convert_bridges(L) :-
%         findall(buselement(bridge, addr(Bus, Device, Function),
%                                   secondary(BusNum), _, _, _, mem, _, PCIe, _),
%                 bridge(PCIe, addr(Bus, Device, Function), _, _, _, _,
%                        _, secondary(BusNum)), L).

% device(PCIe, addr(Bus, Device, Function), Vendor, DeviceID, Class,
%        SubClass, ProgIf, IntPin), L).
% bridge(PCIe, addr(Bus, Device, Function), Vendor, DeviceID, Class, SubClass,
%        ProgIf, secondary(BusNum)), L).
% bar(addr(Bus, Device, Function), BARn, Base, Size, MemOrIO, Prefetch, 32or64).
% buselement(device, addr(Bus, Device, Function), BAR_Secondary, Base, High, Size,
%                   mem, Prefetch, PCIe, Bits).

get_more([],[]).
get_more([H|L], OutL) :-
        get_more(L, InL),
        buselement(device, addr(Bus, Device, Function), _, _, _,
                                  _, _, _, PCIe, _) = H,
        findall(buselement(device, addr(Bus, Device, Function), BARn,
                                  Base, _, Size, MemIO, Prefetch, PCIe, Bits),
                bar(addr(Bus, Device, Function), BARn, Base, Size, MemIO, Prefetch, Bits),
                A),
        append(A, InL, OutL).

% get_more_bridge([],[]).
% get_more_bridge([H|L], OutL) :-
%         get_more_bridge(L, InL),
%         buselement(bridge, addr(Bus, Device, Function), Secondary, _,
%                    _, _, mem, _, PCIe, _) = H,
%         findall(buselement(bridge, addr(Bus, Device, Function), Secondary,
%                                   Base, _, Size, mem, Prefetch, PCIe, Bits),
%                 bar(addr(Bus, Device, Function), _, Base, Size, _, Prefetch, Bits),
%                 A),
%         append(A, InL, OutL).

eval_more([],[]).
eval_more([H|L], OutL) :-
        eval_more(L, InL),
        buselement(device, addr(Bus, Device, Function), BARn, Base, _,
                                  Size, MemIO, Prefetch, PCIe, Bits) = H,
        Limit is Base + Size,
        append([buselement(device, addr(Bus, Device, Function), BARn, Base, Limit,
                                  Size, MemIO, Prefetch, PCIe, Bits)], InL,
               OutL).

filter_io_bars([],[]).
filter_io_bars([H|L], OutL) :-
        filter_io_bars(L, InL),
        buselement(device, addr(Bus, Device, Function), BARn, Base, Limit,
                   Size, MemIO, Prefetch, PCIe, Bits) = H,
        ( MemIO = io ->
            OutL = InL;
            append([buselement(device, addr(Bus, Device, Function), BARn, Base, Limit,
                               Size, MemIO, Prefetch, PCIe, Bits)], InL, OutL)
        ).

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
