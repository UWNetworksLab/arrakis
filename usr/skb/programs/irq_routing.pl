%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lib(ic).

% current settings of link devices
:- dynamic(setPir/2).

% GSIs in use and their Pir
:- dynamic(usedGsi/2).

% current assignments
:- dynamic(assignedGsi/3).

findgsi(Pin, Addr, Gsi, Pir) :-
        (
            % lookup routing table to see if we have an entry
            prt(Addr, Pin, PrtEntry)
        ;
            % if not, compute standard swizzle through bridge
            Addr = addr(Bus, Device, _),
            NewPin is (Device + Pin) mod 4,

            % recurse, looking up mapping for the bridge itself
            bridge(_, BridgeAddr, _, _, _, _, _, secondary(Bus)),
            findgsi(NewPin, BridgeAddr, Gsi, Pir)
        ),
        ( % is this a fixed GSI, or a link device?
            PrtEntry = gsi(Gsi),
            Pir = fixedGsi
        ; 
            PrtEntry = pir(Pir),
            pir(Pir, Gsi)
        ).

assignirq(Pin, Addr, Pir, Gsi) :-
        % determine usable GSIs for this device
        findgsi(Pin, Addr, Gsi, Pir),
        % don't change a previously-configured link device
        (
            Pir = fixedGsi
        ;
            setPir(Pir, _) -> setPir(Pir, Gsi)
        ;
            true
        ),
        % find all GSIs currently in use
        findall(X, usedGsi(X,_), AllGsis),
        % constrain GSIs not to overlap with the new GSI
        ic:alldifferent([Gsi|AllGsis]),
        indomain(Gsi),
        % store settings for future reference
        (
            Pir = fixedGsi
        ;
            assert(setPir(Pir,Gsi))
        ),
        assert(usedGsi(Gsi,Pir)).

assigndeviceirq(Addr) :-
        device(_, Addr, _, _, _, _, _, Pin),
        % ensure we have a sensible value for the Pin field
        Pin >= 0 and Pin < 4,
        ( % check for an exising allocation and return it if present
            assignedGsi(Addr, Pin, Gsi),
            usedGsi(Gsi, Pir)
        ; % assign new IRQ
            assignirq(Pin, Addr, Pir, Gsi),
            assert(assignedGsi(Addr, Pin, Gsi))
        ),
        printf("%s %d\n", [Pir, Gsi]).
