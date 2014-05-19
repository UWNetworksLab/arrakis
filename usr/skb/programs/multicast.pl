%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :-include("boardlayout.pl").
% :-include("gruyere_rtt_facts.txt").

:-lib(branch_and_bound).

% filters out duplicates

filter([], []).
filter([H|T], L) :-
    filter(T, M),
    ( not member(H, M) ->
        append(M, [H], L);
        L = M
    ).


% sanity check: Check first that we have all the necessary information
multicast_sanity_check :-
    is_predicate(nr_running_cores/1),
    is_predicate(cpu_thread/4),
    is_predicate(message_rtt/6),
    nr_running_cores(NrRunningCores),
    findall(X,cpu_thread(X,_,_,_),L),
    length(L,NrRunningCores),
    ExpectedNrRTTMeasurements is NrRunningCores * (NrRunningCores - 1),
    findall(X, message_rtt(X,_,_,_,_,_), L2),
    length(L2, ExpectedNrRTTMeasurements).



% construct links to all the neighbours of APIC_ID on the same package as it
sendNeighbours(APIC_ID, Sends) :-
    % find package containing APIC_ID
    cpu_thread(APIC_ID, Package, _, _),
    % construct links to all my neighbours
    findall(sendto(APIC_ID,X),(cpu_thread(X,Package,_,_),X =\= APIC_ID),Sends).

% creates a list with sendto(SrcCore, DstCore) to define which core should send
% to which other core

% sends(+StartCore, +PackageList, -SendsList)
sends(_, [],[]).
sends(StartAPIC_ID, [H|T],[HS|Sends]) :-
    % find the lowest APIC ID on the package as APIC_ID
    findall(X, cpu_thread(X, H, _, _), APICIDs),
    sort(APICIDs, [APIC_ID|_]),
    % construct a link to it from the start ID
    HS = sendto(StartAPIC_ID, APIC_ID),
    % recurse on other packages
    sends(StartAPIC_ID, T, Sends2),
    % find all the cores on the same package as APIC_ID, and add pairs for them
    sendNeighbours(APIC_ID, M),
    append(Sends2,M,Sends).


% add the rtt number to every sendto tuple

annotate_rtt([],[]).
annotate_rtt([sendto(Src,Dst)|T1],[sendto(Src,Dst,Lat)|T2]) :-
    message_rtt(Src,Dst,Lat1,_,_,_),
    message_rtt(Dst,Src,Lat2,_,_,_),
    Lat is Lat1 + Lat2,
    annotate_rtt(T1,T2).


% constructs the send list starting at StartCore

multicast_tree_cost(StartCore,[SendH|SendList], Cost) :-
    multicast_sanity_check,
    % determine package of start core
    cpu_thread(StartCore, StartPackage, _, _),
    % construct list of other packages
    findall(X, (cpu_thread(_,X,_,_), X =\= StartPackage), L),
    filter(L,PackageList),
    % compute possible links to those packages as SendList1
    sends(StartCore, PackageList, SendList1),
    % compute links from start core to its neighbours
    sendNeighbours(StartCore, Neighbours),
    append(SendList1, Neighbours, SendList2),
    % annotate with RTT of each link
    annotate_rtt(SendList2, SendList3),
    % sort by decreasing RTT
    sort(3, >=, SendList3, [SendH|SendList]),
    % determine cost as maximum single-link RTT
    % XXX: this is not quite right, we really care about maximum end-to-end RTT
    sendto(_,_,Cost) = SendH.



% goal to be called.
% Construct a list of sendto/3 goals, sort them by latency in decreasing order and
% minimize the value of the longest latency

multicast_tree(StartCore,SendList) :-
    minimize(multicast_tree_cost(StartCore, SendList, Cost), Cost).

