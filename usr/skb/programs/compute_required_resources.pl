%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Copyright (c) 2009, 2011, ETH Zurich.
% All rights reserved.
%
% This file is distributed under the terms in the attached LICENSE file.
% If you do not find this file, copies can be found by writing to:
% ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% this file is used for the ASPLOS11 paper to compute the padding overhead for
% different algorithms. the script "mach" loads the algorihtm and this file into
% eclipse_language-clp and outputs these values.


compute_required_resources(Plans, ResList, Name) :-
    ( foreach(Plan, Plans),
      foreach(res(minrealbase(Min), maxrealbase(Max), realres(RealRes), devicesum(ResWithoutPadding), paddingoverhead(PaddingOverhead),
                  rootrange(RootMinMem, RootMaxMem), rootsize(RootSize), rootoverflow(ROMin, ROLimit), rootdifference(BaseDiff, LimitDiff),
                  fillrate(FillRate, FillPercent), consumptionrate(ConsumptionRate, ConsumptionPercent)), ResList)
      do
        compute_required_resource_for_plan(Plan, Min, Max, ResWithoutPadding, RealRes, PaddingOverhead, RootMinMem,
                                           RootMaxMem, RootSize, ROMin, ROLimit, BaseDiff, LimitDiff,
                                           FillRate, FillPercent, ConsumptionRate, ConsumptionPercent)
    ),
% this would output the data for all buses, but we only want to consider one
% because our gnuplotscript would otherwise merge the two values into one bus...
%    ( foreach(Res, ResList)
%      do
%        write_gnuplot_data(Res)
%    ),

% consider only the biggest bus
    sort(7, >=, ResList, ResListSorted),
    [Hres|_] = ResListSorted,
    write_gnuplot_data(Hres),

    nl,
    ( foreach(Plan, Plans)
      do
        write_full_tree(Plan)
    ),
    tell(Name),
    ( foreach(Plan, Plans)
      do
        plan_to_dot(Plan)
    ),
    told.

compute_required_resource_for_plan(BusElements, Min, Max, ResWithoutPadding, RealRes, PaddingOverhead, RootMinMem, RootMaxMem, RootSize,
                                   ROMin, ROLimit, BaseDiff, LimitDiff, FillRate, FillPercent, ConsumptionRate, ConsumptionPercent) :-
    maplist(base, BusElements, Base),
    maplist(high, BusElements, High),
    eclipse_language:min(Base, Min),
    eclipse_language:max(High, Max),
    RealRes is Max - Min,
    compute_unpadded_size(BusElements, ResWithoutPadding),
    PaddingOverhead is RealRes - ResWithoutPadding,
    find_rootbridge_range(BusElements, RootMinMem, RootMaxMem),
    RootSize is RootMaxMem - RootMinMem,
    FillRate is ResWithoutPadding / RootSize,
    FillPercent is FillRate * 100,
    ConsumptionRate is RealRes / RootSize,
    ConsumptionPercent is ConsumptionRate * 100,
    BaseDiff is RootMinMem - Min,
    LimitDiff is Max - RootMaxMem - 1,
    ( BaseDiff < 0 ->
        ROMin is 0;
        ROMin is BaseDiff
    ),
    ( LimitDiff < 0 ->
        ROLimit is 0;
        ROLimit is LimitDiff
    ).



compute_unpadded_size([], 0).
compute_unpadded_size([H|BusElements], UnpaddedSize) :-
    buselement(bridge,_,_,_,_,_,_,_, _, _) = H,
    compute_unpadded_size(BusElements, UnpaddedSize).

compute_unpadded_size([H|BusElements], UnpaddedSize) :-
    buselement(_,_,_,_,_,_,io,_, _, _) = H,
    compute_unpadded_size(BusElements, UnpaddedSize).

compute_unpadded_size([H|BusElements], UnpaddedSize) :-
    buselement(device,Addr,BAR,_,_,_,mem,_, _, _) = H,
    bar(Addr,BAR,_,S,_,_,_),
    compute_unpadded_size(BusElements, S1),
    UnpaddedSize is S1 + S.

compute_unpadded_size_alt(BusElements, UnpaddedSize) :-
    findall(S, (
                member(El, BusElements),
                buselement(device,Addr,BAR,_,_,_,mem,prefetchable, _, _) = El,
                bar(Addr,BAR,_,S,mem,prefetchable,_)
               ), PrefetchSize),
    findall(S, (
                member(El2, BusElements),
                buselement(device,Addr,BAR,_,_,_,mem,nonprefetchable, _, _) = El2,
                bar(Addr,BAR,_,S,mem,nonprefetchable,_)
               ), NonPrefetchSize),
    append(NonPrefetchSize, PrefetchSize, Sizes),
    sum(Sizes, UnpaddedSize).



find_rootbridge_range([H|_], MinMem, MaxMem) :-
    buselement(_,addr(Bus,_,_),_,_,_,_,_,_, _, _) = H,
    rootbridge(_, childbus(MinBus, MaxBus), mem(MinMem, MaxMem)),
    Bus >= MinBus,
    Bus =< MaxBus.


write_gnuplot_data(Result) :-
     res(minrealbase(Min), maxrealbase(Max), realres(RealRes), devicesum(ResWithoutPadding), paddingoverhead(PaddingOverhead),
                   rootrange(RootMinMem, RootMaxMem), rootsize(RootSize), rootoverflow(ROMin, ROLimit), rootdifference(BaseDiff, LimitDiff),
                   fillrate(FillRate, FillPercent), consumptionrate(ConsumptionRate, ConsumptionPercent)) = Result,
     
     write("res"),
     write("\t"),
     write(Min),write("\t"),
     write(Max),write("\t"),
     write(RealRes),write("\t"),
     write(ResWithoutPadding),write("\t"),
     write(PaddingOverhead),write("\t"),
     write(RootMinMem),write("\t"),
     write(RootMaxMem),write("\t"),
     write(RootSize),write("\t"),
     write(ROMin),write("\t"),
     write(ROLimit),write("\t"),
     write(BaseDiff),write("\t"),
     write(LimitDiff),write("\t"),
     write(FillRate),write("\t"),
     write(FillPercent),write("\t"),
     write(ConsumptionRate),write("\t"),
     write(ConsumptionPercent).




write_full_tree(BusElements) :-
    maplist(base, BusElements, Base),
    maplist(high, BusElements, High),
    eclipse_language:min(Base, Min),
    eclipse_language:max(High, Max),
    Size is Max - Min,
%root bridge information
    write("treedata"),
    write("\t"),
    write("base\t"),
    write("\""),
    write("b("),
    write("0"),write(","),
    write("0"),write(","),
    write("0"),write(")"),
    write("\"\t"),
    write("-1"),write("\t"),
    write(Min),write("\t"),
    write(Size),write("\t"),
    write("1\t"),
    write("root bridge\t"),
    write("0"),write("\t"),
    write("mem"),write("\t"),
    write("both"),write("\t"),
    write("pcie"),write("\n"),
    write("treedata"),
    write("\t"),
    write("high\t"),
    write("\""),
    write("b("),
    write("0"),write(","),
    write("0"),write(","),
    write("0"),write(")"),
    write("\"\t"),
    write("-1"),write("\t"),
    write(Max),write("\t"),
    write(Size),write("\t"),
    write("1\t"),
    write("root bridge\t"),
    write("0"),write("\t"),
    write("mem"),write("\t"),
    write("both"),write("\t"),
    write("pcie"),nl,
    write_tree(BusElements).

write_tree([]).
write_tree([buselement(bridge,addr(Bus,Dev,Fun),secondary(Sec),Base,High,Size,Type,Prefetch, PCIe, 0)|BusElements]) :-
    write("treedata"),
    write("\t"),
    write("base\t"),
    write("\""),
    write("b("),
    write(Bus),write(","),
    write(Dev),write(","),
    write(Fun),write(")"),
    write("\"\t"),
    write(Bus),write("\t"),
    write(Base),write("\t"),
    write(Size),write("\t"),
    write("1\t"),
    write(Bus),write("\t"),
    write(Sec),write("\t"),
    write(Type),write("\t"),
    write(Prefetch),write("\t"),
    write(PCIe),write("\n"),
    write("treedata"),
    write("\t"),
    write("high\t"),
    write("\""),
    write("b("),
    write(Bus),write(","),
    write(Dev),write(","),
    write(Fun),write(")"),
    write("\"\t"),
    write(Bus),write("\t"),
    write(High),write("\t"),
    write(Size),write("\t"),
    write("1\t"),
    write(Bus),write("\t"),
    write(Sec),write("\t"),
    write(Type),write("\t"),
    write(Prefetch),write("\t"),
    write(PCIe),nl,
    write_tree(BusElements).

write_tree([buselement(device,addr(Bus,Dev,Fun),BAR,Base,High,Size,Type,Prefetch, PCIe, Pin)|BusElements]) :-
    write("treedata"),
    write("\t"),
    write("base\t"),
    write("\""),
    write("d("),
    write(Bus),write(","),
    write(Dev),write(","),
    write(Fun),write(")"),
    write("\"\t"),
    write(Bus),write("\t"),
    write(Base),write("\t"),
    write(Size),write("\t"),
    write("0\t"),
    write(Bus),write("\t"),
    write(BAR),write("\t"),
    write(Type),write("\t"),
    write(Prefetch),write("\t"),
    write(PCIe),write("\n"),
    write("treedata"),
    write("\t"),
    write("high\t"),
    write("\""),
    write("d("),
    write(Bus),write(","),
    write(Dev),write(","),
    write(Fun),write(")"),
    write("\"\t"),
    write(Bus),write("\t"),
    write(High),write("\t"),
    write(Size),write("\t"),
    write("0\t"),
    write(Bus),write("\t"),
    write(BAR),write("\t"),
    write(Type),write("\t"),
    write(Prefetch),write("\t"),
    write(PCIe),nl,
    write_tree(BusElements).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% dot
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

buses(buselement(_, addr(Bus,_,_), _, _, _, _, _, _,_,_),Bus).

plan_to_dot(BusElements) :-
    maplist(base, BusElements, Base),
    maplist(high, BusElements, High),
    eclipse_language:min(Base, Min),
    eclipse_language:max(High, Max),
    Size is Max - Min,
    maplist(buses, BusElements, Buses),
    eclipse_language:min(Buses,B),
    eclipse_language:max(Buses,H),

    writeln('digraph G {'),
    writeln('\tedge[style=solid,color=black];'),
    writeln('\tnode[color=lightblue,style=filled];'),
    write('\t"Node'),write(B),writeln('" ['),
    write('\t\tlabel="rootbridge: '),
    write(Addr),write('\\l'),
    write('childbuses: ['),write(B),write(', '),write(H),write(']\\l'),
    write('mem: ['),printf("%x",Min),write(', '),printf("%x",Max),write(']\\l'),
%    write('prefetchable: ['),printf("%x",PMin),write(', '),printf("%x",PMax),write(']\\l'),
%    write('non-prefetchable: ['),printf("%x",NPMin),write(', '),printf("%x",NPMax),write(']'),
    writeln('"'),
    writeln('\t\tshape="hexagon"'),
    writeln('\t\tcolor="yellow"'),
    writeln('\t];'),
    plan_to_dot_process_list(BusElements, B),
    writeln('}').

plan_to_dot_process_list([], _).
plan_to_dot_process_list([buselement(bridge, addr(Bus,Dev,Fun), secondary(Sec), L, H, S, Type, Prefetch,_,_)|T], RootSecondary) :-
    write('\t"Node'),
    write(Sec),
    write(Type),
    write(Prefetch),
    writeln('" ['),
    write('\t\tlabel="bridge\\l'),
    write('addr('),write(Bus),write(','),write(Dev),write(','),write(Fun),write(')'),
    write('\\lsecondary: '),write(Sec),
    write('\\l'),write(Type),write(':\\l['),printf("%x",L),write(',\\l '),printf("%x",H),write(']\\l'),
    write('size: '),printf("%x",S),write('\\l'),
    write(Prefetch),writeln('"'),
    writeln('\t\tshape="hexagon"'),
    ( Prefetch = prefetchable ->
        writeln('\t\tcolor=orange');
        true
    ),
    writeln('\t];'),
    write('\t"Node'),write(Sec),write(Type),write(Prefetch),write('" -> "Node'),
    write(Bus),
    ( not Bus =:= RootSecondary ->
        write(Type),
        write(Prefetch);
        true
    ),
    ( Prefetch = prefetchable ->
        writeln('" [color=red, style=bold];');
        writeln('" [color=blue, style=bold];')
    ),
    plan_to_dot_process_list(T,RootSecondary).

plan_to_dot_process_list([buselement(device, addr(Bus,Dev,Fun), BAR, L, H, S, Type, Prefetch,_,_)|T], RootSecondary) :-
    write('\t"Node'),
    write(Bus),write(Dev),write(Fun),write(BAR),
    writeln('" ['),
    write('\t\tlabel="device\\l'),
    write('addr('),write(Bus),write(','),write(Dev),write(','),write(Fun),write(')'),
    write('\\lBAR '),write(BAR),
    write('\\l'),write(Type),write(':\\l['),printf("%x",L),write(',\\l '),printf("%x",H),write(']\\l'),
    write('size: '),printf("%x",S),write('\\l'),
    write(Prefetch),writeln('"'),
    writeln('\t\tshape="ellipse"'),
    ( Prefetch = prefetchable ->
        writeln('\t\tcolor=orange');
        true
    ),
    writeln('\t];'),
    write('\t"Node'),write(Bus),write(Dev),write(Fun),write(BAR),write('" -> "Node'),
    write(Bus),
    ( not Bus =:= RootSecondary ->
        write(Type),
        write(Prefetch);
        true
    ),
    ( Prefetch = prefetchable ->
        writeln('" [color=red];');
        writeln('" [color=blue];')
    ),
    plan_to_dot_process_list(T,RootSecondary).


