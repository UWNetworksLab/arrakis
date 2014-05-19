% test prolog file for the Barcelona workshop

:- include("../data/data_nos4.txt").

test_algo_list(Input, Output) :-
    % create list with "region(Base, Size)" elements
    findall(region(Base, Size),
             (
                % access stored facts
                mem_region_type(Type, Input),
                memory_region(Base, _, Size, Type, _)
             ),
            Output).

