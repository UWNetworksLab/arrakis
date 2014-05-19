:- local store(rh).
:- local store(sequenceTable).

:- dynamic watch/1.
:- lib(lists).

% This can be enabled when external/2 and lib(regex) works correctly (don't
% forget to remove the stuff in skb_main.c in that case)...
%:- lib(regex).
%:- external(save_index/3, p_save_index).
%:- external(remove_index/3, p_remove_index).
%:- external(remove_index/3, p_remove_index).
%:- external(index_intersect/4, p_index_intersect).

%
% Get Record
%
get_object(Name, AList, CList, Object) :-
    make_all_constraints(AList, CList, SConstraints),
    (atom(Name) -> 
        get_by_name(Name, SConstraints, Object)
        ;
        get_by_constraints(Name, SConstraints, Object)
    ).

get_first_object(Name, AList, CList, Object) :-
    make_all_constraints(AList, CList, SConstraints),
    (atom(Name) -> 
        get_by_name(Name, SConstraints, Object)
        ;
        get_by_constraints(Name, SConstraints, Object)
    ), !.

get_by_constraints(Name, Constraints, Object) :-
    find_candidates(Constraints, Candidate),
    ( not var(Name) ->
        Name = name_constraint(Value),
        match(Value, Candidate, []) 
    ; true ),
    match_object(Candidate, Constraints, Object).

get_by_name(Name, Constraints, Object) :-
    atom(Name),
    match_object(Name, Constraints, Object).

match_object(Name, Constraints, object(Name, SList)) :-
    store_get(rh, Name, SList),
    match_constraints(Constraints, SList).

find_candidates(Constraints, RecordName) :-
    length(Constraints, 0), !,
    stored_keys(rh, AllNames),
    iterate_candidates(AllNames, RecordName).
find_candidates(Constraints, RecordName) :-
    not length(Constraints, 0), !,
    get_index_names(Constraints, IdxList),
    find_next_candidate(IdxList, RecordName).

find_next_candidate(AttributeList, NextItem) :-
    index_intersect_aux(AttributeList, 0, NextItem).

% This makes our C predicate non-deterministic
index_intersect_aux(AttributeList, OldState, Item) :-
    index_intersect(rh, AttributeList, OldState, NewItem),
    (
        Item = NewItem
    ;
        index_intersect_aux(AttributeList, NewItem, Item)
    ). 

iterate_candidates([Cur|Rest], RecordName) :- 
    (RecordName = Cur ; 
    iterate_candidates(Rest, RecordName)).

get_index_names([], []).
get_index_names([Cur|CList], [Attribute|AList]) :-
    (Cur = constraint(Attribute, _, _) ; Cur = val(Attribute, _)),
    get_index_names(CList, AList).

make_all_constraints(AList, CList, SConstraints) :-
    convert_attributes(AList, ACList),
    append(ACList, CList, Constraints),
    sort(Constraints, SConstraints). % Sorting allows us to do matching in linear time


%
% Attribute/Constraint Matching
%
string_compare(C, A, B) :-
    atom(A), string(B), !,
    atom_string(A, SA),
    string_compare(C, SA, B).
string_compare(C, A, B) :-
    string(A), atom(B), !,
    atom_string(B, SB),
    string_compare(C, A, SB).
string_compare('==', A, B) :- !,
    compare(=, A, B).
string_compare('!=', A, B) :- !,
    (compare(<, A, B), ! ; compare(>, A, B), !).
string_compare('>=', A, B) :- !,
    (compare(>, A, B), ! ; compare(=, A, B), !).
string_compare('<=', A, B) :- !,
    (compare(<, A, B), ! ; compare(=, A, B), !).
string_compare(C, A, B) :- !,
    compare(C, A, B).

number_compare('==', A, B) :-
    !, A =:= B.
number_compare('!=', A, B) :-
    !, A =\= B.
number_compare('<=', A, B) :-
    !, A =< B.
number_compare(C, A, B) :-
    !, FX =.. [C, A, B],
    call(FX).

match_constraints([], _).
    
% Number comparison
match_constraints([constraint(Key, Comparator, Value)|Rest], [val(Key, SVal)|SRest]) :-
    number(SVal), number(Value), !,
    number_compare(Comparator, SVal, Value),
    match_constraints(Rest, [val(Key, SVal)|SRest]).

% Regular Expression
match_constraints([constraint(Key, match, Value)|Rest], [val(Key, SVal)|SRest]) :-
    !, ( (string(SVal) ; atom(SVal)), (string(Value) ; atom(Value)) ),
    match(Value, SVal, []),
    match_constraints(Rest, [val(Key, SVal)|SRest]).

% String comparison
match_constraints([constraint(Key, Comparator, Value)|Rest], [val(Key, SVal)|SRest]) :-
    ( (string(Value) ; atom(Value)), (string(SVal) ; atom(SVal)) ), !,
    string_compare(Comparator, SVal, Value),
    match_constraints(Rest, [val(Key, SVal)|SRest]).

% Variable
match_constraints([constraint(Key, '==', Value)|Rest], [val(Key, SVal)|SRest]) :-
    var(Value),  !,
    Value = SVal,
    match_constraints(Rest, [val(Key, SVal)|SRest]).

% Skip to next relevant Slot in List
match_constraints([constraint(AKey, Comparator, Value)|Rest], [val(SKey, SVal)|SRest]) :-
    compare(>, AKey, SKey), !,
    match_constraints([constraint(AKey, Comparator, Value)|Rest], SRest).

% Helper functions to convert attributes in constraint and match them against object
prepare_constraint(val(Key, QVal), constraint(Key, ==, QVal)).
convert_attributes(AList, CList) :-
    maplist(prepare_constraint, AList, CList).


%
% Add Record
%
next_sequence(Name, Next) :-
    store_get(sequenceTable, Name, Next),
    !,
    store_inc(sequenceTable, Name).
next_sequence(Name, 0) :-
    store_inc(sequenceTable, Name).

add_seq_object(Name, UList, CList) :-
    next_sequence(Name, Seq),
    number_string(Seq, SeqStr),
    atom_string(Name, NameStr),
    append_strings(NameStr, SeqStr, NameSeqStr),
    atom_string(NameSeq, NameSeqStr),
    add_object(NameSeq, UList, CList).

add_object(Name, UList, CList) :-
    get_object(Name, [], CList, object(Name, SList)),
    del_attribute_index(Name, SList),
    save_object(Name, UList), !.
add_object(Name, UList, CList) :-
    length(CList, 0),
    save_object(Name, UList).

save_object(Name, SList) :-
    transform_attributes(SList, USList),
    store_set(rh, Name, USList),
    set_attribute_index(Name, USList),
    !,
    trigger_watches(object(Name, USList), 1),
    print_object(object(Name, SList)).


transform_attributes(AList, RNDList) :-
    sort(AList, RList),
    filter_duplicates(RList, RNDList).

filter_duplicates([], []).
filter_duplicates([X], [X]) :- !.
filter_duplicates([val(Key, X),val(Key, Y)|Rest], Out) :-
    filter_duplicates([val(Key, Y)|Rest], Out).
filter_duplicates([val(Key1, X), val(Key2, Y)|Rest], [val(Key1, X)|Out]) :-
    Key1 \= Key2,
    filter_duplicates([val(Key2, Y)|Rest], Out).


%
% Attribute Index
%
set_attribute_index(Name, SList) :-
    save_index(rh, SList, Name).
del_attribute_index(Name, SList) :-
    remove_index(rh, SList, Name).

%
% Delete Record
%
del_object(Thing, AList, CList) :-
    get_object(Thing, AList, CList, object(Name, SList)),
    store_delete(rh, Name),
    !,
    del_attribute_index(Name, SList),
    trigger_watches(object(Name, SList), 2).

%
% Watches
%

% TODO
% assert/retract are really bad in terms of performance
% this is nothing else as a subscription, combine this with pubsub 
% as long as the amount of concurrent watches is small this is ok
set_watch(Template, Mode, Recipient) :-
    Recipient = subscriber(Binding, Id, ReplyState, Mode),
    Template = template(Name, AList, CList),
    add_subscription(trigger, Id, Template, Recipient).

trigger_watches(Record, Mode) :-
    find_watches(Record, Watches),
    check_watches(Record, Mode, Watches).

find_watches(Record, L) :-
    coverof(X, find_subscriber(trigger, Record, X), L), !.
find_watches(_, []).

check_watches(_, _, []).
check_watches(Record, Mode, [T|Rest]) :-
    check_watch(Record, Mode, T),
    check_watches(Record, Mode, Rest).

check_watch(Record, Action, subscriber(Binding, Id, ReplyState, Mode)) :-
    Action /\ Mode > 0,
    !,
    format_object(Record, Output),
    trigger_watch(Output, Action, Mode, ReplyState, Id, Retract),
    try_retract(Retract, Id, Binding).
check_watch(_, _, _). % Checking watches should never fail

try_retract(1, Id, Binding) :-
    delete_subscription(trigger, Id, Binding).
try_retract(0, _, _). 

remove_watch(Binding, Id) :-
    store_get(trigger, Id, subscription(_, _, subscriber(Binding, Id, ReplyState, Mode))),
    delete_subscription(trigger, Id, Binding),
    trigger_watch(_, 16, 0, ReplyState, Id, _). % 16 is OCT_REMOVED

%
% Output
%
print_names([]) :-
    flush(1),
    flush(2).
print_names([object(X, _)]) :-
    !,
    write(X),
    flush(1),
    flush(2).
print_names([object(X, _)|Rest]) :-
    write(X),
    write(', '),
    print_names(Rest).

print_object(X) :-
    format_object(X, Out),
    write(Out),
    flush(1),
    flush(2).

format_object(object(Thing, SlotList), O4) :-
    atom_string(Thing, StrThing),
    append_strings(StrThing, " { ", O2),
    format_slots(SlotList, O2, O3),
    !,
    append_strings(O3, " }", O4).

format_slots([], In, Out) :-
    append_strings(In, "", Out).
format_slots([S], In, Out) :-
    format_slot(S, In, Out).
format_slots([S|Rest], In, Out) :-
    format_slot(S, In, Out2),
    append_strings(Out2, ", ", Out3),
    format_slots(Rest, Out3, Out).

format_slot(val(Attr, X), In, Out) :-
    atom_string(Attr, StrAttr),
    append_strings(In, StrAttr, Out1),
    append_strings(Out1, ": ", Out2),
    format_slot_val(X, Out2, Out).

format_slot_val(Val, In, Out) :-
    number(Val),
    number_string(Val, StrVal),
    append_strings(In, StrVal, Out).
format_slot_val(Val, In, Out) :-
    atom(Val),
    atom_string(Val, StrVal),
    append_strings(In, StrVal, Out).
format_slot_val(Val, In, Out) :-
    string(Val),
    append_strings(In, "'", Out1),
    append_strings(Out1, Val, Out2),
    append_strings(Out2, "'", Out).
