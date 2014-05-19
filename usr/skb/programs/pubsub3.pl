:- dynamic subscribed/3.

:- local store(ps). % stores subscriptions
:- local store(trigger). % stores triggers
%:- external(bitfield_add/3, p_bitfield_add).
%:- external(bitfield_remove/3, p_bitfield_remove).
%:- external(bitfield_union/4, p_bitfield_union).

add_subscription(Storage, Id, Template, Subscriber) :-
    Template = template(Name, AList, CList),
    make_all_constraints(AList, CList, Constraints),
    store_set(Storage, Id, subscription(Name, Constraints, Subscriber)),
    bitfield_add(Storage, Constraints, Id).

delete_subscription(Storage, Id, Binding) :-
    store_get(Storage, Id, subscription(_, Constraints, Subscriber)),
    arg(1, Subscriber, Binding),
    store_delete(Storage, Id),
    bitfield_remove(Storage, Constraints, Id).

%% Should really use delete_subscription
unsubscribe(Storage, Id, Binding) :-
    store_get(Storage, Id, subscription(_, Constraints, Subscriber)),
    arg(1, Subscriber, Binding),
    store_delete(Storage, Id),
    bitfield_remove(Storage, Constraints, Id),
    writeln(Subscriber).
    
find_subscriber(Storage, Message, Subscriber) :-
    Message = object(Name, AList),
    sort(AList, SAList),
    SMessage = object(Name, SAList),
    find_subscription_candidates(Storage, SMessage, Candidate),
    store_get(Storage, Candidate, Subscription),
    match_message(SMessage, Subscription),
    Subscription = subscription(_, _, Subscriber).

find_subscription_candidates(Storage, Message, Ids) :-
    Message = object(Name, Attributes),
    (not length(Attributes, 0) ; atom(Name)), !, % TODO
    get_index_names(Attributes, IdxList),
    find_next_subscriber(Storage, IdxList, Ids).

find_next_subscriber(Storage, AttributeList, NextItem) :-
    index_union_aux(Storage, AttributeList, "s", NextItem).

% This makes our C predicate non-deterministic
index_union_aux(Storage, AttributeList, OldState, Item) :-
    bitfield_union(Storage, AttributeList, OldState, NewItem),
    (
        Item = NewItem
    ;
        index_union_aux(Storage, AttributeList, NewItem, Item)
    ).

match_message(Message, Subscription) :-
    Message = object(MName, AList),
    Subscription = subscription(SName, Constraints, _),
    ( not var(SName) ->
        ( not atom(SName) ->
            SName = name_constraint(Value),
            match(Value, MName, [])
        ; SName = MName )
    ; true ),
    match_attributes(AList, Constraints).

% Similar to match_constraints in objects code but works the other
% way around: we match attributs against constraints
match_attributes(_, []).

% Number comparison
match_attributes([val(Key, AValue)|Rest], [constraint(Key, Comparator, CValue)|CRest]) :-
    number(AValue), number(CValue), !,
    number_compare(Comparator, AValue, CValue),
    match_attributes([val(Key, AValue)|Rest], CRest).

% Regular Expression
match_attributes([val(Key, AValue)|Rest], [constraint(Key, match, CValue)|CRest]) :-
    !, ( (string(AValue) ; atom(AValue)), (string(CValue) ; atom(CValue)) ),
    match(CValue, AValue, []),
    match_attributes([val(Key, AValue)|Rest], CRest).

% String comparison
match_attributes([val(Key, AValue)|Rest], [constraint(Key, Comparator, CValue)|CRest]) :-
    ( (string(CValue) ; atom(CValue)), (string(AValue) ; atom(AValue)) ), !,
    string_compare(Comparator, AValue, CValue),
    match_attributes([val(Key, AValue)|Rest], CRest).

% Variable
match_attributes([val(Key, AValue)|Rest], [constraint(Key, '==', CValue)|CRest]) :-
    var(CValue),  !,
    CValue = AValue,
    match_attributes([val(Key, AValue)|Rest], CRest).

% Skip to next relevant Slot in List
match_attributes([val(AKey, AValue)|Rest], [constraint(CKey, Comparator, CValue)|SRest]) :-
    compare(>, CKey, AKey), !,
    match_attributes(Rest, [constraint(CKey, Comparator, CValue)|SRest]).