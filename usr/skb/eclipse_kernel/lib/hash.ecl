% ----------------------------------------------------------------------
% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The ECLiPSe Constraint Logic Programming System. 
% The Initial Developer of the Original Code is  Cisco Systems, Inc. 
% Portions created by the Initial Developer are
% Copyright (C) 1997-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% 
% Hash table library
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Stefano Novello, IC-Parc
% Version:	$Id: hash.ecl,v 1.1.2.1 2009/01/09 01:29:01 jschimpf Exp $
%
% ----------------------------------------------------------------------

:- module(hash).

:- comment(summary, "Hash table library").
:- comment(author, "Stefano Novello, IC-Parc").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2009/01/09 01:29:01 $").

:- export(hash_create/1).
:- export(hash_add/3).
:- export(hash_remove/3).
:- export(hash_find/3).
:- export(hash_contains/2).
:- export(hash_count/2).
:- export(hash_delete/2).
:- export(hash_erase/1).
:- export(hash_get/3).
:- export(hash_set/3).
:- export(hash_iter/2).
:- export(hash_next/4).
:- export(hash_last/1).
:- export(hash_keys/2).
:- export(hash_list/3).
:- export(hash_stat/1).
:- export(hash_update/4).
:- export(hash_clone/2).
:- export(hash_entry/3).
:- export hash_insert_suspension/3.
:- export hash_terminate_suspensions/1.

%:- lib(notify_ports).	% autoload on demand

:- local struct(hash_table(
    	size,		% size of table array
	nb_elems,	% current number of entries
	table,		% array[size] of bucket lists
	keys,		% cached list of keys (for hash_list/2)
	elems,		% cached list of values (for hash_list/2)
	change,		% suspension list, woken on change (or var if unused)
	changed		% notification send port for changes (or var if unused)
    )).

:- local struct(hash_elem(key,elem)).
:- local struct(hash_iter(next_index,bucket,table,eleft)).
:- export portray(property(functor) of hash_table, hash_display/2, []).


t_hash(hash(Key,Size,Hash), Hash is term_hash(Key, -1, Size) + 1).

:- inline(hash/3, t_hash/2).
hash(Key,Size,Hash) :-
	hash(Key,Size,Hash).	% inlined


:- comment(hash_create/1, [
    amode:(hash_create(-) is det),
    args:["Table":"A variable"],
    see_also:[hash_set/3,hash_get/3,hash_erase/1],
    summary:"Creates a new hash table"]).


hash_create(hash_table{
	    size:4,
	    nb_elems:0,
	    table:'[]'([],[],[],[]),
	    keys:[],
	    elems:[],
	    change:_,
	    changed:_
	}).


:- comment(hash_erase/1, [
    amode:(hash_erase(+) is det),
    args:["Table":"Hash table"],
    summary:"Remove all entries in the hash table",
    see_also:[hash_create/1,hash_delete/2]
    ]).

hash_erase(H) :-
	H = hash_table{change:SuspList},
	( var(SuspList) ->
	    hash_erase_simple(H)
	;
	    % we need to send notifications for every removed element
	    hash_list(H, Keys, Vals),
	    hash_erase_simple(H),
	    % notify atomically after table has been erased
	    call_priority((
		( foreach(Key,Keys), foreach(Val,Vals), param(H) do
		    notify(H, rem(Key,Val))
		)
	    ), 2)
	).

    % Overwrite fields with the ones of a fresh table (and gc old fields).
    % Retain only the change/changed fields.
    hash_erase_simple(H) :-
	hash_create(New),
	New = hash_table{size:S,nb_elems:N,table:T,keys:K,elems:E},
	setarg(size of hash_table,	H, S),
	setarg(nb_elems of hash_table,	H, N),
	setarg(table of hash_table,	H, T),
	setarg(keys of hash_table,	H, K),
	setarg(elems of hash_table,	H, E).


:- comment(hash_insert_suspension/3, [
    amode:(hash_insert_suspension(+,+,-) is det),
    args:["Table":"Hash table",
	    "Susp":"A suspension",
	    "Notifications":"A receive port, see library(notify_ports)"
    	],
    summary:"Attach a suspension to be woken on hash table modifications",
    desc:html("
        Attach a suspension Susp to the hash table Table such that it gets
	woken whenever the table changes (i.e. when entries are added, changed
	or removed).
	<p>
	The suspended goal would typically be a demon (because it is going to
	be woken repeatedly, on every change). hash_insert_suspension/3 also
	supplies a Notifications argument, which should be used as one of the
	arguments of the suspended goal (see example). This is a \"receive
	port\" as defined by library(notify_ports), and is used to convey
	information about the changes that happened to the hash table.
	The notifications are of the following form:
	<dl>
	<dt>add(Key,Value)</dt>             <dd>if a table entry was added</dd>
	<dt>chg(Key,OldValue,NewValue)</dt> <dd>if a table entry was modified</dd>
	<dt>rem(Key,OldValue)</dt>          <dd>if a table entry was removed</dd>
	</dl>
	Note that the suspensions will be always be woken <B>after</B> the hash
	table has changed, so they will see the new state when they wake up.
    "),
    see_also:[hash_terminate_suspensions/1,
    	      notify_ports:receive_notifications/3,
    	      notify_ports:foreachnotification/6,
	      library(notify_ports),
	      (demon)/1],
    eg:"
    % Program:

	hash_create_verbose(H) :-
	    hash_create(H),
	    make_suspension(report(Notifications,Susp), 2, Susp),
	    hash_insert_suspension(H, Susp, Notifications).

	:- demon(report/2).
	report(Notifications, Susp) :-
	    notify_ports:receive_notifications(Notifications, List, Status),
	    writeln(changes:List),
	    ( Status = closed -> kill_suspension(Susp) ; true ).


    % Sample execution

    ?- hash_create_verbose(H),
       hash_set(H,k1,v1), hash_set(H,k1,v2), hash_delete(H,k1),
       hash_terminate_suspensions(H).
    changes : [add(k1, v1)]
    changes : [chg(k1, v1, v2)]
    changes : [rem(k1, v2)]
    changes : []

    H = hash(4, 0, [])
    Yes (0.00s cpu)
    "]).

hash_insert_suspension(H, Susp, Receiver) :-
	H = hash_table{change:SuspList},
	Receiver = rec(_),	% assumes knowledge of lib(notify_ports)!!!
	( var(SuspList) ->
	    init_suspension_list(change of hash_table, H),
	    notify_ports:open_sender(changed of hash_table, H)
	;
	    true
	),
	enter_suspension_list(change of hash_table, H, Susp),
	notify_ports:open_receiver(changed of hash_table, H, 1, Receiver).


:- comment(hash_terminate_suspensions/1, [
    amode:(hash_terminate_suspensions(+) is det),
    args:["Table":"Hash table"],
    summary:"Wake and terminate all suspensions attached to the hash table",
    desc:html("Wake all suspensions attached to the hash table and close the
    	corresponding notification port. This informs the suspended goals that
	there will be no further notifications, and gives them the opportunity
	to clean themselves up.  This should normally be done once the hash
	table will no longer be used (or modified).
    "),
    see_also:[hash_create/1,hash_insert_suspension/3],
    eg:"see hash_insert_suspension/3"]).

hash_terminate_suspensions(H) :-
	H = hash_table{change:SuspList},
	( var(SuspList) -> true ;
	    notify_ports:close_sender(changed of hash_table, H),
	    schedule_suspensions(change of hash_table, H),
	    setarg(change of hash_table, H, _),
	    setarg(changed of hash_table, H, _),
	    wake
	).


notify(H, Message) :-
	notify_ports:send_notification(changed of hash_table, H, Message) ->
	schedule_suspensions(change of hash_table, H),
	wake.


% CAUTION: this is like copy_term, but must not share anything,
% even if ground...
hash_clone(Old, New) :-
	Old = hash_table{size:Size, table:T},
	update_struct(hash_table, [table:NewTable,change:_,changed:_], Old, New),
	functor(NewTable, [], Size),
	( for(I,1,Size), param(T,NewTable) do
	    arg(I, T, OldBucket),
	    arg(I, NewTable, NewBucket),
	    ( foreach(hash_elem{key:K,elem:V}, OldBucket),
	      foreach(hash_elem{key:K,elem:V}, NewBucket) do
		true
	    )
	).


:- comment(hash_set/3, [
    amode:(hash_set(+,++,+) is det),
    args:["Table":"A hash table", "Key":"a ground term", "Value":"Any term"],
    see_also:[hash_get/3,hash_update/4],
    summary:"Add an (or modify the existing) entry with key Key and value Value to the hash table"]).

hash_set(H,Key,Elem) :-
	hash_add(H,Key,Elem).


:- comment(hash_add/3, [
    amode:(hash_add(+,++,+) is det),
    args:["Table":"A hash table", "Key":"a ground term", "Value":"Any term"],
    see_also:[hash_set/3],
    summary:"A synonym for hash_set/3"]).

hash_add(H,Key,Elem) :-
	H = hash_table{size:Size,nb_elems:Nb,table:T,change:Susps},
	setarg(keys of hash_table,H,0),
	setarg(elems of hash_table,H,0),
	hash(Key,Size,Index),
	arg(Index, T, Bucket),
	( member(E,Bucket), E = hash_elem{key:Key,elem:OldElem} ->
	    setarg(elem of hash_elem,E,Elem),
	    ( var(Susps) -> true ; notify(H, chg(Key,OldElem, Elem)))	% should be last
	;
	    Nb1 is Nb + 1,
	    MaxNb is Size * 3,
	    (Nb1 =< MaxNb ->
		Match = hash_elem{key:Key,elem:Elem},
		setarg(nb_elems of hash_table,H,Nb1),
		setarg(Index,T,[Match|Bucket]),
		( var(Susps) -> true ; notify(H, add(Key,Elem)))	% should be last
	    ;
		grow(H),
		hash_add(H,Key,Elem)
	    )
	).

:- comment(hash_remove/3, [
    amode:(hash_remove(+,++,-) is semidet),
    args:["Table":"A hash table", "Key":"a ground term", "Value":"Any term"],
    summary:"Remove the entry with key Key and retrieve its value Value",
    see_also:[hash_get/3,hash_delete/2,hash_erase/1],
    fail_if:"No entry for Key" ]).

hash_remove(H,Key,Elem) :-
	H = hash_table{size:Size,nb_elems:Nb,table:T,change:Susps},
	setarg(keys of hash_table,H,0),
	setarg(elems of hash_table,H,0),
	hash(Key,Size,Index),
	arg(Index, T, Bucket),
	Match = hash_elem{key:Key,elem:Elem0},
	once(delete(Match,Bucket,NewBucket)),	% May fail.
	Nb1 is Nb - 1,
	% Maybe add code to shrink table if 4*elems < array size
	% to save memory - no speed advantage.
	setarg(nb_elems of hash_table,H,Nb1),
	setarg(Index,T,NewBucket),
	( var(Susps) -> true ; notify(H, rem(Key,Elem0))), % after removal, before unification
	Elem = Elem0.


:- comment(hash_delete/2, [
    amode:(hash_delete(+,++) is det),
    args:["Table":"Hash table", "Key":"a ground term"],
    summary:"Remove the entry with key Key (if any)",
    see_also:[hash_remove/3,hash_erase/1]
    ]).

hash_delete(H, Key) :-
	( hash_remove(H, Key, _) -> true ; true ).


:- comment(hash_contains/2, [
    amode:(hash_contains(+,++) is semidet),
    args:["Table":"A hash table", "Key":"a ground term"],
    summary:"Succeeds if there is an entry stored under key Key",
    see_also:[hash_get/3,hash_list/3],
    fail_if:"No entry for Key" ]).

hash_contains(H,Key) :-
	hash_find(H,Key,_).


:- comment(hash_get/3, [
    amode:(hash_get(+,++,-) is semidet),
    args:["Table":"A hash table", "Key":"a ground term", "Value":"Any term"],
    summary:"Find the entry stored under key Key and return its value",
    see_also:[hash_create/1,hash_set/3,hash_list/3,hash_contains/2,hash_update/4],
    fail_if:"No entry for Key" ]).

hash_get(H,Key,Elem) :-
	hash_find(H,Key,Elem).


:- comment(hash_find/3, [
    amode:(hash_find(+,++,-) is semidet),
    args:["Table":"A hash table", "Key":"a ground term", "Value":"Any term"],
    summary:"A synonym for hash_get/3",
    see_also:[hash_get/3],
    fail_if:"No entry for Key" ]).

hash_find(H,Key,Elem) :-
	H = hash_table{size:Size,table:T},
	hash(Key,Size,Index),
	arg(Index, T, Bucket),
	member(hash_elem{key:Key,elem:E}, Bucket),
	!,
	E=Elem.


:- comment(hash_update/4, [
    amode:(hash_update(+,++,-,+) is semidet),
    args:["Table":"A hash table", "Key":"a ground term",
    	"OldValue":"Any term", "NewValue":"Any term"],
    summary:"Lookup and replace the value stored under Key",
    desc:"A combination of hash_get/3 and hash_set/3, but more efficient
    	because there is only one hash lookup.",
    eg:"
	?- hash_create(H),
	   hash_set(H, k, hello),
	   hash_update(H, k, Old, world),
	   hash_get(H, k, New).

	Old = hello
	New = world
	Yes (0.00s cpu)

	% sample code based on hash_update/4:
    	hash_inc(H, Key) :-
	    hash_update(H, Key, N0, N1),
	    N1 is N0+1.

    	hash_addto(H, Key, Value) :-
	    hash_update(H, Key, Values, [Value|Values]).
    ",
    see_also:[hash_get/3, hash_set/3],
    fail_if:"No entry for Key" ]).

hash_update(H,Key,OldElem,Elem) :-
	H = hash_table{size:Size,table:T,change:Susps},
	hash(Key,Size,Index),
	arg(Index, T, Bucket),
	member(Entry, Bucket),
	Entry = hash_elem{key:Key,elem:E},
	!,
	E=OldElem,
	setarg(elems of hash_table,H,0),
	setarg(elem of hash_elem,Entry,Elem),
	( var(Susps) -> true ; notify(H, chg(Key,OldElem, Elem))).	% should be last


    grow(H):-
	H = hash_table{size:OldSize,table:OldT},
	Size is 4 * OldSize,
	functor(T,[],Size),
	( foreacharg([],T) do true ),
	setarg(size of hash_table,H,Size),
	setarg(table of hash_table,H,T),
	( foreacharg(OldBucket,OldT),
	  param(T,Size)
	do
	    ( foreach(Elem,OldBucket),
	      param(T,Size)
	    do
		Elem = hash_elem{key:Key},
	    	hash(Key,Size,Index),
		arg(Index,T,Bucket),
		setarg(Index,T,[Elem|Bucket])
	    )
	).


:- comment(hash_count/2, [
    amode:(hash_count(+,-) is det),
    args:["Table":"A hash table", "Count":"A variable or number"],
    summary:"Returns the number of entries in the table",
    see_also:[hash_create/1,hash_list/3] ]).

hash_count(hash_table{nb_elems:N}, N).


:- comment(hash_entry/3, [
    amode:(hash_entry(+,-,-) is nondet),
    amode:(hash_entry(+,++,-) is semidet),
    args:["Table":"A hash table", "Key":"a term", "Value":"a term"],
    fail_if:"No entry for Key",
    summary:"Succeeds if Key and Value are an entry in table",
    desc:html("
	Like hash_get/3, but allows the Key to be uninstantiated, in which
	case all hash table entries will be enumerated on backtracking.
    "),
    see_also:[hash_get/3,hash_list/3] ]).

hash_entry(Hash, Key, Value) :-
	nonground(Key), !,
	Hash = hash_table{size:Size,table:T},
	between(1, Size, 1, Index),
	arg(Index, T, Bucket),
	member(hash_elem{key:Key,elem:Value}, Bucket).
hash_entry(Hash, Key, Value) :-
	hash_find(Hash, Key, Value).


:- comment(hash_last/1, [amode:(hash_last(+) is semidet),
    args:["Iter":"Hash table iterator structure"],
    summary:"Succeeds if the iterator has reached the end of the table",
    fail_if:"There are further table entries left in the iteration",
    see_also:[hash_iter/2,hash_next/4,hash_list/3] ]).

hash_last(hash_iter{eleft:0}).


:- comment(hash_iter/2, [amode:(hash_iter(+,-) is det),
    args:["Table":"A hash table", "Iter":"Hash table iterator structure (output)"],
    summary:"Create an iterator to traverse the hash table",
    desc:html("
	Create an iterator to traverse the hash table. Note that the
	hash table should not be modified while the iterator is used,
	otherwise the behaviour is undefined."),
    see_also:[hash_next/4,hash_last/1,hash_list/3] ]).

hash_iter(H,Iter) :-
	H = hash_table{table:T,nb_elems:N},
	arg(1, T, Bucket),
	Iter =  hash_iter{next_index:2,bucket:Bucket,table:T,eleft:N}.


:- comment(hash_next/4, [amode:(hash_next(+,-,-,-) is semidet),
    args:["Iter":"Hash table iterator structure",
    	"Key":"A ground term (output)",
    	"Value":"A term (output)",
    	"Iter":"Hash table iterator structure (output)"],
    summary:"Get the next Key - Value pair according to the iterator",
    fail_if:"No further entries left in this iteration",
    see_also:[hash_iter/2,hash_last/1,hash_list/3] ]).

hash_next(hash_iter{next_index:S1,bucket:Bucket,table:T,eleft:N},Key,Elem,Iter) :-
	next_iter(N,S1,Bucket,T,Key,Elem,Iter).

    next_iter(N,S,[],T,Key,Elem,Iter) :-
	N > 0,
	S1 is S+1,
	arg(S, T, Bucket),
	next_iter(N,S1,Bucket,T,Key,Elem,Iter).
    next_iter(N,S,[E|Bucket],T,Key,Elem,Iter) :-
	E = hash_elem{key:Key,elem:Elem},
	N1 is N-1,
	Iter = hash_iter{next_index:S,bucket:Bucket,table:T,eleft:N1}.


:- comment(hash_list/3, [
    amode:(hash_list(+,-,-) is det),
    args:["Table":"A hash table", "Keys":"a variable or list", "Values":"variable or list"],
    summary:"Retrieve the hash table contents",
    desc:html("
	Retrieve the hash table contents in the form of a list of Keys
	and a list of Values. These lists are cached by the hash table
	and only recomputed when the table has changed.")
    ]).

hash_list(H,Keys,List) :-
	H = hash_table{table:T,keys:K,elems:E},
	( E == 0 ->
	    % keys may be cached, but we recompute them anyway
	    (
		foreacharg(Bucket,T),
		fromto(Keys, Keys0, Keys1, []),
		fromto(List, List0, List1, [])
	    do
	    	(
		    foreach(hash_elem{key:Key,elem:Elem}, Bucket),
		    fromto(Keys0,[Key|KT],KT,Keys1),
		    fromto(List0,[Elem|ET],ET,List1)
		do
		    true
		)
	    ),
	    setarg(keys of hash_table ,H,Keys),
	    setarg(elems of hash_table ,H,List)
	;
	    % if elems are cached, keys are cached as well
	    Keys = K,
	    List = E
	).


:- comment(hash_keys/3, [
    amode:(hash_keys(+,-) is det),
    args:["Table":"A hash table", "Keys":"a variable or list"],
    summary:"Retrieve the current hash table keys",
    desc:html("
	Retrieve the hash table keys in the form of a list of Keys.
	This list is cached by the hash table and only recomputed when
	the table has changed.")
    ]).

hash_keys(H,Keys) :-
	H = hash_table{table:T,keys:K},
	( K == 0 ->
	    (
		foreacharg(Bucket,T),
		fromto(Keys, Keys0, Keys1, [])
	    do
	    	(
		    foreach(hash_elem{key:Key}, Bucket),
		    fromto(Keys0,[Key|KT],KT,Keys1)
		do
		    true
		)
	    ),
	    setarg(keys of hash_table,H,Keys)
	;
	    Keys = K
	).


:- export hash_display/2.
hash_display(Table,DisplayTable):-
	Table = hash_table{size:Size,nb_elems:NbElems},
	hash_iter(Table,HashIter),
	(
	    fromto(HashIter,In,Out,none),
	    fromto(DisplayList,Display1,Display0,[])
	do
	    (hash_last(In)->
		 Out = none,
		 Display1 = Display0
	    ;
		 hash_next(In,Key,Value,Out),
		 Display1 = [(Key ->Value)|Display0]
	    )
	),
	DisplayTable= hash(Size,NbElems,DisplayList).


:- comment(hash_stat/1, [amode:(hash_stat(+) is det),
    args:["Table":"A hash table"],
    summary:"Prints statistics about the hash table",
    see_also:[hash_create/1] ]).

hash_stat(hash_table{nb_elems:N,size:Size,table:T}) :-
	(
	    foreacharg(Bucket,T),
	    fromto(0,Max0,Max1,Max),
	    fromto(0,E0,E1,Empties),
	    fromto(0,Sum0,Sum1,Sum)
	do
	    length(Bucket, L),
	    Max1 is max(Max0,L),
	    E1 is E0 + 1 - sgn(L),
	    Sum1 is Sum0+L
	),
	( Sum = N -> true ; printf("Inconsistent table (entries)%n", []) ),
	( functor(T, _, Size) -> true ; printf("Inconsistent table (size)%n", []) ),
	Avg is Sum/(Size-Empties),
	printf("Table size: %w%n", [Size]),
	printf("Number of entries: %w%n", [N]),
	printf("Empty slots: %w%n", [Empties]),
	printf("Average access: %w%n", [Avg]),
	printf("Worst case access: %w%n", [Max]).


