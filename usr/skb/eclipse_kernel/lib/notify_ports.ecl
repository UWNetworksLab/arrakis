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
% Copyright (C) 2005-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: notify_ports.ecl,v 1.1 2008/06/30 17:43:47 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(notify_ports).


:- comment(summary, "One-to-many and many-to-many notification ports").
:- comment(author, "Joachim Schimpf").
:- comment(date, "$Date: 2008/06/30 17:43:47 $").
:- comment(copyright, "Cisco Systems, Inc").

:- comment(desc, html("<P>
    This library implements a nonlogical feature, called notification
    ports.  They are a form of messaging, i.e. there is are send ports
    and attached receive ports, and messages in the form of general terms
    can be passed through these ports.  Both send and receive ports have
    unique handles, which is the nonlogical bit.
    </P><P>
    There are two variants of this feature, one-to-many and many-to-many
    ports.
    </P>

    <h3>One-To-Many</h3>
    <P>
    In the one-to-many variant, messages sent from a single send port
    can be received independently by several receivers. In this setting,
    the message stream is essentially an infinite list, with the sender
    extending the list at the tail and the receivers each individually
    progressing through the list.
    </P><P>
    Straightforward interface:
    <PRE>
	open_sender(-Sender)
	close_sender(+Sender)
	send_notification(+Sender, +Message)
	open_receiver(+Sender, -Receiver)
	open_receiver_init(+Sender, +InitMsgs, -InitMsgsTail, -Receiver)
	receive_notifications(+Receiver, -Messages, -Status)
	foreachnotification(+BaseName, -Message, +Params, +Receiver, -Status, +Goals)
    </PRE>
    There is also a slightly more memory efficient API where sender and
    receiver can be fields of larger structures rather than separate
    substructures. These  larger structures must always be created
    by the caller (in the case of the sender this is often an attribute
    structure, in the case of the receiver it is sometimes advantageous
    to package a suspension together with the receiver in order to kill
    it at the end of all messages):
    <PRE>
	open_sender(+SendPos, +SendStruct)
	close_sender(+SendPos, +SendStruct)
	send_notification(+SendPos, +SendStruct, +Message)
	open_receiver(+SendPos, +SendStruct, +ReceivePos, +ReceiveStruct)
	open_receiver_init(+SendPos, +SendStruct, +InitMsgs, -InitMsgsTail,
				+ReceivePos, +ReceiveStruct)
	receive_notifications(+ReceivePos, +ReceiveStruct, -Messages, -Status)
	foreachnotification(+BaseName, -Message, +Params, +ReceivePos, +ReceiveStruct, -Status, +Goals)
    </PRE>

    <h3>Many-To-Many</h3>
    <P>
    In the many-to-many variant, several send ports can be connected
    to several receive ports in an arbitray manner.
    To enable a receiver to distinguish messages from multiple senders,
    the messages get tagged with a sender- and receiver-specific id as
    they are received.
    </P>
    The corresponding predicates are the following. Note that sender and
    receiver are opened with different predicates, but the send and receive
    predicates are the same as for one-to-many ports:
    <PRE>
	open_tagging_sender(-Sender)
	open_tagged_receiver(+Tag, +Sender, -Receiver)

	send_notification(+Sender, +Message)
	receive_notifications(+Receiver, -Messages, -Status)
	foreachnotification(+BaseName, -Message, +Params, +Receiver, -Status, +Goals)
    </PRE>
    Note that closing of tagging senders is currently not implemented.
    ")).


:- comment(eg, "
    % One-to-many

    % This example shows a typical use of notification ports.
    % A notification port is used in addition to a waking list
    % in order to transfer precise information about the reason for waking.

    % We define a variable attribute (myattr) consisting of a send port
    % and a waking list. 


    :- lib(notify_ports).

    :- meta_attribute(myattr, []).
    :- local struct(myattr(port,susplist)).
    :- local struct(myrec(port,susp)).


    test :-
	    init_var(X),
	    log_all_messages(X),
	    touch_var(X, hello),
	    touch_var(X, out),
	    touch_var(X, there),
	    fini_var(X).


    % initialise and attach our attribute to the given variable
    init_var(X) :-
	    Attr = myattr{},
	    open_sender(port of myattr, Attr),
	    init_suspension_list(susplist of myattr, Attr),
	    add_attribute(X, Attr, myattr).


    % simulate an action on the variable: send a message and wake
    touch_var(_X{myattr:Attr}, Message) ?-
	    send_notification(port of myattr, Attr, Message),
	    schedule_suspensions(susplist of myattr, Attr),
	    wake.

    % finalise the attribute, e.g. before the variable gets instantiated
    fini_var(_X{myattr:Attr}) ?-
            close_sender(port of myattr, Attr),
	    schedule_suspensions(susplist of myattr, Attr),
	    wake.

    % a sample demon that will report every time the variable is touched
    log_all_messages(X{myattr:Attr}) ?-
	    Receiver = myrec{susp:Susp},
	    open_receiver(port of myattr, Attr, port of myrec, Receiver),
	    suspend(log_demon(Receiver), 2, X->myattr:(susplist of myattr), Susp).

    :- demon log_demon/1.
    log_demon(Receiver) :-
	    foreachnotification(log, Message, [], port of myrec, Receiver, Status, (
		writeln(received(Message))
	    )),
	    ( Status = closed ->
		arg(susp of myrec, Receiver, Susp),
		kill_suspension(Susp),
		writeln(closed)
	    ;
		true
	    ).
    

    ( For a many-to-many example, see open_tagged_receiver/3 )
    ").

:- comment(open_sender/1, [
    summary:"Create a send port",
    amode:open_sender(-),
    args:[
	"SendPort":"a variable, will be bound to a structure"
    ]
]).

:- comment(open_tagging_sender/1, [
    summary:"Create a many-to-many send port ",
    amode:open_tagging_sender(-),
    args:[
	"SendPort":"a variable, will be bound to a structure"
    ],
    desc:html("
    Creates a send port that can be used in many-to-many communication,
    i.e. several of these ports can be connected to a single receiver.
    "),
    see_also:[open_tagged_receiver/3, send_notification/2,
    	receive_notifications/3, foreachnotification/6]
]).

:- comment(close_sender/1, [
    summary:"Close a send port",
    amode:close_sender(+),
    args:[
	"SendPort":"a send port structure"
    ]
]).

:- comment(send_notification/2, [
    summary:"Send a notification message",
    amode:send_notification(+, +),
    args:[
	"SendPort":"a send port structure",
	"Message":"arbitrary term"
    ],
    fail_if:"Fails if SendPort is closed",
    desc:html("
    Messages is an arbitrary term that gets send via the send port SendPort.
    All receivers that have connected to this send port at the time of
    sending will be able to receive the message.
    ")
]).

:- comment(open_receiver/2, [
    summary:"Create a receiver for a given notification sender",
    amode:open_receiver(+,-),
    args:[
	"SendPort":"a send port structure",
	"ReceivePort":"a variable, will be bound to a structure"
    ],
    desc:html("
    This predicate creates a receive port listening to messages sent
    via the specified send port.  The new receive port will receive
    all messages that are sent via the send port after the receiver
    has been opened.  Messages that were sent before the receiver has
    been opened will not be received by this receiver.
    "),
    see_also:[open_receiver_init/4]
]).

:- comment(open_tagged_receiver/3, [
    summary:"Create a receiver for one or more tagging senders",
    amode:open_tagged_receiver(+,+,-),
    amode:open_tagged_receiver(+,+,+),
    args:[
	"Tag":"an arbitrary term",
	"SendPort":"a tagged-send-port structure",
	"ReceivePort":"a tagged-receive-port structure or a variable"
    ],
    desc:html("
    This predicate either creates a new receive port and connects it to an
    existing tagged-send port, or connects an existing receive port to an
    additional existing tagged-send port.  The new receive port will receive
    all messages that are sent via the send port after the receiver
    has been opened.  Any messages that were sent before the receiver was
    opened will not be received by this receiver.
    </P><P>
    Messages that arrive on ReceivePort from SendPort will get tagged with
    Tag, i.e. the received message will be a structure of the form
    <PRE>
    	Tag : Message
    </PRE>
    If several senders are connected to ReceivePort, the tag can thus
    be used to identify the origin of the message.
    </P>
    "),
    eg:"
    ?-	open_tagging_sender(S1),
	open_tagging_sender(S2),
	open_tagged_receiver(r1s1, S1, R1),
	open_tagged_receiver(r1s2, S2, R1),
	open_tagged_receiver(r2s1, S1, R2),
	open_tagged_receiver(r2s2, S2, R2),

	send_notification(S1, m1),
	send_notification(S1, m2),
	send_notification(S2, m3),
	send_notification(S1, m4),
	send_notification(S2, m5),

	receive_notifications(R1, R1M1, _),
	receive_notifications(R2, R2M1, _).

    ...
    R1M1 = [r1s1 : m1, r1s1 : m2, r1s2 : m3, r1s1 : m4, r1s2 : m5]
    R2M1 = [r2s1 : m1, r2s1 : m2, r2s2 : m3, r2s1 : m4, r2s2 : m5]
    Yes (0.00s cpu)
    ",
    see_also:[open_tagging_sender/1,send_notification/2,
    	receive_notifications/3, foreachnotification/6]
]).

:- comment(open_receiver_init/4, [
    summary:"Create a receiver for a given notification sender",
    amode:open_receiver_init(+, +, -, -),
    args:[
	"SendPort":"a send port structure",
	"InitialMessages":"the head of a list of initial messages",
	"InitialMessagesTail":"the tail of the list of initial messages",
	"ReceivePort":"a variable, will be bound to a structure"
    ],
    desc:html("
    This predicate creates a receive port listening to messages sent
    via the specified send port.  The new receive port will receive
    all messages that are sent via the send port after the receiver
    has been opened.  Messages that were sent before the receiver has
    been opened will not be received by this receiver.
    <P>
    In addition to open_receiver/2, there is a difference list pair
    (InitialMessages and InitialMessagesTail) which can be used to
    fake a sequence of initial message that will be received on
    the receive port without actually having been sent from the
    associated send port. This feature can be used to bring the
    message receiving agent into a particular starting state.
    "),
    see_also:[open_receiver/2]
]).

:- comment(receive_notifications/3, [
    summary:"Receive a list of currently available notification messages",
    amode:receive_notifications(+, -, -),
    args:[
	"ReceivePort":"a receiver structure as created by open_receiver",
	"Messages":"a variable, will be bound to a list",
	"Status":"a variable, will be bound to 'open' or 'closed'"
    ],
    desc:html("
    This predicate retrieves all the messages that are currently
    available at the given receive port.  This means all messages that
    were sent via the associated send port but have not yet been
    retrieved from this receive port. The messages are listed in the
    order in which they were sent.
    <P>
    The Status argument indicates whether the associated sender is still
    open ('open') or has been closed ('closed'). If closed, no more
    messages will arrive on this receive port in the future.
    "),
    see_also:[foreachnotification/6]
]).

:- comment(foreachnotification/6, [
    summary:"A control construct to iterate over received notifications",
    amode:foreachnotification(+, -, +, +, -, +),
    args:[
	"BaseName":"an atom used as the basename for the generated auxiliary predicate",
	"Message":"a variable",
	"Params":"a list of global variables in the iteration body (as in do/2)",
	"ReceivePort":"a receiver structure as created by open_receiver",
	"Status":"a variable, will be bound to 'open' or 'closed'",
	"Goals":"the goals that will be called for each iteration"
    ],
    desc:html("
    This is a control construct iterating over the currently available
    messages on the given receive port. The purpose is to process the
    received messages one by one without the need to create an auxiliary
    list of received messages. The iteration terminates when there are
    (currently) no more message on the receive port.
    <P>
    When the iteration terminates, the Status argument indicates whether
    the associated sender is still open ('open') or has been closed
    ('closed'). If closed, no more messages will arrive on this receive
    port in the future.
    <P>

    "),
    see_also:[receive_notifications/3],
    eg:"
	process_all_messages(ReceivePort, Log) :-
	    foreachnotification(sample, Message, [Log], ReceivePort, Status, (
		writeln(Log, received(Message)),
		do_something(Message)
	    )),
	    ( Status = closed ->
		writeln(Log, end_of_messages)
	    ;
		writeln(Log, more_coming)
	    ).
    "
]).

:- comment(open_sender/2, [
    summary:"Initialise a structure field as a send port",
    amode:open_sender(+, +),
    args:[
	"Pos":"positive integer, the send port's field number in Struct",
	"Struct":"a structure (with arity Pos or more)"
    ]
]).

:- comment(close_sender/2, [
    summary:"Close a send port on a structure field",
    amode:close_sender(+, +),
    args:[
	"Pos":"positive integer, the send port's field number in Struct",
	"Struct":"a structure whose Pos-th field is a send port"
    ]
]).

:- comment(send_notification/3, [
    summary:"Send a notification message",
    amode:send_notification(+, +, +),
    args:[
	"Pos":"positive integer, the send port's field number in Struct",
	"Struct":"a structure whose Pos-th field is a send port",
	"Message":"arbitrary term"
    ],
    fail_if:"Fails if send port is closed",
    desc:html("
    Pos and Struct identify a send port. Messages is an arbitrary
    term that gets send via this port. All receivers that have connected
    to this send port at the time of sending will be able to receive
    the message.
    ")
]).

:- comment(open_receiver/4, [
    summary:"Create a receiver for a given notification sender",
    amode:open_receiver(+, +, +, +),
    args:[
	"SendPos":"positive integer, the send port's field number in SendStruct",
	"SendStruct":"a structure whose SendPos-th field is a send port",
	"ReceivePos":"positive integer, the send port's field number in ReceiveStruct",
	"ReceiveStruct":"a structure with free field ReceivePos"
    ],
    desc:html("
    SendPos and SendStruct identify a send port.  This predicate
    creates a receive port listening to messages sent via the
    specified send port.  The new receive port will receive all
    messages that are sent via the send port after the receiver has
    been opened.  Messages that were sent before the receiver has been
    opened will not be received by this receiver.
    <P>
    The receiver will be installed in field ReceivePos of the structure
    ReceiveStruct.
    "),
    see_also:[open_receiver_init/6]
]).

:- comment(open_receiver_init/6, [
    summary:"Create a receiver for a given notification sender",
    amode:open_receiver_init(+, +, +, -, +, +),
    args:[
	"SendPos":"positive integer, the send port's field number in SendStruct",
	"SendStruct":"a structure whose SendPos-th field is a send port",
	"InitialMessages":"the head of a list of initial messages",
	"InitialMessagesTail":"the tail of the list of initial messages",
	"ReceivePos":"positive integer, the send port's field number in ReceiveStruct",
	"ReceiveStruct":"a structure with free field ReceivePos"
    ],
    desc:html("
    SendPos and SendStruct identify a send port.  This predicate
    creates a receive port listening to messages sent via the
    specified send port.  The new receive port will receive all
    messages that are sent via the send port after the receiver has
    been opened.  Messages that were sent before the receiver has been
    opened will not be received by this receiver.
    <P>
    The receiver will be installed in field ReceivePos of the structure
    ReceiveStruct.
    <P>
    In addition to open_receiver/4, there is a difference list pair
    (InitialMessages and InitialMessagesTail) which can be used to
    fake a sequence of initial message that will be received on
    the receive port without actually having been sent from the
    associated send port. This feature can be used to bring the
    message receiving agent into a particular starting state.
    "),
    see_also:[open_receiver/4]
]).

:- comment(receive_notifications/4, [
    summary:"Receive a list of currently available notification messages",
    amode:receive_notifications(+,+,-,-),
    args:[
	"ReceivePos":"positive integer, the send port's field number in ReceiveStruct",
	"ReceiveStruct":"a structure",
	"Messages":"a variable",
	"Status":"a variable"
    ],
    desc:html("
    This predicate retrieves all the messages that are currently
    available at the given receive port.  This means all messages that
    were sent via the associated send port but have not yet been
    retrieved from this receive port.
    <P>
    The Status argument indicates whether the associated sender is still
    open ('open') or has been closed ('closed'). If closed, no more
    messages will arrive on this receive port in the future.
    "),
    see_also:[foreachnotification/7]
]).

:- comment(foreachnotification/7, [
    summary:"A control construct to iterate over received notifications",
    amode:foreachnotification(+, -, +, +, +, -, +),
    args:[
	"BaseName":"an atom used as the basename for the generated auxiliary predicate",
	"Message":"a variable",
	"Params":"a list of global variables in the iteration body (as in do/2)",
	"ReceivePos":"positive integer, the send port's field number in ReceiveStruct",
	"ReceiveStruct":"a structure",
	"Status":"a variable, will be bound to 'open' or 'closed'",
	"Goals":"the goals that will be called for each iteration"
    ],
    desc:html("
    This is a control construct iterating over the currently available
    messages on the given receive port. The purpose is to process the
    received messages one by one without the need to create an auxiliary
    list of received messages. The iteration terminates when there are
    (currently) no more message on the receive port.
    <P>
    When the iteration terminates, the Status argument indicates whether
    the associated sender is still open ('open') or has been closed
    ('closed'). If closed, no more messages will arrive on this receive
    port in the future.
    <P>

    "),
    see_also:[receive_notifications/4],
    eg:"see general example for the library"
]).


:- export open_sender/1.
open_sender(SendPort) :-
	SendPort = send([]),
	open_sender(1, SendPort).

:- export open_sender/2.
open_sender(Pos, Attr) :-
	% NOTE: here and in other marked locations, we use setarg/3 instead
	% of arg/3 just to make sure that the variable (that the argument is
	% being set to) is not physically allocated inside the structure!
	setarg(Pos, Attr, _Tail).	% see NOTE

:- export open_tagging_sender/1.
open_tagging_sender(SendPort) :-
	SendPort = tagging_send([]).


:- export close_sender/1.
close_sender(SendPort) :-
	SendPort = send(_),
	close_sender(1, SendPort).
close_sender(SendPort) :-
	SendPort = tagging_send(_),
	error(141, close_sender(SendPort)).	% unimplemented

:- export close_sender/2.
close_sender(Pos, Attr) :-
	arg(Pos, Attr, []).


:- export send_notification/2.
send_notification(SendPort, Event) :-
	SendPort = send(_),
	send_notification(1, SendPort, Event).
send_notification(tagging_send(List), Event) :-
	( foreach(SendPort,List), param(Event) do
	    SendPort = send(_,Tag),
	    send_tagged_notification(1, SendPort, Tag:Event)
	).

:- export send_notification/3.
send_notification(Pos, Attr, Event) :-
	NewFrame = [Event|Tail],
	arg(Pos, Attr, NewFrame),
	setarg(Pos, Attr, Tail).

send_tagged_notification(Pos, Attr, Event) :-
	NewFrame = [Event|Tail],
	arg(Pos, Attr, Tail0),
	deref_tail(Tail0, NewFrame),
	setarg(Pos, Attr, Tail).

    deref_tail(T0, T) :- var(T0), !, T=T0.
    deref_tail([_|T0], T) :- !,
	deref_tail(T0, T).
    deref_tail([], _) :-
    	writeln("notify_ports: trying to send via closed send port"),
	abort.

:- export open_receiver/2.
open_receiver(SendPort, ReceivePort) :-
	ReceivePort = rec([]),
	open_receiver(1, SendPort, 1, ReceivePort).

:- export open_receiver/4.
open_receiver(SendPos, SendStruct, ReceivePos, ReceiveStruct) :-
	arg(SendPos, SendStruct, Events),
	setarg(ReceivePos, ReceiveStruct, Events).		% see NOTE

% ??? can we close a tagged receiver once all associated senders are closed?
% Use a pseudo-messsage for each sender-close and keep a counter in the rec!
:- export open_tagged_receiver/3.
open_tagged_receiver(Tag, SendPort, ReceivePort) :-
	SendPort = tagging_send(List),
	!,
	TaggedSend = send([],Tag),
	open_sender(1, TaggedSend),
	( var(ReceivePort) ->
	    open_receiver(TaggedSend, ReceivePort)
	;
	    arg(1, ReceivePort, Events),
	    setarg(1, TaggedSend, Events)		% see NOTE
	),
	setarg(1, SendPort, [TaggedSend|List]).
open_tagged_receiver(_Tag, _SendPort, _ReceivePort) :-
	writeln("notify_ports: trying to open tagged receiver on normal send port"),
	abort.


:- export open_receiver_init/4.
open_receiver_init(SendPort, InitialEvents, InitialTail, ReceivePort) :-
	ReceivePort = rec([]),
	open_receiver_init(1, SendPort, InitialEvents, InitialTail, 1, ReceivePort).

:- export open_receiver_init/6.
open_receiver_init(SendPos, SendStruct, InitialEvents, InitialTail, ReceivePos, ReceiveStruct) :-
	setarg(ReceivePos, ReceiveStruct, InitialEvents),	% see NOTE
	arg(SendPos, SendStruct, InitialTail).


:- export receive_notifications/3.
receive_notifications(ReceivePort, Events, Status) :-
	receive_notifications(1, ReceivePort, Events, Status).

:- export receive_notifications/4.
receive_notifications(ReceivePos, ReceiveStruct, Events, Status) :-
	arg(ReceivePos, ReceiveStruct, EventStream),
	receive_notifications(ReceivePos, ReceiveStruct, Events, Status, EventStream).

    receive_notifications(ReceivePos, ReceiveStruct, [], Status, EventStream) :-
    	var(EventStream), !,
	Status = open,
	setarg(ReceivePos, ReceiveStruct, EventStream).
    receive_notifications(_ReceivePos, _ReceiveStruct, [], closed, []).
    receive_notifications(ReceivePos, ReceiveStruct, [E|REs], Status, [E|Es]) :-
	receive_notifications(ReceivePos, ReceiveStruct, REs, Status, Es).



:- inline(foreachnotification/6, tr_foreachnotification/3).	
:- export foreachnotification/7.
:- inline(foreachnotification/7, tr_foreachnotification/3).	
tr_foreachnotification(
	    foreachnotification(BaseName, Event, Params, Receiver, Status, Goals),
	    Transformed, Module) :-
	!,
	tr_foreachnotification(
	    foreachnotification(BaseName, Event, Params, 1, Receiver, Status, Goals),
	    Transformed, Module).
tr_foreachnotification(
	    foreachnotification(BaseName, Event, Params, RecPos, Receiver, Status, Goals0),
	    (arg(RecPos,Receiver,EEs), Call),
	    Module) :-
	concat_atoms(BaseName, '_foreachnotification', Name),
	Arity is length(Params) + 4,
	Call =.. [Name,EEs,RecPos,Receiver,Status|Params],
	functor(VarHead, Name, Arity),
	VarHead =.. [Name,EEsIn,RecPosIn,ReceiverIn,StatusIn|_Params],
	functor(NilHead, Name, Arity),
	NilHead =.. [Name,[],RecPosIn,ReceiverIn,StatusIn|_Params],
	RecHead =.. [Name,[Event|Es],RecPosIn,ReceiverIn,StatusIn|Params],
	RecCall =.. [Name,Es,RecPosIn,ReceiverIn,StatusIn|Params],
	expand_goal(Goals0, Goals)@Module,
	sepia_kernel:nested_compile_term([
		(VarHead :- var(EEsIn), !, StatusIn=open, setarg(RecPosIn, ReceiverIn, EEsIn)),
		(NilHead :- !, StatusIn=closed),
		(RecHead :- Goals, RecCall)
	    ])@Module,
	set_flag(Name/Arity, auxiliary, on)@Module.

:- export foreachnotification/6.
:- tool(foreachnotification/6, foreachnotification_/7).
foreachnotification_(BaseName, Event, Params, Receiver, Status, Goals, Module) :-
	tr_foreachnotification(foreachnotification(BaseName, Event, Params, Receiver, Status, Goals),
		Transformed, Module),
	call(Transformed)@Module.

:- tool(foreachnotification/7, foreachnotification_/8).
foreachnotification_(BaseName, Event, Params, RecPos, Receiver, Status, Goals, Module) :-
	tr_foreachnotification(foreachnotification(BaseName, Event, Params, RecPos, Receiver, Status, Goals),
		Transformed, Module),
	call(Transformed)@Module.

