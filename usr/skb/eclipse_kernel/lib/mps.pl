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
% Copyright (C) 1996-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Micha Meier, ECRC
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: mps.pl,v 1.1 2008/06/30 17:43:47 jschimpf Exp $
% ----------------------------------------------------------------------

%
%	The Message Passing System
%

% Available predicates:
%	mps_init(+Host)
%	mps_init(+Host, +Port)
%	mps_error(-Message)
%	mps_port_register(+Key, +Name, +Signature, +Port)
%	mps_port_lookup(+Key, +Name, -Port)
%	mps_port_deregister(+Key, +Name, +Signature)
%	mps_port_allocate(+UpcallProc, -Port)
%	mps_port_deallocate(+Port)
%	mps_send(+Port, ?Term)
%	mps_receive(+Port, ?Term)
%	mps_ping(+Host)
%	mps_ping(+Host, +Port)
%	mps_exit

:- begin_module(sepia_kernel).

:- export
	mps_port_allocate/2,
	mps_send/2,
	mps_receive/2,
	mps_init/1.

:- tool(mps_port_allocate/2, mps_port_allocate/3).
:- tool(mps_send/2, mps_send_body/3).
:- tool(mps_receive/2, mps_receive_body/3).

%:- import
%	bytes_to_term_/3,
%	mps_str_receive/2,
%	mps_str_send/2,
%	term_to_bytes_/3
%    from sepia_kernel.

mps_init(Host) :-
    mps_init(Host, _).

mps_send_body(Port, Term, Module) :-
    term_to_bytes_(Term, String, Module),
    mps_str_send(Port, String).

mps_receive_body(Port, Term, Module) :-
    mps_str_receive(Port, String),
    bytes_to_term_(String, Term, Module).

mps_error_handler(_, Goal) :-
    ( mps_error(Message) -> true ; Message = "Wrong usage !" ),
    printf(error, "Message passing error in %w\n%s\n%b", [Goal,Message]),
    exit_block(abort).

:- set_default_error_handler(176, mps_error_handler/2),
   reset_error_handler(176).

:- comment(mps_exit / 0, [
	summary:"Shutdown the message passing subsystem on this process.\n\n",
	template:"mps_exit",
	desc:html("   mps_exit causes disassociation from the name server and the other\n   processes.  Although not strictly necessary, it is recommended that\n   processes invoke mps_exit just before they terminate. \n\n<P>\n"),
	args:[],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[176 : "Message passing system error"],
	eg:"   \n    [eclipse 1]: mps_init(breeze).\n    yes.\n    [eclipse 2]: mps_port_allocate(true/0, Port).\n    Port = 2050162692\n    yes.\n    [eclipse 3]: mps_send(2050162692,\"Hello World !\").\n    yes.\n    [eclipse 4]: mps_receive(2050162692, Message).\n    Message = \"Hello World !\"\n    yes.\n    [eclipse 5]: mps_port_deallocate(2050162692).\n    yes.\n    [eclipse 6]: mps_exit. \n    yes.\n    [eclipse 7]: mps_exit.\n    message passing error in mps_exit\n\n\n\n\n",
	see_also:[mps_init / 1, mps_ping / 1, mps_port_register / 4, mps_port_lookup / 3, mps_port_deregister / 3, mps_port_allocate / 2, mps_port_deallocate / 1, mps_send / 2, mps_receive / 2]]).
:- comment(mps_init / 1, [
	summary:"Inititialise the message passing subsystem, using name server on Host.\n\n",
	template:"mps_init(+Host)",
	desc:html("   This predicate does initialisation of the message passing subsystem\n   and associates the ECLiPSe process with a name server.  This is the\n   first step to be taken by every process for becoming part of a\n   distributed ECLiPSe application.  The name server is identified by\n   the name of the host on which the name server resides.  The name of\n   the host is a simple string, e.g.  \"tricky\", \"tricky.ecrc.de\", or\n   \"141.1.3.150\".\n\n<P>\n"),
	args:["+Host" : "Atom or string."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[4 : "Host is not instantiated.", 5 : "Host is not a string or atom.", 176 : "Message passing system error"],
	eg:"   \n    [eclipse 1]: mps_init(breeze).\n    yes.\n    [eclipse 2]: mps_port_allocate(true/0, Port).\n    Port = 2050162692\n    yes.\n    [eclipse 3]: mps_send(2050162692,\"Hello World !\").\n    yes.\n    [eclipse 4]: mps_receive(2050162692, Message).\n    Message = \"Hello World !\"\n    yes.\n    [eclipse 5]: mps_port_deallocate(2050162692).\n    yes.\n    [eclipse 6]: mps_exit. \n    yes.\n\n    [eclipse 7]: mps_init(notahost).    \n    message passing error in mps_init(notahost, _81)\n\n\n\n\n",
	see_also:[mps_ping / 1, mps_exit / 0, mps_port_register / 4, mps_port_lookup / 3, mps_port_deregister / 3, mps_port_allocate / 2, mps_port_deallocate / 1, mps_send / 2, mps_receive / 2]]).
:- comment(mps_ping / 1, [
	summary:"Check whether the name server on Host is responding.\n\n",
	template:"mps_ping(+Host)",
	desc:html("    With mps_ping the name server can be pinged, i.e.  it succeeds if the\n    name server is up and running.  Normally, it is not used in an\n    application, but it may be useful for debugging purposes.  Be patient\n    when the mps_ping predicate seems to hang, because it can take a while\n    before mps_ping decides to fail.\n\n<P>\n"),
	args:["+Host" : "Atom or string."],
	resat:"   No.",
	fail_if:"   Fails if the server is not responding.\n\n",
	exceptions:[],
	eg:"   \n    [eclipse 1]: mps_init(breeze).\n    yes.\n    [eclipse 2]: mps_ping(breeze).\n    yes.\n    [eclipse 3]: mps_ping(whirlwind).\n    no (more) solution.\n\n\n\n\n",
	see_also:[mps_init / 1, mps_exit / 0, mps_port_register / 4, mps_port_lookup / 3, mps_port_deregister / 3, mps_port_allocate / 2, mps_port_deallocate / 1, mps_send / 2, mps_receive / 2]]).
:- comment(mps_port_allocate / 2, [
	summary:"Allocate a new message passing Port with notification predicate NotifyPred.\n\n",
	template:"mps_port_allocate(+NotifyPred, -Port)",
	desc:html("    Ports are the end-points of communication, ie.  the points where\n    messages are sent.  A process that wants to receive communication\n    needs to allocate a port and tell the port identifier to the potential\n    senders.  The senders, which will normally be other processes,\n    possibly on remote machines, can then send messages to the port.\n    The owner of the port, ie. the process that allocated it, can then\n    receive the messages using mps_receive/2.\n\n<P>\n    Ports are allocated and deallocated with the mps_port_allocate/2 and\n    mps_port_deallocate/1 predicates.  A port may be associated with a\n    notification predicate.\n\n<P>\n    Ports queue incoming messages.  A port's notification predicate is\n    called by the message passing system whenever a message is delivered\n    to the port with an empty queue.  Note that further messages arriving\n    on the non-empty port do not trigger the notification predicate again. \n    The notifier therefore has to take into account that there may be\n    several messages ready to receive.\n\n<P>\n    Notification predicates have one optional parameter via which the\n    message passing system passes the identifier of the empty port on\n    which a message arrived.  If no notification is wanted (ie.  for\n    polling) set NotifyPred to true/0.\n\n<P>\n"),
	args:["+NotifyPred" : "Term of the form atom/integer.", "-Port" : "A variable."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[4 : "NotifyPred is not instatiated", 5 : "NotifyPred is not of the form atom/integer", 6 : "NotifyPred has wrong arity", 60 : "The specified notification predicate does not exist", 176 : "Message passing system error"],
	eg:"   \n    [eclipse 1]: mps_init(breeze).\n    yes.\n    [eclipse 2]: mps_port_allocate(true/0, Port).\n    Port = 2050162692\n    yes.\n    [eclipse 3]: mps_send(2050162692,\"Hello World !\").\n    yes.\n    [eclipse 4]: mps_receive(2050162692, Message).\n    Message = \"Hello World !\"\n    yes.\n    [eclipse 5]: mps_port_deallocate(2050162692).\n    yes.\n    [eclipse 6]: mps_exit. \n    yes.\n\n\n\n\n",
	see_also:[mps_init / 1, mps_ping / 1, mps_exit / 0, mps_port_register / 4, mps_port_lookup / 3, mps_port_deregister / 3, mps_port_deallocate / 1, mps_send / 2, mps_receive / 2]]).
:- comment(mps_port_deallocate / 1, [
	summary:"Deallocate a message passing port.\n\n",
	template:"mps_port_deallocate(+Port)",
	desc:html("    Ports are the end-points of communication, ie.  the points where\n    messages are sent.  A process that wants to receive communication\n    needs to allocate a port and tell the port identifier to the potential\n    senders.  The senders, which will normally be other processes,\n    possibly on remote machines, can then send messages to the port.\n    The owner of the port, ie. the process that allocated it, can then\n    receive the messages using mps_receive/2.\n\n<P>\n    Ports are allocated and deallocated with the mps_port_allocate/2 and\n    mps_port_deallocate/1 predicates.\n    Note that deallocation of ports can be tricky because it is difficult\n    to ensure that no messages to this port are under way.\n\n<P>\n"),
	args:["+Port" : "A port identifier (integer)."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[4 : "Port is not instantiated", 5 : "Port is not an integer", 176 : "Message passing system error"],
	eg:"   \n    [eclipse 1]: mps_init(breeze).\n    yes.\n    [eclipse 2]: mps_port_allocate(true/0,Port).\n    Port = 2050162692\n    yes.\n    [eclipse 3]: mps_send(2050162692,\"Hello World !\").\n    yes.\n    [eclipse 4]: mps_receive(2050162692, Message).\n    Message = \"Hello World !\"\n    yes.\n    [eclipse 5]: mps_port_deallocate(2050162692).\n    yes.\n    [eclipse 6]: mps_exit. \n    yes.\n\n\n\n\n",
	see_also:[mps_init / 1, mps_ping / 1, mps_exit / 0, mps_port_register / 4, mps_port_lookup / 3, mps_port_deregister / 3, mps_port_allocate / 2, mps_send / 2, mps_receive / 2]]).
:- comment(mps_port_deregister / 3, [
	summary:"Deregister the port registered under the given Name and Key.\n\n",
	template:"mps_port_deregister(+Key, +Name, +Signature)",
	desc:html("    The ECLiPSe message passing system incorporates a name service\n    that enables processes to associate names with their ports. Ports\n    can be registered, looked up and deregistered with the name server.\n\n<P>\n    Port owners register their ports under unique and agreed upon names \n    with the name service. Port users do a port lookup for acquiring a \n    port's identifier that is required for sending messages to the port.\n\n<P>\n    mps_port_deregister/3 removes the name server entry for the given\n    port.  Deregistration is protected by a signature that is passed to\n    the name server at registration time.  To support multiple sessions of\n    a distributed application sharing a single name server, the name\n    server predicates have a session key parameter.\n\n<P>\n"),
	args:["+Key" : "A string, eg. the name of an application session", "+Name" : "A string, eg. the name of a service", "+Signature" : "A string"],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[4 : "Key, Name or Signature is uninstatiated", 5 : "Key, Name or Signature is not a string", 176 : "Message passing system error"],
	eg:"   \n\n\n\n",
	see_also:[mps_init / 1, mps_ping / 1, mps_exit / 0, mps_port_register / 4, mps_port_lookup / 3, mps_port_allocate / 2, mps_port_deallocate / 1, mps_send / 2, mps_receive / 2]]).
:- comment(mps_port_lookup / 3, [
	summary:"Return the port identifier registered under Key and Name in the name server.\n\n",
	template:"mps_port_lookup(+Key, +Name, -Port)",
	desc:html("    The ECLiPSe message passing system incorporates a name service\n    that enables processes to associate names with their ports. Ports\n    can be registered, looked up and deregistered with the name server.\n\n<P>\n    Port owners register their ports under unique and agreed upon names \n    with the name service. Port users do a port lookup for acquiring a \n    port's identifier that is required for sending messages to the port.\n\n<P>\n    mps_port_lookup/3 is used by client processes to lookup port\n    identifiers that have been registered under agreed names.  To support\n    multiple sessions of a distributed application sharing a single name\n    server, the name server predicates have a session key parameter.\n\n<P>\n"),
	args:["+Key" : "A string, eg. the name of an application session", "+Name" : "A string, eg. the name of a service", "-Port" : "A variable"],
	resat:"   No.",
	fail_if:"   Fails if no port is registered under the given Name and Key.\n\n",
	exceptions:[4 : "Key or Name is uninstatiated", 5 : "Key or Name is not a string", 176 : "Message passing system error"],
	eg:"   \n\n\n\n\n",
	see_also:[mps_init / 1, mps_ping / 1, mps_exit / 0, mps_port_register / 4, mps_port_deregister / 3, mps_port_allocate / 2, mps_port_deallocate / 1, mps_send / 2, mps_receive / 2]]).
:- comment(mps_port_register / 4, [
	summary:"Register the port identifier under Key and Name in the name server.\n\n",
	template:"mps_port_register(+Key, +Name, +Signature, -Port)",
	desc:html("    The ECLiPSe message passing system incorporates a name service\n    that enables processes to associate names with their ports. Ports\n    can be registered, looked up and deregistered with the name server.\n\n<P>\n    Port owners register their ports under unique and agreed upon names \n    with the name service. Port users do a port lookup for acquiring a \n    port's identifier that is required for sending messages to the port.\n\n<P>\n    mps_port_register/4 registers the specified port with the name\n    server, associating it with the given Name and Key.  The signature\n    parameter is used to protect the registration from unauthorised\n    deregistration, ie.  only a deregistration with matching signature\n    will be accepted.  To support multiple sessions of a distributed\n    application sharing a single name server, the name server predicates\n    have a session key parameter.\n\n<P>\n"),
	args:["+Key" : "A string, eg. the name of an application session", "+Name" : "A string, eg. the name of a service", "+Signature" : "A string", "-Port" : "A variable"],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[4 : "Key, Name or Signature is uninstantiated", 5 : "Key, Name or Signature is not a string", 176 : "Message passing system error"],
	eg:"   \n\n\n\n",
	see_also:[mps_init / 1, mps_ping / 1, mps_exit / 0, mps_port_lookup / 3, mps_port_deregister / 3, mps_port_allocate / 2, mps_port_deallocate / 1, mps_send / 2, mps_receive / 2]]).
:- comment(mps_receive / 2, [
	summary:"Receive a message on the specified port.\n\n",
	template:"mps_receive(+Port, -Message)",
	desc:html("   ECLiPSe message passing supports asynchronous non-blocking \n   point-to-point communication in heterogeneous environments. The \n   end-points of communication are ports, i.e. messages are sent to and \n   received from ports. Messages are arbitrary Prolog terms. \n\n<P>\n   mps_receive/2 gets the next incoming message from the specified port.\n   Ports queue incoming messages, and mps_receive/2 takes them out one\n   at a time. If fails if the queue is empty.\n\n<P>\n   The port must have been allocated previously in the same process,\n   using mps_port_allocate/2..\n\n<P>\n"),
	args:["+Port" : "A port identifier (integer)", "-Message" : "A variable"],
	resat:"   No.",
	fail_if:"   Fails if no message available on the port.\n\n",
	exceptions:[4 : "Port is not instantiated", 5 : "Port is not an integer", 176 : "Message passing system error"],
	eg:"   \n% code on server\n\n    server(NsrvHost) :-\n\tmps_init(NsrvHost),\n\tmps_port_allocate(true/0, RequestPort),\n\tmps_port_register(\"SampleServer\", \"RunGoal\", \"Sig\", RequestPort),\n\tserver_loop(RequestPort).\n\n    server_loop(RequestPort) :-\n\trepeat,\n\t    mps_receive(RequestPort, request(Goal, ReplyPort)),\n\t    once(Goal),\n\t    mps_send(ReplyPort, Goal),\n\tfail.\n\n% code on client\n\n    client_init(NsrvHost, RequestPort-ReplyPort) :-\n\tmps_init(NsrvHost),\n\tmps_port_allocate(true/0, ReplyPort),\n\trepeat,\n\t    mps_port_lookup(\"SampleServer\", \"RunGoal\", RequestPort),\n\t!.\n\n    remote_once(RequestPort-ReplyPort, Goal) :-\n\tmps_send(RequestPort, request(Goal, ReplyPort)),\n\trepeat,\n\t    mps_receive(ReplyPort, Reply),\n\t!,\n\tGoal = Reply.\n\n% Sample session (client side):\n\n    [eclipse 1]:  [client].\n    client.pl  compiled traceable 624 bytes in 0.00 seconds\n    yes.\n    [eclipse 2]: client_init(breeze, Ports),\n                 remote_once(Ports, X is 3+4).\n\n    Ports = 794820612 - 364511236\n    X = 7\n    yes.\n\n\n\n\n\n",
	see_also:[mps_init / 1, mps_ping / 1, mps_exit / 0, mps_port_register / 4, mps_port_lookup / 3, mps_port_deregister / 3, mps_port_allocate / 2, mps_port_deallocate / 1, mps_send / 2]]).
:- comment(mps_send / 2, [
	summary:"Send the message Term to the specified Port.\n\n",
	template:"mps_send(+Port, +Term)",
	desc:html("   ECLiPSe message passing supports asynchronous non-blocking \n   point-to-point communication in heterogeneous environments. The \n   end-points of communication are ports, i.e. messages are sent to and \n   received from ports. Messages are arbitrary Prolog terms. \n\n<P>\n   mps_send/2 sends a message to a specified port. The port identifier\n   was usually obtained either by name server lookup, or it has\n   been communicated to the sender by an earlier message.\n\n<P>\n"),
	args:["+Port" : "A port identifier (integer).", "+Term" : "An arbitrary Prolog term."],
	resat:"   No.",
	fail_if:"   None.\n\n",
	exceptions:[4 : "Port is not instantiated.", 5 : "Port is not an integer.", 176 : "Message passing system error."],
	eg:"\n% code on server\n\n    server(NsrvHost) :-\n\tmps_init(NsrvHost),\n\tmps_port_allocate(true/0, RequestPort),\n\tmps_port_register(\"SampleServer\", \"RunGoal\", \"Sig\", RequestPort),\n\tserver_loop(RequestPort).\n\n    server_loop(RequestPort) :-\n\trepeat,\n\t    mps_receive(RequestPort, request(Goal, ReplyPort)),\n\t    once(Goal),\n\t    mps_send(ReplyPort, Goal),\n\tfail.\n\n% code on client\n\n    client_init(NsrvHost, RequestPort-ReplyPort) :-\n\tmps_init(NsrvHost),\n\tmps_port_allocate(true/0, ReplyPort),\n\trepeat,\n\t    mps_port_lookup(\"SampleServer\", \"RunGoal\", RequestPort),\n\t!.\n\n    remote_once(RequestPort-ReplyPort, Goal) :-\n\tmps_send(RequestPort, request(Goal, ReplyPort)),\n\trepeat,\n\t    mps_receive(ReplyPort, Reply),\n\t!,\n\tGoal = Reply.\n\n% Sample session (client side):\n\n    [eclipse 1]:  [client].\n    client.pl  compiled traceable 624 bytes in 0.00 seconds\n    yes.\n    [eclipse 2]: client_init(breeze, Ports),\n                 remote_once(Ports, X is 3+4).\n\n    Ports = 794820612 - 364511236\n    X = 7\n    yes.\n\n\n\n\n",
	see_also:[mps_init / 1, mps_ping / 1, mps_exit / 0, mps_port_register / 4, mps_port_lookup / 3, mps_port_deregister / 3, mps_port_allocate / 2, mps_port_deallocate / 1, mps_receive / 2]]).
