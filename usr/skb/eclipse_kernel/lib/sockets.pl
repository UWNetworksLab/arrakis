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
% Copyright (C) 1998-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: sockets.pl,v 1.1 2008/06/30 17:43:49 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * IDENTIFICATION:	sockets.pl
 *
 * DESCRIPTION: 	SICStus Prolog compatibility package
 *
 *
 * CONTENTS:     
 *
 * AUTHOR:               Kish Shen
 *
 * DATE:                24 Sept. 1998
 */

:- module(sockets).

:- comment(summary, "Sicstus compatible sockets interface").
:- comment(author, "Kish Shen, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:49 $").
:- comment(desc, html("
    Note that ECLiPSe provides its own built-in socket manipulation
    predicates which provides similar functionality to this library.
    <P>
    This library is only provided for compatibility with the socket
    manipulation predicates of SICStus Prolog. To use these predicates,
    the sockets library has to be loaded: 
    <PRE>
    :- use_module(library(sockets)).
    </PRE>
    For SICStus 3.0, the sockets predicates are also in a sockets library,
    so no changes are needed to load the library.  However, for older
    versions of SICStus, the predicates are available as built-ins, and no
    library has to be loaded.  So if the code is written for older
    versions of SICStus, then the above line has to be added. 
    <P>
    The sockets library can be used independently of the sicstus library. 
    ")).
:- comment(socket/2, [template:"socket(+Domain, -Socket)",
    summary:"Create a socket",
    see_also:[socket/3]]).
:- comment(socket_bind/2, [template:"socket_bind(+Socket, +Address)",
    summary:"Bind a socket to an address",
    see_also:[bind/2]]).
:- comment(socket_connect/3, [template:"socket_connect(+Socket, +Address, -Stream)",
    summary:"Connect a socket to an address",
    see_also:[connect/2]]).
:- comment(socket_listen/2, [template:"socket_listen(+Socket, +Length)",
    summary:"Limit the maximum of pending connections",
    see_also:[listen/2]]).
:- comment(socket_accept/2, [template:"socket_accept(+Socket, -Stream)",
    summary:"Extract the first connection to socket",
    see_also:[accept/3]]).
:- comment(socket_select/5, [template:"socket_select(+Socket, -NewStream, +TimeOut0, +Streams, -ReadStreams)",
    summary:"Wait for new connection on Socket, and for data on Streams",
    see_also:[select/3,stream_select/3]]).
:- comment(current_host/1, [template:"current_host(?HostName)",
    summary:"Get the host machine name",
    see_also:[get_flag/2]]).
:- comment(stream_select/3, [template:"stream_select(+Streams, +TimeOut0, -ReadStreams)",
    summary:"Wait for data on Streams",
    see_also:[select/3]]).

:- export
      socket/2,
      socket_bind/2,
      socket_connect/3,
      socket_listen/2,
      socket_accept/2,
      socket_select/5,
      current_host/1,
      stream_select/3.



socket(Domain, Socket) :-
    (Domain == 'AF_INET' -> Dom = internet ;
     Domain == 'AF_UNIX' -> Dom = unix ; error(5, socket(Domain,Socket))
    ),
    sepia_kernel:socket(Dom, stream, Socket).

socket_bind(Socket, Address) :-
    translate_address(Address, Name, socket_bind(Socket,Address)),
    sepia_kernel:bind(Socket, Name).

socket_connect(Socket, Address, Stream) :-
    translate_address(Address, Name, socket_connect(Socket,Address,Stream)),
    Socket = Stream,
    sepia_kernel:connect(Socket, Name).

socket_listen(Socket, Length) :-
    sepia_kernel:listen(Socket, Length).

socket_accept(Socket, Stream) :- 
    sepia_kernel: accept(Socket, _, Stream).

socket_select(Socket, NewStream, TimeOut0, Streams, ReadStreams) :-
   translate_timeout(TimeOut0, TimeOut, socket_select(Socket,NewStream,TimeOut0,Streams,ReadStreams)),
   sepia_kernel: select([Socket|Streams], TimeOut, ReadStreams0),
   (delete(Socket, ReadStreams0, ReadStreams) ->
       sepia_kernel: accept(Socket, _, NewStream) ; ReadStreams = ReadStreams0
   ).

stream_select(Streams, TimeOut0, ReadStreams) :-
    translate_timeout(TimeOut0, TimeOut, stream_select(Streams,TimeOut0,ReadStreams)),
    sepia_kernel: select(Streams, TimeOut, ReadStreams).

current_host(HostName) :-
	get_flag(hostname, Shostname),
	atom_string(HostName, Shostname).


translate_timeout(S:MS, TimeOut, _Culprit) ?-
    integer(S),
    integer(MS),
    S > 0, MS > 0, !,
    TimeOut is S + MS/1000000.
translate_timeout(off, TimeOut, _) ?- !,
    TimeOut = block.
translate_timeout(_, _, Culprit) :-
    error(5,Culprit).

translate_address('AF_UNIX'(Name0), Name, _Culprit) ?-
    atom(Name0), !, Name = Name0.
translate_address('AF_INET'(Host,Port), Name, Culprit) ?-
    (Culprit = socket_bind(_,_) -> true ; 
	/* assume to be socket_connect */
        nonvar(Host), nonvar(Port)
    ),
    !, Name = Host/Port.
translate_address(_, _, Culprit) :-
    error(5, Culprit).




