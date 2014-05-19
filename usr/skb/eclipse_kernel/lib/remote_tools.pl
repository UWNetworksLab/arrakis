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
% Copyright (C) 1999-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
%
% ECLiPSe II remote development tools ECLiPSe side interface
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: remote_tools.pl,v 1.1 2008/06/30 17:43:49 jschimpf Exp $
% Authors:	Kish Shen, IC-Parc
%
%----------------------------------------------------------------------
:- module(remote_tools).
:- pragma(system).
:- lib(tracer_tcl).


:- local variable(control_stream).
:- untraceable tools/0, attach_tools/0,attached/1.
:- export attach_tools/0, attach_tools/3, tools/0, attached/1.

:- tool(attach_tools/3, attach_tools/4).

:- import set_default_error_handler/2 from sepia_kernel.


attach_tools :-
	\+attached(_),
	install_guitools,
	remote_connect(_A, ControlStream, post_attach(ControlStream)).

attach_tools(Address, Block, Goal, Module) :-
	\+attached(_),
	install_guitools,
	remote_connect_setup(Address, Con, Soc),
	block(
	   (call(Goal)@Module, 
	    remote_connect_accept(Con, Soc, Block, post_attach(Con), "", _) ->
		true
	   ;
		current_stream(Soc), close(Soc), fail
	   ), Tag,
	   ((current_stream(Soc) -> close(Soc) ; true), 
	    exit_block(Tag)
	   )
	).


post_attach(ControlStream) :-
	set_event_handler(ControlStream, disconnect_handler/0),
	setval(control_stream, ControlStream).


attached(ControlStream) :-
	getval(control_stream, ControlStream), 
	nonvar(ControlStream).

tools :- 
	(attached(ControlStream) ->
	    block(remote_yield(ControlStream), abort, 
	       (writeln(log_output, "Disconnected from remote tools"),
	        exit_block(abort)
	       )
	   )
	;   % not yet attached
	    writeln(error, "Tktools have not yet been attached."),
	    error(6, tools)
	).

disconnect_handler :-
	uninstall_guitools,
	open(queue(""), read, debug_input), % just create a dummy queue
	setval(control_stream, _),
	erase_module(tracer_tcl).

:- skipped tools/0, attach_tools/0.

%----------------------------------------------------
:- comment(summary, "Allow the Tk development tools to be used remotely").

:- comment(desc, 
"This library allows the Tk development tools to be used via the remote Tcl
interface. This allows the development tools to be used by any ECLiPSe
session, and not just one which uses the embedded Tcl interface.").


:- comment(tools/0, [
summary: "Transfer control from ECLiPSe to the development tools.",
desc: html("\
<P>
	Transfer the `control' from ECLiPSe to the development tools,
	allowing the user to use the development tools GUI interactively.
	This predicate returns when control is returned to ECLiPSe.
<P>
        Note that some features of the development tools can be triggered
        without interacting directly with the GUI. For example, placing a
        spy-point on a predicate will cause the tracer tool to pop-up when
        the predicate is called.
"),
see_also: [attach_tools/0,attach_tools/3],
exceptions: [6 : "tools/0 called before development tools have been attached."],
resat: no
]).

:- comment(attach_tools/0, [
summary: "Initiate the attachment of remote development tools",
desc:   html("\
   <P>Initiate the attachment of the remote development tools. This will create
   a connection waiting for the remote development tools to be attached to.
   It will print the hostname and the port number that should be given to
   the development tools. The remote development tools should be started 
   and supplied with the hostname and port number. Once this is done, the
   development tools are ready to be used. Control is initially given back
   to ECLiPSe when the tools have been attached."),
resat: no,
fail_if: "Development tools already attached.",
see_also: [tools/0, attach_tools/3]
]).

:- comment(attach_tools/3, [
summary: "A flexible attachment of remote development tools",
amode: attach_tools(?,++,?),
args: ["Address":  "Address for the remote connection (Host/Port or variable)",
       "TimeOut":  "Time-out interval (int/float or the atom block)",
       "Goal"   :  "Goal to execute during connection"
      ],
desc:   html("\

   <P>Initiate the attachment of the remote development tools, providing the
   user with more control over the connection than attach_tools/0. The
   predicate will create a connection waiting for the remote development
   tools to be attached to.  It will then execute Goal and then tries to
   complete the connection by waiting at most TimeOut seconds for the
   remote development tools to connect.  If TimeOut is the atom block, then
   it will wait indefinitely.

   </P><P>Address is the Host/Port address that the remote connection will be
   made.  This can be left as a variable, so that the system can determine
   its own address, or the user can specify a specific Port. The main
   purpose for Goal is to allow the user to start out the remote
   development tools from it using Address, so that the user does not need
   to manually connect the remote development tools. Control is initially
   given back to ECLiPSe when the tools have been attached.

   </P><P>The predicate will fail or throw an exception if Goal respectively fails
   or throw an exception. It will also fail if it waits more than TimeOut
   seconds for the remote tools to connect. In these cases, the server
   socket will be properly closed.

   </P><P>attach_tools/0 can be implemented using attach_tools/3 as follows:

   <PRE>
         attach_tools :-
             attach_tools(_, block, true).
   </PRE>
   "),
eg: "
    % the following will cause the remote tools to start automatically
    % if tktools can be run from a shell. It will wait 10 seconds before
    % failing and closing the server socket.
    [eclipse 2]:  attach_tools(H/P,10,
         exec(['tktools','--','-h',H,'-p',P], [], Pid)).

    H = 'cow.icparc.ic.ac.uk'
    P = 1953
    Pid = 27678
    Yes (0.04s cpu)
    [eclipse 3]:
   ",
resat: no,
exceptions: [
    4: "Goal or TimeOut are variables",
    5: "TimeOut is not an integer nor float nor atom",
    6: "TimeOut is either too small or large or is an atom other than block"
   ],
fail_if: "   Development tools already attached, or waiting TimeOut seconds  
   for remote tools to connect, or if Goal fails",
see_also: [tools/0, attach_tools/0]
]).

:- comment(attached/1, [
summary: "Checks if remote tools are currently attached or not.",
amode: attached(?),
args: ["ControlStream": "control stream (stream name or variable)"],
fail_if: "Remote tools are not currently attached.",
desc: html("\
<P>
	Checks if the remote development tools have been attached to this 
        ECLiPSe session or not. If attached, the predicate succeeds and 
        returns the stream name of the control stream. If not attached, the
        predicate fails."
)]).





