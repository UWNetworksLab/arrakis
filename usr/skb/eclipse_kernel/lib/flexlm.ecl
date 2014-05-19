%
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
% Dummy module, just for documentation
%

:- module(flexlm).

:- reexport
	licence_checkout/6,
	licence_heartbeat/4,
	licence_checkin/1,
	licence_held/1
    from sepia_kernel.

:- comment(summary, "Interface to Flexlm licence manager").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:45 $").
:- comment(desc, html("\
	The predicates described here are actually implemented in
	in the sepia_kernel module and can be called without loading
	the flexlm library.
    ")).

:- comment(licence_checkout/6, [
    summary:"Check out a licenced feature from FlexLM",
    amode:licence_checkout(+,++,+,+,-,-),
    args:[
	"Feature":"String or atom",
	"Policy":"List of atoms",
	"Version":"String",
	"Path":"String, possibly empty",
	"Message":"Returns error/warning message, if any",
	"Status":"Returns one of the atoms ok, warning, error"
    ],
    fail_if:"None",
    desc:html("
	This maps directly to lp_checkout() of the FlexLM simple API.
<DL>
	<DT>Feature<DD>
	is the name of the feature to be checked out, passed directly
	to FlexLM.

	<DT>Policy<DD>
	is a list of atoms, containing one of
	<UL>
	<LI><EM>restrictive</EM> (the default), <EM>queue</EM>,
		<EM>failsafe</EM>, <EM>lenient</EM>
	</UL>
	and possibly one or more of
	<UL>
	<LI><EM>retry_restrictive</EM>
	<LI><EM>check_baddate</EM>
	<LI><EM>flexlock</EM>
	</UL>
	Note that LM_MANUAL_HEARTBEAT will always be set.  An empty
	list defaults to LM_RESTRICTIVE|LM_MANUAL_HEARTBEAT.

	<DT>Version<DD>
	is a version string, passed directly to FlexLM.

	<DT>Path<DD>
	is the name of the licence file.  If the empty string is
	given, it defaults to \"licence.dat\" in the ECLiPSe top-level
	installation directory.

	The lp_handle is not visible and stored internally by ECLiPSe. 
	The feature name is used to identify the checked out licence
	on the Eclipse level.

	<DT>Status<DD>
	will be bound to one of the atoms <EM>ok</EM>,<EM>warning</EM>,
	<EM>error</EM>

	<DT>Message<DD>
	will be bound to an error or warning message string in case
	the Status is not <EM>ok</EM>
</DL>
	Multiple checkouts of the same feature are allowed, but only if
	they are for the same Policy and Version. 
    "),
    see_also:[get_flag/2,licence_heartbeat/4,licence_checkin/1,licence_held/1],
    eg:"
    ?- sepia_kernel:licence_checkout(swapper, [restrictive], \"2.0\", \"\", Msg, Status),
    	( Status = error ->
	    writeln(error, Msg),
	    abort
	;
	    writeln(warning_output, Msg)
	;
	    true
	).
    "]).

:- comment(licence_heartbeat/4, [
    summary:"Refresh a checked out FlexLM licence",
    amode:licence_heartbeat(+,+,-,-),
    args:[
	"Feature":"String or atom",
	"Minutes":"Integer",
	"Reconnects":"Returns count of reconnects within last Minutes minutes",
	"FailedReconnects":"Returns 0 if ok, otherwise count of failed reconnects"
    ],
    fail_if:"Fails if the given feature has not been checked out",
    desc:html("
	This maps directly to lp_heartbeat() of the FlexLM simple API.
	<P>
	An application should call licence_heartbeat/4 occasionally (e.g.
	every few minutes) in order to make sure that the licence server
	is still running. When everything is ok, zero is returned in the
	last argument, otherwise the number of failed connection attempts.
	<P>
	The second and third argument allow to check whether the licence
	server has been shut down and restarted. If that happens, the
	heartbeat call will reconnect to the server. If many such
	reconnects happen in a short amount of time, this is suspicious
	and may indicate an attempt to illegally obtain more licences.
	<P>
	If a feature had been checked out multiply, a heartbeat is performed
	for every checked out licence, and the reconnection counts are
	added up.
    "),
    see_also:[licence_checkout/6,licence_checkin/1,licence_held/1],
    eg:"
	...,
    	( sepia_kernel:licence_heartbeat(swapper, 10, R, 0) ->
	    ( R > 3 ->
		writeln(error, \"Licence server suspicious\"),
		abort
	    ;
		true
	    )
	;
	    writeln(error, \"Licence server not responding\"),
	    abort
	),
	...
    "]).

:- comment(licence_checkin/1, [
    summary:"Check in a FlexLM licence",
    amode:licence_checkin(+),
    args:["Feature":"String or atom"],
    fail_if:"None",
    desc:html("
	This maps to lp_checkin() of the FlexLM simple API.
	<P>
	If called with a feature name that does not exist or has not
	been checked out, the predicate silently succeeds.
	<P>
	If the given feature has been checked out multiply, make sure
	you do as many calls to licence_checkin/1 as you have done to
	licence_checkout/6.
    "),
    see_also:[licence_checkout/6,licence_heartbeat/4,licence_held/1],
    eg:"
	...,
    	sepia_kernel:licence_checkin(swapper),
	...
    "]).

:- comment(licence_held/1, [
    summary:"Check whether we already have a licence for a particular feature",
    amode:licence_held(+),
    args:["Feature":"String or atom"],
    fail_if:"Licence hasn't been checked out",
    desc:html("
	This checks whether a licence for the given feature has already
	been checked out by this process, i.e. whether licence_checkout/6
	has been called successfully earlier.
    "),
    see_also:[licence_checkout/6,licence_heartbeat/4,licence_checkin/1],
    eg:"
	...,
    	( sepia_kernel:licence_held(swapper) ->
	    true
	;
	    sepia_kernel:licence_checkout(swapper, ..., ..., ..., ..., ...)
	    ...
	),
	...
    "]).

