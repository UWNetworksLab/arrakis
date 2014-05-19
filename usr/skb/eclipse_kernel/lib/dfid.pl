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
% Copyright (C) 1990-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: dfid.pl,v 1.1 2008/06/30 17:43:45 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 * IDENTIFICATION:	dfid.pl 
 *
 * sccsid("%W%          %E%").
 */

/*
 * Bounded depth-first search rule for Sepia.
 * This library file allows to use two  search rules with limited depth:
 *
 *	1) Depth-first iterative deepening.
 *		The whole proof tree is expanded up to a given depth.
 *		If some branches were cut due to the depth limit,
 *		the search is repeated for a depth incremented by
 *		one or a given increment. Solutions which are repeatedly
 *		found are eliminated, and so the effect is that of the
 *		breadth-first search. It is slightly slower than breadth
 *		first search, but it uses much less memory.
 *		Available predicates:
 *
 *		    dfid(Goal, StartDepth, MaxDepth, Increment)
 *			call Goal using the dfid rule, starting at the
 *			depth StartDepth, up to the depth MaxDepth,
 *			with depth being incremented by Increment.
 *
 *		    dfid(Goal)
 *			identical to dfid(Goal, 0, maxint, 1)
 *
 *	2) Depth-first search with limited depth. This is a special case
 *		of the above, where search is performed only up to a certain
 *		depth and not repeated. It is slightly more efficient than using
 *		the general DFID rule.
 *		Available predicate:
 *
 *		    call_depth(Goal, DepthLimit)
 *
 * The depth is defined as the number of ancestors which are not facts
 * (or rules which have only simple subgoals). This differs slightly
 * from the TP use of DFID, where it aims at finding the minimal solution
 * in terms of number of invoked goals. In LP it makes more sense to
 * execute finite branches even if they exceed the depth.
 *
 *	Author: Micha Meier, 1991
 */

% Module is created in C without any imports...
:- import(sepia_kernel)@dfid.

:- begin_module(dfid).

:- import
	get_cut/1
    from sepia_kernel.

:- export
	call_depth/2,
	dfid/1,
	dfid/4.

/*
 * This is done in C code
:-
    make_local_array(depth, global_reference),
    make_local_array(max_depth, global_reference),
    make_local_array(depth_limit, global_reference),
    make_local_array(depth_ov, global_reference).
 */

:-  tool(dfid/1, dfid/2),
    tool(dfid/4, dfid/5),
    tool(call_depth/2, call_depth/3).

:-
    make_local_array(flags),
    get_flag(dfid_compile, DFID),
    setval(flags, DFID),
    set_flag(dfid_compile, off).

dfid(G, M) :-
    dfid(G, 0, 16'7fffffff, 1, M).

dfid(G, From, To, Increment, M) :-
    % Save current values
    parms_in(From, To, Depth, MaxDepth, Limit, OV),
    % Initialize parameters
    setval(max_depth, 0),
    setval(depth_ov, 0),
    dfid_ex(G, From, From, To, Increment, M),
    % Restore original values
    parms_out(Depth, MaxDepth, Limit, OV).

call_depth(G, DepthLimit, M) :-
    parms_in(0, DepthLimit, Depth, MaxDepth, Limit, OV),
    % Initialize parameters
    setval(max_depth, DepthLimit),
    setval(depth_limit, DepthLimit),
    call(G, M),
    % Restore original values
    parms_out(Depth, MaxDepth, Limit, OV).

dfid_ex(G, From, Limit, _, _, M) :-
    setval(depth_limit, Limit),
    call(G, M),
    new_solution(From, Limit).
dfid_ex(G, From, Limit, MaxLimit, Increment, M) :-
    increment_depth(Limit, Increment, NewLimit, MaxLimit),
    dfid_ex(G, From, NewLimit, MaxLimit, Increment, M).

parms_in(From, To, Depth, MaxDepth, Limit, OV) :-
    From =< To,
    % Save current values for nested calls
    getval(depth, Depth),
    getval(max_depth, MaxDepth),
    getval(depth_limit, Limit),
    getval(depth_ov, OV),
    setval(depth, 0).

parms_out(Depth, MaxDepth, Limit, OV) :-
    setval(depth, Depth),
    setval(max_depth, MaxDepth),
    setval(depth_limit, Limit),
    setval(depth_ov, OV).

new_solution(From, Limit) :-
    getval(max_depth, MaxDepth),
    (Limit = From ->
	true
    ;
	MaxDepth = Limit		% filter out previous solutions
    ).

increment_depth(Limit, Increment, NewLimit, MaxLimit) :-
    getval(depth_ov, OV),
    OV \== 0,			% there was a cut branch
    NewLimit is Limit + Increment,
    NewLimit =< MaxLimit,
    setval(depth, 0),
    setval(depth_ov, 0).

:- skipped
    increment_depth/4,
    new_solution/2,
    parms_in/6,
    parms_out/4.

:- untraceable
    call_depth/3,
    dfid/2,
    dfid/5,
    dfid_ex/6,
%   increment_depth/4,		% commented out too see the increment
    new_solution/2,
    parms_in/6,
    parms_out/4.

:-  getval(flags, DFID),
%    set_flag(dfid_compile, DFID),
    set_flag(dfid_compile, on),
    erase_array(flags).

