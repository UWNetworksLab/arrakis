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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf and Andrew Sadler, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: ic_edge_finder3.ecl,v 1.1.1.1 2006/09/23 01:53:46 snovello Exp $
%
% Description:		IC Edge-finder, quadratic algorithm
%
% Author:		J.Schimpf, IC-Parc
%                       A.Sadler, IC-Parc
%
% The ordering booleans: There is an array of booleans with
% one boolean for each pair of tasks. The mapping is as follows:
%
%	Pos in array		Pair of tasks
%
%	Bools[j*(j-1)//2+i+1]	i	j
% e.g.
%	Bools[1]		0	1
%	Bools[2]		0	2
%	Bools[3]		1	2
%	Bools[4]		0	3
%	Bools[5]		1	3
%	Bools[6]		2	3
%
%	Bools[idx] = 0	  <=>	task_i before task_j
%	Bools[idx] = 1	  <=>	task_i after  task_j
%
% Specialise the generic code of generic_edge_finder3.ecl to
% create the IC edge finder3 library.
% ----------------------------------------------------------------------

:- module(ic_edge_finder3).

:- comment(summary, "Cubic edge-finder algorithm for disjunctive and
cumulative constraints for IC").
:- comment(author, "Joachim Schimpf").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(date, "$Date: 2006/09/23 01:53:46 $").
:- comment(desc, "\

    This library implements the cubic edge-finder algorithm for the
    disjunctive and cumulative scheduling constraints for the IC solver.
    It provides the strongest propagation of the three libraries for
    cumulative constraints: ic_cumulative, ic_edge_finder and
    ic_edge_finder3.  It is also computationally the most expensive.

    Note that the same predicates are implemented in both ic_edge_finder
    and ic_edge_finder3 libraries.
").

:- use_module(ic_edge_finder_common).
:- ensure_loaded(ic_cumulative).

:- include('generic_edge_finder3.ecl').

generic_cumulative(Starts, Durations, Resources, Cap) :-
        % Currently the implementation of cumulative requires all
        % variables to have domains
	( foreach(S,Starts) do
		  ic:(S :: -10000000..10000000)
	),
	ic_cumulative:cumulative(Starts, Durations, Resources, Cap).


