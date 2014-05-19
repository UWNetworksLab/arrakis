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
% Copyright (C) 2000 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% 
% Solver for constraints over finite sets of integers (ic wrapper)
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc
% Version:	$Id: ic_sets.ecl,v 1.1.1.1 2006/09/23 01:53:48 snovello Exp $
%
% ----------------------------------------------------------------------

:- module(ic_sets).

:- lib(ic).
:- lib(ic_kernel).

tr_generic_sets(solver_module, ic).
tr_generic_sets(sbds_module, ic_sbds).

:- local macro(solver_module, tr_generic_sets/2, []).
:- local macro(sbds_module, tr_generic_sets/2, []).

:- include(generic_sets).

% ----------------------------------------------------------------------

:- comment(summary, "Solver over sets of integers (cooperates with lib(ic))").
:- comment(eg, "
% Example program: Steiner triplets
% Compute NB triplets of numbers from 1 to N such that
% any two triplets have at most one element in common.
% Try steiner(9,Sets).

:- lib(ic_sets).
:- lib(ic).

steiner(N, Sets) :-
	NB is N * (N-1) // 6,		% compute number of triplets
	intsets(Sets, NB, 1, N),	% initialise the set variables
	( foreach(S,Sets) do
	    #(S,3)			% constrain their cardinality
	),
	( fromto(Sets,[S1|Ss],Ss,[]) do
	    ( foreach(S2,Ss), param(S1) do
		#(S1 /\\ S2, C),		% constrain the cardinality
		C #=< 1			% of pairwise intersections
	    )
	),
        label_sets(Sets).		% search

label_sets([]).
label_sets([S|Ss]) :-
        insetdomain(S,_,_,_),
	label_sets(Ss).
").
