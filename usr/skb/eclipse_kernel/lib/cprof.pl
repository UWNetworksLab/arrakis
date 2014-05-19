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
% Copyright (C) 1992-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: cprof.pl,v 1.1 2008/06/30 17:43:44 jschimpf Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	cprof.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		profile(+N, +Goal)
%			prof_on
%			prof_off
%
% DESCRIPTION:		for use with a profiled sepia
%

:- module(cprof).

:- export cprof/2, cprof/3, cprof_on/0, cprof_off/0.

:- import call_c/2 from sepia_kernel.

:- tool(cprof/2, cprof/3).

cprof(N, Goal, Mod) :-
	cprof_on,
	n_times(N),
	call(Goal, Mod),
	fail.
cprof(_, _, _) :-
	cprof_off.

n_times(0) :- !, fail.
n_times(_).
n_times(N) :-
	-(N, 1, N1),
	n_times(N1).

cprof_on  :- call_c(moncontrol(1),_).

cprof_off :- call_c(moncontrol(0),_).
