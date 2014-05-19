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
% Copyright (C) 1993-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: rationals.pl,v 1.1 2008/06/30 17:43:49 jschimpf Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	rationals.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		macro definition (/)/2
%
% DESCRIPTION:		Parse <int>/<int> as a rational number
%			Switch on the prefer_rationals option
%

:- module(rationals).

:- export
	tr_rat_in/2,
	tr_rat_out/2.

:- set_flag(prefer_rationals, on).


% parse N/D as a rational if N and D are integers or rationals

tr_rat_in(N/D, Rat) :-
	( integer(N) ; rational(N)),
	( integer(D) ; rational(D)),
	Rat is N/D.

:- export macro((/)/2, tr_rat_in/2, []).


% print rationals as (/)/2 structure or integer

tr_rat_out(Rat, Out) :-
	N is numerator(Rat),
	D is denominator(Rat),
	( D == 1 ->
		Out = N
	;
		Out = N/D
	).

:- export macro(type(rational), tr_rat_out/2, [write]).
