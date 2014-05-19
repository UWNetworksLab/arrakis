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
% Copyright (C) 1995 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): 	Joachim Schimpf, IC-Parc, Imperial College, London
%			Stefano Novello, IC-Parc, Imperial College, London
%			Vassilis Liatsos, IC-Parc, Imperial College, London
%			Mark Wallace, IC-Parc, Imperial College, London
%                       Andrew Sadler, IC-Parc, Imperial College, London
%                       Warwick Harvey, IC-Parc, Imperial College, London
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: ic_global.ecl,v 1.1.1.1 2006/09/23 01:53:46 snovello Exp $
%
%
% IDENTIFICATION:	ic_global.ecl
%
% AUTHORS:		Joachim Schimpf, IC-Parc, Imperial College, London
%			Stefano Novello, IC-Parc, Imperial College, London
%			Vassilis Liatsos, IC-Parc, Imperial College, London
%			Mark Wallace, IC-Parc, Imperial College, London
%                       Andrew Sadler, IC-Parc, Imperial College, London
%                       Warwick Harvey, IC-Parc, Imperial College, London
%
% Specialise the generic code of generic_global_constraints.ecl to
% create the IC global constraints library.
% ----------------------------------------------------------------------

:- module(ic_global).

:- comment(summary, "Various global constraints over lists of IC variables").
:- comment(author, "J.Schimpf, V.Liatsos, S.Novello, M.Wallace, A.Sadler, IC-Parc").
:- comment(copyright, "Cisco Systems, Inc.").
:- comment(date, "$Date: 2006/09/23 01:53:46 $").

:- use_module(ic).
:- use_module(ic_kernel).
:- use_module(ic_generic_interface).
:- import get_bounds/3 from ic_generic_interface.
:- import get_domain/2 from ic_generic_interface.

:- include(generic_global_constraints).


% For backwards compatibility...

:- reexport element/3 from ic.

:- comment(element/3, [
	summary:"Value is the Index'th element of the integer list List.",
	template:"element(?Index, ++List, ?Value)",
	args:[
	    "?Index" : "A variable or an integer.",
	    "++List" : "A non-empty list of integers.",
	    "?Value" : "A variable or an integer."
	],
	resat:"No.",
	fail_if:"Fails if Value is not the Index'th element of List.",
	desc:html("Deprecated (should never have been included in this module: it's not a
   global constraint).  Use ic:element/3 instead.
"),
	see_also:[ic:element/3]
    ]).

