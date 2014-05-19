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
% Copyright (C) 1997 - 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf and Andrew Sadler, IC-Parc
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: ic_edge_finder_common.ecl,v 1.1.1.1 2006/09/23 01:53:46 snovello Exp $
%
% Description:		IC specific Prolog toplevel for Edge-finder in C
%
% Author:		J.Schimpf, IC-Parc
%                       A.Sadler, IC-Parc
%
% Specialise the generic code of generic_edge_finder_common.ecl to
% create the IC edge_finder library.
% ----------------------------------------------------------------------

:- module(ic_edge_finder_common).

:- use_module(ic).
:- use_module(ic_kernel).
:- use_module(ic_generic_interface).

:- include(generic_edge_finder_common).

