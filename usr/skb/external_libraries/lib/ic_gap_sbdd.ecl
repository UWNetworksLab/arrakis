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
% Contributor(s): 
% 
% END LICENSE BLOCK
%
% Version of the GAP-SBDD library specialised for IC.
%
% See generic_gap_sbdd.ecl for details.
%

:- module(ic_gap_sbdd).

:- lib(ic).
:- lib(sym_expr).

:- lib(ic_generic_interface).
:- import get_bounds/3 from ic.

trans_var_to_dom_list(var_to_dom_list(Var, Dom), get_domain_as_list(Var, Dom)).
:- local macro(var_to_dom_list/2, trans_var_to_dom_list/2, []).

trans_module(set_module, ic_sets).
:- local macro(set_module/0, trans_module/2, []).

:- include(generic_gap_sbdd).
 
