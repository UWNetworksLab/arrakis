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
% Copyright (C) 1991-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: http_method.pl,v 1.1 2008/06/30 17:43:46 jschimpf Exp $
% ----------------------------------------------------------------------


/*
$Id: http_method.pl,v 1.1 2008/06/30 17:43:46 jschimpf Exp $
*/

:- module(http_method).

:- export
        http_method/6.


/* 
rpc call:
executes the method on the object and return:
- the output of the method (possibly empty)
- a status code for the response status line
- a list of http parameters (in particular the length of the object body).

*/


http_method(_,_,_,_,_,_):-
	http_method(_,_,_,_,_,_).
http_method(Method, Url, _ObjectBody, Output, 200, [contentLength(CL)]):-
	concat_string(["You asked for method ", Method, 
	"on object ", Url], Output),
	string_length(Output, CL).
	

	
