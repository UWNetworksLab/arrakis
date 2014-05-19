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
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: structures.pl,v 1.1 2008/06/30 17:43:49 jschimpf Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	structures.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:
%	predicates:	define_struct(+Template)
%			erase_struct(+StructName)
%			portray_struct(+Stream, +Struct)
%
%	macros:		?Struct with ?Fields
%			+FieldName of +StructName
%
% DESCRIPTION:
%
%	The useful core of this library has been moved to the kernel!
%	This file is just for backward compatibility.
%

:- module(structures).
:- export
	define_struct/1,
	erase_struct/1,
	eq_struct/2,
	portray_struct/2.

:- export op(650, xfx, [eq_struct]).

:- pragma(nodebug).


:- tool(define_struct/1, define_struct/2).
define_struct(Struct, Module) :-
	printf(warning_output,
	    "WARNING: define_struct/1 is obsolete, use export struct or local struct",
	    []),
	export(struct(Struct))@Module.


erase_struct(_Struct) :-
	printf(warning_output, "WARNING: erase_struct/1 is obsolete, ignored", []).

eq_struct(_, _) :-
	printf(warning_output, "WARNING: eq_struct/2 is obsolete", []),
	abort.


:- tool(portray_struct/2, portray_struct/3).
portray_struct(Stream, Struct, Module) :-
	functor(Struct, Functor, Arity),
	functor(Def, Functor, Arity),
	current_struct(Functor, Def)@Module,
	make_list(Def, Struct, Arity, [], List),
	print(Stream, no_macro_expansion(Functor with List)).

make_list(_Struct, _Template, 0, List, List) :- !.
make_list(Struct, Template, N, List0, List) :-
	arg(N, Struct, FieldName),
	arg(N, Template, FieldValue),
	N1 is N-1,
	make_list(Struct, Template, N1, [FieldName:FieldValue|List0], List).

