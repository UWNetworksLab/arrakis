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
% Copyright (C) 1989-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: array.pl,v 1.1 2008/06/30 17:43:42 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	array.pl
 *
 * DESCRIPTION: 	
 *
 *
 * CONTENTS:     
 *
 */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_module(sepia_kernel).
:- system.		% compiler directive to add the SYSTEM flag

:- export
	current_array/2,
	current_store/1,
	make_array/1,
	make_array/2,
	make_local_array/1,
	make_local_array/2.

:-
	tool(current_array/2, current_array_body/3),
	tool(current_store/1, current_store_/2),
	tool(make_array/1, make_array_body/2),
	tool(make_array/2, make_array_body/3),
	tool(make_local_array/1, make_local_array_body/2),
	tool(make_local_array/2, make_local_array_body/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_array_body(Array, Module) :-
	make_array_body(Array, prolog, Module).

make_local_array_body(Array, Module) :-
	make_local_array_body(Array, prolog, Module).

make_array_body(Array, Type, Module) :-
	(make_array_(Array, Type, global, Module) ->
	    true
	;
	    bip_error(make_array(Array, Type), Module)
	).

make_local_array_body(Array, Type, Module) :-
	(make_array_(Array, Type, local, Module) ->
	    true
	;
	    bip_error(make_local_array(Array, Type), Module)
	).


current_array_body(Array, OptionList, Module) :-
	var(Array),
	check_output_list(OptionList),
	!,
	current_functor(Atom, Dim, 1, 0),	% with properties only
	functor(Array, Atom, Dim),
	array_info(Array, OptionList, Module).
current_array_body(Array, OptionList, Module) :-
	(atom(Array) ; compound(Array)),
	check_output_list(OptionList),
	!,
	array_info(Array, OptionList, Module).
current_array_body(Array, OptionList, Module) :-
	error(5, current_array(Array, OptionList), Module).

check_output_list(X) :- var(X), !.
check_output_list([]).
check_output_list([_|_]).



current_store_(Store, Module) :-
	var(Store),
	!,
	current_functor(Atom, Arity, 1, 0),	% with properties only
	functor(Store, Atom, Arity),
	is_store_(Store, Module).
current_store_(Store, Module) :-
	(atom(Store) ; compound(Store)),
	!,
	is_store_(Store, Module).
current_store_(Store, Module) :-
	error(5, current_store(Store), Module).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- skipped
	current_array/2,
	make_array/1,
	make_array/2,
	make_local_array/1,
	make_local_array/2.
