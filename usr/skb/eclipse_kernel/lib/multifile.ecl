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
% Copyright (C) 2008 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf, Coninfer Ltd
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: multifile.ecl,v 1.1 2008/07/20 18:16:51 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(multifile).

:- comment(summary, "Multifile declaration, for Prolog compatibility").
:- comment(author, "Joachim Schimpf, Coninfer Ltd").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/07/20 18:16:51 $").
:- comment(desc, html("
	This library implements the multifile/1 declaration, and allows clauses
	for multifile predicates to be prefixed with their module name.
")).


:- export multifile/1.
:- tool(multifile/1, multifile_/2).
multifile_(P, Module) :- var(P), !,
	error(4, multifile(P), Module).
multifile_((P1,P2), Module) :- !,
	multifile_(P1, Module),
	multifile_(P2, Module).
multifile_([], _Module) :- !.
multifile_([P|Ps], Module) :- !,
	multifile_(P, Module),
	multifile_(Ps, Module).
multifile_(M:P, _Module) :- !,
	multifile_(P, M).
multifile_(N/A, Module) :- !,
	( current_module(Module) ->
	    true
	;
	    % to have a different language dialect, create the module explicitly beforehand
	    create_module(Module)
	),
	dynamic(N/A)@Module,
	export(N/A)@Module.
multifile_(P, Module) :-
	error(5, multifile(P), Module).


% Support Quintus-style "qualified clauses" (for dynamic/multifile predicates only)

:- export macro((:)/2, t_colon_clause/3, [clause]).

t_colon_clause(((Module:Head):-Body), NewClause, CM) ?-
	t_colon_clause((Module:(Head:-Body)), NewClause, CM).
t_colon_clause(Module:Clause, NewClause, CM) :-
	( Module == CM ->
	    NewClause = Clause
	;
	    NewClause = [],
	    assert(Clause)@Module
	).

