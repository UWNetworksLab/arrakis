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
% Version:	$Id: apply.pl,v 1.1 2008/06/30 17:43:41 jschimpf Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% IDENTIFICATION:       apply.pl
%
% AUTHOR:               Joachim Schimpf
%
% CONTENTS:             apply/2,3
%

:- module(apply).
:- export apply/2.
:- export apply_/3.

:- export syntax_option(var_functor_is_apply).

:- comment(summary, "The apply/2 higher-order predicate").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(author, "Joachim Schimpf, ECRC Munich").
:- comment(date, "$Date: 2008/06/30 17:43:41 $").
:- comment(desc, html("
    This library defines the apply/2 predicate which constructs a goal
    from a term and a list of additional arguments:
    <PRE>
    	?- P=plus(1), apply(P, [3,X]).
	P = plus(1)
	X = 4
	Yes (0.00s cpu)
    </PRE>
    Loading this library also enables the syntax option var_functor_is_apply.
    This means that it is allowed to write terms with variables functors,
    which will be parsed as apply/2 terms which can the be executed.
    The above example can thus be written as:
    <PRE>
    	?- P=plus(1), P(3,X).
	P = plus(1)
	X = 4
	Yes (0.00s cpu)
    </PRE>
")).

:- comment(apply/2, [
    summary:"The apply/2 higher-order predicate",
    args:[
	"Term":"An atom or compound term",
	"Args":"A list of arbitrary terms"
    ],
    desc:html("
    Invokes a goal that is formed by appending the elements of the
    list Args as additional arguments to Term.
    "),
    eg:"
    % The following three examples all invoke the goal plus(1,2,X):

    ?- apply(plus, [1,2,X]).
    X = 3

    ?- apply(plus(1), [2,X]).
    X = 3

    ?- apply(plus(1,2), [X]).
    X = 3


    % Error:

    ?- apply(plus(1),[2]).
    calling an undefined procedure plus(1, 2) in module eclipse
    Abort
    "
]).

:- tool(apply/2, apply_/3).

apply_(Term, AddArgs, Module) :- atom(Term), !,
	Goal =.. [Term|AddArgs],
	call(Goal)@Module.
apply_(Term, AddArgs, Module) :-
	Term =.. List,
	append(List, AddArgs, NewList),
	Goal =.. NewList,
	call(Goal)@Module.

