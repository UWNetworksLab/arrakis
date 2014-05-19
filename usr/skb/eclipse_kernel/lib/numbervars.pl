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
% Version:	$Id: numbervars.pl,v 1.1.2.1 2008/12/17 09:43:54 jschimpf Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% IDENTIFICATION:       numbervars.pl
%
% AUTHOR:               Joachim Schimpf
%
% CONTENTS:             numbervars/3
%

:- module(numbervars).

:- comment(summary, "C-Prolog style numbervars predicate").
:- comment(author, "Joachim Schimpf, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/12/17 09:43:54 $").
:- comment(desc, html("
    Implements the numbervars(Term, From, To) predicate of C-Prolog.  Term
    is any term, From and To are integer numbers.  All variables in Term
    are instantiated to terms of the form
    <PRE>
	$VAR(N) 
    </PRE>
    where N is an integer number.  The first encountered variable will be
    coded by the number From, on exit To is instantiated to the next
    unused number. 
    <P>
    This predicate can thus be used to encode nonground term using a
    ground representation.  Note that metaterms can be used for the same
    purpose, but their use is both more efficient and more general,
    because the variables are not actually instantiated and so they can be
    used again as variables when needed. 
    ")).

:- export numbervars/3.
:- export syntax_option('$VAR').	% to print $VAR/1 terms as letters


numbervars('$VAR'(N), N, N1) :- !,
	N1 is N + 1.
numbervars(Term, N, Next) :-
	arity(Term, Arity),
	numbervars(0, Arity, Term, N, Next).

numbervars(Arity, Arity, _, N, Next) :- !, N = Next.
numbervars(I, Arity, Term, N0, N) :-
	I1 is I + 1,
        arg(I1, Term, Arg),
        numbervars(Arg, N0, N1),
        numbervars(I1, Arity, Term, N1, N).

