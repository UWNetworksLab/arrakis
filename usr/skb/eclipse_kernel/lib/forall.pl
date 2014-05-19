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
% Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: forall.pl,v 1.1 2008/06/30 17:43:45 jschimpf Exp $
% ----------------------------------------------------------------------

%
% ECLiPSe PROLOG LIBRARY MODULE
%
% $Id: forall.pl,v 1.1 2008/06/30 17:43:45 jschimpf Exp $
%
% IDENTIFICATION:	forall.pl
%
% AUTHOR:		Joachim Schimpf, IC-Parc
%
% DESCRIPTION:

/*

This is a meta-predicate for writing simple iterations without
the need for an auxiliary recursive predicate.

The syntax is

    IterationSpecs do Goals

and it corresponds to a call to an auxiliary recursive predicate of the form

    aux(...).
    aux(...) :- Goals, aux(...).

IterationSpecs is one (or a comma-separated sequence) of the following:

    fromto(First,In,Out,Last)
    	iterate starting with In=First until Out=Last.
	In and Out are local variables in Goal.

    foreach(X,List)
    	iterate with X ranging over all elements of List
	X is a local variable in Goal.
	Can also be used for constructing a list.

    foreacharg(X,Struct)
    	iterate with X ranging over all elements of List
	X is a local variable in Goal.
	Cannot be used for constructing a term.

    for(I,MinExpr,MaxExpr)
    	iterate over integers from Min to Max.
	I is a local variable in Goal.
	MinExpr and MaxExpr can be arithmetic expressions.
	Can be used only for controlling iteration, ie. MaxExpr cannot
	be uninstantiated.

    count(I,Min,Max)
    	iterate over integers from Min up to Max.
	I is a local variable in Goal.
	Can be used for controlling iteration as well as counting,
	ie. Max can be a variable.

    param(Var1,Var2,...)
    	for declaring variables in Goal global, ie shared with the context.
	By default, variables in Goal are local.

Note that foreach/2, foreacharg/2, count/3, for/3 and param/N are only
specialisations of the more general fromto/4.

The do-operator binds like the semicolon, ie. less than comma.
That means that the whole do-construct should normally be bracketed.

When you use :-pragma(expand) or :-nodbgcomp the do-construct is compiled
into an efficient auxiliary predicate named do__nnn.


EXAMPLES
--------
% iterate over list
foreach(X,[1,2,3]) do writeln(X).

% maplist
(foreach(X,[1,2,3]), foreach(Y,List) do Y is X+3).

% sumlist
(foreach(X,[1,2,3]), fromto(0,In,Out,Sum) do Out is In+X).

% reverse list
(foreach(X,[1,2,3]), fromto([],In,Out,   Rev) do Out=[X|In]).
(foreach(X,[1,2,3]), fromto([],In,[X|In],Rev) do true).

% iterate over integers from 1 up to 5
for(I,1,5) do writeln(I).
count(I,1,5) do writeln(I).

% make list of integers [1,2,3,4,5]
(for(I,1,5), foreach(I,List) do true).
(count(I,1,5), foreach(I,List) do true).

% make a list of length 3
(foreach(_,List), for(_,1,3) do true).
(foreach(_,List), count(_,1,3) do true).

% get the length of a list
(foreach(_,[a,b,c]), count(_,1,N) do true).

% actually, the length/2 builtin is (almost)
length(List, N) :- (foreach(_,List), count(_,1,N) do true).

% filter list elements
(foreach(X,[5,3,8,1,4,6]), fromto(List,Out,In,[]) do
    X>3 -> Out=[X|In] ; Out=In).

% iterate over structure arguments
(foreacharg(X,s(a,b,c,d,e)) do writeln(X)).

% collect args in list (bad example, use =.. if you really want to do that!)
(foreacharg(X,s(a,b,c,d,e)), foreach(X,List) do true).

% collect args reverse
(foreacharg(X,s(a,b,c,d,e)), fromto([],In,[X|In],List) do true).

% or like this:
S = s(a,b,c,d,e), functor(S, _, N),
(fromdownto(N,I,0), foreach(A,List), param(S) do arg(I,S,A)).

% The following two are equivalent
foreach(X,[1,2,3])        do             writeln(X).
fromto([1,2,3],In,Out,[]) do In=[X|Out], writeln(X).

% The following two are equivalent
count(I,1,5)     do            writeln(I).
fromto(0,I0,I,5) do I is I0+1, writeln(I).



REMARKS

Currently it is an undetected error (in the metacalled version) when a
variable which is meant to be local to the body also occurs (or even
gets bound) outside the body.  This could be avoided by always renaming
the locals by macro expansion.

*/

:- module(forall).
