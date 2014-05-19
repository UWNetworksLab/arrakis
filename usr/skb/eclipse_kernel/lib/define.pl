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
% Version:	$Id: define.pl,v 1.1 2008/06/30 17:43:45 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG LIBRARY MODULE
 *
 * IDENTIFICATION:	define.pl
 *
 * AUTHOR:		Mark Shuttleworth (ICL Strategic Systems) &
 *			Emmanuel van Rossum
 *
 * CONTENTS:             
 *			define/2,
 *			define/3,
 *			define_eval/2,
 *			define_erased/0,
 *			define_verbose/1.
 *
 * DESCRIPTION:
 *
 * This library allows to define macro transformation similar
 * to the #define command of the cpp preprocessor. The macro
 * are local to the module where they are defined.
 * 
 * define(+MacroName, ?Term) will expand the following occurences of
 * MacroName into Term.
 * e.g.
 * :- define(m_MAX, 256).
 * :- define(inc(X, Y), (X == m_MAX -> Y = 0 ; Y is X + 1)).
 * 
 * define(+MacroName, ?Term, +Goal) will first execute Goal (the purpose
 * is to construct Term) and then the following occurences of MacroName
 * will be expanded into Term.
 * e.g.
 * :- define(type_error, Err, list_error("type error", Err, _)).
 * :- define(sizes, [A, B, C], (A is m_MAX, B is A // 2, C is B - 1)).
 * 
 * define_eval(+MacroName, ?Term) will evaluate Term and the following
 * occurences of MacroName will be expanded to this evaluated term.
 * It is a shorthand for define(MacroName, EvalTerm, EvalTerm is Term).
 * e.g.
 * :- define_eval(m_HEIGHT, 10).
 * :- define_eval(m_WIDTH, 20).
 * :- define_eval(m_AREA, m_HEIGHT * m_WIDTH).
 * 
 * define_erased/0 is used to erase all the macros defined with
 * define/2,3 or define_eval/2 in the caller module.
 * This is usualy done at the end of the compilation.
 * 
 * define_verbose(on) can be used to display the result of the definitions.
 * This sets an error handler ("macro successfully defined") so, it can be
 * set to do something else for example to make a log file.
 * The default handler is true/0 (not verbose).
 * 
 * A warning is raised when redefining a macro.
 * Set the error handler ("redefining a macro") to true/0 to remove
 * the warning, or to abort/0 to abort the compilation when
 * redefining a macro. The default handler is warning_handler/2.
 * 
 * A type error or an instantiation fault is raised when the MacroName is
 * not an atom or a compound term.
 * 
 * It is safer to protect the MacroName by quoting it so that
 * the MacroName is not expanded when it is redefined.
 * e.g.
 * :- define(no_macro_expansion(m_MAX), 257).
 *				% quoting prevent having define(256, 257).
 * 
 */

:- module(define).

:- export
	define/2,
	define/3,
	define_eval/2,
	define_erased/0,
	define_verbose/1.


:-
	make_local_array(redef),
	make_local_array(verbose),
	define_error("redefining a macro", Redef),
	define_error("macro successfully defined", Verbose),
	setval(redef, Redef),
	setval(verbose, Verbose),
	(import set_default_error_handler/2, warning_handler/2
	from sepia_kernel),
	set_default_error_handler(Redef, warning_handler/2),
	set_default_error_handler(Verbose, true/0),
	reset_error_handler(Redef),
	reset_error_handler(Verbose).

:- tool(define/2, defined/3).
defined(Term, Value, Module) :-
	(var(Term) ->
	    Err = 4
	;
	(not(compound(Term) ; atom(Term)) ->
	    Err = 5
	)),
	!,
	error(Err, define(Term, Value), Module).
defined(Term, Value, Module) :-
	functor(Term, F, A),
	(
	    (
		call(is_predicate(is_defined_macro/2), Module),
		call(is_defined_macro(Term, _), Module)
	    )
	    ->
	    (
		getval(redef, Err),
		error(Err, define(Term, Value)),
		call(retract(is_defined_macro(Term, _)), Module),
		call(erase_macro(F/A), Module)
	    )
	    ;
		true
	),
	call(assert(is_defined_macro(Term, Value)), Module),
	call(define_macro(F/A, is_defined_macro/2, []), Module),
	getval(verbose, Verbose),
	error(Verbose, define(Term, Value)).

:- tool(define/3, define_exec/4).
define_exec(Term, Value, Goal, Module) :-
	call(Goal, Module),
	defined(Term, Value, Module).

:- tool(define_eval/2, define_eval/3).
define_eval(Term, Expression, Module) :-
	define_exec(Term, Value, Value is Expression, Module).

:- tool(define_erased/0, define_erased/1).
define_erased(Module) :-
	call(is_defined_macro(Term, _), Module),
	functor(Term, F, A),
	call(erase_macro(F/A), Module),
	fail.
define_erased(Module) :-
	call(abolish(is_defined_macro/2), Module).

define_verbose(X) :-
	var(X), !,
	error(4, define_verbose(X)).
define_verbose(on) :- !,
	getval(verbose, Err),
	set_error_handler(Err, define_message_handler/2).
define_verbose(off) :- !,
	getval(verbose, Err),
	set_error_handler(Err, true/0).
define_verbose(X) :-
	error(6, define_verbose(X)).

define_message_handler(_error, Goal) :-
	arg(1, Goal, Name),
	arg(2, Goal, Value),
	write(Name),
	write(' defined as '),
	writeln(Value).
