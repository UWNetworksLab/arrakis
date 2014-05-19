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
% Version:	$Id: cprolog.pl,v 1.6 2008/09/04 10:47:53 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	cprolog.pl
 *
 * DESCRIPTION: 	C-Prolog compatibility package.
 *			This is also the basis for the
 *			Quintus and SICStus packages.
 *
 * CONTENTS:     
 *
 */

:- module(cprolog).

:- comment(summary, 'C-Prolog compatibility package').
:- comment(author, 'Various, ECRC Munich').
:- comment(copyright, 'Cisco Systems, Inc').
:- comment(date, '$Date: 2008/09/04 10:47:53 $').
:- comment(desc, html('
    One of the requirements during the development of ECLiPSe has been the
    aim of minimising the work required to port traditional Prolog
    programs to ECLiPSe.  A de-facto standard for many years was the
    C-Prolog dialect, often referred to as Edinburgh Prolog.  Therefore,
    many of the non standard predicates in C-Prolog have also been
    included in ECLiPSe.  It is of course impossible to achieve total
    compatibility between the two systems.  To assist in making the
    changes necessary to run a C-Prolog program on the current version of
    ECLiPSe, we describve here the predicates available in the
    C-Prolog compatibility library and summarise the principal
    differences between ECLiPSe Prolog and C-Prolog. 
    <P>
    Most of the C-Prolog predicates are also ECLiPSe built-in predicates
    and so they can be always accessed. 
    <P>
    Please note that this text does not detail the functionality of
    C-Prolog, refer to the C-Prolog documentation for this information. 
    <P>
    The effect of the compatibility library is local to the module where
    it is loaded. For maximum compatibility, a C-Prolog program should
    be wrapped in a separate module starting with a directive like
    <PRE>
    :- module(mymodule, [], cprolog).
    </PRE>
    In this case, Eclipse-specific language constructs will not be available.
    <P>
    If the compatibility package is loaded into a standard module, e.g. like
    <PRE>
    :- module(mymixedmdule).
    :- use_module(library(cprolog)).
    </PRE>
    then C-Prolog and Eclipse language features can be used together. 
    However, ambiguities must be resolved explicitly and confusion may
    arise from the different meaning of quotes in Eclipse vs C-Prolog.
    <P>
    Note that the C-Prolog compatibility package includes the <EM>cio</EM>
    library (for see/1, seeing/1, seen/0, skip/1, tab/1, tell/1, telling/1,
    told/0).
    <P>
    The following C-Prolog predicates are not available in ECLiPSe, or
    the corresponding predicates have a different semantics: 
    <DL>
    <DT>assert/2, asserta/2, assertz/2, clause/3 
	<DD>ECLiPSe does not support database references for clauses. 
    <DT>expand_term/2 
	<DD>This is not supported.  ECLiPSe provides the macro facility
	    for transforming input terms (see chapter 13). 
    <DT>&#39;LC&#39;/0, &#39;NOLC&#39;/0 
	<DD>These are not supported in ECLiPSe. 
    </DL>
    <P>
    The following differences remain even with the compatibility package: 
    <DL>
    <DT>Database References 
	<DD>ECLiPSe provides database references only for terms in the indexed database, not for program clauses. 
    <DT>Numbers 
	<DD>C-Prolog has a tendency to "prefer" integers over real
	numbers.  For instance, under C-Prolog when the call X is
	4.0/2.0 is made, X is instantiated to an integer.  This
	behaviour does not occur in ECLiPSe.  The order of integers
	and reals in the standard order is different. 
    <DT>Operators 
	<DD>In C-Prolog there is a bug regarding the operator not -- it
	binds closer than its precedence declaration. 
    <DT>Strings 
	<DD>Strings are simulated in C-Prolog by lists.  Under C-Prolog
	mode, ECLiPSe provides this functionality -- double-quoted
	strings are parsed as lists of integers.  This can cause
	confusion when pure ECLiPSe predicates are used in C-Prolog
	mode, e.g.  substring/3 will not accept double-quoted items,
	since they are lists, not ECLiPSe strings.  The built-in
	string_list/2 converts between both representations. 
    <DT>consult/1, reconsult/1 
	<DD>These are implemented by simply calling the ECLiPSe predicate
	compile/1.  By default all compiled procedures are static. 
	Procedures on which assert/1 etc.  will be applied, have to be
	declared as dynamic using dynamic/1.  The notation [-File] for
	reconsult/1 is not supported. 
    <DT>get/1
	<DD>This is similar to the ECLiPSe predicate get/1, but
	control characters and blank spaces are skipped. 
    <DT>put/1
	<DD>This is similar to the ECLiPSe predicate put/1, but it
	first applies arithmetic evaluation to its argument. 
    <DT>heapused/1
	<DD>Needed for evaluating heapused in arithmetic expressions. 
	It returns the sum of code heap and general heap usage. 
    <DT>instance/2
	<DD>Note that this compatibility predicate redefines the
	ECLiPSe builtin of the same name but different meaning (which
	is no longer available in C-Prolog mode).  It is implemented
	using the ECLiPSe predicate referenced_record/2. 
    <DT>log/2, log10/2
	<DD>These are not predicates in C-Prolog (arithmetic
	functors), but in ECLiPSe they are needed for evaluating log/1
	and log10/1 in arithmetic expressions. 
    <DT>ttyput/1
    	<DD>corresponds to the DEC-10 Prolog predicate 
    </DL>
    The list below describes the syntax differences between ECLiPSe
    and C-Prolog.  The following C-Prolog properties are simulated by
    the compatibility package: 
    <UL>
	<LI>single (resp. double) quote must be doubled between single (resp. double) quotes. 
	<LI>$ is a normal character. 
	<LI>the symbol | is not an atom. 
    </UL>
    The following properties of original C-Prolog are not simulated by
    the compatibility package: 
    <UL>
	<LI>a clause can not be ended by end of file. 
	<LI>based integers are not accepted. 
	<LI>comments are not a delimiter (just ignored). 
	<LI>{} is not an atom. 
	<LI>[] can not be a functor. 
    </UL>
    ')).
:- comment(see_also, [library(cio),library(quintus)]).


:- reexport cio.

% suppress deprecation warnings for reexported builtins
:- pragma(deprecated_warnings(not_reexports)).

:- reexport eclipse_language except

	get/1,				% redefined predicates
	put/1,
	instance/2,
	(abolish)/1,

%	op(_,   xfx, (of)),		% don't provide these
%	op(_,   xfx, (with)),
%	op(_,   xfy, (do)),
%	op(_,   xfx, (@)),
%	op(_,   fx, (-?->)),
%	macro((with)/2, _, _),
%	macro((of)/2, _, _),

	macro((if)/2,_,_).

:- export			% temporary, while op/macros still global
	op(0,   xfx, (of)),
	op(0,   xfx, (with)),
	op(0,   xfy, (do)),
	op(0,   xfx, (@)),
	op(0,   fx, (-?->)),
	macro((with)/2, (=)/2, []),
	macro((of)/2, (=)/2, []).

:- local
	op(650,   xfx, (@)).

:- export
	syntax_option(nl_in_quotes),
	syntax_option(no_blanks),
        syntax_option(no_array_subscripts),
	syntax_option(limit_arg_precedence),
	syntax_option(doubled_quote_is_quote),
	syntax_option(bar_is_no_atom),
	syntax_option(no_attributes),
	syntax_option(no_curly_arguments),
	syntax_option(blanks_after_sign),

	chtab(0'$, lower_case),
	chtab(0'\, symbol),		% disable escape sequences
	chtab(128, string_quote),	% there must be some string_quote
	chtab(0'", list_quote),

	op(300, xfx, mod),
	op(500, fx, (+)),
	op(500, fx, (-)),
	op(900, fy, (spy)),
	op(900, fy, (nospy)).

:- export
	(.)/3,		% to evaluate lists in arithmetic expressions
	(abolish)/2,
	consult/1,
	current_functor/2,
	current_predicate/2,
	db_reference/1,
	erased/1,
	fileerrors/0,
	get/1,
	get0/1,
	heapused/1,
	instance/2,
	leash/1,
	log10/2,
	log/2,
	nofileerrors/0,
	primitive/1,
	prompt/2,
	put/1,
	reconsult/1,
	sh/0.

:- skipped
	(abolish)/2,
	consult/1,
	current_functor/2,
	erased/1,
	fileerrors/0,  
	get/1, 
	get0/1, 
	instance/2,
	log10/2,
	nofileerrors/0,
	primitive/1,
	prompt/2,
	put/1,
	reconsult/1.

:- import
	current_predicate_body / 2,
	error_handler / 2,
	get_flag_body/4,
	set_default_error_handler/2,
	system_error_handler / 4,
	undef_dynamic_handler / 3
    from sepia_kernel.

:- system.		% compiler directive to add the SYSTEM flag


/*
 * OTHER DIRECTIVES
 */


:-  tool((abolish)/2, abolish_body/3),
    tool(consult/1, consult_/2),
    tool(reconsult/1, reconsult_/2),
    tool(current_predicate/2, current_predicate_body/3).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	EVENT HANDLERS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cp_inst_fault(_, arg(N, _, _)) :- integer(N), !, fail.
cp_inst_fault(_, functor(_, _, _)) :- !, fail.
cp_inst_fault(X, Y):- error_handler(X,Y).

cp_range_error(_, arg(_, _, _)) :- !, fail.
cp_range_error(X, Y) :- error_handler(X, Y).

cp_type_error(_, arg(_, _, _)) :- !, fail.
cp_type_error(X, Y) :- error_handler(X, Y).

% This simulates Quintus' behaviour
cp_undef_dynamic_handler(_, retract_all(_), _) :- !.
cp_undef_dynamic_handler(_, retractall(_), _) :- !.
cp_undef_dynamic_handler(_, listing(_), _) :- !.
cp_undef_dynamic_handler(_, retract(_), _) :- !, fail.
cp_undef_dynamic_handler(_, clause(_), _) :- !, fail.
cp_undef_dynamic_handler(_, clause(_, _), _) :- !, fail.
cp_undef_dynamic_handler(E, G, M) :-
	undef_dynamic_handler(E, G, M).

cp_access_undefined(_, abolish(_)) :-
	!.
cp_access_undefined(X,Y) :-
	error_handler(X,Y).

nofileerrors_handler(_, open(_, _, _), _, _) :- !, fail.
nofileerrors_handler(N, Goal, CM, LM) :-
	system_error_handler(N, Goal, CM, LM).

fileerrors :-
	set_default_error_handler(170, system_error_handler/4),
	set_default_error_handler(171, error_handler/2),
	reset_event_handler(170),
	reset_event_handler(171).

nofileerrors :-
	set_default_error_handler(170, nofileerrors_handler/4),
	set_default_error_handler(171, fail/0),
	reset_event_handler(170),
	reset_event_handler(171).

:-
	% we may change the default handlers, because we can't switch back
	set_default_error_handler(4, cp_inst_fault/2),
	reset_event_handler(4),
	set_default_error_handler(5, cp_type_error/2),
	reset_event_handler(5),
	set_default_error_handler(6, cp_range_error/2),
	reset_event_handler(6),
	set_default_error_handler(60, cp_access_undefined/2),
	reset_event_handler(60),
	set_default_error_handler(70, cp_undef_dynamic_handler/3),
	reset_event_handler(70),
	set_default_error_handler(100, cp_undef_dynamic_handler/3),
	reset_event_handler(100).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	NEW PREDICATES
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sh :-
	getenv('SHELL', X),
	sh(X).

primitive(X) :- atom(X), !, fail.
primitive(X) :- atomic(X).

db_reference(X) :- type_of(X, handle).

get0(X):- get(input, X).

leash(Ports) :-
	set_leash(_, print),
	set_leash(Ports, stop).


heapused(X) :- X is statistics(general_heap_used) + statistics(code_heap_used).

prompt(Old, New) :-
	get_stream_info(input, prompt, Old),
	set_stream_property(input, prompt, New).

ttyput(Char) :-
	N is Char,
	put(stdout, N).

current_functor(F, T) :-
	current_functor(F/A),
	functor(T, F, A).

abolish_body(N, A, M) :-
	sepia_kernel:abolish(N/A)@M.

consult_(File, Module) :-
	compile(File)@Module.

reconsult_(File, Module) :-
	compile(File)@Module.

erased(Ref) :-
	\+referenced_record(Ref, _).

%  ARITHMETIC

log10(Y,X):- X is ln(Y)/ln(10.0).

log(Y,X) :- X is ln(Y).

.(X,_,X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	REDEFINED PREDICATES
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get(X):- repeat, get(input, X), X > 32, X < 127, !.

put(X):- N is X, put(output,N).

instance(Ref, Term) :-
	referenced_record(Ref, Term).

current_predicate_body(F, T, M) :-
	current_predicate_body(F/A, M),
	functor(T, F, A),
	get_flag_body(F/A, definition_module, M, M).

