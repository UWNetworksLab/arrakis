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
% Version:	$Id: kb.pl,v 1.1 2008/06/30 17:43:47 jschimpf Exp $
% ----------------------------------------------------------------------

/*******************************************************************

File   : kb.pl
Author : Jorge Bocca, Michael Dahmen, Luis Hermosilla
Content: The Deductive Relations operators of MegaLog

Note   : This file is for the MegaLog-Sepia integration. It was forked
	 from "@(#)ecc.pl    1.23         12/20/91"

	 The file has been modified such that the MegaLog compatibility 
	 package for Sepia (megalog.pl) is not needed.

Usage  : In client module do

:- use_module(library(kb)).

*******************************************************************/

:- module_interface(kb).

:- use_module(library(db)).		% for <=>/2 and <->/2

:-	op(900, xfx, '<==>'),	% schema declare/query
	op(900, xfx, '<@@>'),	% schema compatibility 
	op(900, xfx, '<-->'),	% synonym declare/query 
	op(900, xfx, 'isdr'),	% set eval query on derived relations
	op(900, fx, 'isdr'),	% print a deductive relation
	op(900, xfx, 'expand'),	% generate extension
	op(900, fx, 'expand'),	% print extension
	op(900, xfx, '<+++'),	% insert for deductive rel
	op(900, xfx, '<---').	% delete for deductive rel


:- begin_module(kb).

:- use_module(library(database_kernel)).

:- pragma(system).
:- pragma(nodebug).

:- export 
   (<==>)/2, (<@@>)/2, (<-->)/2, (isdr)/2, (isdr)/1, (expand)/2, (expand)/1, 
   (<+++)/2, (<---)/2.

:- export 
   closekb/0, createkb/1, define/1, define_implicit/1,
   degree/2, delete_clause/1, destroykb/0, display_defs/0,
   helpdrel/1, helpkb/0, insert_clause/1,
   insert_clauses_from/1, openkb/1, retract_clause/1,
   retrieve_clause/1, update_defs/0.

:- export domains/2.	% for megalog.pl only

:- import import_body/2, is_predicate_/2 from sepia_kernel.

:- make_local_array(tuples).

/*
** Modules and meta calls :
**
** The knowledge base is hold in module 'knowledge_base'. The definitions
** file is compiled into that module. All rules are executed in that module.
*/

/* create the knowledge base module, and import deductive relations there */

:- current_module(knowledge_base) -> true ; create_module(knowledge_base).
:- call(import kb, knowledge_base).

/**********************************************************************************
**
**
**	[KB]	DEDUCTIVE RELATIONS
**
**********************************************************************************/

/* not a Sepia predicate, but often used */
list([_|_]) ?- true.

/* Constant definitions */

'KB_constant'(definitons_file, "/static_defs.pl").
'KB_constant'(reference_hash,  87654321).
'KB_constant'(list_hash,       87654326).

'KB_exec_clause'((Head :- Body),Goal) :-
	!,
	Head = Goal,
	call(Body, knowledge_base).
'KB_exec_clause'(Goal,Goal).

/*
**	[KB.A]	Setting up the Knowledge Base
**
**	Create, open and close KB
**
*/

/*
*	Create and open a KB
*
*	- create/open the DB (if not opened already),
*	- read in the definitions of external predicates, and
*/

definitons_file_name(FullName) :-
	bang_register( dbdirectory, Path),
	atom_string(Path,PathString),
	'KB_constant'(definitons_file, Name),
	concat_strings(PathString,Name,FullName).

createkb(KB) :-
	createdb(KB),
	definitons_file_name(FullName),
	concat_strings("touch ",FullName,Command),
	system(Command),			% touch <KB-DIR>/static_defs.pl
	compile(FullName,knowledge_base).

openkb(KB) :-
	opendb(KB),
	definitons_file_name(FullName),
	compile(FullName,knowledge_base).

closekb :-
	% all temporary relations are removed when the data base is closed
	closedb.
	
destroykb :-
	definitons_file_name(FullName),
	concat_strings("rm -f ",FullName,Command),
	system(Command),		% rm -f <KB-Dir>/static_defs
	destroydb.


helpkb :-
	'KB_print_help_header',
	current_relation( Rel / _),
	'KB_print_basic_info'( Rel),
	fail.

helpkb :-	
	'KB_print_help_tail'.



'KB_print_help_header' :-
	bang_register( dbdirectory, KB),
	nl,
	write("CURRENT KNOWLEDGE BASE:  "),
	writeln(KB),
	writeln("==============================================================="),
	writeln("Name  -->  Arity  -->  No. of Clauses  -->  Schema"),
	writeln("===============================================================").



'KB_print_help_tail' :-
	writeln("==============================================================="),
	nl.



'KB_print_basic_info'( Rel) :-
	degree( Rel, Arity),
	cardinality( Rel, Cls),
	Rel <==> Schema,
	write(Rel), write("\t"),
	write(Arity), write("\t"),
	write(Cls), write("\t"),
	writeln(Schema).




/*
**	[KB.B]	Schema querying
*/

/* Synonym - A way of using pseudo names for deductive relations */
<-->( Rel, Synonym) :-
	Rel <-> Synonym.



/* Give info about the schema of deductive relation 'Rel' */
helpdrel( Rel) :-
	'KB_real_rname'( Rel, RRel),
	bang_arity( RRel, Arity),
	RRel <==> Format,
	Ary is Arity - 1,  % Ary - 1 to acc. for red tape
	'KB_print_header_rel'( Rel, RRel, Ary, Format),
	'KB_help_tail_drel'( RRel).



'KB_help_tail_drel'( RRel) :-
	bang_cardinality( RRel, Card),
	write('NUMBER OF CLAUSES : '), write(Card), nl, nl.


/* arity  when real name is used */
degree( Rel, Arity) :-
	arity( Rel, Ary),
	Arity is Ary - 1.  % Ary - 1 to acc. for red tape




/*
**	[KB.C]	Create/Format/Remove relation
*/

/* Warning : <=>/2 is a non logical operation, effect depends on bindings */

<==>( Rel, KBformat) :-
	var(KBformat),
	!,
	Rel <=> DBformat,
	DBformat = [term(_,_,_) | DBindex_format],
	'KB_convert_format'(KBformat,DBindex_format).

<==>( Rel, []) :-
	!,
	Rel <=> [].

<==>( Rel, KBformat) :-
	'KB_convert_format'(KBformat,DBindex_format),
	DBformat = [term(source,_,_) | DBindex_format],
	Rel <=> DBformat.


/* obtain attributes' names (without index indicator) */
domains( Rel, AttNames) :-
	nonvar(Rel),
	Rel <==> KBformat,
	'KB_convert_key'(KBformat, AttNames).


/*	--- [KB.C]	Support ---	*/

'KB_convert_format'([], []).

'KB_convert_format'([KBformat | MoreKBS], [DBformat| MoreDBS]) :-
	'KB_convert_att_format'(KBformat,DBformat),
        'KB_convert_format'(MoreKBS,MoreDBS).

/* default is no key (-), default is suppressed when creating KBformat */

'KB_convert_att_format'( +Dom, integer( Dom, _ISz, '+')) :- atom(Dom),!.
'KB_convert_att_format'(  Dom, integer( Dom, _ISz, '-')) :- atom(Dom),!.
'KB_convert_att_format'( -Dom, integer( Dom, _ISz, '-')) :- atom(Dom).

'KB_convert_key'([],[]).
'KB_convert_key'([+Name|T],[Name|R]) :- !, 'KB_convert_key'(T,R).
'KB_convert_key'([-Name|T],[Name|R]) :- !, 'KB_convert_key'(T,R).
'KB_convert_key'([Name|T],[Name|R]) :- !, 'KB_convert_key'(T,R).


/*
**	[KB.D]	Schema Set Up
**
*/

/*
*	[KB.D.1]	Schema compatibilty for union and difference
*/

<@@>( Rel1, Rel2) :-
	degree( Rel1, Arity),	% check existance Rel1
	degree( Rel2, Arity).	% check existance Rel2
	% same arity is sufficient, schemas need not be identical


/*
*	[KB.D.2]	Define Abstract Types and Modes of Access
*/

define(File) :-
	atom(File),
	!,
	atom_string(File,FileS),
	define(FileS).
define(File) :-
	check_single_user_mode,
	string(File),
	compile(File,knowledge_base),	% new definitions into main memory
	definitons_file_name(FullName),
	concat_strings("cat ", File,S1),
	concat_strings(S1," >> ",S2),
	concat_strings(S2,FullName,Command),
	system(Command).		% cat <File> >> <KB-Dir>/static_defs


define_implicit( P/Ary) :-
	call(is_predicate(P/Ary), knowledge_base),
	% already defined
	!.

define_implicit( P/Ary) :-
	A1 is Ary + 1,
	current_relation(P/A1),
	% a permanent relation
	!,
	check_single_user_mode,

	functor(Head, P, Ary),
	Clause = (Head :- retrieve_clause(Head :- Body), call(Body)),
	call(compile_term([Clause]), knowledge_base),

	definitons_file_name(FullName),
        open(FullName, update, Stream),
        seek(Stream, end_of_file),
	writeclause(Stream, Clause),
        close(Stream).

define_implicit( P/Ary) :-
	A1 is Ary + 1,
	current_temp_relation(P/A1),
	% a temporary relation
	!,
	functor(Head, P, Ary),
	Clause = (Head :- retrieve_clause(Head :- Body), call(Body)),
	call(compile_term([Clause]), knowledge_base).

define_implicit(P/Ary) :-
	error(303,define_implicit(P/Ary)),
	fail.



display_defs :-
	definitons_file_name(FullName),
	concat_strings("more ", FullName, Command),
	system(Command).



update_defs :-
	check_single_user_mode,
	definitons_file_name(FullName),
	'KB_edit'(FullName),
	compile(FullName, knowledge_base).




/*      --- [KB.E.1] Support ---     */
 

check_single_user_mode :-
	bang_register(remote_database,0),
	bang_register(shared_memory,0),
	% not in multi user mode
	!.
check_single_user_mode :-
	error(314,update(definitions_file)),
	fail.


'KB_edit'(Defs) :-
	getenv("WINDOW_ME",_),          % verify that Sun Windows running
	!,
	concat_strings( "textedit ", Defs, Edit),
	system(Edit).
'KB_edit'(Defs) :-
	concat_strings( "vi ", Defs, Edit),
	system(Edit).



/*
**	[KB.E]	Insert
**
**	CLAUSE INSERTION :-
**
**	insert_clause( Clause).	% inserts one clause in deductive rel at random
**
**
**	SET INSERTION :-
**
**	relname <+++ Expr.	%  Expr as used in isdr
**	e.g.:
**		?- high_paid_emp <+++ employee where dept == sales.
**		?- high_paid  <+++ [name] :^: employee where dept == sales.
**
**
**	BATCH INSERTION :-
**
**	insert_clauses_from(user)	% adds clauses to appropiate
**					% relations from 'user'
**	insert_clauses_from(File)	% adds clauses to appropiate
**					% relations from a given File
*/

/*
*	[KB.E.1]	Insert clause
*/

insert_clause( Clause) :-
	'KB_store_cl_random'( Clause).



 

/*
*	[KB.E.2]	Insert set of clauses
*/

<+++( X, ProjL :^: Expr) :-
	'KB_real_rname'( X, XReal), !,
	'KB_dr_insert_proj'(Expr, XReal, ProjL).

<+++( X, Expr) :-
	'KB_real_rname'( X, XReal), !,
	'KB_dr_insert_noproj'(Expr, XReal).





/*
*	[KB.E.3]	Batch insertion of clauses
*/

insert_clauses_from(user) :-  /* default: user tty */
        !,
        repeat,
        read(Clause),
        ( ( Clause == end_of_file       , !    ) ;
	  ( Clause = (:- Query), !, call(Query, knowledge_base), !, fail ) ;
	  ( Clause = (?- Query), !, call(Query, knowledge_base), !, fail ) ;
          ( 'KB_store_cl_random'(Clause), fail ) ).

insert_clauses_from(File) :-
        open(File, read, Stream),
        repeat,
        read(Stream,Clause),
        ( ( Clause == end_of_file       , !    ) ;
	  ( Clause = (:- Query), !, call(Query, knowledge_base), !, fail ) ;
	  ( Clause = (?- Query), !, call(Query, knowledge_base), !, fail ) ;
          ( 'KB_store_cl_random'(Clause), fail ) ),
        close(Stream),
	write(File), writeln(' inserted into knowledge base').



'KB_store_cl_random'( Clause) :-
	'KB_identify_clause'( Clause, Rel, Ary, Descr),
	Arity is Ary + 1,
	'KB_real_rname'( Rel, RRel),
	bang_arity( RRel, Arity),
        bang_insert( RRel, [ Clause | Descr]),
        !.

'KB_store_cl_random'( RRel, Args, _Ary, Clause) :-
	'KB_gen_descr'(Args, Descr),
        bang_insert( RRel, [ Clause | Descr]),
        !.



/*
	'KB_identify_clause'( Clause, F, N, Descr)

	- it reports the functor F and the arity N of the head of the Clause
	- it generates a Descr-iptor for the Clause
*/
'KB_identify_clause'((Head :- _), P, N, Descr) :-
	% rule
	!,
	Head =.. [P|Args],
	functor( Head, P, N),
	'KB_gen_descr'(Args, Descr).

'KB_identify_clause'( Clause, P, N, Descr) :-
	% fact
	Clause =.. [P|Args],
	functor( Clause, P, N),
	'KB_gen_descr'(Args, Descr).




/*
	'KB_gen_descr'( Args, InsDescr)

	For the arguments Args in the head of the predicate
	to be added, the descriptor for insertion InsDescr is generated
*/

'KB_gen_descr'( [], []).
'KB_gen_descr'( [Arg| Args], [AttV| Vals]) :-
	'KB_descrins'( Arg, AttV),
	'KB_gen_descr'( Args, Vals).




/*
	'KB_descrins'( Args, ValD)

	For each Arg, it generates the ValD descriptor
*/

'KB_descrins'( Arg, REFX) :-
	var(Arg), !,
	'KB_constant'(reference_hash, REFX).
'KB_descrins'( Arg, AVal) :-
	atomic(Arg), !,
	'KB_descrins_atomic'(Arg, AVal).
'KB_descrins'(  Arg, LISTX) :-
	list(Arg), !,
	'KB_constant'(list_hash, LISTX).
'KB_descrins'(  Arg, TVal) :-
	compound(Arg), !,
	functor(Arg, F, Ary),
	hash( F, AttV),
	TVal is (Ary << 24) \/ AttV.


'KB_descrins_atomic'(  Arg, AttV) :-
	atom(Arg), !,
	hash( Arg, AttV).
'KB_descrins_atomic'( Arg, Arg) :-
	integer(Arg), !.
'KB_descrins_atomic'( Arg, AttV) :-
	real(Arg), !,
	AttV is fix(Arg).
'KB_descrins_atomic'(  Arg, AttV) :-
	string(Arg), !,
	hash( Arg, AttV).







/*	--- [KB.E.2]	Support ---	*/

/* concatenate the result of a query to relation X */
'KB_dr_insert_noproj'( where( Rels, Conds), RX) :- !,
        Temp isdr Rels where Conds,
	'KB_cat_dr'( Temp, RX),
	Temp <==> [].

/*  concatenate relation Rel to relation X */
'KB_dr_insert_noproj'( Rel, RX) :-
	atom(Rel), !,
	'KB_real_rname'( Rel, RRel), !,
	'KB_cat_dr'( RRel, RX).




'KB_dr_insert_proj'( where( Rels, Cond), RX, ProjL) :- !,
        Temp isdr ProjL :^: Rels where Cond,
	'KB_cat_dr'( Temp, RX),
	Temp <==> [].



/*  Concatenate deductive relation RRel to RX - DUPLICATES stay */
'KB_cat_dr'( RRel, RX) :-
	retr_tup( RRel, [Term | Descr]),  /* backtrack here */
	'KB_make_new_term'(Term, RX, NewTerm),
	bang_insert( RX, [NewTerm | Descr]),
	fail.
'KB_cat_dr'( _, _).

'KB_make_new_term'(':-'(Head, Body), NewRel, ':-'(NewHead, Body)) :-
        !,
        Head =.. [_| Args],
        NewHead =.. [NewRel| Args] .
 
'KB_make_new_term'( Clause, NewRel, NewClause) :-
        Clause =.. [_| Args],
        NewClause =.. [NewRel| Args] .









/*
**      [KB.F]     Delete
**
**	CLAUSE DELETION :-
**
**      delete_clause( Clause).    % deletes clause in derived rel
**				     comparison condition is variant
**      retract_clause( Clause).   % deletes clause in derived rel
**				     comparison condition is unification
**
**
**	SET DELETION :-
**
**	The following set operation not yet implemented:
**
**	relname <--- Expr.	%  Expr as used in isdr/expand
**	e.g.:
**		?- high_paid_emp <--- employee where dept == sales.
**		?- high_paid  <--- [name] :^: employee where dept == sales.
**
*/

/*
*	[KB.F.1] Clause at a time deletion
*/

delete_clause( Clause) :-
	'KB_identify_clause'( Clause, Rel, Ary, Descr),
	Arity is Ary + 1,
	'KB_real_rname'( Rel, RRel),
	bang_arity( RRel, Arity),
	bang_retrieve_delete( RRel, [C | Descr], true, variant(Clause,C) ).
	

/* Note :
** Equality on term attribute is variant. The clauses deleted are those 
** that are variants look identical apart from variable renaming. Note that a 
** unification test would give a behaviour close to retract/1.
**
** Unification : bang_retrieve_delete( RRel, [0, Clause | Descr], true)
**
** Note :
** Just replacing the test is not sufficient because 'KB_identify_clause' 
** is only applicable to full instanziated clauses
*/

retract_clause((Head :- Body)) :-
        !,
        retract_clause( Head, Body).
retract_clause(Head) :-
        retract_clause( Head, true).
 
retract_clause( Head, Body) :-
        % var(Body),    % we don't index on body but may unify
        Head =.. [ P| Args],
        functor( Head, P, Ary),
        length(Frame, Ary),
        functor( TestFrame, '$$test', Ary),
        'KB_mod_args'(Args, ModArgs, P, 2), % skip the first attr. - red tape
        !,  % Backtracking implemented in gen_descr/3 and retr_tup/2
        'KB_gen_descr'(ModArgs, Frame, TestFrame, P, 1),
 
        retract_clause( P, Head, Body, Frame).
 
retract_clause(P, Head, true, Frame) :-
        del_tup( P, [ Head | Frame]).
retract_clause(P, Head, Body, Frame) :-
        del_tup( P, [ (Head :- Body) | Frame]).

/*
*	[KB.F.2] Set at a time deletion
*
*	Assume no duplicates
*
*	Note : Delete variant clauses
*/

<---( X, ProjL :^: Expr) :-
	'KB_real_rname'( X, XReal), !,
	'KB_dr_delete_proj'(Expr, XReal, ProjL).

<---( X, Expr) :-
	'KB_real_rname'( X, XReal), !,
	'KB_dr_delete_noproj'(Expr, XReal).






/*	--- [KB.F.2]	Support ---	*/

/* delete the result of a query from relation X */
'KB_dr_delete_noproj'( where( Rels, Conds), RX) :- !,
        Temp isdr Rels where Conds,
	'KB_del_dr'( Temp, RX),
	Temp <==> [].

/*  delete relation Rel from relation RX */
'KB_dr_delete_noproj'( Rel, RX) :-
	atom(Rel), !,
	'KB_real_rname'( Rel, RRel), !,
	'KB_del_dr'( RRel, RX).




/* delete the result of a query from relation RX */
'KB_dr_delete_proj'( where( Rels, Cond), RX, ProjL) :- !,
        Temp isdr ProjL :^: Rels where Cond,
	'KB_del_dr'( Temp, RX),
	Temp <==> [].



/*  Delete clauses appearing in RRel from RX */

'KB_del_dr'( RRel, RX) :-
        retr_tup( RRel, [Clause | Descr]),  %  backtrack here
        bang_retrieve_delete( RX, [C | Descr], true, variant(Clause,C) ),
        fail.
'KB_del_dr'( _, _).




/*
**	[KB.G]	Retrieval
**
**	SINGLE CLAUSE RETRIEVAL AND EXECUTE :-
**
**	retrieve_clause( Clause) - finds through backtracking clauses
**		that unifies with the given clause. Clause must be 
**		sufficiently instaniated to identify preciate name and
**		arity.
**
**	To make its access transparent to users, define:
**		pred(A1, ..., An) :- 
**			retrieve_clause(( pred(A1, ..., An):- Body )),
**			call(Body).
**
**
**	SET RETRIEVAL :-
**
**	isdr	- generates a new deductive relation
**	expand	- generates the expansion of the equivalent deductive rel.
**
**	Rules for building valid retrieval expressions:
**
**	Rel isdr <[A1, .., Am] :^:> Rel <where Cond>		% selection
**	Rel isdr <[A1, .., Am] :^:> R1 :+: R2 <where Cond>	% union
**	Rel isdr <[A1, .., Am] :^:> R1 :-: R2 <where Cond>	% difference
**	Rel isdr <[A1, .., Am] :^:> R1 :*: R2 where Cond	% join
**
**	Rel expand <[A1, .., Am] :^:> Rel <where Cond>		% selection
**	Rel expand <[A1, .., Am] :^:> R1 :+: R2 <where Cond>	% union
**	Rel expand <[A1, .., Am] :^:> R1 :-: R2 <where Cond>	% difference
**	Rel expand <[A1, .., Am] :^:> R1 :*: R2 where Cond	% join
**
**	where Cond is:
**		true				% not applicable to join
**		Att < Cte	(Cte > Att)	% less than
**		Att =< Cte	(Cte >= Att)	% less or equal than
**		Att @< Cte	(Cte @> Att)	% term less than
**		Att @=< Cte	(Cte @>= Att)	% term less or equal than
**		Att == Cte	(Cte == Att)	% equal than
**		Att == Cte	(Att == Cte)	% equal than
**		Att = Cte	(Cte = Att)	% unify
**		Att = Cte	(Att = Cte)	% unify
**		Att >= Cte	(Cte =< Att)	% greater or equal than
**		Att > Cte	(Cte < Att)	% greater than
**		Att @>= Cte	(Cte @=< Att)	% term greater or equal than
**		Att @> Cte	(Cte @< Att)	% term greater than
**		Att1 == Att2			% attributes' equality
**		Att1 = Att2			% attributes' unification
**		Cond1 and Cond2			% logical and
**
**	i.	Att is an unambiguous attribute name. Ambiguous attribute names
**		are disambiguated by prefixing them with <relation name>^.
**		For example:	employee^salary
**
**	ii.	Angle brakets <...> above, are only used to denote an
**		optional part.  Default for projection is "all the attributes"
**		and for condition is "where true", except in joins where it
**		is not allowed.
**
**
**	DISPLAY TO SCREEN :-
**
**	isdr/1		- if the single argument is an atom, it attempts to
**		prints the contents of the deductive relation designated
**		by the atom.
**
**		- if the single argument is a retrieval expression as in
**		isdr/2, then evaluates the expression and prints the
**		resulting deductive relation.
**
**	expand/1 - similar to isdr/1, but it prints the extension instead.
**
*/

/*
*	[KB.G.1]	Tuple at a time retrieval of drel
*/

retrieve_clause((Head :- Body)) :-
	!,
	retrieve_clause( Head, Body).
retrieve_clause(Head) :-
	retrieve_clause( Head, true).

retrieve_clause( Head, Body) :-
	Head =.. [ P| Args],
	functor( Head, P, Ary),
	length(Frame, Ary),
	functor( TestFrame, '$$test', Ary),
	'KB_mod_args'(Args, ModArgs, P, 2), % skip the first attr. - red tape
	!,  % Backtracking implemented in gen_descr/5 and retr_tup/2
	'KB_gen_descr'(ModArgs, Frame, TestFrame, P, 1),
	retr_tup( P, [ Clause | Frame]),
	retrieve_clause( Clause, Head, Body).


retrieve_clause((Head :- Body) , Head1, Body) :-
	% rule
	!,
	Head = Head1.
retrieve_clause( Head, Head, true).
	% fact




/*
*
*	[KB.G.2]	Set at a time evaluation
*
*	isdr/2   --	derive set of rules
*	expand/2 --	expand the derived relation into a new rel.
*
*/

/*	Derive a new relation */
isdr( X, ProjL :^: Expr) :-
	'KB_valid_rel_name'(X), list(ProjL), !,
	'KB_isdr_proj'( Expr, X, ProjL).

isdr( X, Expr) :-
	'KB_valid_rel_name'(X), !,
	'KB_isdr_noproj'( Expr, X).



/*	Expand a new relation */
expand( Rel, DRel) :-
	atom(DRel), !,
	'KB_real_rname'( DRel, DRRel),
	bang_arity( DRRel, Arity),
	DRRel <==> S,
	Rel <==> S,
	Ary is Arity - 1,	% take account of red tape
	'KB_freeze_extension'( Rel, DRRel, Ary).

expand( Rel, Expr) :-
	'KB_valid_rel_name'(X), !,
	X isdr Expr,
	expand( Rel, X),
	X <==> [], !.


'KB_valid_rel_name'(X) :- atom(X).
'KB_valid_rel_name'(X) :- var(X).





/*
*
*	[KB.G.3]	Display a deductive relation to screen
*
*	isdr/1
*	expand/1
*/

isdr( Rel) :-
	atom(Rel), !,
	display_dr(Rel).

isdr( Expr) :-
	X isdr Expr,
	display_dr(X),
	X <==> [].


expand( Rel) :-
	atom(Rel), !,
	display_x(Rel).

expand( Expr) :-
	X isdr Expr,
	display_x(X),
	X <==> [].














/*	--- [KB.G.1]	Support ---	*/

/*
	'KB_mod_args'( Args, ModArgs, Rel, I)

	It modifies the argument list so that uninstantiate those
	arguments that are not part of the key in relation Rel.
	This reduces considerable the amount of backtracking
	in retrieve_clause/1.
*/

'KB_mod_args'([], [], _, _).

'KB_mod_args'([Arg| Args], [Mod|ModArgs], Rel, I) :-
	bang_attribute( Rel, I, F),
	arg( 3, F, K),
	'KB_mod_arg'( K, Arg, Mod),
	I1 is I + 1,
	'KB_mod_args'(Args, ModArgs, Rel, I1).


'KB_mod_arg'( '+', Arg, Arg).
'KB_mod_arg'( '-', _, _Mod).





/*	--- [KB.G.2]	Support ---	*/

/*
	'KB_gen_descr'( Args, Descrs, Frame, Rel, I)

	For the arguments Args in the head of the predicate Rel,
	the descriptor for a goal Descrs is generated
*/

'KB_gen_descr'( [], [], _, _, _).
'KB_gen_descr'( [Arg| Args], [AttV| Vals], TestFrame, Rel, I) :-
	'KB_descrgoal'( Arg, AttV, TestFrame, Rel, I),
	I1 is I + 1,
	'KB_gen_descr'( Args, Vals, TestFrame, Rel, I1).




/*
	'KB_descrgoal'( Arg, DVal, TestFrame, Rel, I)

	For Arg in position I of Rel, it generates the descriptor
	for attribute in position I, placing it into the TestFrame.
	On a goal,  attempt to search for the most instantiated
	head and then gradually, attempt a variable in each argument
*/

'KB_descrgoal'( Arg, _, _, _, _) :-
	var(Arg), !.

'KB_descrgoal'( Arg, DVal, _, _, _) :-
	% not var(Arg),  -- Optim.
	% Deal with a ground argument for the first time
	'KB_descrins'( Arg, DVal).

'KB_descrgoal'( _Arg, REFX, TestFrame, Rel, I) :-
	% Deal with a REDO for a ground argument
	% Prepare a descriptor for a head with a matching variable
	% Avoids an exponential explosion of descriptors, e.g.
	% the case p(X,Y), should never be generated if either p(X, ..) or
	% p(.., Y) have not matching heads in the clauses for the procedure p/2
	% not var(Arg),  -- Optim.
	'KB_constant'(reference_hash, REFX),
	arg( I, TestFrame, REFX),
	TestFrame =.. [ _| TFr],

	%  make sure that there is at least one head with a var in this position
	once( retr_tup( Rel, [ _ | TFr])).








'KB_isdr_noproj'( where( R, Cond), X) :- !,
	'KB_isd_noproj_where'( R, Cond, X), !.

'KB_isdr_noproj'( R, X) :- !,
	'KB_isd_noproj_where'( R, true, X), !.



'KB_isdr_proj'( where( R, Cond), X, Proj) :-
	'KB_isd_proj_where'( R, Cond, Proj, X), !.

'KB_isdr_proj'( R, X, Proj) :-
	'KB_isd_proj_where'( R, true, Proj, X), !.



/*  NO Project  */
'KB_isd_noproj_where'( R1 :+: R2, Cond, X) :- !,
	R1 <@@> R2,
	X isdr R1 where Cond,
	X <+++ R2 where Cond.

'KB_isd_noproj_where'( R1 :-: R2, Cond, X) :- !,
	R1 <@@> R2,
	X isdr R1 where Cond,
	X <--- R2 where Cond.

'KB_isd_noproj_where'( R1 :*: R2, Cond, X) :- !,
	'KBU_cond_parse'( Cond, [R1, R2], CondT),
	'KBX_join2_univ_noproj'( R1, R2, CondT, TempRes),
	'KB_jflatten_src_tree'( R1, R2, Cond, Args1, Args2, Xtras),
	'KB_jflatten_src_tree'( R1, R2, Cond, CpArgs1, CpArgs2, CpXtras),
	'KB_genj_clauses'( Xtras, R1, R2, TempRes, Args1, Args2,
		CpArgs1, CpArgs2, CpXtras, X),
	TempRes <=> [], !.

'KB_isd_noproj_where'( R, Cond, X) :-
	atom(R), !,
	'KBU_cond_parse'( Cond, [R], CondT),
	'KBX_select_univ_noproj'( R, CondT, TempRes),
	'KB_flatten_src_tree'( R, Cond, Args, Xtras),
	'KB_flatten_src_tree'( R, Cond, CpArgs, CpXtras),
	R <==> S,
	'KB_gens_clauses'( Xtras, S, R, TempRes, Args, CpArgs, CpXtras,
		X),
	TempRes <=> [], !.






/*  Project  */
'KB_isd_proj_where'( R1 :+: R2, Cond, Proj, X) :- !,
	R1 <==> Schema1,
	R2 <==> Schema2,
	Schema1 == Schema2,
	X isdr Proj :^: R1 where Cond,
	X <+++ Proj :^: R2 where Cond.

'KB_isd_proj_where'( R1 :-: R2, Cond, Proj, X) :- !,
	R1 <==> Schema1,
	R2 <==> Schema2,
	Schema1 == Schema2,
	X isdr Proj :^: R1 where Cond,
	X <--- Proj :^: R2 where Cond.

'KB_isd_proj_where'( R1 :*: R2, Cond, Proj, X) :- !,
	'KBU_cond_parse'( Cond, [R1, R2], CondT),
	'KBX_join2_univ_noproj'( R1, R2, CondT, TempRes),
	'KB_jflatten_src_tree'(R1, R2, Cond, Args1, Args2, Xtras),
	'KB_jflatten_src_tree'(R1, R2, Cond, Proj,
		CpArgs1, CpArgs2, CpXtras, VProj, SrcPrj),
	'KB_genjp_clauses'( Xtras, VProj, SrcPrj, R1, R2, TempRes, Args1, Args2,
		CpArgs1, CpArgs2, CpXtras, X),
	TempRes <=> [], !.

'KB_isd_proj_where'( R, Cond, Proj, X) :-
	atom(R), !,
	'KBU_cond_parse'( Cond, [R], CondT),
	'KBX_select_univ_noproj'( R, CondT, TempRes),
	'KB_flatten_src_tree'( R, Cond, Args, Xtras),
	'KB_flatten_src_tree'( R, Cond, Proj, CpArgs, CpXtras, VProj, SrcPrj),
	'KB_gensp_clauses'( Xtras, VProj, SrcPrj, R, TempRes, Args,
		CpArgs, CpXtras, X),
	TempRes <=> [], !.






	
/*	--- Generate Condition for Pre-Unification ---	*/


/* Generate a list of codified trees (CondTrees) of the
	conditions (Cond) for relations Rs */
'KBU_cond_parse'( Cond, Rs, CondTrees) :-
	'KB_get_all_formats'( Rs, RFs),
	findall(CondT, 'KBS_cond_parse'( Cond, RFs, CondT), CondTrees).
	% Note : it is not neccessary to remove redundant 'true' conditions
	% from the CondTrees since the database interface allows them



'KBU_tup_cond_parse'( Cond, Atts, CondT) :-
	'KBS_cond_parse'( Cond, 'KB_TaT'(Atts), CondT).


'KBS_cond_parse'( true, _Rs, true) :- !.

'KBS_cond_parse'( false, _Rs, false) :- !.

'KBS_cond_parse'( not C, Rs, not( TC)) :-
	'KBS_cond_parse'( C, Rs, TC).

'KBS_cond_parse'( C1 and C2, Rs, and( TC1, TC2)) :-
	'KBS_cond_parse'( C1, Rs, TC1),
	'KBS_cond_parse'( C2, Rs, TC2).

'KBS_cond_parse'( C1 or C2, Rs, or( TC1, TC2)) :-
	'KBS_cond_parse'( C1, Rs, TC1),
	'KBS_cond_parse'( C2, Rs, TC2).

'KBS_cond_parse'( Att1 == Att2, Rs, eq(AttId1, AttId2)) :-
	'KB_is_attribute'( Rs, 1, Att1, AttId1),
	'KB_is_attribute'( Rs, 1, Att2, AttId2),
	!.

'KBS_cond_parse'( Att1 = Att2, Rs, eq(AttId1, AttId2)) :-
	%  similar to == but allowing for backtracking (no cut)
	'KB_is_attribute'( Rs, 1, Att1, AttId1),
	'KB_is_attribute'( Rs, 1, Att2, AttId2).

'KBS_cond_parse'( Att1 = Att2, Rs, and(eq(AttId1, REFX),diff(AttId2,REFX))) :-
	% deal with Ref tag in Att1
	% forced backtracking
	'KB_constant'(reference_hash, REFX),
	'KB_is_attribute'( Rs, 1, Att1, AttId1),
	'KB_is_attribute'( Rs, 1, Att2, AttId2).

'KBS_cond_parse'( Att1 = Att2, Rs, and(eq(AttId2, REFX),diff(AttId1,REFX))) :-
	% deal with Ref tag in Att2
	'KB_constant'(reference_hash, REFX),
	'KB_is_attribute'( Rs, 1, Att1, AttId1),
	'KB_is_attribute'( Rs, 1, Att2, AttId2), !.


'KBS_cond_parse'( Att < Cte, Rs, less(AttId, Cte)) :-
	number(Cte),
	'KB_is_attribute'( Rs, 1, Att, AttId), !.

'KBS_cond_parse'( Cte < Att, Rs, greater(AttId, Cte)) :-
	number(Cte),
	'KB_is_attribute'( Rs, 1, Att, AttId), !.

'KBS_cond_parse'( Att =< Cte, Rs, less_eq(AttId, Cte)) :-
	number(Cte),
	'KB_is_attribute'( Rs, 1, Att, AttId), !.

'KBS_cond_parse'( Cte =< Att, Rs, greater_eq(AttId, Cte)) :-
	number(Cte),
	'KB_is_attribute'( Rs, 1, Att, AttId), !.

'KBS_cond_parse'( Att > Cte, Rs, greater(AttId, Cte)) :-
	number(Cte),
	'KB_is_attribute'( Rs, 1, Att, AttId), !.

'KBS_cond_parse'( Cte > Att, Rs, less(AttId, Cte)) :-
	number(Cte),
	'KB_is_attribute'( Rs, 1, Att, AttId), !.

'KBS_cond_parse'( Att >= Cte, Rs, greater_eq(AttId, Cte)) :-
	number(Cte),
	'KB_is_attribute'( Rs, 1, Att, AttId), !.

'KBS_cond_parse'( Cte >= Att, Rs, less_eq(AttId, Cte)) :-
	number(Cte),
	'KB_is_attribute'( Rs, 1, Att, AttId), !.

'KBS_cond_parse'( Att == Cte, Rs, eq(AttId, Val)) :-
	'KB_is_attribute'( Rs, 1, Att, AttId),
	'KB_descrins'( Cte, Val), !.

'KBS_cond_parse'( Cte == Att, Rs, eq(AttId, Val)) :-
	'KB_is_attribute'( Rs, 1, Att, AttId),
	'KB_descrins'( Cte, Val), !.


'KBS_cond_parse'( Att = Cte, Rs, eq(AttId, Val)) :-
	%  similar to == but allowing for backtracking (no cut)
	'KB_is_attribute'( Rs, 1, Att, AttId),
	'KB_descrins'( Cte, Val).

'KBS_cond_parse'( Att = _Cte, Rs, eq(AttId, REFX)) :-
	% deal with Ref tag in Att
	'KB_constant'(reference_hash, REFX),
	'KB_is_attribute'( Rs, 1, Att, AttId), !.

'KBS_cond_parse'( Cte = Att, Rs, eq(AttId, Val)) :-
	%  similar to == but allowing for backtracking (no cut)
	'KB_is_attribute'( Rs, 1, Att, AttId),
	'KB_descrins'( Cte, Val).

'KBS_cond_parse'( _Cte = Att, Rs, eq(AttId, REFX)) :-
	% deal with Ref tag in Att
	'KB_constant'(reference_hash, REFX),
	'KB_is_attribute'( Rs, 1, Att, AttId), !.

% trap all other conditions -- test in main memory
'KBS_cond_parse'( _ @< _, _, true) :-  !.	
'KBS_cond_parse'( _ @> _, _, true) :-  !.	
'KBS_cond_parse'( _ @>= _, _, true) :-  !.	
'KBS_cond_parse'( _ @=< _, _, true) :-  !.	










/*
	'KB_flatten_src_tree'/4 - without projection
	'KB_flatten_src_tree'/7 - with projection

	From the condition part of a SELECTION expression in the
	relational algebra, it generates the conjuntion of
	conditions as seen in Prolog form.	The arguments are:
	R 	- relation name
	Cond 	- condition in its algebra form
	[Proj	- projection list in src form]
	Args	- the list of variables per attribute in the relation
	Xtras	- the (extra) conditions as seen by Prolog
	[VProj	- projection list in var form]
	[SrcPrj	- projection list in name form for result rel]

	Example:
		given employee/3 with format employee( Name, Dept, Salary)
		and the query

		X isdr employee where salary > 50 and dept == sales,

	the call to
		'KB_flatten_src_tree'(	employee,
					salary > 50 and dept == sales,
					Args,
					Xtras)
	will instantiate:
		Args to	[Name, Dept, Salary] and
		Xtras to (Dept == sales, Sal > 50)
*/

'KB_flatten_src_tree'( R, Cond, Args, Xtras) :-
	degree( R, Ary),
	length( Args, Ary),
	domains( R, S),
	'KB_flatten'(Cond, S, Args, Xtrs),
	'KB_simpl_flat_tree'(Xtrs, Xtras).




'KB_flatten_src_tree'( R, Cond, Proj, Args, Xtras, VProj, SrcPrj) :-
	degree( R, Ary),
	length( Args, Ary),
	domains( R, S),
	'KB_flatten'(Cond, S, Args, Xtrs),
	'KB_simpl_flat_tree'(Xtrs, Xtras),
	RS =.. [R|S],
	RArgs =.. [R|Args],
	'KB_project'( Proj, [R|S], RS, RArgs, VProj, SrcPrj).





/*  Flatten the condition tree (source) for selections */
'KB_flatten'( and( C1, C2), S, Args, ','(Xtras1, Xtras2)) :-
	'KB_flatten'( C1, S, Args, Xtras1),
	'KB_flatten'( C2, S, Args, Xtras2), !.
'KB_flatten'( LH == RH, S, Args, true) :-
	'KB_inst'( LH, RH, S, Args), !.
'KB_flatten'( LH == RH, S, Args, true) :-
	'KB_inst'( RH, LH, S, Args), !.
'KB_flatten'( LH = RH, S, Args, true) :-
	'KB_inst'( LH, RH, S, Args), !.
'KB_flatten'( LH = RH, S, Args, true) :-
	'KB_inst'( RH, LH, S, Args), !.
'KB_flatten'( LH < RH, S, Args, DLH @< DRH) :-
	'KB_inst_rank'( LH, RH, S, Args, DLH, DRH), !.
'KB_flatten'( LH < RH, S, Args, DLH @< DRH) :-
	'KB_inst_rank'( RH, LH, S, Args, DRH, DLH), !.
'KB_flatten'( LH > RH, S, Args, DLH @> DRH) :-
	'KB_inst_rank'( LH, RH, S, Args, DLH, DRH), !.
'KB_flatten'( LH > RH, S, Args, DLH @> DRH) :-
	'KB_inst_rank'( RH, LH, S, Args, DRH, DLH), !.
'KB_flatten'( LH =< RH, S, Args, DLH @=< DRH) :-
	'KB_inst_rank'( LH, RH, S, Args, DLH, DRH), !.
'KB_flatten'( LH =< RH, S, Args, DLH @=< DRH) :-
	'KB_inst_rank'( RH, LH, S, Args, DRH, DLH), !.
'KB_flatten'( LH >= RH, S, Args, DLH @>= DRH) :-
	'KB_inst_rank'( LH, RH, S, Args, DLH, DRH), !.
'KB_flatten'( LH >= RH, S, Args, DLH @>= DRH) :-
	'KB_inst_rank'( RH, LH, S, Args, DRH, DLH), !.
'KB_flatten'( LH @< RH, S, Args, DLH @< DRH) :-
	'KB_inst_rank'( LH, RH, S, Args, DLH, DRH), !.
'KB_flatten'( LH @< RH, S, Args, DLH @< DRH) :-
	'KB_inst_rank'( RH, LH, S, Args, DRH, DLH), !.
'KB_flatten'( LH @> RH, S, Args, DLH @> DRH) :-
	'KB_inst_rank'( LH, RH, S, Args, DLH, DRH), !.
'KB_flatten'( LH @> RH, S, Args, DLH @> DRH) :-
	'KB_inst_rank'( RH, LH, S, Args, DRH, DLH), !.
'KB_flatten'( LH @=< RH, S, Args, DLH @=< DRH) :-
	'KB_inst_rank'( LH, RH, S, Args, DLH, DRH), !.
'KB_flatten'( LH @=< RH, S, Args, DLH @=< DRH) :-
	'KB_inst_rank'( RH, LH, S, Args, DRH, DLH), !.
'KB_flatten'( LH @>= RH, S, Args, DLH @>= DRH) :-
	'KB_inst_rank'( LH, RH, S, Args, DLH, DRH), !.
'KB_flatten'( LH @>= RH, S, Args, DLH @>= DRH) :-
	'KB_inst_rank'( RH, LH, S, Args, DRH, DLH), !.
'KB_flatten'( _, _, _, true).
	/* No action needed -the correct answer was returned by Bang */



/* Instantiate arguments for selection */
'KB_inst'( Att, Cte, [Att| _], [ Cte| _]) :- !.
'KB_inst'( Att, Cte, [ _| Atts], [ _| Args]) :-
	'KB_inst'( Att, Cte, Atts, Args).

/* Select the variable and the constant to be compared */
'KB_inst_rank'( Att, Cte, [Att| _], [ Var| _], Var, Cte) :- !.
'KB_inst_rank'( Att, Cte, [ _| Atts], [ _| Args], Var, Cte) :-
	'KB_inst_rank'( Att, Cte, Atts, Args, Var, Cte).







/* simplify the flattened expression eliminating redundant 'true' conditions */
'KB_simpl_flat_tree'(','(true, Cond), NewC) :-
	'KB_simpl_flat_tree'(Cond, NewC), !.
'KB_simpl_flat_tree'(','(Cond, true), NewC) :-
	'KB_simpl_flat_tree'(Cond, NewC), !.
'KB_simpl_flat_tree'(','(','(C1, C2), C3), NC) :-
	% C3 \== true
	'KB_simpl_flat_tree'(','(C1, C2), NC1),
	NC1 \== and( C1, C2),
	'KB_simpl_flat_tree'( ','(NC1, C3), NC), !.
'KB_simpl_flat_tree'(','(C3, ','(C1, C2)), NC) :-
	% C3 \== true
	not functor(C3, ',', 2),
	'KB_simpl_flat_tree'(','(C1, C2), NC1),
	NC1 \== and( C1, C2),
	'KB_simpl_flat_tree'( ','(C3, NC1), NC), !.
'KB_simpl_flat_tree'(Cond, Cond) :- !.




/*
	'KB_project'( ProjExprL, RSL, RS, RArgs, VarProjList, NewSchema)

	ProjExprL - list of projection expressions as in query
	RSL - a list with [ SrcRelName| AttNamesList]
	RS - a structure with SrcRelName( AttNamesList)
	VarProjList - List of vars to match heads of new relation
	NewSchema - A list with the schema for the new relation

	Prepare the schema for the new relation
	and it prepares a list of vars to make its head.
*/

'KB_project'( [], _, _, _, [], []).

'KB_project'( [Att| MAtts], RSL, RS, RArgs, [Var|ProjL], [Name| MNames]) :-
	'KB_project1'( Att, RSL, RS, RArgs, Var, Name),
	'KB_project'( MAtts, RSL, RS, RArgs, ProjL, MNames).


'KB_project1'( =(Name, _Expr), _RSL, _RS, _RArgs, _Var, Name) :-
	% 'KB_proj1_expr'(Expr, RSL, RS, RArgs),
	 !.

'KB_project1'( +Att, RSL, RS, RArgs, Var, +Name) :-
	'KB_is_attr'(Att, RSL, [], _, No),
	arg( No, RArgs, Var),
	arg( No, RS, Name), !.

'KB_project1'( Att, RSL, RS, RArgs, Var, Name) :-
	'KB_is_attr'(Att, RSL, [], _, No),
	arg( No, RArgs, Var),
	arg( No, RS, Name), !.





'KB_projectj'( [], _, _, _, _, _, _, [], []).

'KB_projectj'( [Att| MAtts], RSL1, RS1, RArgs1, RSL2, RS2, RArgs2,
		[Var|ProjL], [Name| MNames]) :-
	'KB_project2'( Att, RSL1, RS1, RArgs1, RSL2, RS2, RArgs2, Var, Name),
	'KB_projectj'( MAtts, RSL1, RS1, RArgs1, RSL2, RS2, RArgs2, ProjL, MNames).


'KB_project2'( =(Name, _Expr), _RSL1, _RS1, _RArgs1, _RSL2, _RS2, _RArgs2, _Var, Name) :-
	% 'KB_proj2_expr'(Expr, RSL1, RS1, RArgs1, RSL2, RS2, RArgs2),
	 !.

'KB_project2'( +Att, RSL1, RS1, RArgs1, RSL2, RS2, RArgs2, Var, Name) :-
	'KB_is_attr'(Att, RSL1, RSL2, R, No), !,
	'KB_pick_proj'(R, No, RS1, RArgs1, RS2, RArgs2, Var, Name).

'KB_project2'( Att, RSL1, RS1, RArgs1, RSL2, RS2, RArgs2, Var, Name) :-
	'KB_is_attr'(Att, RSL1, RSL2, R, No), !,
	'KB_pick_proj'(R, No, RS1, RArgs1, RS2, RArgs2, Var, Name).



'KB_pick_proj'(1, No, RS1, RArgs1, _, _, Var, Name) :-
	arg( No, RArgs1, Var),
	arg( No, RS1, Name), !.

'KB_pick_proj'(2, No, _, _, RS2, RArgs2, Var, Name) :-
	arg( No, RArgs2, Var),
	arg( No, RS2, Name), !.







/*
	Generation and Testing of clauses for SELECTION

	'KB_gens_clauses'(...)
		generate result clauses without projection

	'KB_gensp_clauses'(...)
		generate result clauses with projection
*/



/*  Is there a Selection ?  -  there is NO Projection */
'KB_gens_clauses'( true, NewS, R, TempRes, Args, CpArgs, true, X) :- !,
	X <==> NewS,
	'KB_real_rname'( R, RR),
	Goal =.. [ RR | Args],
	Head =.. [ RR | CpArgs],
	NewHead =.. [ X | CpArgs],
	'KB_gens_clause'( R, TempRes, Goal, Head, NewHead).

'KB_gens_clauses'( Xtras, NewS, R, TempRes, Args, CpArgs, CpXtras, X) :- !,
	X <==> NewS,
	'KB_real_rname'( R, RR),
	Goal =.. [ RR | Args],
	Head =.. [ RR | CpArgs],
	NewHead =.. [ X | CpArgs],
	'KB_gensx_clause'( R, TempRes, Goal, Head, NewHead, Xtras, CpXtras).




/*  Is there a Selection ?  -  there is Projection */
'KB_gensp_clauses'( true, VProj, NewS, R, TempRes, Args, CpArgs, true, X) :- !,
	X <==> NewS,
	'KB_real_rname'( R, RR),
	Goal =.. [ RR | Args],
	Head =.. [ RR| CpArgs],
	NewHead =.. [ X| VProj],
	'KB_gens_clause'( R, TempRes, Goal, Head, NewHead).

'KB_gensp_clauses'( Xtras, VProj, NewS, R, TempRes, Args, CpArgs, CpXtras, X) :- !,
	X <==> NewS,
	'KB_real_rname'( R, RR),
	Goal =.. [ RR | Args],
	Head =.. [ RR | CpArgs],
	NewHead =.. [ X | VProj],
	'KB_gensx_clause'( R, TempRes, Goal, Head, NewHead, Xtras, CpXtras).



/*
	GENERATE a new clause for SELECTION

	KB_gens_clause(...)
		generate a new clause without extra args.

	KB_gensx_clause(...)
		generate a new clause with extra args.

*/

'KB_gens_clause'( _R, TempRes, Goal, OldHead, NewHead) :-
	retr_tup( TempRes, [OldClause | _ ]),  /* backtrack here */
	not(not('KB_exec_clause'(OldClause, Goal))),
	'KB_make_new_clause'(OldClause, OldHead, NewHead, NewClause),
	insert_clause( NewClause),
	fail.
'KB_gens_clause'( _, _, _, _, _).

'KB_gensx_clause'( _R, TempRes, Goal, OldHead, NewHead, Xtras, CpXtras) :-
	retr_tup( TempRes, [OldClause | _ ]),  /* backtrack here */
	not(not(('KB_exec_clause'(OldClause, Goal), Xtras))),
	'KB_make_new_clause'(OldClause, OldHead, NewHead, CpXtras, NewClause),
	insert_clause( NewClause),
	fail.
'KB_gensx_clause'( _, _, _, _, _, _, _).




'KB_make_new_clause'(':-'(OldHead, Body), OldHead, NewHead, ':-'(NewHead, Body)) :-
	% rule
	!.
'KB_make_new_clause'( Clause, Clause /*OldHead*/ , NewHead, NewHead).
	% fact
	% Clause = OldHead




'KB_make_new_clause'(':-'(OldHead, Body), OldHead, NewHead, Xtras, ':-'(NewHead,(Body,Xtras))) :-
	% rule
	!.
'KB_make_new_clause'( Clause, Clause, NewHead, Xtras, ':-'(NewHead, Xtras)).
	% fact
	% Clause = OldHead









/*
	'KB_jflatten_src_tree'/6 - without projection
	'KB_jflatten_src_tree'/9 - with projection

	From the condition part of a JOIN expression in the
	relational algebra, it generates the conjuntion of
	conditions as seen in Prolog form.	The arguments are:
	R1/2 	- relation names
	Cond 	- condition in its algebra form
	[Proj	- projection list in src form]
	Args1/2	- the list of variables per attribute in each relation
	Xtras	- the (extra) conditions as seen by Prolog
	[VProj	- projection list in var form]
	[SrcPrj	- the list of new names]

	Example:
		given employee/3 with format employee( Name, Dept, Salary)
		and the query

		X isdr employee where salary > 50 and dept == sales,

	the call to
		'KB_flatten_src_tree'(	employee,
					salary > 50 and dept == sales,
					Args,
					Xtras)
	will instantiate:
		Args to	[Name, Dept, Salary] and
		Xtras to (Dept == sales, Sal > 50)
*/

'KB_jflatten_src_tree'( R1, R2, Cond, Args1, Args2, Xtras) :-
	degree( R1, Ary1),
	length( Args1, Ary1),
	domains(R1, S1),
	degree( R2, Ary2),
	length( Args2, Ary2),
	domains(R2, S2),
	'KB_link_join_atts'( Cond, [R1| S1], [R2| S2], Args1, Args2),
	'KB_flatten'(Cond, [R1|S1], [R2|S2], Args1, Args2, Xtrs),
	'KB_simpl_flat_tree'(Xtrs, Xtras).


'KB_jflatten_src_tree'(R1, R2, Cond, Proj, Args1, Args2, Xtras, VProj, SrcPrj) :-
	degree( R1, Ary1),
	length( Args1, Ary1),
	domains(R1, S1),
	degree( R2, Ary2),
	length( Args2, Ary2),
	domains(R2, S2),
	'KB_link_join_atts'( Cond, [R1| S1], [R2| S2], Args1, Args2),
	'KB_flatten'(Cond, [R1|S1], [R2|S2], Args1, Args2, Xtrs),
	'KB_simpl_flat_tree'(Xtrs, Xtras),
	RS1 =.. [R1|S1],
	RArgs1 =.. [R1|Args1],
	RS2 =.. [R2|S2],
	RArgs2 =.. [R2|Args2],
	'KB_projectj'( Proj, [R1|S1], RS1, RArgs1, [R2|S2], RS2, RArgs2,
		VProj, SrcPrj).





/*
	'KB_link_join_atts'( Cond, RS1, RS2, Args1, Args2)

	unifies the joining variables in both relations.

	The arguments:
	Cond	- the condition as found in the algebra expression
	RS1, RS2 - the relation schemas in the form [Rel|AttNames]
	Args1, Args - the list of variables per attribute in each relation

	Example - given the relations
		employee( Name, Dept, Salary)
		dept( Dept, Location)
	and the algebra expression
		X isdr employee :*: dept
			where employee^dep == dept^dep_id
	the call to
		'KB_link_join_atts'( Att1 == Att2,
					[employee, name, dep, salary],
					[ dept, dep_id, location],
					[E1, E2, E3],
					[D1, D2])
	will instantiate E2 to D1, i.e. - E2 the variable standing for
	the attribute dep in employee to D1 - the variable standing for the
	attribute dep_id in dept
*/

'KB_link_join_atts'( Att1 == Att2, RS1, RS2, Args1, Args2) :-
	'KB_is_attr'( Att1, RS1, RS2, R1, Attno1),
	'KB_is_attr'( Att2, RS1, RS2, R2, Attno2),
	'KB_inst_join_att'( R1, R2, Attno1, Attno2, Args1, Args2), !.

'KB_link_join_atts'( Att1 = Att2, RS1, RS2, Args1, Args2) :-
	'KB_is_attr'( Att1, RS1, RS2, R1, Attno1),
	'KB_is_attr'( Att2, RS1, RS2, R2, Attno2),
	'KB_inst_join_att'( R1, R2, Attno1, Attno2, Args1, Args2), !.

'KB_link_join_atts'( and(C1, C2), RS1, RS2, Args1, Args2) :-
	'KB_link_join_atts'( C1, RS1, RS2, Args1, Args2),
	'KB_link_join_atts'( C2, RS1, RS2, Args1, Args2).

'KB_link_join_atts'( _, _, _, _, _) :- !.  % trap the selection conditions



/*  unifies the linking variables - even within the same relation */
'KB_inst_join_att'( 1, 1, Ano1, Ano2, Args1, _) :-
	RelFrame =.. ['KB_ints_rel'| Args1],
	arg( Ano1, RelFrame, X),
	arg( Ano2, RelFrame, Y),
	X = Y, !.
'KB_inst_join_att'( 2, 2, Ano1, Ano2, _, Args2) :-
	RelFrame =.. ['KB_ints_rel'| Args2],
	arg( Ano1, RelFrame, X),
	arg( Ano2, RelFrame, Y),
	X = Y, !.
'KB_inst_join_att'( 1, 2, Ano1, Ano2, Args1, Args2) :-
	RelFrame1 =.. ['KB_ints_rel1'| Args1],
	RelFrame2 =.. ['KB_ints_rel2'| Args2],
	arg( Ano1, RelFrame1, X),
	arg( Ano2, RelFrame2, Y),
	X = Y, !.
'KB_inst_join_att'( 2, 1, Ano1, Ano2, Args1, Args2) :-
	RelFrame1 =.. ['KB_ints_rel1'| Args1],
	RelFrame2 =.. ['KB_ints_rel2'| Args2],
	arg( Ano1, RelFrame2, X),
	arg( Ano2, RelFrame1, Y),
	X = Y, !.



/*  Flatten the condition tree (source) for joins
	-- the join attributes has already been dealt with */
'KB_flatten'( and( C1, C2), S1, S2, Args1, Args2, ','(Xtras1, Xtras2)) :-
	'KB_flatten'( C1, S1, S2, Args1, Args2, Xtras1),
	'KB_flatten'( C2, S1, S2, Args1, Args2, Xtras2), !.
'KB_flatten'( _R1^_Att1 == _R2^_Att2, _, _, _, _, true) :-
	 !.	%  ignore it -- join args
'KB_flatten'( _R1^_Att1 = _R2^_Att2, _, _, _, _, true) :-
	 !.	%  ignore it -- join args
'KB_flatten'( Att1 == Att2, S1, S2, _, _, true) :-
	'KB_is_attr'(Att1, S1, S2, _, _),
	'KB_is_attr'(Att2, S1, S2, _, _), !.	%  ignore it -- join args
'KB_flatten'( Att1 = Att2, S1, S2, _, _, true) :-
	'KB_is_attr'(Att1, S1, S2, _, _),
	'KB_is_attr'(Att2, S1, S2, _, _), !.	%  ignore it -- join args
'KB_flatten'( Att == Cte, S1, S2, Args1, Args2, true) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst'( R, No, Cte, Args1, Args2), !.
'KB_flatten'( Cte == Att, S1, S2, Args1, Args2, true) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst'( R, No, Cte, Args1, Args2), !.
'KB_flatten'( Att = Cte, S1, S2, Args1, Args2, true) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst'( R, No, Cte, Args1, Args2), !.
'KB_flatten'( Cte = Att, S1, S2, Args1, Args2, true) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst'( R, No, Cte, Args1, Args2), !.
'KB_flatten'( Att1 < Att2, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att1, S1, S2, R1, No1),
	'KB_is_attr'(Att2, S1, S2, R2, No2),
	'KB_inst_atts_rank'( R1, No1, R2, No2, '@<', Args1, Args2, Xtra), !.
'KB_flatten'( Att < Cte, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@<', Args1, Args2, Xtra), !.
'KB_flatten'( Cte < Att, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@>', Args1, Args2, Xtra), !.
'KB_flatten'( Att1 > Att2, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att1, S1, S2, R1, No1),
	'KB_is_attr'(Att2, S1, S2, R2, No2),
	'KB_inst_atts_rank'( R1, No1, R2, No2, '@>', Args1, Args2, Xtra), !.
'KB_flatten'( Att > Cte, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@>', Args1, Args2, Xtra), !.
'KB_flatten'( Cte > Att, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@<', Args1, Args2, Xtra), !.
'KB_flatten'( Att1 =< Att2, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att1, S1, S2, R1, No1),
	'KB_is_attr'(Att2, S1, S2, R2, No2),
	'KB_inst_atts_rank'( R1, No1, R2, No2, '@=<', Args1, Args2, Xtra), !.
'KB_flatten'( Att =< Cte, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@=<', Args1, Args2, Xtra), !.
'KB_flatten'( Cte =< Att, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@>=', Args1, Args2, Xtra), !.
'KB_flatten'( Att1 >= Att2, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att1, S1, S2, R1, No1),
	'KB_is_attr'(Att2, S1, S2, R2, No2),
	'KB_inst_atts_rank'( R1, No1, R2, No2, '@>=', Args1, Args2, Xtra), !.
'KB_flatten'( Att >= Cte, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@>=', Args1, Args2, Xtra), !.
'KB_flatten'( Cte >= Att, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@=<', Args1, Args2, Xtra), !.
'KB_flatten'( Att1 @< Att2, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att1, S1, S2, R1, No1),
	'KB_is_attr'(Att2, S1, S2, R2, No2),
	'KB_inst_atts_rank'( R1, No1, R2, No2, '@<', Args1, Args2, Xtra), !.
'KB_flatten'( Att @< Cte, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@<', Args1, Args2, Xtra), !.
'KB_flatten'( Cte @< Att, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@>', Args1, Args2, Xtra), !.
'KB_flatten'( Att1 @> Att2, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att1, S1, S2, R1, No1),
	'KB_is_attr'(Att2, S1, S2, R2, No2),
	'KB_inst_atts_rank'( R1, No1, R2, No2, '@>', Args1, Args2, Xtra), !.
'KB_flatten'( Att @> Cte, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@>', Args1, Args2, Xtra), !.
'KB_flatten'( Cte @> Att, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@<', Args1, Args2, Xtra), !.
'KB_flatten'( Att1 @=< Att2, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att1, S1, S2, R1, No1),
	'KB_is_attr'(Att2, S1, S2, R2, No2),
	'KB_inst_atts_rank'( R1, No1, R2, No2, '@=<', Args1, Args2, Xtra), !.
'KB_flatten'( Att @=< Cte, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@=<', Args1, Args2, Xtra), !.
'KB_flatten'( Cte @=< Att, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@>=', Args1, Args2, Xtra), !.
'KB_flatten'( Att1 @>= Att2, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att1, S1, S2, R1, No1),
	'KB_is_attr'(Att2, S1, S2, R2, No2),
	'KB_inst_atts_rank'( R1, No1, R2, No2, '@>=', Args1, Args2, Xtra), !.
'KB_flatten'( Att @>= Cte, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@>=', Args1, Args2, Xtra), !.
'KB_flatten'( Cte @>= Att, S1, S2, Args1, Args2, Xtra) :-
	'KB_is_attr'(Att, S1, S2, R, No),
	'KB_inst_rank'( R, No, Cte, '@=<', Args1, Args2, Xtra), !.




'KB_is_attr'(Rel^Att, [Rel|S1], _, 1, No) :-
	'KB_in_pos'( S1, 1, Att, No), !.
'KB_is_attr'(Rel^Att, _, [Rel|S2], 2, No) :-
	'KB_in_pos'( S2, 1, Att, No), !.
'KB_is_attr'(Att, [_|S1], _, 1, No) :-
	atom(Att),
	'KB_in_pos'( S1, 1, Att, No), !.
'KB_is_attr'(Att, _, [_|S2], 2, No) :-
	atom(Att),
	'KB_in_pos'( S2, 1, Att, No), !.


'KB_in_pos'( [], _, _, _) :-
	fail, !.
'KB_in_pos'( [Att| _], I, Att, I) :- !.
'KB_in_pos'( [_| MAtts], I, Att, No) :-
	I1 is I + 1,
	'KB_in_pos'( MAtts, I1, Att, No).

	

/* Instantiate arguments for join */
'KB_inst'( 1, Ano, Cte, Args1, _) :-
	RelFrame =.. ['KB_ints_rel'| Args1],
	arg( Ano, RelFrame, X),
	X = Cte, !.
'KB_inst'( 2, Ano, Cte, _, Args2) :-
	RelFrame =.. ['KB_ints_rel'| Args2],
	arg( Ano, RelFrame, X),
	X = Cte, !.



/* Prepare extra conditions for joins  -
	matches the variable to the attribute and builds Op(Var, Cte) */
'KB_inst_rank'( 1, Ano, Cte, Op, Args1, _, Xcond) :-
	RelFrame =.. ['KB_ints_rel'| Args1],
	arg( Ano, RelFrame, X),
	Xcond =.. [Op, X, Cte], !.
'KB_inst_rank'( 2, Ano, Cte, Op, _, Args2, Xcond) :-
	RelFrame =.. ['KB_ints_rel'| Args2],
	arg( Ano, RelFrame, X),
	Xcond =.. [Op, X, Cte], !.


'KB_inst_atts_rank'( 1, Ano1, 1, Ano2, Op, Args1, _, Xcond) :-
	RelFrame =.. ['KB_ints_rel'| Args1],
	arg( Ano1, RelFrame, X1),
	arg( Ano2, RelFrame, X2),
	Xcond =.. [Op, X1, X2], !.

'KB_inst_atts_rank'( 2, Ano1, 2, Ano2, Op, _, Args2, Xcond) :-
	RelFrame =.. ['KB_ints_rel'| Args2],
	arg( Ano1, RelFrame, X1),
	arg( Ano2, RelFrame, X2),
	Xcond =.. [Op, X1, X2], !.

'KB_inst_atts_rank'( 1, Ano1, 2, Ano2, Op, Args1, Args2, Xcond) :-
	RelFrame1 =.. ['KB_ints_rel'| Args1],
	RelFrame2 =.. ['KB_ints_rel'| Args2],
	arg( Ano1, RelFrame1, X1),
	arg( Ano2, RelFrame2, X2),
	Xcond =.. [Op, X1, X2], !.

'KB_inst_atts_rank'( 2, Ano1, 1, Ano2, Op, Args1, Args2, Xcond) :-
	RelFrame1 =.. ['KB_ints_rel'| Args2],
	RelFrame2 =.. ['KB_ints_rel'| Args1],
	arg( Ano1, RelFrame1, X1),
	arg( Ano2, RelFrame2, X2),
	Xcond =.. [Op, X1, X2], !.





/*
	Generation and Testing of clauses for JOIN

	'KB_genj_clauses'(...)
		generate result clauses without projection

	'KB_genjp_clauses'(...)
		generate result clauses with projection
*/

/*  There is Projection -  Is there Selection ? */
'KB_genjp_clauses'( true, VProj, NewS, R1, R2, TempRes, Args1, Args2,
		CpArgs1, CpArgs2, true, X) :- !,
		X <==> NewS,
	'KB_real_rname'( R1, RR1),
	'KB_real_rname'( R2, RR2),
	Goal1 =.. [ RR1 | Args1],
	Goal2 =.. [ RR2 | Args2],
	OldH1 =.. [ RR1| CpArgs1],
	OldH2 =.. [ RR2| CpArgs2],
	NewH =.. [ X| VProj],
	'KB_genj_clause'( R1, R2, TempRes, Goal1, Goal2, OldH1, OldH2, NewH, X).

'KB_genjp_clauses'( Xtras, VProj, NewS, R1, R2, TempRes, Args1, Args2,
		CpArgs1, CpArgs2, CpXtras, X) :- !,
	X <==> NewS,
	'KB_real_rname'( R1, RR1),
	'KB_real_rname'( R2, RR2),
	Goal1 =.. [ RR1 | Args1],
	Goal2 =.. [ RR2 | Args2],
	OldH1 =.. [ RR1| CpArgs1],
	OldH2 =.. [ RR2| CpArgs2],
	NewH =.. [ X| VProj],
	'KB_genjx_clause'( R1, R2, TempRes, Goal1, Goal2,
		OldH1, OldH2, NewH, Xtras, CpXtras, X).




/*  There is NO Projection -  Is there Selection ? */
'KB_genj_clauses'( true, R1, R2, TempRes, Args1, Args2,
		CpArgs1, CpArgs2, true, X) :- !,
	R1 <==> S1,
	R2 <==> S2,
	append( S1, S2, S),
	X <==> S,
	'KB_real_rname'( R1, RR1),
	'KB_real_rname'( R2, RR2),
	Goal1 =.. [ RR1 | Args1],
	Goal2 =.. [ RR2 | Args2],
	OldH1 =.. [ RR1| CpArgs1],
	OldH2 =.. [ RR2| CpArgs2],
	append( CpArgs1, CpArgs2, CpArgs),
	NewH =.. [ X| CpArgs],
	'KB_genj_clause'( R1, R2, TempRes, Goal1, Goal2, OldH1, OldH2, NewH, X).

'KB_genj_clauses'( Xtras, R1, R2, TempRes, Args1, Args2,
		CpArgs1, CpArgs2, CpXtras, X) :- !,
	R1 <==> S1,
	R2 <==> S2,
	append( S1, S2, S),
	X <==> S,
	'KB_real_rname'( R1, RR1),
	'KB_real_rname'( R2, RR2),
	Goal1 =.. [ RR1 | Args1],
	Goal2 =.. [ RR2 | Args2],
	OldH1 =.. [ RR1| CpArgs1],
	OldH2 =.. [ RR2| CpArgs2],
	append( CpArgs1, CpArgs2, CpArgs),
	NewH =.. [ X| CpArgs],
	'KB_genjx_clause'( R1, R2, TempRes, Goal1, Goal2,
		OldH1, OldH2, NewH, Xtras, CpXtras, X).






/*
	GENERATE a new clause for JOIN

	KB_genj_clause(...)
		generate a new clause without extra args.

	KB_genjx_clause(...)
		generate a new clause with extra args.

*/

/* Rules in Join without extra tests */
'KB_genj_clause'( _R1, _R2, TempRes, Goal1, Goal2, OldHead1, OldHead2, NewHead, _X) :-
	retr_tup( TempRes, [Clause1, Clause2 | _ ]),
	/* EXECUTE it  */
	not(not( ('KB_exec_clause'(Clause1, Goal1),
                  'KB_exec_clause'(Clause2, Goal2) ))),
	'KB_make_new_clause2'(Clause1, Clause2, OldHead1, OldHead2,
		NewHead, NewClause),
	insert_clause( NewClause),
	fail.
'KB_genj_clause'( _, _, _, _, _, _, _, _, _).

/* Rules in Join with extra tests */
'KB_genjx_clause'( _R1, _R2, TempRes, Goal1, Goal2, OldHead1, OldHead2,
    NewHead, Xtras, CpXtras, _X) :-
	retr_tup( TempRes, [Clause1, Clause2 | _ ]),
	/* EXECUTE it  */
	not(not( ('KB_exec_clause'(Clause1, Goal1),
                  'KB_exec_clause'(Clause2, Goal2),
                  Xtras ))),
	'KB_make_new_clause2'(Clause1, Clause2, OldHead1, OldHead2,
		 NewHead, CpXtras, NewClause),
	insert_clause( NewClause),
	fail.
'KB_genjx_clause'( _, _, _, _, _, _, _, _, _, _, _).





'KB_make_new_clause2'(Clause1, Clause2, OldHead1, OldHead2, NewHead, NewClause) :-
	% rule
	Clause1 =  ':-'(OldHead1, Body),
	!,
	'KB_make_new_clause'( Clause2, OldHead2, NewHead, Body, NewClause).

'KB_make_new_clause2'(Clause1, Clause2, OldHead1, OldHead2, NewHead, NewClause) :-
	% rule
	Clause2 =  ':-'(OldHead2, Body),
	!,
	'KB_make_new_clause'( Clause1, OldHead1, NewHead, Body, NewClause).

'KB_make_new_clause2'( Clause1, Clause2, Clause1, Clause2, NewHead, NewHead).
	% facts Clause1 and Clause 2
	% Clause1 = OldHead1, Clause2 = OldHead2





'KB_make_new_clause2'(Clause1, Clause2, OldH1, OldH2, NewHead, Xtra, NewClause) :-
	% rule
	Clause1 =  ':-'(OldH1, Body),
	!,
	'KB_make_new_clause'( Clause2, OldH2, NewHead, ','(Body, Xtra), NewClause).

'KB_make_new_clause2'(Clause1, Clause2, OldH1, OldH2, NewHead, Xtra, NewClause) :-
	% rule
	Clause2 =  ':-'(OldH2, Body),
	!,
	'KB_make_new_clause'( Clause1, OldH1, NewHead, ','(Body, Xtra), NewClause).

'KB_make_new_clause2'( Clause1, Clause2, Clause1, Clause2,
		NewHead, Xtra, ':-'(NewHead, Xtra)).
	% facts Clause1 and Clause 2
	% Clause1 = OldHead1, Clause2 = OldHead2
	


/*
	Selection Pre-Unification
*/

'KBX_select_univ_noproj'( Rel, [CondT| MoreCondTs], X) :-
	'KB_real_rname'( Rel, RR),
	% bang_select( RR, CondT, [att(1,1)], X),
	bang_select( RR, CondT, [], X),
	'KBX_select_univ_noproj_more'( RR, MoreCondTs, X),
	bang_format( RR, _F).



'KBX_select_univ_noproj_more'( _, [], _).

'KBX_select_univ_noproj_more'( RR, [CondT| MoreCondTs], X) :-
	% bang_select( RR, CondT, [att(1,1)], X),
	bang_select( RR, CondT, [], X),
	'KBX_select_univ_noproj_more'( RR, MoreCondTs, X).







/*
	Join Pre-Unification
*/

'KBX_join2_univ_noproj'( R1, R2, CondTs, X) :-
	'KB_real_rname'( R1, RR1),
	'KB_real_rname'( R2, RR2),
	'KBX_do_2_univ_join'( X, RR1, RR2, CondTs),
	/* inherit the attributes from R1 concatenated with R2 */
	R1 <=> F1,
	R2 <=> F2,
	append( F1, F2, _F).

/* seems to be unused [joachim]
'KBX_join2_univ_proj'( R1, R2, _CondT, ProjL, X) :-
	'KB_real_rname'( R1, RR1),
	'KB_real_rname'( R2, RR2),
	'KBX_do_2_univ_join'( X, RR1, RR2, _CondTs),
	'KB_project'( [R1, R2], ProjL, _CProjL).
	% 'KB_mk_temp_rel_format'( X, 1, ProjL, Atts_Temp).
*/



'KBX_do_2_univ_join'( NewR, RR1, RR2, [CondT| MoreCondTs]) :-
	%  There is always at least 1 condition tree
	% bang_join( RR1, RR2, CondT, [att(1,1),att(2,1)], NewR),
	bang_arity(RR1,Arity1),
	bang_arity(RR2,Arity2),
	'KBX_do_2_univ_join_mk_proj'(Arity1,Arity2,ProjList),
	bang_join( RR1, RR2, CondT, ProjList, NewR),
	'KBX_do_2_univ_join_more'( NewR, RR1, RR2, MoreCondTs).



'KBX_do_2_univ_join_more'( _, _, _, []).

'KBX_do_2_univ_join_more'( NewR, RR1, RR2, [CondT| MoreCondTs]) :-
	% bang_join( RR1, RR2, CondT, [att(1,1), att(2,1)], NewR),
	bang_arity(RR1,Arity1),
	bang_arity(RR2,Arity2),
	'KBX_do_2_univ_join_mk_proj'(Arity1,Arity2,ProjList),
	bang_join( RR1, RR2, CondT, ProjList, NewR),
	'KBX_do_2_univ_join_more'( NewR, RR1, RR2, MoreCondTs).

'KBX_do_2_univ_join_mk_proj'(N1,N2,[att(1,1), att(2,1) | R]) :-
	'KBX_do_2_univ_join_mk_proj1'(N1,[],R1,1),
	'KBX_do_2_univ_join_mk_proj1'(N2,[],R2,2),
	append(R1,R2,R).

'KBX_do_2_univ_join_mk_proj1'(1,R,R,_) :- !.
'KBX_do_2_univ_join_mk_proj1'(N,RI,RO,X) :-
	N1 is N - 1,
	'KBX_do_2_univ_join_mk_proj1'(N1,[att(X,N)|RI],RO,X).





/*	EXPAND a deductive relation */

'KB_freeze_extension'( Rel, DRRel, Ary) :-
	length( Args, Ary),
	Goal =.. [DRRel | Args],
	Fact =.. [Rel | Args],
	retr_tup( DRRel, [Clause | _]),  /* backtrack here */
	/*  EXECUTE it.			Backtrack here	*/
	'KB_exec_clause'(Clause, Goal),
	% insert_clause(Fact),
	'KB_store_cl_random'( Rel, Args, Ary, Fact),
	fail.
'KB_freeze_extension'(_, _, _).








/*	--- [KB.G.3]	Support ---	*/

/*  Print the extension of a derived relation on the user's screen */
display_x( Rel) :-
	'KB_real_rname'( Rel, RRel),
	bang_arity( RRel, Arity),
	RRel <==> Format,
	Ary is Arity - 1,  % Arity - 1 acc. for red tape
	'KB_print_header_rel'( Rel, RRel, Ary, Format),
	'KB_print_tups_drel'( RRel, Ary),
	'KB_print_tail_x'( RRel).

/* Not in USE
display_x( N, Rel) :-
	'KB_real_rname'( Rel, RRel),
	bang_arity( RRel, Arity),
	RRel <==> Format,
	Ary is Arity - 1,  % Arity - 1 acc. for red tape
	'KB_print_header_rel'( Rel, RRel, Ary, Format),
	'KB_print_tups_drel'( RRel, Ary, N),
	'KB_print_tail_x'( RRel).
*/



/*  Print a derived relation on the user's screen */
display_dr( Rel) :-
	'KB_real_rname'( Rel, RRel),
	bang_arity( RRel, Arity),
	RRel <==> Format,
	Ary is Arity - 1,  % Arity - 1 acc. for red tape
	'KB_print_header_rel'( Rel, RRel, Ary, Format),
	'KB_print_clauses'( RRel, Ary),
	'KB_print_tail_drel'( RRel).


/*  Print a derived relation on the user's screen using increments of N facts */
display_dr( N, Rel) :-
	'KB_real_rname'( Rel, RRel),
	bang_arity( RRel, Arity),
	RRel <==> Format,
	Ary is Arity - 1,  % Arity - 1 acc. for red tape
	'KB_print_header_rel'( Rel, RRel, Ary, Format),
	'KB_print_clauses'( RRel, Ary, N),
	'KB_print_tail_drel'( RRel).


'KB_print_clauses'( RRel, _Ary) :-
	write( 'CLAUSES :'), nl,
	setval( tuples, 0),
	retr_tup( RRel, [ Term | _]),  /* backtrack here */
	'KB_display_clause'( Term),
	incval( tuples),
	fail.
'KB_print_clauses'(_, _).

'KB_print_clauses'( RRel, _Ary, N) :-
	write( 'CLAUSES :'), nl,
	setval( tuples, 0),
	retr_tup( RRel, [ Term | _]),  /* backtrack here */
	'KB_display_clause'( Term),
	incval( tuples),
	getval( tuples, Card),
	Incr is Card mod N,
	'KB_ask_more'(Incr).
'KB_print_clauses'(_, _, _).

'KB_print_tups_drel'( RRel, Ary) :-
	write( 'CLAUSES :'), nl,
	functor(Goal, RRel, Ary),
	setval( tuples, 0),
	retr_tup( RRel, [Clause | _]),  /* backtrack here */
	/*  EXECUTE it. 		Backtrack here	*/
	'KB_exec_clause'(Clause, Goal),
	write( Goal), writeln('.'),
	incval( tuples),
	fail.
'KB_print_tups_drel'(_, _).





'KB_print_tail_x'( _RRel) :-
	getval( tuples, Card),
	erase_array( tuples/0),
	write('\nNUMBER OF CLAUSES : '), write(Card), nl.



'KB_print_tail_drel'( RRel) :-
	bang_cardinality( RRel, Card),
	erase_array( tuples/0),
	write('\nNUMBER OF CLAUSES : '), write(Card), nl.


'KB_ask_more'(0) :-
	/*  Fail when the input character read is 'y' or 'Y' */
	write('More (y/n) ?  '),
	get_char(C),
	C \== "y",
	C \== "Y".


'KB_display_clause'( Term) :-
	% rule
	Term =  ':-'(Head, Body),
	!,
	write(Head),
	writeln(' :-'),
	'KB_write_body'(Body).

'KB_display_clause'( Term) :-
	% fact
	!,
	write(Term),
	writeln('.').


'KB_write_body'(','(Goal, MGoals)) :-
	write('    '),
	write( Goal),
	writeln(','),
	!,
	'KB_write_body'(MGoals).

'KB_write_body'(Goal) :-
	!,
	write('    '),
	write( Goal),
	writeln('.').

