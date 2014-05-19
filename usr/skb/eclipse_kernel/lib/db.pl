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
% Copyright (C) 1992-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: db.pl,v 1.1 2008/06/30 17:43:45 jschimpf Exp $
% ----------------------------------------------------------------------

/*******************************************************************

File   : db.pl
Author : Jorge Bocca, Michael Dahmen, Michael Stillger
Content: The Relational Algebra of MegaLog

Note   : This file is for the MegaLog-Sepia integration. It was forked
	 from "@(#)rel_alg.pl        1.10         2/26/92"

	 The file has been modified such that the MegaLog compatibility 
	 package for Sepia (megalog.pl) is not needed.

Usage  : In client module do

:- use_module(library(db)).

*******************************************************************/

:- module_interface(db).

:-	op(950, yfx, 'ondb'),	% db handle specification
	op(900, xfx, '<=>'),	% schema declare/query
	op(900, xfx, '<@>'),	% schema compatibility
	op(900, xfx, '<->'),	% frame declare/query
	op(900, xfx, 'isr'),	% relational algebra query sub-language
	op(900, xfx, '<++'),	% relational algebra insert sub-language
	op(900, xfx, '<--'),	% relational algebra delete sub-language
	op(900, xfx, '++>'),	% relational algebra set retrieval
	op(800, yfx, ':^:'),	% projection
	op(750, yfx, 'where'),	% restriction
	op(750, yfx, ':*:'),	% join - 2
	op(750, yfx, ':+:'),	% union
	op(750, yfx, ':-:'),	% difference
	op(720, yfx, 'and'),	% in condition - and
	op(720, yfx, 'or').	% in condition - or


:- begin_module(db).

:- use_module(library(database_kernel)).

:- pragma(system).
:- pragma(nodebug).

:- tool((ondb)/2, (ondb)/3).

:- export (<=>)/2, (<@>)/2, (<->)/2, (isr)/2, (<++)/2, (<--)/2, (++>)/2.

:- export 
   (ondb)/2, (ondb)/3,
   arity/2, cardinality/2, del_tup/1, del_tup/2, del_tup/3,
   helpdb/0, helprel/1, ins_tup/1, ins_tup/2, printrel/1,
   rename_attributes/2, rename_relation/2, 
   retr_tup/1, retr_tup/2, retr_tup/3.

:- export 	/* for module kb only */
   'KB_real_rname'/2, 'KB_get_all_formats'/2, 'KB_is_attribute'/4, 
   'KB_print_header_rel'/4.

:- import import_body/2, is_predicate_/2 from sepia_kernel.

/*
** Modules and meta calls :
**
** This file defines no predicate that uses meta calls
*/

/* not a Sepia predicate, but often used */
list([_|_]) ?- true.

/* make frame for descriptor */
?- dynamic 'KB_synonym'/2.


/*
**	Synonym
**
**	A way of using pseudo names for relations.
**
**	expr	<-> []		==> remove all synonyms
**	expr	<-> atom	==> add a synonym
**	expr	<-> var		==> enumerate all synonyms
**	var	<-> atom	==> get real name (only one)
**
**	where expr is atom  old(atom)  new(atom)
*/

/* remove all 'KB_synonym'(s) of a real relation name */
<->( Rel, Synonym) :-
	<->( Rel, Synonym, 0).

<->( Rel, Synonym, _Database) :-
	Synonym == [],
	version_free_name( Rel, _),
	!,
	retract_all( 'KB_synonym'( _, Rel)), 
	!.

/* set up a synonym */
<->( Rel, Synonym, Database) :-
	atom(Synonym),
	version_free_name( Rel, RRel),
	!,
	bang_existrel_db(RRel, Database),
	retract_all( 'KB_synonym'( Synonym, _)),
	assert( 'KB_synonym'( Synonym, Rel)), 
	!.

/* obtain 'KB_synonym'(s) of a real relation name */
<->( Rel, Synonym, Database) :-
	var(Synonym),
	version_free_name( Rel, RRel),
	!,
	bang_existrel_db(RRel, Database),
	'KB_synonym'( Synonym, Rel).

/* obtain real name for a given synonym */
<->( Rel, Synonym, _Database) :-
	var(Rel),
	atom(Synonym),
	'KB_synonym'( Synonym, Rel), 
	!.

/*	--- Support ---	*/


/* find the name which is free of old, new functor */

version_free_name( old(Rel), Rel) :- atom(Rel).
version_free_name( new(Rel), Rel) :- atom(Rel).
version_free_name( Rel, Rel) :- atom(Rel).

/* obtain real name of a relation i.e. resolves synonyms */
/* does not check whether relation really exists */
/* performs a syntactically check */

'KB_real_rname'( old(Rel), old(RRel)) :-
	'KB_real_rname1'( Rel, RRel),
	!.
'KB_real_rname'( new(Rel), new(RRel)) :-
	'KB_real_rname1'( Rel, RRel),
	!.
'KB_real_rname'( Rel, RRel) :-
	'KB_real_rname1'( Rel, RRel).
	

'KB_real_rname1'( Synonym, Rel) :-
	'KB_synonym'( Synonym, Rel), 
	!.
'KB_real_rname1'( Rel, Rel) :-
	atom(Rel).



/*
**	Schema querying
*/

/* Print the names of all relations in the database. */
 
helpdb :- helpdb(0).

helpdb(Database) :-
	writeln("Permanent relations in database"),
	writeln("-------------------------------"),
	current_relation(Name/Arity, Database),
	writeln(Name/Arity),
	fail.
helpdb(Database) :-
	nl,
	writeln("Temporary relations in database"),
	writeln("-------------------------------"),
	current_temp_relation(Name/Arity, Database),
	writeln(Name/Arity),
	fail.
helpdb(_Database).

/* Give info about the schema of relation 'Rel' */
helprel (Rel) :- helprel(Rel, 0).

helprel( Rel, Database ) :-
	'KB_real_rname'( Rel, RRel1),
	bang_arity_db( RRel1, Arity, Database),
	version_free_name(RRel1, RRel2),
	 <=>( RRel2, Format,Database),
	'KB_print_header_rel'( Rel, RRel2, Arity, Format),
	'KB_print_tail_rel'( RRel1, Database).

/* Give info about the schema of relation 'Rel' and print content */
printrel( Rel) :- printrel( Rel, 0).

printrel( Rel, Database) :-
	'KB_real_rname'( Rel, RRel1),
	bang_arity_db( RRel1, Arity, Database),
	version_free_name(RRel1, RRel2),
	<=>( RRel2, Format, Database),
	'KB_print_header_rel'( Rel, RRel2, Arity, Format),
	'KB_print_tups_rel'( RRel1, Database),
	'KB_print_tail_rel'( RRel1, Database).


/* arity of relation */
arity(Rel, Arity) :-
	arity( Rel, Arity, 0).

arity( Rel, Arity, Database) :-
	'KB_real_rname'( Rel, RRel),
	bang_arity_db( RRel, Arity, Database).

/* cardinality of relation */
cardinality( Rel, Ntups) :-
	cardinality( Rel, Ntups, 0).

cardinality( Rel, Ntups, Database) :-
	'KB_real_rname'( Rel, RRel),
	bang_cardinality_db( RRel, Ntups, Database).


/*	--- Support ---	*/

'KB_print_header_rel'( Rel, RRel, Arity, Format) :-
	nl, nl,
	write( 'RELATION : '),
	write(Rel), write('    [real name: '), write(RRel), write(']'), nl, nl,
	write('ARITY: '), write(Arity), nl, nl,
	write('ATTRIBUTES :'), nl,
	'KB_print_atts'(Format), nl.

'KB_print_atts'([]) :- !.
'KB_print_atts'([Format | MFs]) :-
	write('    '), write(Format), nl,
	'KB_print_atts'( MFs).

'KB_print_tups_rel'( RRel, Database) :-
	write( 'TUPLES :'), nl, !,
	'KB_print_tat'( RRel, Database), nl.

'KB_print_tat'( RRel, Database) :-
	retr_tup_db( RRel, Tup, Database), write('    '), write( Tup), nl, fail.
'KB_print_tat'( _, _Database) :-  !.

/* 'KB_print_tail_rel' is only used in helprel, printrel */
'KB_print_tail_rel'( RRel, Database) :-
	bang_cardinality_db( RRel, Cardinality, Database),
	write('NUMBER OF TUPLES : '), write(Cardinality), nl, nl.



/*
**	Create/Format/Remove relation
**
**	atom	<=> list	==> verify format if relation exists
**	atom	<=> list	==> create permanent relation
**	var	<=> list	==> create temporary relation
**	atom	<=> var		==> obtain relation's format
**	atom	<=> []		==> remove relation
*/

/* verify relation's format if relation exist */
<=>( Rel, Format) :-
	<=>( Rel, Format, 0).

<=>( Rel, Format, Database) :-
	atom(Rel),
	list(Format),
	'KB_real_rname'(Rel, RRel),		% fails silienty
	bang_existrel_db(RRel, Database),
	!,
	bang_format_db( RRel, Format, Database).

/* create permanent relation */
<=>( Rel, Format, Database) :-
	atom(Rel),
	list(Format),
	!,
	/* Note : bang_createrel/3 checks format for
		- format is a list
		- each list element has functor integer/3, real/3, atom/3
		- attribute name is atom or string
		- length is legal integer or variable
		- key is '+', '-' or variable
	   It's not checked that all attribute names are different
	*/
	bang_createrel_db( Rel, Format, [permanent], Database).

/* create temporary relation */
<=>( Rel, Format, Database) :-
	var(Rel),
	list(Format),
	!,
	/* Note : bang_createrel/3 checks format (see above) */
	bang_createrel_db( Rel, Format, [temporary], Database).

/* obtain relation's format */
<=>( Rel, Format, Database) :-
	atom(Rel),
	var(Format),
	!,
	'KB_real_rname'(Rel, RRel),
	bang_format_db( RRel, Format, Database).

/* remove relation */
<=>( Rel, [], Database) :-
	atom(Rel),
	'KB_real_rname'(Rel, RRel),
	bang_destroyrel_db( RRel, Database),
	retract_all('KB_synonym'( _, RRel)), 
	!.


/*	--- Support ---	*/

/* Internal formats  for a list of real or pseudo relations is used */

'KB_get_all_formats'( MRels,  MRFs) :-
	'KB_get_all_formats'( MRels, MRFs, 0).



'KB_get_all_formats'( [], [], _Database) :- !.

'KB_get_all_formats'( [Rel | MRels], [Rel, RRF| MRFs], Database) :-
	'KB_real_rname'( Rel, RR),
	bang_format_db( RR, RRF, Database),
	'KB_get_all_formats'( MRels, MRFs, Database).


/*
**	Schema Compatibility
**
**	R1 and R2 have combatible schemas iff there corresponding
**	attributes have same type and length.
*/

<@>( Rel1, Rel2) :-
	<@>( Rel1, Rel2, 0).

<@>( Rel1, Rel2,Database) :-
	'KB_real_rname'( Rel1, RRel11),
	version_free_name(RRel11, RRel12),
	'KB_real_rname'( Rel2, RRel21),
	version_free_name(RRel21, RRel22),
	<=> (RRel12, F1, Database),		% get format for Rel1
	<=> (RRel22, F2, Database),		% get format for Rel2
	'KB_compatible_formats'( F1, F2),
	!.



/*	--- Support ---	*/

'KB_compatible_formats'( [], []) :- !.

'KB_compatible_formats'([integer(_, Sz, _)| MF1s], [integer(_, Sz, _)| MF2s]) :-
	'KB_compatible_formats'( MF1s, MF2s).

'KB_compatible_formats'([ real( _, Sz, _) | MF1s], [ real( _, Sz, _) | MF2s]) :-
	'KB_compatible_formats'( MF1s, MF2s).

'KB_compatible_formats'([ atom( _, Sz, _) | MF1s], [ atom( _, Sz, _) | MF2s]) :-
	'KB_compatible_formats'( MF1s, MF2s).

'KB_compatible_formats'([ term( _, _, _) | MF1s], [ term( _, _, _) | MF2s]) :-
	'KB_compatible_formats'( MF1s, MF2s).

'KB_compatible_formats'([ code( _, _, _) | MF1s], [ code( _, _, _) | MF2s]) :-
	'KB_compatible_formats'( MF1s, MF2s).


/*
**	Tuple at a time
**
**	ins_tup( rel( a1, ..., an)).	% inserts one tuple in rel
**	ins_tup( rel, [ a1, ..., an]).	% inserts one tuple in rel
**
**	del_tup( rel( a1, ..., an)).	% deletes one tuple in rel
**	del_tup( rel, [ a1, ..., an]).	% deletes one tuple in rel
**	del_tup( rel, [ a1, ..., an], Cond).	% deletes one tuple in rel
**
**	retr_tup( rel( A1, ..., An)).	% Tuple at a time retrieval
**	retr_tup( rel, [ A1, ..., An]).	% Tuple at a time retrieval
**	retr_tup( rel, [ A1, ..., An], Cond).	% Tuple at a time retrieval
*/
/* The predicates ins_tup/1,2; del_tup/1,2 and retr_tup/1,2,3 
** remain for compatibility, but are mapped to the 
** internal new format of the buildins. The new predicates are called
** <old_name>_db like bang_select * see database_kernel.pl *
*/

ins_tup( Term) :-
	ins_tup_db( Term, 0).

ins_tup( Rel, Tuple) :-
	ins_tup_db( Rel, Tuple, 0).

ins_tup_db( Term, Database) :-   
	compound(Term),
	Term \= [_|_],		% Sepia compound include list
	Term =.. [Rel | Tuple],
	ins_tup_db( Rel, Tuple, Database).

ins_tup_db( Rel, Tuple, Database) :-
	'KB_real_rname'( Rel, RRel),
	bang_insert_db( RRel, Tuple, Database).


/* retrieve using relname + list of atts + cond */
del_tup( Term) :-   
	del_tup_db( Term, 0 ).

del_tup( Rel, Tuple) :-
	del_tup_db( Rel, Tuple, 0).

del_tup( Rel, Tuple, Cond) :-
	del_tup_db( Rel, Tuple, Cond, 0).

del_tup_db( Term, Database ) :-
	compound(Term),
	Term \= [_|_],		% Sepia compound include list
	Term =.. [Rel | Tuple],
	del_tup_db( Rel, Tuple, Database).

del_tup_db( Rel, Tuple, Database) :-
	'KB_real_rname'( Rel, RRel),
	bang_retrieve_delete_db( RRel, Tuple, true, Database).

del_tup_db( Rel, Tuple, Cond, Database) :-
	'KB_real_rname'( Rel, RRel),
	'KB_cond_parse'( Cond, [RRel], CondT, Database), 
	bang_retrieve_delete_db( RRel, Tuple, CondT, Database).


/* retrieve using a structure */
retr_tup( Rel, Tuple, Cond) :-
	retr_tup_db( Rel, Tuple, Cond, 0).

/* retrieve using relname + list of atts */
retr_tup( Rel, Tuple) :-
	retr_tup_db( Rel, Tuple, 0).

/* retrieve using a structure */
retr_tup( Term) :-
	retr_tup_db( Term , 0).

retr_tup_db( Term , Database) :-
	compound(Term),
	Term \= [_|_],		% Sepia compound include list
	Term =.. [Rel | Tuple],
	retr_tup_db( Rel, Tuple, Database).

/* retrieve using relname + list of atts */
retr_tup_db( Rel, Tuple, Database) :-
	'KB_real_rname'( Rel, RRel),
	bang_retrieve_db( RRel, Tuple, true, Database).

/* retrieve using relname + list of atts + cond */
retr_tup_db( Rel, Tuple, Cond, Database) :-
	'KB_real_rname'( Rel, RRel),
	'KB_cond_parse'( Cond, [RRel], CondT, Database),
	bang_retrieve_db( RRel, Tuple, CondT, Database).


/*
**	Set at a time
**
**	expression ++> List		     % retrieve result of evaluation
**
**	RelName isr expression		     % create for result of evaluation
**
**	relname <++ [Tuple1, Tuple2, ... ]   % insert tuple list
**	relname <++ expression		     % insert result of evalutation
**
**	relname <-- Tuple1, Tuple2, ... ]    % delete tuple list
**	relname <-- expression		     % delete result of evalutation
**
**	An expression is one of
**
**	[A1, .., Am] :^: Rel 	   where Cond	% selection with projection
**	[A1, .., Am] :^: R1 :*: R2 where Cond	% join with proj.
**	[A1, .., Am] :^: R1 :+: R2 where Cond	% union with projection
**	[A1, .., Am] :^: R1 :-: R2 where Cond	% difference with projection
**
**	both projection and condition are optional
**
**	where Cond is:
**		Att < Cte	(or Cte > Att)	% less than
**		Att =< Cte	(or Cte >= Att)	% less or equal than
**		Att == Cte	(or Cte == Att)	% equal than
**		Att >= Cte	(or Cte =< Att)	% greater or equal than
**		Att > Cte	(or Cte < Att)	% greater than
**		At1 == At2			% attributes' equality
**		Cond and Cond			% logical and
**
**		Att is an unambiguous attribute name. Ambiguous attribute names
**		are disambiguated by prefixing them with <relation name>^. Thus,
**
**			Rel^Att
**
**		For example:	employee^salary
*/

Expr ++> List :-
	++>(Expr, List , 0).

++>(Rel where Cond, List, Database) :-
	'KB_real_rname'( Rel, RRel),
	!,
	'KB_cond_parse'( Cond, [RRel], CondT, Database),
	bang_retrieve_list_db( RRel, CondT, List, Database).
++>(Rel, List, Database) :-
	'KB_real_rname'( Rel, RRel),
	version_free_name( RRel, _),
	!,
	bang_retrieve_list_db( RRel, true, List, Database).
++>(Expr, List, Database) :-
	isr(Y,Expr, Database),
	bang_retrieve_list_db( Y, true, List, Database),
	<=> (Y,[], Database).


OutRel isr Expr :-
	isr (OutRel, Expr, 0).

isr(OutRel, Expr, Database) :-
	insert_set_expression( OutRel, Expr, Database).


OutRel <++ Expr :-
	<++ (OutRel, Expr, 0).

<++(OutRel, Expr, Database) :-
	list(Expr),
	!,
        'KB_real_rname'( OutRel, ORR),
        'KB_insert_tups'( ORR, Expr, Database).
<++(OutRel, Expr, Database) :-
	'KB_real_rname'( OutRel, ORR),
	version_free_name( ORR, Rel),
	bang_existrel_db(Rel, Database),
	!,
	insert_set_expression( ORR, Expr, Database).
<++(OutRel, Expr, _) :-
	error(303, OutRel <++ Expr).
	/* ERROR_CODE(303) : RELATION DOES NOT EXIST */


OutRel <-- Expr :-
	<-- (OutRel, Expr , 0).

<-- (OutRel, Expr , Database) :-
	list(Expr),
	!,
        'KB_real_rname'( OutRel, ORR),
        'KB_delete_tuples'( ORR, Expr, Database).
<-- (OutRel, Expr , Database) :-
	'KB_real_rname'( OutRel, ORR),
	version_free_name( ORR, Rel),
	bang_existrel_db(Rel, Database),
	!,
	delete_set_expression( ORR, Expr, Database).
<-- (OutRel, Expr , _) :-
	error(303, OutRel <-- Expr).
	/* ERROR_CODE(303) : RELATION DOES NOT EXIST */
 
/*--------------------------------------------------------*/
/*	--- Support ---	*/

'KB_insert_tups'( _, [], _Database) :- !.
'KB_insert_tups'( X, [ Tuple | Tuples], Database) :-
	bang_insert_db(X, Tuple, Database),
	'KB_insert_tups'( X, Tuples, Database).

/* delete all tuples matching one in a list of tuples from relation */

'KB_delete_tuples'( _, [], _Database) :- !.
'KB_delete_tuples'( X, [ Tuple | _ ], Database) :-
	bang_retrieve_delete_db( X, Tuple, true, Database),
	fail.
'KB_delete_tuples'( X, [ _ | Tuples], Database) :-
        'KB_delete_tuples'( X, Tuples, Database).
 


/* R1 <-- R2    remove tuples in Expr (relation) from OutRel */
delete_set_expression( OutRel, Expr, Database) :-
        version_free_name(Expr,_),
	!,
	delete_set_kernel( Expr, true, OutRel, Database).
 
/* R1 <-- R2 where C   delete result of selection */
delete_set_expression( OutRel,R where Cond, Database) :-
	!,
        delete_set_kernel( R, Cond, OutRel, Database).
 
/* R1 <-- RelExpr   delete result of general query */
delete_set_expression( OutRel, Expr, Database) :-
        isr (Y, Expr, Database),
	delete_set_kernel( Y, true, OutRel, Database),
        <=>(Y, [], Database).
 


/*	--- Support ---	*/

/* R <-- R where C  i.e. delete from same relation (no difference) */
delete_set_kernel( R, Cond, OutRel, Database) :-
        'KB_real_rname'( R, RR),
	'KB_real_rname'( OutRel, ORR),
	RR == ORR,
	!,
        'KB_cond_parse'( Cond, [R], CondT, Database),
        bang_delete_db( RR, CondT, Database).
 
/* R1 <-- R2 where C i.e. difference operation */
delete_set_kernel( R, Cond, OutRel, Database) :-
        'KB_real_rname'( R, RR),
	'KB_real_rname'( OutRel, ORR),
	'KB_cond_parse'( Cond, [R], CondT, Database),
	(   
	    (	 bang_retrieve_db(RR, Tuple, CondT, Database),
		 bang_retrieve_delete( ORR, Tuple, true, Database),
		 fail
	     )
	     ;
	  	 true
	).
/* Note bang_diff can not be used, cause it requires three different rels */
	





/*
**	optional projection and conditions surrounding a algebra kernel
**
**	default for missing projection is copy all (denoted by [])
**	default for missing condition is no selection (denoted by true)
*/

insert_set_expression( OutRel, ProjL :^: Expr where Cond, Database) :-
	!,
	insert_set_kernel( Expr, Cond, ProjL, OutRel, Database).
insert_set_expression( OutRel, ProjL :^: Expr, Database) :-
	!,
	insert_set_kernel( Expr, true, ProjL, OutRel, Database).
insert_set_expression( OutRel, Expr where Cond, Database) :-
	!,
	insert_set_kernel( Expr, Cond, [], OutRel, Database).
insert_set_expression( OutRel, Expr, Database) :-
	insert_set_kernel( Expr, true, [], OutRel, Database).



/*
**	the kernel of a relational expression is one of
**		R1 :+: R2	union
**		R1 :-: R2	difference
**		R1 :*: R2	join
**		R		simple select/project
*/

insert_set_kernel( R1 :+: R2, Cond, ProjL, OutRel, Database) :-
	!,
	insert_set_kernel(R1, Cond, ProjL, OutRel, Database),
	insert_set_kernel(R2, Cond, ProjL, OutRel, Database).

insert_set_kernel( R1 :-: R2, Cond, ProjL, OutRel, Database) :-
	'KB_real_rname'( R1, RR1),
	'KB_real_rname'( R2, RR2),
	!,
	'KB_cond_parse'( Cond, [R1, R2], CondT, Database),
	'KB_project'( ProjL, [R1], CProjL, Database),	% only RR1 !!
	bang_diff_db( RR1, RR2, CondT, CProjL, OutRel, Database).

insert_set_kernel( R1 :*: R2, Cond, ProjL, OutRel, Database) :-
	'KB_real_rname'( R1, RR1),
	'KB_real_rname'( R2, RR2),
	!,
	'KB_cond_parse'( Cond, [R1, R2], CondT, Database),
	'KB_project'( ProjL, [R1, R2], CProjL, Database),
	bang_join_db( RR1, RR2, CondT, CProjL, OutRel, Database).

insert_set_kernel( R, Cond, ProjL, OutRel, Database) :-
	'KB_real_rname'( R, RR),
	!,
	'KB_cond_parse'( Cond, [R], CondT, Database),
	'KB_project'( ProjL, [R], CProjL, Database),
	bang_select_db( RR, CondT, CProjL, OutRel, Database).

insert_set_kernel( R, Cond, ProjL, OutRel, _) :-
	error(5, OutRel <++ ProjL :^: R where Cond).


/*
**	Projection list handling
**
**	From a list of attribute name i.e. AttName  or  RelName ^ AttName
**	build a list of AttId  i.e. att(RelNo,AttNo)
*/

'KB_project'( [], _, [], _) :- !.

'KB_project'( ProjL, Rels, CProjL, Database) :-
	'KB_get_all_formats'( Rels, RFs, Database),
	'KBX_project'( RFs, ProjL, CProjL),
	!.
'KB_project'( ProjL, _, _, _) :-
	error(321, ProjL),
	!,
	fail.
	/* ERROR_CODE(321) : BAD ATTRIBUTE IN PROJECTION LIST */



'KBX_project'( _, [], []) :- !.

'KBX_project'( RFs, [ A| ProjL], [ AttId |TProjL]) :-
	'KB_is_attribute'( RFs, 1, A, AttId),
	'KBX_project'( RFs, ProjL, TProjL).


/*
**	Condition tree handling
**
**	From a condition tree in relational algebra format build the
**	corresponding tree in internal BANG format.
*/

'KB_cond_parse'( true, _, true, _) :- !.

'KB_cond_parse'( Cond, Rs, CondT, Database) :-
	'KB_get_all_formats'( Rs, RFs, Database),
	'KBX_cond_parse'( Cond, RFs, CondT).

	
/*	--- [H]	Support ---	*/

'KBX_cond_parse'( true, _Rs, true) :- !.

/*
'KBX_cond_parse'( false, _Rs, false) :- !.

'KBX_cond_parse'( not C, Rs, not( TC)) :-
	!,
	'KBX_cond_parse'( C, Rs, TC).
*/

'KBX_cond_parse'( C1 and C2, Rs, and( TC1, TC2)) :-
	!,
	'KBX_cond_parse'( C1, Rs, TC1),
	'KBX_cond_parse'( C2, Rs, TC2).

/*
'KBX_cond_parse'( C1 or C2, Rs, or( TC1, TC2)) :-
	!,
	'KBX_cond_parse'( C1, Rs, TC1),
	'KBX_cond_parse'( C2, Rs, TC2).
*/

'KBX_cond_parse'( SimpleComp, Rs, Expr) :-
	SimpleComp =.. [Comparator, Arg1, Arg2],
	'KBX_cond_arg'( Arg1, Rs, ConvArg1),
	'KBX_cond_arg'( Arg2, Rs, ConvArg2),
	'KBX_cond_op'( Comparator, ConvArg1, ConvArg2, Expr),
	!.
'KBX_cond_parse'( SimpleComp, _, _) :-
	error(322, SimpleComp),
	!,
	fail.
	/* ERROR_CODE(322) : BAD COMPARISON IN WHERE EXPRESSION */
	

/*
** mode('KBX_cond_arg'(in,in,out))
** solve(once_or_fail)
*/

'KBX_cond_arg'( Att, Rs, AttId) :-
	'KB_is_attribute'( Rs, 1, Att, AttId),
	/* AttId = att(RelNo,AttPos) */
	!.
'KBX_cond_arg'( Constant, _, Constant) :-
	atomic( Constant).
	
/*
** construct a simple condition from a comparison operator and two operands
** the BANG builtins require that
**	- functor is eq, diff, less, greater, less_eq, greater_eq
**	- on attribute - constant comparisons the attribute comes first
**	- attribute - attribute comparsions are allowed with all comperators
**	  both within one or between the two relations (join, diff)
**
** mode('KBX_cond_op'(in,in,in,out))
** solve(several)
*/

'KBX_cond_op'( '==' , X, Y, eq(X,Y)) :- X = att(_,_).
'KBX_cond_op'( '==' , X, Y, eq(Y,X)) :- Y = att(_,_).
'KBX_cond_op'( '\\==', X, Y, diff(X,Y)) :- X = att(_,_).
'KBX_cond_op'( '\\==', X, Y, diff(Y,X)) :- Y = att(_,_).
'KBX_cond_op'( '<'  , X, Y, less(X,Y)) :- X = att(_,_).
'KBX_cond_op'( '<'  , X, Y, greater(Y,X)) :- Y = att(_,_).
'KBX_cond_op'( '=<' , X, Y, less_eq(X,Y)) :- X = att(_,_).
'KBX_cond_op'( '=<' , X, Y, greater_eq(Y,X)) :- Y = att(_,_).
'KBX_cond_op'( '>'  , X, Y, greater(X,Y)) :- X = att(_,_).
'KBX_cond_op'( '>'  , X, Y, less(Y,X)) :- Y = att(_,_).
'KBX_cond_op'( '>=' , X, Y, greater_eq(X,Y)) :- X = att(_,_).
'KBX_cond_op'( '>=' , X, Y, less_eq(Y,X)) :- Y = att(_,_).
	




/*
** Give a unique identifier to an attribute - if exists : att(Rno, Pos) 
** Find the position of an attribute in a relation
**
** mode('KB_is_attribute'(in,in,in,out))
** solve(once_or_fail)
*/

'KB_is_attribute'( [ R, RF | _], Rno, R1 ^ Att, att(Rno, AttNo)) :-
	R1 == R,
	atom(Att),
	!,
	'KBS_in_position'( Att, RF, 1, AttNo).

'KB_is_attribute'( [ _R, RF | _], Rno, Att, att(Rno, AttNo)) :-
	atom(Att),
	'KBS_in_position'( Att, RF, 1, AttNo),
	!.

'KB_is_attribute'( [ _, _ | MRs], Rno, Att, AttId) :-
	Rno1 is Rno + 1,
	'KB_is_attribute'( MRs, Rno1, Att, AttId).


/*
** look for an attribute name in a relation's format and return position
**
** type('KBS_in_position'(atom, format, integer, integer))
** mode('KBS_in_position'(in,in,in,out))
** solve(once_or_fail)
*/

'KBS_in_position'( AttName, [ AttFormat| _], N, N) :- 
	arg(1,AttFormat,AttName),
	!.
'KBS_in_position'( X, [ _| T], M, N) :-
	M1 is M + 1,
	'KBS_in_position'( X, T, M1, N).





/*
**	rename relation
*/

rename_attributes( Rel, NewNames) :-
	rename_attributes( Rel, NewNames, 0).

rename_attributes( Rel, NewNames, Database) :-
	'KB_real_rname'( Rel, RRel),
	bang_arity_db( RRel, Arity, Database),
	length( NewNames, Arity),
	bang_renamerel_db( RRel, RRel, NewNames, Database).

rename_relation( Rel, NewRel) :-
	rename_relation( Rel, NewRel, 0).

rename_relation( Rel, NewRel, Database) :-
	bang_renamerel_db( Rel, NewRel, Database).



/*
** DBoperation ondb DBhandle
*/

ondb(Op, DB, _) :- var(Op), !, error(4, ondb(Op, DB)).

ondb(Rel ++> List, DB, _) :- !, ++>(Rel, List, DB).
ondb(OutRel <++ Expr, DB, _) :- !, <++(OutRel, Expr, DB).
ondb(OutRel <-- Expr, DB, _) :- !, <-- (OutRel, Expr , DB).
ondb(Rel1<@> Rel2, DB, _) :- !, <@>( Rel1, Rel2,DB).
ondb(Rel <-> Synonym, DB, _) :- !, <->( Rel, Synonym, DB).
ondb(Rel <=> Format, DB, _) :- !, <=>( Rel, Format, DB).
ondb(OutRel isr Expr, DB, _) :- !, isr(OutRel, Expr, DB).
ondb(arity(Rel, Arity), DB, _) :- !, arity( Rel, Arity, DB).
ondb(cardinality(Rel, Arity), DB, _) :- !, cardinality( Rel, Arity, DB).
ondb(del_tup(Term), DB, _) :- !, del_tup_db( Term, DB).
ondb(del_tup(Rel, Tuple), DB, _) :- !, del_tup_db( Rel, Tuple, DB).
ondb(del_tup(Rel, Tuple, Cond), DB, _) :- !, del_tup_db(Rel, Tuple, Cond, DB).
ondb(helpdb, DB, _) :- !, helpdb(DB).
ondb(helprel(Rel), DB, _) :- !, helprel(Rel, DB).
ondb(ins_tup(Term), DB, _) :- !, ins_tup_db( Term, DB).
ondb(ins_tup(Rel, Tuple), DB, _) :- !, ins_tup_db( Rel, Tuple, DB).
ondb(printrel(Rel), DB, _) :- !, printrel( Rel, DB).
ondb(rename_attributes(Rel, NewNames), DB, _) :- !,
	rename_attributes(Rel, NewNames, DB).
ondb(rename_relation(Rel, NewNames), DB, _) :- !,
	rename_relation(Rel, NewNames, DB).
ondb(retr_tup(Term), DB, _) :- !, retr_tup_db( Term, DB).
ondb(retr_tup(Rel, Tuple), DB, _) :- !, retr_tup_db( Rel, Tuple, DB).
ondb(retr_tup(Rel, Tuple, Cond), DB, _) :- !,
	retr_tup_db( Rel, Tuple, Cond, DB).
ondb(closedb, DB, _) :- !, closedb(DB).
ondb(createdb(A), DB, _) :- !, createdb(A,DB).
ondb(current_relation(A), DB, _) :- !, current_relation(A,DB).
ondb(current_temp_relation(A), DB, _) :- !, current_temp_relation(A,DB).
ondb(destroydb, DB, _) :- !, destroydb(DB).
ondb(destroy_temprels, DB, _) :- !, destroy_temprels(DB).
ondb(opendb(A), DB, _) :- !, opendb(A,DB).
ondb(statistics_desc, DB, _) :- !, statistics_desc(DB).
ondb(statistics_relation(A), DB, _) :- !, statistics_relation(A,DB).

ondb(bang_arity(A,B), DB, _) :- !, bang_arity_db(A,B,DB).
ondb(bang_attribute(A,B,C), DB, _) :- !, bang_attribute_db(A,B,C,DB).
ondb(bang_cardinality(A,B), DB, _) :- !, bang_cardinality_db(A,B,DB).
ondb(bang_createrel(A,B,C), DB, _) :- !, bang_createrel_db(A,B,C,DB).
ondb(bang_delete(A,B), DB, _) :- !, bang_delete_db(A,B,DB).
ondb(bang_delete_tup(A,B), DB, _) :- !, bang_delete_tup_db(A,B,DB).
ondb(bang_delete_tup(A,B,C), DB, _) :- !, bang_delete_tup_db(A,B,C,DB).
ondb(bang_destroyrel(A), DB, _) :- !, bang_destroyrel_db(A,DB).
ondb(bang_diff(A,B,C,D,E), DB, _) :- !, bang_diff_db(A,B,C,D,E,DB).
ondb(bang_diff(A,B,C,D,E,F), DB, M) :- !, bang_diff_db_body(A,B,C,D,E,F,DB,M).
ondb(bang_exist(A,B), DB, _) :- !, bang_exist_db(A,B,DB).
ondb(bang_existrel(A), DB, _) :- !, bang_existrel_db(A,DB).
ondb(bang_format(A,B), DB, _) :- !, bang_format_db(A,B,DB).
ondb(bang_format(A,B,C), DB, _) :- !, bang_format_db(A,B,C,DB).
ondb(bang_insert(A,B), DB, _) :- !, bang_insert_db(A,B,DB).
ondb(bang_insert(A,B,C), DB, _) :- !, bang_insert_db(A,B,C,DB).
ondb(bang_join(A,B,C,D,E), DB, _) :- !, bang_join_db(A,B,C,D,E,DB).
ondb(bang_join(A,B,C,D,E,F), DB, M) :- !, bang_join_db_body(A,B,C,D,E,F,DB,M).
ondb(bang_free_cursor, DB, _) :- !, bang_free_cursor_db(DB).
ondb(bang_register(A,B), DB, _) :- !, bang_register_db(A,B,DB).
ondb(bang_renamerel(A,B), DB, _) :- !, bang_renamerel_db(A,B,DB).
ondb(bang_renamerel(A,B,C), DB, _) :- !, bang_renamerel_db(A,B,C,DB).
ondb(bang_retrieve(A,B,C), DB, _) :- !, bang_retrieve_db(A,B,C,DB).
ondb(bang_retrieve_delete(A,B,C), DB, _) :- !,
	bang_retrieve_delete_db(A,B,C,DB).
ondb(bang_retrieve_delete(A,B,C,D), DB, M) :- !,
	bang_retrieve_delete_db_body(A,B,C,D,DB,M).
ondb(bang_retrieve_lazy(A,B,C), DB, _) :- !, bang_retrieve_lazy_db(A,B,C,DB).
ondb(bang_retrieve_list(A,B,C), DB, _) :- !, bang_retrieve_list_db(A,B,C,DB).
ondb(bang_select(A,B,C,D), DB, _) :- !, bang_select_db(A,B,C,D,DB).
ondb(bang_select(A,B,C,D,E), DB, M) :- !, bang_select_db_body(A,B,C,D,E,DB,M).

ondb(Op, DB, _) :- error(60, ondb(Op, DB)).
