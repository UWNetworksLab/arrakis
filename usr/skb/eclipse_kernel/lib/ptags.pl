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
% Version:	$Id: ptags.pl,v 1.1 2008/06/30 17:43:48 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	ptags.pl
 *
 * DESCRIPTION: 	Tags file creator.
 *
 *
 * CONTENTS:     
 *
 * REVISION HISTORY:
 * AUTHOR	VERSION	DATE	REASON
 * Joachim Witte 1.0	26.06.90
 */

:- module(ptags).

:- comment(summary, "Tags file creator").
:- comment(author, " Joachim Witte, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:48 $").
:- comment(desc, html("
    This library provides a program that checks the source form of a
    Prolog program and creates a tags file for use with the UNIX
    editors ex and vi, similar to ctags(1).  The library is loaded using
    <PRE>
    :- lib(ptags).
    </PRE>
    and the predicates ptags/1, ptags/2 and tags/2 become global.  The
    utility is invoked by
    <PRE>
    :- ptags(+File)
    </PRE>
    or
    <PRE>
    :- ptags(+File, +TagsFile)
    </PRE>
    +TagsFile is the name of the tags file.  If +TagsFile is omitted,
    it defaults to tags. 
    <P>
    The tags file created by the ptags/1, 2 predicates can be used as
    a tags file for vi or ex.  A procedures specified as Name/Arity
    can be found using the command
    <PRE>
    :ta Name
    </PRE>
    If there are several procedures with the same name and different
    arity, the above command will find only one of them.  In this case
    the command
    <PRE>
    :ta Name/Arity
    </PRE>
    should be used.  If the clauses for the procedure are not
    consecutive or if the procedure occurs in more than one file, only
    one occurrence will be put into the tags file.  Which one it will
    be depends on the file name and on the contents of the line that
    is being sought by the :ta command. 
    ")).

% Don't define local operators, because then operators in the tagged file
% might become invisible (bug in ptags' module handling).

:- import
	canonical_path_name/2,
	file_query_body/3,
	read_/3
    from sepia_kernel.

:- export
	ptags/1,
	ptags/2.

:- export
	ptags_all/0,
	tags/2,
	tags1/2.


:- set_error_handler(44, true/0).	% because there is no way of telling
					% whether this error occurs or not


% Make ptags for all .pl files in the current directory
ptags_all :-
	read_directory('.', '*.pl', _, Files),
	ptags(Files).


% ptags/1
% ptags(File)

ptags(File) :-
	ptags(File, 'tags').

% ptags/2
% ptags(File, Tags)

ptags(X, TagsStream) :-
	recreate_read_module,
	ptags1(X, TagsStream).

ptags1(X, Tags) :-
	var(X), !,
	error(4, ptags(X, Tags)).
ptags1([], _) :-
	!.
ptags1(library(_), _) :-
	!.
ptags1([File|Files], Tags) :-
	!,
	(string(Tags) ->
		(TagsS = Tags)
	;
		atom(Tags) ->
		atom_string(Tags, TagsS)
	;
		error(5, ptags(File, Tags))
	),
	get_flag(pid, Pid),
	concat_string(['/tmp/sepia_ptags', Pid], TempFile),
	open(TempFile, write, TagsStream),
	tags1(File, TagsStream),
	tags1(Files, TagsStream),
	close(TagsStream),
	concat_string(['sort +0 -1 -u ', TempFile], ShellString1),
	concat_string([ShellString1, ' > '], ShellString2),
	concat_string([ShellString2, TagsS], ShellString),
	sh(ShellString),
	delete(TempFile).
ptags1(File, Tags) :-
	(string(File) ->			% first convert to a string
		(FileS = File)
	;
		atom(File) ->
		atom_string(File, FileS)
	;
		error(5, ptags(File, Tags))
	),
	(string(Tags) ->
		(TagsS = Tags)
	;
		atom(Tags) ->
		atom_string(Tags, TagsS)
	;
		error(5, ptags(File, Tags))
	),
	(
		get_flag(prolog_suffix, Suffixes),
		member(Suffix, Suffixes),
		Suffix \== ".sd",
		concat_strings(FileS, Suffix, PlFile)
	;
		error(171, ptags(File, Tags))
	),
	exists(PlFile),
	!,
	open(PlFile, read, Stream),
	get_flag(pid, Pid),
	concat_string(['/tmp/sepia_ptags', Pid], TempFile),
	open(TempFile, write, TagsStream),
	ptags_stream(Stream, PlFile, TagsStream),
	close(Stream),
	close(TagsStream),
	concat_string(['sort +0 -1 -u ', TempFile], ShellString1),
	concat_string([ShellString1, ' > '], ShellString2),
	concat_string([ShellString2, TagsS], ShellString),
	sh(ShellString),
	delete(TempFile).

% ptags_stream/3
% ptags_stream(Stream, PlFile, TagsStream)

ptags_stream(Stream, PlFile, TagsStream) :-
	printf("making tags for file %w%n%b", [PlFile]),
	read_(Stream, Term, ptags_read_module),
	ptags_stream(Stream, 0, Term, _OldPId, _NewPId, PlFile, TagsStream).

% ptags_stream/7
% ptags_stream(Stream, Pointer, Term, OldPId, NewPId, PlFile, TagsStream)

ptags_stream(_, _, end_of_file, _, _, _, _) :- 
	!.
ptags_stream(Stream, Pointer, Term, _, NewPId, PlFile, TagsStream) :- 
	ptags_term(Pointer, Term, NewPId, PId, PlFile, TagsStream),
	at(Stream, NewPointer),
	read_(Stream, NewTerm, ptags_read_module),
	ptags_stream(Stream, NewPointer, NewTerm, NewPId, PId, PlFile, TagsStream).

% ptags_term/6
% ptags_term(Pointer, Clause, OldPId, NewPId, PlFile, TagsStream)

ptags_term(_, (:- Goal), _, _, _, TagsStream) :-
	!,
	process_query(Goal, TagsStream).
ptags_term(_, (?- Goal), _, _, _, TagsStream) :-
	!,
	process_query(Goal, TagsStream).
ptags_term(Pointer, Clause, OldPId, NewPId, PlFile, TagsStream) :-
	ptags_pid(Clause, Head, NewPId, Atom, Arity),
	ptags_pointer(Pointer, PlFile, TagsString),
	(NewPId == OldPId ->
		true
	;
		write_ptags(TagsStream, Atom, Arity, PlFile, TagsString),
		ptags_tools(Head, TagsStream, PlFile, TagsString)
	).

ptags_tools(Head, TagsStream, PlFile, TagsString) :-
	recorded(Head, AtomI/ArityI, Ref),
	!,
	write_ptags(TagsStream, AtomI, ArityI, PlFile, TagsString),
	erase(Ref),
	ptags_tools(Head, TagsStream, PlFile, TagsString).
ptags_tools(_, _, _, _).

process_query((A, B), TagsStream) :-
	!,
	process_query(A, TagsStream),
	process_query(B, TagsStream).
process_query(tool(PredI, F/A), _) :-
	!,
	functor(PredB, F, A),
	local_record(F/A),
	recorda(PredB, PredI).
process_query(module(_), _) :-		% a new module, we have to erase
	!,				%  all operators, macros, etc.
	recreate_read_module.		%  to avoid clashes
process_query(module_interface(_), _) :-
	!,
	recreate_read_module.
process_query(lib(Lib), _) :-
	!,
	lib(Lib, ptags_read_module).
process_query(Goal, TagsStream) :-
	file_query_body(Goal, tags1(_, TagsStream), ptags_read_module).

% tags/2
% tags(File, TagsStream)

tags(X, TagsStream) :-
	recreate_read_module,
	tags1(X, TagsStream).

tags1(X, TagsStream) :-
	var(X), !,
	error(4, tags(X, TagsStream)).
tags1([], _) :-
	!.
tags1([File|Files], TagsStream) :-
	!,
	tags1(File, TagsStream),
	tags1(Files, TagsStream).
tags1(library(_), _) :-
	!.					% ignore libraries
tags1(File, TagsStream) :-
	(string(File) ->			% first convert to a string
		(FileS = File)
	;
	atom(File) ->
		atom_string(File, FileS)
	;
		error(5, tags(File, TagsStream))
	),
	(
		get_flag(prolog_suffix, Suffixes),
		member(Suffix, Suffixes),
		Suffix \== ".sd",
		concat_strings(FileS, Suffix, PlFile),
		exists(PlFile),
		!,
		open(PlFile, read, Stream),
		ptags_stream(Stream, PlFile, TagsStream),
		close(Stream)
	;
		printf(error, "*** Warning: file %s does not exist\n%b", [File])
	).

% ptags_pid/2
% ptags_pid(Clause, Head, PId, Atom)

ptags_pid((H :- _), H, PId, Atom, Arity) :- 
	!,
	functor(H, Atom, Arity),
	PId = Atom/Arity.
ptags_pid(H, H, PId, Atom, Arity) :- 
	functor(H, Atom, Arity),
	PId = Atom/Arity.

% ptags_pointer/3
% ptags_pointer(Pointer, PlFile, TagsStream)

ptags_pointer(Pointer, PlFile, TagsString) :-
	open(PlFile, read, Stream),
	seek(Stream, Pointer),
	read_token(Stream, _, _),
	at(Stream, Pointer2),
	seek(Stream, Pointer),
	ptags_string(Stream, Pointer2, TagsString),
	close(Stream).

% ptags_string/3
% ptags_string(Stream, Pointer2, TagsString)

ptags_string(Stream, Pointer2, TagsString) :-
	char_int(N, 10),
	read_string(Stream, N, 80, String),	% truncate long lines
	at(Stream, Pointer),
	((Pointer >= Pointer2) ->
		(TagsString = String)
	;
		ptags_string(Stream, Pointer2, TagsString)
	).

% This is mainly to get rid of local operators ...

recreate_read_module :-
	( current_module(ptags_read_module) ->
		erase_module(ptags_read_module)
	;
		true
	),
	create_module(ptags_read_module),
	call(import(ptags), ptags_read_module).

% write_ptags/5
% write_ptags(TagsStream, Atom, Arity,  PlFile, TagsString)

write_ptags(TagsStream, Atom, Arity, PlFile, TagsString) :-
	(substring(PlFile, "/", 1) ->
		AbsFile = PlFile
	;
	substring(PlFile, "~", 1) ->
		canonical_path_name(PlFile, AbsFile)
	;
	compound(PlFile) ->
		canonical_path_name(PlFile, AbsFile)
	;
		get_flag(cwd, Cwd0),
		(substring(Cwd0, "/auto/", 1) ->
		    Len is string_length(Cwd0) - 5,
		    substring(Cwd0, 6, Len, Cwd)
		;
		    Cwd=Cwd0
		),
		concat_string([Cwd, PlFile], AbsFile)
	),
	printf(TagsStream, '%w	%w	/^', [Atom, AbsFile]),
	(substring(TagsString, "/", _) ->
		slash_for_vi(TagsStream, TagsString)
	;
		write(TagsStream, TagsString)
	),
	nl(TagsStream),
	printf(TagsStream, '%w/%d	%w	/^', [Atom, Arity, AbsFile]),
	(substring(TagsString, "/", _) ->
		slash_for_vi(TagsStream, TagsString)
	;
		write(TagsStream, TagsString)
	),
	nl(TagsStream).

% slash_for_vi/2
% slash_for_vi(TagsStream, TagsString)

slash_for_vi(TagsStream, TagsString) :-
	(
	    (char_int(S, 47),
	    substring(TagsString, _, 1, Char),
	    ((Char == S) ->
		    write(TagsStream, \)
	    ;
		    true
	    ),
	    put_char(TagsStream, Char),
	    fail)
	;
	    true
	).
