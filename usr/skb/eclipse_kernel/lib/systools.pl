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
% Version:	$Id: systools.pl,v 1.1 2008/06/30 17:43:50 jschimpf Exp $
% ----------------------------------------------------------------------

%
% This file contains various Prolog system tools
%

:- module(systools).

:- pragma(deprecated_warnings(off)).

:- export
	compare_dump/1,
	make_quick/1,
	html_index/0,
	html_index/2,
	print_exported/0,
	print_quick/0,
	read_dump_file/1,
	undef_biprefs/0,
	undef_biprefs/1,
	undoc_bips/0,
	undoc_bips/1.

:- import
	bytes_to_term/2,
	compiled_stream/1,
	current_built_in_body/2,
	current_predicate_body/2,
	get_flag_body/4
    from sepia_kernel.


col_width(48).

% Last was 67
make_quick(NL) :-
	printf("reading index file...%n%b", []),
	read_index_file,
	setof(X, (A,B,C,D)^(bip(A, C, D, X, B), not ignored_type(X, _)), BT),
	setof(X, (A,B,C,D)^(bip(A, C, D, X, B), not ignored_type(X, _)), BT),
	findall(X, type(X, _), Types),	
	sort(Types, ST),
	(ST = BT ->			% check if up-to-date
		true
	;
		printf(error, "Types differ:\ndoc: %Dw\nfile: %Dw\n%b",
			[BT, ST]),
		fail
	),
	get_flag(version, Ver),
	printf("writing file quick1...%n%b", []),
	open(quick1, write, qs),
	% These are only Sepia bips, not Megalog
	printf(qs, "ECLIPSE %s BUILT-IN PREDICATES\n", [Ver]),
	output_types(Types),
	close(qs),
	printf("writing file quick.ps...%n%b", []),
	exec("wc -l quick1",[null,WC]), read_token(WC,Lines,_),close(WC),
	Cols is (Lines-1)//(2*NL)+1,
	concat_string(['enscript -o quick.ps -r -B -fTimes-Roman7 -c --columns=',Cols,' -L', NL, ' quick1'], Cmd),
	exec(Cmd, []),
	system("/bin/rm -f quick1 quick??").
/*
	merge_columns(NL),
	printf(error, "Print and remove temporary files? %b", []),
	(tyi(0'y) ->
	    nl(error),
	    print_quick(NL),
	    system("/bin/rm quick1 quick??")
	;
	    true
	).
*/

output_types([]).
output_types([T|Types]) :-
	type(T, TS),
	printf(qs, "\n%% %s\n\n", [TS]),
	output_type(T),
	output_types(Types).

output_type(Type) :-
	(System = sepia ; System = extensions),
	P=N/A,
	bip(N, A, System, Type, File),
	not(ignored(P)),
	get_flag(installation_directory, Inst),
	concat_string([Inst,'/doc/bips/',System,/,Type,/,File,'.txt'], HelpFile),
	(exists(HelpFile) ->
	    (short_descr(P, Line) ->
		printf(qs, "%s\n", [Line])
	    ;
		open(HelpFile, read, HelpStream),
		print_paragraph(HelpStream),
		close(HelpStream)
	    )
	;
	    printf(error, "The help file %s is missing\n%b", [HelpFile])
	),
	fail;true.

print_paragraph(HelpStream) :-
    pred_descr(HelpStream, Line),
    printf(qs, "%s\n", [Line]).

pred_descr(S, Line) :-
	repeat,					% skip empty lines
	read_string(S, end_of_line, Len, Line),
	Len > 0,
	!.

read_index_file :-
    (is_predicate(bip/5) ->
	true
    ;
	get_flag(installation_directory, Inst),
	concat_atom([Inst, '/doc/bips/index.pl'], Index),
	compile(Index)
    ).

type(allsols, "All solutions").
type(arithmetic, "Arithmetic").
type(arrays, "Arrays and global variables").
type(control, "Control").
type(coroutine, "Coroutining").
type(debug, "Debugger").
type(directives, "Compiler directives").
type(env, "Prolog Environment").
type(event, "Event handling").
type(externals, "External predicate interface").
type(iochar, "I/O - character based").
type(iostream, "I/O - stream handling").
type(ioterm, "I/O - term based").
type(liblist, "Lists library").
type(modules, "Modules").
type(database, "Predicate Database").
type(libsort, "Sorts library").
type(libstring, "Strings library predicates").
type(opsys, "Operating system").
type(record, "Recorded Database").
type(stratom, "Strings and Atoms").
type(termcomp, "Term Comparison").
type(termmanip, "Term Manipulation").
type(typetest, "Type testing").
type(fd, "Finite Domains").
type(propia, "Propia").
type(conjunto, "Conjunto").

ignored_type(user, "User defined system predicates").
ignored_type(megalog, "Megalog").
ignored_type(obsolete, "Obsolete").
ignored_type(r, "").
ignored_type(glossary, "").
ignored_type(dbkernel, "").
ignored_type(db, "").
ignored_type(kb, "").
ignored_type(mps, "").
ignored_type(chr, "").

ignored(toplevel_prompt/1).
ignored(user_end/0).
ignored(user_error_exit/0).
ignored(user_loop/1).
ignored(user_start/0).
%ignored((+)/2).
%ignored((-)/2).
%ignored((\)/2).
%ignored((+)/3).
%ignored((-)/3).
%ignored((*)/3).
%ignored((/)/3).
%ignored((//)/3).
%ignored((/\)/3).
%ignored((\/)/3).
%ignored((^)/3).
%ignored((<<)/3).
%ignored((>>)/3).

short_descr(current_macro/4, "current_macro(?Term, ?Pred, ?Opts, ?Mod)").
short_descr(compare_instances/4, "compare_instances(?Rel, ?T1, ?T2, ?MetaTerms)").
short_descr(read_directory/4, "read_directory(+Dir, +Pattern, ?Dirs, ?Files)").
short_descr(read_string/4, "read_string(+Stream, +DelString, ?Len, ?String)").
short_descr(substring/4, "substring(+String1, ?Position, ?Len, ?String2)").
short_descr(substring/5, "substring(+String1, ?Before, ?Length, ?After, ?String2)").

nostr(_, read_string(_, _, 0, '')) :- !.
nostr(N, Goal) :-
	error(default(N), Goal).

:- set_error_handler(190, nostr/2).
:- set_error_handler(198, nostr/2).

merge_columns(NL) :-
	(exists(quickai) -> delete(quickai); true),
	concat_string(['split -', NL, ' quick1 quick'], Cmd),
	exec(Cmd, []),
	(exists(quickai) ->
		printf("Too many columns\n%b", []),
		abort
	;
		true
	),
	open('QUICK', write, qs),
	open(quickaa, read, C1),
	open(quickab, read, C2),
	open(quickac, read, C3),
	open(quickad, read, C4),
	merge(C1, C2, C3, C4),
	close(C1),
	close(C2),
	close(C3),
	close(C4),
	open(quickae, read, C5),
	open(quickaf, read, C6),
	open(quickag, read, C7),
	open(quickah, read, C8),
	merge(C5, C6, C7, C8),
	close(C5),
	close(C6),
	close(C7),
	close(C8),
	close(qs).

merge(C1, C2, C3, C4) :-
	read_string(C1, end_of_line, L1, S1),
	string(S1),
	!,
	put_string(L1, S1),
	read_string(C2, end_of_line, L2, S2),
	put_string(L2, S2),
	read_string(C3, end_of_line, L3, S3),
	put_string(L3, S3),
	read_string(C4, end_of_line, L4, S4),
	put_string(L4, S4),
	nl(qs),
	merge(C1, C2, C3, C4).
merge(_, _, _, _).

put_string(Len, S) :-
	Tabs is max((col_width +7 - Len)//8, 0),
	printf(qs, "%s%*c", [S, Tabs, 0'	]).


print_quick(NL) :-
    concat_string(['enscript -r -B -fTimes-Roman7 -L', NL, ' -p/dev/null QUICK'], Cmd1),
    exec(Cmd1, [null, null, M], Pid),
    wait(Pid, _),
    read_string(M, end_of_line, _, S),
    (substring(S, "wrapped", _) ->
	printf(error, "There are wrapped lines in the output\n%b", []),
	read_string(M, end_of_line, _, T),
	Goal=fail
    ;
	S = T,
	Goal=true
    ),
    close(M),
    (substring(T, "2 pages", _) ->
	Goal,
	concat_string(['enscript -r -B -fTimes-Roman7 -L', NL, ' QUICK'], Cmd2),
	exec(Cmd2, []),
	concat_string(['enscript -r -B -fTimes-Roman7 -L', NL, ' -pQUICK.ps QUICK'], Cmd3),
	exec(Cmd3, [])
    ;
	printf(error, "There are not 2 pages in the output\n%b", []),
	fail
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Print all predicates exported from sepia_kernel together with their class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- compiled_stream(X), get_stream_info(X, name, Name), pathname(Name, Path, _),
	setval(lib_path, Path).
:- external(ul/1).

print_exported :-
    (is_locked(sepia_kernel) ->
	getval(lib_path, Path),
	concat_strings(Path, "unlock.o", UF),
	load(UF),
	external(ul/1, ul),
	ul(sepia_kernel)
    ;
	true
    ),
    current_functor(X),
    arg(2, X) < 256,
    get_flag_body(X, visibility, exported, sepia_kernel),
    get_flag_body(X, defined, on, sepia_kernel),
    (	current_built_in(Y),
	get_flag_body(Y, tool, on, sepia_kernel),
	tool_body(Y, X, sepia_kernel) ->
		Type = 'tool_body        '
    ;
	current_error(N),
	get_error_handler(N, X, sepia_kernel) ->
		Type = 'error_handler    '
    ;
	current_interrupt(N, _),
	get_interrupt_handler(N, X, sepia_kernel) ->
		Type = 'interrupt_handler'
    ;
	call(proc_flags(X, 1, F, sepia_kernel), sepia_kernel),
	F /\ 16'20000000 =:= 0 ->
		Type = 'used internally  '
    ;
		Type = 'not referenced   '
    ),
    printf("%s : %w\n%b", [Type, X]),
    fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Read a dump file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

read_dump_file(File) :-
	open(File, read, In),
	read_string(In, "", 12, _Header),
%	writeq(header = _Header), nl,
	read_dump_stream(In).

read_dump_stream(In) :-
	( at_eof(In) ->
	    true
	;
	    read_dumped_term(In, Term),
%	    writeln(Term),
	    writeclause(Term),
	    read_dump_stream(In)
	).

read_dumped_term(In, Term) :-
	read_integer(In, Length),
	read_string(In, "", Length, String),
	bytes_to_term(String, Term).

read_integer(In, N) :-
	get(In, Byte3),
	get(In, Byte2),
	get(In, Byte1),
	get(In, Byte0),
	N is Byte3 << 24 \/ Byte2 << 16 \/ Byte1 << 8 \/ Byte0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Compare .pl with .eco file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compare_dump(File) :-
	concat_string([File, ".eco"], FileSd),
	concat_string([File, ".pl"], FilePl),
	open(FileSd, read, Sd),
	open(FilePl, read, Pl),
	read_string(Sd, "", 12, _Header),
	compare_streams(Pl, Sd).

compare_streams(Pl, Sd) :-
	read(Pl, TermPl),
	( at_eof(Sd) ->
	    TermSd = end_of_file
	;
	    read_dumped_term(Sd, TermSd)
	),
	( variant(TermPl, TermSd) ->
	    ( TermPl = end_of_file ->
		writeln("Ok")
	    ;
		compare_streams(Pl, Sd)
	    )
	;
	    writeln("********* Terms differ **********"),
	    writeclause(TermPl),
	    writeclause(TermSd)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Make html index files for bip book pages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

html_index :-
	Dest = "/usr/local/eclipse/prerelease/doc/bips",
	BipRoot = "/usr/local/eclipse/prerelease/doc/bips",
	html_index(BipRoot, Dest).

html_index(BipRoot, Dest) :-
	nonvar(Dest),		% where to put the .html files
	nonvar(BipRoot),	% where the bip pages reside

	concat_strings(BipRoot, "/index.pl", Index),
	compile(Index),

	make_full_index(BipRoot, Dest),
	make_grouped_index(BipRoot, Dest).

make_full_index(Root, Dest) :-
	concat_strings(Dest, "/fullindex.html", File),
	open(File, write, Out),
	concat_strings("chmod 644 ", File, Chmod),
	exec(Chmod, []),
	writeln(File),
	writeln(Out, "<HEAD><TITLE>All ECLiPSe Built-In Predicates</TITLE></HEAD><BODY>"),
	writeln(Out, "<H1> All ECLiPSe Built-In Predicates in Alphabetic Order </H1>"),
	writeln(Out, "<OL>"),
	make_links(Root, Out, _, _),
	writeln(Out, "</OL>"),
	close(Out).


make_grouped_index(Root, Dest) :-
	concat_strings(Dest, "/groupindex.html", GroupFile),
	open(GroupFile, write, Out),
	concat_strings("chmod 644 ", GroupFile, Chmod),
	exec(Chmod, []),
	writeln(GroupFile),
	writeln(Out, "<HEAD><TITLE>ECLiPSe Built-In Predicate Groups </TITLE></HEAD><BODY>"),
	writeln(Out, "<H1> ECLiPSe Built-In Predicate Groups </H1>"),
	writeln(Out, "<UL>"),
	(
	    group(Group),
	    printf(Out, "<LI>%s\n<UL>", [Group]),
	    (
		subgroup(Group, SubGroup),
		concat_string(['index_',Group,'_',SubGroup,'.html'],
		    File),
		printf(Out, "<LI><A HREF=\"%s\"> %s </A>\n",
		    [File,SubGroup]),
		concat_string([Dest,/,File], AbsFile),

		open(AbsFile, write, SubOut),
		concat_strings("chmod 644 ", AbsFile, ChmodFile),
		exec(ChmodFile, []),
		writeln(AbsFile),
		printf(SubOut,
		    "<HEAD><TITLE>ECLiPSe Built-In Predicates (%s/%s)\
			</TITLE></HEAD><BODY>\n",
		    [Group, SubGroup]),
		printf(SubOut,
		    "<H1>ECLiPSe Built-In Predicates (%s/%s)</H1><UL>\n",
		    [Group, SubGroup]),
		make_links(Root, SubOut, Group, SubGroup),
		writeln(SubOut, "</UL>"),
		close(SubOut),

		fail
	    ;
		true
	    ),
	    writeln(Out, "</UL>"),
	    fail
	;
	    true
	),
	writeln(Out, "</UL>"),
	close(Out).

make_links(_Root, Out, Group, SubGroup) :-
	bip(Name, Group, SubGroup, File),
	printf(Out, "<LI><A HREF=\"%s/%s/%s\"> %w </A>\n",
		[Group,SubGroup,File,Name]),
	fail.
make_links(_, _, _, _).

group(Group) :-
	setof(G, N^F^SG^bip(N, G, SG, F), GroupList),
	member(Group, GroupList).
	
subgroup(Group, SubGroup) :-
	setof(SG, N^F^bip(N, Group, SG, F), SubGroupList),
	member(SubGroup, SubGroupList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Look for dangling references in and to the bip pages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- coroutine.

undef_biprefs :-
    undef_biprefs(sepia),
    undef_biprefs(megalog),
    undef_biprefs(extensions).

undef_biprefs(Type) :-
    printf("Checking %s for dangling references%n%b", Type),
    set_error_handler(133, true/0),
    set_error_handler(139, true/0),
    concat_string(["collect_bip_files /home/lp/sepia/workdir/sepia/doc_tex/bips/", Type], Comm),
    exec(Comm, [null, N], Pid),
    (Type = extensions ->
	load_extensions
    ;
    Type = megalog ->
	load_db
    ;
	true
    ),
    set_error_handler(190, fail/0),
    set_chtab(0'$, solo),
    set_chtab(0'\, solo),
    read_bip_file(N),
    wait(Pid, _),
    set_chtab(0'/, first_comment),
    set_chtab(0'$, symbol),
    set_chtab(0'\, escape),
    reset_error_handler(133),
    reset_error_handler(139),
    reset_error_handler(190).

load_extensions :-
    lib(fd),
    lib(chr),
    lib(propia),
    lib(conjunto),
    lib(r).

load_db :-
    (get_flag(extension, megalog) ->
	use_module(library(database_kernel)),
	lib(db),
	lib(kb)
    ;
	true
    ).

read_bip_file(N) :-
    read_string(N, end_of_line, _, BipFile),
    !,
    open(BipFile, read, F),
    read_string(F, "", _, File),
    check_bip_defined(BipFile, File),
    find_seealso(File, SeeAlso),
    check_preds(BipFile, SeeAlso, "nonexisting referenced"),
    close(F),
    read_bip_file(N).
read_bip_file(_).

% Find the pred name and check if it is defined
check_bip_defined(BipFile, File) :-
    substring(File, Pos, _, "manpage"),
    Pos1 > Pos + 9,
    substring(File, Pos1, 1, "}"),
    Pos2 > Pos1,
    substring(File, Pos2, 1, "{"),
    Pos3 > Pos2,
    substring(File, Pos3, 1, "}"),
    Start is Pos2 + 1,
    Length is Pos3 - Start,
    substring(File, Start, Length, BipS),
    !,
    check_preds(BipFile, BipS, "nonexisting documented").

find_seealso(File, SeeAlso) :-
    (substring(File, Pos, _, "\\bipseealso") ->
	Pos1 > Pos,
	substring(File, Pos1, 1, "}"),
	Length is Pos1 - Pos - 12,
	Start is Pos + 12,
	substring(File, Start, Length, SeeAlso),
	!
    ;
	SeeAlso = ""
    ).

%
% A finite automaton to find all atom/integer sequences.
%
check_preds(From, Preds, What) :-
    open(Preds, string, S),
    check_pred1(From, S, What),
    close(S).

check_pred1(From, S, What) :-
    read_token(S, Name, T1),
    !,
    (Name = "\\" ->
	check_pred1(From, S, What)
    ;
    Name = backslash ->
	check_pred2(From, S, \, What)
    ;
    Name = sim ->
	check_pred2(From, S, ~, What)
    ;
    Name = verb ->
	get_char(S, Char),
	read_string(S, Char, _, SNew),
	atom_string(New, SNew),
	check_pred2(From, S, New, What)
    ;
    T1 = atom ->
	check_pred2(From, S, Name, What)
    ;
	check_pred1(From, S, What)
    ).
check_pred1(_, _, _).

check_pred2(From, S, Name, What) :-			% we have the atom
    read_token(S, A, B),
    !,
    (A = (/) ->
	check_pred3(From, S, Name, What)
    ;
    A = ("\\") ->
	check_pred2(From, S, Name, What)
    ;
    A = backslash ->
	check_pred2(From, S, Name, What)
    ;
    A = space ->
	check_pred2(From, S, Name, What)
    ;
    A = sim ->
	check_pred2(From, S, '~', What)
    ;
    A = "$" ->
	check_pred2(From, S, Name, What)
    ;
    B = atom ->
	(Name = (/); Name = (\); Name = # ->
	    concat_atoms(Name, A, New)
	;
	    New = A
	),
	check_pred2(From, S, New, What)
    ;
	check_pred1(From, S, What)
    ).
check_pred2(_, _, _, _).

check_pred3(From, S, Name, What) :-			% atom and /
    read_token(S, A, B),
    !,
    (B = integer ->
	write_pred(Name, A, From, What),
	check_pred1(From, S, What)
    ;
    B = atom ->
	check_pred2(From, S, Name, What)
    ;
	check_pred1(From, S, What)
    ).
check_pred3(_, _, _, _).

write_pred(Name, A, From, What) :-
    (valid_pred(Name/A) ->
	true
    ;
	pathname(From, Par, File),
	pathname(Par, _, Type),
	concat_strings(Type, File, FT),
	printf("%-30.30s: %w   \t%s\n%b", [FT, Name/A, What])
    ).

xform_name(Name, RealName) :-
    string_list(Name, L),
    xform_name_list(L, RL),
    string_list(RealName, RL).

xform_name_list([], []).
xform_name_list([0'\|L], P) :-
    !,
    (name("backslash", BL),
    append(BL, R, L) ->
	P = [0'\|S],
	xform_name_list(R, S)
    ;
	xform_name_list(L, P)
    ).
xform_name_list([0'$|L], P) :-
    !,
    xform_name_list(L, P).
xform_name_list([C|L], [C|P]) :-
    xform_name_list(L, P).

valid_pred(begin_module/1).
valid_pred(module_interface/1).
valid_pred(import_from/2).
valid_pred((->)/2).
valid_pred((->)/3).
valid_pred((-?->)/1).
valid_pred(pragma/1).
valid_pred(\^\ /3).				% ^/3
valid_pred(\^\ /2).				% ^/2
valid_pred(Pred) :-
    get_flag(Pred, definition_module, M),
    M \= sepia.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check for builtins which are not documented
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

undoc_bips :-
    % Load all libraries that have help pages
    set_error_handler(133, true/0),
    set_error_handler(139, true/0),
    lib(lists),
    lib(sorts),
    lib(strings),
    lib(profile),
%    lib(statistics),
    load_db,
    load_extensions,
    printf("Undocumented and visible predicates:%n%b", []),
    undoc_bips(sepia_kernel).
undoc_bips :-
    reset_error_handler(133),
    reset_error_handler(139).


undoc_bips(Lib) :-
    (current_module(Lib) -> true; lib(Lib)),
    read_index_file,
    unlock(sepia_kernel, "Sepia"),
    current_module(M),
    (current_built_in_body(Pred, M), T = 'S'
    ;current_predicate_body(Pred, M), T = 'U'),
    get_flag_body(Pred, definition_module, M, M),
    (get_flag(Pred, visibility, global) -> V = 'G'
    ;get_flag(Pred, visibility, imported) -> V = 'E'),
    get_flag(Pred, definition_module, DM),
    not(ignored_module(DM)),
    Pred=N/A,
    not(bip(N,A, _, _, _)),
    not(allowed_export(Pred)),
    printf("(%a%a)%-15.15s: %w%n%b", [T, V, M, Pred]),
    fail.

allowed_export(bytes_to_term/2).
allowed_export(compiled_stream/1).
allowed_export(current_built_in_body/2).
allowed_export(current_predicate_body/2).
allowed_export(get_flag_body/4).
% suspend
allowed_export(unify_suspend/2).
allowed_export(compare_instances_suspend/3).
allowed_export(delayed_goals_suspend/3).
allowed_export(delayed_goals_number_suspend/2).
allowed_export(tr_if_suspend/3).
% FD PREDICATES
% auxiliaries
allowed_export(delayed_goals_number_domain/2).
allowed_export(delayed_goals_domain/3).
allowed_export(copy_term_domain/2).
allowed_export(compare_instances_domain/3).
allowed_export(unify_domain/2).
allowed_export(test_unify_domain/2).
allowed_export(linear_term_range/3).
allowed_export(term_to_linear/2).
allowed_export(dvar_update_nocheck/3).
%    macro transformations
allowed_export(tr_fd_out/2).
allowed_export(tr_fd_arith_out/2).
allowed_export(tr_fd_arith_in/2).
allowed_export(tr_fd_arith_bool/2).
allowed_export(tr_fd_domain_in/2).
allowed_export(tr_fd_domain_out/2).
%    debug macros
allowed_export(debug_handler_d/2).
allowed_export(debug_handler_w/2).
%    expanded macros
allowed_export(fd_ineq/1).
allowed_export(fd_ge/1).
allowed_export(fd_eq/1).
allowed_export(fd_qeq/3).
allowed_export(fd_re/2).
allowed_export(fd_gec/5).
allowed_export(fd_gec_ent/6).
allowed_export(fd_ge/2).
allowed_export(fd_eq/2).
allowed_export(fd_dom_simple/2).
allowed_export(fd_dom_simple/3).
% CHR predicates
allowed_export(tr_chr/2).
allowed_export(chr_start_handler/3).
allowed_export(chr_delayed_goals_handler/3).
allowed_export(coca/1).
allowed_export('CHR='/2).
allowed_export('CHRdelay'/2).
allowed_export('CHRnonvar'/1).
allowed_export('CHRfail'/0).
allowed_export('CHRhead_not_kept'/1).
allowed_export('CHRvar'/1).
allowed_export('CHRgen_num'/1).
allowed_export('CHRcheck_and_mark_applied'/2).
allowed_export('CHRcheck_and_mark_applied'/5).
allowed_export('CHRkeep_heads_checking'/4).
allowed_export('CHRkeep_heads_checking'/6).
allowed_export('CHRalready_in'/1).
allowed_export('CHRkill'/1).
allowed_export('CHRget_delayed_goals'/2).
allowed_export(chr_macro/2).


ignored_module(systools).
ignored_module(define).
ignored_module(sepia).
ignored_module(database_kernel).	% Too many missing - why??
ignored_module(fd_util).		% source is distributed
ignored_module(structures).		% in manual appendix
