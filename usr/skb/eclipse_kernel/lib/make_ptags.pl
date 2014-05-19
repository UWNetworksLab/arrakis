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
% Version:	$Id: make_ptags.pl,v 1.1 2008/06/30 17:43:47 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 */

/*
 * IDENTIFICATION:	make_ptags.pl
 *
 * DESCRIPTION: 	Tags file creator.
 *
 *
 * CONTENTS:     
 *
 * REVISION HISTORY:
 * AUTHOR	VERSION	DATE	REASON
 * Joachim Witte 1.0	07.07.90
 */

:- module(make_ptags).

:- use_module(library(ptags)).

:- export
	make_ptags/0,
	make_ptags/1.

% make_ptags/0
% make_ptags
% creates a prolog tags file "tags" for all *.pl files in the cwd

make_ptags :-
	make_ptags('tags').

% make_ptags/1
% make_ptags(Tags)
% creates a prolog tags file Tags for all *.pl files in the cwd

make_ptags(Tags) :-
	!,
	read_directory(., '*.pl', _, List),
	sort(List, SList),
	% remove files we are not interested in which hide useful tags
	subtract(SList, ["bsi.pl", "oldio.pl"], NewList),
	make_ptags(NewList, Tags).

% make_ptags/2
% make_ptags(List, Tags)

make_ptags(X, Tags) :-
	var(X), !,
	error(4, make_ptags(X, Tags)).
make_ptags([], _) :-
	!.
make_ptags([File|Files], Tags) :-
	!,
	(string(Tags) ->
		TagsS = Tags
	;
		atom(Tags) ->
		atom_string(Tags, TagsS)
	;
		error(5, make_ptags(File, Tags))
	),
	get_flag(pid, Pid),
	concat_string(['/tmp/sepia_ptags', Pid], TempFile),
	open(TempFile, write, TagsStream),
	make_tags([File|Files], TagsStream),
	close(TagsStream),
	reset_error_handler(68),
	concat_string(['sort +0 -1 -u ', TempFile], ShellString1),
	concat_string([ShellString1, ' > '], ShellString2),
	concat_strings(ShellString2, TagsS, ShellString),
	sh(ShellString),
	delete(TempFile).

% make_tags/2
% make_tags(File, TagsStream)

make_tags([], _) :-
	!.
make_tags([File|Files], TagsStream) :-
	!,
	sepia,
	tags(File, TagsStream),
	make_tags(Files, TagsStream).

% sepia/0
% sepia
% resets the compatibility to sepia

sepia :-
	( get_chtab(0'", string_quote) ->
		true
	;
		set_chtab(0'", string_quote)
	),
	set_chtab(0'$, symbol),
	set_chtab(0'&, symbol),
	set_chtab(0'\, escape),
	set_chtab(0'|, special),
	set_chtab(128, blank_space),	% there had to be be some string_quote
	set_flag(syntax_option, not(blanks_in_nil)),
%	set_flag(syntax_option, not(cprolog_bar)),
	set_flag(syntax_option, not(limit_arg_precedence)),
	set_flag(syntax_option, not(nl_in_quotes)),
	set_flag(syntax_option, not(no_blanks)),
	set_error_handler(68, fail/0),	% to cope with macro expansion
	op(500, fx,  (@)),	% introduces pce object reference
	op(500, xfy, (:)),	% separator between pce behaviour and value
	op(100, xfy, (/\)),
	op(1000, fy, (nospy)),
	op(1000, fy, (spy)),
	op(200, xfy, (\/)),
	op(500, fx, '+'),
	op(500, fx, '-'),
	op(900, fy, (not)).
