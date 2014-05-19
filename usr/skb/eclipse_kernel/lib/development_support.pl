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
% Version:	$Id: development_support.pl,v 1.2 2008/08/04 01:13:40 kish_shen Exp $
% ----------------------------------------------------------------------

%
% $Id: development_support.pl,v 1.2 2008/08/04 01:13:40 kish_shen Exp $
%

%----------------------------------------------------------------------
:- module(development_support).
%----------------------------------------------------------------------

:- export
	named_structure/4,
	port_remove_levels/4,
	print_source/3,
	print_struct_names/4,
	valid_attributes_listing/2,
	valid_output_option/3,
	written_term/4,
	perform_transformation/5.

%----------------------------------------------------------------------
:- pragma(system).
:- pragma(nodebug).

:- import
	illegal_macro/5,
	meta_attributes/1,
	get_attribute/3
    from sepia_kernel.


%----------------------------------------------------------------------
% Inspector support
%----------------------------------------------------------------------

port_remove_levels(0, Pos, Pos, Status) :- !, Status = true.
port_remove_levels(_N, [], [], Status) :- !, % too many levels, stop at top
	Status = false.
port_remove_levels(N, [_|Pos0], Pos, Status) :- 
   N1 is N - 1,
   port_remove_levels(N1, Pos0, Pos, Status).

/* written_term(+Goal, +Term, -Written, +Module) returns in Written the written
   form of Term. Goal == Term if Term is to be treated as a goal
*/
written_term(Goal, Term, Written, Module) :-
	get_flag(output_mode, Mode),
	perform_transformation(Goal, Term, Mode, Written, Module).

perform_transformation(Goal, Term, Mode, Written, Module) :-
        ( substring(Mode, "T", _) ->
	    Written = Term
	;
	    ( Goal == Term ->
		portray_term(Term, Written, goal)@Module
	    ;
		portray_term(Term, Written, top_term)@Module
	    )
	).

 
% provides a list of valid attribute specifications (as strings)
valid_attributes_listing(Var, Listing) :-
	meta_attributes(Atts),
	valid_attributes_listing1(Atts, Var, Listing, []).

valid_attributes_listing1([],_, LIn, LOut) ?- !, LOut = LIn.
valid_attributes_listing1([[AttName|Index]|Atts], Var, L0, L) :-
	valid_attributes_listing1(Atts, Var, L0, L1),
	(get_attribute(Var,_,Index) ->
	    term_string(Index-AttName, AttSpec),
	    L1 = [AttSpec|L]
	  ; L1 = L
        ).

% check if Struct has field names. If so, returns in Defs the field names
named_structure(Struct, Module, Defs, A) :-
	functor(Struct, F,A),
	functor(Defs0, F,A),
	functor(Defs, F,A),
	current_struct(F, Defs0)@Module,
	(foreacharg(Name0, Defs0), foreacharg(Name, Defs), param(Defs0) do
	    (atom(Name0) ->
		Name0 = Name
	    ; Name0 = Name:_Substruct ->
	    	true
            ;
	     printf(error, "ERROR: unrecognised format for structure field index: %w in %w.%n", [Name0,Defs0]),
             Name = '%%UNKNOWN%%'
	    )
        ).

print_struct_names(A, A, Stream, Defs) ?- !,
	arg(A, Defs, Name),
	printf(Stream, "%d=%w", [A,Name]).
print_struct_names(N, A, Stream, Defs) :-
	arg(N, Defs, Name),
	printf(Stream, "%d=%w ", [N,Name]),
        N1 is N + 1,
	print_struct_names(N1, A, Stream, Defs).


%----------------------------------------------------------------------
% Print predicate definition (source)
%----------------------------------------------------------------------

print_source(Stream, N/A, M) :-
	(
	    atom(N),
	    get_flag(N/A, definition_module, DM)@M,
	    get_flag(N/A, source_file, AbsFile)@M,
	    get_flag(N/A, source_line, Line)@M,
	    get_flag(N/A, source_offset, Start)@M,
	    rel_file_name(AbsFile, File),
	    printf(Stream, "%w in file %s, line %d:%n", [N/A,File,Line]),
	    open(File, read, SS),
	    seek(SS, Start),
	    block(find_end_of_pred(SS, N, A, DM, Stop), _, (close(SS),fail)),
	    echo_source(SS, Start, Stop, Stream),
	    close(SS),
	    !
	;
	    
	    printf(Stream, "%n*** Can't print source for %w in module %w", [N/A, M])
	).

    rel_file_name(AbsFile, Rel) :-
	getcwd(Cwd),
	atom_string(AbsFile, FileS),
	( append_strings(Cwd, Rel, FileS) ->
	    true
	;
	    Rel = FileS
	).

    % this might not always work due to operators, macros etc...
    find_end_of_pred(SS, N, A, M, Pos) :-
    	repeat,
	    at(SS, Pos),
	    read(SS, Clause)@M,
	    \+ same_functor(Clause, N, A),
	!.

    same_functor(Head :- _, N, A) ?- functor(Head, N, A).
    same_functor(Head ?- _, N, A) ?- functor(Head, N, A).
    same_functor(Head, N, A) ?- functor(Head, N, A).

    echo_source(InStream, From, To, OutStream) :-
	seek(InStream, From),
	repeat,
	    read_string(InStream, end_of_line, _, Line),
	    writeln(OutStream, Line),
	at(InStream) >= To.


%----------------------------------------------------------------------
% Output mode format character codes
% valid_output_option(?OptionChar, -Description, -IncompatibleOptions)
%----------------------------------------------------------------------

valid_output_option(0'O, "don't use operator syntax",		[]).
valid_output_option(0'D, "disregard print depth",		[]).
valid_output_option(0'K, "don't print unnecessary spaces",	[]).
valid_output_option(0'., "print lists in ./2 notation", 	[]).
valid_output_option(0'Q, "quote atoms if necessary",		[]).
valid_output_option(0'v, "print all variables like _12345",	[0'V,0'_]).
valid_output_option(0'V, "print all variables like Varname_12345", [0'v,0'_]).
valid_output_option(0'_, "print all variables as _",		[0'v,0'V]).
valid_output_option(0'P, "use portray/1,2",			[]).
valid_output_option(0'U, "use portray/1,2 even for variables",	[]).
valid_output_option(0'm, "print meta-attributes using their print handlers", [0'M]).
valid_output_option(0'M, "print meta-attributes literally",	[0'm]).
valid_output_option(0'C, "print as a clause (apply clause transformations)", []).
valid_output_option(0'T, "do not apply write transformations",	[]).

