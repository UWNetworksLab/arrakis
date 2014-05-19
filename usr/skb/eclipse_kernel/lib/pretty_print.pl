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
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: pretty_print.pl,v 1.1 2008/06/30 17:43:48 jschimpf Exp $
% ----------------------------------------------------------------------

/*
 * SEPIA PROLOG SOURCE MODULE
 *
 * IDENTIFICATION:	pretty_print.pl 
 *
 * DESCRIPTION: 	Utility predicates, for user convenience.
 *
 * CONTENTS:
 *
 */


:- module(pretty_print).

:- comment(summary, "Pretty-printing of complex terms").
:- comment(author, "Micha Meier, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:48 $").
:- comment(pretty_print/3, [template:"pretty_print(+Stream, +Term, +Max)",
    summary:"Print a term on the given stream, split it if its width exceeds Max"
    ]).

:- export pretty_print/3.

% Print a term on the given stream, split it if its size exceeds Max
pretty_print(Stream, Term, Max) :-
        open(string(""), write, S),
        pp(S, Term, 0, Max),
        get_stream_info(S, name, String),
        close(S),
        write(Stream, String).

% First try to print the term using write/2, if it is too big,
% split it onto separate lines
pp(S, Term, Offset, MaxSize) :-
        at(S, Start),
        write(S, Term),
        (at(S) < Start + MaxSize ->
            true
        ;
            seek(S, Start),      % rewind the output
            NewOffset is Offset + 3,
            functor(Term, F, N),
            printf(S, "%a(\n", F),
            pp_arg(S, 1, N, Term, NewOffset, MaxSize),
            printf(S, "%*c)", [Offset, 0' ])
        ).

pp_arg(S, N, N, Term, Off, MaxSize) :-
        !,
        printf(S, "%*c", [Off, 0' ]),
        arg(N, Term, Arg),
        pp(S, Arg, Off, MaxSize),
        nl(S).
pp_arg(S, I, N, Term, Off, MaxSize) :-
        printf(S, "%*c", [Off, 0' ]),
        arg(I, Term, Arg),
        pp(S, Arg, Off, MaxSize),
        write(S, ',\n'),
        I1 is I + 1,
        pp_arg(S, I1, N, Term, Off, MaxSize).

