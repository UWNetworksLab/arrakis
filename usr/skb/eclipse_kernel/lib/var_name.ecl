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
% Copyright (C) 2001-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: var_name.ecl,v 1.1 2008/06/30 17:43:50 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(var_name).

:- comment(summary, "Allows variables to be given stable and unique names.").

:- comment(desc, html("\
    <P>
    The var_name library is used to allow variables to be given unique and
    stable names. These names are printed whenever the variable is printed.
    The main novel feature is that the name is unique at any time, and that
    this name is stable in that it would not change between runs of the same
    program.
    </P><P>
    These variable names are intended solely for purposes of debugging
    and visualisation of a program execution. Any use of this feature
    in the actual program logic or algorithm is strongly discouraged.
    </P>
")).


:- export set_var_name/2, set_var_name_counter/2, get_var_name/2, get_a_var_name/2.
 
:- meta_attribute(var_name, []).

:- lib(hash).

:- local reference(names).


% if this is changed, need to change code for _print_var() in write.c!
:- local struct(vname(basename, number)).

:- import set_bip_error/1, get_bip_error/1 from sepia_kernel.

%
% set_var_name(?Var, ++BaseName)
%
set_var_name(V, BaseName0) :-
	check_var_name(BaseName0, BaseName), !,
	get_name_counter(BaseName, C0),
	set_var_name1(V, BaseName, C0, C),
	set_name_counter(BaseName, C).
set_var_name(V, BaseName) :-
	get_bip_error(E),
	error(E, set_var_name(V, BaseName)).

set_var_name1(V, BaseName, C0, C) :-
	var(V), !,
	set_a_var_name(V, BaseName, C0, C).
set_var_name1(S, BaseName, C0, C) :-
	compound(S), !,
	(foreacharg(A, S), fromto(C0, C1,C2, C), param(BaseName) do
	    set_var_name1(A, BaseName, C1, C2)
	).
set_var_name1(_, _, C, C).


set_var_name_counter(BaseName0, BaseCount) :-
	(integer(BaseCount) -> true ; set_bip_error(5)),
	check_var_name(BaseName0, BaseName), !,
	\+is_existing_name(BaseName),
	get_name_counter(BaseName, _),
	set_name_counter(BaseName, BaseCount).
set_var_name_counter(BaseName, BaseCount) :-
	get_bip_error(E),
	error(E, set_var_name_counter(BaseName, BaseCount)).

check_var_name(BaseName0, BaseName) :-
	(atomic(BaseName0) -> true ; set_bip_error(5)),
	concat_string([BaseName0], BaseName), 
	valid_var_name(BaseName).


set_a_var_name(V, BaseName, C0, C) :-
	\+ has_var_name(V),
	% fails if V already has a name
	add_attribute(V, Attr),
	C is C0 + 1,
	Attr = vname{basename: BaseName, number: C0}.


valid_var_name(BaseName) :-
	string_code(BaseName, 1, First),
	get_chtab(First, Class),
	var_class(Class).

    var_class(underline) :- !.
    var_class(upper_case) :- !.
    var_class(_) :- set_bip_error(6).


has_var_name(_{var_name:Attr}) ?-
	nonvar(Attr).
	    

get_name_counter(BaseName, C) :-
	getval(names, H0),
	( H0 == 0 ->   % not used yet
	    hash_create(H),
	    setval(names, H),
	    C = 0
	; hash_find(H0,BaseName, E) ->
	    E = C  % just the counter for now
	;   C = 0  % start at 0
        ).

set_name_counter(BaseName, C) :-
	getval(names, H), % assume get_name_counter/1 called...
	hash_add(H, BaseName, C).

is_existing_name(BaseName) :-
	getval(names, H0),
	H0 \== 0, !,
	hash_find(H0, BaseName, _).


get_a_var_name(Var, Name) :-		% backward compatibility
	get_var_name(Var, Name).

get_var_name(_{var_name:Attr}, Name) ?-
% this should be the same format as in write.c
	nonvar(Attr), 
	Attr = vname{basename: BaseName, number: C0},
	concat_string([BaseName, '#', C0], Name).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- comment(get_var_name/2, [
	summary: "Retrieve the name given to a variable via set_var_name/2",
	args: ["Var":  "Named variable",
	       "Name": "Variable, will be bound to a string"
	      ],
	amode: get_var_name(?,-),
	fail_if: "Var is not a variable or has not been named via set_var_name/2",
	desc: html("\
	<P>
	  Used to retrieve the unique name that was given to a variable via
	  the set_var_name/2 predicate. This name is always of the form
	  BaseName#N, where BaseName is the base name that was specified in
	  set_var_name/2, and N is an integer.
	</P><P>
	  These variable names are intended solely for purposes of debugging
	  and visualisation of a program execution. Any use of this feature
	  in the actual program logic or algorithm is strongly discouraged.
	</P>
	"),
	see_also: [set_var_name/2]
]).


:- comment(set_var_name/2, [
	summary: "Give a unique name to each variable in Vars based on BaseName and a counter.",
	args: ["Vars":      "Variable(s) to be named (Prolog term)",
	       "BaseName": "BaseName of variable (atom or string)"
	      ],
	amode: set_var_name(?,++),
	desc: html("\
    <P>
      Give unique and stable names to the variables in Vars. The names have
      the format BaseName#N where N is the value of a counter, which is
      incremented each time a variable is named by <TT>set_var_name/2</TT>. 
      Thus all such named variables are given a unique name. This name is 
      printed wherever the `source name' of a variable would otherwise
      be printed.  Wherever the system would normally add an arbitrary
      number to the printed name (e.g. the 'V' option in printf, or the
      variables(full) option in write_term printing Name_Number), this
      is not done with named variables.

    </P><P>
      BaseName must begin with a capital letter, thus the name looks like a
      normal ECLiPSe variable name, but they can be recognised as named
      variables because a normal variable name cannot contain the '#'
      symbol.  Each BaseName has its own counter, which by default starts
      at 0 and is incremented each time a variable with that BaseName is
      named. The default starting value can be changed by
      set_var_name_counter/2 before any calls to set_var_name/2 with the
      BaseName.  Note that the counter value is backtrackable.
      
    </P><P>
      Vars is usually a variable, or a list of variables that the user would
      like to have the same base name BaseName. Vars is traversed in the usual
      depth-first left-to-right manner and the variables found numbered 
      consecutively with the same base name BaseName. 

    </P><P>
      The main feature of this name is that the name is unique and stable.
      The name is unique in that at any given time during an ECLiPSe
      execution, the combination of the BaseName and counter can only refer
      to one variable. The name is stable in that this unique name would
      not change between runs of the program or running the program on
      different platforms. Thus, this allows the user to for example
      uniquely identify a variable between a normal and a debugging run of
      the program.

    </P><P>
      The predicate fails if any variables in Vars has already been given a
      name. In particular, this means that all variables in Vars should
      occur only once. When two named variable are unified, the older name
      is retained, as in existing rules for variable names.

   </P><P>
      The variable name is implemented as an attribute. Thus all named
      variables become attributed variables, which is recognised by
      ECLiPSe and treated specially when a named variable is printed. It is
      possible to construct a named variable by directly creating the
      attribute, either by reading in a previous named variable written
      using writeq, or by the user explicitly constructing the
      attribute. In such cases, the name is not recorded by the system and
      may not be unique in that set_var_name/2 would not take it into
      account in naming new variables.

"),
	fail_if: "Var contain variable(s) that have already been named by set_var_name/2.",
	exceptions: [
             5: "BaseName is not an atom or string.",
             6: "BaseName does not begin with a capital letter."
        ],
	resat: no,
	see_also: [get_var_name/2, set_var_name_counter/2],
	index: ["variable name", "naming variable", "stable and unique names"],
	eg: "\

[eclipse 1]: lib(var_name).
var_name.ecl compiled traceable 1308 bytes in 0.02 seconds

Yes (0.03s cpu)
[eclipse 2]: 


[eclipse 3]: set_var_name(X, 'Myname'), set_var_name(Y, \"Myname\").

X = Myname#0
Y = Myname#1
Yes (0.00s cpu)
[eclipse 4]: set_var_name(X, 'Count'), writeln(X).
Count#0

X = Count#0
Yes (0.00s cpu)

% name is backtrackable. Original source name printed in second branch
[eclipse 5]: (set_var_name(X, 'Name') ; true), writeln(X), fail.       
Name#0
X

No (0.00s cpu)

% naming more than one variable at a time...
[eclipse 10]: length(L,5), set_var_name(L, 'Start').

L = [Start#0, Start#1, Start#2, Start#3, Start#4]
Yes (0.00s cpu)

% counter is backtrackable
[eclipse 11]: (set_var_name([X,Y], 'Name') ; set_var_name(Y, 'Name')), writeln(X-Y), fail.
Name#0 - Name#1
X - Name#0

No (0.00s cpu)

% each base name has its own counter
[eclipse 12]: set_var_name(X, 'First'), set_var_name(Y, 'Second'), writeln(X-Y).
First#0 - Second#0

X = First#0
Y = Second#0
Yes (0.00s cpu)

% older name is retained on unification
[eclipse 9]:  set_var_name(X, 'First'), set_var_name(Y, 'Second'), X = Y, writeln(Y).
First#0

X = First#0
Y = First#0
Yes (0.00s cpu)

[eclipse 10]: lib(fd), X::[1..5], set_var_name(X, 'Domain'), writeln(X).
fd_domain.pl compiled traceable 22556 bytes in 0.04 seconds
fd_arith.pl compiled traceable 72296 bytes in 0.13 seconds
fd_util.pl compiled traceable 2128 bytes in 0.02 seconds
fd_chip.pl compiled traceable 4720 bytes in 0.05 seconds
fd_elipsys.pl compiled traceable 11036 bytes in 0.02 seconds
fd.pl      compiled traceable 17256 bytes in 0.29 seconds
Domain#0{[1..5]}

X = Domain#0{[1..5]}
Yes (0.29s cpu)

% fails if variable already named
[eclipse 7]: set_var_name(X, 'Name'), set_var_name(X, 'New').

No (0.00s cpu)

% no number is attached to name when printed with 'V' option
[eclipse 14]: set_var_name(X, 'Myname'), printf(\"%Vw%n%Vw%n\", [X,Y]).
Myname#0
Y_177

X = Myname#0
Y = Y
Yes (0.00s cpu)

% writeq does not print the name
[eclipse 15]: set_var_name(X, 'Myname'), writeq(X).
_282{suspend : _285, var_name : vname(\"Myname\", 0)}
X = Myname#0
Yes (0.00s cpu)

[eclipse 12]: set_var_name(X, 123).
out of range in set_var_name(X, 123)
Abort
[eclipse 13]: set_var_name(X, atomic).
out of range in set_var_name(X, atomic)
Abort
[eclipse 14]: set_var_name(X, f(structure)).
type error in set_var_name(X, f(structure))
Abort


"
]).

:- comment(set_var_name_counter/2, [
	summary: "Set the initial value for the counter for variable name BaseName.",
	args: ["BaseName": "A valid base-name (atom or string)",
	       "Start":    "Start value for counter (integer)"
	      ],
	amode: set_var_name_counter(++,++),
	desc: html("\
    <P>
	Set the initial counter value for a variable name BaseName that will
        then be used in set_var_name/2, with Start as the initial value of 
        the counter. If BaseName already exists (i.e. variables have already
        been named using BaseName), the predicate will fail.

    </P><P>
        It is expected that the main use of this predicate will be to change
        the default counter start value from 0 to 1. However, no restrictions 
        are placed on what the start value may be.
"),
	fail_if: "BaseName already exists.",
	exceptions: [
             5: "BaseName is not an atom or string.",
	     5: "Start is not an integer.",
             6: "BaseName does not begin with a capital letter."
        ],
	resat: no,
	see_also: [set_var_name/2]
]).

