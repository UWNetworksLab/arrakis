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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): J. Chamois
% 
% END LICENSE BLOCK
:- module(regex).

:- comment(categories, ["Algorithms","Programming Utilities"]).
:- comment(summary, "Interface to POSIX regular expression handling").
:- comment(author, "J Chamois").
:- comment(date, "$Date: 2009/02/19 05:45:20 $").
:- comment(copyright, "Public domain").

:- export
	compile_pattern/3,
	match/2, match/3,
	match/4,
	matchsub/4,
	matchall/4,
	split/4.

:- get_flag(hostarch, Arch),
   get_flag(object_suffix, SO),
   concat_string([Arch,"/eregex.",SO], SOFile),
   ( exists(SOFile) ->
       load(SOFile),
	external(compile_pattern/3, ec_regcomp),% (Pattern,Options,Compiled)
	external(match/3, ec_regmatch),		% (Pattern,String,Options)
	external(match/4, ec_regmatch4),	% (Pattern,String,Options,Match)
	external(matchsub/4, ec_regmatchsub),	% (Pattern,String,Options,Subs)
	external(matchall/4, ec_regmatchall),	% (Pattern,String,Options,All)
	external(split/4, ec_regsplit)		% (Pattern,String,Options,All)
   ;
	% Because of libraries test, don't make error messages at load time
   	compile_term([
	    compile_pattern(P,O,C) :- error(141, compile_pattern(P,O,C)),
	    match(P,S,O) :- error(141, match(P,S,O)),
	    match(P,S,O,M) :- error(141, match(P,S,O,M)),
	    matchsub(P,S,O,M) :- error(141, matchsub(P,S,O,M)),
	    matchall(P,S,O,M) :- error(141, matchall(P,S,O,M)),
	    split(P,S,O,M) :- error(141, split(P,S,O,M))
	])
%   	printf(error,
%	    "Sorry, library(regex) currently not available on %s architecture%n",
%	    [Arch])
   ).

match(Pattern, String) :-
	match(Pattern, String, []).


/*
bench(N) :-
	P = "ab+a",
	S = "shghcgbfhscggfcgabahdfnavlkb",
	cputime(T0),
	bench1(N, P, S), cputime(T1), R1 is T1-T0, writeln(R1),
	bench2(N, P, S), cputime(T2), R2 is T2-T1, writeln(R2),
	bench3(N, P, S), cputime(T3), R3 is T3-T2, writeln(R3),
	true.

bench1(N, P, S) :-
	between(1,N,1,_),
	match(P, S, []),
	fail.
bench1(_, _, _).

bench2(N, P, S) :-
	between(1,N,1,_),
	match(P, S, [], _),
	fail.
bench2(_, _, _).

bench3(N, P, S) :-
	compile_pattern(P, [], C),
	between(1,N,1,_),
	match(C, S, [], _),
	fail.
bench3(_, _, _).

bench1(N, P, S) :-
	( for(_,1,N),param(P,S) do match(P, S, []) ).

bench2(N, P, S) :-
	( for(_,1,N),param(P,S) do match(P, S, [], _) ).

bench3(N, P, S) :-
	compile_pattern(P, [], C),
	( for(_,1,N),param(C,S) do match(C, S, [], _) ).
*/


%----------------------------------------------------------------------
% DOCUMENTATION
%----------------------------------------------------------------------

:- comment(desc, html("
    This library implements an ECLiPSe API for POSIX 1003.2 regular
    expressions (on Unix systems it calls the regular expression
    functions from the standard library, on Windows it uses Henry
    Spencer's regex library version 3.8).

    <H3>Regular Expressions</H3>
    This is just a very brief summary of the essentials.  For details
    of regular expressions see any POSIX regex(7) man page.
    Two types of regular expressions are supported:
    <DL>
    <DT>Extended Regular Expressions (the default)</DT>
	<DD>These are described below and correspond essentially to those
	understood by the UNIX egrep command.</DD>
    <DT>Basic Regular Expressions</DT>
	<DD>These correspond essentially to those in the UNIX ed editor
	or the grep command, and are mostly obsolete.</DD>
    </DL>
    Note that our choice of default differs from the POSIX 1003.2 C API.

    <H4>Characters</H4>
    Every character stands for itself, except for the characters
    ^.[$()|*+?{\\ which must be escaped with a \\ to prevent
    them from having special meaning (and note that, since the ECLiPSe
    parser already interprets backslashes, you will have escape the
    backslash with another backslash in your ECLiPSe source string).
    <DL>
    <DT>.</DT>
	<DD>Matches any character</DD>
    <DT>[aeiou]</DT>
	<DD>Matches any of the characters between the brackets</DD>
    <DT>[^aeiou]</DT>
	<DD>Matches any character except those listed</DD>
    <DT>[a-z0-9]</DT>
	<DD>Matches any character in the given ranges</DD>
    </DL>

    <H4>Anchors</H4>
    <DL>
    <DT>^</DT>
	<DD>Matches at the beginning of the string (or line)</DD>
    <DT>$</DT>
	<DD>Matches at the end of the string (or line)</DD>
    </DL>

    <H4>Repetition</H4>
    <DL>
    <DT>?</DT>
	<DD>Matches the preceding element 0 or 1 times</DD>
    <DT>*</DT>
	<DD>Matches the preceding element 0 or more times</DD>
    <DT>+</DT>
	<DD>Matches the preceding element 1 or more times</DD>
    <DT>{3}</DT>
	<DD>Matches the preceding element 3 times</DD>
    <DT>{1,3}</DT>
	<DD>Matches the preceding element 1 to 3 times</DD>
    </DL>

    <H4>Grouping</H4>
    <DL>
    <DT>(subexpr)</DT>
	<DD>Matches the parenthesized expression. This grouping is used
	in connection with the repetition operators, or for indicating 
	subexpressions whose matches are to be captured and returned</DD>
    <DT>(one|two|three)</DT>
	<DD>Matches any of the alternative expressions</DD>
    </DL>

    <H3>Options</H3>
    Most of the predicates in this library accept a list of options.
    The accepted options are:
    <DL>
    <DT><TT>basic</TT></DT>
	<DD>Interpret the pattern as a Basic Regular Expression, rather
	than the default Extended Regular Expression.</DD>
    <DT><TT>extended</TT></DT>
	<DD>Interpret the pattern as an Extended Regular Expression
	(this flag is redundant since this is the default).</DD>
    <DT><TT>icase</TT></DT>
	<DD>Ignore case when matching.</DD>
    <DT><TT>newline</TT></DT>
	<DD>Treat newlines specially, i.e. don't treat them as normal
	characters and make ^ match after a newline and $ before a newline.
	By default, newlines are treated as ordinary characters.</DD>
    <DT><TT>notbol</TT></DT>
	<DD>Don't interpret the beginning of the string as the beginning
	of a line, i.e. don't let ^ match there.</DD>
    <DT><TT>noteol</TT></DT>
	<DD>Don't interpret the end of the string as the end of a line,
	i.e. don't let $ match there.</DD>
	<DD></DD>
    </DL>
    <H3>Shortcomings</H3>
    <OL>
    <LI>
    Due to limitations of the underlying implementation, the predicates
    in this library do not handle embedded NUL characters in strings correctly
    (they are interpreted as the end of the string).
    <LI>
    POSIX regular expressions don't seem to have a notion of  \"noncapturing
    parentheses\", i.e. parentheses that are only used for grouping, not for
    indicating that one wants to capture the matching substring.
    <LI>
    In an environment like ECLiPSe, one would like to be able to do things like
    <PRE>
	?- ideal_match(\"(/[^/]*)+\", \"/usr/local/eclipse\", L).
	L = [\"/usr\", \"/local\", \"/eclipse\"]
	Yes
    </PRE>
    i.e. capture every instance of a matching subexpression.  There seems
    to be no way to do that with a POSIX regexp implementation.
    </OL>
")).

:- comment(match/2, [
    summary:"A substring of String matches the regular expression Pattern",
    args:["Pattern":"A string (or a compiled pattern handle)",
    	"String":"A string"],
    amode:(match(+,+) is semidet),
    fail_if:"String does not match Pattern",
    desc:html("
	Succeeds if all or a substring of String matches the regular
	expression Pattern. For the description of regular expressions
	see the library(regex) page.
	<P>
    	Equivalent to match(Pattern, String, []).
    "),
    see_also:[library(regex),match/3,match/4,matchsub/4,matchall/4,split/4,compile_pattern/3],
    eg:"
    ?- match(\"aca\", \"abracadabra\").
    Yes

    ?- match(\"^a[cd]a$\", \"abracadabra\").
    No

    ?- match(\"^a[cd]a$\", \"ada\").
    Yes
    "
]).

:- comment(match/3, [
    summary:"A substring of String matches the regular expression Pattern",
    args:["Pattern":"A string (or a compiled pattern handle)",
    	"String":"A string",
	"Options":"List of atoms"],
    amode:(match(+,+,+) is semidet),
    fail_if:"String does not match Pattern",
    see_also:[library(regex),match/2,match/4,matchall/4,matchsub/4,split/4,compile_pattern/3],
    desc:html("
	Succeeds if all or a substring of String matches the regular
	expression Pattern. For the description of regular expressions
	see the library(regex) page.
	<P>
    	Options is a (possibly empty) list of atomic option names,
	as described in the library(regex) page.
    "),
    eg:"
    ?- match(\"april\", \"April\", []).
    No
    ?- match(\"april\", \"April\", [icase]).
    Yes

    ?- match(\"(^(a[cd]a)$)+\", \"aca\\nada\", []).
    No
    ?- match(\"(^(a[cd]a)$)+\", \"aca\\nada\", [newline]).
    Yes

    ?- match(\"\\\\<word\\\\>\", \"a word only\", [basic]).
    Yes
    ?- match(\"\\\\<word\\\\>\", \"not words though\", [basic]).
    No
    "
]).


:- comment(match/4, [
    summary:"Match is the first substring of String that matches the regular expression Pattern",
    args:["Pattern":"A string (or a compiled pattern handle)",
    	"String":"A string",
	"Options":"List of atoms",
	"Match":"Output: a string"],
    amode:(match(+,+,+,-) is semidet),
    fail_if:"String does not match Pattern",
    see_also:[library(regex),match/2,match/3,matchsub/4,matchall/4,compile_pattern/3],
    desc:html("
	Succeeds if all or a substring of String matches the regular
	expression Pattern. For the description of regular expressions
	see the library(regex) page.
	<P>
    	Options is a (possibly empty) list of atomic option names,
	as described in the library(regex) page.
	<P>
	Match is bound to the first substring of String which matches
	Pattern.
	<P>
	Note that this predicate does not return any information about
	matching (parenthesised) sub-expressions!
    "),
    eg:"
    ?- match(\"<[a-z]+>\", \"Text with <HTML> tags\", [icase], M).
    M = \"<HTML>\"
    Yes
    "
]).


:- comment(matchsub/4, [
    summary:"A substring of String matches the regular expression Pattern and SubMatches are matching sub-expressions",
    args:["Pattern":"A string (or a compiled pattern handle)",
    	"String":"A string",
	"Options":"List of atoms",
	"SubMatches":"Output: List of strings"],
    amode:(matchsub(+,+,+,-) is semidet),
    fail_if:"String does not match Pattern",
    see_also:[library(regex),match/2,match/3,match/4,compile_pattern/3],
    desc:html("
	Succeeds if all or a substring of String matches the regular
	expression Pattern. For the description of regular expressions
	see the library(regex) page.
	<P>
    	Options is a (possibly empty) list of atomic option names,
	as described in the library(regex) page.
	<P>
	SubMatches is bound to a list of strings, each corresponding to
	a parenthesized subexpression in Pattern. These subexpressions
	are ordered according to the position of their opening parenthesis
	within the pattern. The matching string appears on the corresponding
	position in the SubMatches list. Note that, if a subexpression matches
	several times, only the last match is returned.
	<P>
    "),
    eg:"
    ?- matchsub(\"Name:([^,]+), Age:([0-9]+),\", \"Name:Fred, Age:34,\", [], L).
    L = [\"Fred\", \"34\"]
    Yes
    "
]).


:- comment(matchall/4, [
    summary:"AllMatches is a list of substrings of String which match the regular expression Pattern",
    args:["Pattern":"A string (or a compiled pattern handle)",
    	"String":"A string",
	"Options":"List of atoms",
	"AllMatches":"Output: List of strings"],
    amode:(matchall(+,+,+,-) is det),
    fail_if:"None",
    see_also:[library(regex),match/2,match/3,match/4,split/4,compile_pattern/3],
    desc:html("
	This predicates always succeeds.
	<P>
    	Options is a (possibly empty) list of atomic option names,
	as described in the library(regex) page.
	<P>
	AllMatches is bound to a list of strings. If the input string String
	does not match the pattern, the list is empty. Otherwise the list
	contains substrings of String which match the entire pattern,
	ordered according to their occurrence within String. No overlapping
	matches are returned, i.e. the next match is found by examining the
	remainder of String after the previous match.
	<P>
	Note that this predicate does not return any information about
	matching (parenthesised) sub-expressions!
    "),
    eg:"
    ?- matchall(\"[0-9]+\", \" blue 27 red123 green99\", [], L).
    L = [\"27\", \"123\", \"99\"]
    Yes

    ?- matchall(\"([0-9]+|[^0-9]+)\", \" blue 27 red123 green99\", [], L).
    L = [\" blue \", \"27\", \" red\", \"123\", \" green\", \"99\"]
    Yes
    "
]).


:- comment(split/4, [
    summary:"Parts is a list of substrings, partitioning String according to Pattern",
    args:["Pattern":"A string (or a compiled pattern handle)",
    	"String":"A string",
	"Options":"List of atoms",
	"Parts":"Output: List of strings"],
    amode:(split(+,+,+,-) is det),
    fail_if:"None",
    see_also:[library(regex),matchall/4,compile_pattern/3,concat_string/2,split_string/4],
    desc:html("
	This predicates always succeeds.
	<P>
    	Options is a (possibly empty) list of atomic option names,
	as described in the library(regex) page.
	<P>
	Parts is bound to a list of strings which are consecutive substrings
	of the input string String (i.e. concatenating this list using
	concat_strings/2 will yield the original String). The list is
	constructed such that it has an odd number of elements, where the
	even numbered elements match the pattern, and the odd numbered
	elements contains those portions of String that did not match
	the Pattern. Some of these substrings may be empty.
	<P>
	This partitioning of the String can be used to construct a new
	string with the matches replaced by something else. Use the
	following code pattern:
	<PRE>
	    split(Pattern, String, Options, Parts),
	    (
		fromto(Parts, [NoMatch,Match|NMs], NMs, [Last]),
		fromto(Repl,  [NoMatch,Subst|NSs], NSs, [Last])
	    do
		%%% compute Subst from Match here %%%
	    ),
	    concat_string(Repl, NewString),
	</PRE>
	<P>
	Note that the split/4 predicate does not return any information about
	matching (parenthesised) sub-expressions!
    "),
    eg:"
    ?- split(\"cad\", \"abracadabra\", [], Parts).
    Parts = [\"abra\", \"cad\", \"abra\"]
    Yes (0.00s cpu)

    ?- split(\"bra\", \"abracadabra\", [], Parts).
    Parts = [\"a\", \"bra\", \"cada\", \"bra\", \"\"]
    Yes (0.00s cpu)

    ?- split(\"bla\", \"abracadabra\", [], Parts).
    Parts = [\"abracadabra\"]
    Yes (0.00s cpu)

    ?- split(\"a\", \"aaa\", [], Parts).
    Parts = [\"\", \"a\", \"\", \"a\", \"\", \"a\", \"\"]
    Yes (0.00s cpu)
 
    ?- split(\"%[a-z]\", \"format %s %f=%d.\", [], Parts).
    Parts = [\"format \", \"%s\", \" \", \"%f\", \"=\", \"%d\", \".\"]
    Yes (0.00s cpu)


    % With the following definition
    replace(Pattern, New, String, NewString) :-
	split(Pattern, String, [], Parts),
	(
	    fromto(Parts, [NoMatch,_Match|NMs], NMs, [Last]),
	    fromto(Repl,  [NoMatch, Subst|NSs], NSs, [Last]),
	    param(New)
	do
	    Subst = New
	),
	concat_string(Repl, NewString).

    ?- replace(\"2\", \"to\", \"2 be or not 2 be\", S).
    S = \"to be or not to be\"
    Yes (0.00s cpu)
    "
]).


:- comment(compile_pattern/3, [
    summary:"Precompile a pattern for repeated use",
    args:["Pattern":"A string",
	"Options":"List of atoms",
    	"CompiledPattern":"Output: a compiled pattern handle"],
    amode:(compile_pattern(+,+,-) is det),
    fail_if:"None",
    see_also:[library(regex),match/2,match/3,match/4,matchsub/4,matchall/4],
    desc:html("
	All matching predicates in this library accept either a regular
	expression in string form, or a precompiled regular expression.
	The matching predicates will execute faster if invoked with a
	precompiled pattern instead of the string.  Whenever a pattern
	needs to be matched more than once, it will typically be more
	efficient to work with a precompiled pattern.
	<P>
    	Options is a (possibly empty) list of atomic option names,
	as described in the library(regex) page.  The options give here
	should be the same as the ones given to the matching-predicates later.
	<P>
	The space consumed by the compiled pattern will be automatically
	reclaimed on failure, or on garbage collection when no longer needed.
    "),
    eg:"
    ?- compile_pattern(\"ab+a\", [], C),
       match(C, \"bbabbbaab\", [], M1),
       match(C, \"abacus\", [], M2).

    C = 'HANDLE'(16'00025c60)
    M1 = \"abbba\"
    M2 = \"aba\"
    Yes
    "
]).


