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
% Copyright (C) 2002-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc, Imperial College
% Version:	$Id: module_options.ecl,v 1.1 2008/06/30 17:43:47 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(module_options).

:- comment(summary, "Utility library to manage options within a library module").
:- comment(author,"Joachim Schimpf").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date,"$Date: 2008/06/30 17:43:47 $").
:- comment(desc, html("<P>
	This library provides utilities to manage option settings on behalf
	of other library modules. The basic idea is that each client module
	can define what it considers to be valid option names and values, plus
	the structure in terms of which the set of all options will be stored
	and returned.
</P><P>
	For each client library, global default option settings are maintained
	which can be modified (in a non-backtrackable fashion) using
	set_default_option/2. Whenever the settings are retrieved, the
	defaults can be individually overridden using a user-supplied option
	list.
</P><P>
	Every client module has to define 3 local predicates:
<DL>
	<DT>valid_option_field(?Name, -FieldIndex)<DD>
	    defines the option names (atoms) and the position (integer)
	    of the option value within an option structure.
	<DT>valid_option_value(+Name, +Value)<DD>
	    defines what constitues a valid option value, by
	    provides a type/range check for Value. The predicate should
	    fail if Value is not a valid value for option Name.
	<DT>default_options(-OptionStructure)<DD>
	    this should be a single fact which should return a structure.
	    The structure arguments define the initial default settings
	    for each option field. The structure's functor defines the
	    skeleton in terms of which option settings will be returned
	    by get_options/2.
	    Note that the structure can have extra fields which are
	    not defined as valid options, are not user-modifiable, and
	    will therefore always be returned unchanged by get_options/2.
</DL>
	A typical application would in addition define a toplevel predicate
	that accepts a user-supplied option list per invocation, and possibly
	a predicate to modify the global default settings.  Sample usage:
<PRE>
	:- module(my_module).

	:- lib(module_options).

	valid_option_field(a, 1).
	valid_option_field(b, 2).
	valid_option_field(c, 3).

	valid_option_value(a, Value) :- integer(Value).
	valid_option_value(b, Value) :- atom(Value).
	valid_option_value(c, Value) :- atom(Value).

	default_options(options(23,hello,world,there)).

	:- export my_set_default_option/2.
	my_set_default_option(Name, Value) :-
	    set_default_option(Name, Value).

	:- export my_predicate/2.
	my_predicate(Arguments, OptionList) :-
	    ( get_options(OptionList, OptionStruct) ->
		...
	    ;
		printf(error, \"Invalid option list: %w%n\", [OptionList]),
		print_default_options(error),
		abort
	    ).
</PRE>
	In practice, it is recommended to use structure notation for the
	option structure for better readability and maintainability, i.e.
<PRE>
	:- module(my_module).

	:- lib(module_options).

	:- local struct(options(a,b,c,d)).

	valid_option_field(a, a of options).
	valid_option_field(b, b of options).
	valid_option_field(c, c of options).

	valid_option_value(a, Value) :- integer(Value).
	valid_option_value(b, Value) :- atom(Value).
	valid_option_value(c, Value) :- atom(Value).

	default_options(options{a:23,b:hello,c:world,d:there}).

	:- export my_set_default_option/2.
	my_set_default_option(Name, Value) :-
	    set_default_option(Name, Value).

	:- export my_predicate/2.
	my_predicate(Arguments, OptionList) :-
	    ( get_options(OptionList, OptionStruct) ->
		...
	    ;
		printf(error, \"Invalid option list: %w%n\", [OptionList]),
		print_default_options(error),
		abort
	    ).
</PRE>
	It is not absulotely necessary to define a predicate like
	my_set_default_option/2 since <CODE>my_set_default_option(opt,val)</CODE>
	is equivalent to <CODE>set_default_option(opt,val)@my_module</CODE>.
</P>
")).


:- export get_options/2.
:- comment(get_options/2, [
    summary:"Create a structure OptionStruct from OptionList and the context module's default settings",
    args:["OptionList":"List of Name:Value pairs",
    	"OptionStruct":"Variable, will be bound to a structure"],
    amode:get_options(+,-),
    fail_if:"OptionList contains illegal names or values",
    see_also:[library(module_options),set_default_option/2, print_default_options/1],
    desc:html("<P>
    The exact behaviour of this predicate depends on the module from where
    it is invoked: It returns a structure OptionStruct, whose skeleton and
    default field values are defined by the predicate default_options/1 in
    the context module. Moreover, OptionList is analysed, and any option
    values specified there will show up in the corresponding fields of the
    returned structure OptionStruct, instead of the default values defined
    by default_options/1.
</P><P>
    The option names in OptionList must be valid according to the predicate
    valid_option_field/2 in the context module, and the corresponding values
    must be valid according to valid_option_value/2.
</P>
    ")
]).
:- tool(get_options/2, get_options/3).
get_options(OptionList, Options, Module) :-
	get_default_options(DefaultOptions, Module),
	functor(DefaultOptions, F, N),
	functor(Options, F, N),
	check_and_get_options_from_list(OptionList, Options, Module),
	(
	    for(I,1,N),
	    param(DefaultOptions,Options)
	do
	    arg(I, Options, Value),
	    ( var(Value) ->
	    	arg(I, DefaultOptions, Value)	% fill in default
	    ;
	    	true
	    )
	).

    get_default_options(DefaultOptions, Module) :-
	( current_array(Module, _) ->
	    getval(Module, DefaultOptions)
	;
	    once(default_options(DefaultOptions))@Module,
	    local(variable(Module, DefaultOptions))
	).


    check_and_get_options_from_list([], _Options, _Module) ?- true.
    check_and_get_options_from_list([Name:V|Xs], Options, Module) ?-
	atom(Name), nonvar(V),
	once(valid_option_field(Name, Field))@Module,
	once(valid_option_value(Name, V))@Module,
	arg(Field, Options, V),
	check_and_get_options_from_list(Xs, Options, Module).


:- export set_default_option/2.
:- comment(set_default_option/2, [
    summary:"Permanently set the default value for the given option in the context module",
    args:["OptionName":"An atom",
    	"OptionValue":"A valid option term"],
    amode:set_default_option(+,+),
    exceptions:[4:"OptionName or OptionValue are uninstantiated",
    	5:"OptionName is not an atom",
	6:"OptionName is not a valid option, or OptionValue is not a valid value in the context module"],
    see_also:[library(module_options),get_options/2, print_default_options/1],
    desc:html("<P>
    The exact behaviour of this predicate depends on the module from where
    it is invoked: It changes the default value for a named option in the
    context module. The initial default values are defined by the predicate
    default_options/1 in the context module.
</P><P>
    The option name OptionName must be valid according to the predicate
    valid_option_field/2 in the context module, and OptionValue
    must be valid according to valid_option_value/2.
</P>
    ")
]).
:- tool(set_default_option/2, set_default_option/3).
set_default_option(Option, Value, Module) :-
	( nonvar(Option), nonvar(Value) ->
	    ( atom(Option) ->
	    	(
		    once(valid_option_field(Option, Field))@Module,
		    once(valid_option_value(Option, Value))@Module,
		    get_default_options(DefaultOptions, Module),
		    setarg(Field, DefaultOptions, Value),
		    setval(Module, DefaultOptions)
		->
		    setval(default_options, DefaultOptions)@Module
		;
		    print_default_options(error, Module),
		    error(6, set_default_option(Option, Value), Module)
		)
	    ;
		error(5, set_default_option(Option, Value), Module)
	    )
	;
	    error(4, set_default_option(Option, Value), Module)
	).


:- export print_default_options/1.
:- comment(print_default_options/1, [
    summary:"Print the valid options and their current default values to Stream",
    args:["Stream":"An output stream"],
    amode:print_default_options(+),
    see_also:[library(module_options),get_options/2, set_default_option/2],
    desc:html("<P>
    The exact behaviour of this predicate depends on the module from where
    it is invoked: It prints a list of the valid options and their current
    default values onto the stream Stream.
</P>
    ")
]).
:- tool(print_default_options/1,print_default_options/2).
print_default_options(Stream, Module) :-
	printf(Stream, "Valid options for module %w:%n", [Module]),
	get_default_options(DefaultOptions, Module),
	call(valid_option_field(Name, Field))@Module,
	arg(Field, DefaultOptions, Value),
	printf(Stream, "   %-24s (default: %Qw)%n", [Name,Value]),
	fail.
print_default_options(_, _).

