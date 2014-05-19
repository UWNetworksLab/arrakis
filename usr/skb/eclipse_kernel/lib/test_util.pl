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
% Version:	$Id: test_util.pl,v 1.1 2008/06/30 17:43:50 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(test_util).

:- lib(calendar).

:- export op(1110,xfx,(should_give)).
:- export op(1110,xf,(should_fail)).
:- export op(1110,xfx,(should_throw)).
:- export op(1110,xfx,(should_raise)).

:- export test/2, test/1, test_info/2.
:- export (should_give)/2, (should_fail)/1,
	(should_throw)/2, (should_raise)/2,
	(should_give)/3, (should_fail)/2,
	(should_throw)/3, (should_raise)/3,
	get_failed_test_count/1.

:- export make_integer/1, make_float/1, make_rational/1, make_bignum/1,
          make_interval/1, make_neginteger/1, make_negfloat/1,
          make_negrational/1, make_negbignum/1, make_neginterval/1,
          make_atom/1, make_nil/1, make_string/1, make_struct/1,
          make_list/1, make_var/1.


:- tool((should_give)/2,  should_give_body/3).
:- tool((should_fail)/1,  should_fail_body/2).
:- tool((should_throw)/2, should_throw_body/3).
:- tool((should_raise)/2, should_raise_body/3).

:- tool((should_give)/3,  should_give_body/4).
:- tool((should_fail)/2,  should_fail_body/3).
:- tool((should_throw)/3, should_throw_body/4).
:- tool((should_raise)/3, should_raise_body/4).

:- tool(test_info/2, test_info_body/3).
:- tool(test/2, test_body/3).
:- tool(test/1, test_body/2).

%% Number of tests
:- local variable(test_count).
:- setval(test_count, 0).
%% Number of failed tests
:- local variable(failed_test_count).
:- setval(failed_test_count, 0).
%% Line number of current test
:- local variable(test_line).
:- setval(test_line, 0).

:- set_stream(testlog, output).
:- set_stream(test_csv_log, null).

:- comment(summary, "Utilities for automated program tests").
:- comment(author, "Joachim Schimpf, IC-Parc").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:50 $").
:- comment(desc, html("
    Use this library as follows: Write a file with test patterns, using
    the primitives should_fail/1, should_give/2, should_throw/2 and
    should_raise/2, e.g.
    <PRE>
    3.0 > 3 should_fail.
    X is 3.0+4 should_give X=7.0.
    exit_block(ball) should_throw ball.
    number_string(hello,_) should_raise 5.
    </PRE>
    The file name should have a .tst extension, e.g. mytests.tst.
    Then run all the test in the file by calling test(mytests).  This will
    print a message for every test pattern that does not behave as expected.
    The messages go to a stream called testlog (which defaults to output).

    <P>
    Alternatively, you can write a file with test patterns, using the
    primitives should_fail/2, should_give/3, should_throw/3 and
    should_raise/3, e.g.
    <PRE>
    should_fail(3.0 > 3, test_float_not_greater_than_integer).
    should_give(X is 3.0+4, X=7.0, test_float_plus_integer_equals_float).
    should_throw(exit_block(ball),ball,test_exit_block).
    should_raise(number_string(hello,_),5,test_raises_5).
    </PRE>
    Here the extra last argument serves as a name for the test (or a short
    description).  It can be an atom or a string and it is used to output
    results in comma separated format to a stream called test_csv_log
    (defaults to null), e.g. test(mytest) should output the following to
    test_csv_log:
    <PRE>
    test_float_not_greater_than_integer,pass,2001-10-29,16:59:20,0.00
    test_float_plus_integer_equals_float,pass,2001-10-29,16:59:20,0.01
    test_exit_block,pass,2001-10-29,16:59:20,0.00
    test_raises_5,pass,2001-10-29,16:59:20,0.00
    </PRE>
    The first value is the name of the test (last argument in test pattern).
    The second value is either `pass' or `fail' indicating whether the
    particular test was successful or not.  The third and fourth values show
    the date and time (UTC) the test was run (in ISO 8601 format).  The last
    value shows the CPU time taken to run the test.
    Extra values can be appended at the head of the comma separated values
    by using test_info/2, e.g. test_info(mytest,test_result) would change
    the output to test_csv_log as follows:
    <PRE>
    test_result,test_float_not_greater_than_integer,pass,2001-10-29,16:59:20,0.00
    test_result,test_float_plus_integer_equals_float,pass,2001-10-29,16:59:20,0.01
    test_result,test_exit_block,pass,2001-10-29,16:59:20,0.00
    test_result,test_raises_5,pass,2001-10-29,16:59:20,0.00
    </PRE>
    This can be extremely useful, as useful information as the name of the
    module tested, the directory where it is located, the name of the host,
    etc. can be added to the log. 
    ")).

:- comment((should_fail)/1, [
    template:"+Goal should_fail",
    summary:"Run the goal Goal and print a message if it doesn't fail",
    eg:"3.0 > 3 should_fail.",
    see_also:[(should_give)/2, (should_throw)/2, (should_raise)/2, (should_fail)/2, 
              (should_give)/3, (should_throw)/3, (should_raise)/3]
    ]).
:- comment((should_fail)/2, [
    template:"should_fail(+Goal,+TestName)",
    summary:"Run the goal Goal and print a message if it doesn't fail",
    eg:"should_fail(3.0 > 3, test_float_not_greater_than_integer).",
    see_also:[(should_give)/2, (should_throw)/2, (should_raise)/2, (should_fail)/1, 
              (should_give)/3, (should_throw)/3, (should_raise)/3]
    ]).
 :- comment((should_give)/2, [
    template:"+Goal should_give +CheckGoal",
    summary:"Run the goal Goal and print a message if the result doesn't satisfy CheckGoal",
    eg:"X is 3.0+4 should_give X=7.0.",
    see_also:[(should_fail)/1, (should_throw)/2, (should_raise)/2, (should_give)/3,
	      (should_fail)/2, (should_throw)/3, (should_raise)/3]
    ]).
:- comment((should_give)/3, [
    template:"should_give(+Goal,+CheckGoal,+TestName)",
    summary:"Run the goal Goal and print a message if the result doesn't satisfy CheckGoal",
    eg:"should_give(X is 3.0+4, X=7.0, test_float_plus_integer_equals_float).",
    see_also:[(should_fail)/1, (should_throw)/2, (should_raise)/2, (should_give)/2,
	      (should_fail)/2, (should_throw)/3, (should_raise)/3]
    ]).

:- comment((should_throw)/2, [
    template:"+Goal should_throw +Exception",
    summary:"Run the goal Goal and print a message if it doesn't throw Exception",
    eg:"exit_block(ball) should_throw ball.",
    see_also:[(should_give)/2, (should_fail)/1, (should_raise)/2, (should_throw)/3,
	      (should_give)/3, (should_fail)/2, (should_raise)/3]
    ]).
:- comment((should_throw)/3, [
    template:"should_throw(+Goal,+Exception,+TestName)",
    summary:"Run the goal Goal and print a message if it doesn't throw Exception",
    eg:"    should_throw(exit_block(ball),ball,test_exit_block).",
    see_also:[(should_give)/2, (should_fail)/1, (should_raise)/2, (should_throw)/2,
	      (should_give)/3, (should_fail)/2, (should_raise)/3]
    ]).
:- comment((should_raise)/2, [
    template:"+Goal should_raise +Event",
    summary:"Run the goal Goal and print a message if it doesn't raise Event.",
    eg:"number_string(hello,_) should_raise 5.  % type error",
    see_also:[(should_give)/2, (should_fail)/1, (should_throw)/2, (should_raise/3),
	      (should_give)/3, (should_fail)/2, (should_throw)/3]
    ]).
:- comment((should_raise)/3, [
    template:"should_raise(+Goal,+Event,+TestName)",
    summary:"Run the goal Goal and print a message if it doesn't raise Event.",
    eg:"should_raise(number_string(hello,_),5,test_raises_5). % type error",
    see_also:[(should_give)/2, (should_fail)/1, (should_throw)/2, (should_raise/2),
	      (should_give)/3, (should_fail)/2, (should_throw)/3]
    ]).

:- comment((get_failed_test_count)/1, [
    template:"get_failed_test_count(-N)",
    summary:"Returns the number of tests that failed.",
    desc: html("
<P>
    The test framework counts the number of tests which fail; use this
    predicate to retrieve this number.
    ")
]).

:- comment((test)/1, [
    template:"test(+File)",
    summary:"Runs all the test patterns in File.",
    see_also:[test/2, test_info/2,
	      (should_give)/2, (should_fail)/1, (should_throw)/2, (should_raise)/2,
	      (should_give)/3, (should_fail)/2, (should_throw)/3, (should_raise)/3]
    ]).

:- comment((test)/2, [
    template:"test(+File, +Option)",
    summary:"Runs all the test patterns in File.",
    desc:html("\
    Runs all the test patterns in File. Option is either 'call' (the default)
    or 'compile'.
    When 'call' is chosen, every test goal gets executed simply by metacall
    using call/1.
    When 'compile' is chosen, every test goal gets compiled into an auxiliary
    predicate (with all compile-time transformations applied), which in turn
    is then executed.
    "),
    see_also:[test_info/2,
	      (should_give)/2, (should_fail)/1, (should_throw)/2, (should_raise)/2,
	      (should_give)/3, (should_fail)/2, (should_throw)/3, (should_raise)/3]
    ]).

:- comment((test_info)/2, [
    template:"test_info(+File,+Info)",
    summary:"Runs all the test patterns in File, printing the Info string in test_csv_log.",
    see_also:[test/1,
	      (should_give)/2, (should_fail)/1, (should_throw)/2, (should_raise)/2,
	      (should_give)/3, (should_fail)/2, (should_throw)/3, (should_raise)/3]
    ]).


get_failed_test_count(FailedTestsCount) :-
	getval(failed_test_count, FailedTestsCount).

test_body(File, Module) :-
	test_body(File, call, "", Module).


test_info_body(File, Info, Module) :-
	test_body(File, call, Info, Module).

test_body(File, Type, Module) :-
	test_body(File, Type, "", Module).
	
test_body(File, Type, Info, Module) :-
	existing_file(File, ["",".tst"], [readable], File1), !,
	open(File1, read, S),
	printf(testlog, "%nRunning tests from file (using %w) %w...%n%b", [Type,File1]),
        setval(test_count, 0),
	setval(failed_test_count, 0),
	test_stream(S, Type, Info, Module),
	close(S).
test_body(File, _Type, _Info, _Module) :-
	printf(testlog, "%nTest file not found: %w%n%b", [File]).

test_stream(S, Type, Info, M) :-
	repeat, 
	   get_stream_info(S, line, Line),
	   setval(test_line, Line),
	   read(S, Test)@M,
	   ( Test == end_of_file ->
	       !,  
	       getval(test_count, N),
	       getval(failed_test_count, FN),
	       printf(testlog, "%n%d tests done.%n%d tests failed.%n%b", [N,FN])
	   ;
	       ( Info == "" ->
		     true
	       ;
		     printf(test_csv_log, "%q,", [Info])
	       ),
	       cputime(Start),

	       do_test(Type, Test, M),

	       cputime(End),
	       CPUTime is End - Start,
	       get_date_and_time_strings(DateString, TimeString),
	       printf(test_csv_log, "%w,%w,%f%n", [DateString, TimeString, CPUTime]),
	       incval(test_count),
	       fail
	   ).

printf_with_line(OutStream, Message, Params) :-
	getval(test_line, Line),
	( Line > 0 ->
	    concat_strings("====== Line %d: ", Message, Format),
	    printf(OutStream, Format, [Line|Params])
    	;
	    concat_strings("====== ", Message, Format),
	    printf(OutStream, Format, Params)
	).

get_date_and_time_strings(DateString, TimeString) :-
	get_date_and_time(Date, Time),
	date_to_string(Date, DateString),
	time_to_string(Time, TimeString).

date_to_string(Year-Month-Day, DateString) :-
	open(string(""), write, DateStream),
	printf(DateStream, "%04d-%02d-%02d", [Year, Month, Day]),
	get_stream_info(DateStream, name, DateString),
	close(DateStream).

time_to_string(Hour:Minute:Second, TimeString) :-
	open(string(""), write, TimeStream),
	printf(TimeStream, "%02d:%02d:%02.0f", [Hour, Minute, Second]),
	get_stream_info(TimeStream, name, TimeString),
	close(TimeStream).

get_date_and_time(Date, Time) :-
	mjd_now(MJD),
	mjd_to_ymd(MJD, Date),
	mjd_to_time(MJD,Time).


do_test(call, Test, M) ?-
	once(Test)@M.
do_test(compile, Test, M) ?-
	compile_test_call(Test, M).

compile_test_call(Test, M) :-
	extract_test_goal(Test, Goal0, Head, NewTest),
	block(expand_goal(Goal0, Goal)@M,
	   _, Goal0 = Goal % ignore problem until compilation
	),
	term_variables(Goal, Vars),
	Head =.. ['$test__',_|Vars],
        % catch and ignore `illegal goal' error if should_raise
	get_event_handler(131, HH, HM),
	set_event_handler(131, expected_handler/0),
	block(
          (compile_term((Head:-Goal))@M ->
	         arg(1, Head, Test),
	         once(NewTest)@M,
	         functor(Head, F,A),
	         abolish(F/A)@M
	   ;
		 incval(failed_test_count),
                 printf_with_line(testlog, "Compilation of goal failed unexpectedly:%n%q%n%n%b", [Test])
          ), Tag,
	  ((Tag == ignore, nonvar(Test), Test = should_raise(_,_)) ->
	         true
	    ;
		 incval(failed_test_count),
                 printf_with_line(testlog, "Compilation of goal aborted unexpectedly:%n%q%n%n%b", [Test])
          )
        ),
	set_event_handler(131, HH)@HM.

% extract_test_goal(+Test, -Goal, -TemplateGoal, -TemplateTest)
%     extract the test goal Goal from the test pattern Test, and
%     create a new template for the test that will be used to call
%     the compiled test, the call itself, TemplateGoal, should be
%     filled in before calling TemplateTest
extract_test_goal(should_fail(Goal), G0, G1, NewTest) ?- !,
	G0 = Goal,
	NewTest = should_fail(G1).
extract_test_goal(should_fail(Goal,_Name), G0, G1, NewTest) ?- !,
        G0 = Goal,
	NewTest = should_fail(G1).

extract_test_goal(should_give(Goal,Check), G0, G1, NewTest) ?- !,
	G0 = Goal,
	NewTest = should_give(G1,Check).
extract_test_goal(should_give(Goal,Check,_Name), G0, G1, NewTest) ?- !,
	G0 = Goal,
	NewTest = should_give(G1,Check).

extract_test_goal(should_raise(Goal,ErrorId), G0, G1, NewTest) ?- !,
	G0 = Goal,
	NewTest = should_raise(G1,ErrorId).
extract_test_goal(should_raise(Goal,ErrorId,_Name), G0, G1, NewTest) ?- !,
	G0 = Goal,
	NewTest = should_raise(G1,ErrorId).

extract_test_goal(should_throw(Goal,Expected), G0, G1, NewTest) ?- !,
	G0 = Goal,
	NewTest = should_throw(G1,Expected).
extract_test_goal(should_throw(Goal,Expected,_Name), G0, G1, NewTest) ?- !,
	G0 = Goal,
	NewTest = should_throw(G1,Expected).

extract_test_goal(Goal, Goal, G1, G1).

should_fail_body(Goal, Module) :-
	block(should_fail1(Goal, Module), _, true).

should_fail_body(Goal, Name, Module) :-
	printf(test_csv_log, "%q,", [Name]),
	block(should_fail1(Goal, Module), _, true).

    should_fail1(Goal, Module) :-
	( block(call(Goal)@Module, Tag, unexpected_exit(Goal,Tag)) ->
	    unexpected_success(Goal)
	;
	    write(test_csv_log, "pass,")
	).


should_give_body(Goal, Check, Module) :-
	block(should_give1(Goal, Check, Module), _, true).

should_give_body(Goal, Check, Name, Module) :-
	printf(test_csv_log, "%q,", [Name]),
	block(should_give1(Goal, Check, Module), _, true).

    should_give1(Goal, Check, Module) :-
	( block(call(Goal)@Module, Tag, unexpected_exit(Goal,Tag)) ->
	    ( block(call(Check)@Module, Tag, unexpected_exit(Check,Tag)) ->
		garbage_collect,  % try to provoke any gc bugs
		write(test_csv_log, "pass,")
	    ;
		write(test_csv_log, "fail,"),
		incval(failed_test_count),
		printf_with_line(testlog, "goal gave unexpected result:%n%q%n",[Goal]),
		printf(testlog, "------ did not satisfy:%n%q%n%n%b", [Check])
	    )
	;
	    unexpected_failure(Goal)
	).

should_throw_body(Goal, Expected, Module) :-
	block(should_throw1(Goal, Expected, Module), _, true).

should_throw_body(Goal, Expected, Name, Module) :-
	printf(test_csv_log, "%q,", [Name]),
	block(should_throw1(Goal, Expected, Module), _, true).

    should_throw1(Goal, Expected, Module) :-
	( block(call(Goal)@Module, Tag, expected_exit(Goal,Expected,Tag)) ->
	    unexpected_success(Goal)
	;
	    unexpected_failure(Goal)
	).

should_raise_body(Goal, ErrorId, Module) :-
	block(should_raise1(Goal, ErrorId, Module), _, true).

should_raise_body(Goal, ErrorId, Name, Module) :-
	printf(test_csv_log, "%q,", [Name]),
	block(should_raise1(Goal, ErrorId, Module), _, true).

    should_raise1(Goal, ErrorId, Module) :-
	get_event_handler(ErrorId, H, M),
	set_event_handler(ErrorId, expected_handler/0),
	( block(call(Goal)@Module, Tag,
	    (set_event_handler(ErrorId, H)@M, expected_exit(Goal,ignore,Tag)))
	->
	    unexpected_success(Goal)
	;
	    unexpected_failure(Goal)
	),
	set_event_handler(ErrorId, H)@M.


    expected_handler :-
	exit_block(ignore).

    unexpected_success(Goal) :-
	%% TEST has failed
	write(test_csv_log, "fail,"),
	incval(failed_test_count),
	printf_with_line(testlog, "goal succeeded unexpectedly:%n%q%n%n%b", [Goal]).

    unexpected_failure(Goal) :-
	%% TEST has failed
	write(test_csv_log, "fail,"),
	incval(failed_test_count),
	printf_with_line(testlog, "goal failed unexpectedly:%n%q%n%n%b", [Goal]).

    unexpected_exit(Goal, Tag) :-
	%% TEST has failed
	write(test_csv_log, "fail,"),
	incval(failed_test_count),
	printf_with_line(testlog, "goal unexpectedly did exit_block(%w):%n%q%n%n%b", [Tag,Goal]),
	exit_block(ignore).

    expected_exit(Goal,Expected, Actual) :-
	( Expected==Actual ->
	    %% Test succeeded
	    write(test_csv_log, "pass,")
	;
	    %% TEST has failed
	    write(test_csv_log, "fail,"),
	    incval(failed_test_count),
	    printf_with_line(testlog, "goal unexpectedly did exit_block(%w):%n%q%n%n%b", [Actual,Goal])
	),
	exit_block(ignore).


%%%%%%%%%%%%%%%%%%%
%
% type generation predicates -- avoid problems with smart compilers
%

make_integer(1).
make_float(1.1).
make_interval(1.0__1.1).
make_neginteger(-1).
make_negfloat(-1.1).
make_neginterval(-1.1__-1.0).
make_atom(atom).
make_nil([]).
make_string("string").
make_struct(f(1,2,3)).
make_list([1,2,3]).
make_var(_).

% The following hack is to make this file compile even when we
% don't support bignum/rationals.  Fail at runtime instead.

make_rational(X) :- number_string(X, "1_1").
make_bignum(X) :- number_string(X, "111111111111111111111111111111111111111111111111111").
make_negrational(X) :- number_string(X, "-1_1").
make_negbignum(X) :- number_string(X, "-111111111111111111111111111111111111111111111111111").

