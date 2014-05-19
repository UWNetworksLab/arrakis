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
% Version:	$Id: spell.pl,v 1.1 2008/06/30 17:43:49 jschimpf Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% IDENTIFICATION:       spell.pl
% AUTHOR:               Stefano Novello
% PROJECT:              ECLIPSE
%
% The effect of loading this module is to modify the error handler
% called when an undefined procedure has been called. A spelling correction
% algorithm is used to see if the cause was a misspelling of an existing
% predicate.
%

:- module(spell).
:- system.

:- comment(summary, "Predicate name spelling correction").
:- comment(author, "Stefano Novello, ECRC Munich").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:49 $").
:- comment(desc, html("
    The effect of loading this library is to modify the event handler
    for calling an undefined procedure.  A spelling correction
    algorithm is used to see if the cause was a misspelling of an
    existing predicate. 
    ")).

:- import current_predicate_body/2 from sepia_kernel.
:- import error_handler/2 from sepia_kernel.

% spell_handler/3
% spell_handler(+errno,+culprit,+module)
:- mode spell_handler(+,+,+).
% The spell handler prints a list of predicates whose spelling is
% similar to the one found. If the user wants the handler to correct the
% spelling, the handler will do so and call the correct definition.
% If no correction is found the usual error handler is called.
% A more sophisticated system would detect the error handler at the time
% of calling and call that. This one has the advantage of being robust
% since error_handler/2 form sepia_kernel is guaranteed never to be abolished.

spell_handler(N,Culprit,Module) :-
	functor(Culprit,Functor,Arity),
	correct(Functor,Corr,Module),
	!,
	error_id(N,Message),
	printf(error, "%s %w in module %w\nPossibly meant: ",
		[Message, Culprit, Module]),
	( confirm(Arity,Corr,CorrFunctor) ->
		callcorrect(Culprit,CorrFunctor,Module)
	;
		error(157,_)
	).
	

spell_handler(N,Culprit,_Module) :-
	error_handler(N,Culprit).


:- set_event_handler(68,spell_handler/3).


% confirm/3
% confirm(+Arity,+ListOfCorrections,-CorrectFunctor)
:- mode confirm(+,+,-).
% This is true if
%	there is only one correction	and
%	the arity of this correction matches that of undefined predicate  and
%	the user accepts that the system apply the correction	and
%	CorrectFunctor is the functor of the only correction.
% In any case it has the side effect of writing out the list of suggested
% corrections.

confirm(Arity,[CorrFunctor/Arity],CorrFunctor) :-
	!,
	write(error,CorrFunctor/Arity),
	write(error,' Correct (y/n)? '),
	flush(error),
	tyi(Ch),
	nl(error),
	(Ch = 0'y ; Ch = 0'Y).
confirm(_Arity,Corrs,_) :-
	writeln(error,Corrs),
	nl(error),
	fail.


% callcorrect/3
% callcorrect(+CulpritTerm,+CorrectFunctor,+Module)
:- mode callcorrect(+,+,+).
% This calls the CorrectFunctor with the arguments of Culprit

callcorrect(Culprit,Functor,Module) :-
	Culprit =.. [_|Args],
	Call =.. [Functor|Args],
	call(Call)@Module.


% correct/3
% correct(+Functor,+ListOfCorrection,Module)
:- mode correct(+,-,+).
% Pre Conditions
% Functor is an atom that is not the functor of any visible predicate in Module
%
% This is true if ListOfCorrection is a non empty list of terms Functor/Arity
% that are definitions visible in Module. The list is sorted and free of
% duplicates.
% The definitions are either
%	Definitions whose functor is Functor but have different arities
%	or Whose functor is similar to Functor. (1 or 2 spelling mistakes)

correct(F , Ps, Module) :-
	setof(F/A,current_pred(F/A,Module),Ps),
	!.
correct(F , Ps,Module) :-
	setof(Pred,spell1(F,Pred,Module),Ps),
	!.
correct(F , Ps,Module) :-
	setof(Pred,spell2(F,Pred,Module),Ps).


% current_pred/2
% current_pred(-Functor/Arity,Module)
:- mode current_pred(-,+).
% True is Functor/Arity is visible in Module.
current_pred(X,Module) :- current_predicate_body(X,Module).
current_pred(X,_) :- current_built_in(X).


% spell1/4
% spell1(+IncorrectFunctor,-CorrectFunctor/Arity,+Module)
:- mode spell1(+,-,+).
% True is CorrectFunctor/Arity is a predicate visible in Module
% whose functor is similar to IncorrectFunctor except for one spelling
% mistake.

spell1(Guess,Functor/Arity,Module) :-
	atom_string(Guess,StringGuess),
	string_length(StringGuess,LenGuess),
	string_list(StringGuess,ListGuess),
	current_pred(Functor/Arity,Module),
	LDiff is atom_length(Functor) - LenGuess,
	checkldiff1(LDiff),
	convldiff(LDiff,AbsLDiff,ListGuess,ListFunctor,Short,Long),
	atom_string(Functor,StringFunctor),
	string_list(StringFunctor,ListFunctor),
	diff1(AbsLDiff,Short,Long).

% checkldiff(1,2)/1
% checkldiff(1,2)(+DifferenceinStringLengths)
:- mode checkldiff1(+).
:- mode checkldiff2(+).
% Is true if DifferenceinStringLengths could be the difference in length
% between two strings where one is different from the other only through
% (one,two) spelling mistake(s).
checkldiff1(-1).
checkldiff1(0).
checkldiff1(1).

checkldiff2(-2).
checkldiff2(-1).
checkldiff2(0).
checkldiff2(1).
checkldiff2(2).



% convldiff/6
% convldiff(+Difference,-AbsoluteofDifference,+First,+Second,-Short,-Long)
:- mode convldiff(+,-,+,+,-,-).
% Pre-conditions
% 	First and Second are 2 strings  AND
%       Difference is length(Second)-length(First)
%	Difference in the range -2 to +2
% Is true if 
%	Short is the shorter of these	AND
%	Long is the longer of these	AND
%	AbsoluteofDifference is abs(Difference)
convldiff(-2,2,X,Y,Y,X).
convldiff(-1,1,X,Y,Y,X).
convldiff(0,0,X,Y,X,Y).
convldiff(1,1,X,Y,X,Y).
convldiff(2,2,X,Y,X,Y).



% spell2/4
% spell2(+IncorrectFunctor,-CorrectFunctor/Arity,+Module)
:- mode spell2(+,-,+).
% True is CorrectFunctor/Arity is a predicate visible in Module
% whose functor is similat to IncorrectFunctor except for two spelling
% mistakes.
% IncorrectFunctor is checked to be longer than 4 characters. This is
% just to keep the number of alternatives given, small for short functors.

spell2(Guess,Functor/Arity,Module) :-
	atom_string(Guess,StringGuess),
	string_length(StringGuess,LenGuess),
	LenGuess > 4,
	string_list(StringGuess,ListGuess),
	current_pred(Functor/Arity,Module),
	LDiff is atom_length(Functor) - LenGuess,
	checkldiff2(LDiff),
	convldiff(LDiff,AbsLDiff,ListGuess,ListFunctor,Short,Long),
	atom_string(Functor,StringFunctor),
	string_list(StringFunctor,ListFunctor),
	diff2(AbsLDiff,Short,Long).


% diff1/3
% diff1(LengthDifference,ShortList,LongList)
:- mode diff1(+,+,+).
% Pre-conditions
%	LongList is a list LengthDifference elements longer than ShortList
%
% diff1 is true if by making 1 modification to ShortList it can be made
% equal to LongList.
% allowed modifications are
%	a) Change an element
%	b) Add an element
%	c) Exchange 2 elements.
% knowing the difference in length of the lists means one does not have
% to try all possible modifications.
diff1(LDiff,[A|X],[A|Y]) :-
	!,
	diff1(LDiff,X,Y).
diff1(0,[_|X],[_|X]).
diff1(0,[A,B|X],[B,A|X]).
diff1(1,X,[_|X]).


% diff2/3
% diff2(LengthDifference,ShortList,LongList)
:- mode diff2(+,+,+).
% Pre-conditions
%	LongList is a list LengthDifference elements longer than ShortList
%
% diff2 is true if by making 2 modifications to ShortList it can be made
% equal to LongList.
% allowed modifications are
%       Change an element
%       Add an element
%	Remove an element
%       Exchange 2 elements.
% By using the constraint that one can only make the spelling mistakes while
% typing from left to right some computation can be eliminated. i.e. if we
% modify the head of the list we only need consider the tail, not the whole
% modified list.
diff2(LDiff,[A|X],[A|Y]) :-
	!,
	diff2(LDiff,X,Y).
diff2(0,[_|X],[_|Y]) :-
	diff1(0,X,Y).
diff2(0,[_|X],Y) :-
	diff1(1,X,Y).
diff2(0,X,[_|Y]) :-
	diff1(1,Y,X).
diff2(0,[A,B|X],[B,A|Y]) :-
	diff1(0,X,Y).
diff2(1,X,[_|Y]) :-
	diff1(0,X,Y).
diff2(1,[_|X],[_|Y]) :-
	diff1(1,X,Y).
diff2(1,[A,B|X],[B,A|Y]) :-
	diff1(1,X,Y).
diff2(2,X,[_|Y]) :-
	diff1(1,X,Y).

end_of_file.

% THE REST OF THIS FILE CONTAINS TEST CODE AND CODE THAT TURNED OUT NOT TO
% BE USEFUL

% diff3 is like diff1 and diff2 it can spot similarities with 3 spelling
% errors. It is not tested.
diff3(LDiff,[A|X],[A|Y]) :-
	!,
	diff3(LDiff,X,Y).
diff3(0,[_|X],[_|Y]) :-
	diff2(0,X,Y).
diff3(0,[_|X],Y) :-
	diff2(1,X,Y).
diff3(0,X,[_|Y]) :-
	diff2(1,Y,X).
diff3(0,[A,B|X],[B,A|Y]) :-
	diff2(0,X,Y).

diff3(1,[_|X],[_|Y]) :-
	diff2(1,X,Y).
diff3(1,X,[_|Y]) :-
	diff2(0,Y,X).
diff3(1,[_|X],Y) :-
	diff2(2,X,Y).
diff3(1,[A,B|X],[B,A|Y]) :-
	diff2(1,X,Y).

diff3(2,X,[_|Y]) :-
	diff2(1,X,Y).
diff3(2,[_|X],[_|Y]) :-
	diff2(2,X,Y).
diff3(2,[A,B|X],[B,A|Y]) :-
	diff2(2,X,Y).
diff3(3,X,[_|Y]) :-
	diff2(2,X,Y).


