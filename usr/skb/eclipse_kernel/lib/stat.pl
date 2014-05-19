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
% Version:	$Id: stat.pl,v 1.1 2008/06/30 17:43:49 jschimpf Exp $
% ----------------------------------------------------------------------

%
% This file contains some predicates to support the production
% of virtual machine statistics with SEPIA.
% If SEPIA is compiled with the PRINTAM option, virtual instruction
% counting can be switched on and off using
%
%	vm_statistics(on).		and
%	vm_statistics(off).
%
% All virtual machine instructions that are executed beetween these
% two calls are counted in the global array vm_inst_ctr.
% By default the counting is switched off and the counters are zero.
% The size of the array corresponds to the number of VM instructions.
% There is another global array vm_inst_name that holds the names of
% the corresponding VM instructions (strings).
%
% Predicates defined here:
%
% reset_counters.
%	resets the whole counter array to zero.
%
% write_counters(File).
%	outputs the counter values to a file
%
% read_counters(File).
%	initializes the counter values from a file
%
% table.
%	pretty-prints the instruction statistics.
%	the list is sorted according to the counter value.
%	instructions that were never executed are not printed.
%
% table(File).
%	the same but the output goes to a file.
%
%
% No longer supported (because emulator support removed):
%
% pair_table(Inst1, Inst2, Number).
%	pretty-prints the instruction pair statistics.
%	the list is sorted according to the counter value.
%	instructions that were never executed are not printed.
%
% pair_table(File, Inst1, Inst2, Number).
%	the same but the output goes to a file.
%

:- module(stat).

:- export
	reset_counters/0,
	read_counters/1,
	write_counters/1,
%	pair_table/3,
%	pair_table/4,
	table/0,
	table/1.

:- reexport
	vm_statistics/1
   from sepia_kernel.

reset_counters :-
	sepia_kernel:vm_statistics(off), 
	current_array(vm_inst_ctr(L1), _),
	reset_counters(L1).
%	current_array(vm_pairs_ctr(L2), _),
%	reset_pair_counters(L2),

reset_counters(L) :-
	L > 0, !,
	L1 is L - 1,
	setval(vm_inst_ctr(L1), 0),
	reset_counters(L1).
reset_counters(_).

/*
reset_pair_counters(L) :-
	L > 0, !,
	L1 is L - 1,
	setval(vm_pairs_ctr(L1), 0),
	reset_pair_counters(L1).
reset_pair_counters(_).
*/


write_counters(File) :-
	open(File,write,Str),
	current_array(vm_inst_ctr(L), _),
	write_counters(Str, 0, L),
	close(Str).

write_counters(Str, This, Last) :-
	This < Last, !,
	getval(vm_inst_ctr(This), Val),
	write(Str, Val),
	write(Str, '. '),
	Next is This + 1,
	write_counters(Str, Next, Last).
write_counters(_, _, _).


read_counters(File) :-
	open(File,read,Str),
	current_array(vm_inst_ctr(L), _),
	read_counters(Str, 0, L),
	close(Str).

read_counters(Str, This, Last) :-
	This < Last, !,
	read(Str, Val),
	setval(vm_inst_ctr(This), Val),
	Next is This + 1,
	read_counters(Str, Next, Last).
read_counters(_, _, _).


table(File) :-
	sepia_kernel:vm_statistics(off), 
	open(File,write,Str),
	get_stream(output,Oldstream),
	set_stream(output,Str),
	table,
	set_stream(output,Oldstream),
	close(Str).

table :- 
	sepia_kernel:vm_statistics(off), 
	current_array(vm_inst_name(Ln), _),
	count(0, Ln, 0, Total, [], List),
	sort(2, >=, List, Sorted),
	nl,
	writeln('SEPIA abstract machine instruction statistics'),
	nl,
	writeln(
	'Order   Element Name                            Ctr  % of Total  Cum %'
	),
	writeln(
	'----------------------------------------------------------------------'
	), 
	do(0, Sorted, Total, 0),
	writeln('                                               ----'),
	write('TOTAL                                          '),
	writeln(Total), 
	writeln('                                               ----').


count(Max, Max, Total, Total, List, List) :- !.

count(El, Max, Total, Newtotal, List1, Newlist1) :-
	getval(vm_inst_ctr(El), Val),
	Tot is Total + Val,
	Nextel is El + 1,
	count(Nextel, Max, Tot, Newtotal, [[El, Val] | List1], Newlist1).  

do(_, [], _, _) :- !.

do(_, [[_ | [0] ] | _], _, _) :- !.

do(This, [[El | [Val] ] | T], Total, Cum) :-
	Val > 0,
	Order is This + 1,
	write(Order),
	write('\t'),
	write(El),
	write('\t'),
	getval(vm_inst_name(El), Name),
	write(Name),
	L is 34 - string_length(Name) - fix(ln(Val)/ln(10)),
	spaces(L), 
	write(Val), 
	write('  '),
	Perc is Val/Total,
	Percentage is fix(Perc * 10000 + 0.499) / 100,
	(Percentage >= 10.0 -> true; write(' ')),
	write(Percentage),
	write(' %	'), 
	NewCum is Cum + Perc,
	CumPerc is fix(NewCum * 1000 + 0.499)/10,
	write(CumPerc),
	write(' %'),
	nl,
	do(Order, T, Total, NewCum).


/*
pair_table(File, Inst1, Inst2, Number) :-
	sepia_kernel:vm_statistics(off), 
	open(File,write,Str),
	get_stream(output,Oldstream),
	set_stream(output,Str),
	pair_table(Inst1, Inst2, Number),
	set_stream(output,Oldstream),
	close(Str).

pair_table(Inst1, Inst2, Number) :-
	sepia_kernel:vm_statistics(off), 
	current_array(vm_inst_name(Ln), _),
	(
		(
			atomic(Inst1),
			atomic(Inst2),
			spaces_to_name(Inst1, Name1),
			spaces_to_name(Inst2, Name2),
			one_pair(Name1, Name2)
		)
		;
		(
			(
				(
					var(Inst1),
					var(Inst2),
					atomic(Number),
					current_array(vm_pairs_ctr(Lp), _),
					all_pairs(Number, List, Total, Lp, Ln)
				)
				;
				(
					Bound is Ln -1,
					(
						(
							atomic(Inst1),
							spaces_to_name(Inst1, Name1),
							name_to_el(Name1, Num1, Bound),
							First is Num1 * Ln,
							Last is First + Ln -1,
							(
								atomic(Number)
								;
								Number = Ln
							)
						)
						;
						(
							atomic(Inst2),
							spaces_to_name(Inst2, Name2),
							name_to_el(Name2, First, Bound),
							Increment = Ln,
							Last is First + Ln * Ln - Ln,
							(
								atomic(Number)
								;
								Number = Ln
							)
						)
						;
						(
							true
						)
					),
					(
						atomic(First)
						;
						First = 0
					),
					(
						atomic(Increment)
						;
						Increment = 1
					),
					(
						atomic(Last)
						;
						current_array(vm_pairs_ctr(Last), _)
					),
					(
						atomic(Number)
						;
						Number = Last
					),
					pair_count(First, Last, Increment, 0, Total, [], List)
				)
			),
			sort(1, >=, List, Sorted),
			nl,
			writeln('SEPIA abstract machine instruction pair statistics'),
			nl,
			writeln(
			'Order    Name1                    Name2                           Ctr  Relative'
			),
			writeln(
			'-------------------------------------------------------------------------------'
			), 
			pair_do(0, Sorted, Total, Number),
			writeln('                                                                 ----'),
			write('TOTAL                                                            '),
			writeln(Total), 
			writeln('                                                                 ----')
		)
	).
pair_table(_, _, _) :- !.

pair_count(Max, Max, _Increment, Total, Total, List, List) :- !.

pair_count(El, Max, Increment, Total, Newtotal, List1, Newlist1) :-
	current_array(vm_inst_name(Ln), _),
	getval(vm_pairs_ctr(El), Val),
	Tot is Total + Val,
	Nextel is El + Increment,
	El1 is El // Ln,
	El2 is El mod Ln,
	pair_count(Nextel, Max, Increment, Tot, Newtotal, [[Val, El1, El2] | List1], Newlist1).  


pair_do(_, _, _, 0) :- !.

pair_do(_, [], _, _) :- !.

pair_do(_, [[0, _, _] | _], _, _) :- !.

pair_do(This, [[Val, El1, El2] | T], Total, Maxnum) :-
	Val > 0,
	Order is This + 1,
	write(Order),
	write('\t'),
	getval(vm_inst_name(El1), Name1),
	write(Name1),
	L1 is 26 - string_length(Name1),
	spaces(L1), 
	getval(vm_inst_name(El2), Name2),
	write(Name2),
	L2 is 34 - string_length(Name2) - fix(ln(Val)/ln(10)),
	spaces(L2), 
	write(Val), 
	write('  '),
	Perc is Val/Total,
	Percentage is fix(Perc * 10000 + 0.499) / 100,
	(Percentage >= 10.0 -> true; write(' ')),
	write(Percentage),
	write(' %'), 
	nl,
	!,
	Order =< Maxnum,
	pair_do(Order, T, Total, Maxnum).

pair_do(_, _, _, _) :- !.


one_pair(Inst1, Inst2) :-
	current_array(vm_pairs_ctr(Lp), _),
	current_array(vm_inst_name(Ln), _),
	total_pairs(0, Ln, 0, Total),
	Bound is Ln -1,
	name_to_el(Inst1, El1, Bound),
	name_to_el(Inst2, El2, Bound),
	El is El1 * Ln + El2,
	getval(vm_pairs_ctr(El), Val),
	nl,
	writeln('SEPIA abstract machine instruction pair counter'),
	nl,
	writeln(
	'Name1                     Name2                    Ctr   Relative'
	),
	write(Inst1),
	write(Inst2),
	write(Val), 
	write('  '),
	Perc is Val/Total,
	Percentage is fix(Perc * 10000 + 0.499) / 100,
	(Percentage >= 10.0 -> true; write(' ')),
	write(Percentage),
	writeln(' %'), 
	writeln('                                                 -----'),
	write('TOTAL                                            '),
	writeln(Total), 
	writeln('                                                 -----').

total_pairs(Max, Max, Total, Total) :- !.

total_pairs(El, Max, Total, Newtotal) :-
	getval(vm_inst_ctr(El), Val),
	Tot is Total + Val,
	Nextel is El + 1,
	total_pairs(Nextel, Max,  Tot, Newtotal).  
	
name_to_el(Name, El, Bound) :-
	Bound > 0, !,
	(
		getval(vm_inst_name(Bound), Name),
		El is Bound
	;
		Newbound is Bound - 1,
		name_to_el(Name, El, Newbound)
	).
name_to_el(_, _, _).
*/

spaces(Num) :-
	Num > 0, !,
	write(' '),
	Num1 is Num - 1,
	spaces(Num1).
spaces(_Num).

spaces_to_name(Name, Newname) :-
	L is 26 - string_length(Name),
	append_spaces(Name, Newname, L).

append_spaces(Name, Newname, Num) :-
	Num > 0, !,
	substring("                              ", 0, Num, _, Spaces),
	append_strings(Name, Spaces, Newname).
append_spaces(_, _, _).

