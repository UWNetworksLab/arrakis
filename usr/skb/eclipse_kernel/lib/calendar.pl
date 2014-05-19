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
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: calendar.pl,v 1.1 2008/06/30 17:43:42 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(calendar).

:- comment(summary,
    "Routines for calendar computations, based on modified julian dates (MJD).").

:- comment(date, "$Date: 2008/06/30 17:43:42 $").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(author, "Joachim Schimpf, IC-Parc").
:- comment(index, ["date and time","julian date","ISO 8601"]).
:- comment(desc, html("\
	Julian Dates (JD) and Modified Julian Dates (MJD) are a
	consecutive day numbering scheme widely used in astronomy,
	space travel etc. It is defined for a long period from
	12 noon, 1 Jan 4713 BC  to  12 noon, 1 Jan 3268 AD.
	<P>
	Here we use MJDs as the central representation (JDs are a bit
	awkward because they change at noon and are very large numbers).
	<P>
	Note that you can use fractional MJDs to denote the time of day.
	The time is then defined to be Universal Time (UT, formerly GMT).
	That means that every day has a unique integer number,
	or every time point has a unique float representation!
	(Using double floats, the resolution is better than 10
	microseconds until the year 2576, and better than 100
	microseconds until the year 24826).
	<P>
	Differences between times are obviously trivial to compute,
	and so are weekdays (by simple mod(7) operation).
	<P>
	The code is valid for dates starting from
		 1 Mar 0004 = MJD -677422 = JD 1722578.5
	<P>
	The relationship between JD and MJD is simply
	MJD = JD-2400000.5, ie MJD 0 = 17 Nov 1858.")).

% Implementation note:
%	To simplify the leap year computations, we work internally with years
%	that start on the 1st of Mar with day 0 and end on 28 or 29 Feb of
%	the next year with day 364 or 365. Also, we internally use a day
%	numbering starting from (nonexistent) 1.3.0000.
%
%	The 15 Oct 1582 (MJD -100840, JD 2299160.5) marks the starting
%	date of the gregorian calendar reform (modified leap year rule).

:- export
	date_to_mjd/2,		% date_to_mjd(+D/M/Y, -MJD)
	mjd_to_date/2,		% mjd_to_date(+MJD, -D/M/Y)
	mjd_to_weekday/2,	% mjd_to_weekday(+MJD, -DayName)
	mjd_to_dow/3,		% mjd_to_dow(+MJD, +FirstWeekday, -DoW)
	mjd_to_dow/2,		% mjd_to_dow(+MJD, -DoW)
	mjd_to_time/2,		% mjd_to_time(+MJD, -H:M:S)  GMT/UT
	time_to_mjd/2,		% time_to_mjd(+H:M:S, -MJD)  MJD < 1.0
	mjd_to_dy/2,		% mjd_to_dy(+MJD, -DoY/Y)
	dy_to_mjd/2,		% dy_to_mjd(+DoY/Y, -MJD)
	mjd_to_dwy/2,		% mjd_to_dwy(+MJD, -DoW/WoY/Y)
	mjd_to_dwy/3,		% mjd_to_dwy(+MJD, +FirstWeekday, -DoW/WoY/Y)
	dwy_to_mjd/2,		% dwy_to_mjd(+DoW/WoY/Y, -MJD)
	dwy_to_mjd/3,		% dwy_to_mjd(+DoW/WoY/Y, +FirstWeekday, -MJD)
	ymd_to_mjd/2,		% ymd_to_mjd(+YMD, -MJD)
	mjd_to_ymd/2,		% mjd_to_ymd(+MJD, -YMD)
	mjd_to_ywd/2,		% mjd_to_ywd(+MJD, -YWD)
	ywd_to_mjd/2,		% ywd_to_mjd(+YWD, -MJD)

	unix_to_mjd/2,		% unix_to_mjd(+UnixSec, -MJD)
	mjd_to_unix/2,		% mjd_to_unix(+MJD, -UnixSec)
	mjd_now/1,		% mjd_now(-MJD)

	jd_to_mjd/2,		% jd_to_mjd(+JD, -MJD)
	mjd_to_jd/2.		% mjd_to_jd(+MJD, -JD)

:- comment(date_to_mjd/2, [
    summary:"Convert a date in day/month/year form to an MJD day number",
    args:["DMY":"A structure of the form D/M/Y where D,M and Y are integers",
    	"MJD":"Variable or integer"],
    amode:date_to_mjd(++,?),
    see_also:[mjd_to_date/2]
    ]).
:- comment(mjd_to_date/2, [
    summary:"Converts an MJD day number into the corresponding Day/Month/Year",
    args:["MJD":"Integer or float","DMY":"Variable or structure"],
    amode:mjd_to_date(++,?),
    see_also:[date_to_mjd/2]
    ]).
:- comment(ymd_to_mjd/2, [
    summary:"Convert a date in ISO8601 Year-Month-Day form to an MJD day number",
    args:["YMD":"A structure of the form Y-M-D where D,M and Y are integers",
    	"MJD":"Variable or integer"],
    amode:ymd_to_mjd(++,?),
    see_also:[mjd_to_ymd/2]
    ]).
:- comment(mjd_to_ymd/2, [
    summary:"Converts an MJD day number into the corresponding ISO8601 Year-Month-Day form",
    args:["MJD":"Integer or float","YMD":"Variable or structure"],
    amode:mjd_to_ymd(++,?),
    see_also:[ymd_to_mjd/2]
    ]).
:- comment(time_to_mjd/2, [
    summary:"Convert the time in H:M:S form to a float MJD <1.0",
    desc:html("Returns a float MJD <1.0 encoding the time of day (UTC/GMT). This can be added to an integral day number to obtain a full MJD."),
    args:["HMS":"A structure of the form H:M:S where H and M are integers and S is a float",
	"MJD":"Variable or float"],
    amode:time_to_mjd(++,?)
    ]).
:- comment(mjd_to_time/2, [
    summary:"Extracts the time in H:M:S form from a float MJD",
    desc:html("returns the time of day (UTC/GMT) corresponding to the given MJD as Hour:Minute:Seconds structure, where Hour and Minute are integers and Seconds is a float"),
    args:["MJD":"Integer or float", "HMS":"Variable or structure"],
    amode:mjd_to_time(++,?)
    ]).
:- comment(mjd_to_weekday/2, [
    summary:"returns the weekday of the specified MJD as atom monday, tuesday etc",
    args:["MJD":"Integer or float", "DayName":"Variable or atom"],
    amode:mjd_to_weekday(++,?)
    ]).
:- comment(mjd_to_dy/2, [
    summary:"Convert an MJD to a DayOfYear/Year representation",
    desc:html("Convert MJD to a DayOfYear/Year representation, where DayOfYear is the relative day number starting with 1 on every January 1st"),
    args:["MJD":"Integer or float", "DY":"Variable or structure of the form DayOfYear/Year"],
    amode:mjd_to_dy(++,?)
    ]).
:- comment(dy_to_mjd/2, [
    summary:"Convert a DayOfYear/Year representation to MJD",
    desc:html("Convert a DayOfYear/Year representation to MJD, where DayOfYear is the relative day number starting with 1 on every January 1st"),
    args:["DY":"structure of the form DayOfYear/Year",
	"MJD":"Variable or integer"],
    amode:dy_to_mjd(++,?)
    ]).
:- comment(mjd_to_dwy/2, [
    summary:"Convert an MJD to a DayOfWeek/WeekOfYear/Year representation",
    desc:html("Convert MJDs to a DayOfWeek/WeekOfYear/Year representation,
    	where DayOfWeek is the day number within the week (1 for monday up to
	7 for sunday), and WeekOfYear is the week number within the year
	(starting with 1 for the week that contains January 1st)"),
    args:["MJD":"Integer or float", "DWY":"Variable or structure of the form Day/Week/Year"],
    amode:mjd_to_dwy(++,?)
    ]).
:- comment(dwy_to_mjd/2, [
    summary:"Convert a DayOfWeek/WeekOfYear/Year representation to MJD",
    desc:html("Convert a DayOfWeek/WeekOfYear/Year representation to MJD,
    	where DayOfWeek is the day number within the week (1 for monday up to
	7 for sunday), and WeekOfYear is the week number within the year
	(starting with 1 for the week that contains January 1st)"),
    args:["DWY":"structure of the form Day/Week/Year",
	"MJD":"Variable or integer"],
    amode:dwy_to_mjd(++,?)
    ]).
:- comment(mjd_to_dwy/3, [
    summary:"Convert an MJD to a DayOfWeek/WeekOfYear/Year representation",
    desc:html("as mjd_to_dwy/2, but allows to choose a different starting day for weeks, specified as atom monday, tuesday etc"),
    args:["MJD":"Integer or float",
	"FirstWeekday":"Atom (monday,tuesday,etc)",
	"DWY":"Structure of the form Day/Week/Year" ],
    amode:mjd_to_dwy(++,++,?)
    ]).
:- comment(dwy_to_mjd/3, [
    summary:"Convert a DayOfWeek/WeekOfYear/Year representation to MJD",
    desc:html("as dwy_to_mjd/2, but allows to choose a different starting day for weeks, specified as atom monday, tuesday etc"),
    args:["DWY":"Structure of the form Day/Week/Year",
	"FirstWeekday":"Atom (monday,tuesday,etc)",
	"MJD":"Variable or integer"],
    amode:dwy_to_mjd(++,++,?)
    ]).
:- comment(mjd_to_ywd/2, [
    summary:"Convert an MJD to ISO8601 Year-Week-Day representation",
    desc:html("Convert MJDs to a Year-WeekOfYear-DayOfWeek representation
	according to ISO 8601, where DayOfWeek is the day number within the
	week (1 for monday up to 7 for sunday), and WeekOfYear is the ISO8601
	week number (where week 1 is the week containing January 4th).
	Note that January 1 to 3 may belong to the previous year."),
    args:["MJD":"Integer or float", "YWD":"Variable or structure of the form Year-Week-Day"],
    amode:mjd_to_ywd(++,?)
    ]).
:- comment(ywd_to_mjd/2, [
    summary:"Convert an ISO8601 Year-Week-Day representation to MJD",
    desc:html("Convert a Year-WeekOfYear-DayOfWeek representation to MJD,
    	where DayOfWeek is the day number within the week (1 for monday
	up to 7 for sunday), and WeekOfYear is the ISO8601 week number
	(where week 1 is the week containing January 4th). Note that
	January 1 to 3 may belong to the previous year."),
    args:["DWY":"structure of the form Year-Week-Day",
	"MJD":"Variable or integer"],
    amode:ywd_to_mjd(++,?)
    ]).
:- comment(unix_to_mjd/2, [
    summary:"Convert the UNIX time representation into a (float) MJD",
    args:["UnixTime":"Integer or float (seconds since 1 Jan 1970)",
	"MJD":"Variable or float"],
    amode:unix_to_mjd(++,?)
    ]).
:- comment(mjd_to_unix/2, [
    summary:"Convert an MJD to the UNIX time representation",
    args:["MJD":"Integer or float",
    	"UnixTime":"Variable or integer"],
    amode:mjd_to_unix(++,?)
    ]).
:- comment(mjd_now/1, [
    summary:"Returns the current date/time as (float) MJD",
    args:["MJD":"Variable or float"],
    amode:mjd_now(?)
    ]).
:- comment(jd_to_mjd/2, [
    summary:"Convert Julian Dates (JD) to Modified Julian Dates (MJD)",
    desc:html("Convert Julian Dates (JD) to Modified Julian Dates (MJD). The relationship is simply MJD = JD-2400000.5"),
    args:["JD":"Integer or float",
	"MJD":"Variable or float"],
    amode:jd_to_mjd(++,?)
    ]).
:- comment(mjd_to_jd/2, [
    summary:"Convert Modified Julian Dates (MJD) to Julian Dates (JD)",
    desc:html("Convert Modified Julian Dates (MJD) to Julian Dates (JD). The relationship is simply JD = MJD+2400000.5"),
    args:["MJD":"Integer or float",
	"JD":"Variable or float"],
    amode:mjd_to_jd(++,?)
    ]).




%---------------------------------------------------------------------
:- pragma(expand).
:- pragma(nodebug).
%---------------------------------------------------------------------

date_to_mjd(Din/Min/Yin, MJD) :-
	date_to_internal(Din, Min, Yin, DayNr, Y),
	dy_leap_days(DayNr, Y, Leapdays),
	MJD is DayNr + 365*Y + Leapdays - 678883.


mjd_to_date(MJD, Dout/Mout/Yout) :-
	mjd_to_internal(MJD, Days, Leapdays),
	MJD1 is MJD + 1,
	mjd_to_internal(MJD1, _Days1, Leapdays1),
	Y is (Days - Leapdays1)//365,
	DayNr is Days - Y*365 - Leapdays,
	date_from_internal(Dout, Mout, Yout, DayNr, Y).


:- mode mjd_to_internal(+,-,-).
mjd_to_internal(MJD, DD, Leapdays) :-
	MJD >= -100840, !, 		% after 15 Oct 1582: gregorian
	DD is fix(MJD + 678881),	% 1 Mar 0000 if always gregorian
	Mod400 is DD mod 146097,	% mod days in 400 gregorian years
	Mod100 is Mod400 mod 36524,	% mod days in 100 gregorian years
	Leapdays is (DD//146097)*97 + (Mod400//36524)*24 + (Mod100//1461).
mjd_to_internal(MJD, DD, Leapdays) :-
	DD is fix(MJD + 678883),	% 1 Mar 0000 for real
	Leapdays is DD//1461.		% julian rule


:- mode dy_leap_days(+,+,-).
dy_leap_days(DayNr, Y, Leapdays) :-
	Y*365 + DayNr >= 577658, !, 		% after 15 Oct 1582?
	Leapdays is Y//4 - Y//100 + Y//400 + 2.	% gregorian rule
dy_leap_days(_, Y, Leapdays) :-
	Leapdays is Y//4.			% julian rule


date_to_internal(D, M, Y, DayNr, Ynorm) :-
	month_offset(M, Moffs, Yoffs),
	DayNr is Moffs + D - 1,
	Ynorm is Y + Yoffs.

date_from_internal(D, M, Y, DayNr, Ynorm) :-
	month_offset(Month, Moffs, Yoffs),
	DayNr >= Moffs,
	!,
	M = Month,
	D is DayNr - Moffs + 1,
	Y is Ynorm - Yoffs.


% month_offset(Month, MonthOffs, YearOffs)
% month_offset(1..12,    0..365,    -1..0)
:- mode month_offset(?, -, -).	% clause order important!
month_offset( 2, 337, -1).	% Feb
month_offset( 1, 306, -1).	% Jan
month_offset(12, 275,  0).	% Dec
month_offset(11, 245,  0).	% Nov
month_offset(10, 214,  0).	% Oct
month_offset( 9, 184,  0).	% Sep
month_offset( 8, 153,  0).	% Aug
month_offset( 7, 122,  0).	% Jul
month_offset( 6,  92,  0).	% Jun
month_offset( 5,  61,  0).	% May
month_offset( 4,  31,  0).	% Apr
month_offset( 3,   0,  0).	% Mar


%---------------------------------------------------------------------

mjd_to_time(MJD, H:M:S) :-
	Frac is MJD-floor(MJD),
	H is fix(Frac*24),
	M is fix(Frac*1440) mod 60,
	S is Frac*86400 - H*3600 - M*60.

time_to_mjd(H:M:S, MJD) :- !,
	MJD is ((H*60 + M)*60 + S)/86400.
time_to_mjd(H:M, MJD) :-
	MJD is (H*60 + M)/1440.

%---------------------------------------------------------------------

unix_to_mjd(Seconds_since_1970, MJD) :-
	MJD is 40587 + Seconds_since_1970/86400.

mjd_to_unix(MJD, Seconds_since_1970) :-
	Seconds_since_1970 is fix(round(86400*(MJD - 40587))).

mjd_now(MJD) :-
	get_flag(unix_time, T),
	unix_to_mjd(T, MJD).

%---------------------------------------------------------------------

jd_to_mjd(JD, MJD) :-
	MJD is JD-2400000.5.

mjd_to_jd(MJD, JD) :-
	JD is MJD+2400000.5.

%---------------------------------------------------------------------
% CAUTION: Make sure argument of fix() is always positive!!!

mjd_to_dy(MJD, DoY/Y) :-
	mjd_to_date(MJD, _/_/Y),
	DoY is fix(MJD - date_to_mjd(1/1/Y)) + 1.

dy_to_mjd(DoY/Y, MJD) :-
	MJD is date_to_mjd(1/1/Y) + DoY - 1.
	

ymd_to_mjd(Yin-Min-Din, MJD) :-		% ISO 8601
	date_to_mjd(Din/Min/Yin, MJD).

mjd_to_ymd(MJD, Yin-Min-Din) :-		% ISO 8601
	mjd_to_date(MJD, Din/Min/Yin).


mjd_to_dow(MJD, DoW) :-
	mjd_to_dow(MJD, monday, DoW).

mjd_to_dow(MJD, FirstWeekday, DoW) :-
	DoW is fix(MJD + 678881 + wd_nr(FirstWeekday)) mod 7 + 1.

    wd_nr(monday, 2).
    wd_nr(tuesday, 1).
    wd_nr(wednesday, 0).
    wd_nr(thursday, 6).
    wd_nr(friday, 5).
    wd_nr(saturday, 4).
    wd_nr(sunday, 3).


mjd_to_dwy(MJD, DWY) :-
	mjd_to_dwy(MJD, monday, DWY).

mjd_to_dwy(MJD, FirstWeekday, DoW/WoY/Y) :-
	mjd_to_date(MJD, _/_/Y),
	date_to_mjd(1/1/Y, Jan1st),
	mjd_to_dow(Jan1st, FirstWeekday, Jan1stW),
	mjd_to_dow(MJD, FirstWeekday, DoW),
	WoY is fix((MJD-Jan1st) - (DoW-Jan1stW) + 7) // 7.

dwy_to_mjd(DWY, MJD) :-
	dwy_to_mjd(DWY, monday, MJD).

dwy_to_mjd(DoW/WoY/Y, FirstWeekday, MJD) :-
	date_to_mjd(1/1/Y, Jan1st),
	mjd_to_dow(Jan1st, FirstWeekday, Jan1stW),
	MJD is Jan1st + 7*WoY + (DoW-Jan1stW) - 7.


% ISO8601 week numbering
mjd_to_ywd(MJD, WY-WoY-DoW) :-
	mjd_to_date(MJD, _/_/Y),
	date_to_mjd(4/1/Y, Jan4th),
	mjd_to_dow(Jan4th, monday, Jan4thW),	% Week 1 contains Jan 4th
	MondayWeek0 is Jan4th - Jan4thW - 6,
	WoY0 is fix(MJD - MondayWeek0) // 7,
	( WoY0 == 0 ->
	    WY is Y-1,
	    date_to_mjd(4/1/WY, PrevJan4th),
	    mjd_to_dow(PrevJan4th, monday, PrevJan4thW),
	    PrevMondayWeek0 is PrevJan4th - PrevJan4thW - 6,
	    WoY is (MJD - PrevMondayWeek0) // 7
	;
	    WoY = WoY0,
	    WY is Y
	),
	mjd_to_dow(MJD, monday, DoW).

ywd_to_mjd(WY-WoY-DoW, MJD) :-
	date_to_mjd(4/1/WY, Jan4th),	% in week 1
	mjd_to_dow(Jan4th, monday, Jan4thW),
	MJD is Jan4th + 7*WoY + (DoW-Jan4thW) - 7.


mjd_to_weekday(MJD, WD) :-
	I is fix(MJD + 678881) mod 7 + 1,	% avoid negative modulus!
	arg(I, wd(wednesday, thursday, friday,
			saturday, sunday, monday, tuesday), WD).

/***
%-----------------------------------------------------------------------
% exhaustive test
% should only print the 10 missing days 5..14 Oct 1582
%-----------------------------------------------------------------------
test_all :-
	setval(mjd, -678577), between(1,    3267, 1, Y),
%	setval(mjd, -101117), between(1582, 3267, 1, Y),

	put(13), write(Y), flush(output),
	between(1, 12, 1, M),
	(leap(Y) ->
	    arg(M, d(31,29,31,30,31,30,31,31,30,31,30,31), MD)
	;
	    arg(M, d(31,28,31,30,31,30,31,31,30,31,30,31), MD)
	),
	between(1, MD, 1, D),
	DateIn = D/M/Y,
	date_to_mjd(DateIn, MJD),
	mjd_to_date(MJD, DateOut),
	( DateOut == DateIn ->
	    getval(mjd, MJDexp),
	    ( MJD =:= MJDexp ->
		incval(mjd)
	    ;
		printf("wrong MJD generated: %w\n", date_to_mjd(DateIn, MJDexp) -> MJD)
	    )
	;
	    printf("reverse conversion failed: %w\n", mjd_to_date(MJD, DateIn) -> DateOut)
	),
	fail.
test_all.

leap(Y) :-
	Y mod 4 =:= 0,
	( Y < 1582 ->
	    true
	; Y mod 100 =:= 0 ->
	    Y mod 400 =:= 0
	;
	    true
	).

now :-
	mjd_now(MJD),
	mjd_to_date(MJD, Date),
	mjd_to_time(MJD, Time),
	mjd_to_weekday(MJD, W),
	writeln((W,Date,Time,mjd=MJD)).
***/

:- comment(eg, html("
What day of the week was the 29th of December 1959?
<PRE>
[eclipse 1]: lib(calendar).
[eclipse 2]: date_to_mjd(29/12/1959, MJD), mjd_to_weekday(MJD,W).
MJD = 36931
W = tuesday
</PRE>
What date and time is it now?
<PRE>
[eclipse 3]: mjd_now(MJD), mjd_to_date(MJD,Date), mjd_to_time(MJD,Time).
Date = 19 / 5 / 1999
MJD = 51317.456238425926
Time = 10 : 56 : 59.000000017695129
</PRE>
How many days are there in the 20th century?
<PRE>
[eclipse 4]: N is date_to_mjd(1/1/2001) - date_to_mjd(1/1/1901).
N = 36525
</PRE>
The library code does not detect invalid dates, but this is easily done by converting a date to its MJD and
back and checking whether they match:
<PRE>
[eclipse 5]: [user].
valid_date(Date) :-
        date_to_mjd(Date,MJD),
        mjd_to_date(MJD,Date).

[eclipse 6]: valid_date(29/2/1900). % 1900 is not a leap year!
no (more) solution.
</PRE>"
)).
