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
% Copyright (C) 1999-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Parc Technologies Ltd
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: time_log.ecl,v 1.1 2008/06/30 17:43:50 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(time_log).

:- export initialise_time_logging/1.
:- export log_time_local/2.
:- export collate_time_logs/4.

:- comment(summary, "Module for logging and collating test times").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:50 $").
:- comment(desc, html("\
	This module is used for logging and collating the times taken to run
	tests (but could be used for other similar purposes).  Before
	running any tests, call initialise_time_logging/1.  This deletes any
	old administrative files from a previous test run.  Then, within the
	test harness for an individual test, call log_time_local/2 to log
	the time taken for the test to a temporary local file.  Finally,
	once all the tests have completed successfully, call
	collate_time_logs/4 to collect all the local administrative files
	and add all the data to the master log file.
")).

:- comment(initialise_time_logging/1, [
    summary:"Prepare for time logging",
    amode:initialise_time_logging(++),
    args:["Directory":"Root test directory"],
    desc:html("\
	This predicate prepares the given directory for time logging.
	Essentially all it does is traverse the directory tree looking for
	old administrative files and deleting them.
")]).

:- comment(log_time_local/2, [
    summary:"Log a time in the local directory",
    amode:log_time_local(++, ++),
    args:[
    	"Name":"Name of test",
	"Time":"Time taken to run test"
    ],
    desc:html("\
	Logs to a temporary administrative file in the local directory that
	the test with the given name took the specified time to execute.
")]).

:- comment(collate_time_logs/4, [
    summary:"Collate local time logs into master log",
    amode:collate_time_logs(++, ++, ++, ++),
    args:[
	"Directory":"Root test directory",
	"Package":"ECLiPSe package used",
	"Embedding":"ECLiPSe embedding used",
	"MasterLogFile":"Master log file"
    ],
    desc:html("\
    	Collates all the temporary administrative files in the directory
	tree specified and logs them to the specified master log file.</P><P>

	Package specifies which ECLiPSe package was used, such as `standard'
	or `runtime'.</P><P>

	Embedding specifies which ECLiPSe embedding was used, such as
	`standalone' or `java'.</P><P>
")]).


:- lib(calendar).


:- local struct(local_time(
		name,			% Name of test in local directory
		time			% Time taken for test
	    )).

:- local struct(test_info(
		test_subdir,		% Subdirectory test was in
		test_name,		% Name of test
		test_time		% Time test took to run
	    )).
:- local struct(date_info(
		collection_date,	% Date info was collected
		collection_time		% Time info was collected
	    )).
:- local struct(version_info(
		eclipse_version		% 5.2, etc.
		%build_number
	    )).
:- local struct(config_info(
		eclipse_package,	% standard or runtime
		eclipse_embedding	% standalone or java
	    )).
:- local struct(eclipse_info(
		eclipse_version_info:version_info,
		eclipse_config_info:config_info,
		eclipse_location	% $ECLIPSEDIR
	    )).
:- local struct(machine_info(
		architecture,		% i386_linux, etc.
		machine_name		% goat.icparc.ic.ac.uk, etc.
	    )).
:- local struct(user_info(
		user_name		% wh, js10, etc.
	    )).
:- local struct(time_log(
		test_info : test_info,
		date_info : date_info,
		eclipse_info : eclipse_info,
		machine_info : machine_info,
		user_info : user_info
	    )).

:- local portray(test_info/3, portray_struct/2, [write]).
:- local portray(date_info/2, portray_struct/2, [write]).
:- local portray(version_info/1, portray_struct/2, [write]).
:- local portray(config_info/2, portray_struct/2, [write]).
:- local portray(eclipse_info/3, portray_struct/2, [write]).
:- local portray(machine_info/2, portray_struct/2, [write]).
:- local portray(user_info/1, portray_struct/2, [write]).
:- local portray(time_log/5, portray_struct/2, [write]).


    %
    % Initialise time logging --- by deleting any local time log files.
    %
initialise_time_logging(Directory) :-
	find(Directory, ".time_log", LocalLogFiles),
	(
	    foreach(LogFile, LocalLogFiles)
	do
	    delete(LogFile)
	).

    %
    % Log a test time locally
    %
log_time_local(Name, Time) :-
	open(".time_log", append, Stream),
	printf(Stream, "local_time(%q, %.2f).%n", [Name, Time]),
	close(Stream).

    %
    % Collect all the local time log files, add the global data, and append
    % them to the master log file.
    %
collate_time_logs(Directory, Package, Embedding, MasterLogFile) :-
	create_info_template(Package, Embedding, Template),
	find(Directory, ".time_log", LocalLogFiles),
	split_string(Directory, "/", "/", DirPartList),
	length(DirPartList, NDirParts),
	open(MasterLogFile, append, Stream),
	(
	    (
		foreach(LogFile, LocalLogFiles),
		param(NDirParts, Template, Stream)
	    do
	    	process_local_log_file(LogFile, NDirParts, Template, Stream)
	    )
	->
	    true
	;
	    printf(error, "Error collating local time logs.%n", [])
	),
	close(Stream).

process_local_log_file(LogFile, NStripDirParts, Template, Stream) :-
	% Extract the name of the test subdirectory.
	split_string(LogFile, "/", "/", LogFilePartList),
	drop_n(NStripDirParts, LogFilePartList, LogFilePartList1),
	drop_last(LogFilePartList1, TestSubdirPartList),
	join_string(TestSubdirPartList, "/", TestSubdir),

	% Read and process the log file.
	read_file_terms_as_list(LogFile, LocalTimeList),
	(
	    foreach(LocalTime, LocalTimeList),
	    param(Template, TestSubdir, Stream)
	do
	    LocalTime = local_time{
		    name:	TestName,
		    time:	TestTime
	    	},
	    TestInfo = test_info{
		    test_subdir:TestSubdir,
		    test_name:	TestName,
		    test_time:	TestTime
		},
	    update_struct(time_log, [test_info:TestInfo], Template, LogTerm),
	    printf(Stream, "%QDPw.%n", [LogTerm]),
	    flush(Stream)
	).

create_info_template(Package, Embedding, Template) :-
	% Collection date info.
	mjd_now(MJD),
	mjd_to_ymd(MJD, CollectionDate),
	mjd_to_time(MJD, Hour:Minute:Second0),
	Second is fix(Second0),
	CollectionTime = Hour:Minute:Second,
	DateInfo = date_info{
		collection_date:	CollectionDate,
		collection_time:	CollectionTime
	    },

	% Eclipse info.
	get_flag(version, EclipseVersion),
	% XXX - Get build number as well
	VersionInfo = version_info{
		eclipse_version:	EclipseVersion
	    },
	ConfigInfo = config_info{
		eclipse_package:	Package,
		eclipse_embedding:	Embedding
	    },
	get_flag(installation_directory, EclipseLocation),
	EclipseInfo = eclipse_info{
		eclipse_version_info:	VersionInfo,
		eclipse_config_info:	ConfigInfo,
		eclipse_location:	EclipseLocation
	    },

	% Machine info.
	get_flag(hostarch, Architecture),
	get_flag(hostname, MachineName),
	MachineInfo = machine_info{
		architecture:	Architecture,
		machine_name:	MachineName
	    },

	% User info.
	( getenv("LOGNAME", UserName) ->
	    true
	;
	    UserName = unknown
	),
	UserInfo = user_info{user_name:UserName},

	Template = time_log{
		date_info:		DateInfo,
		eclipse_info:		EclipseInfo,
		machine_info:		MachineInfo,
		user_info:		UserInfo
	    }.


find(Directory, Pattern, FileList) :-
	find(Directory, Pattern, FileList, []).

find(Directory, Pattern, FileList0, FileList) :-
	read_directory(Directory, Pattern, SubdirList, LocalFileList),
	(
	    foreach(LocalFile, LocalFileList),
	    fromto(FileList0, [File | Tail], Tail, FileList1),
	    param(Directory)
	do
	    join_string([Directory, LocalFile], "/", File)
	),
	(
	    foreach(Subdir, SubdirList),
	    fromto(FileList1, FileListOut, FileListIn, FileList),
	    param(Directory, Pattern)
	do
	    join_string([Directory, Subdir], "/", Dir),
	    find(Dir, Pattern, FileListOut, FileListIn)
	).

read_file_terms_as_list(File, List) :-
	open(File, read, Stream),
	(
	    read(Stream, Entry0),
	    (
		fromto(Entry0, PrevEntry, NextEntry, end_of_file),
		foreach(Entry, List),
		param(Stream)
	    do
		Entry = PrevEntry,
		read(Stream, NextEntry)
	    )
	->
	    close(Stream)
	;
	    % Make sure we close the stream even if something failed.
	    close(Stream),
	    fail
	).

drop_n(N, List, Remainder) :-
	( N > 0 ->
	    List = [_ | Tail],
	    N_1 is N - 1,
	    drop_n(N_1, Tail, Remainder)
	;
	    N =:= 0,
	    Remainder = List
	).

drop_last([_], []) :-
	!.
drop_last([X | Xs], [X | Ys]) :-
	drop_last(Xs, Ys).


portray_struct(Term, no_macro_expansion(with(Functor,List))) :-
	functor(Term, Functor, Arity),
	functor(Template, Functor, Arity),
	current_struct(Functor, Template),
	Term =.. [_ | Args],
	Template =.. [_ | Fields],
	(
	    foreach(Arg, Args),
	    foreach(Field, Fields),
	    foreach(Name:Arg, List)
	do
	    ( Field = Name : _ ->
	    	true
	    ;
	    	Name = Field
	    )
	).

