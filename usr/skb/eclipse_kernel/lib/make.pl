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
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: make.pl,v 1.1 2008/06/30 17:43:47 jschimpf Exp $
% ----------------------------------------------------------------------

%
% SEPIA PROLOG LIBRARY MODULE
%
% IDENTIFICATION:	make.pl
%
% AUTHOR:		Joachim Schimpf
%
% CONTENTS:		make/0
%			compiled/0
%
% DESCRIPTION:
%
%	When this library is loaded, the system records all the compiled
%	files and their modification dates.
%
%	When make/0 is invoked, all files that have been modified since
%	their last compiliation are recompiled.
%
%	compiled/0 prints a list of the compiled files.
%
%	The idea for make/0 is due to Jan Wielemaker (SWI-Prolog).
%

:- module(make).

:- global make/0, compiled/0.

:- skipped make/0, compiled/0.

:- untraceable make/0, compiled/0.

:- local_record(compiled_file).

% event handler to record the compiled files, the modules they are
% compiled into and the date of the file when compiled

record_compiled_files(N, CompInfo, Module) :-
	CompInfo = (File, _, IsDump),
	IsDump \== dumped,
	File \== user,
	File \== term,
	!,
	get_file_info(File, mtime, Time),
	(erase(compiled_file, .(File, _, _)) -> true ; true),
	record(compiled_file, .(File, Module, Time)),
	error(default(N), CompInfo, Module).
record_compiled_files(N, CompInfo, Module) :-
	error(default(N), CompInfo, Module).

:- set_error_handler(139, record_compiled_files/3).


% recompile all modified files

make :-
	recorded(compiled_file, .(File, Module, Time)),
	    get_file_info(File, mtime) =\= Time,
	    compile(File, Module),
	    fail.
make.


% print a list of compiled files and if they were modified since

compiled :-
	recorded(compiled_file, .(File, _, Time)),
	write(File),
	(get_file_info(File, mtime) =\= Time ->
		writeln(" (modified)")
	;
		nl
	),
	fail.
compiled.

