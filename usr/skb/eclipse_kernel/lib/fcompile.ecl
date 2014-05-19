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
% Copyright (C) 2006,2007 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf.
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Component:	ECLiPSe III compiler - backward compatibility
% Version:	$Id: fcompile.ecl,v 1.1 2008/06/30 17:43:45 jschimpf Exp $
% ----------------------------------------------------------------------

:- module(fcompile).

:- comment(summary,	"ECLiPSe III compiler - backward compatibility").
:- comment(copyright,	"Cisco Technology Inc").
:- comment(author,	"Joachim Schimpf").
:- comment(date,	"$Date: 2008/06/30 17:43:45 $").
:- comment(status,	obsolete).

:- comment(desc, html("
    This module contains fcompile/1,2 for backward compatibility.
")).


:- lib(ecl_compiler).


%----------------------------------------------------------------------
% Compatibility: fcompile
%----------------------------------------------------------------------

:- comment(fcompile/1, 
[summary: "Generates a byte-form object file from the ECLiPSe source File.",
 args: ["File":"Name of source file (Atom or string)"], 
 amode: fcompile(++),
 see_also: [fcompile/2], 
 desc: html("\
<P>
   This predicate is obsolete, use compile/2 instead.
</P><P>
   Same as <TT>fcompile/2</TT> with all options set to their default
   values: the object file is placed in the current working directory,
   byte format is generated, non-verbose, and fcompile tries to
   compile the program first.</P>")
]).

:- comment(fcompile/2, 
[
summary: "Generates an object file from the ECLiPSe source File with specified options.",
args: ["File":"Name of source file (Atom or string)",
        "Options":"List of valid options and their values"],
see_also: [fcompile/1], 
amode: fcompile(++,++), 
eg:    "\
   fcompile(my_prog, []).   % equivalent to fcompile(my_prog)

   fcompile(my_prog, [format:text, outdir:'../']).
   % generate the object file in text format, and place it in the parent dir
",
desc: html("\
<P>
   This predicate is obsolete, use compile/2 instead.
</P><P>
   Compiles the specified Prolog source file and generates an object file
   with the same base name as the source file but with suffix <TT>.eco</TT>.
   Object files can be loaded by the built-in predicates <TT>use_module/1</TT>,
   <TT>lib/1</TT>, <TT>compile/1</TT> and <TT>ensure_loaded/1</TT>, and also
   with the eclipse -b command-line option.
</P><P>
   File must be instantiated to a legitimate specification for an existing
   file except for the suffix, which may be omitted. Options is a list of
   Option:Value pairs where Option and value can be:
</P>
<DL>
   <DT>compile: YesOrNo

       <DD> YesOrNo is either the  atom yes or no. For 'yes', fcompile will
       try to first compile File (checking that it has not already been
       compiled first).  This is usually what is required, as it ensures
       that File can be properly read to generate the object file. The
       default is 'yes'.

   <DT>format: ByteOrText

      <DD>ByteOrText is either the atom <TT>byte</TT> or <TT>text</TT>.  If 'text', the
      object file will be in a textual format.  If 'byte', the object file
      will be in a binary format which is larger, but will load faster
      and is not human readable. The default is byte.

   <DT>outdir: OutputDirectory

       <DD> OutputDirectory is the directory where the generated object
       file will be placed. It can be an atom or a string. The default is
       the current working directory.

   <DT>verbose: YesOrNo

       <DD> YesOrNo is either the  atom yes or no. For 'yes', fcompile will
       report in detail the predicates it is dumping out. This is probably
       only needed for debugging fcompile, to trace its progress. The default
       is 'no'.
</DL><P>
   The predicate will look for File with a `source' suffix (i.e. no
   suffix, <TT>.ecl</TT> or <TT>.pl</TT>), compile the file by calling compile, and
   generate an object form of the source file with suffix <TT>.eco</TT>.
   The user should use <TT>include/1</TT> directives to include all files that are
   in the same module as the master file, and <TT>use_module/1</TT> directives for
   files that define a different module. Files mentioned in include
   directives will not need to be fcompiled separately for their object
   form.
</P><P>
   This object form can be loaded into an ECLiPSe session even on a
   different operating system/hardware platform from the one on which
   it was generated.  However, the object format may change incompatibly
   between different releases of ECLiPSe.
</P><P>
   The fcompile library does not need to be loaded in order to load the object
   file.  The built-in predicates <TT>ensure_loaded/1</TT>, <TT>use_module/1</TT>
   and <TT>lib/1,2</TT> will try to load an object file in preference to a
   source file, if both are present.  The compile built-ins on the
   other hand will prefer source files, unless the <TT>.eco</TT>
   suffix is explicitly given.
</P><P>
   It is recommended that object files always contain proper modules.
   If an object file contains module-free code, then loading it into
   an existing module that already has code can cause conflicts with
   auxiliary predicates (e.g. from <TT>do/2</TT> loop constructs).
</P><P>
   <EM>Restrictions:</EM>
<UL>
     <LI>macro definitions should be quoted using 
     <TT>no_macro_expansion/1</TT>, e.g.
     <PRE>
         :- local macro(no_macro_expansion(maxint), 9999).
     </PRE>

     <LI>directives in the module should not change the state 
         of compiled code.

     <LI>big integer constants between -2^63 and -2^31 and between
     	2^31 and 2^63 should not appear directly in the source, and
	will provoke a warning because the generated object code will
	not be portable between 32 and 64 bit machines.
</UL>
</P>")
]).

:- export fcompile/1.
:- tool(fcompile/1, fcompile_/2).
fcompile_(File, Module) :-
	compile_(File, [output:eco,load:new], Module).

:- export fcompile/2.
:- tool(fcompile/2, fcompile_/3).
fcompile_(File, Options0, Module) :-
	% translate old fcompile's compile-option into load-option
	( delete(compile:CompileOpt, Options0, Options1) ->
	    ( memberchk(load:LoadOpt, Options1) ->
	        ( option_compile_load(CompileOpt, LoadOpt) ->
		    Options2 = Options1
		;
		    printf(error, "Incompatible compile/load options: %w%n",
		    	[fcompile(File, Options0)]),
		    abort
		)
	    ; CompileOpt == yes ->
		Options2 = [load:all|Options1]
	    ;
		Options2 = [load:none|Options1]
	    )
	;
	    ( memberchk(load:_LoadOpt, Options0) ->
		Options2 = Options0
	    ;
		Options2 = [load:new|Options0]
	    )
	),

	( delete(output:_, Options2, Options3) ->
	    printf(error, "Ignoring output: option, using output:eco%n",[])
	;
	    Options3 = Options2
	),
	Options4 = [output:eco|Options3],

	( delete(format:Fmt, Options4, Options5) ->
	    printf(error, "Ignoring format:%w option, using text (preliminary)%n",[Fmt])
	;
	    Options5 = Options4
	),

	( delete(verbose:Verb0, Options5, Options6) ->
	    option_verbose(Verb0, Verb1),
	    Options7 = [verbose:Verb1|Options6]
	;
	    Options7 = Options5
	),

	compile(File, Options7)@Module.

    option_compile_load(yes, all).
    option_compile_load(no, new).

    option_verbose(yes, 1).
    option_verbose(no, 0).

