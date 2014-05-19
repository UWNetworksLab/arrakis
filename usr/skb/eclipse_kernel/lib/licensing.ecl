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
% Copyright (C) 2005-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% 
% Tools for issuing licences
%
% System:	ECLiPSe Constraint Logic Programming System
% Author/s:	Joachim Schimpf, IC-Parc
% Version:	$Id: licensing.ecl,v 1.1 2008/06/30 17:43:47 jschimpf Exp $
%
% ----------------------------------------------------------------------

:- module(licensing).

:- comment(summary, "Eclipse Licencing Facility").
:- comment(author, "Joachim Schimpf, IC-Parc").
:- comment(copyright, "Cisco Systems, Inc").
:- comment(date, "$Date: 2008/06/30 17:43:47 $").

:- comment(desc, html("
    <h3>ECLiPSe Licencing Facility</h3>
    <P>
    When ECLiPSe starts up, it looks for the file licence.ecl in its
    library directory (&lt;eclipsedir&gt;/lib).  If this file is not
    present, an academic version is assumed and started with a
    corresponding warning banner being displayed.
    </P><P>
    If the licence file is present, the licence information is checked,
    and if successful, ECLiPSe starts up. If the licence is invalid,
    ECLiPSe prints a message on the error stream and exits. A licence
    can be invalid for several reasons: its signature may be invalid
    (i.e. the licence has been tampered with), the hostid does not
    match the hostid for which the licence was issued, or the licence's
    expiry date has passed.
    </P>
    <h3>Licence File</h3>
    <P>
    The licence file contains one or more signed licence terms in human
    readable ECLiPSe syntax. A signed licence terms is a term of the
    form signed(Licence, Signature), where Licence is a licence term as
    described below, and Signature is a string representing a cryptographic
    signature.
    </P><P>
    The actual licence term is a list of Name:Value pairs.
    Currently understood by the licence checking code in the ECLiPSe
    kernel are:
    </P><DL>
    <DT>licensee</DT>
	<DD>The name of the licensee (string). This is mandatory</DD>
    <DT>host</DT>
	<DD>The hostid of the licenced host (string). If present, this must
	match the result of calling get_flag(hostid, HostId) on the machine
	where ECLiPSe is run.</DD>
    <DT>expiry</DT>
	<DD>The expiry date of the licence (integer). This is in seconds
	since 1/1/1970. If present, the licence is only valid before
	the given date. If missing, the licence does not expire.</DD>
    <DT>version</DT>
	<DD>The highest version that this licence applies to (list of
	two integers [Major,Minor]). The licence is only valid for
	ECLiPSe version up to Major.Minor.<DD>
    </DL>
    <h3>Licence Creation</h3>
    <P>
    Licences are created using the predicates provided in lib(licensing).
    For instance:
    </P><PRE>
    ?- lib(licensing).
    calendar.pl compiled traceable 9096 bytes in 0.20 seconds
    licensing.ecl compiled traceable 3392 bytes in 0.29 seconds
    Yes (0.30s cpu)

    ?- make_licence.
    Licensee?  <unquoted string>  John Smith
    Host Id?  <unquoted string>  Sun_Microsystems#2158452416
    Expiry Date?  Year-Month-Day.  2005-10-31.
    Licenced version?  [Major,Minor].  [5,9].

    signed([licensee : \"John Smith\", host : \"Sun_Microsystems#2158452416\",
            expiry : 1130716800], \"168690125945654305...1612674986192631\").
    Yes (0.19s cpu)
    </PRE><P>
    The source code of this library is to be kept confidential because it
    contains the private key used for signing the licences.
    </P>
")).

:- lib(calendar).


:- export append_licence/1.
:- comment(append_licence/1, [
    amode:append_licence(+),
    args:["File":"Atom or string"],
    summary:"Make licence term and append it to file",
    desc:html("Prompt user for input, create a licence term, sign it,
    	and append the result to the file with the given name."),
    see_also:[make_licence/0,make_licence/1]
    ]).

append_licence(File) :-
	open(File, append, Out),
	make_licence(Out),
	close(Out).

:- export make_licence/0.
:- comment(make_licence/0, [
    summary:"Make licence term and print it",
    desc:html("Prompt user for input, create a licence term, sign it,
	and write the result to standard output."),
    see_also:[append_licence/1,make_licence/1]
    ]).

make_licence :-
	make_licence(output).

:- export make_licence/1.
:- comment(make_licence/1, [
    summary:"Make licence term and print it to Stream",
    args:["Stream":"A (write) stream identifier"],
    desc:("Prompt user for input, create a licence term, sign it,
	and write the result to the given stream."),
    see_also:[append_licence/1,make_licence/0]
    ]).

make_licence(Out) :-
	write("Licensee?  <unquoted string> "), read_string(end_of_line, _, Licensee),
	write("Host Id?  <unquoted string> "), read_string(end_of_line, _, HostId),
	once (
	    repeat,
	    write("Expiry Date?  Year-Month-Day. "),
	    read(ExpiryDate),
	    ymd_to_mjd(ExpiryDate, ExpiryMjd),
	    mjd_to_unix(ExpiryMjd, Expiry)
	
	),
	once (
	    repeat,
	    write("Licenced version?  [Major,Minor]. "),
	    read(Version),
	    nonvar(Version),
	    Version = [Major,Minor],
	    integer(Major),
	    integer(Minor)
	),

	LicenceTerm = [
	    licensee:	Licensee,
	    host:	HostId,
	    expiry:	Expiry],

	sign(LicenceTerm, SignedLicenceTerm),

	writeq(Out, SignedLicenceTerm),
	writeln(Out, .).

valid_date(Date) :-
	string(Date),
	ymd_to_mjd(Date, _).


ask(Prompt, Data, Validate) :-
	printf("%s? %b", [Prompt]),
	read(Data),
	Goal =.. [Validate,Data],
	call(Goal).


%:- export sign_file/2.

sign_file(InFile, OutFile) :-
	open(InFile, read, In),
	open(OutFile, write, Out),
	sign_stream(In, Out),
	close(In),
	close(Out).

sign_stream(In, Out) :-
	read(In, Term),
	( Term == end_of_file ->
	    true
	; Term = signed(_,_) ->
	    writeq(Out, Term),
	    writeln(Out, .),
	    sign_stream(In, Out)
	;
	    sign(Term, SignedTerm),
	    writeq(Out, SignedTerm),
	    writeln(Out, .),
	    sign_stream(In, Out)
	).

sign(Term, signed(Term, SignatureString)) :-
	sepia_kernel:hash_secure(Term, Digest, sha),
	private_key(D, N),
	Signature is sepia_kernel:powm(Digest,D,N),
	open(string(""), write, S),
%	printf(S, "%36R", [Signature]),
	printf(S, "%10R", [Signature]),
	get_stream_info(S, name, SignatureString).

private_key(9840154045999550030761227840677457033601125459811247886358199883215804998615140097724487329137055555005858326019124584570617300837058886173463595157643775533903378593089247235090102095125624740138636680034702094824497941678762877486270055011217264966930221572613935912461384757893091609805736415776978239985,
    21914161071951772490417739500054678264714316157992140467021105282300879910358542740162430501913497561468260342080059381256137594184082254908360199026967589435446562798562242943975279574163853396385755498066856539655902646718824668922469051215343559030281711267234935602376733839726736220820352137086182611433).


keylen(KeyLen) :-
	private_key(_D, N),
	( fromto(N,N1,N2,1), count(_,1,KeyLen) do N2 is N1//2 ).

