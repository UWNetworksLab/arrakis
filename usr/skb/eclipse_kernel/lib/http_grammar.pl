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
% Copyright (C) 1991-2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): ECRC GmbH
% Contributor(s): IC-Parc, Imperal College London
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: http_grammar.pl,v 1.1 2008/06/30 17:43:46 jschimpf Exp $
% ----------------------------------------------------------------------

/* 
$Id: http_grammar.pl,v 1.1 2008/06/30 17:43:46 jschimpf Exp $
*/
/* 
P is a string containing general_header or request_header
or object_header.

HttpParams is a list of terms representing _interesting_ params, e.g:
bodyLength(BL).

The DCG is defined according to the http document.

The predicate token list performs the lexical analysis
*/	

:- module(http_grammar).

:- export
        status/5,
	header/3,
	token_to_list/2,
	list_to_token/2.


% status line
status(Version, ErrorCode, ErrorPhrase) -->
        version(Version), errorCode(ErrorCode), errorPhrase(ErrorPhrase).

version(Version) --> ["HTTP"], [/], [Version].
errorCode(ErrorCode)--> [ErrorCode].
errorPhrase(ErrorPhrase) --> error_list(ErrorWords), 
        {concat_string(ErrorWords, ErrorPhrase)}.

error_list([X, " "|T]) --> [X], error_list(T).
error_list([]) --> [].

	
% header
header(P) --> gene_head(P) ; requ_head(P) ; resp_head(P) ; obje_head(P).

% general header fields
gene_head(connection(L)) --> ["Connection"], [:], connect_option_list(L).
gene_head(keepAlive(T)) --> ["Keep"], [-], ["Alive"], [:], [T], {number(T)}.
gene_head(date(D)) --> ["Date"], [:], date(D).
gene_head(forwarded) --> ["Forwarded"], [:], forwarded.
gene_head(mandatory) --> ["Mandatory"], [:], mandatory.
gene_head(messageId) --> ["Message"], [-], ["ID"], [:], address_spec.
gene_head(messageId) --> ["Message"], [-], ["id"], [:], address_spec.
gene_head(mimeVersion) --> ["MIME"], [-], ["Version"], [:], mimeVersion.
gene_head(mimeVersion) --> ["MIME"], [-], [version], [:], mimeVersion.


connect_option_list([C|T]) --> connect_option(C), sep, connect_option_list(T).
connect_option_list([C]) --> connect_option(C).

connect_option(C) --> [T], [=],[W],
	{stratom(T), stratom(W), concat_string([T, =, W], C)}.
connect_option(C) --> [C], {stratom(C)}.

date(D) --> rest(D).
forwarded --> rest.
mandatory --> rest.
address_spec --> rest.
mimeVersion --> rest.

% request header fields
requ_head(userAgent) --> ["User"], [-], ["Agent"], [:], user_agent.
requ_head(userAgent) --> ["User"], [-], [agent], [:], user_agent.
requ_head(ifModifiedSince(D)) --> ["If"], [-], ["Modified"], [-], ["Since"], 
	[:], http_date(D).
requ_head(pragma) --> ["Pragma"], [:], pragma.
requ_head(authorization) --> ["Authorization"], [:], authorization.
requ_head(proxyAuthorization) --> ["Proxy"], [-], ["Authorization"], [:], 
	proxy_authorization.
requ_head(proxyAuthorization) --> ["Proxy"], [-], [authorization], [:], 
	proxy_authorization.
requ_head(referer) --> ["Referer"], [:], uri.
requ_head(from) --> ["From"], [:], from.
% !!!!!!!!!!!!!
requ_head(from) --> ["Host"], [:], from. % HACK netscape 2.0beta !!!
% !!!!!!!!!!!!!!!!
requ_head(acceptMediaType(MediaTypeList)) --> ["Accept"], [:], 
	media_type(MediaTypeList).
requ_head(accept_encoding) --> ["Accept"], [-], ["Encoding"], [:], 
	encoding.
requ_head(accept_encoding) --> ["Accept"], [-], [encoding], [:], 
	encoding.
requ_head(accept_charset) --> ["Accept"], [-], ["Charset"], [:], 
	charset.
requ_head(accept_charset) --> ["Accept"], [-], [charset], [:], 
	charset.
requ_head(acceptLanguage) --> ["Accept"], [-], ["Language"], [:], 
	language.
requ_head(acceptLanguage) --> ["Accept"], [-], [language], [:], 
	language.


user_agent --> rest.
http_date(D) --> rest(D).
pragma --> rest.
authorization --> rest.
proxy_authorization --> rest.
uri --> rest. 
from --> rest.
media_type_list([MediaType|L]) --> media_type(MediaType), media_type_list(L).
media_typelist([]) --> [].
encoding --> rest.
charset --> rest.
language --> rest.

media_type(mt(T, ST)) --> type_subtype(T, ST).
%media_type(mt(T, ST)) --> type_subtype(T, ST), [;], rest.

type_subtype(*, *) --> ['*/*'].
type_subtype(T, *) --> ttype(T), ['/*'].
type_subtype(*, ST) --> ['*/'], subtype(ST).
type_subtype(T, ST) --> ttype(T), [/], subtype(ST).

ttype(T) --> [T], {stratom(T)}; [T], [-], rest, {stratom(T)}.
subtype(ST) --> [ST], {stratom(ST)}; [ST], [-], rest, {stratom(ST)}.

% response header fields
resp_head(server) --> ["Server"], [:], server.
resp_head(wwwAuthenticate) --> ["WWW"], [-], ["Authenticate"], [:], www_authenticate.
resp_head(wwwAuthenticate) --> ["WWW"], [-], [authenticate], [:], www_authenticate.
resp_head(proxyAuthenticate) --> ["Proxy"], [-], ["Authenticate"], [:], 
	proxy_authenticate.
resp_head(proxyAuthenticate) --> ["Proxy"], [-], [authenticate], [:], 
	proxy_authenticate.
resp_head(retryAfter) --> ["Retry"], [-], ["After"], [:], retry_after.
resp_head(retryAfter) --> ["Retry"], [-], [after], [:], retry_after.

server --> rest.
www_authenticate --> rest.
proxy_authenticate --> rest.
retry_after --> rest.

% object header fields
obje_head(acceptRanges) --> ["Accept"], [-], ["Ranges"], [:], rest.
obje_head(acceptRanges) --> ["Accept"], [-], ["ranges"], [:], rest.
obje_head(allow) --> ["Allow"], [:], allow.
obje_head(contentLength(CL)) --> ["Content"], [-], ["Length"], [:], [CL].
obje_head(contentLength(CL)) --> ["Content"], [-], [length], [:], [CL].
obje_head(contentType(CT)) --> ["Content"], [-], ["Type"], [:], media_type(CT).
obje_head(contentType(CT)) --> ["Content"], [-], [type], [:], media_type(CT).
obje_head(contentEncoding) --> ["Content"], [-], ["Encoding"], [:], encoding.
obje_head(contentEncoding) --> ["Content"], [-], [encoding], [:], encoding.
obje_head(contentTransferEncoding) --> 
	["Content"], [-], ["Transfer"], [-], ["Encoding"], 
	[:], content_transfer_encoding.
obje_head(contentTransferEncoding) --> 
	["Content"], [-], [transfer], [-], [encoding], 
	[:], content_transfer_encoding.
obje_head(contentLanguage) --> ["Content"], [-], ["Language"], [:], language.
obje_head(contentLanguage) --> ["Content"], [-], [language], [:], language.
obje_head(etag) --> ["ETag"], [:], rest.
obje_head(etag) --> ["Etag"], [:], rest.
obje_head(expires(D)) --> ["Expires"], [:], http_date(D).
obje_head(lastModified(D)) --> ["Last"], [-], ["Modified"], [:], http_date(D).
obje_head(lastModified(D)) --> ["Last"], [-], [modified], [:], http_date(D).
obje_head(uriHeader) --> ["URI"], [:], uri_header.
obje_head(location) --> ["Location"], [:], uri.
obje_head(version) --> ["Version"], [:], version.
obje_head(derivedFrom) --> ["Derived"], [-], ["From"], [:], derived_from.
obje_head(derivedFrom) --> ["Derived"], [-], [from], [:], derived_from.
obje_head(title) --> ["Title"], [:], title.
obje_head(link) --> ["Link"], [:], link.
obje_head(link) --> ["X"], [-], ["Pad"], rest.
	
allow --> rest.
content_transfer_encoding --> rest.
uri_header --> rest.
version --> rest.
derived_from --> rest.
title --> rest.
link --> rest.

% common
rest --> [], !.
rest --> [_], rest.

rest([]) --> [], !.
rest([H|T]) --> [H],!, rest(T).
rest(Val) --> [Val].

sep --> [","], ssep.
ssep --> [] ; [","].


/*
correspondance between a stream and a list of token.
Used for lexical analysis (token_to_list).
Used for pretty printing  (list_to_token).
*/

token_to_list(Stream, List):-
	read_token(Stream, T, _),
	( T == end_of_file 
          -> List = []
        ;  List = [T|L],
	 token_to_list(Stream, L)
        ).

list_to_token([X|_], _):-
	var(X), !.
list_to_token([X|L], S):-
	write(S, X),
	list_to_token(L, S).
	
stratom(X) :- string(X).
stratom(X) :- atom(X).
