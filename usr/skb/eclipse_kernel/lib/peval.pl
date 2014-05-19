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
% 
% END LICENSE BLOCK
%
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: peval.pl,v 1.1 2008/06/30 17:43:48 jschimpf Exp $
% ----------------------------------------------------------------------

/*
Partial evaluator for pure Prolog plus ineq, ~=, cut and not. No other
builtins. Uses no cut execution rules, but uses auxiliary predicate
creation to preserve correctness. Needs directives `unfold(G)'. No
folding or generalisation is done.
*/

pe(InFile,OutFile) :-
   erase_all(program), readprog(InFile), setval(newname,0),
   unfold_loop, writeto(OutFile), show_program, written, !.

unfold_loop :-
   recorded(program,F/A/L), splitlist(L,L1,(H:-T),L2),
   splitconj(T,T1,G,T2), want_to_unfold(G), !, unfold(G,U),
   (cutcalls(U) -> acc(H,T1,G,T2,N),
                   append(L1,[(H:-T1,N)],L2), append(L2,T2,L3)
                 ; expand(H,T1,G=U,T2,X),
                   append(L1,X,L1U), append(L1U,L2,L3)),
   erase(program,F/A/_), record(program,F/A/L3),
   unfold_loop.
unfold_loop.

acc(H,T1,G,T2,N) :-
   cj(G,T2,GT2), newpred(N,GT2,H+T1), cj(T1,GT2,X), addclause((N:-X)).

newpred(N,R,V) :- varset(R,S), intersect(S,V,S1), newpred1(N,S1).

newpred1(N,S) :-
   getval(newname,I), atom_string(aux,Aux), term_string(I,Istr),
   append_strings(Aux,Istr,Nstr), atom_string(Name,Nstr),
   N=..[Name|S], incval(newname).

varset(T,S) :- varset(T,Z-Z,S-[]), !.

varset(T,Si,So) :- var(T), not vdmember(T,Si), !, dappend(Si,[T|Z]-Z,So).
varset(T,Si,Si) :- var(T), !.
varset(T,Si,So) :- T=..[_|L], varsetl(L,Si,So).

varsetl([],Si,Si).
varsetl([H|T],Si,So) :- varset(H,Si,St), varsetl(T,St,So).

vdmember(V,A-B) :- A==B, !, fail.
vdmember(V,[A|B]-C) :- V==A; vdmember(V,B-C).

dappend(A-B,B-C,A-C).

intersect([],_,[]) :- !.
intersect([A|B],V,[A|C]) :- occurs(A,V), !, intersect(B,V,C).
intersect([_|B],V,C) :- intersect(B,V,C).

splitlist([A|B],[A|C],D,E) :- splitlist(B,C,D,E).
splitlist([A|B],[],A,B).

splitconj(((A,B),C),D,E,F) :- !, splitconj((A,B,C),D,E,F).
splitconj((A,B),AC,D,E) :- splitconj(B,C,D,E), cj(A,C,AC).
splitconj((A,B),true,A,B) :- !.
splitconj(A,true,A,true).

want_to_unfold(G) :- unfold(G).

unfold(A,[A]) :- builtin(A), !, A.
unfold(A,L) :- functor(A,P,Q), recorded(program,P/Q/AL), !,
               matches(AL,A,L1), L=L1.
unfold(_,[]).

matches(L,G,L1) :- matches1(L,G,L2), final_cut(L2,L1).

matches1([],_,[]) :- !.
matches1([(A:-B)|C],D,E) :- not unify(A,D), !, matches1(C,D,E).
matches1([(A:-!)|C],D,[(A:-true)]) :- instance(D,A), !.
matches1([(A:-!,B)|C],D,[(A:-B)]) :- instance(D,A), !.
matches1([(A:-B)|C],D,[(A:-B)|E]) :- matches1(C,D,E).

final_cut([],[]) :- !.
final_cut([(A:-!)],[(A:-true)]) :- !.
final_cut([(A:-!,B)],[(A:-B)]) :- !.
final_cut([A|L],[A|L1]) :- final_cut(L,L1).

cutcalls(L) :- not not (member((_:-T),L), hascuts(T)).

hascuts((A,B)) :- !, (hascuts(A); hascuts(B)).
hascuts(!).

suspendable(!) :- !.
suspendable(ineq(V,A,B)) :- !, not unfold_ineq(V,A,B).
suspendable(A~=B) :- !, not unfold_ineq([],A,B).
suspendable((not G)) :- !.
suspendable(F) :- not unfold(F).

unfold_ineq(V,A,B) :- A==B.
unfold_ineq(V,A,B) :- copy_term(V,V1), not (A=B, variant(V,V1)).

builtin(ineq(_,_,_)).
builtin(_ ~= _).

readprog(F) :-
   clear_declarations, open(F,read,input_file), open(temp,write,decs_file),
   readclauses(prog_mode), close(input_file), close(decs_file),
   compile(temp).

readclauses(prog_mode) :- read(input_file,X), X\==end_of_file, !,
                          clause_process(X,Mode), readclauses(Mode).
readclauses(decs_mode) :- read(input_file,X), X\==end_of_file, !,
                          write(decs_file,X), writeln(decs_file,"."),
                          readclauses(decs_mode).
readclauses(_).

clear_declarations :-
   open(temp,write,decs_file), writeln(decs_file,"open(dummy)."),
   close(decs_file), compile(temp).

clause_process(declarations,decs_mode) :- !.
clause_process((P:-T),prog_mode) :- !, addclause((P:-T)),
                                       program_clause(P,T).
clause_process(X,prog_mode) :- addclause((X:-true)),
                               program_clause(X,true).

program_clause(H,T) :- functor(H,F,A), recorded(program,F/A/L),
                       member((X:-T),L), unify(H,X).

show_program :- recorded(program,_/_/P), wrn(""), member(C,P),
                writeclau(C), fail.
show_program.

writeto(F) :- open(F,write,file), set_stream(result,file).

written :- set_stream(result,output), close(file).

res(F) :- writeto(F), show_program, written.

writeclau(C) :- writeclau1(C), fail.
writeclau(_).

writeclau1((H:-true)) :- !,
   writeclau1(H).
writeclau1((H:-T)) :- !,
   clarify((H:-T)), wr(H), wrn(" :-"), writebody(T), wrn(".").
writeclau1(H) :-
   clarify(H), wr(H), wrn(".").

writebody(((A,B),C)) :- !, writebody((A,B,C)).
writebody((A,B)) :- !, writebody(A), wrn(","), writebody(B).
writebody((A;B)) :- !, wr("("), writebody(A), wrn(";"), writebody(B), wr(")").
writebody(call(A)) :- !, writebody(A).
writebody(A) :- wr("   "), wr(A).

wr(X) :- write(result,X).
wrn(X) :- writeln(result,X).
wrc(X) :- not not (clarify(X), wr(X)).
wrnc(X) :- not not (clarify(X), wrn(X)).

clarify(T) :- clarify1(65,_,T).

clarify1(Ni,Ni,_) :- Ni>89, !.
clarify1(Ni,No,T) :- var(T), !, No is Ni+1, char_int(X,Ni), atom_string(T,X).
clarify1(Ni,No,T) :- T=..[_|L], clarify1l(Ni,No,L).

clarify1l(Ni,Ni,[]) :- !.
clarify1l(Ni,No,[H|T]) :- clarify1(Ni,Nt,H), clarify1l(Nt,No,T).

expand(_,_,_=[],_,[]).
expand(H,T1,G=[(G1:-T)|L],T2,[(Hc:-A)|B]) :-
   copy_term((H,T1,G,T2),(Hc,T1c,Gc,T2c)), G1=Gc,
   cj(T1c,T,T3), cj(T3,T2c,A), expand(H,T1,G=L,T2,B).

cj(true,X,X) :- !.
cj(X,true,X) :- !.
cj(X,Y,(X,Y)).

unify(A,B) :- var(A), !, unify1(A,B).
unify(A,B) :- var(B), !, unify2(B,A).
unify(A,B) :- A=..[F|LA], B=..[F|LB], unifyl(LA,LB).

unify1(A,B) :- var(B), !, A=B.
unify1(A,B) :- occurs(A,B), !, fail.
unify1(A,A).

unify2(A,B) :- occurs(A,B), !, fail.
unify2(A,A).

unifyl([],[]) :- !.
unifyl([A|LA],[B|LB]) :- unify(A,B), unifyl(LA,LB).

addclause((H:-T)) :- functor(H,F,A), erase_program(F/A/L),
                     append(L,[(H:-T)],L1), record(program,F/A/L1).

erase_program(F/A/L) :- erase(program,F/A/L), !.
erase_program(F/A/[]).

?- set_stream(result,output), set_flag(gc,on), set_flag(gc_interval,500000).
