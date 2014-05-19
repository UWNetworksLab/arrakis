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
% Version:	$Id: paddy.pl,v 1.2 2008/08/04 10:28:36 jschimpf Exp $
% ----------------------------------------------------------------------

%                   The PADDY system.

% PUT BINDING PROPAGATION IN POST-TRANSFORMATION!

:- module(paddy).

:- pragma(deprecated_warnings(off)).
:- pragma(undeclared_warnings(off)).
:- pragma(nowarnings).	% lots of singleton variables in this file!

:- local (help)/0.

:- local
   variable(bounds),
   variable(index),
   variable(clause_id),
   variable(source_files),
   variable(progsize),
   variable(temp),
   variable(many_patterns),
   variable(name_count),
   variable(prune),
   variable(pattern_count),
   variable(pointer),
   variable(predicate_size),
   variable(pattern_number),
   variable(term_depth).

:- import
      term_size/2, current_array_body/3, setval_body/3, getval_body/3,
      make_local_array_body/2, erase_array_body/2
   from sepia_kernel.

:- export 
      pin/1, p/0, p/1, p/2, pout/1, pout/0,
      term_depth/1, pattern_number/1, bounds/1.

:- dynamic
      temp_side/1, temp_prop/1, temp_head_pred/1, temp_delay_pred/1,
      temp_op/3, temp_dynamic_pred/1, temp_pd_predicate/1,
      temp_parallel_pred/1, deprolog_module/2, deprolog_file/1.

array_size(F,N,T) :-
   functor(Old, F, 1),
   (current_array(Old,_) ->
       erase_array(F/1)
   ;   true),
   X=..[F,N], make_local_array(X,T).

bounds :-
   setval(progsize,0), getval(bounds,N),
   array_size(prune,N,byte),
   array_size(auxdef,N,byte),
   array_size(transformed,N,byte),
   array_size(recursive,N,byte),
   array_size(side,N,byte),
   array_size(prop,N,byte),
   array_size(clau,N,prolog),
   array_size(prog,N,prolog),
   array_size(proc,N,prolog).

:- set_flag(print_depth,100), set_stream(divert,output),
   set_stream(log_output,null),
   ensure_loaded(library(lists)),
   set_stream(log_output,output),
   setval(term_depth,5), setval(pattern_number,100),
   setval(predicate_size,2000), setval(bounds,2000), bounds.

pattern_number(N) :-
   getval(pattern_number,N1), setval(pattern_number,N),
   write("   pattern_number changed from "), write(N1),
   write(" to "), writeln(N).

term_depth(N) :-
   getval(term_depth,N1), setval(term_depth,N),
   write("   term_depth changed from "), write(N1),
   write(" to "), writeln(N).

pin(In) :- deprolog(In), static_analysis.

p :- partial_deduction.

pout :- write_relevant_clauses.

pout(Out) :- write_relevant_clauses(Out).

p(In) :- pin(In), partial_deduction, write_relevant_clauses.

p(In,Out) :- pin(In), partial_deduction, write_relevant_clauses(Out).

write_relevant_clauses(Out) :-
   divert(Out),
   writeclause(divert,(?-((current_predicate(get_cut/1) ->
                              true
                          ;   import get_cut/1 from sepia_kernel)))),
   writeclause(divert,(?-((current_predicate(cut_to/1) ->
                              true
                          ;   import cut_to/1 from sepia_kernel)))),
   getval(source_files,SF), pathnames(SF,SF1), writeclause(divert,(?-SF1)),
   write_relevant_clauses,
   undivert.

pathnames([],[]).
pathnames([F|L],[F1|L1]) :-
   name_string(F,Fc),
   (substring(Fc,"/",1) ->
       F1=F, pathnames(L,L1)
   ;   get_flag(cwd,X), append_strings(X,Fc,F1), pathnames(L,L1)).

name_string(A,B) :-
   atom(A) ->
      atom_string(A,B)
  ;   B=A.

write_relevant_clauses :-
   not (relevant_pred(F,A,L), nl(divert),
        rmember(I,L), getval(clau(I),(H,T)),
        not writeclause(divert,(H:-T))).

bounds(N) :-
   getval(bounds,N1), setval(bounds,N), bounds,
   write("   Bounds changed from "), write(N1),
   write(" to "), writeln(N).

% Preprocessor (deprolog) for PADDY.

deprolog(In) :-
   deprolog_initialise,
   compile_term(cut_pred('0','0')),
   start_compile_stream(cut),
   clear_table(cut_table),
   clear_table(head_table),
   read_pd_file(In),
   clear_table(head_table),
   clear_table(cut_table),
   end_compile_stream(cut),
   make_static(temp_dynamic_pred(P),dynamic_pred(P)),
   make_static(temp_delay_pred(P),delay_pred(P)),
   make_static(temp_parallel_pred(P),parallel_pred(P)),
   drop_ops,
   add_cut_args,
   make_static(temp_head_pred(X),head_pred(X)),
   store_prog.

deprolog_initialise :-
   retract_all(temp_head_pred(_)), retract_all(deprolog_module(_,_)),
   retract_all(temp_dynamic_pred(_)), retract_all(temp_pd_predicate(_)),
   retract_all(temp_parallel_pred(_)),
   clear_table(index_table), setval(clause_id,0), setval(index,0),
   retract_all(temp_delay_pred(_)), retract_all(temp_op(_,_,_)),
   retract_all(deprolog_file(_)).

read_pd_file(In) :-
   exists(In) ->
      open(In,read,S), read(S,X),
      ((X=(?-L); X=(:-L)) ->
         setval(source_files,L),
         read_prolog(L), read_pd_clauses(S), close(S),
         make_static(temp_pd_predicate(P),pd_predicate(P))
      ;  write("   PADDY ERROR: file "), write(In),
         writeln(" must begin with `:-[..]'"), abort)
  ;   write("   PADDY ERROR: query file "), write(In),
      writeln(" does not exist"), abort.

read_pd_clauses(S) :-
   read(S,X),
   (X==end_of_file ->
      true
   ;X=(H:-T), functor(H,F,A), concat_atom([F,'_',A],FA) ->
      (table_entry(FA,head_table) ->
         write("   PADDY ERROR: "), write(F/A),
         writeln(" already occurred in the program"), abort
      ;  true),
      (temp_pd_predicate(H) ->
         write("   PADDY ERROR: "), write(F/A),
         writeln(" has more than one clause"), abort
      ;  true),
      (pure_conjunction(T) ->
         true
      ;  writeln("   PADDY ERROR: "), writeclause((H:-T)),
         writeln(" is not a valid query clause"), abort),
      incval(index), getval(index,Ind), check_bounds(Ind),
      setval(proc(Ind),[]), predicate_key(H,Hk),
      write_table(Hk,Ind,index_table), addclause(Ind,(H:-T)),
      functor(G,F,A), assert(temp_pd_predicate(G)),
      assert(temp_head_pred(G)), write_table(FA,'0',head_table),
      read_pd_clauses(S)
   ;  writeln("   PADDY ERROR: "), writeclause(X),
      writeln(" is not a valid query clause"), abort).

pure_conjunction((A,B)) :- !, pure_conjunction(A), pure_conjunction(B).
pure_conjunction(A) :- A\=(_;_), A\=(not _), A\=once(_), A\=(_->_), A\==!.

read_prolog([]) :- !.
read_prolog([H|T]) :- !,
   read_prolog(H), read_prolog(T).
read_prolog(F) :-
   exists(F), !, write("   Enter file "), writeln(F),
   asserta(deprolog_file(F)), open(F,read,S), read_prolog_clause(S),
   close(S), retract(deprolog_file(F)), write("   Exit file "), writeln(F).
read_prolog(F) :-
   term_string(F,FS), append_strings(FS,".pl",FSPL), exists(FSPL), !,
   write("   Reading "), writeln(FSPL), asserta(deprolog_file(F)),
   open(FSPL,read,S), read_prolog_clause(S),
   close(S), retract(deprolog_file(F)), write("   Exit file "), writeln(F).
read_prolog(F) :-
   write("   PADDY warning: could not find file "), writeln(F).

read_prolog_clause(S) :-
   read(S,X),
   (X==end_of_file ->
      deprolog_file(F),
      (retract(deprolog_module(M,F)) ->
         write("   Skipped module "), writeln(M)
      ;  true)
   ;  prolog_clause_analyse(X), read_prolog_clause(S)).

prolog_clause_analyse((:-X)) :- !, dec_process((?-X)).
prolog_clause_analyse((?-X)) :- !, dec_process((?-X)).
prolog_clause_analyse((delay P if C)) :- !, assert(temp_delay_pred(P)). % DUPS?
prolog_clause_analyse(_) :- deprolog_module(_,_), !.
prolog_clause_analyse((H:-T)) :- !, prolog_clause_process(H,T).
prolog_clause_analyse(X) :- prolog_clause_process(X,true).

dec_process((?-A,B)) :- not (varof(V,A), occurs(V,B)), !,
                        dec_process((?-A)), dec_process((?-B)).
dec_process((?-compile(F))) :- !,
   (nonvar(F), exists(F) ->
      read_prolog(F)
   ;nonvar(F), term_string(F,FS),
    append_strings(FS,".pl",FSPL), exists(FSPL) ->
      read_prolog(FSPL)
   ;  write("   PADDY warning: could not find file "),
      writeln(F)).
dec_process((?-[A,B|T])) :- !, dec_process((?-compile(A))),
                               dec_process((?-[B|T])).
dec_process((?-[A])) :- !, dec_process((?-compile(A))).
dec_process((?-op(P,A,N))) :- !, assert(temp_op(P,A,N)), op(P,A,N).
dec_process((?-local_op(P,A,N))) :- !, assert(temp_op(P,A,N)), op(P,A,N).
dec_process((?-global_op(P,A,N))) :- !, assert(temp_op(P,A,N)), op(P,A,N).
dec_process((?-dynamic Spec)) :- !,
   not (extract_atom(F/A,Spec), functor(P,F,A),
        not assert(temp_dynamic_pred(P))).
dec_process((?-parallel Spec)) :- !,
   not (extract_atom(F/A,Spec), functor(P,F,A),
        not assert(temp_parallel_pred(P))).
dec_process((?-module(M))) :- !,
   deprolog_file(F),
   (retract(deprolog_module(M1,F)) ->
      write("   Skipped module "), writeln(M1)
   ;  true),
   asserta(deprolog_module(M,F)),
   write("   Skipping module "), writeln(M).
dec_process((?-G)).

prolog_clause_process(H,T) :-
   functor(H,HF,HA), concat_atom([HF,'_',HA],FA),
   (table_entry(FA,head_table) ->
      true
   ;  functor(H1,HF,HA), assert(temp_head_pred(H1)),
      write_table(FA,'0',head_table)),
   prolog_body_process(HF/HA,T,T1), predicate_key(H,Hk),
   (read_table(Hk,Ind,index_table) ->
      addclause(Ind,(H:-T1))
   ;  incval(index), getval(index,Ind), check_bounds(Ind),
      setval(proc(Ind),[]), write_table(Hk,Ind,index_table),
      addclause(Ind,(H:-T1))).

prolog_body_process(_,G,P) :-
   var(G), !, P=call(G).
prolog_body_process(HF/HA,call(A),P) :-
   !, prolog_body_process(HF/HA,A,P).
prolog_body_process(HF/HA,(A->B;C),P) :-
   !, cond_process(HF/HA,(A->B;C),K,D),
   prolog_body_process(HF/HA,(get_cut(K),D),P).
prolog_body_process(HF/HA,(A->B),P) :-
   !, cond_process(HF/HA,(A->B),K,D),
   prolog_body_process(HF/HA,(get_cut(K),D),P).
prolog_body_process(HF/HA,(A;B),(C;D)) :-
   !, prolog_body_process(HF/HA,A,C), prolog_body_process(HF/HA,B,D).
prolog_body_process(HF/HA,(A,B),(C,D)) :-
   !, prolog_body_process(HF/HA,A,C), prolog_body_process(HF/HA,B,D).
prolog_body_process(HF/HA,(not A),P) :-
   !, prolog_body_process(HF/HA,(get_cut(C),(A,cut_to(C),fail;true)),P).
prolog_body_process(HF/HA,once(A),P) :-
   !, prolog_body_process(HF/HA,(get_cut(C),A,cut_to(C)),P).
prolog_body_process(HF/HA,!,!) :-
   !, concat_atom([HF,'_',HA],FA),
   (table_entry(FA,cut_table) ->
      true
   ;  write_table(FA,'0',cut_table),
      stream_compile_term(cut,cut_pred(HF,HA))).
prolog_body_process(HF/HA,(if A then B else C),(if D then E else F)) :-
   !, prolog_body_process(HF/HA,A,D), prolog_body_process(HF/HA,B,E),
   prolog_body_process(HF/HA,C,F).
prolog_body_process(_,G,G).

cond_process(HF/HA,X,K,X) :- var(X), !.
cond_process(HF/HA,(A->B;C),K,(A,cut_to(K),B;Z)) :- !,
   cond_process(HF/HA,C,K,Z).
cond_process(HF/HA,(A->B),K,(A,cut_to(K),B)) :- !.
cond_process(HF/HA,!,K,!) :- !,
   concat_atom([HF,'_',HA],FA),
   (table_entry(FA,cut_table) ->
      true
   ;  write_table(FA,'0',cut_table),
      stream_compile_term(cut,cut_pred(HF,HA))).
cond_process(HF/HA,X,K,X).

drop_ops :-
   not (retract(temp_op(P,A,N)), current_op(P,A,N), not abolish_op(N,A)).

add_cut_args :-
   not (cut_pred(F,A), F/A\=='0'/'0', functor(G,F,A),
        not delay_pred(G), not dynamic_pred(G), not parallel_pred(G),
        not (incval(index), getval(index,IndC),
             G=..[F|Z], append(Z,[C],ZC), concat_atom([F,'_',cut],FC),
             GC=..[FC|ZC], assert(temp_head_pred(GC)),
             predicate_key(G,Gk), read_table(Gk,Ind,index_table),
             predicate_key(GC,GCk), write_table(GCk,IndC,index_table),
             getval(proc(Ind),L), setval(proc(IndC),L), setval(proc(Ind),[]),
             addclause(Ind,(G:-get_cut(C),GC)),
             add_cut_tos(L,FC))).

add_cut_tos([],HF1).
add_cut_tos([I|L],HF1) :-
   getval(clau(I),(H,T)), H=..[_|X], append(X,[C],X1),
   H1=..[HF1|X1], add_cut_tos_body(C,T,T1),
   setval(clau(I),(H1,T1)), add_cut_tos(L,HF1).

add_cut_tos_body(C,(A,B),(X,Y)) :- !,
   add_cut_tos_body(C,A,X), add_cut_tos_body(C,B,Y).
add_cut_tos_body(C,(A;B),(X;Y)) :- !,
   add_cut_tos_body(C,A,X), add_cut_tos_body(C,B,Y).
add_cut_tos_body(C,!,cut_to(C)) :- !.
add_cut_tos_body(C,T,T).

store_prog :-
   setval(progsize,0),
   not (head_pred(P), predicate_key(P,Pk),
        read_table(Pk,Ind,index_table), getval(proc(Ind),L),
        rmember(I,L), getval(clau(I),(H,T)),
        incval(progsize), getval(progsize,PS),
        not setval(prog(PS),(H,T))).

% Static Analyser for PADDY

static_analysis :-
   static_initialise,
   setup_callgraph,
   setup_symbols,
   setup_body_preds,
   setup_side_preds,
   setup_prop_preds,
   setup_rec_analysis,
   make_name_table.

make_name_table :-
   clear_table(name_table),
   not ((head_pred(G); body_pred(G), not head_pred(G)),
        functor(G,F,_), not write_table(F,'0',name_table)).

static_initialise :-
   retract_all(temp_side(_)), retract_all(temp_prop(_)).

setup_callgraph :-
   compile_term(calls('0','0')),
   start_compile_stream(comp),
   clear_table(call_table),
   not (head_pred(H), calls_atom(H,G), head_pred(G),
        functor(G,X,Y), functor(H,F,A),
        concat_atom([F,/,A,/,X,/,Y],FAXY),
        not table_entry(FAXY,call_table),
        not (stream_compile_term(comp,calls(F/A,X/Y)),
             write_table(FAXY,'0',call_table))),
   clear_table(call_table),
   end_compile_stream(comp).

setup_symbols :-
   start_compile_stream(comp),
   clear_table(symbol_table),
   stream_compile_term(comp,symbol(true)),
   stream_compile_term(comp,symbol(_=_)),
   write_table('true/0','0',symbol_table),
   write_table('=/2','0',symbol_table),
   not (head_pred(H), predicate_key(H,Hk),
        read_table(Hk,Ind,index_table),
        getval(proc(Ind),L), member(I,L), getval(clau(I),C),
        extract_atom(A,C), not symbols(A)),
   clear_table(symbol_table),
   end_compile_stream(comp).

symbols(T) :-
   var(T) ->
      true
  ;number(T) ->
      true
  ;string(T) ->
      true
  ;   functor(T,F,A), concat_atom([F,'_',A],FA),
      (table_entry(FA,symbol_table) ->
         true
      ;  functor(T1,F,A), stream_compile_term(comp,symbol(T1)),
         write_table(FA,'0',symbol_table)),
      symbols(A,T).

symbols(N,T) :-
   N==0 ->
      true
  ;   arg(N,T,X), symbols(X), M is N-1, symbols(M,T).

setup_side_preds :-
   clear_table(side_table),
   not (head_pred(G), once((calls_atom(G,X), side_atom(X))),
        not (predicate_key(G,Gk), write_table(Gk,'0',side_table),
             assert(temp_side(G)))),
   propagate_side,
   make_static(temp_side(S),side(S)),
   clear_table(side_table).

setup_prop_preds :-
   clear_table(prop_table),
   not (head_pred(G), once((calls_atom(G,X), prop_atom(X))),
        not (predicate_key(G,Gk), write_table(Gk,'0',prop_table),
             assert(temp_prop(G)))),
   propagate_prop,
   make_static(temp_prop(S),prop(S)),
   clear_table(prop_table).

propagate_side :-
   setval(temp,no),
   not (temp_side(G), functor(G,F,A), calls(F1/A1,F/A),
        functor(X,F1,A1), concat_atom([F1,'_',A1],FA1),
        not table_entry(FA1,side_table),
        not (setval(temp,yes), write_table(FA1,'0',side_table),
             assert(temp_side(X)))),
   getval(temp,yes), !,
   propagate_side.
propagate_side.

propagate_prop :-
   setval(temp,no),
   not (temp_prop(G), functor(G,F,A), calls(F1/A1,F/A),
        functor(X,F1,A1), concat_atom([F1,'_',A1],FA1),
        not table_entry(FA1,prop_table),
        not (setval(temp,yes), write_table(FA1,'0',prop_table),
             assert(temp_prop(X)))),
   getval(temp,yes), !,
   propagate_prop.
propagate_prop.

calls_atom(G,X) :-
   predicate_key(G,Gk), read_table(Gk,Ind,index_table),
   getval(proc(Ind),L), rmember(Id,L), getval(clau(Id),(_,T)),
   extract_atom(X,T).

side_atom(G) :- var(G), !.
side_atom(call(X)) :- !, side_atom(X).
side_atom(G) :- not head_pred(G), not open_no_side(G).

prop_atom(G) :- var(G), !.
prop_atom(call(X)) :- !, prop_atom(X).
prop_atom(G) :- not head_pred(G), not open_no_prop(G).

setup_body_preds :-
   start_compile_stream(body),
   clear_table(body_table),
   not (head_pred(P), not setup_body_pred(P)),
   clear_table(body_table),
   end_compile_stream(body).

setup_body_pred(P) :-
   predicate_key(P,Pk), read_table(Pk,Ind,index_table), getval(proc(Ind),L),
   not (member(Id,L), getval(clau(Id),(_,T)), extract_atom(A,T),
        predicate_key(A,Ak), not setup_body_atom(A,Ak)).

setup_body_atom(A,Ak) :-
   table_entry(Ak,body_table) ->
      true
  ;   write_table(Ak,'0',body_table),
      functor(A,X,Y), functor(A1,X,Y),
      stream_compile_term(body,body_pred(A1)).

setup_rec_analysis :-
   compile_term(non_recursive('0')),
   clear_table(nonrec_table),
   start_compile_stream(nonrec),
   not (head_pred(H), functor(H,F,A),
        not (calls(F/A,X/Y), functor(Z,X,Y), head_pred(Z)),
        not (stream_compile_term(nonrec,non_recursive(H)),
             concat_atom([F,'_',A],FA), write_table(FA,'0',nonrec_table))),
   propagate_nonrec,
   end_compile_stream(nonrec),
   clear_table(nonrec_table).

propagate_nonrec :-
   setval(temp,no),
   not (head_pred(H), functor(H,F,A),
        concat_atom([F,'_',A],FA), not table_entry(FA,nonrec_table),
        not (calls(F/A,X/Y), concat_atom([X,'_',Y],XY),
             not table_entry(XY,nonrec_table)),
        not (setval(temp,yes),
             write_table(FA,'0',nonrec_table),
             stream_compile_term(nonrec,non_recursive(H)))),
   getval(temp,yes), !,
   propagate_nonrec.
propagate_nonrec.

% Partial deduction phase

partial_deduction :-
   pd_initialise,
   cputime(T1),
   main_transformation,
   post_transformation,
   cputime(T2), T is T2-T1,
   write("   Transformation took "),
   write(T), writeln(" seconds").

% Initialisation

pd_initialise :-
   clear_table(index_table), clear_table(defn_table),
   clear_table(aux_table),
   setval(many_patterns,false), setval(clause_id,0),
   setval(name_count,0), setval(index,0), setval(prune,0),
   setval(pattern_count,0),
   getval(progsize,S), setval(pointer,0), check_program_exists(S),
   pd_readclauses(S).

check_program_exists(S) :-
   S==0 ->
      writeln("   PADDY ERROR: empty program"), abort
  ;   true.

pd_readclauses(S) :-
   incval(pointer), getval(pointer,P),
   (P>S ->
      true
   ;  getval(prog(P),(H,T)), once(pd_clause_process(H,T)), pd_readclauses(S)).

pd_clause_process(H,T) :-
   pd_body_process(T,T1), predicate_key(H,Hk),
   (read_table(Hk,Ind,index_table) ->
      addclause(Ind,(H:-T1))
   ;  incval(index), getval(index,Ind), check_bounds(Ind),
      setval(proc(Ind),[]), write_table(Hk,Ind,index_table),
      addclause(Ind,(H:-T1))).

pd_body_process((A,B),(C,D)) :-
   !, pd_body_process(A,C), pd_body_process(B,D).
pd_body_process((A;B),(C;D)) :-
   !, pd_body_process(A,C), pd_body_process(B,D).
pd_body_process(call(A),call(A)) :-
   var(A), !.
pd_body_process(call(A),P) :-
   !, pd_body_process(A,P).
pd_body_process(G,G).

% Transformation

main_transformation :-
   not (pd_predicate(I), functor(I,F,A), write("   Goal "), writeln(F/A),
        predicate_key(I,Ik), read_table(Ik,II,index_table),
        getval(proc(II),[Id]), setval(proc(II),[]), getval(clau(Id),(I,G)),
        not transform(II,i,I,G)).

transform(II,IU,I,G) :-
   setval(transformed(II),0), first_rest(G,F,R),
   unfold(II,IU,(I:-F,R),no_prune), fail
  ;set_properties(I,II).

set_properties(I,Ind) :-
   setval(transformed(Ind),1), functor(I,F,A),
   getval(proc(Ind),L), recursive_class(L,F,A,Ind),
   improve_side_class(L,F,A,Ind), improve_prop_class(L,F,A,Ind).

recursive_class(L,F,A,Ind) :-
   risky_recursion(F,A,L) ->
      setval(recursive(Ind),2)
  ;direct_recursion(L,F,A) ->
      setval(recursive(Ind),1)
  ;   setval(recursive(Ind),0).

risky_recursion(F,A,L) :-
   member(Id,L), getval(clau(Id),(_,T)), extract_atom(G,T),
   not functor(G,F,A), predicate_key(G,Gk), read_table(Gk,Ind,index_table),
   (getval(transformed(Ind),0); getval(recursive(Ind),2)).

direct_recursion(L,F,A) :-
   member(Id,L), getval(clau(Id),(_,T)), extract_atom(G,T),
   functor(G,F,A).

improve_side_class(L,F,A,Ind) :-
   getval(side(Ind),1),
   not (rmember(Id,L), getval(clau(Id),(_,G)),
        extract_atom(X,G), not functor(X,F,A), may_be_side(X)) ->
      setval(side(Ind),0)
  ;   true.

improve_prop_class(L,F,A,Ind) :-
   getval(prop(Ind),1),
   not (rmember(Id,L), getval(clau(Id),(_,G)),
        extract_atom(X,G), not functor(X,F,A), may_be_prop(X)) ->
      setval(prop(Ind),0)
  ;   true.

unfold(II,IU,(I:-F,R),Prune) :-
   F==true, R==true ->
      addclause(II,(I:-true))
  ;head_pred(F), not dynamic_pred(F),
   not parallel_pred(F), not delay_pred(F) ->
      (IU==u, not non_recursive(F) ->
         make_fold(F,Ff,Indf), may_reunfold_def(Indf,Ff,R,F1,R1),
         unfold(II,u,(I:-F1,R1),no_prune)
      ;  predicate_key(F,Fk), read_table(Fk,Ind,index_table),
         step(F,Ind,R,RF,RR,NewPrune), unfold(II,u,(I:-RF,RR),NewPrune))
  ;F=call(G), nonvar(G) ->
      unfold(II,IU,(I:-G,R),Prune)
  ;executable_open(F) ->
      execute_open(F), first_rest(R,RF,RR),
      unfold(II,u,(I:-RF,RR),Prune)
  ;F=(F1;F2) ->
      (first_rest1(F1,R,Fd,Rd); first_rest1(F2,R,Fd,Rd)),
      unfold(II,IU,(I:-Fd,Rd),Prune)
  ;nonrecursive_auxdef(F,Ind) ->
      step(F,Ind,R,RF,RR,NewPrune), unfold(II,u,(I:-RF,RR),NewPrune)
  ;   may_prune(F,Prune,I), new_aux(I1,R,I,F,Ind1),
      may_reunfold_aux(II,Ind1,I,F,I1).

nonrecursive_auxdef(F,Ind) :-
   not head_pred(F), predicate_key(F,Fk), read_table(Fk,Ind,index_table),
   getval(transformed(Ind),1), getval(recursive(Ind),0).

make_fold(F,Ff,Ind1) :-
   pattern_key(F,Sk),
   (read_table(Sk,[Ff1,F1,Ind,N1],defn_table) ->
      (instance(F,F1) ->
         Ff=Ff1, F=F1, Ind1=Ind
      ;term_size(F,N), N<N1 ->
         choose_fold(Sk,F,F,Ff,Ind1)
      ;  generalise(F,F1,G), choose_fold(Sk,G,F,Ff,Ind1))
   ;  choose_fold(Sk,F,F,Ff,Ind1)).

choose_fold(Sk,Fg,F,Fold,Ind) :-
   new_def(Fg,Sk,Foldg,Indg), transform(Indg,i,Foldg,Fg),
   (getval(recursive(Indg),0) ->
      Fold=Foldg, F=Fg, Ind=Indg
   ;read_table(Sk,[Foldr,Fr,Indr,_],defn_table), instance(F,Fr) ->
      Fold=Foldr, F=Fr, Ind=Indr
   ;  Fold=Foldg, F=Fg, Ind=Indg).

new_aux(I1,R,I,F,Ind) :-
   aux_key(R,Rk),
   (read_table(Rk,[I1,R1,Ind],aux_table), getval(transformed(Ind),1),
    variant(R,R1), R=R1, internal_check(R,I1,I+F) ->
      true
   ;  newpred(I1,R,I+F), incval(index), getval(index,Ind),
      check_bounds(Ind), predicate_key(I1,I1k),
      write_table(I1k,Ind,index_table), setval(proc(Ind),[]),
      setval(auxdef(Ind),0), tentative_side_class(Ind,R),
      tentative_prop_class(Ind,R),
      write_table(Rk,[I1,R,Ind],aux_table), transform(Ind,u,I1,R)).

aux_key(A,B) :-
   getval(term_depth,N), aux_key(A,C,N),
   (term_size(C,CS), getval(predicate_size,PS), CS>PS ->
      B='0'
   ;  term_string(C,D), atom_string(B,D)).

aux_key(((A,B),C),D,N) :- !, aux_key((A,B,C),D,N).
aux_key((A,B),(C,D),N) :- !, sk(N,A,C), aux_key(B,D,N).
aux_key(((A;B);C),D,N) :- !, aux_key((A;B;C),D,N).
aux_key((A;B),(C;D),N) :- !, sk(N,A,C), aux_key(B,D,N).
aux_key(A,B,N) :- sk(N,A,B).

may_reunfold_aux(II,Ind,I,F,I1) :-
   getval(proc(Ind),L), may_reunfold_aux(L,II,Ind,I,F,I1).

may_reunfold_aux([],II,Ind,I,F,I1) :- !,
   may_be_side(F), cj(F,fail,F1), addclause(II,(I:-F1)).
may_reunfold_aux([Id],II,Ind,I,F,I1) :- !,
   getval(clau(Id),(I1c,T)),
   (may_be_prop(F) ->
      unifier(I1,I1c,E), cj(E,T,ET), cj(F,ET,F1), addclause(II,(I:-F1))
   ;  I1=I1c, cj(F,T,FT), addclause(II,(I:-FT))).
may_reunfold_aux(L,II,Ind,I,F,I1) :-
   cj(F,I1,F1), addclause(II,(I:-F1)).

calls_cut_to(Id,K,I) :-
   getval(clau(Id),(I,T)),
   (first_rest(T,cut_to(_),T1) ->
      true
   ;  T1=T),
   extract_atom(cut_to(V),T1), V==K.

unifier(A,B,E) :- unifier(A,B,[],E1), list_to_tuple(E1,E).

unifier(A,B,Ei,Eo) :-
   var(A) ->
      (member(X=Y,Ei), (X==A; Y==A) ->
         Eo=Ei
      ;  Eo=[A=B|Ei])
  ;var(B) ->
      (member(X=Y,Ei), (X==B; Y==B) ->
         Eo=Ei
      ;  Eo=[B=A|Ei])
  ;   A=..[FA|LA], B=..[FB|LB],
      (FA==FB ->
         unifierl(LA,LB,Ei,Eo)
      ;  Eo=[fail]).

unifierl(LA,LB,Ei,Eo) :-
   LA==[] ->
      (LB==[] ->
         Eo=Ei
      ;  Eo=[fail])
  ;LB==[] ->
      Eo=[fail]
  ;   LA=[A|RA], LB=[B|RB],
      unifier(A,B,Ei,Et), unifierl(RA,RB,Et,Eo).

list_to_tuple(L,T) :-
   L=[] ->
      T=true
  ;L=[X|Y] ->
      list_to_tuple(Y,A), eqjoin(X,A,T).

eqjoin(A,B,C) :-
   A==fail ->
      C=fail
  ;B==fail ->
      C=fail
  ;   C=(A,B).

internal_check(R1,I1,Rest) :-
   not (varof(V,R1), not occurs(V,I1), occurs(V,Rest)).

new_def(G,Sk,Ff,Ind) :-
   newpred(Ff,G), incval(index), getval(index,Ind),
   check_bounds(Ind), predicate_key(Ff,Ffk), setval(auxdef(Ind),1),
   write_table(Ffk,Ind,index_table), setval(proc(Ind),[]),
   tentative_side_class(Ind,G), tentative_prop_class(Ind,G),
   term_size(G,N), write_table(Sk,[Ff,G,Ind,N],defn_table),
   check_pattern_number.

check_pattern_number :-
   incval(pattern_count), getval(pattern_count,N),
   getval(pattern_number,M), getval(term_depth,D),
   (N==M, D>1 ->
      setval(many_patterns,true),
      nl, writeln("   PADDY warning: pattern number exceeded")
  ;   true).

step(F,Ind,R,RF,RR,NewPrune) :-
   getval(proc(Ind),L), setup_prune_point(L,NewPrune),
   select_clause(NewPrune,Id,L), getval(clau(Id),(F,U)),
   first_rest1(U,R,RF,RR).

setup_prune_point([],no_prune).
setup_prune_point([_],no_prune).
setup_prune_point([_,_|_],NewPrune) :-
   incval(prune), getval(prune,NewPrune),
   check_bounds(NewPrune), setval(prune(NewPrune),0).

may_prune(_,no_prune,_) :- !.
may_prune(cut_to(_),Prune,I) :- most_general(I), !, setval(prune(Prune),1).
may_prune(_,_,_).

select_clause(Prune,Id,[_|L]) :- select_clause(Prune,Id,L).
select_clause(no_prune,Id,[Id|_]) :- !.
select_clause(Prune,Id,[Id|_]) :- getval(prune(Prune),0).

may_reunfold_def(Ind,Ff,R,F1,R1) :-
   getval(transformed(Ind),1), getval(proc(Ind),[Id]) ->
      getval(clau(Id),(Ff,T)), first_rest1(T,R,F1,R1)
  ;   F1=Ff, R1=R.

% Post-transformation

post_transformation :-
   find_relevant_code, expand_dets, find_relevant_code,
   drop_equals, cut_args, cut_delabelling, last_cut_deletion,
   setup_trim, cut_reductions, add_once.

find_relevant_code :-
   compile_term(relevant_pred('0','0','0')),
   start_compile_stream(relevant),
   clear_table(relevant),
   not (pd_predicate(G), functor(G,F,A), concat_atom([F,'_',A],FA),
        not table_entry(FA,relevant), read_table(FA,Ind,index_table),
        not find_relevant(FA,F/A,Ind)),
   clear_table(relevant),
   end_compile_stream(relevant).

find_relevant(FA,F/A,Ind) :-
    getval(proc(Ind),L), L\==[] ->
      write_table(FA,'0',relevant),
      stream_compile_term(relevant,relevant_pred(F,A,L)), relevant_list(L)
   ;  true.

relevant_list([]).
relevant_list([I|L]) :-
   getval(clau(I),(_,T)), relevant_body(T), relevant_list(L).

relevant_body(T) :-
   not (extract_atom(P,T), not body_pred(P), not head_pred(P),
        predicate_key(P,Pk), read_table(Pk,Ind,index_table), functor(P,F,A),
        concat_atom([F,'_',A],FA), not table_entry(FA,relevant),
        not find_relevant(FA,F/A,Ind)).

expand_dets :-
   not (relevant_pred(F,A,L), member(Id,L), getval(clau(Id),(H,T)),
        not (expand_dets(H,T,T1), setval(clau(Id),(H,T1)))).

expand_dets(H,((A,B),C),T) :-
   !, expand_dets(H,(A,B,C),T).
expand_dets(H,(A,B),(E,TX)) :-
   predicate_key(A,Ak), read_table(Ak,Ind,index_table),
   getval(proc(Ind),[Id]), !,
   getval(clau(Id),(P,T)), expand_dets(H,B,X), cj(T,X,TX), unifier(A,P,E).
expand_dets(H,(A,B),AT) :-
   !, expand_dets(H,B,T), cj(A,T,AT).
expand_dets(H,A,(E,T)) :-
   predicate_key(A,Ak), read_table(Ak,Ind,index_table),
   getval(proc(Ind),[Id]), !,
   getval(clau(Id),(P,T)), unifier(A,P,E).
expand_dets(H,A,A).

add_once :-
   not (relevant_pred(F,A,L), member(Id,L), getval(clau(Id),(H,T)),
        split5(T,TL,get_cut(K),TM,cut_to(K1),TR), K==K1,
        not (cj(TL,once(TM),LM), cj(LM,TR,LMR), setval(clau(Id),(H,LMR)))).

split5(T,A,B,C,D,E) :-
   split(T,A,T1),
   split(T1,B,T2), B\==true,
   split(T2,C,T3), C\==true,
   split(T3,D,E), D\==true.

split(T,L,R) :-
   split2(T,L,R)
  ;L=T, R=true.

split2(T,L,R) :-
   T=((A,B),C) ->
      split2((A,B,C),L,R)
  ;   (L=true, R=T
      ;T=(H,S), split2(S,X,R), cj(H,X,L)).

drop_equals :-
   not (relevant_pred(F,A,L), member(Id,L), getval(clau(Id),(H,T)),
        not drop_equals(Id,H,true,T,no)).

drop_equals(Id,H,X,A,Z) :-
   A==true ->
      (Z==yes ->
         setval(clau(Id),(H,X))
      ;  true)
  ;   first_rest(A,F,R),
      (F=(P=Q), safe_equals(P=Q,(H,X)) ->
         P=Q, drop_equals(Id,H,X,R,yes)
      ;  cj(X,F,XF), drop_equals(Id,H,XF,R,Z)).

% could further weaken safe_equals

safe_equals(P=Q,T) :-
   not P\=Q,
   (not not (copy_term(T,T1), P=Q, variant(T,T1)) ->
      true
   ;  not (extract_atom(A,T), may_be_prop(A))).

cut_args :-
   compile_term([cut_arg('0','0','0'),no_cut_arg('0','0')]),
   start_compile_stream(cut_arg),
   start_compile_stream(no_cut_arg),
   clear_table(cut_arg),
   clear_table(no_cut_arg),
   not (relevant_pred(_,_,L), rmember(Id,L), getval(clau(Id),(_,T)),
        pair(T,A,B), functor(B,F,M), relevant_pred(F,M,_),
        concat_atom([F,'_',M],FM), not table_entry(FM,no_cut_arg),
        not new_cut_arg(A,B,F,M,FM)),
   clear_table(no_cut_arg),
   clear_table(cut_arg),
   end_compile_stream(no_cut_arg),
   end_compile_stream(cut_arg).

new_cut_arg(A,B,F,M,FM) :-
   A=get_cut(K), interval(1,N,M), arg(N,B,V), V==K ->
     (read_table(FM,N1,cut_arg), N\==N1 ->
        write_table(FM,'0',no_cut_arg),
        stream_compile_term(no_cut_arg,no_cut_arg(F,M))
     ;read_table(FM,N,cut_arg) ->
        true
     ;  write_table(FM,N,cut_arg),
        stream_compile_term(cut_arg,cut_arg(F,M,N)))
  ;  write_table(FM,'0',no_cut_arg),
     stream_compile_term(no_cut_arg,no_cut_arg(F,M)).

cut_delabelling :-
   not (cut_arg(F,M,N), (F,M,N)\==('0','0','0'),
        not no_cut_arg(F,M), relevant_pred(F,M,L), rmember(Id,L),
        not (getval(clau(Id),(H,T)), arg(N,H,K),
             cut_delabel(T,K,T1), setval(clau(Id),(H,T1)))).

cut_delabel((A;B),K,(X;Y)) :- !, cut_delabel(A,K,X), cut_delabel(B,K,Y).
cut_delabel((A,B),K,(X,Y)) :- !, cut_delabel(A,K,X), cut_delabel(B,K,Y).
cut_delabel(cut_to(V),K,!) :- V==K, !.
cut_delabel(X,_,X).

pair(A,X,Y) :- pairtail((true,A),X,R), pairhead(R,Y).

pairtail(((A,B),C),X,R) :- !,
   pairtail((A,B,C),X,R).
pairtail(((A;B),C),X,R) :- !,
   pairhead(C,F),
   (pairtail((A,F),X,R); pairtail((B,F),X,R); pairtail(C,X,R)).
pairtail((A,B),X,Y) :- !,
   (X=A, Y=B; pairtail(B,X,Y)).
pairtail((A;B),X,R) :-
   (pairtail(A,X,R); pairtail(B,X,R)).

pairhead((A,B),F) :- !,
   pairhead(A,F).
pairhead((A;B),F) :- !,
   (pairhead(A,F); pairhead(B,F)).
pairhead(F,F).

last_cut_deletion :-
   not (relevant_pred(F,A,[Id|L]), getval(clau(Id),(H,T)),
        first_rest(T,!,R), not setval(clau(Id),(H,R))).

cut_reductions :-
   not (relevant_pred(F,A,L), rmember(Id,L),
        not (getval(clau(Id),(H,T)), trim_body((H,T),(Hd,Td)),
             tidy(Td,T1), setval(clau(Id),(Hd,T1)))).

setup_trim :-
   clear_table(redarg_table),
   repeat, setval(temp,no),
   setup_trim_loop,
   getval(temp,no), !.

setup_trim_loop :-
   not (relevant_pred(F,A,L), F\=='0',
        nlist(A,[],Ri), trim_intersect(L,Ri,Ro), Ro\==[],
        functor(P,F,A), concat_atom([F,'_',A],Pk),
        (read_table(Pk,Ro1,redarg_table) ->
           Ro1\==Ro
        ;  true),
        setval(temp,yes), not write_table(Pk,Ro,redarg_table)).

trim_intersect([],Ri,Ri) :- !.
trim_intersect(_,[],[]) :- !.
trim_intersect([I|L],Ri,Ro) :-
   getval(clau(I),(H,T)), redargs(Ri,H,T,Rt), trim_intersect(L,Rt,Ro).

trim_body((A,B),(X,Y)) :- !, trim_body(A,X), trim_body(B,Y).
trim_body((A;B),(X;Y)) :- !, trim_body(A,X), trim_body(B,Y).
trim_body(G,G) :- pd_predicate(G), !.
trim_body(G,D) :- predicate_key(G,Gk), read_table(Gk,L,redarg_table),
                  L\==[], !, G=..[F|X], droplis(X,L,Y,1), D=..[F|Y].
trim_body(G,G).

redargs([],_,_,[]).
redargs([N|Ri],H,T,Ro) :-
   arg(N,H,V), var(V), functor(H,HF,HA),
   not will_occur(V,T,N,HF,HA), unique_in_head(V,H,N,HA) ->
      Ro=[N|R], redargs(Ri,H,T,R)
  ;   redargs(Ri,H,T,Ro).

will_occur(V,T,N,HF,HA) :-
   extract_atom(A,T), functor(A,AF,AA),
   (concat_atom([AF,'_',AA],Ak), read_table(Ak,L,redarg_table) ->
       true
   ;   L=[]),
   (HF/HA=AF/AA ->
      interval(1,I,AA), not memberchk(I,[N|L])
   ;  interval(1,I,AA), not memberchk(I,L)),
   arg(I,A,X), occurs(V,X).

unique_in_head(V,H,N,HN) :-
   not (interval(1,I,HN), I\==N, arg(I,H,A), occurs(V,A)).

nlist(0,Li,Li) :- !.
nlist(N,Li,Lo) :- M is N-1, nlist(M,[N|Li],Lo).

droplis([],L,[],_) :- !.
droplis(X,[],X,_) :- !.
droplis([A|B],[N|L],Y,N) :- !, M is N+1, droplis(B,L,Y,M).
droplis([A|B],L,[A|C],N) :- M is N+1, droplis(B,L,C,M).

% General facilities

make_static(D,S) :-
   not D, !, S=..[F|L], dummies(L,L1), S1=..[F|L1], compile_term(S1).
make_static(D,S) :-
   start_compile_stream(comp),
   not (retract(D), not stream_compile_term(comp,S)),
   end_compile_stream(comp).

dummies([],[]).
dummies([_|L],['0'|D]) :- dummies(L,D).

start_compile_stream(S) :-
   open(_,string,S).

stream_compile_term(S,X) :-
   printf(S,"%q. ",X).

end_compile_stream(S) :-
   seek(S,0), set_stream(log_output,null),
   compile_stream(S), set_stream(log_output,output),
   close(S).

% Generalisation (due to Joachim Schimpf)

generalise(A,B,G) :-
   map(A,B,G,[],Map),
   sort(0,=<,Map,SortedMap),
   unify_duplicates(SortedMap).

map(A,B,G,Map,NewMap) :-
   (nonvar(A), nonvar(B), functor(A,Name,Arity), functor(B,Name,Arity) ->
       functor(G,Name,Arity), map_arg(Arity,A,B,G,Map,NewMap)
    ;  NewMap=[subst(A,B,G)|Map]).

map_arg(0,A,B,G,NewMap,NewMap) :- !.
map_arg(N,A,B,G,Map0,NewMap) :-
   arg(N,A,An), arg(N,B,Bn), arg(N,G,Gn),
   map(An,Bn,Gn,Map0,Map1), N1 is N-1,
   map_arg(N1,A,B,G,Map1,NewMap).

unify_duplicates(M) :-
   M=[subst(A1,B1,G1)|T], T=[subst(A2,B2,G2)|_] ->
      (A1==A2, B1==B2 ->
         G1=G2
      ;  true),
      unify_duplicates(T)
   ;  true.

% New predicate generation

newpred(N,R) :-
   varset(R,S), newpred1(N,S,R).

newpred(N,R,R1) :-
   varset_inter(R1,R,S), newpred1(N,S,R).

newpred1(N,S,R) :-
   (R=(_,_) ->
      Z=aux
   ;R=(_;_) ->
      Z=aux
   ;  functor(R,Z,_)),
   getval(name_count,NC),
   once((interval(NC,K,9999999), concat_atom([Z,'_',K],Name),
         not table_entry(Name,name_table))),
   K1 is K+1, setval(name_count,K1),
   write_table(Name,'0',name_table), N=..[Name|S].

% Pattern function

pattern_key(A,S) :-
   getval(term_depth,N),
   (N>1, getval(many_patterns,true) ->
      M=1
   ;  M=N),
   sk(N,A,S1),
   term_string(S1,Ss), atom_string(S,Ss).

:- export sk/3.

sk(0,_,'0') :- !.
sk(_,T,'0') :- var(T), !.
sk(N,T,S) :- symbol(T), !, functor(T,F,A), functor(S,F,A),
             M is N-1, sk(A,M,T,S).
sk(_,_,'0').

sk(0,_,_,_) :- !.
sk(A,N,T,S) :-
   arg(A,T,X), sk(N,X,Y), arg(A,S,Y), B is A-1, sk(B,N,T,S).

% Side effect and propagation sensitivity

may_be_side(G) :- var(G), !.
may_be_side(call(X)) :- !, may_be_side(X).
may_be_side(G) :- head_pred(G), !, side(G).
may_be_side(G) :- predicate_key(G,Gk), read_table(Gk,Ind,index_table), !,
                  getval(side(Ind),1).
may_be_side(G) :- not open_no_side(G).

may_be_prop(G) :- var(G), !.
may_be_prop(call(X)) :- !, may_be_prop(X).
may_be_prop(G) :- head_pred(G), !,
                  (prop(G) ->
                     true
                  ;  side(G), nonground(G)).
may_be_prop(G) :- predicate_key(G,Gk), read_table(Gk,Ind,index_table), !,
                  (getval(side(Ind),1) ->
                     true
                  ;  getval(prop(Ind),1), nonground(G)).
may_be_prop(G) :- not open_no_prop(G).

tentative_side_class(Ind,G) :-
   extract_atom(X,G), may_be_side(X) ->
      setval(side(Ind),1)
  ;   setval(side(Ind),0).

tentative_prop_class(Ind,G) :-
   extract_atom(X,G), may_be_prop(X) ->
      setval(prop(Ind),1)
  ;   setval(prop(Ind),0).

/* TABLES:
write_table writes Term to Table given key Atom.
read_table retrieves it via Sepia hashing.
delete_entry deletes an entry with key Atom from Table.
table_entry tests to see if Table has an entry with key Atom.
clear_table empties Table if it exists and starts it off again empty.
*/

predicate_key(P,K) :- functor(P,F,A), concat_atom([F,'_',A],K).

write_table(Atom,Term,Table) :-
   (current_array_body(Atom,_,Table) ->
       true
   ;   make_local_array_body(Atom,Table)),
   setval_body(Atom,Term,Table).

read_table(Atom,Term,Table) :-
   current_array_body(Atom,_,Table) ->
      getval_body(Atom,Term,Table).

delete_entry(Atom,Table) :-
   erase_array_body(Atom,Table).

table_entry(Atom,Table) :-
   current_array_body(Atom,_,Table).

clear_table(Table) :-
   erase_module(Table), create_module(Table).

% BUILT-IN SIDE EFFECTS
% Logic & control
open_no_side(call(G)) :- nonvar(G), open_no_side(G).
open_no_side(fail).
open_no_side(false).
open_no_side(true).
open_no_side(get_cut(_)).
% Database
open_no_side(clause(_)).
open_no_side(clause(_,_)).
open_no_side(current_built_in(_)).
open_no_side(current_predicate(_)).
open_no_side(get_flag(_,_,_)).
open_no_side(is_dynamic(_)).
% Internal Indexed database
open_no_side(current_record(_)).
open_no_side(is_record(_)).
open_no_side(recorded(_,_)).
open_no_side(recorded(_,_,_)).
open_no_side(recorded_list(_,_)).
open_no_side(referenced_record(_,_)).
% Type testing
open_no_side(atom(_)).
open_no_side(atomic(_)).
open_no_side(integer(_)).
open_no_side(nonground(_)).
open_no_side(nonvar(_)).
open_no_side(number(_)).
open_no_side(float(_)).
open_no_side(string(_)).
open_no_side(type_of(_,_)).
open_no_side(var(_)).
% Term comparison
open_no_side(_==_).
open_no_side(_\=_).
open_no_side(_\==_).
open_no_side(_@<_).
open_no_side(_@=<_).
open_no_side(_@>_).
open_no_side(_@>=_).
open_no_side(compare(_,_,_)).
open_no_side(compare_instances(_,_,_)).
open_no_side(instance(_,_)).
open_no_side(occurs(_,_)).
open_no_side(variant(_,_)).
% Term manipulation
open_no_side(_=.._).
open_no_side(arg(_,_,_)).
open_no_side(atom_string(_,_)).
open_no_side(char_int(_,_)).
open_no_side(copy_term(_,_)).
open_no_side(functor(_,_,_)).
open_no_side(integer_atom(_,_)).
open_no_side(name(_,_)).
open_no_side(string_list(_,_)).
open_no_side(term_string(_,_)).
% All solution
% Arithmetic
open_no_side(+(_,_,_)).
open_no_side(*(_,_,_)).
open_no_side(-(_,_)).
open_no_side(-(_,_,_)).
open_no_side(<<(_,_,_)).
open_no_side(>>(_,_,_)).
open_no_side(\(_,_)).
open_no_side(\/(_,_,_)).
open_no_side(+(_,_)).
open_no_side(_<_).
open_no_side(_=<_).
open_no_side(_=\=_).
open_no_side(_>_).
open_no_side(_>=_).
open_no_side(_=:=_).
open_no_side(/\(_,_,_)).
open_no_side(/(_,_,_)).
open_no_side(//(_,_,_)).
open_no_side(_ is _).
open_no_side(abs(_,_)).
open_no_side(acos(_,_)).
open_no_side(asin(_,_)).
open_no_side(atan(_,_)).
open_no_side(cos(_,_)).
open_no_side(exp(_,_)).
open_no_side(fix(_,_)).
open_no_side(float(_,_)).
open_no_side(ln(_,_)).
open_no_side(max(_,_,_)).
open_no_side(min(_,_,_)).
open_no_side(mod(_,_,_)).
open_no_side(plus(_,_,_)).
open_no_side(round(_,_)).
open_no_side(sin(_,_)).
open_no_side(sqrt(_,_)).
open_no_side(tan(_,_)).
open_no_side(times(_,_,_)).
open_no_side(xor(_,_,_)).
open_no_side(^(_,_,_)).
% Strings & atoms
open_no_side(atom_length(_,_)).
open_no_side(concat_atom(_,_)).
open_no_side(concat_atoms(_,_,_)).
open_no_side(concat_string(_,_)).
open_no_side(concat_strings(_,_,_)).
open_no_side(string_length(_,_)).
open_no_side(substring(_,_,_)).
% Module handling
open_no_side(current_module(_)).
open_no_side(is_locked(_)).
open_no_side(is_module(_)).
open_no_side(is_protected(_)).
open_no_side(tool_body(_,_,_)).
% Stream I/O
open_no_side(at(_,_)).
open_no_side(at_eof(_)).
open_no_side(current_stream(_,_,_)).
open_no_side(get_stream(_,_)).
open_no_side(stream_number(_)).
% Character I/O
% Term I/O
% Event handling
open_no_side(current_error(_)).
open_no_side(current_interrupt(_,_)).
open_no_side(error_id(_,_)).
open_no_side(get_error_handler(_,_)).
open_no_side(get_error_handler(_,_,_)).
open_no_side(get_interrupt_flag(_,_)).
open_no_side(get_interrupt_handler(_,_)).
open_no_side(get_interrupt_handler(_,_,_)).
open_no_side(list_error(_,_,_)).
% Debugging
open_no_side(get_leash(_,_)).
% Arrays & global variables
open_no_side(current_array(_,_,_)).
open_no_side(current_array(_,_)).
open_no_side(getval(_,_)).
% Coroutining
open_no_side(~X) :- open_no_side(X).
open_no_side(_~=_).
open_no_side(delayed_goals(_)).
open_no_side(delayed_goals_number(_,_)).
open_no_side(no_delayed_goals).
% Constructive negation
open_no_side(ineq(_,_,_)).
% External Interface
% Prolog environment
open_no_side(current_atom(_)).
open_no_side(current_functor(_)).
open_no_side(current_op(_)).
open_no_side(is_built_in(_)).
open_no_side(is_predicate(_)).
open_no_side(phrase(_,_)).
open_no_side(phrase(_,_,_)).
open_no_side(statistics(_,_)).
% Operating system
open_no_side(argc(_)).
open_no_side(argv(_,_)).
open_no_side(cputime(_)).
open_no_side(date(_)).
open_no_side(exists(_)).
open_no_side(get_file_info(_)).
open_no_side(getcwd(_)).
open_no_side(getenv(_,_)).
open_no_side(pathname(_,_)).
open_no_side(pathname(_,_,_)).
open_no_side(read_directory(_,_,_,_)).
open_no_side(random(_)).
open_no_side(suffix(_,_)).
% Libraries

% BUILT-IN BACKWARD PROPAGATION SENSITIVITY
% Logic & control
open_no_prop(call(G)) :- nonvar(G), open_no_prop(G).
open_no_prop(fail).
open_no_prop(false).
open_no_prop(true).
open_no_prop(get_cut(_)).
% Database
open_no_prop(clause(_)).
open_no_prop(clause(_,_)).
open_no_prop(current_built_in(_)).
open_no_prop(current_predicate(_)).
open_no_prop(get_flag(_,_,_)).
open_no_prop(is_dynamic(_)).
% Internal Indexed database
open_no_prop(current_record(_)).
open_no_prop(is_record(_)).
open_no_prop(recorded(_,_)).
open_no_prop(recorded(_,_,_)).
open_no_prop(recorded_list(_,_)).
open_no_prop(referenced_record(_,_)).
% Type testing
% Term manipulation
open_no_prop(_=.._).
open_no_prop(arg(_,_,_)).
open_no_prop(atom_string(_,_)).
open_no_prop(char_int(_,_)).
open_no_prop(functor(_,_,_)).
open_no_prop(integer_atom(_,_)).
open_no_prop(name(_,_)).
open_no_prop(string_list(_,_)).
% All solution
% Arithmetic
open_no_prop(*(_,_,_)).
open_no_prop(+(_,_,_)).
open_no_prop(-(_,_)).
open_no_prop(-(_,_,_)).
open_no_prop(<<(_,_,_)).
open_no_prop(>>(_,_,_)).
open_no_prop(\(_,_)).
open_no_prop(\/(_,_,_)).
open_no_prop(+(_,_)).
open_no_prop(_<_).
open_no_prop(_=<_).
open_no_prop(_=\=_).
open_no_prop(_>_).
open_no_prop(_>=_).
open_no_prop(_=:=_).
open_no_prop(/\(_,_,_)).
open_no_prop(/(_,_,_)).
open_no_prop(//(_,_,_)).
open_no_prop(_ is _).
open_no_prop(abs(_,_)).
open_no_prop(acos(_,_)).
open_no_prop(asin(_,_)).
open_no_prop(atan(_,_)).
open_no_prop(cos(_,_)).
open_no_prop(exp(_,_)).
open_no_prop(fix(_,_)).
open_no_prop(float(_,_)).
open_no_prop(ln(_,_)).
open_no_prop(max(_,_,_)).
open_no_prop(min(_,_,_)).
open_no_prop(mod(_,_,_)).
open_no_prop(plus(_,_,_)).
open_no_prop(round(_,_)).
open_no_prop(sin(_,_)).
open_no_prop(sqrt(_,_)).
open_no_prop(tan(_,_)).
open_no_prop(times(_,_,_)).
open_no_prop(xor(_,_,_)).
open_no_prop(^(_,_,_)).
% Strings & atoms
open_no_prop(atom_length(_,_)).
open_no_prop(concat_atom(_,_)).
open_no_prop(concat_atoms(_,_,_)).
open_no_prop(concat_string(_,_)).
open_no_prop(concat_strings(_,_,_)).
open_no_prop(string_length(_,_)).
open_no_prop(substring(_,_,_)).
% Module handling
open_no_prop(current_module(_)).
open_no_prop(is_locked(_)).
open_no_prop(is_module(_)).
open_no_prop(is_protected(_)).
open_no_prop(tool_body(_,_,_)).
% Stream I/O
open_no_prop(at(_,_)).
open_no_prop(at_eof(_)).
open_no_prop(current_stream(_,_,_)).
open_no_prop(get_stream(_,_)).
open_no_prop(stream_number(_)).
% Character I/O
% Term I/O
% Event handling
open_no_prop(current_error(_)).
open_no_prop(current_interrupt(_,_)).
open_no_prop(error_id(_,_)).
open_no_prop(get_error_handler(_,_)).
open_no_prop(get_error_handler(_,_,_)).
open_no_prop(get_interrupt_flag(_,_)).
open_no_prop(get_interrupt_handler(_,_)).
open_no_prop(get_interrupt_handler(_,_,_)).
open_no_prop(list_error(_,_,_)).
% Debugging
open_no_prop(get_leash(_,_)).
% Arrays & global variables
open_no_prop(current_array(_,_,_)).
open_no_prop(current_array(_,_)).
open_no_prop(getval(_,_)).
% Coroutining
open_no_prop(_ ~= _).
open_no_prop(no_delayed_goals).
% Constructive negation
open_no_prop(ineq(_,_,_)).
% External Interface
% Prolog environment
open_no_prop(current_atom(_)).
open_no_prop(current_functor(_)).
open_no_prop(is_built_in(_)).
open_no_prop(is_predicate(_)).
open_no_prop(phrase(_,_)).
open_no_prop(phrase(_,_,_)).
open_no_prop(statistics(_,_)).
% Operating system
open_no_prop(argc(_)).
open_no_prop(argv(_,_)).
open_no_prop(cputime(_)).
open_no_prop(date(_)).
open_no_prop(exists(_)).
open_no_prop(getcwd(_)).
open_no_prop(getenv(_,_)).
open_no_prop(pathname(_,_)).
open_no_prop(pathname(_,_,_)).
open_no_prop(random(_)).
open_no_prop(read_directory(_,_,_,_)).
open_no_prop(suffix(_,_)).
% Libraries

% BUILT-IN EXECUTABILITY
% Logic & control
executable_open(call(G)) :- nonvar(G), executable_open(G).
executable_open(fail).
executable_open(false).
% Database
executable_open(clause(X)) :- nonvar(X).
executable_open(clause(X,_)) :-
   nonvar(X), (functor(X,F,A), current_built_in(F/A)
              ;head_pred(X), not dynamic_pred(X)).
executable_open(current_built_in(X)) :- ground(X).
% Internal Indexed database
% Type testing
executable_open(atom(A)) :- nonvar(A).
executable_open(atomic(A)) :- nonvar(A).
executable_open(compound(A)) :- atomic(A); compound(A).
executable_open(integer(A)) :- nonvar(A).
executable_open(nonground(A)) :- ground(A).
executable_open(nonvar(A)) :- nonvar(A).
executable_open(number(A)) :- nonvar(A).
executable_open(float(A)) :- nonvar(A).
executable_open(string(A)) :- nonvar(A).
executable_open(type_of(A,B)) :- ground(A); compound(A).
executable_open(var(V)) :- nonvar(V).
% Term comparison
executable_open(A==B) :- A==B; A\=B.
executable_open(A\=B) :- A==B; A\=B.
executable_open(A\==B) :- A==B; A\=B.
executable_open(A=B).
executable_open(A@<B) :- ground(A), ground(B).
executable_open(A@=<B) :- ground(A), ground(B).
executable_open(A@>B) :- ground(A), ground(B).
executable_open(A@>=B) :- ground(A), ground(B).
executable_open(compare(A,B,C)) :- ground(B), ground(C).
%executable_open(compare_instances(A,B,C)) :- ?
%   ground(B), ground(C); B\=C.               ?
%executable_open(instance(A,B)) :-            ?
%  ground(A), ground(B); A\=B.                ?
executable_open(occurs(A,B)) :- occurs(A,B).
executable_open(variant(A,B)) :- A==B; A\=B.
% Term manipulation
executable_open(A=..B) :- nonvar(A); clist(B), B=[H|T], atom(H).
executable_open(arg(A,B,C)) :- nonvar(A), compound(B).
executable_open(atom_string(A,B)) :- ground(A); ground(B).
executable_open(char_int(A,B)) :- ground(A); ground(B).
executable_open(copy_term(A,B)) :-
  ground(A); ground(B); A\=B.
executable_open(functor(A,B,C)) :- nonvar(A); ground(B), ground(C).
executable_open(integer_atom(A,B)) :- ground(A); ground(B).
executable_open(name(A,B)) :- ground(A); ground(B).
executable_open(string_list(A,B)) :- ground(A); ground(B).
executable_open(term_string(A,B)) :- ground(A); ground(B).
% All solution
% Arithmetic
executable_open(*(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(+(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(-(A,B)) :- nonvar(A).
executable_open(-(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(<<(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(>>(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(\(A,B)) :- nonvar(A).
executable_open(\/(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(+(A,B)) :- nonvar(A).
executable_open(A<B) :- ground(A), ground(B).
executable_open(A=<B) :- ground(A), ground(B).
executable_open(A=\=B) :- ground(A), ground(B).
executable_open(A>B) :- ground(A), ground(B).
executable_open(A>=B) :- ground(A), ground(B).
executable_open(A=:=B) :- ground(A), ground(B).
executable_open(/\(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(/(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(//(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(A is B) :- ground(B).
executable_open(abs(A,B)) :- nonvar(A).
executable_open(acos(A,B)) :- nonvar(A).
executable_open(asin(A,B)) :- nonvar(A).
executable_open(atan(A,B)) :- nonvar(A).
executable_open(cos(A,B)) :- nonvar(A).
executable_open(exp(A,B)) :- nonvar(A).
executable_open(fix(A,B)) :- nonvar(A).
executable_open(float(A,B)) :- nonvar(A).
executable_open(ln(A,B)) :- nonvar(A).
executable_open(max(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(min(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(mod(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(plus(A,B,C)) :- nonvar(A), (nonvar(B); nonvar(C));
                                nonvar(B), nonvar(C).
executable_open(round(A,B)) :- nonvar(A).
executable_open(sin(A,B)) :- nonvar(A).
executable_open(sqrt(A,B)) :- nonvar(A).
executable_open(tan(A,B)) :- nonvar(A).
executable_open(times(A,B,C)) :- nonvar(A), (nonvar(B); nonvar(C));
                                 nonvar(B), nonvar(C).
executable_open(xor(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(^(A,B,C)) :- nonvar(A), nonvar(B).
% Strings & atoms
executable_open(atom_length(A,B)) :- nonvar(A).
executable_open(concat_atom(A,B)) :- ground(A).
executable_open(concat_atoms(A,B,C)) :- ground(A), ground(B).
executable_open(concat_string(A,B)) :- ground(A).
executable_open(concat_strings(A,B,C)) :- nonvar(A), nonvar(B).
executable_open(string_length(A,B)) :- nonvar(A).
executable_open(substring(A,B,C)) :- nonvar(A), nonvar(B).
% Module handling
% Stream I/O
% Character I/O
% Term I/O
% Event handling
% Debugging
% Arrays & global variables
% Coroutining
executable_open(A~=B) :- A==B; A\=B.
% Constructive negation
executable_open(ineq(V,A,B)) :- ineq_expand(V,A,B).
% External Interface
% Prolog environment
executable_open(is_built_in(A)) :- nonvar(A), A=(B/C), nonvar(B), nonvar(C).
% Operating system
% Libraries

% EXECUTION OF BUILT-INS

execute_open(clause((A:-B))) :- !, fail.
execute_open(clause(A)) :- !, fail.
execute_open(clause(A,B)) :- !, fail.
execute_open(ineq(V,A,B)) :- !, A\=B.
execute_open((A~=B)) :- !, A\=B.
execute_open(G) :- G.

ineq_expand(V,A,B) :- A==B.
ineq_expand(V,A,B) :- copy_term(V,V1), not (A=B, variant(V,V1)).

clist(L) :- nonvar(L), (L==[]; L=[_|T], clist(T)).

addclause(Ind,(H:-T)) :-
   metacall_process(T,T1), tidy(T1,Tp), % for gc(A),ct(A),fail etc
   (Tp==fail ->
      true
   ;  incval(clause_id), getval(clause_id,ID), check_bounds(ID),
      setval(clau(ID),(H,Tp)), getval(proc(Ind),L),
      setval(proc(Ind),[ID|L])).

metacall_process(T,Tm) :-
   var(T) ->
      Tm=call(T)
  ;T=call(X), nonvar(X) ->
      metacall_process(X,Tm)
  ;   Tm=T.

check_bounds(Ind) :-
   getval(bounds,Ind) ->
      writeln("   PADDY ERROR: transformation halted, bounds exceeded."),
      writeln("   The bounds can be increased using `bounds'"),
      writeln("   (type `help' for details)."), abort
  ;   true.

extract_atom(G,(A,B)) :- !, (extract_atom(G,A); extract_atom(G,B)).
extract_atom(G,(A;B)) :- !, (extract_atom(G,A); extract_atom(G,B)).
extract_atom(T,T).

rmember(X,[A|B]) :- rmember(X,B); X=A.

% ASSUMES get_cut(K) => K NOT IN CLAUSE HEAD! TRUE FOR AUTO-GEN GC...

tidy(A,B) :- norm(A,C), tidy1(C,B).

norm(((A;B);C),X) :- !, norm((A;B;C),X).
norm((A;B),(C;D)) :- !, norm(A,C), norm(B,D).
norm(((A,B),C),X) :- !, norm((A,B,C),X).
norm((A,B),(C,D)) :- !, norm(A,C), norm(B,D).
norm(A,A).

tidy1((A;B),C) :- !, tidy1(A,D), tidy1(B,E), dj(D,E,C).
tidy1((get_cut(X),A),B) :- !, tidy1(A,C), shift_gc(X,C,B).
tidy1(get_cut(_),true) :- !.
tidy1((cut_to(X),cut_to(Y),A),B) :- !, tidy1((cut_to(Y),A),B).
tidy1((cut_to(X),cut_to(Y)),cut_to(Y)) :- !.
tidy1((cut_to(X),A),B) :- !, tidy1(A,C), shift_ct(X,C,B).
tidy1((!,cut_to(X),A),B) :- !, tidy1((cut_to(X),A),B).
tidy1((!,cut_to(X)),cut_to(X)) :- !.
tidy1((call(A),B),C) :- nonvar(A), !, tidy1((A,B),C).
tidy1(call(A),C) :- nonvar(A), !, tidy1(A,C).
tidy1((A,B),C) :- !, tidy1(B,D), cj(A,D,C).
tidy1(A,A).

/* Could also have:
tidy1((cut_to(X),(A;B)),D) :- !, tidy1((cut_to(X),A;cut_to(X),B),D).
tidy1(((A;B),C),D) :- !, tidy1((A,C;B,C),D).
*/

shift_gc(X,(A;B),AXB) :- not occurs(X,A), !, shift_gc(X,B,XB), dj(A,XB,AXB).
shift_gc(X,(A=B,C),D) :- A\==X, B\==X, !, shift_gc(X,C,E), cj(A=B,E,D).
shift_gc(X,A,A) :- A\=(_;_), not occurs(X,A), !.
shift_gc(X,(cut_to(Y),A),B) :- X==Y, !, shift_gc(X,A,B).
shift_gc(X,cut_to(Y),true) :- X==Y, !.
shift_gc(X,(get_cut(Y),A),(X=Y,B)) :- !, cj(get_cut(X),A,B).
shift_gc(X,A,B) :- cj(get_cut(X),A,B).

shift_ct(X,C,B) :-
   (C=(get_cut(Y),D) ->
      cj(Y=X,D,E), B=(cut_to(X),E)
   ;  cj(cut_to(X),C,B)).

cj(fail,_,fail) :- !.
cj(abort,_,abort) :- !.
cj(true,B,B) :- !.
cj(A,true,A) :- !.
cj(A,B,(A,B)).

dj(fail,B,B) :- !.
dj(abort,B,abort) :- !.
dj(A,fail,A) :- !.
dj(A,B,(A;B)).

first_rest(((A,B),C),F,R) :- !, first_rest((A,B,C),F,R).
first_rest((F,R),F1,R1) :- !, F=F1, R=R1.
first_rest(T,T,true).

first_rest1(fail,_,fail,fail) :- !.
first_rest1(true,A,B,C) :- !, first_rest(A,B,C).
first_rest1(A,true,B,C) :- !, first_rest(A,B,C).
first_rest1(((A,B),C),D,X,Y) :- !, first_rest1((A,B,C),D,X,Y).
first_rest1((A,fail),_,A,fail) :- !.
first_rest1((A,true),C,A,C) :- !.
first_rest1((A,B),C,A,(B,C)) :- !.
first_rest1(A,B,A,B).

natural(0).
natural(I) :- natural(J), I is J+1.

interval(A,A,B) :- A=<B.
interval(A,B,C) :- A<C, D is A+1, interval(D,B,C).

most_general(H) :- functor(H,_,N), copy_term(H,H1), most_general(N,H1).

most_general(0,H) :- !.
most_general(N,H) :-
   arg(N,H,A), var(A), A=N, M is N-1, most_general(M,H).

varof(V,T) :-
   term_string(T,X), open(X,string,I), readvar(I,T,S1),
   close(I), member([_|V],S1).

% A slight flaw in this varset: (A,B,A) -> [B,A], ie ordering changed
% for repeated variables. But it's faster than explicitly coding it.
% For varset_inter should have largest argument first, for speed.

varset(T,S) :-
   term_string(T,X), open(X,string,I),
   readvar(I,T,S1), close(I), strip_names(S1,S).

strip_names([],[]).
strip_names([[_|A]|B],[A|C]) :- strip_names(B,C).

varset_inter(A,B,S) :- varset(A,T), inter(T,B,S).

inter([],_,[]).
inter([A|B],C,D) :- (occurs(A,C) -> D=[A|E]; D=E), inter(B,C,E).

divert(F) :- open(F,write,file), set_stream(divert,file).

undivert :- set_stream(divert,output), close(file).

help :-
   writeln("COMMANDS"),
   nl,
   writeln("p(Infile)"),
   writeln("   partially deduces Infile, result to screen"),
   writeln("p(Infile,Outfile)"),
   writeln("   partially deduces Infile, result to Outfile"),
   nl, 
   writeln("pin(Infile)"),
   writeln("   reads in the query file Infile"),
   writeln("p"),
   writeln("   performs the partial deduction"),
   writeln("pout(Outfile)"),
   writeln("   writes the result to the file Outfile"),
   writeln("pout"),
   writeln("   writes the result to the screen"),
   nl,
   writeln("term_depth(N)"),
   writeln("   sets the term abstraction depth to N (default 5)"),
   writeln("pattern_number(N)"),
   writeln("   sets the threshold number of patterns to N (default 100)"),
   writeln("bounds(N)"),
   writeln("   sets the array sizes to N").

?- writeln("   *-------------------------------------------------------*"),
   writeln("   |            The PADDY partial deduction system         |"),
   writeln("   |                                                       |"),
   writeln("   | S.D.Prestwich                               ECRC 1992 |"),
   writeln("   |                                                       |"),
   writeln("   |                  (type `help' for help)               |"),
   writeln("   *-------------------------------------------------------*").

:- set_error_handler(231, (help)/0).

