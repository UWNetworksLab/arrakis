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
% Copyright (C) 2006 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Warwick Harvey and Kish Shen, IC-Parc
% 
% END LICENSE BLOCK
%---------------------------------------------------------------------
%
% IC search module.
%
% System:       ECLiPSe Constraint Logic Programming System
% Author/s:     Warwick Harvey, IC-Parc
%               Kish Shen,      IC-Parc
%
%	This module is essentially an extract of the search-related
%	predicates from the RIA module, written by Joachim Schimpf and
%	Stefano Novello; and from the fd_search module, by Helmut Simonis.
%
% This module provides the search-related components of the IC library, a
% combined finite domain and floating point interval propagation solver.
%
%---------------------------------------------------------------------

:- module(ic_search).

%---------------------------------------------------------------------
%
% Imports and exports.
%

:- comment(summary, "This library provides the search-related components of the IC-library").

:-comment(desc,html("\
  This library provides the search-related components of the IC-library,
  essentially those adapted from fd_search, and RIA.  Provided is a generic
  search routine (for integer domain) which implements a number of partial
  search methods (complete, credit, lds, bbs, dbs) and some of their
  combinations. For floating point domains, the search facilities from RIA
  are provided.
")).

:- use_module(ic_kernel).
:- use_module(ic_constraints).

:- export squash/3, locate/4, locate/3, locate/2.

:- use_module(ic).

%---------------------------------------------------------------------
% generalised search/6 and friends originally from fd_search
%

:- use_module(ic_generic_interface).
:- import get_bounds/3 from ic_generic_interface.

:- include(generic_search).
:- comment(include, generic_search_comments).


%---------------------------------------------------------------------

% register the stats events
:- ic_stat_register_event(ic_split,'splits'),
	ic_stat_register_event(ic_squash,'squashes').


%---------------------------------------------------------------------
%
% Precision management.
%
% Precision is the required accuracy as specified in a call to locate or
% squash.  Basically, intervals are only split/squashed when their width
% exceeds the specified precision.
%
% Note that the precision should be significantly larger than the threshold
% at which propagation is suspended.
%

    %
    % precision_threshold_ratio(-Ratio)
    %	Returns the minimum ratio required between the precision and the
    %	threshold (i.e. the precision must be at least Ratio times the
    %	threshold).
    %
precision_threshold_ratio(100).

    %
    % check_precision(++Prec)
    %	Ensures that the threshold is sufficiently smaller than the
    %	precision by lowering the threshold if required.
    %
    %	Fails if Prec is not a float.
    %
check_precision(Prec) :-
	float(Prec),
	get_threshold(Threshold),
	precision_threshold_ratio(Ratio),
	( Prec < Threshold * Ratio ->
	    NewT is Prec / Ratio,
	    printf(warning_output,
		    "WARNING: Propagation threshold lowered to %w%n", [NewT]),
	    set_threshold(NewT)
	;
	    true
	).

%---------------------------------------------------------------------
%
% Search predicates.
%

    %
    % locate(+LocateVars, ++Precision)
    % locate(+LocateVars, ++Precision, ++LinLog)
    % locate(+LocateVars, +SquashVars, ++Precision, ++LinLog)
    %	Search for solutions by refining the domains of the variables in the
    %	list LocateVars until their domains match the specified precision
    %	(i.e. their widths in absolute and relative terms are less than
    %	Precision).  Search is done by nondeterministically splitting the
    %	domains of the variables in LocateVars, possibly interleaved by
    %	squashing operations on the variables in the list SquashVars.
    %	LinLog specifies whether the splits should be linear (lin) or
    %	logarithmic (log).
    %
    %	By default, no variables are squashed and logarithmic splitting is
    %	used.
    %

locate(Vars, Prec) :-
	locate(Vars, Prec, log).

locate(Vars, Prec, LinLog) :-
	locate(Vars, [], Prec, LinLog).

locate(LocateVars, SquashVars, Prec, LinLog) :-
	collection_to_list(LocateVars, LocateList),
	collection_to_list(SquashVars, SquashList),
	linlog(LinLog, _, _),
	check_precision(Prec),
	!,
	squash0(SquashList, Prec, LinLog),	% initial squash
	list_to_queue(LocateList, Queue, End),
	locate_vars(Queue, End, SquashList, Prec, LinLog).
locate(LocateVars, SquashVars, Prec, LinLog) :-
	error(5, locate(LocateVars,SquashVars,Prec,LinLog)).


    %
    % Notes:
    %
    % Splitting with several variables: We maintain a queue of variables,
    % and start splitting from the first.  If splitting causes propagation
    % (on the split variable) then continue splitting this one, otherwise
    % take the next one and put the old one at the end of the queue.
    %
    % (Idea for "weaker squash": Between locate steps, use a variant of
    % squash that only cuts off halves.  Note that if there is only one
    % solution, splitting off halves always works, down to the precision.
    % Squashing with smaller fractions is only good for narrowing down an
    % interval with many solutions)
    %

locate_vars(Queue, End, SquashVars, Precision, LinLog) :-
	( Queue == End ->
	    true
	;
	    Queue = [X | Queue1],
	    split(X, SquashVars, Precision, LinLog, Result),
	    ( Result = good ->	% try X again
		locate_vars(Queue, End, SquashVars, Precision, LinLog)
	    ; Result = bad ->	% move X to end of queue
		End = [X | End1],
		locate_vars(Queue1, End1, SquashVars, Precision, LinLog)
	    ; % Result = done	% remove X from queue
		locate_vars(Queue1, End, SquashVars, Precision, LinLog)
	    )
	).

split(X, SquashVars, Prec, LinLog, Result) :-
	var(X),
	get_float_bounds(X, XL, XH),
	linlog(LinLog, LinLogCode, _),
	( ria_binop(LinLogCode, XL, XH, Prec, 0.5, Split, _) ->
	    %%writeln(split(XL-Split-XH)),
	    ic_event(ic_split),
	    ( sgn(Split, 1) ->
		(
		    Lold = XL, Hold = Split, impose_max(X, Split)
		;
		    Lold = Split, Hold = XH, impose_min(X, Split)
		)
	    ;
		(
		    Lold = Split, Hold = XH, impose_min(X, Split)
		;
		    Lold = XL, Hold = Split, impose_max(X, Split)
		)
	    ),
	    wake,	% Propagate.

	    % Possibly do some squashing.
	    squash0(SquashVars, Prec, LinLog),

	    % Now check whether it did something.
	    get_float_bounds(X, Lnew, Hnew),
	    ( Lnew > Lold ->
	    	Result = good
	    ; Hnew < Hold ->
	    	Result = good
	    ;
	    	Result = bad
	    )
	;
	    Result = done
	).
split(X, _, _, _, Result) :-
	number(X),
	Result = done.


    %
    % squash(+Vars, ++Precision, ++LinLog)
    %	This refines the range constaints on the variables Vars using the
    %	algorithm described in Oliver L'homme's paper Consistency Techniques
    %	for Numeric CSPs 1993.  It attempts to reduce the domain of a
    %	variable, and if it fails uses this to set the new range of that
    %	variable to the intersection of the old range and the complement of
    %	the failed range.  The domains it tries range from one old bound to
    %	a specific ratio of the domain and are halved iteratively.  squash/4
    %	does this halving iteration.  The LinLog flag parameter can be
    %	either lin or log for linear or logarithmic splitting of the domain.
    %
squash(Vars, Prec, LinLog) :-
	collection_to_list(Vars, List),
	linlog(LinLog, _, _),
	check_precision(Prec),
	!,
	squash0(List, Prec, LinLog).
squash(Vars, Prec, LinLog) :-
	error(5, squash(Vars, Prec, LinLog)).

squash0([], _, _).
squash0(Vars, Prec, LinLog) :-
	Vars = [_ | _],
	linlog(LinLog, Lo, Hi),
	vars2pairs(Vars, Pairs, Lo, Hi),
	squash1(Pairs, Prec, 1.0).

    % Iterate over possible sizes.
squash1([], _Prec, _Ratio).
squash1([Pair | Pairs], Prec, Ratio):-
	NewRatio is 0.5 * Ratio,
	squash1ratio([Pair | Pairs], Remaining, Prec, NewRatio),
	squash1(Remaining, Prec, NewRatio).

    %
    % squash1ratio(+Pairs, -Remaining, ++Precision, ++Ratio)
    %	Iterate over all variables repeatedly until fixpoint reached.
    %	Always succeeds, possibly restricting the ranges of some of the
    %	variables.  Fixed point is reached if there are no more pairs to be
    %	squashed, or if the list is traversed once through with nothing
    %	reduced.
    %
squash1ratio([], [], _Precision, _Ratio).
squash1ratio([Head | Tail], Remaining, Precision, Ratio) :-
	squashv([Head | Tail], Remaining0, Precision, Ratio, Reduce),
	( 1 = Reduce ->
		squash1ratio(Remaining0, Remaining, Precision, Ratio)
	;
		Remaining0 = Remaining
	).

    %
    % squashv(+Pairs, -Remaining, ++Precision, ++Ratio, -Reduced)
    %	Goes through all the pairs in the list attempting to squash at each
    %	variable bound.  Reduced=1 if squashing is successful for at least
    %	one bound, =0 if no bounds were squashed.  The operator encodes
    %	which bound to squash, and whether to use lin or log splitting.  If
    %	splitting fails (split so small we go below precision), then this is
    %	a failure to reduce the domain.  Otherwise we attempt to restrict
    %	the domain.  If we have succeed in reducing the domain we continue
    %	trying with this bound, otherwise we proceed to a different var
    %	bound pair.
    %
squashv([], [], _Prec, _Ratio, 0).
squashv([Pair | Tail], Remain, Prec, Ratio, Reduce) :-
	Pair = Var - Op,
	get_float_bounds(Var, Lo, Hi),
	%%writeln(squashv-Var-Lo-Hi),
	( ria_binop(Op, Lo, Hi, Prec, Ratio, Split, _) ->
	    %%writeln(squashv-Op-Ratio-(Lo,Hi)=Split),
		ic_event(ic_squash),
		( can_restrict(Op, Var, Split) ->
			restrict(Op, Var, Split),
			% try bound again
			% Mark Success
			Reduce = 1,
			% try bound again
			squashv([Pair | Tail], Remain, Prec, Ratio,_)
		;
			% no restrict move to next pair
			% keep pair for next iteration
%			write("."), flush(output),
			Remain = [Pair | Remain0],
			squashv(Tail, Remain0, Prec, Ratio, Reduce)
		)
	;
		% no split throw var away
		% writeln(Op-Ratio-(Lo,Hi)=failed),
%		write(p), flush(output),
		squashv(Tail, Remain, Prec, Ratio, Reduce)
	).

    %
    % can_restrict(++Op, ?Var, ++Split)
    % restrict(++Op, ?Var, ++Split)
    %	This tries to restrict the domain of a variable.  If can_restrict/3
    %   succeeds we learn that the variable's domain can be restricted to the
    %   complement of what failed, which is what restrict/3 does.
    %   The Op input parameter says what bound to try, but strangely encoded.
    %

can_restrict(ria_binop(logsplit), Var, Split) :-
	not ( impose_max(Var, Split), wake ).
can_restrict(ria_binop(logsplit_upper), Var, Split) :-
	not ( impose_min(Var, Split), wake ).
can_restrict(ria_binop(linsplit), Var, Split) :-
	not ( impose_max(Var, Split), wake ).
can_restrict(ria_binop(linsplit_upper), Var, Split) :-
	not ( impose_min(Var, Split), wake ).


restrict(ria_binop(logsplit), Var, Split) :-
	impose_min(Var, Split), wake.
restrict(ria_binop(logsplit_upper), Var, Split) :-
	impose_max(Var, Split), wake.
restrict(ria_binop(linsplit), Var, Split) :-
	impose_min(Var, Split), wake.
restrict(ria_binop(linsplit_upper), Var, Split) :-
	impose_max(Var, Split), wake.



    %
    % vars2pairs(+Vars, -Pairs, ++SplitOpLo, ++SplitOpHi)
    %	Preprocesses the list of variables Vars to yield a list of
    %	variable-split_operator pairs, which is what we iterate over.
    %	The split_operator encodes which bound and type of split.
    %
vars2pairs([], [], _Lo, _Hi).
vars2pairs([Var | Tail0], [Var - Lo, Var - Hi | Tail], Lo, Hi) :-
	%%writeln(vars2pairs-Var),
	vars2pairs(Tail0, Tail, Lo, Hi).


    % The routine in C to split needs these special numbers.
linlog(lin, ria_binop(linsplit), ria_binop(linsplit_upper)).
linlog(log, ria_binop(logsplit), ria_binop(logsplit_upper)).


list_to_queue([], T, T).
list_to_queue([X | Xs], [X | Ys], T) :-
	list_to_queue(Xs, Ys, T).

list(X) :- var(X), !, fail.
list([]).
list([_ | _]).

