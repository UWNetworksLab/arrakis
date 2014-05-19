% ----------------------------------------------------------------------
% System:	ECLiPSe Constraint Logic Programming System
% Version:	$Id: queues.pl,v 1.1 2008/06/30 17:43:48 jschimpf Exp $
%
% Copyright:	This library has been adapted from code from the Edinburgh
%		DEC-10 Prolog Library, whose copyright notice says:
%
%		These files are all in the "public domain" so you can
%		use them freely, copy them, incorporate them into
%		programs of your own and so forth without payment. 
%		The work of producing them in the first place and of
%		organising them as detailed here has been funded over
%		the years at Edinburgh University mainly by the
%		Science and Engineering Research Council.  Their
%		dissemination has been encouraged by the Alvey Special
%		Interest Group:  Artificial Intelligence.  We would
%		appreciate it if you were to acknowledge these bodies
%		when you use or re-distribute any of these files.
% ----------------------------------------------------------------------

%   File   : QUEUES.PL
%   Author : R.A.O'Keefe
%   Updated: Friday November 18th, 1983, 8:09:31 pm
%   Purpose: define queue operations
%   Needs  : lib(lists) for append/3.

:- module(queues).		% ECLiPSe header
:- export
	make_queue/1,		%   create empty queue
	join_queue/3,		%   add element to end of queue
	list_join_queue/3,	%   add many elements to end of queue
	jump_queue/3,		%   add element to front of queue
	list_jump_queue/3,	%   add many elements to front of queue
	head_queue/2,		%   look at first element of queue
	serve_queue/3,		%   remove first element of queue
	length_queue/2,		%   count elements of queue
	empty_queue/1,		%   test whether queue is empty
	list_to_queue/2,	%   convert list to queue
	queue_to_list/2.	%   convert queue to list

:- mode
	make_queue(-),
	join_queue(+, +, -),
	list_join_queue(+, +, -),
	jump_queue(+, +, -),
	list_jump_queue(+, +, -),
	head_queue(+, ?),
	serve_queue(+, ?, -),
	length_queue(+, ?),
	length_queue(?, ?, +, -),
	empty_queue(+),
	list_to_queue(+, -),
	queue_to_list(+, -),
	queue_to_list(?, ?, -).


:- comment(summary, "define queue operations").
:- comment(author, "R.A.O'Keefe").
:- comment(copyright, 'This file is in the public domain').
:- comment(date, "Friday November 18th, 1983, 8:09:31 pm").
:- comment(desc, html("<P>
    In this package, a queue is represented as a term Front-Back,  where
    Front  is  a list and Back is a tail of that list, and is normally a
    variable.  join_queue will only work when the Back  is  a  variable,
    the  other routines will accept any tail.  The elements of the queue
    are the list difference, that is, all the elements starting at Front
    and stopping at Back.  Examples:
<PRE>
	[a,b,c,d,e|Z]-Z	    has elements a,b,c,d,e
	[a,b,c,d,e]-[d,e]   has elements a,b,c
	Z-Z		    has no elements
	[1,2,3]-[1,2,3]	    has no elements
</PRE>
")).


:- comment(make_queue/1, [
    summary:"creates a new empty queue",
    template:"make_queue(Queue)",
    desc:html("
    creates a new empty queue.  It will also match empty queues, but
    because Prolog doesn't do the occurs check, it will also match
    other queues, creating circular lists.  So this should ONLY be
    used to make new queues.
    ")]).

make_queue(X-X).

:- comment(join_queue/3, [
    summary:"adds the new element at the end of the queue",
    template:"join_queue(Element, OldQueue, NewQueue)",
    desc:html("
    adds the new element at the end of the queue.  The old queue is
    side-effected, so you *can't* do
<PRE>
 	join_queue(1, OldQ, NewQ1),
 	join_queue(2, OldQ, NewQ2).
</PRE>
    There isn't any easy way of doing that, sensible though it might
    be.  You *can* do
<PRE>
 	join_queue(1, OldQ, MidQ),
 	join_queue(2, MidQ, NewQ).
</PRE>
    "),
    see_also:[list_join_queue/3]]).

join_queue(Element, Front-[Element|Back], Front-Back).

:- comment(list_join_queue/3, [
    summary:"adds the new elements at the end of the queue",
    template:"list_join_queue(List, OldQueue, NewQueue)",
    desc:html("
    adds the new elements at the end of the queue.  The elements are
    added in the same order that they appear in the list, e.g.
<PRE>
    list_join_queue([y,z], [a,b,c|M]-M, [a,b,c,y,z|N]-N).
</PRE>
    ")]).

list_join_queue(List, Front-OldBack, Front-NewBack) :-
	append(List, NewBack, OldBack).

:- comment(jump_queue/3, [
    summary:"adds the new element at the front of the list",
    template:"jump_queue(Element, OldQueue, NewQueue)",
    desc:html("
    adds the new element at the front of the list.  Unlike join_queue,
<PRE>
 	jump_queue(1, OldQ, NewQ1),
 	jump_queue(2, OldQ, NewQ2)
</PRE>
    *does* work, though if you add things at the end of NewQ1 they
    will also show up in NewQ2.  Note that
<PRE>
 	jump_queue(1, OldQ, MidQ),
 	jump_queue(2, MidQ, NewQ)
</PRE>
    makes NewQ start 2, 1, ...
    ")]).

jump_queue(Element, Front-Back, [Element|Front]-Back).



:- comment(list_jump_queue/3, [
    summary:"adds all the elements of List at the front of the queue",
    template:"list_jump_queue(List, OldQueue, NewQueue)",
    desc:html("
    adds all the elements of List at the front of the queue.  There  are
    two  ways  we might do this.  We could add all the elements one at a
    time, so that they would appear at the beginning of the queue in the
    opposite order to the order they had in the list, or  we  could  add
    them in one lump, so that they have the same order in the  queue  as
    in  the  list.   As you can easily add the elements one at a time if
    that is what you want, I have chosen the latter.
    ")]).

list_jump_queue(List, OldFront-Back, NewFront-Back) :-
	append(List, OldFront, NewFront).
%	reverse(List, OldFront, NewFront).	% for the other definition



:- comment(head_queue/2, [
    summary:"unifies Head with the first element of the queue",
    template:"head_queue(Queue, Head)",
    desc:html("
    unifies Head with the first element of the queue.  The tricky part
    is that we might be at the end of a queue: Back-Back, with Back a
    variable, and in that case this predicate should not succeed, as we
    don't know what that element is or whether it exists yet.
    ")]).

head_queue(Front-Back, Head) :-
	Front \== Back,		%  the queue is not empty
	Front = [Head|_].



:- comment(serve_queue/3, [
    summary:"removes the first element of the queue for service",
    template:"serve_queue(OldQueue, Head, NewQueue)"]).

serve_queue(OldFront-Back, Head, NewFront-Back) :-
	OldFront \== Back,
	OldFront = [Head|NewFront].



:- comment(empty_queue/1, [
    summary:"tests whether the queue is empty",
    template:"empty_queue(Queue)",
    desc:html("
    tests whether the queue is empty.  If the back of a queue were
    guaranteed to be a variable, we could have
<PRE>
 	empty_queue(Front-Back) :- var(Front).
</PRE>
    but I don't see why you shouldn't be able to treat difference
    lists as queues if you want to.
    ")]).

empty_queue(Front-Back) :-
	Front == Back.



:- comment(length_queue/2, [
    summary:"counts the number of elements currently in the queue",
    template:"length_queue(Queue, Length)",
    desc:html("
    counts the number of elements currently in the queue.  Note that
    we have to be careful in checking for the end of the list, we
    can't test for [] the way length(List) does.
    ")]).

length_queue(Front-Back, Length) :-
	length_queue(Front, Back, 0, N),
	Length = N.

length_queue(Front, Back, N, N) :-
	Front == Back, !.
length_queue([_|Front], Back, K, N) :-
	L is K+1,
	length_queue(Front, Back, L, N).



:- comment(list_to_queue/2, [
    summary:"creates a new queue with the same elements as List",
    template:"list_to_queue(List, Queue)"]).

list_to_queue(List, Front-Back) :-
	append(List, Back, Front).



:- comment(queue_to_list/2, [
    summary:"creates a new list with the same elements as Queue",
    template:"queue_to_list(Queue, List)"]).

queue_to_list(Front-Back, List) :-
	queue_to_list(Front, Back, List).

queue_to_list(Front, Back, Ans) :-
	Front == Back, !, Ans = [].
queue_to_list([Head|Front], Back, [Head|Tail]) :-
	queue_to_list(Front, Back, Tail).


