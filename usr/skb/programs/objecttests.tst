% eclipse -e "lib(test_util), [objects2], test(objecttests)."

%
% Index Tests
%
find_candidates([constraint(notFound, _, 0)], Index) should_fail.
find_candidates([constraint(mix, _, X)], Index) should_give X=[o2, o3, o4, o5, o7, o8].
find_candidates([constraint(mix, _, 's')], Index) should_give X=[o0, o1, o2, o3, o4, o5, o7, o8, o9].
find_candidates([constraint(mix, _, 0)], Index) should_give X=[o0, o1, o9].


%
% Basic Tests
%
get_object(_, [val(a,0)], [], X) should_give X=object(o0, [val(a, 0), val(mix, 1.0)]).
get_object(_, [val(a, 1), val(mix, 20)], [], X) should_give X=object(o1, [val(a, 1), val(mix, 20)]).
get_object(_, [val(mix, 'attr1')], [], X) should_give X=object(o2, [val(a, 2), val(mix, attr1)]).
get_object(_, [val(a, 3), val(mix, 'attr1')], [], X) should_fail.
get_object(_, [val(a, 3)], [constraint(mix, ==, "attr2")], X) should_give X=object(o3, [val(a, 3), val(mix, "attr2")]).
get_object(_, [val(a, 3)], [constraint(mix, <, "attr3")], X) should_give X=object(o3, [val(a, 3), val(mix, "attr2")]).
get_object(_, [val(a, 3)], [constraint(mix, >, "attr1")], X) should_give X=object(o3, [val(a, 3), val(mix, "attr2")]).


%
% String constraints
%
get_object(_, [val(a, 3)], [constraint(mix, '!=', "attrX")], X) should_give X=object(o3, [val(a, 3), val(mix, "attr2")]).
get_object(_, [val(a, 3)], [constraint(mix, <=, "attr3")], X) should_give X=object(o3, [val(a, 3), val(mix, "attr2")]).
get_object(_, [val(a, 3)], [constraint(mix, <=, "attr2")], X) should_give X=object(o3, [val(a, 3), val(mix, "attr2")]).
get_object(_, [val(a, 3)], [constraint(mix, <=, "attr1")], X) should_fail.
get_object(_, [val(a, 3)], [constraint(mix, >=, "attr2")], X) should_give X=object(o3, [val(a, 3), val(mix, "attr2")]).
get_object(_, [val(a, 3)], [constraint(mix, >=, "attr1")], X) should_give X=object(o3, [val(a, 3), val(mix, "attr2")]).
get_object(_, [val(a, 3)], [constraint(mix, >=, "attr3")], X) should_fail.

findall(X, get_object(_, [], [constraint(mix, <=, "attr3")], X), L)
should_give L=[object(o2, [val(a, 2), val(mix, attr1)]), object(o3, [val(a, 3), val(mix, "attr2")]), object(o5, [val(a, 5), val(mix, "12")]), object(o8, [val(a, 8), val(mix, "12")])].


%
% Number constraints
%
get_object(_, [val(mix, 1.0992)], [], X) should_give X=object(o9, [val(a, 9), val(mix, 1.0992)]).

findall(X, get_object(_, [], [constraint(mix, >, 1)], X), L)
should_give L=[object(o1, [val(a, 1), val(mix, 20)]), object(o9, [val(a, 9), val(mix, 1.0992)])].

findall(X, get_object(_, [], [constraint(mix, ==, 1)], X), L)
should_give L=[object(o0, [val(a, 0), val(mix, 1.0)])].

findall(X, get_object(_, [], [constraint(mix, =<, 1)], X), L)
should_give L=[object(o0, [val(a, 0), val(mix, 1.0)])].

findall(X, get_object(_, [], [constraint(mix, >=, 1)], X), L)
should_give L=[object(o0, [val(a, 0), val(mix, 1.0)]), object(o1, [val(a, 1), val(mix, 20)]), object(o9, [val(a, 9), val(mix, 1.0992)])].


%
% Variable Constraints
%
findall(X, get_object(_, [val(mix, Z)], [], X), L)
should_give L=[object(o0, [val(a, 0), val(mix, 1.0)]), object(o1, [val(a, 1), val(mix, 20)]), object(o2, [val(a, 2), val(mix, attr1)]), object(o3, [val(a, 3), val(mix, "attr2")]), object(o4, [val(a, 4), val(mix, "test str")]), object(o5, [val(a, 5), val(mix, "12")]), object(o7, [val(a, 7), val(mix, "test str 123")]), object(o8, [val(a, 8), val(mix, "12")]), object(o9, [val(a, 9), val(mix, 1.0992)])].


%
% Regex Constraints
%
findall(X, get_object(_, [], [constraint(mix, match, "^test.*")], X), L)
should_give L=[object(o4, [val(a, 4), val(mix, "test str")]), object(o7, [val(a, 7), val(mix, "test str 123")])].


%
% Constraint on Name
%
findall(X, get_object(name_constraint(o1), [], [], X), L)
should_give L=[object(o1, [val(a, 1), val(mix, 20)])].

findall(X, get_object(name_constraint("^o[1|2|3]$"), [], [], X), L)
should_give L=[object(o1, [val(a, 1), val(mix, 20)]), object(o2, [val(a, 2), val(mix, attr1)]), object(o3, [val(a, 3), val(mix, "attr2")])].

findall(X, get_object(name_constraint("^o[1|2|3]$"), [val(a,3)], [], X), L)
should_give L=[object(o3, [val(a, 3), val(mix, "attr2")])].


