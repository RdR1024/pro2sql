% Investigation into an approach for an aggregate predicate
% Flags can be used for aggregate values in a failure-driven "loop"
% This could be generalised into aggregate(Goal), where Goal contains
% arguments with aggregation functions, like foo(Department,avg(Age)).
%
% Such an aggregate predicate would give Prolog the ability to process
% aggregations the same way that SQL does.
p(1,john,13).
p(2,jake,19).
p(3,jane,22).
p(4,joan,22).
p(5,jack,17).

:- set_flag(count,0).
:- set_flag(avg,0).
:- set_flag(sum,0).
allp:-
    p(_,Name,Age),
    writeln(Name),
    flag(count,N,N+1),
    flag(sum,S,S+Age),
    flag(avg,_,S/(N+1)),
    fail.
allp.

/*
to generalise:
aggregate(Goal):-
    for every arg of Goal create a flag in a list Flags,
    initialise flags in Flags,
    ! don't backtrack beyond this point,
    execute goal,
    update flags,
    fail.
aggregate(Goal):-
    rebuild Flags from Goal,
    retrieve each flag value from Flags,
    and unify with arguments of Goal,
    (delete flags).

Notes: it probably needs separate group_agg and all_agg predicates if we
also want the equivalent of GROUP BY
*/