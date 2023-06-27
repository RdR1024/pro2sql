%  Unit tests.
%  Load this file as usual (e.g. [test001]) and then
%  `:- run_tests.`
%
%  See the SWI-Prolog documentation for Prolog Unit Tests
:- current_output(S), set_stream(S,alias(curout)).
:- begin_tests(pro2sql).
:- use_module('../src/prolog/pro2sql.pro').

%   Define a table and a query predicate
%   These would be part of a normal prolog progam
table_def(person,[id,name,age],[prefix-myproj,table-people]).
adults(Name,Age):-
    person(_,Name,Age),
    Age >=21.
teens_noKaren(Name,Age):-
    person(_,Name,Age),
    between(13,19,Age),
    \+ member(Name,[karen,sheila]).


%  Test translation of queries
test(adults,[nondet]):-
    pro2sql(adults(Name,Age),SQL),
    format(curout,'~n~w~n',[SQL]).

test(teens_noKaren,[nondet]):-
    pro2sql(teens_noKaren(Name,Age),SQL),
    format(curout,'~n~w~n',[SQL]).

:- end_tests(pro2sql).