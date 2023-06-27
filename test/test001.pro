%  Unit tests.
%  Load this file as usual (e.g. [test001]) and then
%  `:- run_tests.`
%
%  See the SWI-Prolog documentation for Prolog Unit Tests

:- current_output(S), set_stream(S,alias(stdout)).  % unit tests seem to lose `user_output`
:- begin_tests(pro2sql).
:- use_module('../src/prolog/pro2sql.pro').

%   Define a table and a query predicate
%   These would be part of a normal prolog progam
:- load_csv('people.csv',[functor(person),prefix(tiny),table(people),fields([id,name,age])]).
:- [adults].
:- [teens_noKaren].

%  Test translation of queries
test(adults,[nondet]):-
    pro2sql(adults(Name,Age),SQL),
    format(stdout,'~n~w~n',[SQL]).

test(adults_noKaren,[nondet]):-
    pro2sql(adults_noKaren(Name,Age),SQL),
    format(stdout,'~n~w~n',[SQL]).

:- end_tests(pro2sql).