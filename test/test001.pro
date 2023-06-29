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
:- [test_queries].

%  Test translation of queries
test(adults,[nondet]):-
    pro2sql(adults(Name,Age),SQL),
    SQL = 'SELECT people.name,people.age FROM tiny.people WHERE people.age >= 21',
    Name = 'people.name',
    Age = 'people.age',
    format(stdout,'~n~w~n',[SQL]).

test(adults_noKaren,[nondet]):-
    pro2sql(adults_noKaren(_Name,_Age),SQL),
    SQL = 'SELECT people.name,people.age FROM tiny.people WHERE people.age BETWEEN 21 AND 100 AND NOT ( people.name IN ("karen","sheila") )',
    format(stdout,'~n~w~n',[SQL]).

test(company_age,[nondet]):-
    pro2sql(company_age(group(_Company),avg(_Age)),SQL),
    SQL = 'SELECT employee.company,avg(people.age) FROM tiny.people,tiny.employee WHERE employee.name = people.name GROUP BY employee.company ORDER BY employee.company',
    format(stdout,'~n~w~n',[SQL]).

test(from_dog_years,[nondet]):-
    pro2sql(from_dog_years(_Name,_Age*7),SQL),
    SQL='SELECT people.name,people.age*7 FROM tiny.people',
    format(stdout,'~n~w~n',[SQL]).

:- end_tests(pro2sql).