% test queries
:- load_csv('employee.csv',[prefix(tiny),fields([name,company])]).
:- load_csv('people.csv',[functor(person),prefix(tiny),table(people),fields([id,name,age])]).

adults(Name,Age):-
    person(_,Name,Age),
    Age >=21.

adults_noKaren(Name,Age):-
    person(_,Name,Age),
    between(21,100,Age),
    \+ member(Name,[karen,sheila]).

company_age(group(Company),avg(Age)):-
    person(_,Name,Age),
    employee(Name,Company).

from_dog_years(Name,Age*7):-
    person(_,Name,Age).