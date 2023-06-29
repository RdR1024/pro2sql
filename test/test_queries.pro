% test queries
:- load_csv('employee.csv',2,[prefix(tiny)]).
:- load_csv('people.csv',3,[functor(person),prefix(tiny),table(people)]).

adults(Name,Age):-
    person(_,Name,Age),
    Age >=21.

adults_noKaren(Name,Age):-
    person(_,Name,Age),
    between(21,100,Age),
    \+ member(Name,[karen,kate]).

company_age(group(Company),order(Company),avg(Age)):-
    person(_,Name,Age),
    employee(Name,Company).

from_dog_years(Name,Age*7):-
    person(_,Name,Age).

re_query(Name):-
    person(_,Name,_Age),
    re_match(".*e$",Name).

avg_age(avg(Age)):-
    person(_,_,Age).

seniors(Name,Age):-
    avg_age_result(Avg),
    person(_,Name,Age),
    Age > Avg.