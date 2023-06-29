% test queries

adults(Name,Age):-
    person(_,Name,Age),
    Age >=21.

adults_noKaren(Name,Age):-
    person(_,Name,Age),
    between(21,100,Age),
    \+ member(Name,[karen,sheila]).
