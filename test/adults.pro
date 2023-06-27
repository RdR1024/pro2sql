% test query
adults(Name,Age):-
    person(_,Name,Age),
    Age >=21.
