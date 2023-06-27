% test query
teens_noKaren(Name,Age):-
    person(_,Name,Age),
    between(21,100,Age),
    \+ member(Name,[karen,sheila]).
