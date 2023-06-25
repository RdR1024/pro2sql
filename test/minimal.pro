% a minimal test for pro2sql
% The pro2sql/2 predicate will translate the first argument (a prolog goal) into
% an SQL SELECT query.


% Definition of the table predicate
% Follows the template: <predicate>_def(table_name,field1name-field1type,field2name-field2type,...)
name_age_def(name_age,id-int,name-string,age-int).

name_age_query(ID,Name,Age):-
    name_age(ID,Name,Age),
    Age >= 21.
% Test:
% :- pro2sql(name_age_query/3,SQL).  % same as pro2sql([],name_age_query,SQL).
% SQL = 'SELECT id,name,age FROM name_age WHERE age > 21'


avg_age_query(Avg):-
    avg(name_age(_,_,Age),Avg).
% Note: avg(Pred,Avg) is a known predicate that
% calculates the average of the field indicated in a predicate head.
% All other argument positions in the head should be underscore.
% 
% Test:
% :- pro2sql(avg_age_query/3,SQL).
% SQL = 'SELECT avg(age) FROM name_age'

unique_names_query(Count):-
    count(name_age(_,Name,_),[distinct],Count).

number_of_rows_query(Rows):-
    count(name_age/3,[],Rows).

