module(pro2sql,[
    head_to_sql/3


]).

/** <module> Lightweight Prolog to SQL compiler

# Introduction
This module is a minimal compiler of prolog query predicates to sql SELECT statements. I'm inspired by Draxler's work [1][], but I needed something smaller and more efficient, just for a subset of SELECT statements. 



# References

[1] "Draxler C (1992) Prolog to SQL Compiler, version 1.0. Technical report, CIS Centre for Information and Speech, University of Munich"

**/

%%  head_to_sql(+Head,+S^ST,-Select^NST) is semidet.
%   Takes the head of a prolog query predicate and extracts its arguments
%   into a difference list that will ultimately become the contents of the
%   sql SELECT clause.
%
%   Example
%   ~~~
%   :- head_to_sql(name_age_query(Name,Age),S^S,Select^NST).
%   S=Select, Select = [Name,Age|NST]
%   ~~~
%
%   @arg Head   Prolog predicate that represents a query
%   @arg S      The starting list of arguments
%   @arg ST     The tail variable of F, used to append new arguments to
%   @arg Select The resulting list of arguments
%   @arg NST    The new tail variable of the resulting argument list
head_to_sql(Head,Select^ST,Select^NST):-
    Head =.. [_|Args],
    append(Args,NST,ST).

%%  clause_to_sql(+Clause,+S^ST,+F^FT,W^WT,Select^NST,From^NFT,Where^NWT) is semidet.
%   Translates a body clause from a prolog query predicate into elements of an sql
%   SELECT, FROM, or WHERE clause.
%
%   Example
%   ~~~
%   :- assert(person_def(id,name,age,[prefix=myproj,table='people'])).
%   :- clause_to_sql(name(ID,Name,Age),[Name,Age|ST]^ST,F^F,W^W,Select^NST,From^NFT,Where^NWT).
%   Select = ['people.name','people.age'|NST]
%   From = ['myproj.person'|NFT]
%   W = Where, Where = NWT
%   ~~~
%
%   @arg Clause     The clause to be translated
%   @arg S          The initial list of arguments for the SELECT clause
%   @arg ST         The tail of the S list
%   @arg F          The initial list of arguments for the FOR clause
%   @arg FT         The tail of the F list
%   @arg W          The initial list of arguments for the WHERE clause
%   @arg WT         The tail of the W list
%   @arg Select     The resulting list of arguments for the SELECT clause
%   @arg NST        The tail of the resulting Select list
%   @arg For        The resulting list of arguments for the FOR clause
%   @arg NFT        The tail of the resulting For list
%   @arg Where      The resulting list of arguments for the WHERE clause
%   @arg NWT        The tail of the Where list
