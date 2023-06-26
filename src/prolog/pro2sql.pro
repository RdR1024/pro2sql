module(pro2sql,[
    head_sql/3,
    clauses_sql/7,
    clause_sql/7,
    args_sql/4

]).

/** <module> Lightweight Prolog to SQL compiler

# Introduction
This module is a minimal compiler of prolog query predicates to sql SELECT statements. I'm inspired by Draxler's work [1][], but I needed something smaller and more efficient, just for a subset of SELECT statements.  The subset is just those that are equivalent to constraint predicate queries:

* No aggregation (i.e. no min,max,sum,avg,count), also no sql HAVING 
* No grouping or ordering
* Can use constraints like Age > 16, etc.
* Can do "joins" through shared variable names between predicates

The aim is that a "query predicate" (a prolog predicate intended to retrieve records from fact predicates) works the same in prolog as its SQL translation works on SQL data -- except that SQL returns a table, where prolog returns variable result alternatives.

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
%   :- head_sql(name_age_query(Name,Age),S^S,Select^NST).
%   S=Select, Select = [Name,Age|NST]
%   ~~~
%
%   @arg Head   Prolog predicate that represents a query
%   @arg S      The starting list of arguments
%   @arg ST     The tail variable of F, used to append new arguments to
%   @arg Select The resulting list of arguments
%   @arg NST    The new tail variable of the resulting argument list
head_sql(Head,Select^ST,Select^NST):-
    Head =.. [_|Args],
    append(Args,NST,ST).

%%  clauses_sql(+Clauses,+S^ST,+F^FT,W^WT,Select^NST,From^NFT,Where^NWT) is semidet.
%   Translates body clauses from a prolog query predicate into elements of an sql
%   query.
%
%   Example
%   ~~~
%   :- assert(table_def(person,[id,name,age],[prefix-myproj,table-'people'])).
%   :- assert(table_def(employee,[name,company],[prefix-myproj])).
%   :- clauses_sql([person(_,Name,Age),employee(Name,Company)],[Name,Age,Company|ST]^ST,F^F,W^W, Select^NST,From^NFT,Where^NWT).
%
%   ~~~
%
%   @arg Clauses        List of body clauses
%   @arg S^ST           Initial difference list of arguments for the SELECT clause
%   @arg F^FT           Initial difference list of arguments for the FROM clause
%   @arg W^WT           Initial difference list of arguments for the WHERE clause
%   @arg Select^NST     Resulting diffence list of arguments for the SELECT clause
%   @arg From^NFT       Resulting difference list of arguments for the FROM clause
%   @arg Where^NWT      Resulting difference list of arguments for the WHERE clause
clauses_sql([],S^ST,F^FT,W^WT,S^ST,F^FT,W^WT).
clauses_sql([Clause|Cs],S^ST,F^FT,W^WT,Select^NST,From^NFT,Where^NWT):-
    clause_sql(Clause,S^ST,F^FT,W^WT,S2^ST2,F2^FT2,W2^WT2),
    clauses_sql(Cs,S2^ST2,F2^FT2,W2^WT2,Select^NST,From^NFT,Where^NWT).

%%  clause_sql(+Clause,+S^ST,+F^FT,W^WT,Select^NST,From^NFT,Where^NWT) is semidet.
%   Translates a body clause from a prolog query predicate into elements of an sql
%   SELECT, FROM, and WHERE clause.
%
%   Example
%   ~~~
%   :- assert(table_def(person,[id,name,age],[prefix-myproj,table-'people'])).
%   :- clause_sql(person(_,Name,Age),[Name,Age|ST]^ST,F^F,W^W,Select^NST,From^NFT,Where^NWT).
%   NST,From^NFT,Where^NWT).
%   Name = 'people.name',
%   Age = 'people.age',
%   ST = NST,
%   F = From, From = ['myproj.people'|NFT],
%   W = Where, Where = NWT, NWT = [],
%   Select = ['people.name', 'people.age'|NST] .   
%   ~~~
%
%   @arg Clause     The clause to be translated
%   @arg S          The initial list of arguments for the SELECT clause
%   @arg ST         The tail of the S list
%   @arg F          The initial list of arguments for the FROM clause
%   @arg FT         The tail of the F list
%   @arg W          The initial list of arguments for the WHERE clause
%   @arg WT         The tail of the W list
%   @arg Select     The resulting list of arguments for the SELECT clause
%   @arg NST        The tail of the resulting Select list
%   @arg For        The resulting list of arguments for the FROM clause
%   @arg NFT        The tail of the resulting For list
%   @arg Where      The resulting list of arguments for the WHERE clause
%   @arg NWT        The tail of the Where list
clause_sql(Clause,Select^ST,From^FT,Where^WT,Select^ST,From^NFT,Where^NWT):-
    Clause =.. [Func|Args],
    table_def(Func,Fields,Options),
    ( member(prefix-Pref,Options) -> true; Pref='' ),
    ( member(table-Tab,Options) -> true; Tab=Func),
    ( Pref = '' -> Table=Tab; atomic_list_concat([Pref,'.',Tab],Table)),
    append([Table],NFT,FT),
    args_sql(Tab,Fields,Args,Wh),
    (   Wh=[] 
    ->  ( Where=Where, NWT=WT )
    ;   append(Wh,NWT,WT)
    ).

%%  args_sql(+Table,+Fields,+Args,-Where) is semidet.
%   Translate predicate arguments to sql field names, and/or WHERE field constraints.
%
%   Examples
%   ~~~
%   :- args_sql(person,[id,name,age],[_,Name,Age],Where).
%   Name = 'person.name'
%   Age = 'person.age'
%   Where = []
%
%   :- args_sql(person,[id,name,age],[_,Name,16],Where).
%   Name = 'person.name'
%   Where = ['person.age' = 16]
%   ~~~
%
%   @arg Table      Atomic table name
%   @arg Fields     List of table field names, matched in order of predicate argument
%   @arg Args       List of arguments (originating from a query predicate)
%   @arg Where      List of sql WHERE clauses, in case an Arg is nonvar
args_sql(_,[],[],[]).
args_sql(Table,[Field|Fs],[Arg|As],Where):-
    var(Arg),
    atomic_list_concat([Table,'.',Field],Arg),!,
    args_sql(Table,Fs,As,Where).
args_sql(Table,[F|Fs],[Arg|As],[Field=Arg|Where]):-
    nonvar(Arg),
    atomic_list_concat([Table,'.',F],Field),!,
    args_sql(Table,Fs,As,Where).



