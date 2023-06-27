module(pro2sql,[
    head_sql/3,
    clauses_sql/7,
    clause_sql/7,
    args_sql/4,
    pro2sql/2,
    op(1100,xfy,or),
    op(900,fy,not),
    op(700,xfy,'<='),
    op(700,xfy,'<>')
]).

/** <module> Lightweight Prolog to SQL compiler

# Introduction
This module is a minimal compiler of prolog query predicates to sql SELECT statements. I'm inspired by Draxler's work [1][], but I couldn't access his original paper, and it was too much work trying to use Mungall's implementation without much documentation. It seemed to me that it would be less work to implement a minimalist translator:

* No aggregation (i.e. no min,max,sum,avg,count), also no sql HAVING 
* No grouping or ordering
* Can use constraints like Age > 16, etc.
* Can do "joins" through shared variable names between predicates

The aim is that a "query predicate" (a prolog predicate intended to retrieve records from fact predicates) works the same in prolog as its SQL translation works on SQL data -- except that SQL returns a table, where prolog returns variable result alternatives. The use case is where you want to grab an extract from a database through SQL and then further query it in Prolog.

# References

[1] "Draxler C (1992) Prolog to SQL Compiler, version 1.0. Technical report, CIS Centre for Information and Speech, University of Munich"

**/

%   Some operators to make translation easier.
%   The clause_sql predicate first translates prolog constraints (e.g. X > Y) into clauses
%   that use SQL operators. For that it helps if we define SQL operators, to keep infix notation.
:- op(1100,xfy,or).
:- op(900,fy,not).
:- op(700,xfy,'<=').
:- op(700,xfy,'<>').

%%  table_def(+TableName,+FieldList,+Options) is multi.
%   Defines the table name and field names of an SQL table. `TableName` should match
%   the functor (predicate name) of the prolog predicate that represents the table.
%   If the actual table or view has a different name, this can be given as the option
%   `table-tablename`.
%
%   Example. Note: we're using `assert` here from the prolog prompt, but `table_def/3` can 
%   be asserted from a prolog file like any other predicate.
%   ~~~
%   :- assert(table_def(person,[id,name,age],[prefix-myproj,table-'people'])).
%   ~~~
%
%   @arg TableName      Atom that matches the predicate name, representing an SQL table, or a fact in Prolog.
%   @arg FieldList      List that contains atomic field names of the SQL table.
%   @arg Options        List of options. Currently either `prefix-PrefixForTables` and/or `table-TableName`
:-  dynamic table_def/3.
:-  multifile table_def/3.

%%  pro2sql(+Head,-SQL):-
%   Translate a prolog predicate to an sql SELECT statement.
%
%   Example
%   ~~~
%   :- assert(table_def(person,[id,name,age],[prefix-myproj,table-'people'])).
%   :- assert( (adults(Name,Age):- person(_,Name,Age), Age >=21) ).
%   :- pro2sql(adults(Name,Age), SQL).
%   SQL = 'SELECT Name, Age FROM myproj.people WHERE people.age >= 21'
%   ~~~
%
%   @arg Head   Predicate head clause of the predicate to translate into SQL
%   @arg SQL    Resulting SQL text
pro2sql(Head,SQL):-
    Head =.. [Func|Args],
    length(Args,N),
    current_predicate(Func/N),
    clause(Head,Body),
    head_sql(Head,S^S,S1^ST),
    clauses_sql(Body,S1^ST,F^F,W^W,Sel^[],Fro^[],Whe^[]),
    atomic_list_concat(Sel,',',Select),
    atomic_list_concat(Fro,',',From),
    atomic_list_concat(Whe,' and ',Where),
    format(atom(SQL),'SELECT ~w FROM ~w WHERE ~w',[Select,From,Where]).

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
%   :- clauses_sql((person(_,Name,Age),employee(Name,Company)),[Name,Age,Company|ST]^ST,F^F,W^W, Select^NST,From^NFT,Where^NWT).
%
%   :- clauses_sql((person(_,Name,Age),Age >= 16),[Name,Age|ST]^ST,F^F,W^W,Select^NST,From^NFT,Where^NWT).
%   Name = 'people.name',
%   Age = 'people.age',
%   ST = NST,
%   F = From, From = ['myproj.people'|NFT],
%   W = Where, Where = ['people.age'>=16|NWT],
%   Select = ['people.name', 'people.age'|NST] .
%   ~~~
%
%   @arg Clauses        Conjunction of body clauses
%   @arg S^ST           Initial difference list of arguments for the SELECT clause
%   @arg F^FT           Initial difference list of arguments for the FROM clause
%   @arg W^WT           Initial difference list of arguments for the WHERE clause
%   @arg Select^NST     Resulting diffence list of arguments for the SELECT clause
%   @arg From^NFT       Resulting difference list of arguments for the FROM clause
%   @arg Where^NWT      Resulting difference list of arguments for the WHERE clause
clauses_sql((Clause,Cs),S^ST,F^FT,W^WT,Select^NST,From^NFT,Where^NWT):-
    clause_sql(Clause,S^ST,F^FT,W^WT,S2^ST2,F2^FT2,W2^WT2),
    clauses_sql(Cs,S2^ST2,F2^FT2,W2^WT2,Select^NST,From^NFT,Where^NWT).
clauses_sql(Clause,S^ST,F^FT,W^WT,Select^NST,From^NFT,Where^NWT):-
    Clause \= (_,_),
    clause_sql(Clause,S^ST,F^FT,W^WT,Select^NST,From^NFT,Where^NWT).


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

%   A simple predicate clause, where the predicate functor is defined as a table
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

%   A constraint clause. TODO: 'or' and 'not'
clause_sql(Clause,S^ST,F^FT,Where^WT,S^ST,F^FT,Where^NWT):-
    Clause =.. [Op,Arg1,Arg2],
    member(Op-Sop,['<'-'<','>'-'>','='-'=','=='-'=','>='-'>=','=<'-'<=','\\='-'<>']),
    format(atom(NewClause),'~w ~w ~k',[Arg1,Sop,Arg2]),
    append([NewClause],NWT,WT).

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



