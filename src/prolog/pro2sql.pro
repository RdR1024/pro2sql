:-module(pro2sql,[
    head_sql/3,
    clauses_sql/7,
    clause_sql/7,
    args_sql/4,
    pro2sql/2,
    nondot_str/2,
    concat_to_atom/3
]).

:- module_transparent 
    head_sql/3,
    clauses_sql/7,
    clause_sql/7,
    args_sql/4,
    pro2sql/2,
    nondot_str/2,
    concat_to_atom/3.


/** <module> Lightweight Prolog to SQL compiler

# pro2sql

A lightweight Prolog to SQL SELECT translator.

# Introduction
This module is a minimal compiler of prolog query predicates to sql SELECT statements. I'm inspired by Draxler's work [1][], but I couldn't access his original paper, and it was too much work trying to use Mungall's implementation without much documentation. It seemed to me that it would be less work to implement a minimalist translator:

* No aggregation (i.e. no min,max,sum,avg,count), also no sql HAVING 
* No grouping or ordering
* Can use constraints like Age > 16, etc.
* Can do "joins" through shared variable names between predicates
* `member/2` translates to X IN List
* Can use SWI-Prolog `between/3` 

The aim is that a "query predicate" (a prolog predicate intended to retrieve records from fact predicates) works the same in prolog as in SQL -- except that SQL returns a table, where prolog returns variable result alternatives. The use case is where you want to extract from a database through SQL and then further query results table in Prolog.

# Notes
* field names are always translated with the table name as prefix. We also use this to determine if an atom in a constraint (e.g. `'mytable.person' = jones`) should be translated as a string (e.g. "jones"). This method is not perfect -- there could be atoms that contain full-stops (periods) that _should_ be translated as a string.  Therefore, the recommendation is to store string values as strings in Prolog.
* TODO: use SWI-Prolog's regular expressions to implement LIKE

# Installation
For the moment, just copy the file `src\prolog\pro2sql.pro`, or clone the git repo and copy it locally.

I will create a SWI-Prolog package when I've finished my current TODO list.

# References

[1] "Draxler C (1992) Prolog to SQL Compiler, version 1.0. Technical report, CIS Centre for Information and Speech, University of Munich"

**/

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
%   Name = 'people.name',
%   Age = 'people.age',
%   SQL = 'SELECT Name, Age FROM myproj.people WHERE people.age >= 21'
%
%   :- assert( (teens(Name,Age):- person(_,Name,Age), between(16,21,Age), \+ member(Name,[john,jane])) ).
%   :- pro2sql(teens(Name,Age),SQL).
%   Name = 'people.name',
%   Age = 'people.age',
%   SQL = 'SELECT people.name,people.age FROM myproj.people WHERE people.age BETWEEN 16 AND 21 AND NOT ( people.name IN ("john","jane") )' .
%   ~~~
%
%   @arg Head   Predicate head clause of the predicate to translate into SQL
%   @arg SQL    Resulting SQL text
pro2sql(Head,SQL):-
    Head =.. [Func|Args],
    length(Args,N),
    current_predicate(Func/N),
    context_module(M),
    M:clause(Head,Body),
    head_sql(Head,S^S,S1^ST),
    clauses_sql(Body,S1^ST,F^F,W^W,Sel^[],Fro^[],Whe^[]),
    atomic_list_concat(Sel,',',Select),
    atomic_list_concat(Fro,',',From),
    atomic_list_concat(Whe,' ',Where),
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

%   Conjunctive body clauses
clauses_sql((Clause,Cs),S^ST,F^FT,W^WT,Select^NST,From^NFT,Where^NWT):-
    clauses_sql(Clause,S^ST,F^FT,W^WT,S2^ST2,F2^FT2,W2^WT2),
    ( WT == WT2 -> WT2a=WT2; append(['AND'],WT2a,WT2)),
    clauses_sql(Cs,S2^ST2,F2^FT2,W2^WT2a,Select^NST,From^NFT,Where^NWT).

%   Disjunctive body clauses
clauses_sql((Clause;Cs),S^ST,F^FT,W^WT,Select^NST,From^NFT,Where^NWT):-
    clauses_sql(Clause,S^ST,F^FT,W^WT,S2^ST2,F2^FT2,W2^WT2),
    ( WT == WT2 -> WT2a=WT2; append(['OR'],WT2a,WT2)),
    clauses_sql(Cs,S2^ST2,F2^FT2,W2^WT2a,Select^NST,From^NFT,Where^NWT).

%   Negated body clause
clauses_sql((\+ Clause),S^ST,F^FT,W^WT,Select^NST,From^NFT,Where^NWT):-
    append(['NOT ('],WT2,WT),
    clauses_sql(Clause,S^ST,F^FT,W^WT2,Select^NST,From^NFT,Where^WT2a),
    append([')'],NWT,WT2a).

%   Last clause
clauses_sql(Clause,S^ST,F^FT,W^WT,Select^NST,From^NFT,Where^NWT):-
    Clause \= (_,_), Clause \= (_;_),
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
%   @arg S^ST           Initial difference list of arguments for the SELECT clause
%   @arg F^FT           Initial difference list of arguments for the FROM clause
%   @arg W^WT           Initial difference list of arguments for the WHERE clause
%   @arg Select^NST     Resulting diffence list of arguments for the SELECT clause
%   @arg From^NFT       Resulting difference list of arguments for the FROM clause
%   @arg Where^NWT      Resulting difference list of arguments for the WHERE clause

%   A simple predicate clause, where the predicate functor is defined as a table
clause_sql(Clause,Select^ST,From^FT,Where^WT,Select^ST,From^NFT,Where^NWT):-
    Clause =.. [Func|Args],
    context_module(M),
    M:table_def(Func,Fields,Options),
    ( member(prefix-Pref,Options) -> true; Pref='' ),
    ( member(table-Tab,Options) -> true; Tab=Func),
    ( Pref = '' -> Table=Tab; atomic_list_concat([Pref,'.',Tab],Table)),
    append([Table],NFT,FT),
    args_sql(Tab,Fields,Args,Wh),
    (   Wh=[] 
    ->  ( Where=Where, NWT=WT )
    ;   append(Wh,NWT,WT)
    ).

%   membership constraint clause.
clause_sql(Clause,S^ST,F^FT,Where^WT,S^ST,F^FT,Where^NWT):-
    Clause =.. [member,Arg1,Arg2],
    maplist(nondot_str,Arg2,Arg2s),
    concat_to_atom(Arg2s,',',A2),
    format(atom(NewClause),'~w IN (~w)',[Arg1,A2]),
    append([NewClause],NWT,WT).

%   between constraint clause.
clause_sql(Clause,S^ST,F^FT,Where^WT,S^ST,F^FT,Where^NWT):-
    Clause =.. [between,Low,High,Value],
    format(atom(NewClause),'~w BETWEEN ~w AND ~w',[Value,Low,High]),
    append([NewClause],NWT,WT).

%   A comparative constraint clause.
clause_sql(Clause,S^ST,F^FT,Where^WT,S^ST,F^FT,Where^NWT):-
    Clause =.. [Op,Arg1,Arg2], Op\=member, Op\=between,
    member(Op-Sop,['<'-'<','>'-'>','='-'=','=='-'=','>='-'>=','=<'-'<=','\\='-'<>']),
    nondot_str(Arg2,A2),
    format(atom(NewClause),'~w ~w ~k',[Arg1,Sop,A2]),
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
%   Where = ['person.age = 16']
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
args_sql(Table,[F|Fs],[Arg|As],[Field|Where]):-
    nonvar(Arg),
    atomic_list_concat([Table,'.',F],Fld),
    nondot_str(Arg,A),
    format(atom(Field),'~w = ~k',[Fld,A]),!,
    args_sql(Table,Fs,As,Where).


%%  nondot_str(+Atom,-Convert) is semidet.
%   Convert an atom that does not contain a full-stop into a string
%
%   Example
%   ~~~
%
%   ~~~
%
%   @arg Atom       the atom value
%   @arg Convert    the resulting atom or string
nondot_str(Atom,Convert):-
    ( (atom(Atom),(Atom\=null,Atom\='NULL'),atomic_list_concat([Atom],'.',Atom)) 
    -> atom_string(Atom,Convert)
    ; Convert=Atom
    ).

%%  concat_to_atom(+List,+Sep,-Atom) is semidet.
%   Like `atomic_list_concat/3` but strings retain quotes.
%
%   Example
%   ~~~
%   :- concat_to_atom([hello,"John"],' ',Atom).
%   Atom = 'hello "John"'
%   ~~~
%
%   @arg List   List of atomic values to convert
%   @arg Sep    Atom used as separator
%   @arg Atom   The resulting atom with concatenated text
concat_to_atom([A|As],Sep,Result):-
    format(atom(First),'~k',[A]),
    concat_to_atom(As,Sep,First,Result).
concat_to_atom([],_,Result,Result).
concat_to_atom([A|As],Sep,Current,Result):-
    format(atom(Temp),'~w~w~k',[Current,Sep,A]),
    concat_to_atom(As,Sep,Temp,Result).