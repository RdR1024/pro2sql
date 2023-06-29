:-module(pro2sql,[
    pro2sql/2,
    pro2sql/3,
    nondot_str/2,
    concat_to_atom/2,
    concat_to_atom/3,
    member_dif/2,
    load_csv/3,
    load_csv/4,
    bq/4,
    op(800,fy,distinct),
    op(800,fy,all)
]).

/** <module> Lightweight Prolog to SQL compiler
# pro2sql

A lightweight Prolog to SQL SELECT translator.

## Introduction
This module is a minimal compiler of prolog "query predicates" to sql SELECT statements. I'm inspired by Draxler's work [1][], but I couldn't access his original paper, and it was too much work trying to use Mungall's implementation without much documentation. It seemed to me that it would be less work (and more fun) to implement a minimalist translator:

* Automatically translates constraints like Age > 16, etc.
* Autmatically translates `member/2` to `... IN List`
* Automatically translates SWI-Prolog `between/3`
* Does "joins" through shared variable names between predicates
* No LIKE, but instead REGEXP_CONTAINS (used in Google SQL). Configurable for other SQLs with `set_flag(regexp,Template)`.
* DISTINCT and ALL are implemented as one-place ops in Prolog (for head arguments).
* Only implements translation to SELECT...  (no UPDATE, etc.)
* No subqueries. Generate subqueries as result tables first and then use as joins.
* Includes a simple utility predicate that will translate a prolog query predicate, send it to Google BigQuery, and load the
  results as fact predicates.

The aim is that a "query predicate" (a prolog predicate intended to retrieve results from fact predicates) works the same in prolog as in SQL -- except that SQL returns a table, where prolog returns variable result alternatives. 

The use case is where you want to extract from a database through SQL and then further query the results in Prolog.

## Installation
In SWI-Prolog, `package_install(pro2sql).`

Otherwise, clone the git repository (https://github.com/RdR1024/pro2sql), and copy the files from `...pro2sql/src/prolog/`

## Usage

Let's assume that you have an SQL table called `person` with fields `id,name,age`.  In Prolog, assert the following table definition predicate:

~~~
:- assert(table_def(person,[id,name,age],[prefix(mydataset)])).
~~~

Let's also assume that you have also have a predicate that would list all people with age >= 21.

~~~
adult(Name,Age):- person(_,_,Age), Age >= 21.
~~~

This query predicate is stock-standard Prolog. And if you had fact predicates for `person/3` then the query predicate would simply give you all the names and ages of people over 21.  However, let's say that the data is not in Prolog, but in the SQL database. You can translate that predicate into an SQL query like this:

~~~
:- pro2sql(adult(Name,Age),SQL).
Name = person.name
Age = person.age
SQL = 'SELECT person.name, person.age FROM mydataset.person WHERE person.age >= 21'
~~~

And you can run this query on Google's BigQuery like this (assuming you have an account and that the `person` table is available in the `mydataset` dataset):

~~~
:- bq(adult(Name,Age),[name,age],Status,[]).
~~~

By default, the results become available in Prolog as `adult_result(Name,Age)` (same predicate name as the query, but with `_result` suffix).  The results also exist in a downloaded `.csv` table, with the column headings given in the second argument of `bq/4`.

~~~
:- adult_result(Name,Age).
Name = john
Age = 23 ; ...
~~~


For more examples, see the code comments and also `test_queries.pro` in the test folder of the repository. The `bq/4` predicate currently uses BigQuery commandline utility (bq).  I intend to write an BiqQuery API interface for Prolog at some time in the future.  In the meantime, I have also left some notes on BigQuery bq in the `.../doc` folder.

## Status

This code is currently `alpha`.

All requirements for now are implemented, and it seems to be working with a small set of tests.  However, I want to test it more comprehensively before calling it `beta`.


## Notes
* field names are always translated with the table name as prefix. We also use this to determine if an atom 
  in a constraint (e.g. `'mytable.person' = jones`) should be translated as a string (e.g. `"jones"`). This method is not perfect -- there could be atoms that contain full-stops (periods) that _should_ be translated as a string.  Therefore, the recommendation is to store string values as strings in Prolog.
* The arguments in the head of the prolog query predicate may contain aggregation functions, for example 
  `company_age(group(Company), avg(Age))`.  These aggregation functions will translate to SQL and work fine. 
  But if you use the predicate directly in Prolog, the aggregation function won't do anything. Rather, to get the same results as SQL, but in Prolog, you would have to run the query predicate through an aggregate predicate (like the `aggregate/3` from `library(aggregate)`.

  For example, assume that you have a query predicate `tonnage(sum(Weight)):- package(_,_,Weight)`.  In Prolog, you could get
  the same results as SQL, by applying `aggregate/3` -- that is, `aggregate(sum(Weight),tonnage(sum(Weight)),Total)`.

## Code Notes
I use a lot of _difference lists_.  My preferred notation is `List^Tail`, where `List = [x1,x2,x3,...|Tail]`. In other words, the `^Tail` suffix simply provides another copy of the tail variable. For example I use `Select^ST` as the difference list for the SELECT clauses.  Typically, I use `xT` for the initial tail variabe (e.g. `ST`,`FT`,`WT` for the SELECT,FROM and WHERE tails respectively) and `NxT` for the "new" (after processing) tails.  

The initial (empty) value when calling a predicate with difference lists is something like `S^S`. With difference lists, adding a new value happens like this: `ST = [NewValue|NST]`.  Here, the new value is made into a list with the new tail and then that list is unified with the old tail.  Alternative, if we have a list of new values, `append(NewValues,NST,ST)` would do the same.

## References

[1] "Draxler C (1992) Prolog to SQL Compiler, version 1.0. Technical report, CIS Centre for Information and Speech, University of Munich"

## License
BSD 3-Clause License

Copyright (c) 2023, Richard de Rozario

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

**/

:- use_module('./file_path_name_ext.pro').

:- module_transparent 
    head_sql/9,
    headargs_sql/9,
    clauses_sql/8,
    clause_sql/8,
    args_sql/4,
    pro2sql/2,
    pro2sql/3,
    nondot_str/2,
    concat_to_atom/2,
    concat_to_atom/3,
    member_dif/2,
    load_csv/3,
    load_csv/4,
    bq/4.

:- op(800,fy,distinct).
:- op(800,fy,all).
:- set_flag(regexp,'REGEXP_CONTAINS (~w, r\'~w\')').

%%  table_def(+TableName,+FieldList,+Options) is multi.
%   Defines the table name and field names of an SQL table. `TableName` should match
%   the functor (predicate name) of the prolog predicate that represents the table.
%   If the actual table or view has a different name, this can be given as the option
%   `table(tablename)`.
%
%   Example. Note: we're using `assert` here from the prolog prompt, but `table_def/3` can 
%   be asserted from a prolog file like any other predicate.  Also, `load_csv/2` will
%   automatically create a `table_dev/3`.
%   ~~~
%   :- assert(table_def(person,[id,name,age],[prefix(tiny),table(people)])).
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
%   :- assert(table_def(person,[id,name,age],[prefix(tiny),table(people)])).
%   :- assert( (adults(Name,Age):- person(_,Name,Age), Age >=21) ).
%   :- pro2sql(adults(Name,Age), SQL).
%   Name = 'people.name',
%   Age = 'people.age',
%   SQL = 'SELECT people.name,people.age FROM tiny.people WHERE people.age >= 21'
%
%   :- assert( (teens(Name,Age):- person(_,Name,Age), between(16,21,Age), \+ member(Name,[john,jane])) ).
%   :- pro2sql(teens(Name,Age),SQL).
%   Name = 'people.name',
%   Age = 'people.age',
%   SQL = 'SELECT people.name,people.age FROM tiny.people WHERE people.age BETWEEN 16 AND 21 AND NOT ( people.name IN ("john","jane") )' .
%   ~~~
%
%   @arg Head   Predicate head clause of the predicate to translate into SQL
%   @arg SQL    Resulting SQL text
pro2sql(Head,SQL):-
    context_module(CM),
    pro2sql(CM,Head,SQL).
pro2sql(CM,Head,SQL):-
    Head =.. [Func|Args],
    length(Args,N),
    current_predicate(Func/N),
    CM:clause(Head,Body),
    head_sql(Head,S^S,G^G,H^H,R^R,S1^ST,Gro^[],Hav^[],Ord^[]),
    clauses_sql(CM,Body,S1^ST,F^F,W^W,Sel^[],Fro^[],Whe^[]),
    concat_to_atom(Sel,',',Select),
    concat_to_atom(Fro,',',From),
    ( Whe=[] ->  Where=''; concat_to_atom([' WHERE'|Whe],' ',Where) ),
    ( Gro=[] -> Group=''; concat_to_atom(Gro,',',Grou), format(atom(Group),' GROUP BY ~w',[Grou])),
    ( Hav=[] -> Having=''; concat_to_atom(Hav,',',Havi), format(atom(Group),' HAVING ~w',[Havi])),
    ( Ord=[] -> Order=''; concat_to_atom(Ord,',',Orde), format(atom(Order),' ORDER BY ~w',[Orde])),
    format(atom(SQL),'SELECT ~w FROM ~w~w~w~w~w',[Select,From,Where,Group,Having,Order]).

%%  head_to_sql(+Head,+S^ST,-Select^NST) is semidet.
%   Takes the head of a prolog query predicate and extracts its arguments
%   into a difference list that will ultimately become the contents of the
%   sql SELECT clause.
%
%   Example
%   ~~~
%   :- head_sql(name_age_query(Name,Age),S^S,G^G,R^R,Select^NST,Group^NGT,Order^NRT).
%   S=Select, Select = [Name,Age|NST]
%
%   :- head_sql(company_age(group(Company),avg(Age)),S^S,G^G,R^R,Select^NST,Group^NGT,Order^R).
%   S=Select, Select = [Company,avg(Age)]
%   Group = [Company]
%   ~~~
%
%   @arg Head           Prolog predicate that represents a query
%   @arg S^ST           Initial difference list of arguments for the SELECT clause
%   @arg Select^NST     Resulting diffence list of arguments for the SELECT clause
head_sql(Head,S^ST,G^GT,H^HT,R^RT,S^NST,G^NGT,H^NHT,R^NRT):-
    Head =.. [_|Args],
    headargs_sql(Args,S^ST,G^GT,H^HT,R^RT,S^NST,G^NGT,H^NHT,R^NRT).

%%  headargs_sql(+Args,S^ST,G^GT,H^HT,R^RT,Select^NST,Group^NGT,Having^NHT,Order^NRT) is semidet.
%   Translates a list of predicate head arguments into clauses for SELECT,
%   GROUP BY and ORDER BY.
%   
%   Argument types and translations:
%   * order(Expr)   - (Expr) added to the Select list and to the Order list.
%   * group(Var)    - Var is added to the Select and Group list
%   * having(Expr)  - (Expr) is added to the Having list
%   * min(Expr)     - translated as min(Expr)
%   * max(Expr)     - translated as max(Expr)
%   * count(Expr)   - translated as count(Expr)
%   * sum(Expr)     - translated as sum(Expr)
%   * avg(Expr)     - translated as avg(Expr)
%   * Variable      - kept as is, and ultimately instantiated with the table.field name.
%   * Expr          - arithmetic expression, translated as `(Expr)`
%
%   Example
%   ~~~
%   :- headargs_sql([group(Company),avg(Age)],S^S,G^G,R^R,Select^NST,Group^NGT,Order^NRT).
%   S=Select, Select=[Company,avg(Age)|NST]
%   G=Group, Group=[Company|NGT]
%   R=Order, Order=NRT
%   ~~~
%
%   @arg Args       List of arguments from the head of a query predicate
%   @arg S^ST       Initial difference list of arguments for the SELECT clause
%   @arg Select^NST Resulting diffence list of arguments for the SELECT clause
%   @arg G^GT       Initial difference list of arguments for the GROUP clause
%   @arg Group^NGT  Resulting diffence list of arguments for the GROUP clause
%   @arg H^HT       Initial difference list of arguments for the HAVING clause
%   @arg Having^NHT Resulting diffence list of arguments for the HAVING clause
%   @arg R^RT       Initial difference list of arguments for the ORDER clause
%   @arg Order^NRT  Resulting diffence list of arguments for the ORDER clause
headargs_sql([],S^ST,G^GT,H^HT,R^RT,S^ST,G^GT,H^HT,R^RT).
headargs_sql([A|Args],S^ST,G^GT,H^HT,R^RT,S^NST,G^NGT,H^NHT,R^NRT):-
    var(A),
    ST = [A|ST2],!,
    headargs_sql(Args,S^ST2,G^GT,H^HT,R^RT,S^NST,G^NGT,H^NHT,R^NRT).
headargs_sql([order(Expr)|Args],S^ST,G^GT,H^HT,R^RT,S^NST,G^NGT,H^NHT,R^NRT):-
    (member_dif(Expr,S^ST) -> ST=ST2; ST = [Expr|ST2]),
    RT = [Expr|RT2],!,
    headargs_sql(Args,S^ST2,G^GT,H^HT,R^RT2,S^NST,G^NGT,H^NHT,R^NRT).
headargs_sql([group(Expr)|Args],S^ST,G^GT,H^HT,R^RT,S^NST,G^NGT,H^NHT,R^NRT):-
    (member_dif(Expr,S^ST) -> ST=ST2; ST = [Expr|ST2]),
    GT = [Expr|GT2],!,
    headargs_sql(Args,S^ST2,G^GT2,H^HT,R^RT,S^NST,G^NGT,H^NHT,R^NRT).
headargs_sql([having(Expr)|Args],S^ST,G^GT,H^HT,R^RT,S^NST,G^NGT,H^NHT,R^NRT):-
    HT = [Expr|HT2],!,
    headargs_sql(Args,S^ST,G^GT,H^HT2,R^RT,S^NST,G^NGT,H^NHT,R^NRT).
headargs_sql([F|Args],S^ST,G^GT,H^HT,R^RT,S^NST,G^NGT,H^NHT,R^NRT):-
    compound(F),
    F =.. [Func,_Expr],
    (   member(Func,[min,max,count,sum,avg])
    ->  ST = [F|ST2]
    ;   ST = [(F)|ST2]
    ),!,
    headargs_sql(Args,S^ST2,G^GT,H^HT,R^RT,S^NST,G^NGT,H^NHT,R^NRT).
headargs_sql([A|Args],S^ST,G^GT,H^HT,R^RT,S^NST,G^NGT,H^NHT,R^NRT):-
    ST = [A|ST2],!,
    headargs_sql(Args,S^ST2,G^GT,H^HT,R^RT,S^NST,G^NGT,H^NHT,R^NRT).



%%  clauses_sql(+Clauses,+S^ST,+F^FT,W^WT,Select^NST,From^NFT,Where^NWT) is semidet.
%   Translates body clauses from a prolog query predicate into elements of an sql
%   query.
%
%   Example
%   ~~~
%   :- assert(table_def(person,[id,name,age],[prefix(tiny),table(people)])).
%   :- assert(table_def(employee,[name,company],[prefix(tiny)])).
%   :- clauses_sql((person(_,Name,Age),employee(Name,Company)),[Name,Age,Company|ST]^ST,F^F,W^W, Select^NST,From^NFT,Where^NWT).
%
%   :- clauses_sql((person(_,Name,Age),Age >= 16),[Name,Age|ST]^ST,F^F,W^W,Select^NST,From^NFT,Where^NWT).
%   Name = 'people.name',
%   Age = 'people.age',
%   ST = NST,
%   F = From, From = ['tiny.people'|NFT],
%   W = Where, Where = ['people.age'>=16|NWT],
%   Select = ['people.name', 'people.age'|NST] .
%   ~~~
%
%   @arg M              Context Module
%   @arg Clauses        Conjunction of body clauses
%   @arg S^ST           Initial difference list of arguments for the SELECT clause
%   @arg F^FT           Initial difference list of arguments for the FROM clause
%   @arg W^WT           Initial difference list of arguments for the WHERE clause
%   @arg Select^NST     Resulting diffence list of arguments for the SELECT clause
%   @arg From^NFT       Resulting difference list of arguments for the FROM clause
%   @arg Where^NWT      Resulting difference list of arguments for the WHERE clause

%   Conjunctive body clauses
clauses_sql(CM,(Clause,Cs),S^ST,F^FT,W^WT,Select^NST,From^NFT,Where^NWT):-
    clauses_sql(CM,Clause,S^ST,F^FT,W^WT,S2^ST2,F2^FT2,W2^WT2),
    ( WT == WT2 -> WT2a=WT2; WT2=['AND'|WT2a]), 
    clauses_sql(CM,Cs,S2^ST2,F2^FT2,W2^WT2a,Select^NST,From^NFT,Where^NWT).

%   Disjunctive body clauses
clauses_sql(CM,(Clause;Cs),S^ST,F^FT,W^WT,Select^NST,From^NFT,Where^NWT):-
    clauses_sql(CM,Clause,S^ST,F^FT,W^WT,S2^ST2,F2^FT2,W2^WT2),
    ( WT == WT2 -> WT2a=WT2; WT2=['OR'|WT2a]),   
    clauses_sql(CM,Cs,S2^ST2,F2^FT2,W2^WT2a,Select^NST,From^NFT,Where^NWT).

%   Negated body clause
clauses_sql(CM,(\+ Clause),S^ST,F^FT,W^WT,Select^NST,From^NFT,Where^NWT):-
    WT = ['NOT ('|WT2],
    clauses_sql(CM,Clause,S^ST,F^FT,W^WT2,Select^NST,From^NFT,Where^WT2a),
    WT2a = [')'|NWT].

%   Last clause
clauses_sql(CM,Clause,S^ST,F^FT,W^WT,Select^NST,From^NFT,Where^NWT):-
    Clause \= (_,_), Clause \= (_;_),
    clause_sql(CM,Clause,S^ST,F^FT,W^WT,Select^NST,From^NFT,Where^NWT).


%%  clause_sql(CM,+Clause,+S^ST,+F^FT,W^WT,Select^NST,From^NFT,Where^NWT) is semidet.
%   Translates a body clause from a prolog query predicate into elements of an sql
%   SELECT, FROM, and WHERE clause.
%
%   Example
%   ~~~
%   :- assert(table_def(person,[id,name,age],[prefix(tiny),table(people)])).
%   :- clause_sql(person(_,Name,Age),[Name,Age|ST]^ST,F^F,W^W,Select^NST,From^NFT,Where^NWT).
%   NST,From^NFT,Where^NWT).
%   Name = 'people.name',
%   Age = 'people.age',
%   ST = NST,
%   F = From, From = ['tiny.people'|NFT],
%   W = Where, Where = NWT, NWT = [],
%   Select = ['people.name', 'people.age'|NST] .   
%   ~~~
%
%   @arg M              Context Module
%   @arg Clause         The clause to be translated
%   @arg S^ST           Initial difference list of arguments for the SELECT clause
%   @arg F^FT           Initial difference list of arguments for the FROM clause
%   @arg W^WT           Initial difference list of arguments for the WHERE clause
%   @arg Select^NST     Resulting diffence list of arguments for the SELECT clause
%   @arg From^NFT       Resulting difference list of arguments for the FROM clause
%   @arg Where^NWT      Resulting difference list of arguments for the WHERE clause

%   A simple predicate clause, where the predicate functor is defined as a table
clause_sql(CM,Clause,Select^ST,From^FT,Where^WT,Select^ST,From^NFT,Where^NWT):-
    Clause =.. [Func|Args],
    CM:table_def(Func,Fields,Options),
    ( member(prefix(Pref),Options) -> true; Pref='' ),
    ( member(table(Tab),Options) -> true; Tab=Func),
    ( Pref = '' -> Table=Tab; concat_to_atom([Pref,'.',Tab],Table)),
    FT = [Table|NFT],
    args_sql(Tab,Fields,Args,Wh),
    (   Wh=[] 
    ->  ( Where=Where, NWT=WT )
    ;   append(Wh,NWT,WT)
    ).

%   membership constraint clause. ('IN' for SQL)
clause_sql(_,Clause,S^ST,F^FT,Where^WT,S^ST,F^FT,Where^NWT):-
    Clause =.. [member,Arg1,Arg2],
    maplist(nondot_str,Arg2,Arg2s),
    concat_to_atom(Arg2s,',',A2),
    format(atom(NewClause),'~w IN (~w)',[Arg1,A2]),
    WT = [NewClause|NWT].

%   between constraint clause.
clause_sql(_,Clause,S^ST,F^FT,Where^WT,S^ST,F^FT,Where^NWT):-
    Clause =.. [between,Low,High,Value],
    format(atom(NewClause),'~w BETWEEN ~w AND ~w',[Value,Low,High]),
    WT = [NewClause|NWT].

%   re_match constraint clause. 
clause_sql(_,Clause,S^ST,F^FT,Where^WT,S^ST,F^FT,Where^NWT):-
    Clause =.. [re_match,Arg1,Arg2],
    get_flag(regexp,RegexpTemplate),
    format(atom(NewClause),RegexpTemplate,[Arg2,Arg1]),
    WT = [NewClause|NWT].

%   A comparative constraint clause.
clause_sql(_,Clause,S^ST,F^FT,Where^WT,S^ST,F^FT,Where^NWT):-
    Clause =.. [Op,Arg1,Arg2],
    member(Op-Sop,['<'-'<','>'-'>','='-'=','=='-'=','>='-'>=','=<'-'<=','\\='-'<>']),
    nondot_str(Arg2,A2),
    format(atom(NewClause),'~w ~w ~k',[Arg1,Sop,A2]),
    WT = [NewClause|NWT].
   


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
    (atom(A)->T='~w = ~w';T='~w = ~k'),
    format(atom(Field),T,[Fld,A]),!,
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
    ( (atom(Atom),(Atom\=null,Atom\='NULL'),atomic_list_concat(L,'.',Atom),L=[Atom]) 
    -> atom_string(Atom,Convert)
    ; Convert=Atom
    ).

%%  concat_to_atom(+List,-Atom) is semidet.
%%  concat_to_atom(+List,+Sep,-Atom) is semidet.
%   Like `atomic_list_concat/3` but strings retain quotes and 
%   compound terms are converted to atoms
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
concat_to_atom(List,Atom):- concat_to_atom(List,'',Atom).
concat_to_atom([A|As],Sep,Result):-
    (string(A)->T='~k';T='~w'),
    format(atom(First),T,[A]),
    concat_to_atom(As,Sep,First,Result).
concat_to_atom([],_,Result,Result).
concat_to_atom([A|As],Sep,Current,Result):-
    (string(A)->T='~w~w~k';T='~w~w~w'),
    format(atom(Temp),T,[Current,Sep,A]),
    concat_to_atom(As,Sep,Temp,Result).


%%  member_dif(+Item,+List^T) is semidet.
%   Check if `Item` is a member of a difference list.
%
%   Example
%   ~~~
%   :- member(c,[a,b,c,d|T]^T).
%   true.
%   ~~~
%
%   @arg Item   The term to check against `List`
%   @arg List^T The difference list
member_dif(_,Xs^T):-Xs==T,!,fail.
member_dif(Item,[X|Xs]^T):-
    (Item=X -> true; member_dif(Item,Xs^T)).

%%  load_csv(+File,+Fields,+Options) is semidet.
%   Load the contents of a headerless csv file into the prolog database
%   as clauses of the functor specified in `Options`.
%
%   Options:
%   * functor(F)    - `F` is an atom that will be used as the predicate name for the rows read into the Prolog database
%   * table(T)      - 'T' is the table name (as used for SQL). This option is passed on to `table_def/3`.
%   * prefix(P)     - 'P' is a atom that will be used as a prefix for the table (as used in the SQL FROM clause). This option is passed on to `table_def/3`.
%   * nodef         - option to omit creation of a `table_def/3` table definition (i.e. not an SQL table import)
%   * headings(true)- true (default) means the csv has a headings (i.e. column names) row
%
%   Example (assumes existence of a file `people.csv`)
%   ~~~
%   :- load_csv('people.csv',3,[functor(person),prefix(tiny),table(people)]).
%   :- table_def(person,Fields,Options).
%   Fields = [id,name,age]
%   Options = [prefix(tiny),table(people)] .
%
%   :- person(ID,Name,Age).
%   ID = 1
%   Name = john
%   Age = 13 ;...
%   ~~~
%
%   @arg File       The csv file to load
%   @arg Fields     List of atom field names. If Fields is an Integer, it specifies 
%                   the number of csv columns and the field names may be read from first row of csv.
%   @arg Options    Options that control the fact predicates loaded into Prolog
load_csv(File,Fs,Options):-
    context_module(CM),
    load_csv(CM,File,Fs,Options).
load_csv(CM,File,Fs,Options):-
    file_path_name_ext(File,_,FName,_),
    (member(functor(Func),Options) -> true; Func=FName),
    (integer(Fs) -> (N=Fs,Fields=[]); (Fields=Fs,length(Fields,N))),
    (member(table(Table),Options) -> D1=[table(Table)]; D1=[]),
    (member(prefix(Prefix),Options) -> DefOptions=[prefix(Prefix)|D1]; DefOptions=D1),
    (member(nodef,Options)
    ->  true
    ;   (   CM:retractall(table_def(Func,_,_)),
            CM:assert(table_def(Func,Fields,DefOptions))
        )
    ),
    CM:abolish(Func/N),
    CSVops=[functor(Func),convert(true)],
    load_csv_rows(CM,File,CSVops),!,
    ( member(headings(false),Options)
    ->  true
    ;   (   functor(H,Func,N),
            H =.. [_|Headers],
            once(CM:retract(H)),
            (   Fields=[]
            ->  ( CM:retractall(table_def(Func,_,_)),
                  CM:assert(table_def(Func,Headers,DefOptions))
                )
            ;   true
            )
        )
    ).

%%  load_csv(+File,+Options) is semidet.
%   Fail loop to load rows of csv file into the prolog database.
%   For use with `load_csv/2`.

load_csv_rows(CM,Input,CSVOps):-
    csv_read_file_row(Input,Row,CSVOps),
    (Row=end_of_file -> true; CM:assertz(Row)),
    fail.
load_csv_rows(_,_,_):-!.

%%  bq(+QueryPred,+Fields,-Status,+Opts) is semidet.
%   Query the Google BigQuery database by translating the Prolog 
%   predicate `QueryPred` into SQL and sending it to BigQuery.
%   Results are automatically loaded into Prolog as fact predicates
%   `Func/N` from Opts `functor(Func)`.  If no functor is given in `Opts`
%   the `<pred>_result` convention is used, where `<pred>` is the functor
%   of the `QueryPred`.
%
%   Notes: at the moment this is coded with the assumption that 
%   BigQuery's CLI utility is installed (i.e. `bq`).  At some point
%   we'll replace that with direct calls to the BQ REST API.
%
%   Options:
%   * table(T)      - 'T' is the table name (as used for SQL). This option is passed on to `table_def/3`.
%   * prefix(P)     - 'P' is a atom that will be used as a prefix for the table (as used in the SQL FROM clause).
%                     This option is passed on to `table_def/3`.
%   * nodef         - option to omit creation of a `table_def/3` table definition (i.e. not an SQL table import)
%   * headings(true)- true (default) means the csv has a headings (i.e. column names) row. This option is passed to `load_csv/3`
%
%   Example
%   ~~~
%   :- [test_queries].
%   :- bq(adults(_,_),[name,age],Status,[]).
%   Status = 0 .
%   :- adults_result(Name,Age).
%   Name = jane,
%   Age = 22 ; ...
%   ~~~
%
%   @arg QueryPred  The prolog query predicate
%   @arg Status     0=successful, 1=error (error text in the results file Func.csv)
%   @arg Fields     Integer indicating number of result fields (columns), or a list of column headers
%   @arg Opts       Options
bq(Pred,Fields,Status,Opts):-
    context_module(CM),
    pro2sql(CM,Pred,SQL),
    ( member(functor(Func),Opts) -> true; (Pred=..[F|_], atom_concat(F,'_result',Func)) ),
    format(atom(File),'~w.csv',[Func]),
    format(atom(Command),'bq query --format=csv "~w" > ~w',[SQL,File]),
    shell(Command,Status),
    (   Status==0
    ->  load_csv(CM,File,Fields,Opts)
    ;   true
    ),!.