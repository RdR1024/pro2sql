# pro2sql

A lightweight Prolog to SQL SELECT translator.

## Introduction
This module is a minimal compiler of prolog "query predicates" to sql SELECT statements. I'm inspired by Draxler's work [1][], but I couldn't access his original paper, and it was too much work trying to use Mungall's implementation without much documentation. It seemed to me that it would be less work to implement a minimalist translator:

* Automatically translates constraints like Age > 16, etc.
* Autmatically translates `member/2` translates to `... IN List`
* Automatically translates SWI-Prolog `between/3`
* Does "joins" through shared variable names between predicates
* No LIKE, but instead REGEXP_CONTAINS (used in Google SQL). Configurable for other SQLs with `set_flag(regexp,Template)`.
* Only implements translation to SELECT...  (no UPDATE, etc.)
* No subqueries. Generate subqueries as result tables first and then use as joins.
* Includes a simple utility predicate that will translate a prolog query predicate, send it to Google BigQuery, and load the
  results as fact predicates.

The aim is that a "query predicate" (a prolog predicate intended to retrieve results from fact predicates) works the same in prolog as in SQL -- except that SQL returns a table, where prolog returns variable result alternatives. 

The use case is where you want to extract from a database through SQL and then further query the results in Prolog.

## Installation
In SWI-Prolog, `package_install(pro2sql).`

Otherwise, clone the git repository, and copy the files from `...pro2sql/src/prolog/`

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
  in a constraint (e.g. `'mytable.person' = jones`) should be translated as a string (e.g. "jones"). This method is not perfect -- there could be atoms that contain full-stops (periods) that _should_ be translated as a string.  Therefore, the recommendation is to store string values as strings in Prolog.
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
