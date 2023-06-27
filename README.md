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
* No LIKE, but instead REGEXP_MATCH

The aim is that a "query predicate" (a prolog predicate intended to retrieve records from fact predicates) works the same in prolog as in SQL -- except that SQL returns a table, where prolog returns variable result alternatives. The use case is where you want to extract from a database through SQL and then further query results table in Prolog.

# Notes
* field names are always translated with the table name as prefix. We also use this to determine if an atom in a constraint (e.g. `'mytable.person' = jones`) should be translated as a string (e.g. "jones"). This method is not perfect -- there could be atoms that contain full-stops (periods) that _should_ be translated as a string.  Therefore, the recommendation is to store string values as strings in Prolog.
* TODO:
    - Add a utility to query BigQuery and load the results
    - allow aggregate functions and create an aggregation predicate that processes them in for Prolog

# Installation
For the moment, just copy the file `src\prolog\pro2sql.pro`, or clone the git repo and copy it locally.

I will create a SWI-Prolog package when I've finished my current TODO list.

# References

[1] "Draxler C (1992) Prolog to SQL Compiler, version 1.0. Technical report, CIS Centre for Information and Speech, University of Munich"

