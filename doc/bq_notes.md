# BigQuery CLI ( bq ) notes

# Introduction
These are some notes and sample queries for the Google BigQuery commandline tool (`bq`).

## Ensure you have a Google Cloud account and the Command Line Interface

If you don't yet have a Google Could account (which you need to access BigQuery), use this guide (https://k21academy.com/google-cloud/create-google-cloud-free-tier-account/) to create one.

Install the command line interface (CLI), following these instructions: https://cloud.google.com/sdk/docs/install

NOTE: to install in the Windows WSL (i.e. linux on windows), I found that I needed to install `xdg-utils` and `wslu` first. After that, in linux set an environment variable `BROWSER=wslview` (obviously, you'll want to do this in `.bashrc` so that it is permanent).

You should be able to use the command `bq` from your commandline to query.


## Make a new dataset in the current project and upload some data

~~~
bq make tiny
bq load --autodetect tiny.people people.csv
bq load --skip_leading_rows=1 tiny.employee employee.csv name:string,company:string
~~~

# To remove a table

~~~
bq rm -t tiny.people
~~~


## Get the list of columns from a table

~~~
bq query --format=csv --nouse_legacy_sql 'select column_name, data_type from gdelt-bq.gdeltv2.INFORMA
TION_SCHEMA.COLUMNS where table_name="events"'
~~~

## Execute SQL from a file with output to another file

~~~
bq query --format=csv --nouse_legacy_sql < myquery.sql > myresults.csv
~~~

## Make --use_legacy_sql=false the default

Add the following line in `~/.bashrc`
~~~
export BIGQUERYRC='~/.bigqueryrc'

~~~

Then edit `~/.bigqueryrc` and add the following lines
~~~
[query]
--use_legacy_sql=false
~~~


## Adding rows to an existing table

~~~
bq query 'insert into tiny.people values (6,"karen",42),(7,"sheila",50)'
~~~

## aggregation sql, incl. GROUP BY

~~~
bq query 'select employee.company, avg(people.age) as avg_age from tiny.people, tiny.employee where people.name = employee.name group by employee.company'
~~~
