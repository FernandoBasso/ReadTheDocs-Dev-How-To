---
title: Databases and SQL
description: Notes on designing, managing and using databases and SQL query language.
---

# Databases and SQL

SQL stands for Structured Query Language.
No mater which (relational) database vendor being used (PostgreSQL, MariaDB, etc.), the language to work with the data and other aspects is the SQL language.
SQL does much more than just *query* data, though.

It is common to pronounce SQL as three letters S Q L (and not as the word “sequel”), which means we say “*an* SQL statement” rather than “*a* SQL (sequel) statement”.

Some notes, concepts, ideas, examples and guides on how to do work with:

- Data Definition Language (DDL).
- Data Manipulation Language (DML).
- Data Control Language (DCL). 
- A few other types of queries.

I use mostly PostgreSQL and MariaDB, so expect most examples to work on those.

SQL is a standardized language (even though each vendor adds their own features, commands and other facilities) on top of the standard language.
The standard is **NOT** free (like HTML, CSS or ECMAScript, among others).
One has to purchase it on [their website](https://www.iso.org/standard/63555.html).
Thankfully, we can be learn for free on the web by reading DB vendors documentation and doing online challenges.
Same resources:

- [PostgreSQL docs](https://www.postgresql.org/docs/current/index.html).
- [MariaDB docs](https://mariadb.com/kb/en/).
- [Codewars SQL for Beginners](https://www.codewars.com/collections/sql-for-beginners).
- [HackerRank SQL challenges](https://www.hackerrank.com/domains/sql).


## Basics

SQL statements are composed of *keywords* (defined in the standard), *identifiers* (names of tables, columns, views, etc., defined by the DB user or admin), and *constants*.

```sql
SELECT
    id
  , name
  , skill
  , power
FROM jedis
WHERE power >= 78
;
```

We wrote the keywords in UPPERCASE (common practice).
The identifiers are `jedis` (table name) and `id`, `name`, `skill` and `power` (column names).
78 is a numeric constant.

`>=` is an operator, which is a special kind of keyword.

```{toctree}
---
hidden: true
maxdepth: 6
caption: Databases and SQL
---

useful-postgresql-commands.md
basic-sql-1.md
```
