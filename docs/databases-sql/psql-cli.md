---
title: PSQL CLI tips and useful settings :: PostgreSQL
description: Let's take a look at some useful psql cli settings and ideas which turn out to be useful when working with the sql prompt.
---

# PSQL CLI :: PostgreSQL

## Setting the Prompt

We can login into Postgres using the psql cli using a shell command similar to this:

```
$ psql -h somehost -U user -d my_proj_db_devel -W
Password:
psql (15.3, server 14.8 (Debian 14.8-1.pgdg120+1))
Type "help" for help.

my_proj_db_devel=#
```

The, `my_proj_db_devel=#' is the prompt shown.
Sometimes, though, the database name is too long, causing the prompt to end up far to the right leaving not so much room for typing a command, or some other thing one might not like about it, like the looks, etc.

Let's make our psql prompt show `SQL> ` instead:

```text
my_proj_db_devel=# \set PROMPT1 'SQL> '

SQL> SELECT ROUND(3.14159, 2) AS my_pi;
 my_pi 
-------
  3.14
(1 row)

SQL>
```

On occasion, it looks cleaner and more pleasant to look at :)

## Visually display null values

Sometimes, for debugging purposes, show visual representation of `null` values instead of just seeing an empty cell in the results.

```text
SQL> SELECT col_a, col_b
FROM tbl_a LEFT OUTER JOIN tbl_b
ON col_a = col_b;
 col_a | col_b 
-------+-------
   102 |   102
   104 |   104
   106 |   106
   107 |
(4 rows)
```

Note the last cell, to the rigth of 107 simply shows nothing at all.
Is that an empty string or really null?
Not sure...
Well, we can instruct psql to show a specific string for `null` values:


```text
SQL> \pset null '<null>'
Null display is "<null>".

SQL> SELECT col_a, col_b
FROM tbl_a LEFT OUTER JOIN tbl_b
ON col_a = col_b;

 col_a | col_b 
-------+-------
   102 |   102
   104 |   104
   106 |   106
   107 | <null>
(4 rows)
```

Now it shows `<null>` which may help in some situations.

To _unset_, make it an empty string so nothing is shown.

```text
SQL> \pset null ''
```

Note that we must use single quotes.
Double quotes mean something else:

```text
SQL> \pset null ""
Null display is """".

SQL> SELECT col_a, col_b
FROM tbl_a LEFT OUTER JOIN tbl_b
ON col_a = col_b;

 col_a | col_b 
-------+-------
   102 |   102
   104 |   104
   106 |   106
   107 |    ""
(4 rows)
```

Note it now misleadingly shows `null` as an emtpy string `""`.
Probably not what we wanted.
