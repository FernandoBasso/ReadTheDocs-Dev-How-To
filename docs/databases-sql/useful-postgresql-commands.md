---
title: Useful PostgreSQL Commands
description: Some useful commands to create and manage roles (users), databases and tables in PostgreSQL.
---

# Useful PostgreSQL Commands

:::{admonition} PG example user
:class: info

Many of these examples use a role (user) called *devel*.
:::

## Miscellaneous Commands

Get PostgresSQL version:

```sql
SELECT version();
```

Get type of a value:

```text
SELECT pg_typeof(1);
pg_typeof | integer

SELECT pg_typeof(1) as type_of_1;
type_of_1 | integer

SELECT pg_typeof(1::NUMERIC);
pg_typeof | numeric

blog1_dev=# SELECT pg_typeof('xyz');
pg_typeof | unknown

blog1_dev=# SELECT pg_typeof('xyz'::VARCHAR(8));
pg_typeof | character varying

blog1_dev=# SELECT pg_typeof('xyz'::TEXT);
pg_typeof | text
```

List pg types:

```text
SELECT oid, typname, typlen FROM pg_type;
```

See:

- [System Information Functions and Operators (pg docs)](https://www.postgresql.org/docs/14/functions-info.html)


## Roles (users)

Create role:

```sql
CREATE ROLE devel
WITH LOGIN PASSWORD 's3cr37' CREATEDB REPLICATION
VALID UNTIL 'infinity';
```

## Creating Databases

Create database:

```sql
CREATE DATABASE devel WITH
    ENCODING='UTF-8'
    OWNER=devel
    LC_CTYPE='en_US.UTF-8'
    LC_COLLATE='en_US.UTF-8'
    TEMPLATE=template0
    CONNECTION LIMIT=3;
```

Make role `devel` as powerful as postgres role:

```sql
ALTER USER devel WITH SUPERUSER;
```

Reverse the effects of the command above:

```sql
ALTER USER devel WITH NOSUPERUSER;
```

## Backup & Restore

Dump:

```shell-session
$ pg_dump -U devel -W mydb -f mydb.sql
``` 

Import:

```shell-session
psql -U devel -d mydb -f _stuff/mydb.sql
```

Restore a Heroku Postgres dump:

```shell-session
$ pg_restore -U devel -d mydb mydb.backup
```

## Renaming a Database

Make sure no user or application is connected to the database otherwise this command will not work:

```text
devel=# ALTER DATABASE blog RENAME TO blog_dev;
ERROR:  database "blog" is being accessed by other users
DETAIL:  There is 1 other session using the database.
```

After no sessions are active, it should work:

```text
devel=# ALTER DATABASE blog RENAME TO blog_dev;
ALTER DATABASE
```

Note the output is simply “ALTER DATABASE”, which means the command ran successfully.

References:

- [ALTER DATABASE PostgreSQL docs](https://www.postgresql.org/docs/current/sql-alterdatabase.html).

## Tables

PostgreSQL has `\d` and `\dt` to inspect a table.
Those are not standard SQL, but PostgreSQL specific features.
We can also use standard SQL to inspect a table (should work across many different database vendors):

```sql
SELECT 
    table_name
  , column_name 
  , data_type 
FROM 
   information_schema.columns
WHERE 
   table_name = 'users';
```

There is an overwhelming number of columns to display.
See it for yourself:

```sql
SELECT *
FROM information_schema.columns
WHERE table_name = 'users';
```
