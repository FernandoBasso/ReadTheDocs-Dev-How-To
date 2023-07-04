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

### Intro Notes

`pg_dump` can dump data in a few different formats.
SQL (plain text) dump output must be fed back into `psql`.
The “other formats” should be fed to `pg_restore`.

Also:

> It is not guaranteed that `pg_dump`'s output can be loaded into a server of an older major version — not even if the dump was taken from a server of that version. 
> Loading a dump file into an older server may require manual editing of the dump file to remove syntax not understood by the older server.
> -- [pg_dump v15 docs](https://www.postgresql.org/docs/current/app-pgdump.html)

### From local database

Dump a local database in plain text (SQL) format:

```shell-session
$ pg_dump -U devel -W -C -Fp mydb -f mydb.sql
``` 

Import a plain text (SQL) dump:

```shell-session
psql -U devel -d mydb -f _stuff/mydb.sql
```

Restore a Heroku Postgres dump:

```shell-session
$ pg_restore -U devel -d mydb mydb.backup
```

## From Docker Container

One approach is to use `pg_dump` from the host machine, if available.

```
$ pg_dump -h <host> -U <user> -C -Fp mydb --file ./mydb.sql
```

But if `pg_dump` version is older than the one on the server, it will refuse to try to dump the data from a newer version, or some other incompatibilities (see pg_dump docs).
If it is newer, it could introduce syntax and options that would then fail to be fed back to a server running an older version, thus requiring manual fixes on the dumped SQL.

Another approach is to use `docker exec` and dump the database using the container’s version of `pg_dump`.
This way, `pg_dump` does NOT need to be installed on the host machine and it will certainly match the version of the PostgreSQL server running on the container, reducing the likelihood of problems and incompatibilities.

```
$ docker exec postgresql14-playground-1 pg_dump --version
pg_dump (PostgreSQL) 14.8 (Debian 14.8-1.pgdg120+1)
```

So, we can do something like this:

```
$ docker exec pgsql-container \
    pg_dump -U devel -C -Fp mydb \
    | tee ./mydb.sql
```

We could also replace the _pipe_ and `tee` with a redirection:

```
$ docker exec pgsql-container \
    pg_dump -U devel -C -Fp mydb \
    > ./mydb.sql
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
