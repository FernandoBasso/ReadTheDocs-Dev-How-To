---
title: Useful PostgreSQL Commands
description: Some useful commands to create and manage roles (users), databases and tables in PostgreSQL.
---

# Useful PostgreSQL Commands

:::{admonition} PG example user
:class: info

Many of these examples use a role (user) called *devel*.
:::

Create role:

```sql
CREATE ROLE devel
WITH LOGIN PASSWORD 's3cr37' CREATEDB REPLICATION
VALID UNTIL 'infinity';
```

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

Make use as powerful as postgres user:

```sql
ALTER USER devel WITH SUPERUSER;
```

Reverse the command above:

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
