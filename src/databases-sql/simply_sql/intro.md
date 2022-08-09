# 01 Intro | Simply SQL

## DDL

```sql
CREATE DATABASE simply_sql_dev
    WITH
    OWNER = devel
    ENCODING = 'UTF8'
    LC_COLLATE = 'en_US.UTF-8'
    LC_CTYPE = 'en_US.UTF-8'
    TABLESPACE = pg_default
    CONNECTION LIMIT = 4
    IS_TEMPLATE = False;

COMMENT ON DATABASE simply_sql_dev
    IS 'A database for studying the book Simply SQL.';
```

```text
devel=# \c simply_sql_dev;
```

```sql
CREATE TABLE teams 
(
    id          INTEGER       NOT NULL PRIMARY KEY
  , name        VARCHAR(37)   NOT NULL
  , conference  VARCHAR(2)    NULL
);
```

Note that the `ALTER` statement is making use of a `DROP` clause:

```sql
ALTER TABLE teams DROP COLUMN conference;
```

And here we have a `DROP` statement:

```sql
DROP TABLE teams;
```

## DML

