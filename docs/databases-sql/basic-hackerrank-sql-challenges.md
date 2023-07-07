---
title: Basic HackerRank SQL Challenges
description: Notes and solutions on the HackerRank SQL challenges for MySQL/MariaDB.
---

# Baisc HackerRank SQL Challenges

## Intro

Unless otherwise noted, assume these examples where tested on MySQL and MySQL CLI. I prefer MariaDB and PostgreSQL by it seems HackerRank does not provide any of those (as of June 18, 2023 at least).

```{note}
RackerRank SQL challenges come in three main levels of difficulty:

- Basic
- Intermediate
- Advanced

This page deals with the basic ones.

```

![HackerRank SQL MySQL / MariaDB Dropdown Option](/staticassets/hackerrank-sql-mysql-db-dropdown.png)

## Revising The Select Query I

- [Revising the Select Query I :: HackerRank SQL Challenge](https://www.hackerrank.com/challenges/revising-the-select-query).

```sql
SELECT
    id
  , name
  , countrycode
  , district
  , population
FROM city
WHERE population >= 100000
AND countrycode = 'USA';
```

We can replace all column names with the star `*`, but this is more readable as all column names are explicitly stated in the query.

Also, we could use `LIKE` in place of `=` for this case: `… AND countrycode LIKE 'USA'`.

## Revising the Select Query II

- [Revising the Select Query II :: HackerRank SQL Challenge](https://www.hackerrank.com/challenges/revising-the-select-query-2).

```sql
SELECT name
FROM city
WHERE population > 120000
AND countrycode LIKE 'USA';
```

## Select All

- [Select All :: HackerRank SQL Challenge](https://www.hackerrank.com/challenges/select-all-sql).

```sql
SELECT
    id
  , name
  , countrycode
  , district
  , population
FROM city;
```

Again, writing the column names explicitly makes it more self-documenting than simply using `*`.

## Select By ID

- [Select By ID :: HackerRank SQL Challenge](https://www.hackerrank.com/challenges/select-by-id).

```sql
SELECT
    id
  , name
  , countrycode
  , district
  , population
FROM city
WHERE id = 1661;
```

## Japanese Cities' Attributes

- [Japanese Cities' Attributes :: HackerRank SQL Challenge](https://www.hackerrank.com/challenges/japanese-cities-attributes).

```sql
SELECT name
FROM city
WHERE countrycode = 'JPN';
```

## Weather Observation Station 1

- [Weather Observation Station 1 :: HackerRank SQL Challenge](https://www.hackerrank.com/challenges/weather-observation-station-1).

```sql
SELECT
    city
  , state
FROM station;
```

## Weather Observation Station 2

- [Weather Observation Station 2 :: HackerRank SQL Challenge](https://www.hackerrank.com/challenges/weather-observation-station-2).

```
SELECT
    ROUND(SUM(lat_n), 2) AS lat
  , ROUND(SUM(long_w), 2) AS lon
FROM station;
```

So we basically round the result of the sum and rename the output columns as `lat` and `lon`.

We can `SUM` any numeric column, e.g.:

```
SELECT SUM(id) FROM users;
```

And also `ROUND(value, num_decimal_places)`, e.g.:

```
mydb=# SELECT ROUND(3.141592653589793, 5) AS my_pi;
  my_pi
---------
 3.14159
```

## Weather Observation Station 3

- [Weather Observation Station 3 :: HackerRank SQL Challenge](https://www.hackerrank.com/challenges/weather-observation-station-3).

```sql
SELECT DISTINCT city
FROM station
WHERE id % 2 = 0;
```

We just `SELECT DISTINCT` to avoid duplicates.

For the “even IDs”, the old, battle-tested tested integer division compared with 0 does the trick. It seems `%` has higher precedence than `=`, but we could wrap the remainder division in parentheses too if we wanted to make sure or make it more explicit/clear:

```sql
SELECT DISTINCT city
FROM station
WHERE (id % 2) = 0;
```

## Weather Observation Station 4

- [Weather Observation Station 4 :: HackerRank SQL Challenge](https://www.hackerrank.com/challenges/weather-observation-station-4).

```sql
SELECT (COUNT(city) - COUNT(DISTINCT city)) AS count
FROM station;
```

Looks like they consider only the city name (`city`) column to determine if the city is the same or not.

`COUNT(city)` returns some number, and `COUNT(DISTINCT city)` can potentially return another number, which we subtract from the first, producing the correct result expected by the challenge.

Note we didn't call `DISTINCT` as a function, with parentheses, like `DISTINCT(city)`.

Assume this table and data:

```sql
CREATE TABLE users (
    id INTEGER PRIMARY KEY
  , name VARCHAR(128) NOT NULL
);

INSERT INTO users (
    id
  , name
) VALUES
    (1, 'Yoda')
  , (2, 'Ahsoka tano')
  , (3, 'Aayla Secura')
  , (4, 'Leia')
  , (5, 'Leia');
```

Then see how it is possible to use both `DISTINCT name` or `DISTINCT(name)` inside `COUNT()`:

```text
> SELECT id, name FROM users;
 id |     name     
----+--------------
  1 | Yoda
  2 | Ahsoka tano
  3 | Aayla Secura
  4 | Leia
  5 | Leia
(5 rows)

> SELECT COUNT(name) FROM users;
 count 
-------
     5
(1 row)

                 parentheses
               --------------
> SELECT COUNT(DISTINCT(name)) FROM users;
 count 
-------
     4
(1 row)

               no parentheses
               -------------
> SELECT COUNT(DISTINCT name) FROM users;
 count 
-------
     4
(1 row)

                                parentheses
                              --------------
> SELECT (COUNT(name) - COUNT(DISTINCT(name))) AS count FROM users;
 count 
-------
     1
(1 row)

                              no parentheses
                              -------------
> SELECT (COUNT(name) - COUNT(DISTINCT name)) AS count FROM users;
 count 
-------
     1
(1 row)

```

I tested this on both PostgreSQL and MariaDB and both vendors accept both syntaxes.

## Weather Observation Station 6

- [Weather Observation Station 6 :: HackerRank SQL Challenge](https://www.hackerrank.com/challenges/weather-observation-station-6).

Works on DB vendors whose collation allows case insensitive matches:

```sql
SELECT DISTINCT city
FROM station
WHERE
     city LIKE 'a%'
  OR city LIKE 'e%'
  OR city LIKE 'i%'
  OR city LIKE 'o%'
  OR city LIKE 'u%';
```

This approach extracts the first char of the `title` column and uses `IN` to compare.
It relies on the fact that cities start with an uppercase letter:

```sql
SELECT DISTINCT title
FROM entries
WHERE
  SUBSTR(title, 1, 1)
  IN('A', 'E', 'I', 'O', 'U');
```

And this one, similar to the above, just lowercases the first char of the title before doing the comparison:

```sql
SELECT DISTINCT title
FROM entries
WHERE
  LOWER(SUBSTR(title, 1, 1))
  IN('a', 'e', 'i', 'o', 'u');
```
