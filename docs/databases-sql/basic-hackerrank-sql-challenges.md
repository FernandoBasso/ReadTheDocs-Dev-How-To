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
