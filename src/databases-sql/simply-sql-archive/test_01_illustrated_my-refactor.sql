/*  Chapter 3: The FROM Clause  */

/*

NOTE: I decided to rename the tables and columns names
in an attempt to avoid all those A a B b stuff from the
example queries:

    SELECT a, b
    FROM a INNER JOIN b
    ON a = b;

Now, the queries will look like this instead:

    SQL> SELECT col_a, col_b
    FROM tbl_a INNER JOIN tbl_b
    ON col_a = col_b;

    col_a | col_b
    -------+-------
      102 |   102
      104 |   104
      106 |   106
    (3 rows)

*/

/*  Create and populate a pair of test tables to
    illustrate the various types of join
*/

CREATE TABLE tbl_a
( col_a   SMALLINT   NOT NULL PRIMARY KEY )
;
INSERT INTO tbl_a
VALUES
  (102),(104),(106),(107)
;
CREATE TABLE tbl_b
( col_b   SMALLINT   NOT NULL PRIMARY KEY )
;
INSERT INTO tbl_b
VALUES
  (101),(102),(104),(106),(108)
;
