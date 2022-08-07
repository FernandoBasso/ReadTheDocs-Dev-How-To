CREATE ROLE blog1
WITH LOGIN PASSWORD 'blog1' REPLICATION
VALID UNTIL 'infinity';

CREATE DATABASE blog1_dev WITH
    ENCODING='UTF8'
    OWNER=blog1
    LC_CTYPE='en_US.UTF-8'
    LC_COLLATE='en_US.UTF-8'
    TEMPLATE=template0
    CONNECTION LIMIT=3;

\c blog1_dev;

CREATE TABLE users (
    id INTEGER PRIMARY KEY
  , name VARCHAR(128) NOT NULL
);

INSERT INTO users (
    id
  , name
) VALUES
    (1, 'User 1')
  , (2, 'User 2')
  , (3, 'User 3');
