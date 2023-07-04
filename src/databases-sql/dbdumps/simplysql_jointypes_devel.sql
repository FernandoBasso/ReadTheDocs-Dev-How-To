--
-- PostgreSQL database dump
--

-- Dumped from database version 14.8 (Debian 14.8-1.pgdg120+1)
-- Dumped by pg_dump version 14.8 (Debian 14.8-1.pgdg120+1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: simplysql_jointypes_devel; Type: DATABASE; Schema: -; Owner: devel
--

CREATE DATABASE simplysql_jointypes_devel WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.UTF-8';


ALTER DATABASE simplysql_jointypes_devel OWNER TO devel;

\connect simplysql_jointypes_devel

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

--
-- Name: simplysql_jointypes_devel; Type: DATABASE PROPERTIES; Schema: -; Owner: devel
--

ALTER DATABASE simplysql_jointypes_devel CONNECTION LIMIT = 3;


\connect simplysql_jointypes_devel

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: tbl_a; Type: TABLE; Schema: public; Owner: devel
--

CREATE TABLE public.tbl_a (
    col_a smallint NOT NULL
);


ALTER TABLE public.tbl_a OWNER TO devel;

--
-- Name: tbl_b; Type: TABLE; Schema: public; Owner: devel
--

CREATE TABLE public.tbl_b (
    col_b smallint NOT NULL
);


ALTER TABLE public.tbl_b OWNER TO devel;

--
-- Data for Name: tbl_a; Type: TABLE DATA; Schema: public; Owner: devel
--

COPY public.tbl_a (col_a) FROM stdin;
102
104
106
107
\.


--
-- Data for Name: tbl_b; Type: TABLE DATA; Schema: public; Owner: devel
--

COPY public.tbl_b (col_b) FROM stdin;
101
102
104
106
108
\.


--
-- Name: tbl_a tbl_a_pkey; Type: CONSTRAINT; Schema: public; Owner: devel
--

ALTER TABLE ONLY public.tbl_a
    ADD CONSTRAINT tbl_a_pkey PRIMARY KEY (col_a);


--
-- Name: tbl_b tbl_b_pkey; Type: CONSTRAINT; Schema: public; Owner: devel
--

ALTER TABLE ONLY public.tbl_b
    ADD CONSTRAINT tbl_b_pkey PRIMARY KEY (col_b);


--
-- PostgreSQL database dump complete
--

