--
-- PostgreSQL database dump
--

-- Dumped from database version 14.8 (Debian 14.8-1.pgdg120+1)
-- Dumped by pg_dump version 15.3

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

CREATE DATABASE simplysql_jointypes_devel WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE_PROVIDER = libc LOCALE = 'en_US.UTF-8';


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

--
-- Name: public; Type: SCHEMA; Schema: -; Owner: devel
--

-- *not* creating schema, since initdb creates it


ALTER SCHEMA public OWNER TO devel;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: a; Type: TABLE; Schema: public; Owner: devel
--

CREATE TABLE public.a (
    a smallint NOT NULL
);


ALTER TABLE public.a OWNER TO devel;

--
-- Name: b; Type: TABLE; Schema: public; Owner: devel
--

CREATE TABLE public.b (
    b smallint NOT NULL
);


ALTER TABLE public.b OWNER TO devel;

--
-- Data for Name: a; Type: TABLE DATA; Schema: public; Owner: devel
--

COPY public.a (a) FROM stdin;
102
104
106
107
\.


--
-- Data for Name: b; Type: TABLE DATA; Schema: public; Owner: devel
--

COPY public.b (b) FROM stdin;
101
102
104
106
108
\.


--
-- Name: a a_pkey; Type: CONSTRAINT; Schema: public; Owner: devel
--

ALTER TABLE ONLY public.a
    ADD CONSTRAINT a_pkey PRIMARY KEY (a);


--
-- Name: b b_pkey; Type: CONSTRAINT; Schema: public; Owner: devel
--

ALTER TABLE ONLY public.b
    ADD CONSTRAINT b_pkey PRIMARY KEY (b);


--
-- Name: SCHEMA public; Type: ACL; Schema: -; Owner: devel
--

REVOKE USAGE ON SCHEMA public FROM PUBLIC;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

