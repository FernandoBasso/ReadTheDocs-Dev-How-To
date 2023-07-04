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
-- Name: simplysql_teamsgames_devel; Type: DATABASE; Schema: -; Owner: devel
--

CREATE DATABASE simplysql_teamsgames_devel WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.UTF-8';


ALTER DATABASE simplysql_teamsgames_devel OWNER TO devel;

\connect simplysql_teamsgames_devel

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
-- Name: simplysql_teamsgames_devel; Type: DATABASE PROPERTIES; Schema: -; Owner: devel
--

ALTER DATABASE simplysql_teamsgames_devel CONNECTION LIMIT = 3;


\connect simplysql_teamsgames_devel

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
-- Name: teams; Type: TABLE; Schema: public; Owner: devel
--

CREATE TABLE public.teams (
    id integer NOT NULL,
    name character varying(37) NOT NULL,
    conference character varying(2)
);


ALTER TABLE public.teams OWNER TO devel;

--
-- Data for Name: teams; Type: TABLE DATA; Schema: public; Owner: devel
--

COPY public.teams (id, name, conference) FROM stdin;
37	Havoc	F
63	Brewers	C
9	Riff Raff	E
\.


--
-- Name: teams teams_pkey; Type: CONSTRAINT; Schema: public; Owner: devel
--

ALTER TABLE ONLY public.teams
    ADD CONSTRAINT teams_pkey PRIMARY KEY (id);


--
-- PostgreSQL database dump complete
--

