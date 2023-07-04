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
-- Name: simplysql_cms_devel; Type: DATABASE; Schema: -; Owner: devel
--

CREATE DATABASE simplysql_cms_devel WITH TEMPLATE = template0 ENCODING = 'UTF8' LOCALE = 'en_US.UTF-8';


ALTER DATABASE simplysql_cms_devel OWNER TO devel;

\connect simplysql_cms_devel

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
-- Name: simplysql_cms_devel; Type: DATABASE PROPERTIES; Schema: -; Owner: devel
--

ALTER DATABASE simplysql_cms_devel CONNECTION LIMIT = 3;


\connect simplysql_cms_devel

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
-- Name: categories; Type: TABLE; Schema: public; Owner: devel
--

CREATE TABLE public.categories (
    category character varying(9) NOT NULL,
    name character varying(37) NOT NULL
);


ALTER TABLE public.categories OWNER TO devel;

--
-- Name: entries; Type: TABLE; Schema: public; Owner: devel
--

CREATE TABLE public.entries (
    id integer NOT NULL,
    title character varying(99) NOT NULL,
    created timestamp without time zone NOT NULL,
    updated timestamp without time zone,
    category character varying(37),
    content text
);


ALTER TABLE public.entries OWNER TO devel;

--
-- Data for Name: categories; Type: TABLE DATA; Schema: public; Owner: devel
--

COPY public.categories (category, name) FROM stdin;
blog	Log on to My Blog
humor	Humorous Anecdotes
angst	Stories from the Id
advice	Gentle Words of Advice
science	Our Spectacular Universe
\.


--
-- Data for Name: entries; Type: TABLE DATA; Schema: public; Owner: devel
--

COPY public.entries (id, title, created, updated, category, content) FROM stdin;
423	What If I Get Sick and Die?	2008-12-30 00:00:00	2009-03-11 00:00:00	angst	\N
537	Be Nice to Everybody	2009-03-02 00:00:00	\N	advice	\N
573	Hello Statue	2009-03-17 00:00:00	\N	humor	\N
598	The Size of Our Galaxy	2009-04-03 00:00:00	\N	science	\N
524	Uncle Karl and the Gasoline	2009-02-28 00:00:00	\N	humor	When I was about nine or ten, my Uncle Karl, who\nwould've been in his late teens or early twenties, once performed\nwhat to me seemed like a magic trick.\n\nUsing a rubber hose, which he snaked down into the gas tank of my \nfather's car, he siphoned some gasoline into his mouth, lit a match, \nheld it up a few inches in front of his face, and then, with explosive \nforce, sprayed the gasoline out towards the lit match.\n\nOf course, a huge fireball erupted, much to the delight of the kids\nwatching. I don't recall if he did it more than once.\n\nThe funny part of this story? We lived to tell it.\n\nKarl was like that.
\.


--
-- Name: categories categories_pkey; Type: CONSTRAINT; Schema: public; Owner: devel
--

ALTER TABLE ONLY public.categories
    ADD CONSTRAINT categories_pkey PRIMARY KEY (category);


--
-- Name: entries entries_pkey; Type: CONSTRAINT; Schema: public; Owner: devel
--

ALTER TABLE ONLY public.entries
    ADD CONSTRAINT entries_pkey PRIMARY KEY (id);


--
-- PostgreSQL database dump complete
--

