--
-- Find two actors who cast together the most and list the titles of those
-- movies. Order the result set alphabetically.
--

WITH most_acted_together AS (
  SELECT
	  fa1.actor_id AS a1_id
	, fa2.actor_id AS a2_id
  FROM film_actor fa1
  INNER JOIN film_actor fa2
    ON fa1.film_id = fa2.film_id
    WHERE fa1.actor_id <> fa2.actor_id
  GROUP BY fa1.actor_id, fa2.actor_id
  ORDER BY COUNT(fa1.film_id) DESC LIMIT 1
)
SELECT
    (SELECT CONCAT(first_name, ' ', last_name)
	   FROM actor
	   WHERE actor_id = mat.a1_id) AS first_actor
  , (SELECT CONCAT(first_name, ' ', last_name)
       FROM actor
       WHERE actor_id = mat.a2_id) AS second_actor
  , f.title AS title
FROM most_acted_together mat
  INNER JOIN film_actor fa1
    ON mat.a1_id = fa1.actor_id
  INNER JOIN film_actor fa2
    ON mat.a2_id = fa2.actor_id
  INNER JOIN film f
    ON fa1.film_id = f.film_id
  AND fa2.film_id = f.film_id
ORDER BY first_actor, second_actor, title ASC;

--
--  "first_actor"       "second_actor"       "title"
--  "Jennifer Davis"    "Penelope Guiness"   "Anaconda Confessions"
--  "Jennifer Davis"    "Penelope Guiness"   "Angels Life"
--
