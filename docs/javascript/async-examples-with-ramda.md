# Async Examples with Ramda

## Fetch and Pick Keys

Imagine a collection of jedi:

<details>
<summary>Jedi DB values</summary>

```javascript
var jediDB = [
  {
    id: 1,
    name: "Yoda",
    skill: "The Force",
    level: 100,
  },
  {
    id: 2,
    name: "Ahsoka Tano",
    skill: "Lightsaber",
    level: 97,
  },
  {
    id: 3,
    name: "Aayla Secura",
    skill: "Mind trick",
    level: 99,
  },
];
```
</details>

And a function that fetches a jedi by their id:

```javascript
/**
 * Fetches a jedi by the ID.
 *
 * NOTE: Pretend this is performing an HTTP request to
 * a JSON API endpoint.
 */
function fetchJedi(id, timeMs = 256) {
  return new Promise((resolve) => {
    setTimeout(() => {
      var jedi = jediDB.find(jedi => jedi.id === id);
      resolve(jedi);
    }, timeMs);
  });
}
```

Then, using `pipe` (or `compose`), fetch the jedi and extract some of its properties:

```javascript
var fetchJediById = pipe(
  fetchJedi,
  andThen(pick(['name', 'skill'])),
);

fetchJediById(3).then(log);
//=> { name: 'Aayla Secura', skill: 'Mind trick' }
```

Or we can replace the `fetchJediById(id).then(log)` with yet another `pipe` + `andThen`:

```javascript
pipe(
  fetchJediById,
  andThen(log),
)(1);
//=> { name: 'Yoda', skill: 'The Force' }
```
