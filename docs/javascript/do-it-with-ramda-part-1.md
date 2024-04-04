# Do It With Ramda Part I

## Find users by name

Consider this array of jedi objects:

```javascript
const users = [
  { name: 'Ahsoka Tano', level: 9 },
  { name: 'Darth Vader', level: 8 },
  { name: 'Aayla Secura', level: 9 },
  { name: 'Yoda', level: 10 },
];
```

How to return the object whose name matches?

Vanilla JavaScript:

```javascript
function findByName(name, users) {
  return users.find(user => user.name === name);
}

findByName('Aayla Secura', users);
//=> { name: 'Aayla Secura', level: 9 }
```

Ramda:

```javascript
const findByName = compose(find, propEq('name'));

findByName('Aayla Secura')(users);
//=> { name: 'Aayla Secura', level: 9 }
```

* [Should 'pathEq/propEq' and 'pathSatisfies/propSatisfies' parameters' order be consistent (Ramda issue)](https://github.com/ramda/ramda/issues/2937).
