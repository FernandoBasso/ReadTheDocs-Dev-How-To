# Spread Syntax
- [Spread Syntax](#spread-syntax)
  - [intro](#intro)
  - [copy object](#copy-object)
  - [copy object and add new property to it](#copy-object-and-add-new-property-to-it)
  - [copy object and overwrite property](#copy-object-and-overwrite-property)
  - [removing properties from objects](#removing-properties-from-objects)
  - [gotcha](#gotcha)


$$
a^2+b^2=c^2
$$

## intro

Spread syntax is useful for many things.
One of them is to help us with immutable data.

```js
const log = console.log.bind(console);
```

## copy object

If we do `obj2 = obj1`, then `obj2` is a reference to `obj1`, and changing one affects the other, because they are actually two references to the same object in memory.
To really make a new copy of an object, we can use the _spread syntax_ to help us out!

```js
// Create an object.
const yoda1 = { id: 1, name: 'Yoda' };

// Make a copy of the original object.
const yoda2 = { ...yoda1 };

// Change the original object.
yoda1.name = 'Master Yoda';

// Change the copy.
yoda2.id = 2;

log(yoda1, yoda2);
// [object Object] {
//   id: 1,
//   name: "Master Yoda"
// }
// [object Object] {
//   id: 2,
//   name: "Yoda"
// }
```

Pay attention to the fact that changing `yoda1.name` does not cause `yoda2.name` to change, and changing `yoda2.id` does not affect `yoda1.name`.
That means that `yoda2` is really a copy and not a reference.

## copy object and add new property to it

Create an object with two properties.

```js
const yoda1 = {
  id: 1,
  name: 'Master Yoda',
};

log(yoda1);
// [object Object] {
//   id: 1,
//   name: "Master Yoda"
// }
```

Create a new object which has all the properties of the previous object, and adds a new `skill` property.

```js
const yoda2 = {
  ...yodav1, // <1>
  skill: 'Foresight',
};

log(yodav2);
// [object Object] {
//   id: 1,
//   name: "Master Yoda",
//   skill: "Foresight"
// }

```

1. Note the use of the spread syntax *to the right* of the _assignment operator_. In that position, it “expands” the properties of the object, which are added to the new object being created.

## copy object and overwrite property

We can also copy an object, overwriting any of its original properties.

```js
const yoda3 = {
  ...yoda2,
  skill: 'Teach the ways of the force', // <1>
};

log(yoda3);
// [object Object] {
//   id: 1,
//   name: "Master Yoda",
//   skill: "Teach the ways of the force"
// }
```

1. `yoda2` already has a `skill` property, but we have overwritten the old value with a new value.

## removing properties from objects

Just to get started, let's retrieve individual properties from objects using _destructuring_ syntax.

```js
const jedi = {
  id: 1,
  name: 'Yoda',
  skill: 'The Force',
};

const { name, skill } = jedi;
log(name, skill);
// 'Yoda'
// 'The Force'
```

And we can also make an antire new object _without_ some properties of the original object by making use of *destructuring* and the *rest syntax*.

```js
const { id, ...jediNoId } = jedi; // <1>
log(jediNoId);

// [object Object] {
//   name: "Yoda",
//   skill: "The Force"
// }
```

1. This time we use the _spread syntax_ *to the left of the assignment operator*. We also _destructure_ the id (but don't care about it afterwards), which leaves all the remaining properties to end up in `jediNoId`. One downside is that now we have an `id` constant lying around in the scope where we defined it.

## gotcha

BEWARE: `const` and literal values mean you cannot assign new values to a given constant. Also, you cannot assign a new array, or object, to an existing const object or array.

```js
const str = 'foo';
str = 'bar' // Error.

const arr = [10, 20];
arr = [20, 30]; // Error.

// But this is possible:
const arr = [10, 20];
arr[2] = 30; // OK.
arr.push(40); // OK.
// arr is now [10, 20, 30, 40]

const obj = { id: 1 };
obj = { foo: 'bar' }; // Error.

// But this is possible.
const obj = { id: 1 };
obj.name = 'Yoda';
obj['skill'] = 'Foresight';
log(obj);
// [object Object] {
//   id: 1,
//   name: "Yoda",
//   skill: "Foresight"
// }
```

So, what is *constant* is the reference. Once a _const name_ is pointing to some value in memory, not even Linus Torvalds can change it afterwards. But properties of that thing in memory can be freely changed (altought there is `Object.freeze` that could be used).
