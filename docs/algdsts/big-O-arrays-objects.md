# Big-O of Arrays and Objects

Let's take a look at time complexity of array and object methods in JavaScript.

## Big O of Objects

Objects are *unordered* key/value pairs. Nice to use when order is not needed or required. Most operations on objects are fast. They have no concept of beginning, middle or end.

- access → O(1) → direct access to each value by key;
- insertion → O(1);
- deletion → O(1);
- search → O(n), linear time

Same concrete examples:

- `Object.keys`: O(n)
- `Object.values`: O(n)
- `Object.entries`: O(n)
- `obj.hasOwnProperty`: O(1)

## Arrays

Arrays are like ordered lists. Use them when order is needed. Some operations are fast, some are not so fast.

- Access → O(1). Direct access to each value by index.
- Insertion → O(?). it depends.
  - Insert at index 0 is  O(n) because it has to change the index of all other elements.
  - Insert or remove at end of array is O(1). No indexes needs to change.

- `push()` is always faster then `shift()` and `unshift()` since `push()` adds to the end, while `shift()` and `unshift()` requires changing the indexes of all other elements.
- `unshift(val)` adds val at position 0 and return val. Moves all other index one index to the right.
- Remove at index 0 is O(n) because it has to change the index of all elements.
- Remove last is O(1) because it doesn't need to change the index of other elements.
- Remove at some other position is O(n) (it depends). More to the end requires less indexes changes, more to the beginning requires more indexes changes.
- `shift()` removes and return val at position 0.
