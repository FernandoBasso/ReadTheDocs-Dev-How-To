# Big-O of Arrays and Objects

Let's take a look at time complexity of array and object methods in JavaScript.

## Big O of Objects

Objects are *unordered* key/value pairs. Nice to use when order is not needed or required. Most operations on objects are fast. They have no concept of beginning, middle or end.

- Access → O(1) → direct access to each value by key.
- Insertion → O(1).
- Deletion → O(1).
- Search → O(n), linear time.

Same concrete examples:

- `Object.keys`: O(n).
- `Object.values`: O(n).
- `Object.entries`: O(n).
- `obj.hasOwnProperty`: O(1).

## Big O of Arrays

Arrays are like ordered lists. Use them when order is needed. Some operations are fast, some are not so fast.

- Access → O(1). Direct access to each value by index.
- Insertion → O(?). It depends.
  - Insert at index 0 is O(n) because it has to change the index of all other elements.
  - Insert or remove at end of array is O(1). No indexes needs to change.

`push()` is always faster then `shift()` and `unshift()` since `push()` adds to the end, while `shift()` and `unshift()` requires changing the indexes of all other elements because they operate on the beginning of the array.

`unshift(val)` adds val at position 0 and return val. Moves all other index one index to the right.

Removing at index 0 is O(n) because it has to change the index of all elements.  Removing last is O(1) because it doesn't need to change the index of other elements.

Removing at some other position is O(n) (it depends). Removing elements near the end requires less indexes changes; removing more to the beginning requires more indexes changes.
