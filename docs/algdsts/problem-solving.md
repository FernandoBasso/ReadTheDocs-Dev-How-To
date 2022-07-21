---
title: Problem Solving | Algorithms and Data Structures in TypeScript and JavaScript
description: Some tips, ideas and examples on how to apply problem-solving approaches and how to think about programming problems and algorithms in general.
---

# Problem-Solving

**NOTE**: Many of these ideas come from the book [How to Solve It](https://en.wikipedia.org/wiki/How_to_Solve_It) by George Pólya.

Solving a problem you don't know how to solve:

- How to make it solvable?
- Apply some ideas, patterns, approaches, strategies that help solve problems.
- Think of steps to make it easier.

Almost everything that we do in programming involves some kind of algorithm.
It is the foundation for being a successful problem-solver and software developer/programmer.

How to improve?

- Devise a plan for solving problems.
- Master common problem-solving patterns.
- Practice!
- Practice!!
- Practice!!!
- Wax on, wax off.

Problem Solving:

- Explore concrete examples.
- Break it down (outline a solution in text or some pseudo-code)
- Solve or simplify and solve a simplified version first.
- Review, think about the lessons learned.
- Refactor and improve if possible.

Understand the problem first. Don't jump right in just because you are under time constraint.

A “sum” function of two numbers may not be so simple...

```shell-session
$ deno repl
Deno 1.23.4
> 8e256 + 8e256
1.6e+257
> 8e256 * 8e256
Infinity
> 0.1 + 0.2
0.30000000000000004
```

## Understand The Problem

1. Restate the question/problem on your own words.
2. What about the inputs?
3. What about the outputs?
4. Can the outputs be determined from the inputs?
   Do I have enough information to solve this problem?
6. How should I label the important pieces of data?

## Concrete Examples

- Examples help understand the problem better.
    - simple
    - complex
    - edge cases (empty inputs, invalid input, out of range, etc.)
- Examples are useful as sanity checks that the solution work.
  I always do this through TDD.

### Frequency Counter Example

Problem: Write a function that takes a string and return a frequency count of each character.

```
f("hi");
// → { h: 1, i: 1 }

f("hello");
// → { h: 1, e: 1, l: 2, o: 1 }

f("Racecar");
// → What about R vs r?

f(null);
// → {}

f("");
// → {}
```

- Should we handle only alphabetic, ASCII chars?
- Should we consider other symbols like spaces, dollar symbol, numbers, etc.?
- Should we consider uppercase and lowercase to be different?
- How to handle empty input? What should we return in such cases?
- How to handle invalid input? What should we return in such cases?

## Break It Down

Outline the algorithm to be used, some pseudo-code or textual ideas **before writing any actual code**.

### v1

You may start with something like this:

- Initialize empty frequencies object to return at the end.
- Loop over the input string checking each character.
- Return object.

Hmm. “Check each character” seems to be where the bulk of the work will concentrate.
Let's add more details to that part.

Let's assume we are to handle only alphanumeric `[A-Za-z0-9]` and ignore any other character.

### v2

- Initialize empty frequencies object to return at the end.
- Loop over the input string checking each character.
    - If char is not alphanumeric, don’t do anything.
    - If the char is alphanumeric:
        - Lowercase the char.
        - If that char is not in frequencies, then add it with as key with value 1.
        - If that char is found in frequencies, increment its value by 1.
- Return frequencies object.

## Solve the problem (or a simplified version of it)

If the problem seems unsurmountable to you, try to solve a simplified version of it first.
You can initially leave of the edge cases, ignore time and space complexity, and/or any other fancy stuff that seems to make the problem harder.

This is what the book Think Like a Programmer calls “reducing the problem”.
It means “make it simpler”.

By solving simpler version of it, you also gain insights into the harder, real problem you are trying to solve.

For the char frequency count problem described above, we could initially count all characters, including non alphanumeric ones, and consider uppercase and lowercase letters to be different.
Then, later we could improve the solution and make it consider uppercase and lowercase letters to be the same, followed by making sure we ignore non alphanumeric chars.

### charCount() example step by step

Let's suppose we want to count the chars in the string "Hi. Are you there?".
We want to collect the frequency of alphanumeric characters only, ignoring any other character that is not alphanumeric.

#### v1 Simplest Scenario

The first attempt just implements the very basic loop and counts each character indiscriminately.
We start with a simple test case that doesn't include uppercase or non-alphanumeric characters.

```js
const { charCount } = require('./charCount-v1');

describe('charCount()', () => {
  it('should work with lowercase alphabetic strings', () => {
    expect(charCount('hello')).toEqual({
      h: 1,
      e: 1,
      l: 2,
      o: 1,
    });
  }) ;
});
```

```js
function charCount(s) {
  const freq = {};

  for (let i = 0; i < s.length; ++i) {
    let chr = s[i];

    if (freq[chr] === undefined)
      freq[chr] = 1;
    else
      freq[chr] += 1;
  }

  return freq;
}
```

This first version has one test for a simple, single word, lowercase alphabetic string and the logic inside the function just counts every character (be it alphanumeric or not).
If we pass it a string with punctuation, spaces, and other non alphanumeric characters, it would count those.
Uppercase and lowercase chars would be considered different and counted separately too.

#### v2 Uppercase and Lowercase

With this second version, we also handle uppercase and lowercase characters as the same.
We achieve this by lowercasing the character.

```js
describe('charCount()', () => {
  //
  // ...previous test case...
  //

  it('should handle input in a case insensitive way', () => {
    expect(charCount('Racecar')).toEqual({
      r: 2, // R and r.
      a: 2,
      c: 2,
      e: 1,
    });
  });
});
```

```js
function charCount(s) {
  const frequencies = {};

  for (let i = 0; i < s.length; ++i) {
    let c = s[i].toLowerCase();

    if (frequencies[c] === undefined)
      frequencies[c] = 1;
    else frequencies[c] += 1;
  }

  return frequencies;
}
```

#### v3 Ignore Non-Alphanumeric Chars

We now reach a complete implementation.
We ignore non-alphanumeric characters using a regular expression.

```js
describe('charCount()', () => {
  //
  // ...previous test cases...
  //

  it('should ignore non-alphanumeric chars', () => {
    //
    // Should ignore the space " " and "!".
    //
    expect(charCount('Hi there!')).toEqual({
      h: 2,
      i: 1,
      t: 1,
      e: 2,
      r: 1,
    });
  });

  it('should work with strings including digits', () => {
    expect(charCount('3-way handshake (SYN, SYN-ACK, ACK)')).toEqual({
      3: 1,
      a: 5,
      c: 2,
      d: 1,
      e: 1,
      h: 2,
      k: 3,
      n: 3,
      s: 3,
      w: 1,
      y: 3,
    });
  });
});
```

```js
function charCount(s) {
  const frequencies = {};

  for (let i = 0; i < s.length; ++i) {
    let c = s[i].toLowerCase();

    if (!/[a-z0-9]/.test(c)) continue;

    if (frequencies[c] === undefined)
      frequencies[c] = 1;
    else frequencies[c] += 1;
  }

  return frequencies;
}
```

#### v4 Refactor Regexp Bit Into Helper Function

The `if` handling the regexp test is probably simple enough, but it is a good approach to extract it into a helper function.
We can give the function a name, making it much more self-documenting, add JSDoc to the function, test it if we want, etc.

```{admonition} Should I Unit-Test Helper Functions‽
:class: info

There are two schools of though regarding testing helper functions used for specific purposes.
If a function is generic and reused many times in a project, it is recommended to test and document it as thoroughly ans humanly possible.

However, if it is used for a specific purpose in a single place or module, some people argue it does not need (or should) be tested separately because it is implicitly tested with the code that uses it.
```

```js
/**
 * Checks whether the `c` is an alphanumeric character.
 *
 * @param {string} A 1-character string.
 * @return {boolean}
 */
function isAlphaNum(c) {
  return /[a-z0-9]/.test(c);
}

/**
 * Count the frequency of alphanumeric chars in the input string.
 *
 * This third version finally makes sure we ignore non-alphanumeric
 * characters through the use of the `/[a-z0-9]/`  regexp.
 *
 * @param {string} s
 * @return {object} An object whose keys are the char counted
 *   and the value is the number occurrences of that char.
 *
 * @example
 * charCount("racecar");
 * // → { r: 2, a: 2, c: 2, e: 1}
 *
 * @example
 * charCount("hey");
 * // → { h: 1, e: 1, y: 1 }
 */ 
function charCount(s) {
  const frequencies = {};

  for (let i = 0; i < s.length; ++i) {
    let c = s[i].toLowerCase();

    if (!isAlphaNum(c)) continue;

    if (frequencies[c] === undefined)
      frequencies[c] = 1;
    else frequencies[c] += 1;
  }

  return frequencies;
}
```
