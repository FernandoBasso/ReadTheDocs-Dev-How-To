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
