---
title: HackerRank Challenges
description: Studies and notes on solutions for HackerRank problems.
---

# HackerRank Challenges

In this section we attempt to document some solutions for HackerRank challenges.

The goal is to have one ore more solutions for each challenge, using one or more programming paradigms (procedural, functional, whatever) as means of practicing and learning.

Making notes and creating explanations describing the techniques, concepts and steps used in a solution is a good way of understanding things better.

The code is hosted in the [Gitlab repository for this site](https://gitlab.com/devhowto/Dev-How-To).

```{admonition} About these solutions and explanations
:class: danger

Do not use these solutions (or others found in repositories over the web) on HackerRank.
Try solving them yourself and only them see how others have done it to try to learn from them.

You don't learn by just copying code.
```

## TDD and Developing the Solutions

### Deno

We are using [Deno](https://deno.land) to run TypeScript tests and solutions.

- https://deno.land/manual/getting_started/installation
- https://deno.land/manual/testing

Clone and `cd` into the directory where the solutions are being developed:

```shell-session
$ git clone git@gitlab.com:devhowto/Dev-How-To.git

$ cd Dev-How-To/src/hackerrank/typescript
```

Run all unit tests:

```shell-session
$ deno test --import-map ./import-map.json
```

Run unit tests for the challenge under development, for example:

```shell-session
$ deno test --import-map ./import-map.json \
    1-easy/big-sum/bigSum.test.ts
```

```{toctree}
---
hidden:
maxdepth: 6
caption: Easy HackerRank Challenges
---

1-easy/index.md
helper-functions.md
```
