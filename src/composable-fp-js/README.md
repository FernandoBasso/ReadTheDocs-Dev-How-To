# Composable Functional JavaScript

[Free course on Egghead with Brian Lonsdorf](https://egghead.io/lessons/javascript-linear-data-flow-with-container-style-types-box)
(creator of Mostly Adequate Guide to Functional Programming).

## Runnint Unit Tests

We have a single spec (test) file for each topic on the videos. When a
given topic presents multiple approaches to solve a problem, the same
spec file import the different implementations. That is, changing the
implementation to even more FP style should not need different tests.

```shell-session
$ npm run vid01/*.spec.js
```

Or in watch mode:

```shell-session
$ npm run test -- --watch vid01/*.spec.js 
```

## Documenting The Source

We try to follow
[these
conventions](https://developers.google.com/style/reference-verbs).

We use

```text
Produces the next char in the alphabet.
```

instead of

```text
Produce the next char in the alphabet.
```

That is, use “the third person”, and not the imperative.

To make sure you get it right, complete this sentence: “When invoked,
this function...”. Examples:

```text
When invoked, this function ...

  ... produces the sum of its arguments
```

```text
When invoked, this method ...

  ... splits the string into words.
```

Just make sure to write a proper sentence, with proper casing and other
punctuation.

**NOTE** Most of my ideas regarding unit tests and documentation come
from the **awesome, totally mind blowing book**
[How to Design Programs](https://htdp.org/).
It is the book that most changed (for the much better) the
way I think about software development in general.

See the existing source code files for real examples. I also have a
[blog post](https://fernandobasso.dev/programming/2021-03-06-documenting-source-code.html)
about this topic.

