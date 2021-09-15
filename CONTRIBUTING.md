# Contributing

- [Contributing](#contributing)
  - [Commit Messages](#commit-messages)
  - [Commit Types](#commit-types)
    - [List of Previously Used Types](#list-of-previously-used-types)
    - [Project-Specific Commit Types](#project-specific-commit-types)

## Commit Messages

We try to use all possible commit message
[good practices](https://chris.beams.io/posts/git-commit/).

Also, do a few `git log` commands to get a feel for this project's take on
commit messages.

We also use “types”. See next section.

## Commit Types

Anyone is welcome to add, fix, correct and enlighten anything in this
project. Since this is a wiki of sorts project, which is published at Read The
Docs using Sphinx, many different topics are commited here. Therefore, let's
try to write commit title (subjects) with a commit *type* prefix.

Examples of types:

- docs: related to readme, contributing (not docs about the tutorial on the
  topics themselves
- chore: update configs and dependencies
- cmdline: for anything command line, coreutils, unix tools

For programming langages, let's use the standard file extension:

- c: C programming Langage.
- js: JavaScript
- hs: Haskell
- rb: Ruby
- ts: TypeScript
- scm: Scheme
- rkt: Racket

For notes on books, tutorials, courses, etc., create a type using some
sort of abbreviation or accronym. See [Project-Specific Commit Types](#project-specific-commit-types) below.

For example, for the [Mostly Adequate Guide to Functional
Programming](https://github.com/MostlyAdequate/mostly-adequate-guide),
the type is “magfp”. So, the commit message would look like:

```
magfp: Add solutions for chapter 5 on composition
```

For the [Haskell From First Principles](https://haskellbook.com/) book,
we chose the type “hffp”:

```
hffp: Add notes on foldr vs foldr
```

It is important that once we use a type for a book or tutorial, we stick
to that type for all commits related to that book or tutorial.

### List of Previously Used Types

To find a list of previously used types, run something like this:

```
$ git log --format='%s' \
    | grep '^[a-z]\+:' \
    | sed 's/:.\+//' \
    | sort
    | uniq
```

Alternatively, just run the bash script (which basically just does the same as
the command line above):

```
$ ./list-commit-message-types.bash
```

### Project-Specific Commit Types

By “project-specific types” we mean commit types for books, tutorials,
etc. (mentioned above), and not generic commit types like “rb” or “c".

- hffp: [Haskell From First Principles](https://haskellbook.com/)
- magfp: [Mostly Adequate Guide to Functional Programming](https://github.com/MostlyAdequate/mostly-adequate-guide)
- compfpjs: [Composable Functional JavaScript](https://egghead.io/courses/professor-frisby-introduces-composable-functional-javascript)

