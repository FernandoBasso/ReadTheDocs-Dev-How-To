# Contributing


- [Contributing](#contributing)
  - [Commit Messages](#commit-messages)
  - [Commit Types](#commit-types)
    - [commit-msg git hook](#commit-msg-git-hook)
    - [List of Previously Used Types](#list-of-previously-used-types)
    - [Project-Specific Commit Types](#project-specific-commit-types)
  - [Including Files and File Names](#including-files-and-file-names)

## Commit Messages

We try to use all possible commit message
[good practices](https://chris.beams.io/posts/git-commit/).

Also, do a few `git log` commands to get a feel for this project's take on
commit messages.

We also use “types”. See next section.

## Commit Types

Anyone is welcome to add, fix, correct and enlighten anything in this
project.
Since this is a wiki of sorts project, which is published at Read The
Docs using Sphinx, many different topics are committed here.
Therefore, let's try to write commit title (subjects) with a commit *type* prefix.

Examples of types:

- docs: related to readme, contributing (not docs about the tutorial on the
  topics themselves
- chore: update configs and dependencies
- cmdline: for anything command line, coreutils, unix tools

For programming languages, let's use the standard file extension:

- c: C programming Language.
- js: JavaScript
- hs: Haskell
- rb: Ruby
- ts: TypeScript
- scm: Scheme
- rkt: Racket

For notes on books, tutorials, courses, etc., create a type using some
sort of abbreviation or acronym.
See [Project-Specific Commit Types](#project-specific-commit-types) below.

For example, for the [Mostly Adequate Guide to Functional
Programming](https://github.com/MostlyAdequate/mostly-adequate-guide),
the type is “magfp”.
So, the commit message would look like:

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

### commit-msg git hook

We have a pre-commit hook that validates the commit type.
Take a look at `.githooks/commit-msg`.

Update your local git settings to use the git hooks location for this project:

```shell-session
$ cd /path/to/this/project
$ git config --local core.hooksPath '.githooks'
```

We manually update the script's array of valid commit types when a new one
needs to be introduced.

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

## Including Files and File Names

Many `.md` and `.rst` files include other files, something like this:

```text
:::{literalinclude} /../src/haskell/folds/FooBar.hs
:language: haskell
:lines: 9-
:::
```

Whenever a source file is removed or renamed, we **MUST** make sure to check if its included in any other file and update the includes accordingly.

For example, with `grep`:

```shell-session
$ grep \
    --color \
    --recursive \
    --exclude-dir=.git \
    --include='*.md' \
    --include='*.rst' \
    FooBar.hs
```

Or with Ripgrep:

```shell-session
$ rb \
    --type md
    -- type rst \
    --fixed-strings \
    FooBar.hs
```

Of course, searches can be made from most editors as well.
And it is probably a good idea to do a search without specifying the file types just in case.

**NOTE**: Make sure to do the search from the root of the project so we are sure to find matches if there are any.

As an example, suppose we have `docs/haskell/intro.md`, and it contains this include:

```text
:::{literalinclude} /../src/haskell/folds/FooBar.hs
:language: haskell
:lines: 9-
:::
```

And we rename `src/haskell/FooBar.hs` to `src/haskell/HelloWorld.hs`.
Then, we **MUST** do a search for `FooBar.hs`, which causes us to find out it is being used/included in `docs/haskell/intro.md`.
We than update that include to the new name:

```text
:::{literalinclude} /../src/haskell/folds/FooBar.hs
:language: haskell
:lines: 9-
:::
```

It is a annoying having to go through such troubles, but this way we can use literal includes without copying and pasting examples left and right, and still avoid broken pages with broken includes and empty examples in the middle of text and explanations.
