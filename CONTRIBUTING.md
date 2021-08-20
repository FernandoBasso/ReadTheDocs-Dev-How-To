# Contributing

Anyone is welcome to add, fix, correct and enlighten anything in this
project. Since this is a wiki of sorts project, which is published at Read The
Docs using Sphinx, many different topics are commited here. Therefore, let's
try to write commit title (subjects) with a tag-like prefix.

Examples of tags:

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

To find a list of previously used tags, run something like this:

```
$ git log --format='%s' \
    | grep '^[a-z]\+:' \
    | sed 's/:.\+//' \
    | sort
    | uniq

cmdline
docs

Also, do a few `git log` commands to get a feel for this project's take on
commit messages. Nothing new or fancy. We just try to stick to them.

Alternatively, just run the bash script (which basically just does the same as
the command line above):

```
$ ./list-commit-message-tags.bash
```

