# λ Dev How To λ

* [Intro](#intro)
* [Running Locally](#running-locally)
* [Branching and Publishing](#branching-and-publishing)

## Intro

This repository intends to be a wiki of sorts about Haskell (and functional
programming in general).

**NOTE**: The [Gitlab repo](https://gitlab.com/devhowto/Dev-HowTo) is the
*main one where all the action happens and people can collaborate. The [Github
*repo](https://github.com/FernandoBasso/Dev-How-To) is just a mirror.

## Running Locally

From the root directory (not from the `docs/` directory) simply run this:

```text
$ make develop
```

Then point your web browser http://localhost:2001. If you prefer to have a
specific local domain, you can add something like this in `/etc/hosts`:

```text
##
# The Dev How To project is setup to run locally on port 2001.
#
# • http://local.devhowto.dev:2001
#
127.0.0.1 local.devhowto.dev
```

Then access http://local.devhowto.dev:2001 from your browser.

## Branching and Publishing

**NOTE** This is a work in progress and the things described below are
an attempt to see how the workflow holds up in the long run.

So far the two branches used for deploying to Read The Docs are `stage`
and `main`. Generally, I try something on `stage` first and if things
work (no extension is missing, some Sphinx syntax I tried does work,
etc.) then I update `main` too.

There is also a `drafts` branch where I store stuff that is not
minimally suitable for being published to the site until further
improvements are made. Things I want to keep track of to improve on
later as time permits.

The workflow goes as follows:

Checkout to the `drafts` branch (we will always stay on this branch, as
the other branches are updated from this branch using specific commands)
and undo the last “drafts” commit and unstage everything.

The `drafts` branch must **always** have a commit with the message
“DRAFTS”. Then, we undo that last commit:

```
$ git checkout drafts
$ git reset --soft HEAD~1
$ git restore -- ./
```

**NOTE**: Make sure the last commit is indeed a *DRAFTS* commit before
the `reset --soft` command.

Work and write at will, and then commit only things to be pushed to
either `stage` or `main` with a proper message for the thing to be
published:

```
$ git add -- ./path/to/NEW/files
$ git add --patch -- ./path/to/CHANGED/files
$ git commit <readl, professional, quality commit>
```

This commit above is **NOT** the DRAFTS commit, but a real, publishable,
quality content commit. So, be professional with the message, body and
the contents of the commit.

Then, publish that commit to `stage` and possibly also `main`:

```
$ git push gl drafts:stage
$ git push gl drafts:main
```

Then, commit all the rest again (in `drafts`) and push:

```
$ git add -- ./
$ git commit -m 'DRAFTS'
```

At this point, we published what we wanted and safely stored our drafts
in the remote repo.

It is possible to never have to leave the `drafts` branch to follow this
workflow. Whatever unpublished things we have are always safe on the
server. We just have to keep this somewhat awkward and verbose
committing and uncommiting of the drafts. It is not that hard, though,
it it works for our purposes.

Here's a summary:

```
$ git checkout drafts
$ git reset --soft HEAD~1
$ git restore --staged -- ./

$ git add -- ./path/to/NEW/files
$ git add --patch -- ./path/to/CHANGED/files
$ git commit <readl, professional, quality commit>

$ git push gl drafts:stage
$ git push gl drafts:main

$ git add -- ./
$ git commit -m DRAFTS
```

