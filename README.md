# λ Dev How To λ

* [Intro](#intro)
* [Running Locally](#running-locally)
* [Branching and Publishing](#branching-and-publishing)
* [License](#license)

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
an attempt to see how the workflow holds up in the long run. This
workflow works because I'm working on this by myself. I proper feature
branch workflow would be necessary should more people interact with this
repository.

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
$ git commit <real, professional, quality commit>
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

**IMPORTANT**: The `drafts` branch always contains the good commits and
the DRAFTS commit, except we never publish to `stage` or `main` with the
DRAFT commit on the `drafts` branch. When pushing to `stage` or `main`
branches, the DRAFTS commits must always be undone first. So, all
branches end up having the same commits, except that `stage` and `main`
never have the DRAFTS commit.

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
$ git commit <real, professional, quality commit>

$ git push gl drafts:stage
$ git push gl drafts:main

$ git add -- ./
$ git commit -m DRAFTS
```

## License

My intent with this website is to have the my study notes easily
accessible online for my own personal use. I share them publicly with
the hope that it may occasionally help others as well.

Besides my own ideas, insights, examples and explanations, this website
also includes personal notes from a wide range of resources I have been
using over the years to learn and improve (mailing-lists, IRC chats,
blog posts, forums, specs, man and info pages, tutorials, books,
conversations with other people, etc.).

Because of this, unless explicitly noted, this project is licensed under
the Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA
4.0):
[human readable](https://creativecommons.org/licenses/by-nc-sa/4.0/)
[actual license](https://creativecommons.org/licenses/by-nc-sa/4.0/legalcode).

