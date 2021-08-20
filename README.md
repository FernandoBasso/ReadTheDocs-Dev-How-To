# Haskell How To

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

