# λ Dev How To λ

* [Intro](#intro)
* [Running Locally](#running-locally)
* [Branching and Publishing](#branching-and-publishing)
* [License](#license)

## Intro

This repository holds code and text explanations about programming topics that
interest me. Access the published website at:

- https://www.devhowto.dev

**NOTE**: The [Gitlab repo](https://gitlab.com/devhowto/Dev-HowTo) is the
main/official one where all the action happens and people can collaborate. The
[Github repo](https://github.com/FernandoBasso/Dev-How-To) is just a mirror.

## Local Setup

Xubuntu:

```text
$ sudo apt install python3-sphinx python3-pip
```

Then, with pip, install thse:

```text
$ pip install sphinx-autobuild furo
```

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

Develop on the `drafts` branch and push to the remote. When ready to
publish (trigger RTD build), then also push to `main`. My remote is
named `gl` (not `origin`), so I do:

```
$ git push gl drafts:main
```

After a few minutes the changes should be visible on
https://www.devhowto.dev.

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

