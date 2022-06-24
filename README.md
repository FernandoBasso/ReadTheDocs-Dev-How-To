# λ Dev How To λ

- [λ Dev How To λ](#λ-dev-how-to-λ)
  - [Intro](#intro)
  - [Local Setup](#local-setup)
  - [Running Locally](#running-locally)
  - [Branching and Publishing](#branching-and-publishing)
  - [Directory Structure](#directory-structure)
  - [Markdown](#markdown)
  - [License](#license)

## Intro

This repository holds code and text explanations about programming topics that
interest me. Access the published website at:

- https://www.devhowto.dev

**NOTE**: The [Gitlab repo](https://gitlab.com/devhowto/Dev-How-To) is the
main/official one where all the action happens and people can collaborate. The
[Github repo](https://github.com/FernandoBasso/Dev-How-To) is just a mirror.

## Local Setup

Arch Linux:

```
$ sudo pacman -S python-sphinx python-pip --needed
```

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

## Directory Structure

Ideally, I would solve many challenges in the main languages that
interest me (currently, at least). The directory structure would
go like this:

```text
$ mkdir -pv \
    {lisp,scheme,javascript,typescript,haskell,ruby,C}/{1..8}kyu
```

And to create the directories for each language's solution for a
problem, like the ‘diplomas’ challenge, something like this:

```text
$ mkdir -pv \
    {lisp,scheme,javascript,typescript,haskell,ruby,C}/6kyu/diplomas
```

## Markdown

When writing in Markdown, we use the [One Sentence Per Line](https://asciidoctor.org/docs/asciidoc-recommended-practices/#one-sentence-per-line) approach.

Quoted directly from [Asciidoctor recommended practices](https://asciidoctor.org/docs/asciidoc-recommended-practices/#one-sentence-per-line):

> Don’t wrap text at a fixed column width.
> Instead, put each sentence on its own line, a technique called sentence per line.
> This technique is similar to how you write and organize source code.
> The result can be spectacular.
>
> Here are some of the advantages of using the sentence per line style:
>
> - It prevents reflows (meaning a change early in the paragraph won’t cause the remaining lines in the paragraph to reposition).
> - You can easily swap sentences.
> - You can easily separate or join paragraphs.
> - You can comment out sentences or add commentary to them.
> - You can spot sentences which are too long or sentences that vary widely in length.
> - You can spot redundant (and thus mundane) patterns in your writing.
>
> We picked up this idea from the writing guide in the Neo4j documentation.
> However, it seems like the idea dates back a discovery by Buckminster Fuller in the 1930s, who called it [ventilated prose](https://vanemden.wordpress.com/2009/01/01/ventilated-prose/).
> The technique was also recommended in 2009 by Brandon Rhodes in a blog post about [Semantic Linefeeds](https://rhodesmill.org/brandon/2012/one-sentence-per-line/).

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
