# reStructuredText

- [reStructuredText](#restructuredtext)
  - [Headings a.k.a Sections](#headings-aka-sections)
  - [Include source code examples](#include-source-code-examples)
  - [Images](#images)
  - [Typing Unicode](#typing-unicode)

## Headings a.k.a Sections

reST uses the term Section. She
[reST Section Docs](https://docutils.sourceforge.io/docs/ref/rst/restructuredtext.html#sections).

For this project, we chose the following characters to adorn titles and
headings (sections):

`=` 0x3d EQUALS SIGN with overline for page titles (heading 1):

```
===============
Main Page Title
===============
```

Page titles are the only ones adorned with overline. All other ones
have only underline adornments.

`-` 0x2d HYPHEN-MINUS for headings level 2:

```
Heading Two Title
-----------------
```

`~` 0x7e TILDE for headings level 3:

```
Heading Three Title
~~~~~~~~~~~~~~~~~~~
```

`.` 0x2e FULL STOP for headings level 4:

```
Heading Four Tilde
..................
```

Perhaps `"` 0x22 QUOTATION MARK for headings level five, and `'` 0x27
APOSTROPHE for headings level 6 (if we ever come to that depth of
nested subsections ':D).

## Include source code examples

Our docs, tutorials, tips, QandAs, etc are in the `docs/`
directory. We also have source code inside `src/`. They are sibling
directories. When we want to use reST's `literalinclude`, we can
either provide a relative or absolute path.

**This project opts to always use absolute paths when including source
code files from `src/` inside `.rst` files in `docs/`**. The main
reason for this is that if we move files and directories around, we
can do some grep search to find out which files are referencing a
given path, so we can update include paths where necessary.

As a side note, it is OK to include `.rst`f files inside `.rst` files
using relative paths if it makes sense, like a main `.rst` file
including subsections from files in the same or near directories
inside `docs/`.

In reST, includes with `/` mean **absolute path**. But it seems it is
absolute to the `docs/` directory (for Sphinx at least), not the real
root directory of the project.

```
$ tree -CF -L 1
.
├── CONTRIBUTING.md
├── Makefile
├── README.md
├── docs/
├── list-commit-message-tags.bash*
├── reST.md
└── src/
```

So, from files inside `docs/` we can do `/../<rest of the path>` to
include files from stuff above the docs directory. For example, in any
`.rst` file inside docs, we can do something like this:

```
.. literalinclude:: /../src/cmdline/kitty-set-window-title.bash
   :language: bash
   :linenos:
```

If we just say `/src/cmdline/...` it will be the same as
`/docs/src/cmdline/...`, which is incorrect. `src/` is in the root dir
of this project, side by side with `docs/`. It is a little annoying
have to use that `/../` path but at least it works.

See docs for
[literalinclude here](https://www.sphinx-doc.org/en/master/usage/restructuredtext/directives.html#directive-literalinclude).

## Images

Images paths can be relative to the current .rst file or absolute. In
this project, if images are inside ``docs/_static/``, then we always
use absolute paths for images so we can search and replace more easily
since we sould have more context (the full path) to match against.

The “full path”, such as it is, is computed from the ``docs/``
directory. So, if we have a file in ``docs/_static/foo/bar.png``, we
include it like this:

    .. image:: /_static/haskell/ash-alien-haskell.png
       :alt: Ash from Alien 1979 movie talking about the perfect organizm.

Using a path of ``/docs/_static/...`` would be incorrect. From the
point of view of Sphinx, ``docs/`` **is** the root directory (from
where documentation (site, pdf, epub) is generated, so, we start with
`/` followed by subdirs inside ``docs/``.

On the other hand, if the images is close to the file it is included
in, then we prefer to use relative paths becase then we can move
groups of files with their images and their relative paths would still
be correct. Example:

    $ tree -CF -L 1 docs/haskell/
    docs/haskell/
    ├── ash-alien-haskell.png
    ├── hffp/
    ├── index.rst
    └── sectioning.rst

Then in either ``index.rst`` or ``sectioning.rst`` we include the
image like this:

    .. image:: ./ash-alien-haskell.png
       :alt: Ash talking about the perfect orgamizm.

Always include the *alt* text.

[Read more about Sphinx and images](https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html#images).

## Typing Unicode

When we need to write a unicode code point we write it with the following
format ``<char> U+<hex-number> NAME``, i.e.:

```
- U+2D HYPHEN MINUS
💩 U+01f4a9 PILE OF POO
	U+09 HORIZONTAL TABULATION, (also CHARACTER TABULATION)
```

Alphabetic characters can be written both in uppercase or lowercase,
that is, both “U+01F4A9” and “U+01f4a9” are fine. Just make sure the
leading “U” is uppercase. Some characters have
[aliases](https://en.wikipedia.org/wiki/Unicode_alias_names_and_abbreviations).
Use your judgement when to write the alternative names or not.

Always use 2, 4, 6 or 8 characters for the hex value. For example,
these are valid as per our guidelines:

```
U+002d HYPHEN MINUS
U+2D HYPHEN MINUS

U+00a0 NO-BREAK SPACE
U+00a0 NBSP

U+a0 NO-BREAK SPACE
U+a0 NBSP

U+01F4A9 PILE OF POO
U+0001f4A9 PILE OF POO
```

### Application-Specific Syntax

The guidelines above apply when writing Unicode code points in general
terms, not related to application-specific syntax. It is okay to write
application-specific notation when needed. For example, in Vim, to
type a code point between 1 and 4 bytes, we can use either the
lowercase "u" or the uppercase "U", like this:

```
Ctrl+v u 002d
Ctrl+v U 002d
```

If Vim thinks is waiting for more characters you can still ask vim to
process the current input so far:

```
Ctrl+v u 2d<Esc>
Ctrl+v u 2d<Space>

Ctrl+v U 1f4a9<Esc>
Ctrl+v U 01f4a9<Esc>
```

Other key presses may also stop processing of the input entered so
far, depending on the situation, like `<Tab>`, `<Enter>` and
others. Also not that Vim does not require all uppercase, or an
even-number of bytes (2, 4, 6 or 8). So, in these cases it is okay to
write in whatever the syntax or requirements the given tool you are
writing about needs them to be.

For more vim-specific information, see `:help i_CTRL-v`.

For emacs, we do `C-c q` to insert no-visible characters,
and `C-x 8 RET` to insert a Unicode code point, either by its hex
value or its Unicode name. See `M-x describe-function RET
insert-char RET` or `M-x describe-key RET` followed by `C-x 8 RET`.

