# reStructuredText

* [Include source code examples](#include-source-code-examples)
* [Images](#images)

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

