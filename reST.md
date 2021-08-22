# reStructuredText

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

