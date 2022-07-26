import time
from datetime import datetime

# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
# import os
# import sys
# sys.path.insert(0, os.path.abspath('.'))


# -- Project information -----------------------------------------------------

html_title = 'Dev How To'

##
# It would be nice to have a custom logo for this project, but in the
# mean time, we can use a Creative Commons licensed one.
#
# https://commons.wikimedia.org/wiki/File:Antu_x-terminal-emulator.svg
#
html_logo = '_static/Antu_x-terminal-emulator.png'
html_favicon = '_static/Antu_x-terminal-emulator-favicon.png'

project = 'Dev How To'
author = 'Fernando Basso'
copyright = str(datetime.now().year) + ' ' + author

# The full version, including alpha/beta/rc tags
release = '0.0.1'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
  #
  # Causes “warning duplicate label <some heading> other instance in...”
  #
  # It seems we don't actually need it. We can add a label before a heading:
  #
  #   .. _foo bar:
  #
  #   My Foo Bar Heading
  #   ------------------
  #
  # And then, on another page, reference the label:
  #
  #    More information on :ref:`foo bar`.
  #
  # This way, even if a heading changes, its label does not need to
  # also changes (unless we want to) and the cross-page links still
  # work.
  #
  # 'sphinx.ext.autosectionlabel',

  ##
  # https://www.sphinx-doc.org/en/master/usage/markdown.html
  #
  'myst_parser',
]

myst_enable_extensions = [
  ##
  # • https://myst-parser.readthedocs.io/en/latest/syntax/roles-and-directives.html?highlight=directives#nesting-directives
  # • https://myst-parser.readthedocs.io/en/latest/syntax/optional.html#syntax-colon-fence
  #
  'colon_fence',
  'strikethrough',
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

html_theme_options = {
  'sidebar_hide_name': False,
}

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'furo'


# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']

timestamp = str(time.time())

html_js_files = [
  'qanda.js' + '?v=' + timestamp,
  'left-nav.js' + '?v=' + timestamp,
]

html_css_files = [
  'qanda.css' + '?v=' + timestamp,
  'global.css' + '?v=' + timestamp,
]
