Ruby Installation and Environment
=================================

configs
^^^^^^^

Where are my ruby executable files?::

    # From the Shell
    irb --simple-prompt -rrbconfig

    # From inside IRB
    RbConfig::CONFIG['bindir']

To see everything, do something like::

    # Install awesome_print:
    gem install awesome_print

    # Start irb with RbConfig loaded:
    irb --simple-prompt -rrbconfig

    # From inside irb, load awesome_print
    require 'awesome_print'

    # Print (awesome_print is used by typing 'ap'):
    ap RbConfig::CONFIG

Some useful dirs to know about:

* ``rubylibdir`` → Ruby standard library
* ``bindir`` → Ruby command-line tools
* ``archdir`` → Architecture-specific extensions and libraries (compiled, binary files)
* ``sitedir`` → Your own or third-party extensions and libraries (written in Ruby)
* ``vendordir`` → Third-party extensions and libraries (written in Ruby)
* ``sitelibdir`` → Your own Ruby language extensions (written in Ruby)
* ``sitearchdir`` → Your own Ruby language extensions (written in C)

*Library* means some Ruby code that provides a specific functionality or facility. *Extension* generally means something written in C.

loading libs
^^^^^^^^^^^^

Working on it...


