================================================================
Building and Installing Chicken Scheme 5.3.0rc3 on macOS Big Sur
================================================================

`Chicken Scheme 5.3.0 release candidate 3 has been announced`_. I had
some trouble installing it on my macOS Big Sur 11.6 work machine. The
reason seems to be that ``Makefile.macosx`` has some hard-coded paths
for XCode and other macOS related command line tools. It looks like in
recent macOS versions these things are not on the path they used to be
in the past, and thus the problems.

The solution is to override a few default ``Makefile.macosx`` path
variables so that they point to the correct locations of ``make``,
``gcc`` and ``ar`` programs on the system.

Thanks to `Mario Domenech Goulart`_ for always helping out with these
things over IRC <3.

For the short version see the :ref:`tl;dr compiling chicken 5.3.0
rc3 on macOS` section below.

.. _`Chicken Scheme 5.3.0 release candidate 3 has been announced`:
   https://lists.nongnu.org/archive/html/chicken-users/2021-09/msg00020.html

.. _`Mario Domenech Goulart`:
   http://parenteses.org/mario/

Downloading and Unpacking the Source
------------------------------------

.. code:: bash

   $ mkdir -pv ~/local/build
   $ cd !$
   $ wget https://code.call-cc.org/dev-snapshots/2021/09/20/chicken-5.3.0rc3.tar.gz
   $ tar zxvf chicken-5.3.0rc3.tar.gz
   $ cd chicken-5.3.0rc3

Installing GNU Make and GCC
---------------------------

According to the README, ompiling Chicken Scheme requires GNU
Make. I'll also use GCC instead of the default compiler bundled with
XCode command line tools.

.. note::

   brew installs stuff in ``/usr/local/Cellar/`` and then makes
   aliases to ``/usr/local/opt/``. When customizing ``PATH`` we should
   point to ``/usr/local/opt/<program>``

Installing GNU Make
~~~~~~~~~~~~~~~~~~~

.. code:: bash

   $ brew install make

It'll print some information like:

.. code:: text

   GNU "make" has been installed as "gmake".
   If you need to use it as "make", you can add a "gnubin" directory
   to your PATH from your bashrc like:

     PATH="/usr/local/opt/make/libexec/gnubin:$PATH"

This way, we can invoke ``make`` and it will be our GNU make, not the
BSD make bundled with macOS. After the setup, check with this command:

.. code:: shell-session

   $ make --version
   GNU Make 3.81
   Copyright (C) 2006  Free Software Foundation, Inc.
   This is free software; see the source for copying conditions.
   There is NO warranty; not even for MERCHANTABILITY or FITNESS FOR A
   PARTICULAR PURPOSE.

   This program built for i386-apple-darwin11.3.0

Installing GNU GCC
~~~~~~~~~~~~~~~~~~

.. code:: bash

   $ brew install gcc

One caveat is that we do not have this newly installed ``gcc`` as a
``gcc`` command. If you do ``gcc<Tab><Tab>`` (no space before hitting
<Tab>), you'll see a few different gcc commands.

On my machine, ``gcc`` is ``/usr/bin/gcc`` (a Clang, XCode-setup
thing), and ``gcc-11`` is ``/usr/local/opt/gcc/gcc-11``. We should
also add it to the path:

.. code:: bash

   PATH="/usr/local/opt/gcc/bin:$PATH"

.. code:: shell-session

   $ which gcc
   /usr/bin/gcc

   $ which gcc-11
   /usr/local/opt/gcc/bin/gcc-11


Trying to Compile
-----------------

The build instructions are very simple. With GNU make available,
simply run:

.. code:: bash

   $ make \
       PLATFORM=macosx \
       PREFIX=$HOME/local/bin/chicken-5.3.0rc3 \
       --jobs

But the build stops with errors like this:

.. code:: text

   make: /Applications/Xcode.app/Contents/Developer/usr/bin/gcc: No such file or directory

And if we look at ``Makefile.macosx``, we see:

.. code:: makefile

   XCODE_DEVELOPER ?= /Applications/Xcode.app/Contents/Developer
   XCODE_TOOL_PATH ?= $(XCODE_DEVELOPER)/Toolchains/XcodeDefault.xctoolchain/usr/bin
   C_COMPILER ?= $(XCODE_DEVELOPER)/usr/bin/gcc
   ARCH ?= $(shell sh $(SRCDIR)/config-arch.sh)

If we look for information on ``gcc``

.. code:: shell-session

   $ gcc --version
   Configured with:
     --prefix=/Library/Developer/CommandLineTools/usr
     --with-gxx-include-dir=/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/c++/4.2.1
   Apple clang version 13.0.0 (clang-1300.0.29.3)
   Target: x86_64-apple-darwin20.6.0
   Thread model: posix
   InstalledDir: /Library/Developer/CommandLineTools/usr/bin

   $ which gcc
   /usr/bin/gcc

So, ``Makefile.macosx``is making the compilation process look for
``gcc`` in the wrong place.

Compiling With Appropriate Paths
--------------------------------

From the previous inspections we now know that we have ``gcc`` in
``/usr/bin/gcc`` and also ``gcc-11`` in
``/usr/local/opt/gcc/bin/gcc-11``

Let's tell make to use ``gcc-11``:

.. code:: bash

   $ make \
       C_COMPILER=/usr/local/opt/gcc/bin/gcc-11 \
       PLATFORM=macosx \
       PREFIX=$HOME/local/bin/chicken-5.3.0rc3 \
       --jobs

The compilation now gets performed correctly! But we end up with
another error:

.. code:: text

   make:
   /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/ar:
   No such file or directory

Again, the problem is the path of the command line tools. We need to
tell ``make`` where this ``ar`` (from ``man ar``: “create and maintain
library archives”) program is located:

.. code:: shell-session

   $ which ar
   /usr/bin/ar

Finally, if we run this command, it should work:

.. code:: bash

   $ make \
       C_COMPILER=/usr/local/opt/gcc/bin/gcc-11 \
       XCODE_TOOL_PATH=/usr/bin \
       PLATFORM=macosx \
       PREFIX=$HOME/local/bin/chicken-5.3.0rc3 \
       --jobs

Installing The Built Files
--------------------------

Finally, install the files to a directory like
``~/local/bin/chicken-5.3.0rc3``:

.. code:: bash

   $ make \
       XCODE_TOOL_PATH=/usr/bin \
       PREFIX="$HOME/local/bin/chicken-5.3.0rc3" \
       install

These are the Chicken-related command line tools installed:

.. code:: shell-session

   $ tree -CFa ~/local/bin/chicken-5.3.0rc3/bin/
  ~/local/bin/chicken-5.3.0rc3/bin/
  ├── chicken*
  ├── chicken-do*
  ├── chicken-install*
  ├── chicken-profile*
  ├── chicken-status*
  ├── chicken-uninstall*
  ├── csc*
  ├── csi*
  └── feathers*

It is a good idea to also add these command line programs to the
``PATH``. For most shells, a line like this on the shell's startup
rc file would work:

.. code:: bash

   export PATH="$HOME/local/bin/chicken-5.3.0rc3/bin:$PATH"

Restart your terminal or source the rc file and you should have all
those chicken-related commands available:

.. code:: shell-session

   $ which csi
   /Users/<you>/local/bin/chicken-5.3.0rc3/bin/csi

   $ which chicken-install
   /Users/<you>/local/bin/chicken-5.3.0rc3/bin/chicken-install

.. _tl;dr compiling chicken 5.3.0 rc3 on macOS:

TL;DR
-----

Install GNU Make, GCC, and compile and install Chicken Scheme
overriding some path-related variables from ``Makefile.macosx``:

.. code-block:: bash

   $ brew isntall make gcc

   $ mkdir -pv ~/local/build
   $ cd !$
   $ wget https://code.call-cc.org/dev-snapshots/2021/09/20/chicken-5.3.0rc3.tar.gz
   $ tar zxvf chicken-5.3.0rc3.tar.gz
   $ cd chicken-5.3.0rc3

   $ make \
      C_COMPILER=/usr/local/opt/gcc/bin/gcc-11 \
      XCODE_TOOL_PATH=/usr/bin \
      PLATFORM=macosx \
      PREFIX=$HOME/local/bin/chicken-5.3.0rc3 \
      --jobs

   $ make \
      XCODE_TOOL_PATH=/usr/bin \
      PLATFORM=macosx \
      PREFIX=$HOME/local/bin/chicken-5.3.0rc3 \
      install

   $ cp -v ~/.bashrc{,bkp}

   $ printf \
       '%s\n' \
       'export PATH="$HOME/local/bin/chicken-5.3.0rc3/bin:$PATH"' \
       >> ~/.bashrc

   $ source ~/.bashrc

   $ printf %s "$PATH" | sed 's/:/\n/g' | grep chicken
   /Users/<you>/local/bin/chicken-5.3.0rc3/bin

   $ csi -help

😅

------------------------------------------------------------------------

Page last updated on <2021-09-25 Sat 10:55>.
