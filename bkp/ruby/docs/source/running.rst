Running Ruby Programs
=====================

man ruby excerpt::

    -c             Causes Ruby to check the syntax of the script and exit
                   without executing. If there are no syntax errors, Ruby
                   will print “Syntax OK” to the standard output.

    -w             Enables verbose mode without printing version message at
                   the beginning.  It sets the $VERBOSE variable to true.

Sometimes a syntax error on like 2, for instance, will be reported on some other line (or the end of the file/input) becuase the ruby parser keeps trying until it finally has to give up, like, when there is nothing more to read from the source file.



