#
# Ruby has many built in globals, like
#
# $0, the name startup file for the current program (because one program
# generally is composed of many files).
#
# $:, for the load path.
#
# $$, for the PID.
#
# Some globals also have more readable names, like $LOAD_PATH.
#

# The startup file.
puts $0

# same as $PID, if you require 'English'
puts $$

# same as $: available even without requiring 'English', it seems.
puts $LOAD_PATH

# same as $IGNORECASE, if require 'English'
puts $=

