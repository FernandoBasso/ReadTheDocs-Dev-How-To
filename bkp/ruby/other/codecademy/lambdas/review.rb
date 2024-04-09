#
# block
# -----
#
# A bit of code between `do ... end` or `{ ... }`. It is not an object
# but can be passed to methods like `each`, `collect` or `select`.
#
#
# proc
# ----
#
# A saved block we can use over and over again.
#
#
# lambda
# ------
#
# Similar to a proc, but it cares about the number of arguments
# and returns to the calling method (unlike blocks and procs) rather
# than returning immediately to the ‘outer’ block.
#
