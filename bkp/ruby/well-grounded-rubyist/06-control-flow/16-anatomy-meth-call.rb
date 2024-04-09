#
# The anatomy of a method call
# ----------------------------
#
# Every method call in Ruby has the following syntax:
#
#  - A receiver object or variable (defaulting to self if absent)
#  - A dot (required if there’s an explicit receiver; disallowed otherwise)
#  - A method name (required)
#  - An argument list (optional; defaults to () )
#  - A code block (optional; no default)
#
# Note in particular that the argument list and the code block are separate.
# Their existence varies independently. All of these are syntactically
# legitimate Ruby method calls:
#

loop { puts "Hi" }
loop() { puts "Hi" }
string.scan(/[^,]+/)
string.scan(/[^,]+/) {|word| puts word }

