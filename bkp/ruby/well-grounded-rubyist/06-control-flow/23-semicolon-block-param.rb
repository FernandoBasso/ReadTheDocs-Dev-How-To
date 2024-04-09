def a_method
  foo = 1
  bar = 2
  1.times do |i ; foo, bar|
    foo = 11
    bar = 22
    puts "#{foo}, #{bar}"
    # → 11, 22
  end
  puts "out: #{foo}, #{bar}"
  # → 1, 2
end

a_method

#
# By using `;' in the parameter list for the block, we tell ruby to
# _overshadow_ any outer variable with the same name. That means any matter
# with `foo' and `bar' inside the block will not affect the `foo' and `bar'
# from the outer scope.
#
# Blocks allows three types of variables:
# - from the outer scope
# - block parameters
# - block-locals, listed after the semicolon.
#

