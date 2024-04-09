
#
# This is TRICKY!
#
# Even though we don't have a parameter in `run_block` method.
#
# When we use `Proc.new` without giving it a code block, it will
# look to see if the current scope has been passed a code block
# and if so, it will use that.
#

def run_block
    p = Proc.new # <1>
    p.call
end

run_block { p 'And it works!' }

#
# `Proc.new` always wants a block. It can be a explicit code
# block or an implicit one like in <1>.
#
