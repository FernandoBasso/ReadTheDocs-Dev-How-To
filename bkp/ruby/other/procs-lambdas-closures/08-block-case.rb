
several = Proc.new { |num| num > 3 && num < 8 }
many    = Proc.new { |num| num > 3 && num < 8 }
few     = Proc.new { |num| num == 3 }
couple  = Proc.new { |num| num == 2 }
none    = Proc.new { |num| num == 0 }

0.upto(10) do |n|
    case n
    when several
        p 'several'
    when many
        p 'many'
    when few
        p 'few'
    when 'couple'
        p 'couple'
    when none
        p 'none'
    end
end

#
# On the `when` lines, the interpreter calls those procs and
# see if they return true or false to know what to do.
#
