#!/usr/bin/env ruby

$num = nil

def do_sth
  $num ||= begin
            2 + 5 / 0
          rescue
            p 'nope'
          end
end

do_sth
p $num
