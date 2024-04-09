#!/usr/bin/env ruby -wKuU


#
# All the three syntaxes work.
#

3.times do; print 'foo '; end
puts

3.times do print 'bar ' end
puts

3.times { print 'the force ' }
