#!/usr/bin/env ruby -wU

require 'benchmark'

string_az = Hash[('a'..'z').to_a.zip((1..26)).to_a]

symbol_az = Hash[(:a..:z).to_a.zip((1..26)).to_a]


string_time = Benchmark.realtime do
    100000_000.times do
        string_az['r']
    end
end

symbol_time = Benchmark.realtime do
    100000_000.times do
        symbol_az[:r]
    end
end

puts string_time
puts symbol_time
