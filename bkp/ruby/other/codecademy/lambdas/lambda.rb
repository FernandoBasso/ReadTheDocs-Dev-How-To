#!/usr/bin/env ruby -wU

crew = {
  captain: 'Picard',
  first_officer: 'Riker',
  lt_cdr: 'Data',
  lt: 'Worf',
  ensign: 'Ro',
  counselor: 'Troi',
  chief_engineer: 'LaForge',
  doctor: 'Crusher'
}

first_half = lambda do |k, v|
    v > 'R'
end

res = crew.select(&first_half)
puts res.inspect
