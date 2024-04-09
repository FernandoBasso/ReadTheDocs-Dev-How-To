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
    v < 'M'
end

#
# & converts the lambda to a block.
#
a_to_m = crew.select(&first_half)
