class Ticket
  attr_accessor :venue, :date

  #
  # Using `self' inside instance methods is the same as using `@'.
  #

  def initialize(venue, date)
    self.venue = venue
    self.date = date
  end

  #
  # `===' is what will be used in `case/when' constructs.
  #
  def ===(other)
    self.venue == other.venue
  end
end

t1 = Ticket.new('Galactic Theather', '2000/09/15')
t2 = Ticket.new('The Empire Building', '2000/09/19')
t3 = Ticket.new('Galactic Theather', '2000/09/19')

# And here, two ticket objects _match_ if they happen to have the same venue.
# Of course, the `Ticket' class could implement `===' to math only if all the
# attributes had the same contents.
case t1
when t2
  puts 't2 same location as t1'
  exit
when t3
  puts 't3 same location as t1'
  exit
else
  puts 'What the poop‽'
end

