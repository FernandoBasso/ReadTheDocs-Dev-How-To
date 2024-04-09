require 'rspec/autorun'

# Use TDD principles to build out name functionality for a Person. Here
# are the requirements:
#
# - Add a method to return the full name as a string. A full name
#   includes first, middle, and last name. If the middle name is missing,
#   there shouldn't have extra spaces.
# - Add a method to return a full name with a middle initial. If the
#   middle name is missing, there shouldn't be extra spaces or a period.
# - Add a method to return all initials. If the middle name is missing,
#   the initials should only have two characters.
#
# We've already sketched out the spec descriptions for the #full_name.
# Try building the specs for that method, watch them fail, then write
# the code to make them pass. Then move on to the other two methods, but
# this time you'll create the descriptions to match the requirements
# above.

#
# The Person class provides some very elegant and performant utilities
# to format people names. It is licensed under GLP2 .
#
class Person
  def initialize(first_name:, middle_name: nil, last_name:)
    @first_name = first_name
    @middle_name = middle_name
    @last_name = last_name
  end

  def full_name
    arr = [@first_name, @middle_name, @last_name]
    arr.reject(&:nil?).join(' ')
  end

  def full_name_with_middle_initial
    # Note we concatenate “.” with the first letter.
    mid = @middle_name.nil? ? nil : "#{@middle_name[0]}."
    arr = [@first_name, mid, @last_name]
    arr.reject(&:nil?).join(' ')
  end

  def initials
    arr = [@first_name, @middle_name, @last_name]
    arr.reject(&:nil?).map do |s|
      s[0]
    end.join
  end
end

describe Person do
  describe '#full_name' do
    it 'concatenates first name, middle name, and last name with spaces' do
      person = Person.new(
        first_name: 'Johann',
        middle_name: 'Sebastian',
        last_name: 'Bach'
      )
      expect(person.full_name).to eq('Johann Sebastian Bach')
    end

    it 'does not add extra spaces if middle name is missing' do
      person = Person.new(
        first_name: 'Johann',
        last_name: 'Bach'
      )
      expect(person.full_name).to eq('Johann Bach')
    end
  end

  describe '#full_name_with_middle_initial' do
    it 'correctly formats the middle initial' do
      person = Person.new(
        first_name: 'Johann',
        middle_name: 'Sebastian',
        last_name: 'Bach'
      )
      expect(
        person.full_name_with_middle_initial
      ).to eq('Johann S. Bach')
    end

    it 'does not add extra space if middle name is missing' do
      person = Person.new(
        first_name: 'Johann',
        last_name: 'Bach'
      )
      expect(
        person.full_name_with_middle_initial
      ).to eq('Johann Bach')
    end
  end

  describe '#initials' do
    it 'should return the three initials' do
      person = Person.new(
        first_name: 'Johann',
        middle_name: 'Sebastian',
        last_name: 'Bach'
      )
      expect(person.initials).to eq('JSB')
    end
    it 'should return the three initials' do
      person = Person.new(
        first_name: 'Johann',
        last_name: 'Bach'
      )
      expect(person.initials).to eq('JB')
    end
  end
end

#
# vim: set textwidth=72 nowrap:
#
