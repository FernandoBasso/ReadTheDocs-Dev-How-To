class Person

  PEOPLE = []

  attr_reader :name, :hobbies, :friends

  def initialize(name)
    @name = name
    @hobbies = []
    @friends = []
    PEOPLE << self
  end

  def has_hobby(hobby)
    @hobbies << hobby
  end

  def has_friend(friend)
    @friends << friend
  end

  def self.method_missing(method, *args)
    method_s = method.to_s
    # `all_with_friends', `all_with_hobbies', etc.
    if method_s.start_with?('all_with_')
      # Get end of string, from char 9 to the end.
      # So, `attr' is something like `friends', `hobbies', etc.
      attr = method_s['all_with_'.length..-1]
      # attr = method_s[9..-1]
      if self.public_method_defined?(attr)
        # `Array#find_all' seems to be the same as `Array#select'.
        PEOPLE.find_all do |person|
          # Like `person.friends.include?(person_obj)' or
          # `person.hobbies.include?('some hobby')
          person.__send__(attr).include?(args.first)
        end
      else
        raise(ArgumentError, "Cannot find method `#{attr}'.")
      end
    else
      super
    end
  end
end

john = Person.new('John')
paul = Person.new('Paul')
george = Person.new('George')
ringo = Person.new('Ringo')
john.has_friend(paul)
john.has_friend(george)

george.has_friend(paul)
ringo.has_hobby('rings')

Person.all_with_friends(paul).each do |person|
  puts "#{person.name} is friends with #{person.name}"
end

Person.all_with_hobbies('rings').each do |person|
  puts "#{person.name} is into rings."
end

#
# In the above class definition, all occurrences `self' inside the Person class
# refers to the Person class itself.
#

