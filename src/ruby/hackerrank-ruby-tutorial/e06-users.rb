class User
  def initialize(name, is_admin)
    @name = name
    @admin = is_admin
  end

  def say_hello
    p "Hello, #{@name}!"
  end

  def admin?
    @admin
  end
end

users = [
  User.new('Yoda', true),
  User.new('Ahsoka', false),
  User.new('Aayla', false),
]

users.each do |user|
  unless user.admin?
    user.say_hello
  end
end
#
# → "Hello, Ahsoka!"
# → "Hello, Aayla!"
##
