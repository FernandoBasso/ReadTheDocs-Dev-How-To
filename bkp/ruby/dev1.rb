
class Account
  attr_accessor :balance

  def initialize(amount = 0)
    self.balance = amount
  end

  def +(value)
    self.balance += value
  end

  def -(value)
    self.balance -= value
  end

  def to_s
    self.balance.to_s
  end
end

account = Account.new(100)
puts account
# → 100

account += 5
puts account
# → 105

account -= 2
puts account
# → 103

