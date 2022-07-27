require 'rspec'

def fact(n)
  (1..n).inject(&:*)
end

combination = lambda do |n|
  lambda do |r|
    fact(n) / (fact(r) * fact(n - r))
  end
end

n_c_r = combination.call(4)
p n_c_r.call(2)

describe 'combination() curried' do
  it 'should work on 4 and 2' do
    expect(combination.call(4).call(2)).to eq 6
  end
end
