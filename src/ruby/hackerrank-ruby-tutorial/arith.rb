require 'rspec'

module Arith
  Add = lambda do |acc, e|
    acc + e
  end
end

describe 'add(x, y)' do
  it 'should add x and y' do
    expect(Arith::Add.call(-1, 1)).to eq 0
  end

  it 'should work when made into a block' do
    expect((1..5).inject(0, &Arith::Add)).to eq 15
  end
end
