require 'rspec'
require_relative 'is_prime_v1'

describe 'prime?(n)' do
  it 'should return false for non-prime numbers' do
    expect(prime?(0)).to eq false
    expect(prime?(1)).to eq false
    expect(prime?(4)).to eq false
    expect(prime?(22)).to eq false
    expect(prime?(192)).to eq false
  end

  it 'should return true for non-prime numbers' do
    expect(prime?(2)).to eq true
    expect(prime?(13)).to eq true
    expect(prime?(191)).to eq true
  end
end
