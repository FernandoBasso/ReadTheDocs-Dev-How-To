require 'rspec'
require_relative 'palindromic_primes_v1'

describe 'prime_palind()' do
  it 'should generate the first two palindromic primes' do
    expect(prime_palind(2)).to eq [2, 3]
  end

  it 'should generate the first three palindromic primes' do
    expect(prime_palind(3)).to eq [2, 3, 5]
  end

  it 'should generate the first ten palindromic primes' do
    expect(prime_palind(10)).to eq [
      2,
      3,
      5,
      7,
      11,
      101,
      131,
      151,
      181,
      191
    ]
  end
end
