require 'rspec'

##
# Make array of `len` powers to the `exp` exponent.
#
def make_pow_arr(exp, len)
  ##
  # Each integer from 1 to infinity is the base,
  # which we raise to the `exp` exponent until
  # we reach `len` length.
  #
  1.upto(Float::INFINITY).lazy.map do |base|
    base ** exp
  end.first(len)
end

describe 'lazy pow()' do
  it 'should find power of single number' do
    expect(make_pow_arr(2, 1)).to eq [1 ** 2]
    expect(make_pow_arr(4, 1)).to eq [1 ** 4]
  end

  it 'should find power of first two integers' do
    expect(make_pow_arr(3, 2)).to eq [1 ** 3, 2 ** 3]
  end

  it 'should find power of first three integers' do
    expect(make_pow_arr(2, 3)).to eq [1 ** 2, 2 ** 2, 3 ** 2]
  end

  it 'should find pwer of first 6 integers' do
    expect(make_pow_arr(2, 6)).to eq [
      1 ** 2,
      2 ** 2,
      3 ** 2,
      4 ** 2,
      5 ** 2,
      6 ** 2
    ]
  end
end
