require 'rspec'

##
# Raises `base` to the `exponent`. Curried
#
power = lambda do |base|
  lambda do |exponent|
    base ** exponent
  end
end

describe 'power() curried' do
  it 'should work with 2 and 3' do
    expect(power.call(2).call(3)).to eq 8
  end

  it 'should work with -2 and 3' do
    expect(power.call(-2).call(3)).to eq(-8)
  end

  it 'should work with 2 and 8' do
    expect(power.call(2).call(8)).to eq(256)
  end

  it 'should work with 2 and -2' do
    #
    # 2 ** -3 is 1/8.
    #
    expect(power.call(2).call(-3)).to eq(1.0 / 2 / 2 / 2)
  end
end
