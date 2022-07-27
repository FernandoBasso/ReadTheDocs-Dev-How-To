require 'rspec'

##
# Tries two generate the number 3. Attempts two times by default
# before giving up.
#
# Returns 3 if it is generated within `tries` tries; else,
# return `nil`.
#
def gen3(tries: 2)
  ##
  # A random number between 1 and 4, inclusive.
  #
  num = rand(1..4)

  return num if num == 3 && tries.positive?

  ##
  # Recurse while `tries` is greater than zero.
  #
  return gen3(tries: tries - 1) if tries.positive?
end

##
# Tries two times to generate 3.
p gen3

##
# 0 tries. Will never find 3.
#
p gen3(tries: 0)

##
# 10 tries will make it very likely to find 3 before giving up.
#
p gen3(tries: 10)

describe 'gen3()' do
  it 'should never generate 3 with zero tries' do
    expect(gen3(tries: 0)).to eq nil
  end

  it 'should return 3 with a few tries and 3 is generated' do
    allow_any_instance_of(Kernel).to receive(:rand).and_return(3)

    expect(gen3(tries: 4)).to eq 3
  end

  it 'should return nil with a few tries when no 3 is generated' do
    allow_any_instance_of(Object).to receive(:rand).and_return(2)

    expect(gen3(tries: 4)).to eq nil
  end
end
