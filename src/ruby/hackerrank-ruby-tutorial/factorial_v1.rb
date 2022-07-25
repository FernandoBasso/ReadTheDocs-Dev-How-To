require 'rspec'

##
# Computes the factorial of `n`.
#
# This is a recursive definition.
#
# ASSUME: `n` is an integer greater than or equal to 1.
#
def factorial(n)
  return 1 if n == 1 || n.zero?

  n * factorial(n - 1)
end

describe 'factorial()' do
  it 'should compute the factorial of 1' do
    expect(factorial(1)).to eq 1
  end

  it 'should compute the factorial of 2' do
    expect(factorial(2)).to eq 2 * 1
  end

  it 'should compute the factorial of 5' do
    expect(factorial(5)).to eq 5 * 4 * 3 * 2 * 1
  end
end
