require 'rspec'
require_relative 'arith_geometric_seq_sum_v1'

##
# The function is t(n) = n ** 2 + 1.
#

describe 'sum_terms()' do
  it 'should total 0 when n is 0' do
    ##
    # Sum zero terms of the series. Nothing to sum.
    # Don't even apply the function.
    #
    expect(sum_terms(0)).to eq(0)
  end

  it 'should total 3 when n is 1' do
    ##
    # 0 + 1 ** 2 + 1
    # 0 +   1    + 1
    # 2
    #
    expect(sum_terms(1)).to eq(2)
  end

  it 'should total 7 when n is 2' do
    ##
    # 2 + 2 ** 2 + 1
    # 2 +   4    + 1
    # 7
    #
    expect(sum_terms(2)).to eq(7)
  end

  it 'should total 17 when n is 3' do
    ##
    # 7 + 3 ** 2 + 1
    # 7 +   9    + 1
    # 17
    #
    expect(sum_terms(3)).to eq(17)
  end

  it 'should total 27 when n is 4' do
    ##
    # 17 + 4 ** 2 + 1
    # 17     16   + 1
    # 34
    #
    expect(sum_terms(4)).to eq(34)
  end
end
