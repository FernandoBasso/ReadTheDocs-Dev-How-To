require 'rspec'
require_relative './ice_cream_parlor_v2'

describe 'parlor(money, flavorPrices)' do
  it 'handles input sorted in ascending order' do
    expect(parlor(6, [1, 3, 4, 5, 6])).to eq [1, 4]
  end

  it 'handles input sorted in descending order' do
    expect(parlor(6, [6, 5, 4, 3, 1])).to eq [2, 5]
  end

  it 'handles input that is not sorted' do
    expect(parlor(6, [3, 2, 5, 7, 1])).to eq [3, 5]
  end

  it 'handles input with duplicates' do
    expect(parlor(6, [2, 3, 5, 3, 2, 7])).to eq [2, 4]
  end
end
