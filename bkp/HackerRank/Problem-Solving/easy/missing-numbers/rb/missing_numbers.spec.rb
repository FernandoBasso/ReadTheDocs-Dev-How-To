require 'rspec'
require_relative 'missing_numbers_v1'

#
# Think of “tests” like this:
#
# • design tool
# • documentation
# • testing
# • regression testing
#

describe 'missing_nums(arr, brr)' do
  context 'when the two inputs are empty' do
    it 'should return an empty array' do
      expect(missing_nums([], [])).to eq([])
    end
  end

  context 'when brr is smaller than arr' do
    it 'should return an empty array' do
      expect(missing_nums([1, 2], [])).to eq([])
      expect(missing_nums([1, 2], [1])).to eq([])
      expect(missing_nums([1, 2], [2])).to eq([])
    end
  end

  context 'when the two arrays have the same elements' do
    it 'should return an empty array !!!' do
      arr = [1, 2, 3]
      brr = [1, 2, 3]

      expect(missing_nums(arr, brr)).to eq([])

      arr2 = [1, 3, 2]
      brr2 = [2, 1, 3]

      expect(missing_nums(arr2, brr2)).to eq([])
    end
  end

  context 'when there is a missing number' do
    it 'should return the array with that missing number' do
      arr = [1, 2]
      brr = [1, 2, 3]

      expect(missing_nums(arr, brr)).to eq([3])
    end
  end

  context 'when all the numbers are missing arr' do
    it 'should return all the numbers in the brr' do
      arr = []
      brr = [1, 2, 3]

      expect(missing_nums(arr, brr)).to eq(brr)

      arr2 = [1, 2, 3]
      brr2 = [4, 5, 6]

      expect(missing_nums(arr2, brr2)).to eq(brr2)
    end
  end

  context 'when there is a missing number with different frequencies' do
    it 'should return the array with that missing number' do
      arr = [1, 2, 3]
      brr = [1, 2, 3, 3, 3, 3]

      expect(missing_nums(arr, brr)).to eq([3])

      arr = [3, 2, 1]
      brr = [3, 3, 1, 3, 2, 3]

      expect(missing_nums(arr, brr)).to eq([3])
    end
  end

  context 'input example from HackerRank' do
    it 'should return sorted array' do
      arr = [203, 204, 205, 206, 207, 208, 203, 204, 205, 206]
      brr = [203, 204, 204, 205, 206, 207, 205, 208, 203, 206, 205, 206, 204]

      expect(missing_nums(arr, brr)).to eq([204, 205, 206])
    end

    it 'should sort by the number values' do
      arr = [11, 4, 11, 7, 13, 4, 12, 11, 10, 14]
      brr = [11, 4, 11, 7, 3, 7, 10, 13, 4, 8, 12, 11, 10, 14, 12]

      expect(missing_nums(arr, brr)).to eq([3, 7, 8, 10, 12])
    end
  end
end

