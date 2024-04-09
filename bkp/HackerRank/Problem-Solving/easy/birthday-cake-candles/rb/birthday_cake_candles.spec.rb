require 'rspec'
require_relative './birthday_cake_candles_v2'

describe 'birthday_cake_candles(candles)' do
  describe 'single-element array' do
    it 'should find 3 one time' do
      expect(birthday_cake_candles([3])).to eq 1
    end
  end

  describe 'array with no repeated numbers' do
    it 'should find 5 one time' do
      expect(birthday_cake_candles([5, 3, 0])).to eq 1
    end
  end

  describe 'array with repeated numbers' do
    it 'should find 7 three times' do
      expect(
        birthday_cake_candles([7, 3, 5, 1, 7, 7, 2])
      ).to eq 3
    end
  end
end
