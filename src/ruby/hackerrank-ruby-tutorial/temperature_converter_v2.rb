#
# tags: [temperature, converter, lookup-table]
#

#
# rubocop:disable Metrics/BlockLength
#

require 'rspec'

##
# A temperature converter lookup table of sorts with a lambda
# for each case.
#
CONVERSION_TABLE = {
  fahrenheit: {
    celsius: ->(t) { (t - 32) * (5.0 / 9.0) },
    kelvin: ->(t) { (t + 459.67) * (5.0 / 9.0) }
  },
  celsius: {
    kelvin: ->(t) { t + 273.15 },
    fahrenheit: ->(t) { t * (9.0 / 5.0) + 32 }
  },
  kelvin: {
    fahrenheit: ->(t) { t * (9.0 / 5.0) - 459.67 },
    celsius: ->(t) { t - 273.15 }
  }
}.freeze

##
# A temperature converter which handles converstions between fahrenheit,
# celsius, and kelvin.
#
def convert(t, input_scale: 'celsius', output_scale: 'kelvin')
  CONVERSION_TABLE[input_scale.to_sym][output_scale.to_sym].call(t)
end

describe 'convert()' do
  describe 'celsius → kelvin' do
    it 'should convert zero' do
      expect(convert(0)).to eq 273.15
    end

    it 'should convert -7' do
      expect(convert(
               -7,
               input_scale: 'celsius',
               output_scale: 'kelvin'
             )).to eq 266.15
    end
  end

  describe 'celsius → fahrenheit' do
    it 'should convert zero' do
      expect(convert(
               0,
               input_scale: 'celsius',
               output_scale: 'fahrenheit'
             )).to eq 32
    end

    it 'should convert -7' do
      expect(convert(
               -7,
               input_scale: 'celsius',
               output_scale: 'fahrenheit'
             )).to eq(19.4)
    end
  end

  describe 'fahrenheit → kelvin' do
    it 'should convert zero' do
      expect(convert(
               0,
               input_scale: 'fahrenheit',
               output_scale: 'kelvin'
             )).to be_within(0.01).of(255.37)
    end

    it 'should convert -7' do
      expect(convert(
               -7,
               input_scale: 'fahrenheit',
               output_scale: 'kelvin'
             )).to be_within(0.01).of(251.48)
    end
  end

  describe 'fahrenheit → celsius' do
    it 'should convert zero' do
      expect(convert(
               0,
               input_scale: 'fahrenheit',
               output_scale: 'celsius'
             )).to be_within(0.01).of(-17.77)
    end

    it 'should convert -7' do
      expect(convert(
               -7,
               input_scale: 'fahrenheit',
               output_scale: 'celsius'
             )).to be_within(0.01).of(-21.66)
    end
  end

  describe 'kelvin → fahrenheit' do
    it 'should convert zero' do
      expect(convert(
               0,
               input_scale: 'kelvin',
               output_scale: 'fahrenheit'
             )).to be_within(0.01).of(-459.67)
    end

    it 'should convert -7' do
      expect(convert(
               -7,
               input_scale: 'kelvin',
               output_scale: 'fahrenheit'
             )).to be_within(0.01).of(-472.27)
    end
  end

  describe 'kelvin → celsius' do
    it 'should convert zero' do
      expect(convert(
               0,
               input_scale: 'kelvin',
               output_scale: 'celsius'
             )).to be_within(0.01).of(-273.15)
    end

    it 'should convert -7' do
      expect(convert(
               -7,
               input_scale: 'kelvin',
               output_scale: 'celsius'
             )).to be_within(0.01).of(-280.15)
    end
  end
end
