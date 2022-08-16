#
# tags: [temperature, converter, keyword-arguments, case-when]
#

#
# rubocop:disable Metrics/CyclomaticComplexity
# rubocop:disable Metrics/BlockLength
#

require 'rspec'

##
# A temperature converter class with a single static `convert`
# method.
#
class TemperatureConverter
  class << self
    def convert(temperature, input_scale: 'celsius', output_scale: 'kelvin')
      case input_scale
      when 'celsius'
        case output_scale
        when 'kelvin'
          temperature + 273.15
        when 'fahrenheit'
          temperature * (9.0 / 5.0) + 32
        end
      when 'fahrenheit'
        case output_scale
        when 'kelvin'
          (temperature + 459.67) * (5.0 / 9.0)
        when 'celsius'
          (temperature - 32) * (5.0 / 9.0)
        end
      when 'kelvin'
        case output_scale
        when 'fahrenheit'
          temperature * (9.0 / 5.0) - 459.67
        when 'celsius'
          temperature - 273.15
        end
      end
    end
  end
end

describe 'TemperatureConverter.convert()' do
  describe 'celsius → kelvin' do
    it 'should convert zero' do
      expect(TemperatureConverter.convert(0)).to eq 273.15
    end

    it 'should convert -7' do
      expect(TemperatureConverter.convert(
               -7,
               input_scale: 'celsius',
               output_scale: 'kelvin'
             )).to eq 266.15
    end
  end

  describe 'celsius → fahrenheit' do
    it 'should convert zero' do
      expect(TemperatureConverter.convert(
               0,
               input_scale: 'celsius',
               output_scale: 'fahrenheit'
             )).to eq 32
    end

    it 'should convert -7' do
      expect(TemperatureConverter.convert(
               -7,
               input_scale: 'celsius',
               output_scale: 'fahrenheit'
             )).to eq(19.4)
    end
  end

  describe 'fahrenheit → kelvin' do
    it 'should convert zero' do
      expect(TemperatureConverter.convert(
               0,
               input_scale: 'fahrenheit',
               output_scale: 'kelvin'
             )).to be_within(0.01).of(255.37)
    end

    it 'should convert -7' do
      expect(TemperatureConverter.convert(
               -7,
               input_scale: 'fahrenheit',
               output_scale: 'kelvin'
             )).to be_within(0.01).of(251.48)
    end
  end

  describe 'fahrenheit → celsius' do
    it 'should convert zero' do
      expect(TemperatureConverter.convert(
               0,
               input_scale: 'fahrenheit',
               output_scale: 'celsius'
             )).to be_within(0.01).of(-17.77)
    end

    it 'should convert -7' do
      expect(TemperatureConverter.convert(
               -7,
               input_scale: 'fahrenheit',
               output_scale: 'celsius'
             )).to be_within(0.01).of(-21.66)
    end
  end

  describe 'kelvin → fahrenheit' do
    it 'should convert zero' do
      expect(TemperatureConverter.convert(
               0,
               input_scale: 'kelvin',
               output_scale: 'fahrenheit'
             )).to be_within(0.01).of(-459.67)
    end

    it 'should convert -7' do
      expect(TemperatureConverter.convert(
               -7,
               input_scale: 'kelvin',
               output_scale: 'fahrenheit'
             )).to be_within(0.01).of(-472.27)
    end
  end

  describe 'kelvin → celsius' do
    it 'should convert zero' do
      expect(TemperatureConverter.convert(
               0,
               input_scale: 'kelvin',
               output_scale: 'celsius'
             )).to be_within(0.01).of(-273.15)
    end

    it 'should convert -7' do
      expect(TemperatureConverter.convert(
               -7,
               input_scale: 'kelvin',
               output_scale: 'celsius'
             )).to be_within(0.01).of(-280.15)
    end
  end
end
