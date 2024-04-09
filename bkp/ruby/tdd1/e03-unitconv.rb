require 'rspec/autorun'

DimensionalMismatchError = Class.new(StandardError)

Quantity = Struct.new(:amount, :unit)

class UnitConv
  def initialize(initial_qty, target_unit)
    @initial_qty = initial_qty
    @target_unit = target_unit
  end

  def convert
    Quantity.new(
      @initial_qty.amount * conversion_factor(
        from: @initial_qty.unit,
        to: @target_unit
      ),
      @target_unit
    )
  end

  private

  CONVERSION_FACTORS = {
    litter: {
      liter: 1,
      cup: 4.226775,
      pint: 2.11338,
    },
    gram: {
      gram: 1,
      kilogram: 1000,
    },
  }

  def conversion_factor(from:, to:)
    dimension = common_dimension(from, to)

    if dimension.nil?
      raise(DimensionalMismatchError, "Can't convert from #{from} to #{to}")
    end

    CONVERSION_FACTORS[dimension][to] / CONVERSION_FACTORS[dimension][from]
  end

  def common_dimension(from, to)
    CONVERSION_FACTORS.keys.find do |canonical_unit|
      CONVERSION_FACTORS[canonical_unit].keys.include?(from) &&
        CONVERSION_FACTORS[canonical_unit].keys.include?(to)
    end
  end
end

describe 'UnitConv' do
  describe '#convert' do
    it 'translates between objects of the same dimension' do
      cups = Quantity.new(2, :cup)
      unit_conv = UnitConv.new(cups, :liter)

      result = unit_conv.convert

      expect(result.amount).to be_within(0.001).of(0.473)
      expect(result.unit).to eq(:liter)
    end

    it 'can convert between quantities of the same unit' do
      cups = Quantity.new(2, :cup)
      unit_conv = UnitConv.new(cups, :cup)

      result = unit_conv.convert

      expect(result.amount).to be_within(0.001).of(2)
      expect(result.unit).to eq(:cup)
    end

    it 'raises error if quantities are of differing dimensions' do
      cups = Quantity.new(2, :cup)
      conv = UnitConv.new(cups, :gram)

      # To check for errors, instead of passing a primitive value to
      # expect, we pass a block. It is rspec especial syntax to allow
      # testing that exceptions have been raised.
      expect { conv.convert }.to raise_error(DimensionalMismatchError)
    end
  end
end
