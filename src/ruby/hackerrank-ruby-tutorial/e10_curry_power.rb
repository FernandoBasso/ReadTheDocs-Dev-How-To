require 'awesome_print'
require 'rspec'

##
# Raises base `b` to exponent `b`.
#
def pow(b, e)
  b ** e
end

##
# Run with:
#
#   $ rspec <filename>.rb
#
describe 'pow()' do
  it 'should raise 3 to 2' do
    expect(pow(3, 2)).to eq 9
  end

  it 'shouldraise -3 to 2' do
    expect(pow(-3, 2)).to eq 9
  end
end
