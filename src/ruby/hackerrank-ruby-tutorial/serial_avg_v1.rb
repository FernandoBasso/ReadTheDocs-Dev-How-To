require 'rspec'

##
# Calculate the averate of XX.XX and YY.YY serial number.
#
# Result on average calculation is rounded to the two decimal
# places. Example:
#
#   >> (-12.43 + -56.78) / 2.0
#   => -34.605000000000004
#   >> ((-12.43 + -56.78) / 2.0).round(2)
#   => -34.61
#
# Note how -34.605 becames -34.61.
#
def serial_avg(s)
  sss = s[0, 3]
  x = s[3, 6].to_f
  y = s[9, 6].to_f

  avg = ((x + y) / 2.0).round(2)

  "#{sss}#{format('%.2f', avg)}"
end

describe 'serial_avg()' do
  it 'should work with .00 decimals' do
    expect(serial_avg('002-10.00-20.00')).to eq '002-15.00'
  end

  it 'should work with odd decimals' do
    expect(serial_avg('003-10.13-20.48')).to eq '003-15.31'
  end

  it 'should work with hackerrank test case' do
    expect(serial_avg('001-12.43-56.78')).to eq '001-34.61'
  end
end
