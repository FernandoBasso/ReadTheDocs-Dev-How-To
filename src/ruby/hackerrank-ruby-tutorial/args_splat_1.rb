require 'rspec'

##
# Sums all arguments.
#
# Note the `*` in front of `xs`. It is the splat operator.
#
def sum_arr(*xs)
  xs.inject(0) { |acc, e| acc + e }
end

describe 'sum_arr()' do
  it 'should sum zero arguments' do
    expect(sum_arr).to eq 0
  end

  it 'should sum 1 arguments' do
    expect(sum_arr(42)).to eq 42
    expect(sum_arr(-1)).to eq(-1)
  end

  it 'should sum 2 or more arguments' do
    expect(sum_arr(0, 0, 0)).to eq 0
    expect(sum_arr(-1, 1)).to eq 0
    expect(sum_arr(-1, -10, -100, -1000)).to eq(-1111)
  end
end
