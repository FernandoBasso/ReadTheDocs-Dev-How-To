require 'rspec'

def full_name(first, *rest)
  [first, *rest].join(' ')
end

describe 'full_name()' do
  it 'should work with single name' do
    expect(full_name('Yoda')).to eq 'Yoda'
  end

  it 'should work with two-word names' do
    expect(full_name('Ahsoka', 'Tano')).to eq('Ahsoka Tano')
  end

  it 'should work with three-or-more-word names' do
    expect(
      full_name('Albus', 'Percival', 'Wulfric', 'Brian', 'Dumbledore')
    ).to eq 'Albus Percival Wulfric Brian Dumbledore'
  end
end
