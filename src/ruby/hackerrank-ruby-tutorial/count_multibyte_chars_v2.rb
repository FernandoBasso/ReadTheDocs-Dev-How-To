require 'rspec'

##
# Counts the number of bytes in the char `c`.
#
def count_bytes(c)
  count = 0

  c.each_byte do |b|
    count += 1
  end

  count
end

##
# Counts the number of multibyte chars in the string `s`.
#
# Example: 'ab λ' has four chars, but only 'λ' is a multibyte char.
# The others are ASCII-compatible, single byte chars (including
# the space). Therefore, 'ab λ' has 1 multibyte char.
#
def count_mbc(s)
  num_multibyte_chars = 0

  s.each_char do |c|
    if count_bytes(c) > 1
      num_multibyte_chars += 1
    end
  end

  num_multibyte_chars
end

describe 'count_mbc()' do
  it 'should work with empty string' do
    expect(count_mbc('')).to eq 0
  end

  it 'should work with a single multibyte char' do
    # 0x2714
    expect(count_mbc('✔')).to eq 1
    # 0x0001f4a9
    expect(count_mbc('💩')).to eq 1
  end

  it 'should work with multiple multibyte chars' do
    expect(count_mbc('✔💩')).to eq 2
  end

  it 'should work with mixed ASCII-like and multibyte chars' do
    expect(count_mbc('lambda λ')).to eq 1
    expect(count_mbc('¥1000')).to eq 1
    expect(count_mbc('May the ✔ source be 💩 with λ you!')).to eq 3
  end
end
