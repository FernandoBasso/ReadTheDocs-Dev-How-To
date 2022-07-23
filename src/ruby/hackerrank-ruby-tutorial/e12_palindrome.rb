require 'rspec'

##
# Checks wheter `s` is a palindrome.
#
# ASSUME: `s` is a string with no uppercase chars.
#
# REFERENCES:
#
# • https://www.dictionary.com/e/palindromic-word/
#
def palind?(s)
  return true if s.empty? || s.size == 1
  return false if s[0] != s[-1]

  palind?(s[1, s.size - 2])
end

describe 'palind?()' do
  it 'should be true for empty string' do
    expect(palind?('')).to eq true
  end

  it 'shold be true for single char strings' do
    expect(palind?('z')).to eq true
    expect(palind?('7')).to eq true
  end

  it 'shold work with 2-char strings' do
    expect(palind?('zz')).to eq true
    expect(palind?('77')).to eq true
    expect(palind?('xy')).to eq false
    expect(palind?('76')).to eq false
  end

  it 'should work with 3-or-more char strings' do
    expect(palind?('ana')).to eq true
    expect(palind?('anna')).to eq true
    expect(palind?('racecar')).to eq true
    expect(palind?('rotator')).to eq true
    expect(palind?('')).to eq true
    expect(palind?('level')).to eq true
    expect(palind?('any')).to eq false
    expect(palind?('thought')).to eq false
  end
end
