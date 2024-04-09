
#
# Normalizes a word so it can be more easily compared.
#
normalize = -> (w) { w.split('').sort.join }

#
# Verifies that a word matches the target word.
#
is_match = -> (normalized_target, word) do
  normalized_target == normalize.call(word)
end

#
# Makes use of the helper lambda to do the filtering of
# the words.
#
def anagrams(word, filename, predicate)
  normalized = word.split('').sort.join

  file = File.read(filename)

  matches = file.lines.select do |line|
    predicate.call(normalized, line.chop!)
  end

  p matches
end

#
# Make sure to `gem install rspec`.
#
require 'rspec'

describe 'Find anagrams' do
  expected_result = ["heros", "horse", "shore"]
  it 'produces an array of anagrams' do
    expect(
      anagrams('horse', '../anagram-word-list.dat', is_match)
    ).to eq expected_result
  end
end

anagrams('horse', '../anagram-word-list.dat', is_match)
