#
# Devise a function that gets one parameter 'w' and returns all the anagrams
# for 'w' from the file word list file. The file wl.txt contains a huge list of
# valid words and the idea is to find the anagrams of ‘w’ included in such a
# list.
#
# "Anagram": An anagram is a type of word play, the result of rearranging the
# letters of a word or phrase to produce a new word or phrase, using all the
# original letters exactly once; for example orchestra can be rearranged into
# carthorse.
#
#   anagrams('horse') should return:
#   ['heros', 'horse', 'shore']
#
# You can assume the absolute/relative path to wl.txt to be fixed and,
# therefore hardcore in the code, or you can also change the firm of the
# method to for example anagrams("horse", ./wl.txt).
#

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
    predicate.call(normalized, line.chomp!)
  end
end

#
# Make sure to `gem install rspec`.
#
# Run:
#
#   rspec anagram.rb
#
require 'rspec'

describe 'Find anagrams' do
  expected_result = ["heros", "horse", "shore"]
  it 'produces an array of anagrams' do
    expect(anagrams('horse', './wl.txt', is_match)).to eq expected_result
  end
end

# anagrams('horse', './wl.txt', is_match)

