require 'awesome_print'

evens_and_odds = (1..5).group_by(&:odd?)
p evens_and_odds

##
# To be a jedi master, your skill level must be >= 80.
# A padawan has skill level < 80.
#

jedis = {
  'Yoda': 100,
  'Ahsoka Tano': 93,
  'Aayla Secura': 91,
  'Luke Skywalker': 93,
  'Anakin Skywalker': 60
}

groups = jedis.group_by do |_k, v|
  v < 80 ? :padawan : :master
end

ap groups[:padawan]
#
# → [
# →     [0] [
# →         [0] :"Anakin Skywalker",
# →         [1] 60
# →     ]
# → ]
##

ap groups[:master]
#
# → [
# →     [0] [
# →         [0] :Yoda,
# →         [1] 100
# →     ],
# →     [1] [
# →         [0] :"Ahsoka Tano",
# →         [1] 93
# →     ],
# →     [2] [
# →         [0] :"Aayla Secura",
# →         [1] 91
# →     ],
# →     [3] [
# →         [0] :"Luke Skywalker",
# →         [1] 93
# →     ]
# → ]
##
