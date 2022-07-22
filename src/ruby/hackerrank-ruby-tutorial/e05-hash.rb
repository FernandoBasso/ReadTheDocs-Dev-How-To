jedi {
  id: 1,
  name: 'Yoda',
  skill: 'The Force'
}

jedi.each do |k, v|
  p k v
  p v
end
#
# → :id
# → 1
# → :name
# → "Yoda"
# → :skill
# → "The Force"
##

jedi.each do |arr|
  p arr
end
#
# → [:id, 1]
# → [:name, "Yoda"]
# → [:skill, "The Force"]
##
