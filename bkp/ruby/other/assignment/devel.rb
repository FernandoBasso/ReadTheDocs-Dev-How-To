
class Jedi
    def skill=(skill)
        @skill = skill
    end
end

yoda = Jedi.new.skill = 'the force'
puts yoda
