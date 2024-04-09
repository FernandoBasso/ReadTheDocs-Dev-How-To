#
# Ruby doesn't know thta `+' means additions to humans.
#

gotcha = Object.new
def gotcha.+(o)
  'Not a number, gotcha!'
end

p gotcha.+(3)
p gotcha + 5

