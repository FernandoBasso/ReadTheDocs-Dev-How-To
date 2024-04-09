class Title
  def initialize(text)
    @text = text
  end

  def to_s
    @text
  end

  def +@
    @text.upcase
  end
end

title = Title.new('May the force be with you!')
puts (+title)


#
# Without parenthesis around ~+title~, we get a warning about ambiguity.
#
