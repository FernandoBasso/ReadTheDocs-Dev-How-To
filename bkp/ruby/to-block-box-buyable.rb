class Box
  def initialize(name, avail)
    @name = name
    @available = avail
  end

  def to_s
    "#{@name}, #{@available.to_s}"
  end

  def buyable?
    @available
  end
end

class Page
  attr_accessor :boxes

  def initialize(boxes)
    @boxes = boxes
  end

  def buyable_boxes
    @boxes.select(&:buyable?)
  end

  def unbuyable_boxes
    @boxes.reject { |box| ! box.byable? }
  end
end

b1 = Box.new 'box 1', true
b2 = Box.new 'box 2', false

arr = [b1, b2]

page = Page.new(arr)
puts page.buyable_boxes
# → box 1, true

# vim: set shiftwidth=2 softtabstop=2:
