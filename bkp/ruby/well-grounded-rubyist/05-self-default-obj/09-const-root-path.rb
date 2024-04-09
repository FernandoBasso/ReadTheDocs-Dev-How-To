#
# Sometimes we may need to start the constant lookup process from the
# top level. For that, we start with `::'.
#

class Cello
  class String
    A_STR = 440 # 440Hz
    def initialize(pitch)
      @pitch = pitch
    end
  end

  attr_reader :maker, :date
  def initialize(maker, date)
     @maker = maker
     @date = date
     @a = String.new(String::A_STR)
     # Other strings here...
  end

  def history
    # Now, for some reason, we need Ruby's built int String, not Cello::String.
    ::String.new(@maker + ', ' + @date.strftime('%b, %Y'))
  end

  # NOTE: At this scope, String is Cello::String, not Ruby's built in
  # String. If we want Ruby's built in, we need to write ::String.
end

puts Cello.new('Giannini', Time.new('2018-01-01')).history()
# →  Giannini, Jan, 2018

#
# We don't need to also use :: in ::Time.new because there is no clash of
# class names for Time. But we _had_ to do it for string.
#
#
# NOTE: for simplicity sake, we used lots of hard-coded values here.
#
