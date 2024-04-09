class Integer
  def our_times
    counter = 0
    until counter == self
      # Call the block passing it the current `counter'.
      yield(counter)
      counter += 1
    end
    # Not required for it to work. Just that we return the number
    # that is used as the receiver.
    self
  end
end

3.our_times do |num|
  puts "num: #{num}"
end
