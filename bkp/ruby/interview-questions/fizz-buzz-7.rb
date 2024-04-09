#
# https://www.tomdalling.com/blog/software-design/fizzbuzz-in-too-much-detail/
#
# Lazy Generation
#

def fizzbuzz_generator(start, triggers)
  Enumerator.new do |yielder|
    i = start
    loop do
      parts = triggers.select { |_, p| p.call(i) }
      i_result = parts.size > 0 ? parts.map(&:first).join : i
      yielder.yield(i_result)
      i += 1
      sleep 1
    end
  end
end

fizzbuzzator = fizzbuzz_generator(1, [
  ['Fizz', -> (n){ n % 3 == 0 }],
  ['Buzz', -> (n){ n % 5 == 0 }],
  ['Zazz', -> (n){ n < 10 }],
])

loop { puts fizzbuzzator.next }
