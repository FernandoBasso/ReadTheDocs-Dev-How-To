##
# Counts the frequencies in `xs`.
#
# @param xs [Array<Integer>
# @return [Hash]
#
def count_freqs(xs)
  freqs = {}

  xs.each do |x|
    freqs[x] = freqs[x] ? freqs[x] + 1 : 1
  end

  freqs
end

##
# Finds numbers that are present in `brr` but missing in `arr`.
#
# @param {number[]} arr
# @param {number[]} brr
# @returns {number[]} The array of the missing numbers (the
#   difference).
#
def missing_nums(arr, brr)
  freqs_arr = count_freqs(arr)
  freqs_brr = count_freqs(brr)
  missing = []

  freqs_brr.each_key do |key|
    missing << key if freqs_brr[key] != freqs_arr[key]
  end

  missing.sort
end
