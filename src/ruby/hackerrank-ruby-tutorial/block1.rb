def f
  p 'ini'
  yield(1)
  p 'end'
end

f do |n|
  p "Param from yield: #{n}"
end
