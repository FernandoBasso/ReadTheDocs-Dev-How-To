
class Thing

  def get
    # We try to return @@num, which was never initialized.
    @@num
  end

end

obj = Thing.new
puts obj.get
# →  `get': uninitialized class variable @@num in Thing (NameError)
