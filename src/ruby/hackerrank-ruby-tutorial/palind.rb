##
# Checks wheter `s` is a palindrome.
#
# ASSUME: `s` is a string with no uppercase chars.
#
# REFERENCES:
#
# • https://www.dictionary.com/e/palindromic-word/
#
def palind?(s)
  return true if s.empty? || s.size == 1
  return false if s[0] != s[-1]

  palind?(s[1, s.size - 2])
end
