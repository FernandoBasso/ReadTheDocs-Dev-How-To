##
# Checks wheter `s` is a palindrome.
#
# This solutions reverses the string and checks whether it is
# equal to the original string.
#
# ASSUME: `s` is a string with no uppercase chars.
#
# REFERENCES:
#
# • https://www.dictionary.com/e/palindromic-word/
#
def palind?(s)
  s == s.reverse
end
