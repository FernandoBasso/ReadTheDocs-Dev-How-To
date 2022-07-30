##
# tags: [ruby-vs-javascript, procs, scope]
##

#
# A sanitized string is one that:
#
# - Contains no newlines.
# - No tabs.
# - Words and punctuation are separated by a single space.
#

##
# Sanitizes a string.
#
sanitize_string = proc do |s|
  s.chomp.strip.gsub(/\s+/, ' ')
end

##
# Sanitizes an array of strings.
#
def process_text(lines, sanitizer: sanitize_string)
  lines.map(&sanitizer).join(' ')
end

#
# In JavaScript, we would be able to make `sanitizer` default to
# `sanitize_string` which is in the enclosing scope. In ruby, we can't
# do it.
#
