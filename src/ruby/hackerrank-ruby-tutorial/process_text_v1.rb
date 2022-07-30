##
# tags: [string, method, chomp, strip, gsub]
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
def sanitize_string(s)
  s.chomp.strip.gsub(/\s+/, ' ')
end

##
# Sanitizes an array of strings.
#
def process_text(lines)
  lines.map do |line|
    sanitize_string(line).strip
  end.join(' ')
end
