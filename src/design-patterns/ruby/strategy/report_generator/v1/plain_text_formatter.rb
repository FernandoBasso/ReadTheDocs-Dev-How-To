require_relative 'formatter'

##
# Formats the report in Plain Text.
#
class PlainTextFormatter < Formatter
  def output_report(title, text)
    puts("===== #{title} =====")

    text.each do |line|
      puts(line)
    end
  end
end
