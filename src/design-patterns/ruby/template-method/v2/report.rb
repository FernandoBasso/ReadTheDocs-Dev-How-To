# rubocop:disable Style/CaseLikeIf
# rubocop:disable Style/GuardClause

##
# A report generator that outputs HTML or plain text.
#
# Cons:
#
# • Logic for HTML and Plain Text tangled in a mess.
# • Add new formats will be hellish and could break
#   existing (working) format output.
# • It violates a design pattern principle that says we
#   should isolate code that changes from code that doesn't
#   change. HTML output was already working, but to add
#   Plain Text format we had to add logic to HTML-related
#   code and mixed everything up. This is bad.
#
class Report
  def initialize
    @title = 'Monthly Report'
    @text = ['Things are going', 'really, really well!']
  end

  def output_report(format)
    if format == :plain
      puts("*** #{@title} ***")
    elsif format == :html
      puts('<html>')
      puts(' <head>')
      puts(" <title>#{@title}</title>")
      puts(' </head>')
      puts(' <body>')
    else
      raise "Unknown format: #{format}"
    end

    @text.each do |line|
      if format == :plain
        puts(line)
      else
        puts("  <p>#{line}</p>")
      end
    end

    if format == :html
      puts(' </body>')
      puts('</html>')
    end
  end
end

puts Report.new.output_report(:plain)
puts Report.new.output_report(:html)
