##
# A report generator that outputs HTML or plain text.
#
# Note that we always output these, no matter the format:
#
# • Header.
# • Title.
# • Each line of actual report (the content/body of the report).
# • Footer (any trailing stuff required by the format).
#
# So, we can Define an abstract base class with a master method that
# performs the basic steps listed above, but that leaves the details of
# each step to a subclass.
#
# Pros of this implementation:
#
# • Adheres to the principle of separating code that stay
#   the same from code that changes.
##

##
# An *abstract* report class (except Ruby does not have abstract
# classes, so, we make do by throwing exceptions if client code try to
# invoke those methods directly).
#
# Format-specific report classes will inherit from this `Report` class
# and override all methods, except `output_report`.
#
class Report
  def initialize
    @title = 'Monthly Report'
    @text = ['Things are going', 'really, really well.']
  end

  ##
  # The methods we call here should be overridden accordingly by
  # subclasses.
  #
  # We throw exceptions for “abstract” methods so subclasses are
  # forced to implement then. However, we DO NOT throw for methods
  # that do not always need to be overridden, because a few would
  # be implemented as empty/do nothing in the concrete classes.
  #
  # For example, `output_body_end` (and a few others) need to do nothing
  # for Plain Text, so, why force `PlainTextReport` to implement then?
  #
  def output_report
    output_start
    output_head
    output_body_start
    output_body
    output_body_end
    output_end
  end

  def output_body
    @text.each do |line|
      output_line(line)
    end
  end

  ##
  # A hook method. Does nothing by default. `HTMLReport` would override
  # it, but perhaps `PlainTextReport` would not.
  def output_start; end

  ##
  # This hook method outputs the title by default. It helps document
  # what is expected from child classes should they wish to override
  # this method: “Oh, I need to output the title here!”
  #
  def output_head
    output_line(@title)
  end

  ##
  # Another hook method. May or may not be overridden.
  #
  def output_body_start; end

  ##
  # This one MUST be overridden by all subclasses. We throw an exception
  # to make sure if it is called without implementation, client code
  # will know immediately about it.
  #
  # All report types must output “a line” according to their type.
  #
  def output_line(_line)
    raise 'Called abstract method: output_line'
  end

  ##
  # Hook method. Not all report types need a specific body end. Some
  # will simply need nothing at all so they can use this default, empty
  # implementation.
  #
  def output_body_end; end

  ##
  # Hook method. Not all report types need a special “output ending”
  # therefore not all report types will need to override this
  #
  def output_end; end
end

##
# Generates HTML report.
#
# This class overrides all methods from `Report` except for
# `output_report`.
#
class HTMLReport < Report
  def output_start
    puts('<html>')
  end

  def output_head
    puts(' <head>')
    puts(" <title>#{@title}</title>")
    puts(' </head>')
  end

  def output_body_start
    puts('<body>')
  end

  def output_line(line)
    puts(" <p>#{line}</p>")
  end

  def output_body_end
    puts('</body>')
  end

  def output_end
    puts('</html>')
  end
end

##
# Generates Plain Text report.
#
# Note that some methods do not need to output anything, so, they are
# defined just to override the “abstract” methods in the “abstract”
# `Report` base class, except for `output_report`.
#
class PlainTextReport < Report
  def output_start; end

  def output_head
    puts
    puts '-' * 48
    puts("**** #{@title} ****")
    puts
  end

  def output_body_start; end

  def output_line(line)
    puts(line)
  end

  def output_body_end; end

  def output_end; end
end

html_report = HTMLReport.new
plain_text_report = PlainTextReport.new

html_report.output_report

plain_text_report.output_report
