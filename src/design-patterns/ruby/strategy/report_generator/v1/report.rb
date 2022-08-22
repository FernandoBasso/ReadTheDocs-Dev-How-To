##
# A reporter class that delegates reporting to instances of subclasses
# of `Formatter`.
#
class Report
  attr_reader :title, :text
  attr_accessor :formatter

  ##
  # We need an instance of `Formatter` here.
  #
  def initialize(formatter)
    @formatter = formatter

    @title = 'Nostromo Final Report'
    @text = [
      'This is Ripley, last survivor of the Nostromo.',
      'Signing off.'
    ]
  end

  ##
  # `output_report` is implemented in subclass of `Formatter`.
  #
  def output_report
    @formatter.output_report(@title, @text)
  end
end
