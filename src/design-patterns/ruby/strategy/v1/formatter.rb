##
# A base abstract class.
#
class Formatter
  ##
  # This method should be implemented by subclasses.
  #
  def output_report(_title, _text)
    raise 'Implement `output_report` in `Formatter` subclasses.'
  end
end
