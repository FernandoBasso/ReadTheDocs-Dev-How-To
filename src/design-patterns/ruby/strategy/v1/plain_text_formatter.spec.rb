require 'rspec'
require_relative 'report'
require_relative 'plain_text_formatter'

describe PlainTextFormatter do
  it 'should output document title' do
    report = Report.new(PlainTextFormatter.new)

    expect do
      report.output_report
    end.to output(/===== Nostromo Final Report =====/).to_stdout
  end

  it 'should should output document body' do
    report = Report.new(PlainTextFormatter.new)

    lines_to_match = [
      'This is Ripley, last survivor of the Nostromo.',
      'Signing off.'
    ].join("\n")

    expect do
      report.output_report
    end.to output(/#{lines_to_match}/).to_stdout
  end
end
