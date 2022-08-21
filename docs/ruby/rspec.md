---
title: RSpec Tips and Examples | Ruby
description: Some notes, tips and examples on how to test for specific stuff with RSpec.
---

# RSpec | Ruby

## Assert STDOUT output

We want to assert output to STDOUT:

```rb
it 'should work' do
  expect { puts 'Aayla Secura' }.to output(/secura/i).to_stdout
end
```

With some class method that outputs to STDOUT:

```rb
describe PlainTextFormatter do
  it 'should output report title' do
    report = Report.new(PlainTextFormatter.new)

    expect do
      report.output_report
    end.to output(/===== Nostromo Report =====/).to_stdout
  end

  it 'should should output report body content' do
    report = Report.new(PlainTextFormatter.new)

    lines_to_match = [
      'This is Ripley, last survivor of the Nostromo.',
      'Signing off.'
    ].join("\n")

    expect { report.output_report }.to output(/#{lines}/).to_stdout
  end
end
```

### References

- [RSpec output matcher docson Relish](https://relishapp.com/rspec/rspec-expectations/v/3-11/docs/built-in-matchers/output-matcher).
- [Testing STDOUT output in RSpec (StackOverflow)](https://stackoverflow.com/questions/16507067/testing-stdout-output-in-rspec).
