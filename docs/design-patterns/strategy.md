---
title: Strategy Pattern | Design Patterns
description: Concepts, notes and practical examples on the strategy behavioral design pattern.
---

# Strategy Pattern

![Strategy Pattern Diagram](./strategy.assets/strategy-1.drawio.svg)

The *strategy* pattern (a.k.a *policy* pattern) allows selecting an algorithm at *runtime*.
This is achieved by creating a family of objects that implement the same interface so client code can select which one of those objects (which strategy/algorithm) to use at runtime.

The user of the strategy is called the *context* and the algorithms are the *strategies*.

The strategy pattern is based on composition and delegation (not on class inheritance).

## Use Cases

### Report Generator

![Report Generator Strategy Pattern Diagram](./strategy.assets/strategy-report-generator-1.drawio.svg)

The *context* here is `Report`, and the strategies are `PlainTextFormatter` and `HTMLFormatter`. `Report` doesn't know about the details of each report format. It just knows it can call `output_format` from the strategy `@formatter` and be done with it.

#### Ruby

formatter.rb:

:::{literalinclude} /../src/design-patterns/ruby/strategy/report_generator/v1/formatter.rb
:language: ruby
:::

plain_text_formatter.rb:

:::{literalinclude} /../src/design-patterns/ruby/strategy/report_generator/v1/plain_text_formatter.rb
:language: ruby
:::

html_formatter.rb:

:::{literalinclude} /../src/design-patterns/ruby/strategy/report_generator/v1/html_formatter.rb
:language: ruby
:::

report.rb:

:::{literalinclude} /../src/design-patterns/ruby/strategy/report_generator/v1/report.rb
:language: ruby
:::

### Calculating Taxes

A tax calculator may choose different algorithms depending on the state or other attribute.

## References

- [Strategy design pattern (Wikipedia)](https://en.wikipedia.org/wiki/Strategy_pattern).
