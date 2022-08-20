---
title: Template Method Pattern | Design Patterns
description: Concepts, notes and practical examples on the template method behavioral design pattern.
---

# Template Method Pattern

![Template Method](./template-method.assets/template-method-1.drawio.svg)

The template method is a behavioral design pattern.
It implemented as a method in a supperclass which defines an skeleton of operations in terms of high-level steps, which are then implemented by subclass methods.

Non-abstract methods that can be overridden in the concrete subclasses of the template method pattern are called *hook methods*.

Hook methods allow subclasses to override the base implementation and do something different or accept the default implementation.

Hook methods are also also used to let subclasses know (document) what is expected from them should they wish to override the parent implementation.
Hook methods may be empty, or contain some code.

One important thing to note about the template method pattern is that it is built based on inheritance (not through composition or implementing interfaces).

Using inheritance means the subclasses are always tangled up with the superclass.
It is just how inheritance works.

Also, we get little or no runtime flexibility.
Once we select a concrete implementation, we can only select a different implementation by changing the source code instead being able to do it at runtime.

## Use Cases

### Report Generator

A report generator where we would generate HTML, Plain Text, XML, JSON, etc. reports.

Note that we always output these, no matter the format:

- Header.
- Title.
- Each line of actual report (the content/body of the report).
- Footer (any trailing stuff required by the format).

So, we can Define an abstract base class with a master method that performs the basic steps listed above, but that leaves the details of each step to a subclass.

Pros of this implementation:

- Adheres to the principle of separating code that stay the same from code that changes.

![Template Method Report Generator Example](./template-method.assets/template-method-report-generator-1.drawio.svg)

### Report Generator Ruby Example

:::{literalinclude} /../src/design-patterns/ruby/template-method/v4/report.rb
:language: ruby
:::

### Email

An email can be sent using HTML and/or Plain Text, and with or without attachments.
This could be a good fit for this pattern.

## References

- [Template method pattern (Wikipedia)](https://en.wikipedia.org/wiki/Template_method_pattern).
