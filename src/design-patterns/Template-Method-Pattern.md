# Template Method Pattern

Non-abstract methods that can be overridden in the concrete classes of the Template Method pattern are called *hook methods*.

Hook methods allow subclasses to override the base implementation and do something different or accept the default implementation.

Hook methods are also also used to let subclasses know (document) what is expected from them should they wish to override the parent implementation. Hook methods may be empty, or contain some code.

One important thing to note about the template method pattern is that it is built based on inheritance (not through composition or implementing interfaces).

Using inheritance means the subclasses are always tangled up with the superclass.
It is just how inheritance works.

Also, we get little or no runtime flexibility. Once we select a concrete implementation, we can only select a different implementation by changing the source code instead of doing it at runtime.

## Use Cases for the Template Method Pattern

### Report Generator

A report generator where we would generate HTML, Plain Text, XML, JSON, etc. reports.

### Email

An email can be sent using HTML and/or Plain Text, and with or without attachments.
This could be a good fit for this pattern.

## References

- [Template method pattern (Wikipedia)](https://en.wikipedia.org/wiki/Template_method_pattern).
