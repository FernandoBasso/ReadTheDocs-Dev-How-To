# Template Pattern



Non-abstract methods that can be overridden in the concrete classes of the Template Method pattern are called *hook methods*.

Hook methods allow subclasses to override the base implementation and do something different or accept the default implementation.

Hook methods are also also used to let subclasses know (document) what is expected from them should they wish to override the parent implementation. Hook methods may be empty, or contain some code.
