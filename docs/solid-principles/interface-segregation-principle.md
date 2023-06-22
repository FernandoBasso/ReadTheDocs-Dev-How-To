# Interface Segregation Principle

- [Interface Segregation Principle](#interface-segregation-principle)
  - [Print and Fax Machines (bad design)](#print-and-fax-machines-bad-design)
  - [Print and Fax Machines (better design)](#print-and-fax-machines-better-design)
  - [Identifying ISP Violations](#identifying-isp-violations)

> No client should be forced to depend on methods it does not use.

## Print and Fax Machines (bad design)

**NOTE**: This design violates the Interface Segregation Principle.

**INFO**: Methods marked with «noop» mean they perform no operation at all. They are blank, empty, no-operation methods overridden just to satisfy the non-segregated `Multi` interface.

```{uml}
@startuml
skinparam DefaultFontName Source Code Pro
skinparam DefaultFontSize 15

interface Multi {
  +print(): void
  +getPrintSpoolDetails(): String
  +scan(): void
  +scanPhoto(): void
  +fax(): void
  +internetFax(): void
}

note top of XeroxWorkCenter
XeroxWorkCenter really provides
all the functionality described
in the Multi interface.
endnote

class XeroxWorkCenter implements Multi {
  +print(): void
  +getPrintSpoolDetails(): String
  +scan(): void
  +scanPhoto(): void
  +fax(): void
  +internetFax(): void
}

note bottom of XeroxWorkCenter
All methods really
do something!
end note

class HPPrinterAndScanner implements Multi {
  +print(): void
  +getPrintSpoolDetails(): String
  +scan(): void
  +scanPhoto(): void
  +fax(): void <<noop>>
  +internetFax(): void <<noop>>
}

note bottom of HPPrinterAndScanner
fax() and internetFax()
do nothing.
end note

class CannonPrinter implements Multi {
  +print(): void
  +getPrintSpoolDetails(): String
  +scan(): void <<noop>>
  +scanPhoto(): void <<noop>>
  +fax(): void <<noop>>
  +internetFax(): void <<noop>>
}

note bottom of CannonPrinter
Only fax() and getPrintSpoolDetails()
do something. The other methods
do nothing.
end note

@enduml
```

We have an interface describing 8 methods. Three classes implement this interface. One of them really implements all the methods, but the other two classes implement only some methods, leaving other method with *blank* “implementations”, meaning, invoking those methods do nothing for those classes.

Overriding methods with *blank/empty* implementations is almost always an indication of questionable, smelly, or bad design altogether.

The interface is not *segregated* here. It tries to do too much. Again, “No client should be forced to depend on methods it does not use.”

A programer creates an instance of `CannonPrinter` and observes they can invoke the `internetFax()` method. Except it won't do anything. The developer will trust the editor/IDE auto-complete and intellisense features and assume that method really does send an internet fax.

If I create an `ArrayList` and I see it has an `add()`, is it too much to expect that it adds the value/object to the list? No. We expect it to do just that. Same if I see an `internetFax()` method. I expect it sends internet fax. Simple as that.

This is a catastrophe in the making! Don't design things like this.


## Print and Fax Machines (better design)

**NOTE**: This design abides by the Interface Segregation Principle.

Let's split the big interface into smaller, more specialized interfaces.

```{uml}
@startuml
skinparam DefaultFontName Source Code Pro
skinparam DefaultFontSize 15

interface Print {
  +print(): void
  +getPrintSpoolDetails(): String
}

interface Scan {
  +scan(): void
  +scanPhoto(): void
}

interface Fax {
  +fax(): void
  +internetFax(): void
}

class XeroxWorkCenter {
  +print(): void
  +getPrintSpoolDetails(): String
  +scan(): void
  +scanPhoto(): void
  +fax(): void
  +internetFax(): void
}

class HPPrinterAndScanner {
  +print(): void
  +getPrintSpoolDetails(): String
  +scan(): void
  +scanPhoto(): void
}

class CannonPrinter {
  +print(): void
  +getPrintSpoolDetails(): String
}

Print <|.up. XeroxWorkCenter
Scan <|.up. XeroxWorkCenter
Fax <|.up. XeroxWorkCenter

Print <|.down. HPPrinterAndScanner
Scan <|.down. HPPrinterAndScanner

Print <|.down. CannonPrinter
@enduml
```

First of all, we *segregated* the bloated interface into smaller, cohesive interface. The smaller interfaces more closely abide by the Single Responsibility principle.

Now classes are not forced to have blank implementations for methods they really cannot honor. For example, `CannonPrinter` will not have a blank, untruthful `fax()` method. It now only implements methods it can really fulfill.

As an aside, it would be possible to have a *parent* interface with common methods, and then more dedicated and specialized interfaces adding further methods as necessary.

## Identifying ISP Violations

- Fat interfaces (too many methods);
- Interfaces with low cohesion (methods that don't relate or really belong together);
- Empty method implementations;

Fat interfaces may be the a consequence of breaking the Single Responsibility Principle. An interface have a single responsibility.

Printing, scanning and faxing are three different concepts. No single interface should really handle all those concepts

By segregating the interfaces, we indirectly end up following the Liskov Substitution Principle, because we can replace the class type `CannonPrinter` with the interface `Print`, and vice-versa.

Most of the SOLID Principles are intricately linked to one another.

> SOLID Principles complement each other, and work together in unison, to achieve the common purpose of well-designed software.

```{uml}
@startuml
skinparam DefaultFontName Source Code Pro
skinparam DefaultFontSize 15

interface Print {
  +print(): void
  +getPrintSpoolDetails(): String
}

interface Scan {
  +scan(): void
  +scanPhoto(): void
}

interface Fax {
  +fax(): void
  +internetFax(): void
}

class XeroxWorkCenter {
  +print(): void
  +getPrintSpoolDetails(): String
  +scan(): void
  +scanPhoto(): void
  +fax(): void
  +internetFax(): void
}

class HPPrinterAndScanner {
  +print(): void
  +getPrintSpoolDetails(): String
  +scan(): void
  +scanPhoto(): void
}

class CannonPrinter {
  +print(): void
  +getPrintSpoolDetails(): String
}

Print <|.up. XeroxWorkCenter
Scan <|.up. XeroxWorkCenter
Fax <|.up. XeroxWorkCenter

Print <|.down. HPPrinterAndScanner
Scan <|.down. HPPrinterAndScanner

Print <|.down. CannonPrinter
@enduml
```
