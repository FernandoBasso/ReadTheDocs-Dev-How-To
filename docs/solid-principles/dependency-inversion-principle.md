# Dependency Inversion Principle

- [Dependency Inversion Principle](#dependency-inversion-principle)
  - [Intro](#intro)
  - [eCommerce Example (bad design)](#ecommerce-example-bad-design)
  - [eCommerce Example (better design)](#ecommerce-example-better-design)
  - [Dependency Injection](#dependency-injection)

## Intro

> High-level modules should not depend on low-level modules. Both should depend on abstractions.

> Abstractions should not depend on details. Details should depend on abstractions.

## eCommerce Example (bad design)

```{uml}
@startuml
skinparam DefaultFontName Source Code Pro
skinparam DefaultFontSize 15
skinparam RankSep 50

package "eCommerce" {
  node "High Level Components" {
    component ProductCatalog
    component PaymentProcessor
    component CustomerProfile
  }

  interface Communication

  note right of Communication
  <b>Communication</b> is both
  low and hight level module.
  end note

  node "Low Level Components" {
    component SQLProductRepository

    component GooglePayGateway
    component WireTransfer

    component EmailSender
    component VoiceDialer
  }

  ProductCatalog ..> SQLProductRepository: depends on

  PaymentProcessor ..> GooglePayGateway: depends on
  PaymentProcessor ..> WireTransfer: depends on

  CustomerProfile ..> Communication: depends on

  Communication ..> EmailSender: depends on
  Communication ..> VoiceDialer: depends on
}
@enduml
```

Is `Communication` a high-level module, or a low-level module? Tricky question. It is both.

From the `CustomerProfile` module's perspective, then `Communication` is a low-level module, but from the `EmailSender` and `VoiceDialer` modules, it is high-level.

A module being high- or low-level is not an absolute fact, but a relative one. It depends on the context of the module relative to the surrounding modules.

Remember:
> **High-level modules should not depend on low-level modules. Both should depend on abstractions.**

Our “depends on” associations goes directly against the principle above. Our hight-level modules should depend on abstractions instead.

`ProductCatalog` (a high-level module) is directly depending on `SQLProductRepository` (a low level-module), violating the Dependency Inversion Principle.

Here's a class diagram of our bad design (considering only `ProductCatalog` for now):

```{uml}
@startuml
skinparam DefaultFontName Source Code Pro
skinparam DefaultFontSize 15

class ProductCatalog {
  +listAllProducts(): void
}

class SQLProductRepository {
  +getAllProductNames(): List<String>
}

ProductCatalog -up-> SQLProductRepository: uses

@enduml
```

## eCommerce Example (better design)

We should not instantiate `SQLProductRepository` directly. Instead, we should create a `ProductRepository` interface, and create a product repository factory abstraction to be used by `ProductCatalog. 

```{uml}
@startuml
skinparam DefaultFontName Source Code Pro
skinparam DefaultFontSize 15
skinparam RankSep 50

package "eCommerce" {
  node "High Level Components" {
    component ProductCatalog
  }

  node "Abstractions" {
    component ProductFactory
  }

  node "Low Level Components" {
    component SQLProductRepository
  }

  ProductCatalog ..> ProductFactory: depends on
  ProductFactory ..> SQLProductRepository: depends on
}
@enduml
```

Here's a class diagram for the `ProductCatalog` high-level component:

```{uml}
@startuml
skinparam DefaultFontName Source Code Pro
skinparam DefaultFontSize 15

interface ProductRepository {
  +getAllProductNames(): List<String>
}

class SQLProductRepository {
  +getAllProductNames(): List<String>
}

class ProductFactory {
  {static} create(): ProductRepository <<static>>
}

class ProductCatalog {
  +listAllProducts(): void
}

ProductCatalog -r-> ProductFactory: uses
ProductCatalog --> ProductRepository: depends on
SQLProductRepository .l.|> ProductRepository: implements
ProductFactory --> ProductRepository: uses
ProductFactory --> SQLProductRepository: uses
ProductRepository --> SQLProductRepository: depends on

@enduml
```

Now, our reference type is `ProductRepository` (an interface), making it loosely coupled with `SQLProductRepository`.

```java
public class ProductCatalog {
  public void listAllProducts() {
    ProductRepository productRepository = ProductFactory.create();

    List<String> products = productRepository.getAllProductNames();

    for (String product : products) {
      System.out.println(product);
    }
  }
}
```

What about this?

> **Abstractions should not depend on details. Details should depend on abstractions.**

The low-level modules are the modules that deal with the details. Note that now `ProductRepository` depends on `SQLProductRepository`, and not the other way around. Great, we are following the guideline above as well.

## Dependency Injection

Notice even though we delegated the responsibility of creating a product repository to a factory, `ProductCatalog` is still responsible for bootstrapping this instantiation. Ideally, `ProductCatalog` should not worry about where and when this instantiation should occur.

We could provide the instantiated product repository to the `ProductCatalog` class even without it explicitly asking for it.

Here's the class diagram of this new design:

```{uml}
@startuml
skinparam DefaultFontName Source Code Pro
skinparam DefaultFontSize 15

interface ProductRepository {
  +getAllProductNames(): List<String>
}

class ECommerceApplication {
  +main(): void
}

class SQLProductRepository {
  +getAllProductNames(): List<String>
}

class ProductFactory {
  {static} create(): ProductRepository <<static>>
}

class ProductCatalog {
  +ProductCatalog(ProductRepository: repository): void
  +listAllProducts(): void
}

ECommerceApplication -l-> ProductCatalog: uses
ECommerceApplication -d-> ProductFactory: uses
ProductCatalog --> ProductRepository: depends on
SQLProductRepository .r.|> ProductRepository: implements
ProductFactory --> ProductRepository: uses
ProductFactory --> SQLProductRepository: uses
ProductRepository --> SQLProductRepository: depends on
@enduml
```

We introduced a new class `ECommerceApplication` that provides an instance of `ProductRepository` to `ProductCatalog` through its constructor. That relieves `ProductCatalog` from worrying about where and when to instantiate a `ProductRepository`.

Dependency Injection not only avoids tight coupling, and goes further as it completely dissociates a class from going out of its way to instantiate its dependencies.
