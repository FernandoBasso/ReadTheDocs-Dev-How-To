# Open Closed Principle

- [Open Closed Principle](#open-closed-principle)
  - [Insurances Example](#insurances-example)
    - [Initial (flawed) Design](#initial-flawed-design)
    - [Improved Design](#improved-design)
  - [Benefits of OCP](#benefits-of-ocp)

## Insurances Example

We'll start with a design that violates the Open Closed Principle, which forces us to modify and add code to a class every time we want to add a new insurance type. Then we'll improve on it to a (better) design that follows the Open Closed Principle, which enables us to add new features without modifying existing code.

### Initial (flawed) Design

```java
public class HealthInsuranceCustomerProfile {
  public boolean isLoyalCustomer() {
    return true;
  }
}

public class InsurancePremiumDiscountCalculator {
  public int
  calcPremiumDiscountPercent(HealthCustomerInsuranceProfile customer) {
    if (customer.isLoyalCustomer()) {
      return 20;
    }

    return 0;
  }
}
```

Note that `calcPremiumDiscountPercent()` method takes a `HealthInsuranceCustomerProfile` object.


```{uml}
@startuml
skinparam DefaultFontName Source Code Pro
skinparam DefaultFontSize 15

class InsurancePremiumDiscountCalculator {
  +calcPremiumDiscountPercent(HealthInsuranceCustomerProfile customer): void
}

class HealthCustomerInsuranceProfile {
  +isLoyalCustomer(): boolean
}

InsurancePremiumDiscountCalculator ..> HealthCustomerInsuranceProfile : uses
@enduml
```

What if we need to calculate discount for another type of insurance?

```java
public class VehicleInsuranceCustomerProfile {
  public boolean isLoyalCustomer() {
    return true;
  }
}
```

```{uml}
@startuml
skinparam DefaultFontName Source Code Pro
skinparam DefaultFontSize 15

class InsurancePremiumDiscountCalculator {
  +calcPremiumDiscountPercent(HealthInsuranceCustomerProfile customer): void
  +calcPremiumDiscountPercent(VehicleInsuranceCustomerProfile customer): void
}

note top of InsurancePremiumDiscountCalculator
We have to **overload**
calcPremiumDiscountPercent() to take
the the new type as argument.
endnote

class HealthCustomerInsuranceProfile {
  +isLoyalCustomer(): boolean
}

class VehicleCustomerInsuranceProfile {
  +isLoyalCustomer(): boolean
}

InsurancePremiumDiscountCalculator ..> HealthCustomerInsuranceProfile : uses
InsurancePremiumDiscountCalculator ..> VehicleCustomerInsuranceProfile : uses
@enduml
```

What if we add house insurance, life insurance, and other types of insurances? With the current design, we have to keep creating new overloads for the `calcPremiumDiscountPercent()` method for each new type of insurance.

To add a new feature, we have to keep adding code to the existing `InsurancePremiumDiscountCalculator` class, violating the Open Closed Principle. The existing code is supposed to be closed for modification (it is OK to create new classes or other things, though).

### Improved Design

We'll add an interface `CustomerProfile` to the design, keep only one `calcPremiumDiscountPercent()` method, and make the insurance classes implement that interface.

```{uml}
@startuml
skinparam DefaultFontName Source Code Pro
skinparam DefaultFontSize 15
top to bottom direction

interface CustomerProfile {
  +isLoyalCustomer(): boolean
}

class HealthCustomerInsuranceProfile implements CustomerProfile {
  +isLoyalCustomer(): boolean
}

class VehicleCustomerInsuranceProfile implements CustomerProfile {
  +isLoyalCustomer(): boolean
}

InsurancePremiumDiscountCalculator .up.> HealthCustomerInsuranceProfile : uses
InsurancePremiumDiscountCalculator .up.> VehicleCustomerInsuranceProfile : uses

class InsurancePremiumDiscountCalculator {
  +calcPremiumDiscountPercent(CustomerProfile customer): int
}
@enduml
```

With this design, whenever we want to add new insurance types, we simply create new classes that implement `CustomerProfile`, not even needing to touch the the calculator class.

## Benefits of OCP

- Easy of adding new features;
- Minimal cost of testing and developing new features;
- Decouples the design. By applying Open Closed Principle, we also “unwittingly” also abode by the Single Responsibility Principle. That is, to follow OCP, we implicitly also end up following SRP.
  - Applying OCP is a subjective decision, rather than an objective one.

**WARNING**: Use OCP judiciously. Don't blindly apply this principle to every design. It will sometimes create a huge number of classes or components that may make the entire design more complex and harder (not easier) to maintain. Use your best judgement.
