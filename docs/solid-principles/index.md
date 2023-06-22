---
title: SOLID Principles
description: Notes, tips and examples on the SOLID Principles.
---

# SOLID Principles

This section contains concepts, notes and examples with diagrams and code about design patterns.

We mostly show text, diagrams and the main pieces of code in the pages, and [unit tests can be found in the source code directory](https://gitlab.com/devhowto/Dev-How-To/-/tree/main/src/design-patterns) for each example.

Some notes and examples on [SOLID principles](https://en.wikipedia.org/wiki/SOLID).

- SRP: Single Responsibility
- OCP: Open–Closed
- LSP: Liskov Substitution
- ISP: Interface Segregation
- DIP: Dependency Inversion

**NOTE**: The [main repo is the one on Gitlab](https://gitlab.com/devhowto/solid-principles). The [Github repo](https://github.com/devhowto/SOLID-principles) is only a mirror. Discussion, issues, PRs, etc. should be happen on [Gitlab](https://gitlab.com/devhowto/solid-principles).

**NOTE**: Gitlab renders the PlantUML diagrams (Github does not). Therefore, we strongly recommend that this text be read on the Gitlab repo.

## Java

* [Maven in 5 Minutes (official docs)](https://maven.apache.org/guides/getting-started/maven-in-five-minutes.html)

The Java setup was achieve with this:

``` 
$ mvn archetype:generate \
    -DgroupId=dev.fernandobasso.solid \
    -DartifactId=java \
    -DarchetypeArtifactId=maven-archetype-quickstart \
    -DarchetypeVersion=1.4 \
    -DinteractiveMode=false
```

And tested with the first time with:

```text

$ cd java

$ mvn package 

$ java \
    -cp target/java-1.0-SNAPSHOT.jar \
    dev.fernandobasso.solid.App
```

### Running the Examples

Check `java/Makefile`. Run the examples with something like:

```text
$ make hello
$ make lsp_productsdiscount_nok1
```

```{toctree}
---
maxdepth: 6
caption: Topics
---

open-closed-principle.md
liskov-substitution-principle.md
interface-segregation-principle.md
dependency-inversion-principle.md
```

