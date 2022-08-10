---
title: Command Line | Ruby on Rails
description: Useful command line examples used with Ruby on Rails.
tags: ror, rails, ruby, console, shell, command-line
---

# Command Line | Ruby on Rails

tags: [ror, rails, ruby, console, shell, command-line]

## Basic Commands

IMPORTANT: By convention, model names are camel-cased singular and correspond to lowercased plural table names.
So an Article model expects a table named articles; a Person model expects a table named people.

```shell-session
$ rails db:create
$ rails db:create:all
$ rails dbconsole
$ rails db:migrate
```

Creating a model also creates unit tests and migrations for that model.

Show docs for generating a model:

```shell-session
$ rails generate model
```

Suppose you already have a database and need no migration:

```shell-session
$ rails generate model Category --no-migration
```

Generate migration from a docker container:

```shell-session
$ docker exec -it rails-cms_rails_1 \
    bin/rails generate model Article

$ rails db:migrate --dry-run
$ rails db:rollback
```

Some other commands:

```shell-session
$ rails generate model Article
$ rails generate controller articles
$ rails destroy controller articles
$ rails db:rollback
$ rails destroy model Article
```

```shell-session
$ rails generate scaffold Article \
    title:string{256} \
    body:text \
    published_at:datetime

$ rails generate scaffold Article \
    title:string{256} \
    body:text \
    excerpt:string{512} \
    location:string{256} \
    published_at:datetime --no-migration

$ rails generate migration \
    add_excerpt_and_location_to_articles \
    excerpt:string{512} \
    location:string{256}
```
