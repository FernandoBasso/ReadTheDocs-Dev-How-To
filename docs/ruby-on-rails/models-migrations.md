---
title: Models and Migrations | Ruby on Rails
description: Some notes and useful examples on models and migrations on Ruby on Rails.
tags: ror, rails, ruby, model, migration, command-line
---

# Models and Migrations | Ruby on Rails

tags: [ror, rails, ruby, model, migration, command-line]

IMPORTANT: By convention, model names are camel-cased singular and correspond to lowercased plural table names.
So an Article model expects a table named articles; a Person model expects a table named people.

## Creating models and migrations

Creating a model also creates unit tests and migrations for that model.

```shell-session
$ rails generate model

$ rails generate model Category --no-migration

$ docker exec -it rails-cms_rails_1 \
  bin/rails generate model Article
```

```
class CreateArticles < ActiveRecord::Migration[6.1]
  def change
	create_table :articles do |t|
	  t.string :title
	  t.text :body
	  t.datetime :published_at
	  t.timestamps
	end
  end
end
```

Schema is the term given to the properties that make up a table: the table’s name, its columns, and its column types, as well as any default values a column will have.

```shell-session
$ rails db:migrate --dry-run
$ rails db:rollback
```

:::{admonition} Active Record
:class: info

In his book Patterns for Enterprise Application Architecture, Martin Folwer proposed the idea that a one-to-one mapping exists between a database record and the object that represents it. David Heinemeier Hansson based his/rails ORM on Fowler's pattern.
:::

## Operations on the model

An example rails console session:

```text
$ bin/rails console
Article.column_names
ap Article
Article.methods.size

rails db:reset

article = Article.new
article.new_record?
article.attributes

article.title = 'foo'
article.body = 'bar'
article.published_at = Date.new
article.save
article.new_record?
article.id

Time.zone.now
```

Careful:

```
article = Article.new(title: 'Active Record', body: 'May the record be with you').save
```

`article` is true, not the record/model, because that `save` method returns `true`.

```
Article.create(title: 'The Force', body: 'Learn the ways of the force')
Article.where('published_at IS NOT NULL')
```

- `article.save` returns `true` or `false`.
- `article.create(...)` `returns the created object.

```rb
begin
  Article.find(1037)
rescue ActiveRecord::RecordNotFound
  puts "We couldn't find that record"
end
```

Some `where` clause examples:

```rb
Article.where('id > %d', 1)
Article.where(['id > ?', 1])
Article.where('id > %d AND id < %d', 1, 4).pluck(:id, :title)
```

Causes “`disallow_raw_sql!':
Query method called with non-attribute argument(s): [:id, :title] (ActiveRecord::UnknownAttributeReference)”
Article.where('id > %d AND id < %d', 1, 4).pluck([:id, :title])

:::{admonition} RoR 6 update vs update_attributes on older versions
:class: warn

In RoR >= 6 we use `update`, while on older versions we use `update_attributes`.
:::

```
Article.first.update(published_at: 1.day.ago)
```

There are two styles of row deletion: destroy and delete.
The destroy style works on the instance.
The delete style operates on the class, which is to say it operates on the table rather than a given row from that table.

```rb
Article.create(title: 'Del me', body: '...')
```

Doesn't work because where returns a an array-like collection relation):

```rb
Article.where(title: 'Del me').destroy
```

The above requires ID params in order to work. But this works:

```rb
Article.where(title: 'Del me').first.destroy
```

Find and destroy removes from database but keeps the object around. Although the object remains hydrated (retains all its attributes), it’s frozen.

The last line with awesome print still display the article attributes, even though it has been destroyed from the DB:

```rb
article.find(1)
article.destroy
ap article
```

And we can destroy multiple rows:

```rb
Article.destroy(1)
Article.destroy([1, 7, 25])
```

`delete` and `delete_all`: The delete family of methods differs from destroy in that they don’t instantiate or perform callbacks on the object they’re deleting.
They remove the row immediately from the database.

```
Article.delete(4)
Article.delete([10, 11])
```

`Model.find(1, 2, 3)` interprets args as array, but `Model.delete(1, 2, 3)` fails and does not interpret args as array.
We must use `Model.delete([1, 2, 3])` array explicitly.

```rb
Article.find(1).update(published_at: 5.days.ago)
Article.find_by(['published_at < ?', 3.days.ago])
Article.delete_by(['published_at < ?', 2.days.ago])
```

In the docs `Foo#meth` means you can call the meth method on an instance of `Foo class` (but not  `Foo.meth` on the class itself).

## Errors

```text
$ rails console
article = Article.new
article.save
article.errors
» <ActiveModel::Error attribute=title, type=blank, options={}>


> article.errors.full_messages
> article.errors.messages
=> {:title=>["can't be blank"], :body=>["can't be blank"]}
errors.messages[:body]
=> ["can't be blank"]

> article = Article.new
» #<Article:0x0000558063cac388 id: nil, title: nil...
> article.save
» false
> article.errors.size
» 2
> article.valid?
» false
```

Model enhancement is when we endow models with attributes and capabilities that go beyond what is gotten with subclassing.

We say that models associate to one another.

Wrap a table called users:

```rb
class User < ActiveRecord::Base
end
```

To add methods to a model is to add domain logic to them.
The model, thus, encapsulates all the domain logic: access rules, validations, relationships, etc.

FAT MODELS are models that have some logic that would sometimes be in the view, like formatting a title or date.
Instead of formatting a title or a date in several different views, you have a method on the model that is reused (view helpers could be used for this purpose too).
