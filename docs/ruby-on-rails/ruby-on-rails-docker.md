---
title: Ruby on Rails and Docker | Ruby on Rails
description: Some times and examples on how to dockerize and run a Ruby on Rails applications on docker and how to solve common problems.
tags: ror, rails, docker, container
---

# Ruby on Rails and Docker

tags: [ror, rails, docker, container]

## bundler “/ is not writable”

```shell-session
$ docker exec -it simple_cms_rails_1 \
    bin/rails generate controller Subjects index show new edit delete

`/` is not writable.
Bundler will use `/tmp/bundler20210424-45-3hj1l45' as your home directory temporarily.
Running via Spring preloader in process 63
...rest of the output...
```

Another side effect is that files become un-writable for the developer on the host machine.

The problem is that running docker with user with same *uid:gid* of host machine causes problems because that user has no name and no home inside the container.
Seems like this solves it:

```text
#
# Dockerfile.rails.dev
#

RUN useradd \
  --create-home \
  --home-dir /usr/src/app \
  --shell /bin/bash \
  --groups sudo,root \
  --uid 1000 \
  devel
```

Then, of course, we need to run migrations, then we should be able to run tests:

```
$ docker exec -it search-repos_api_1 bash
root@081715d7c20b:/usr/src/app# bin/rails db:migrate RAILS_ENV=test

root@081715d7c20b:/usr/src/app# exit

$ docker exec -it search-repos_api_1 bin/rails test
```

## Can't run tests

```shell-session
$ docker exec -it search-repos_api_1 bin/rails test
Running via Spring preloader in process 36
<redacted>/active_record/connection_adapters/postgresql_adapter.rb:81:in
`rescue in new_client':
FATAL:  database "api_test" does not exist (ActiveRecord::NoDatabaseError)
```

That is easy! Run:

```shell-session
$ docker exec -it search-repos_api_1 rake db:test:prepare
$ docker exec -it search-repos_api_1 bin/rails test
```
