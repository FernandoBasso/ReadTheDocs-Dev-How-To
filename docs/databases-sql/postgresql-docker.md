---
title: PostgresSQL on Docker
description: Some notes and examples on how to run PostgreSQL on Docker.
---

# PostgreSQL Docker

```shell-session
$ docker exec -it postgresql14-playground-1 psql -U devel -W
```

```
docker run -it --rm \
	-e POSTGRES_HOST_AUTH_METHOD=scram-sha-256 \
	-e POSTGRES_INITDB_ARGS=--auth-host=scram-sha-256 \
  -e POSTGRES_USER=user1 \
	-e POSTGRES_PASSWORD=h3cr3t \
	--name pg1 \
	postgres:14
```

Nothing seems to allow me to use md5, or password, or scram-sha-256...

> initdb: warning: enabling "trust" authentication for local connections You can
> change this by editing pg_hba.conf or using the option -A, or
> --auth-local and --auth-host, the next time you run initdb.
