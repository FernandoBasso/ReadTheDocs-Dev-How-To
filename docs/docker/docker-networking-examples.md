---
title: Networking Examples :: Docker
description: Some practical examples of working with, inspecting and debugging Docker networking
---

# Networking Example 1

## A custom nginx image with ping utility

Create a `Dockerfile` to build an image derived from `nginx` official image and install the `ping` command line utility:

```yaml
FROM nginx:latest

##
# Directory inside the container.
#
# WORKDIR /home/node/app

# COPY app /home/node/app

RUN apt update
RUN apt install inetutils-ping --yes

##
# CMD executes when we actually run an instance of the
# container from that image.
#
# Executed when we do `docker run ...`.
#
CMD ["nginx", "-g", "daemon off;"]
```

Then build the image:

```text
$ docker build . --tag nginx_ping
```

Then, run the ‘nginx_ping’ image as a container naming it ‘pingserv1’:

```text
$ docker run --rm --detach --name pingserv1 nginx_ping
```

```{tip}
The `--rm` option is used so that after the container is stoped, it (the container) is removed and we don't end up with lots of one-off, test, temporary containers lingering around.

We also use the `--detach` flag so the container runs in detached mode and we get the shell prompt back to run further commands.
```

The container should now be running:

```
$ docker ps
CONTAINER ID   IMAGE         COMMAND                  CREATED          STATUS          PORTS      NAMES
1190f8c095cf   nginx_ping    "/docker-entrypoint.…"   15 seconds ago   Up 15 seconds   80/tcp     pingserv1
```

Then, from that container ‘pingserv1’, let's ping the Arch Linux website:

```text
$ docker exec pingserv1 ping -c 1 www.archlinux.org
PING www.archlinux.org (95.217.163.246): 56 data bytes
64 bytes from 95.217.163.246: icmp_seq=0 ttl=45 time=234.463 ms
--- www.archlinux.org ping statistics ---
1 packets transmitted, 1 packets received, 0% packet loss
round-trip min/avg/max/stddev = 234.463/234.463/234.463/0.000 ms
```

Finally, stop the container:

```text
$ docker stop pingserv1
```

## Pinging containers through their IPs

