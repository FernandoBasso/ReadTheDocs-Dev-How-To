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

Let's run two containers from our previously created ‘nginx_ping’ custom image.
Run this command line on your terminal:

```text
$ for n in 1 2 ; do
    docker run --rm --detach --name "serv$n" nginx_ping
  done

22019a981e986869791eee9d32d2f3b21dec482b880c92257d846162986219b2
a5c2f180fd8803916bbe927df56271c508652c1dfce2e8263378aff1adcb1180
```

Now `docker ps` should show `serv1` and `serv2` are running.
For example, we can inspect them.

```text
$ docker inspect serv1
$ docker inspect serv2
```

Because `docker inspect` returns a JSON output, we could use [jq](https://github.com/jqlang/jq) to help us filter out the output:

```text
$ docker inspect serv1 | jq '.[0].NetworkSettings.Networks'
{
  "bridge": {
    "IPAMConfig": null,
    "Links": null,
    "Aliases": null,
    "NetworkID": "f3a83b1d58b94272b4344a530061dd206d3f5fbf837316a7456d2acf80034e06",
    "EndpointID": "8ed0975e8da0949300fcf093d7446665cf4252bd683c6cd74aecd9e7dce4d840",
    "Gateway": "172.17.0.1",
    "IPAddress": "172.17.0.2",
    "IPPrefixLen": 16,
    "IPv6Gateway": "",
    "GlobalIPv6Address": "",
    "GlobalIPv6PrefixLen": 0,
    "MacAddress": "02:42:ac:11:00:02",
    "DriverOpts": null
  }
}
```

In my case when writting this, Docker assinged the IP 172.17.0.2 to serv1, and 172.17.0.3 to serv2.

Let's look at their `/etc/hosts`:

```text
$ docker exec serv1 cat /etc/hosts
127.0.0.1	localhost
::1	localhost ip6-localhost ip6-loopback
fe00::0	ip6-localnet
ff00::0	ip6-mcastprefix
ff02::1	ip6-allnodes
ff02::2	ip6-allrouters
172.17.0.2	22019a981e98

$ docker exec serv2 cat /etc/hosts
127.0.0.1	localhost
::1	localhost ip6-localhost ip6-loopback
fe00::0	ip6-localnet
ff00::0	ip6-mcastprefix
ff02::1	ip6-allnodes
ff02::2	ip6-allrouters
172.17.0.3	a5c2f180fd88
```

```text
$ docker exec serv1 hostname
22019a981e98

$ docker exec serv2 hostname
a5c2f180fd88
```

Both containers can ping themselves and also one another using their IP addresses.

For example, let's make serv1 ping serv2's by IP address:

```text
$ docker exec serv1 ping -c 1 172.17.0.3
PING 172.17.0.3 (172.17.0.3): 56 data bytes
64 bytes from 172.17.0.3: icmp_seq=0 ttl=64 time=0.062 ms
--- 172.17.0.3 ping statistics ---
1 packets transmitted, 1 packets received, 0% packet loss
round-trip min/avg/max/stddev = 0.062/0.062/0.062/0.000 ms
```

Let's stop the two running containers as we are going to try something else next.

```text
$ docker stop serv{1,2}
serv1
serv2
```
