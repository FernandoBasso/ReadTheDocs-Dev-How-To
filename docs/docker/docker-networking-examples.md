---
title: Networking Examples :: Docker
description: Some practical examples of working with, inspecting and debugging Docker networking
---

# Networking Example 1

## Intro

By default, containers connect to the so-called “default bridge network”.
In this post, we'll explore a few options including the default and user defined bridge networks.

When the Docker daemon is started, it creates a default bridge network (which is inferior to user-defined bridge networks), and then, when containers are run, unless otherwise specified, they attach to that default bridge network by default.

Read more:

- [Networking overview :: Docker Engine docs](https://docs.docker.com/network/)
- [Bridge network driver :: Docker Engine docks](https://docs.docker.com/network/drivers/bridge/)

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

In my case when testing and writing this, Docker assigned the IP 172.17.0.2 to serv1, and 172.17.0.3 to serv2.
Also, by default Docker creates “bridge” networks for our containers.
On the host machine (I'm running Arch Linux as I write this), Docker also creates a lot of stuff, including the `docker0` network bridge:

```text
$ ip addr show docker0
4: docker0: <BROADCAST,MULTICAST,UP,LOWER_UP> mtu 1500 qdisc noqueue state UP group default 
    link/ether 02:42:16:6f:0d:76 brd ff:ff:ff:ff:ff:ff
    inet 172.17.0.1/16 brd 172.17.255.255 scope global docker0
       valid_lft forever preferred_lft forever
    inet6 fe80::42:16ff:fe6f:d76/64 scope link proto kernel_ll 
       valid_lft forever preferred_lft forever
```

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

And note how the (auto-generated) host names for each container matches what is their `/etc/hosts` (as one would expect):

```text
$ docker exec serv1 hostname
22019a981e98

$ docker exec serv2 hostname
a5c2f180fd88
```

Both containers can ping themselves and also one another using their IP addresses.

For example, from serv1, ping serv2 by IP address:

```text
$ docker exec serv1 ping -c 1 172.17.0.3
PING 172.17.0.3 (172.17.0.3): 56 data bytes
64 bytes from 172.17.0.3: icmp_seq=0 ttl=64 time=0.062 ms
--- 172.17.0.3 ping statistics ---
1 packets transmitted, 1 packets received, 0% packet loss
round-trip min/avg/max/stddev = 0.062/0.062/0.062/0.000 ms
```

And, dropping into serv2's shell, ping serv1 by IP address:

```text
$ docker exec -it serv2 bash
root@41e947121645:/# ping -c 1 172.17.0.2
PING 172.17.0.2 (172.17.0.2): 56 data bytes
64 bytes from 172.17.0.2: icmp_seq=0 ttl=64 time=0.088 ms
--- 172.17.0.2 ping statistics ---
1 packets transmitted, 1 packets received, 0% packet loss
round-trip min/avg/max/stddev = 0.088/0.088/0.088/0.000 ms
```

And as a side note, from the host, it is possible to ping the containers by their IPs:

```text
$ ping -c 1 172.17.0.2
PING 172.17.0.2 (172.17.0.2) 56(84) bytes of data.
64 bytes from 172.17.0.2: icmp_seq=1 ttl=64 time=0.045 ms

--- 172.17.0.2 ping statistics ---
1 packets transmitted, 1 received, 0% packet loss, time 0ms
rtt min/avg/max/mdev = 0.045/0.045/0.045/0.000 ms
```

## Pinging by hostname

As the containers do have a hostname (an auto-generated hash) as we saw earlier, let's try to ping one container from the other using that hostname:

```text
$ docker exec serv2 hostname
41e947121645

$ docker exec serv1 ping -c 1 41e947121645
ping: unknown host
```

With the current setup, it is not possible to ping containers from one another or the host system using their (auto-generated) host names.

Let's stop the two running containers as we are going to try something else next.

```text
$ docker stop serv{1,2}
serv1
serv2
```

And then let's run two new containers and giving them an explicit, custom (not auto-generated) hostname:

```text
$ for n in 1 2 ; do
    docker run --rm --detach \
      --hostname "$serv_$n" \
      --name "serv_$n" nginx_ping
  done

798d3628c5baab3a5e133e60836ca95b4c8020abf7b006eb5719ee9aa00e8bcc
263c7a760a1dcd4af81243fda2722b410b4fc8ecace7c778cff77572c7062556
```

The host names will simply be the same as the container name, that is, ‘serv_1’ and ‘serv_2’.

To recap, in the previous attempt, we could only reach one container from another through their IP addresses because we were using the default bridge network and letting the containers auto-generate a host name.
However, by running containers and giving them explicit host names, they still automatically attach to the default bridge network, but the still cannot reach one another by host name.

```text
$ docker inspect bridge | jq '.[0].Containers'
{
  "0a5ca3ab2989c6c192d5d4a81c49b107d9fde5f639efb0c3169ce23c7fbbdac2": {
    "Name": "serv_1",
    "EndpointID": "d6a51b6b24b3be4371ab186a569bd378eff733455cb30800db9c8143afe7fc97",
    "MacAddress": "02:42:ac:11:00:02",
    "IPv4Address": "172.17.0.2/16",
    "IPv6Address": ""
  },
  "87991a26c7efe30a8677a8cc0c826850c03f687d4b8c31eb1099a87b4c358814": {
    "Name": "serv_2",
    "EndpointID": "41f674d8d72fe8fa2a2ebbf619f7859043294eb02856e118641524e47010b6d7",
    "MacAddress": "02:42:ac:11:00:03",
    "IPv4Address": "172.17.0.3/16",
    "IPv6Address": ""
  }
}
```

![Docker Network Example 1](/staticassets/docker-hostname-ex-1.png)

Observe how our containers are using the host names we provided, besides IP address and other related stuff.

Finally, let's try to ping one another by the custom host name:

```text
$ docker exec serv_1 ping -c 1 serv_2
ping: unknown host
```

Still not working!

If we read [the docs](https://docs.docker.com/network/drivers/bridge/#differences-between-user-defined-bridges-and-the-default-bridge), as of 2023, they say this:

> Containers on the default bridge network can only access each other by IP addresses, unless you use the --link option, which is considered legacy.
> On a user-defined bridge network, containers can resolve each other by name or alias.

Well, let's refrain from resorting to legacy features.
Software development presents enough challenges already without we looking for trouble 😅.

On the next example we'll explore a user-defined bridge network, which is recommended over the default bridge.
