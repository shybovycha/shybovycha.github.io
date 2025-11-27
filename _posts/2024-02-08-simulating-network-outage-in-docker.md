---
layout: post
title: "Simulating network outages in Docker"
date: '2024-02-08T14:01:24+10:00'
tags: [docker, networking, devops, testing, firewall, iptables, containers, linux, debugging, infrastructure]
---

Recently, for my work project I needed to simulate a system of ours going offline for a period of time (similar to having a network outage on a customer side).

I figured there are two ways to do it:

* using Docker' networks
* using host OS' firewall

With Docker network it is as easy as

```sh
docker network disconnect <network-name> <container-name>
```

This way you disconnect a container from a network (even if it is a bridge network, exposing the container to the host OS).

You can find a list of networks with `docker network ls` and list of containers with `docker ps`.

To roll it back (simulate recovery from an outage), simply

```sh
$ docker network connect <network-name> <container-name>
```

Disconnecting a container from a network is okay, but sometimes you might want to have a fine-grain control over the outage,
like forbid a specific IP or port being accessed by your container.

With the firewall it is totally possible, but the steps differ for Linux and OSX.

In Linux you use `iptables` and control a rule group specific to Docker only:

```sh
$ iptables -I DOCKER -p tcp --dport 27017 -j DROP
```

The above command will block all TCP traffic on port `27017` for all Docker containers.
To revert this, run

```sh
$ iptables -I DOCKER -p tcp —dport 22 ACCEPT
```

To control a specific IP address use the `-s` parameter:

```sh
$ iptables -I DOCKER -p tcp -s 192.168.0.10 --dport 27017 -j DROP
$ iptables -I DOCKER -p tcp -s 192.168.0.10 --dport 27017 -j ACCEPT
```

On the other hand, OSX uses a tool with BSD roots, `pf`.
You can use the `/etc/pf.conf` file to mess with firewall rules.

Blocking traffic is achieved by adding a rule like below to the `/etc/pf.conf` file:

```
block drop out quick proto tcp from any to any port 27017
```

followed by reloading the rule list with

```sh
$ pfctl -f /etc/pf.conf
```

In case `pf` is disabled, one is enabled running

```sh
$ pfctl -e
```

Re-enabling is as easy as removing (or commenting out) the rule line in the `/etc/pf.conf` file and reloading it with `pfctl -f /etc/pf.conf`.
