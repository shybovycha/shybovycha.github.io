---
layout: post
title: Erlang practice
date: '2015-01-28T15:29:00+01:00'
tags:
- rtfm
- erlang
- programming
- tutorial
tumblr_url: http://shybovycha.tumblr.com/post/109392787226/erlang-practice
---

## Foreword

First time I faced functional programming, I was impressed. Just a bit. That was back in 2012. The second time, I was studying real functional programming at the university. Today was the final test.

We were taught many interesting things about functional programming and lot of things about Haskell. But there were only two lectures and two practices on Erlang. And nothing was told about its distributed programming abilities.

I was a bit disappointed by this fact. So, I turned on my girlfriend's laptop, installed Erlang and created this short intro to distributed programming in Erlang.

<!--more-->

## Requirements

This short intro does not include Erlang tutorial and requires you to have at least two machines - either Virtual or hardware, if you wish. Even that does not really matter!

## How to get out of a train?

<img src="https://31.media.tumblr.com/151302ca545100e6c298290f0d5827fe/tumblr_inline_niw5irzdiz1qh5oee.jpg" alt="Gordon Freeman trying to get out of a train"/>

The first thing I gonna tell you, is really handy tip.

**There are three ways to exit Erlang' shell:**

* **"classic":** hit <kbd>Ctrl + C</kbd>, then press <kbd>a</kbd> _(Abort)_ and <kbd>Return</kbd>
* **"UNIX-way":** two times hit <kbd>Ctrl + C</kbd>
* **"Erlang-way":** simply type `q().` and hit <kbd>Return</kbd>

You'll be happy to know 'bout last two - they are just easier!

## Rock'n'Roll!

So, let's just dive into distributed programming! First thing you'll gonna need - is to know your machines' IP addresses. Then you'll gonna need to point each of them to the other one - just set each other's hostname in the `/etc/hosts` file _(for Windows it's `C:\Windows\system32\drivers\etc\hosts`)_s.

I have had two laptops I named `moonode` _(my laptop)_ and `foo` _(my girlfriend's laptop)_. So, on my Ubuntu, I added this line to `/etc/hosts`:

```
192.168.2.33    foonode
```

And in `C:\Windows\system32\drivers\etc\hosts` on my girlfriend's laptop I added this:

```
192.168.2.237   moonode
```

<img src="https://31.media.tumblr.com/97da86d4a989b381e3b6be0069ebfdff/tumblr_inline_niw5kauPUD1qh5oee.png" alt="hosts in Windows"/>

**Note:** I was sitting at home, so laptops were connected just to my home WiFi router. And that's great news for enyone, who wants to try that at home!

Each Erlang instance was run with the corresponding shortname of machine: `erl -sname moo@moonode` and `erl -sname foo@foonode`.

Both machines should have the same cookie to communicate. That's basic Erlang security, for your great good. Cookie is just a upper-cased word, stored in a `.erlang.cookie` file. For Windows, that file is in the `C:\Windows\` or `C:\Users\username\` directory. In Linux that's in `/home/username/` directory.

## Wrapping-up

So, short summary on what you should have to run distributely in Erlang:

* hostname(-s) of other node(-s) in your `hosts` file
* same cookie for all your nodes in `.erlang.cookie` file
* running instances with corresponding shortnames and hosts

## Running stuff

To show some code, I wrote this short module:

```erlang
-module(test).
-export([ start/0 ]).

start() ->
    receive
        { msg, M } ->
            io:format(">> ~s~n", [ M ]),
            start();

        finish ->
            io:format("< finish received >~n"),
            ok
    end.
```

Now, let's start that!

First, I registered that process with some name on the `foo` node:

```erlang
c(test1).
register(foo_pid, spawn(test1, start, [])).
```

And then, the only thing I needed to do - is just send messages from node `moo`!

```erlang
{ foo_pid, foo@foonode } ! { msg, "Obey!" }.
flush().
```

<img src="https://31.media.tumblr.com/2e65bfca4e6815d4eced646bee021e5f/tumblr_inline_niw5l8tH3Y1qh5oee.png" alt="moonode"/>

<img src="https://31.media.tumblr.com/36462ec965b63fc9183a216f4be73c8d/tumblr_inline_niw5lnNbgc1qh5oee.png" alt="foonode"/>

Much cooler than writing ping-pong programs, huh? =)
