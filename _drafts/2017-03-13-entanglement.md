---
layout: post
title: Entanglement
date: '2017-03-12T01:57:24+02:00'
---

<img data-src="{{ '/images/entanglement/ios-screen1.png' | prepend: site.baseurl }}" alt="">

# Entanglement?

Some time ago there was a game popular over there, called Entanglement:

<img data-src="{{ '/images/entanglement/web-screen1.webp' | prepend: site.baseurl }}" alt="">

There are a few implementations of this game under Android:

<img data-src="{{ '/images/entanglement/android-screen1.webp' | prepend: site.baseurl }}" alt="">
<img data-src="{{ '/images/entanglement/android-screen2.webp' | prepend: site.baseurl }}" alt="">

But there was no such game for iOS. And as I pursued my second M. Sc. degree, I have had a course
"iOS development", where we were learning to use Swift and iOS platform.

As a course project I decided to implement this game. In Swift. Originally the game was implemented
in Swift 2 _(there was no Swift 3 back those days)_.

And recently I decided to refactor the game a bit and update it to the most recent Swift version
_(which is Swift 3 by the moment of writing this post)_.

In this post I'll describe some interesting decisions I made while creating this game.

<!--more-->

# Field

The core of the game are tiles. The tiles are hexagons. And the whole field is hexagonal.
The problem was to handle the positions of all the tiles in a handy manner. Thus I decided
to go from a standard cartesian coordinate system, which has 90&deg; angle between axes
and go for a one with 120&deg;:

<img data-src="{{ '/images/entanglement/120-deg-coordinate-system.png' | prepend: site.baseurl }}" alt="">

Here, `u` and `v` vectors represent coordinate axes and `w` vector is a diagonal. So the tiles
could now be allocated so their centers are at the points of the coordinate system with integer
coordinates:

<img data-src="{{ '/images/entanglement/tiles-1.png' | prepend: site.baseurl }}" alt="">

# Tiles

Each tile should have six lines. To do that, I created 12 _"connection slots"_, two on each
side of a tile, as shown below:

<img data-src="{{ '/images/entanglement/tile-connections-1.png' | prepend: site.baseurl }}" alt="">

Then, by generating six random pairs of integer numbers from range `[1, 12]`, we can obtain actual
connections inside a single tile. Then a tile might look like this:

<img data-src="{{ '/images/entanglement/tile-sample.png' | prepend: site.baseurl }}" alt="">
