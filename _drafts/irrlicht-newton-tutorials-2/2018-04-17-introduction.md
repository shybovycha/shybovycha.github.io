---
layout: post
title: "Game development tutorials: introduction"
tags: [cpp, irrlicht, physics, game-development, newton-dynamics, 3d-graphics, game-programming, gamedev]
---

Game development is a very broad topic with variety of choices. There are tons of material on this topic already,
so you might ask: "why another one?". My answer is this set of tutorials itself. Since last revision of my old [Game development with Newton Game Dynamics and Irrlicht]() tutorial, I came to a conclusion that I'd not stick to one specific technology / framework but rather show a comparison of a few so that you can choose which one to continue working with.

And since every game is different, some things might not even be relevant for your case - it makes little to none sense having 3D physics simulation in a tic-tac-toe game. So each article in this tutorials set will be completely optional to follow and you can go through them in whatever order you like.

_Yet it might sound fun and handy, having to cover a few technologies implies a lot of work for me, so these tutorials might come out separated by large fractions of time._

## A few words about architecture

* there are few ways to design a game: 
  - craft levels and 3d models and then manually set event triggers and AI logic (by using coordinates and so on)
  - use level editor and put invisible trigger objects or use object identifiers to get them in the game logic later
  - use level editor and assign each object its logic inside editor itself
  - bundle all the logic (AI, levels, UI, etc.) in one executable
  - extract AI and game / level logic into scripts
