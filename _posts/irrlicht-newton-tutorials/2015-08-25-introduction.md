---
layout: post
title: "Irrlicht Newton GD tutorial: introduction"
date: '2015-08-25T18:01:00+01:00'
---

## What will you learn?

This tutorial covers the development of a game from a very beginning. This includes:

* planning the **architecture** for a game
* developing the **game engine**
* defining game logic with **scripts**
* creating **3d models**
* and **distributing the application**

And of course, the whole tutorial is built around **Irrlicht** and **Newton Game Dynamics** libraries.

As for *{{ site.time | date: '%d %b %Y' }}*, the tutorial covers:

* an introduction to NewtonGD
* creating Irrlicht-powered application
* controlling the application logic with Lua scripts
* building application with CMake

## Why writing everything from scratch? Why not use Unity / Unreal Engine / *you-name-it*?

There are some well-known and used game engines like **Unreal Engine 4**, **Unity 3D** and others. They all come with enormous amount of learning materials. So why this tutorial might be interesting for you? You might want to know how things work and thus get most flexibility out of your tools. Or even start working on building your own tools.

## What should I know to proceed?

I expect you to have at least some experience with these three things:

* C++
* computer graphics
* game development

The latter - to make sure, you will not call 3d models “textures” or miss the “script” word’ meaning.

## Why these libraries?

Irrlicht is easy to use and contains all the features you will need to build a game - resource management, support for majority of asset formats out-of-the-box, user input (keyboard, mouse and joystick events handling), GUI (Graphical User Interface - buttons, text inputs, etc.).

Irrlicht is easy to use and contains all the features you will need to build a game - resource management, support for majority of
asset formats out-of-the-box, user input *(keyboard, mouse and joystick events handling)*, GUI
*(Graphical User Interface - buttons, text inputs, etc.)*.

Newton Game Dynamics is also very easy to use; it is constantly developed; and it is darn powerful!

If you are still interested - please proceed to
<a href="/irrlicht-newton-tutorials/2015/08/26/application-architecture.html" class="btn btn-success">the first chapter</a>.
