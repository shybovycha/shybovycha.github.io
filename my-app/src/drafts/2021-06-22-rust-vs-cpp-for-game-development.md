---
layout: post
title: "Rust vs C++ for game development"
date: 22-06-2021T11:13:09+0800
---

Recently I've stumbled upon a blog (Rust vs C++ for game development)[https://blog.logrocket.com/rust-vs-c-for-game-development/].

It seemed more like Rust propaganda rather than a valid comparison for many reasons: it put Blender, Panda3D and Unity in the C++ game engines section, which is unreasonable enough already. Yet the blog completely missed the Unreal Engine, which is ridiculous. The blog completely missed the C++ tools section, yet listed a ton of "tools" in the Rust section (despite vast majority of them being direct ports from C++). And the piece of code comparing OOP vs more functional-like (called "data-oriented" in the blog) approach.

The only reason to use Rust and not C++ for game development listed in the article is memory safety, preventing memory leaks (you won't need to remember this statement, since this is the main selling point of Rust, but this would draw an interesting discussion down the line).

Since I have been doing some graphics and game development recently, I was quite inspired by the blog. So here is my take on the matter.

## General issues with the blog

There were few things with the original blog that I just wanted to get straight:

* Unity is purely C#
* Blender and Panda3D are both Python with the latter offering some C++ API (IIRC)
* Panda3D is as old as Python 2
* Unreal Engine, which is one of the most popular engines and which is entirely written in C++ (and its API is C++), is completely missing from the list.
* the C++ tools section is completely blank, despite offering many more things than Rust ecosystem
* the comparison of OOP and non-OOP approaches is invalid, since the latter could easily be implemented in C++ - it is apples to oranges comparison
* the libraries for Rust are all translations of the exact same C++ libraries, AFAIK

## C++ for game development

I won't give any comments on the standard or preferrence when it comes to developing games, since it heavily depends on the developer(-s) and the project requirements.

But I can comment on C++ tools I've been working with.

### Graphics APIs

There are official SDKs for all of the major graphics APIs in C++ (well, maybe except OSX' Metal) - Vulkan, OpenGL and DirectX.

### Development tools

There are plenty of IDEs and toolkits developed specifically for C++: Visual Studio, CLion, and many more lightweight or less specialized options such as VS Code, Emacs, VIM, Eclipse.

Talking about compilers, there are Visual Studio, GNU, LLVM / Clang families of compilers, all equipped with the decent debugger.

TODO: debugger comparison - data representation / "view data as...", call stack frames inspection, system libraries views

On the sad note, build tools and dependency management in C+ world is horrible, despite the solid mature age of the language. Things like CMake are clumsy to use, Bazel and Buck are not really supported by the existing libraries and frameworks, xmake is quite young yet promising and fills the gaps between CMake and Make.

On the dependency management note, it is truly pathetic with only VCPKG getting up to speed since 2019 and others such as Conan being well behind. And yet all of them barely contain a fraction of existing solutions in the repositories or allow extremely limited functionality, lacking such important features as versioning and compatibility requirements.

TODO: build file example for CMake + VCPKG vs xmake; vcpkg and conan repo overview (amount of packages, some packages update frequency)

### Ready solutions

There is an insane amount of ready and well-tested, well-supported (including community and good amount of official and 3rd party documentation) engines and frameworks for C++.

Think Unity and Unreal Engine for starters - they both include all the features to kick-start your game development. With the asset stores built-in, you can literally build a full-featured game in matter of days. Some simpler options include Godot engine.

More specialized options, such as OGRE, gfx, Dilligent Engine are extremely powerful graphics / rendering engines.

Physics simulation engines include Bullet Physics, PhysX and Newton Game Dynamics.

On a much lower scale, there are libraries designed for window and input handling, such as SDL (1 & 2) and SFML. They also offer other features for image loading and manipulation, graphics context initialization and sound handling.

The lowest level of abstraction is just using the plain graphics API directly. The missing features could be fulfilled with the use of 3rd party libraries, such as GLM for math, Open Assimp for 3D model loading, STB libraries for image loading, GLAD for OpenGL API calls, etc.

TODO: list with last update / release dates?

## Rust for game development

The main selling point of Rust is memory safety. With its ownership approach, it promises to rarely (if ever) leak the memory. And still offer the performance compared to the one of C++ or even C (with less template and abstraction layer clutter).

### Graphics APIs

DirectX is only accessible via COM (which is essentially a Windows dependency injection system). And the existing crates for DirectX in Rust are either more than 5 years old (now in 2021) or are archived and not supported anymore.

There are libraries providing the pointers to OpenGL functions in some of the 3rd party packages such as [glium](https://github.com/glium/glium), [gl-rs](https://github.com/brendanzab/gl-rs/)

TODO: find more about this

### Development tools

TODO: find more of this

### Ready solutions

TODO: find more of this

## A word about memory safety

The issue of memory leaking is often raised when talking about C++ vs anything else. The thing is: with C++11 standard, it is a de-facto standard to use the smart pointers, which utilize reference counting to prevent memory leaks. Surely enough, if you write your code with raw pointers, you expose your program to memory leak issues.

But this is not to forget Rust also allows the `unsafe` blocks. Which are extensively used in both FFI (foreign function interface, a mechanism used to call existing functions from external libraries, which is utilized by most of the existing Rust libraries) and existing 3rd party libraries (such as [`windows-rs`](https://github.com/microsoft/windows-rs) and [`com-rs`](https://github.com/microsoft/com-rs)).

In my opinion, a valid comparison would show the differences in implementing specific aspects of a game with both languages (image manipulation, using different graphics APIs, shaders, loading assets, build tools and IDEs, debuggers, dependency managers).

C++ and Rust worlds are totally different since C++ ecosystem has been around for longer and many more tools have emerged and matured with it - as opposite to Rust ecosystem, which can barely offer a fraction of what C++ world has. But not to say the development tools are better in C++ - they are just awful IMO.