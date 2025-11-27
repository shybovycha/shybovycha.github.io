---
layout: post
title: 'Gamedev in Rust with WGPU'
date: '2022-12-08T00:24:00+00:00'
tags: [rest, cpp, backend, game-development, text-editors, programming, c++, opengl, graphics-programming, api]
---

## The good

Build system - it exists and does not require users to mess with semi-random build instructions just to build the project with few (literally few) dependencies.

WGPU is an awesome invention - not only does it have a neat API, but it also supports Vulkan, DX11, DX12 and OpenGL as rendering backend.
The shading language is also very pleasant to work with.

## The bad

The situation with libraries in Rust is pathetic: there are either heaps or zero packages (crates) for the same thing,
for the most part marked as "work in progress" and having the version like `0.16.1-alpha`. Most of them have last updates from ages ago.

<img src="/images/gamedev-in-rust-with-wgpu/crates-search-1-assimp.png" loading="lazy" alt="Searching for 'assimp' on crates.io">
<img src="/images/gamedev-in-rust-with-wgpu/crates-search-2-bullet-physics.png" loading="lazy" alt="Searching for 'bullet physics' on crates.io">
<img src="/images/gamedev-in-rust-with-wgpu/crates-search-3-physx.png" loading="lazy" alt="Searching for 'physx' on crates.io">
<img src="/images/gamedev-in-rust-with-wgpu/crates-search-4-graphics-math.png" loading="lazy" alt="Searching for 'graphics math' on crates.io">

This makes selecting the right tool for the job really tricky.

Using good old SDL or SFML is much more complex than in native C++: the docs mention there are either unresolved thread-safety issues (with SFML) or one needs to build the thing from sources (with SDL).

Crates' version compatibility is another issue - consider two crates, `imgui` and `imgui-wgpu`. The latter requires `imgui="0.8"` and will fail if your project has anything newer as a dependency.
Dependencies are not transitive, which makes this whole ecosystem similar to CMake and vcpkg.

## The ugly

IDE support - it is simply non-existent - Vim, Emacs, VS Code and 3rd party plugin for IntelliJ is all you get.
And I have no idea how to debug Rust code.

If you want to use `imgui` on Windows - good luck, you need to setup `g++` - the existing crate does not know how to select an existing compiler.
However, this only happens if you are ~~lazy like me~~ a normal advanced Windows user and used Chocolately to install Rust.
In fact, no one will tell you that is wrong. But if you want the best experience, you should download the `rustup-init` thingo from the Rust website,
make it install Cargo and mess with environment variables (e.g. set the `Path` correctly - it rarely works automagically). Then you will get the compiler
set up for use with MSVC instead of Clang or GCC.

And back to the crates and dependency versions: sometimes you might see compile-time errors like this one

```
error[E0308]: mismatched types
   --> src\main.rs:171:9
    |
169 |     platform.attach_window(
    |              ------------- arguments to this function are incorrect
170 |         imgui.io_mut(),
171 |         &window,
    |         ^^^^^^^ expected struct `winit::window::Window`, found a different struct `winit::window::Window`
    |
    = note: expected reference `&winit::window::Window` (struct `winit::window::Window`)
               found reference `&winit::window::Window` (struct `winit::window::Window`)
    = note: perhaps two different versions of crate `winit` are being used?
note: associated function defined here
```

So you will have to align the crates versions to the exact bugfix version.
