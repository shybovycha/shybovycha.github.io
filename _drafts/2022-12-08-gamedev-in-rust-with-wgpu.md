---
layout: post
title: 'Gamedev in Rust with WGPU'
date: '2022-12-08T00:24:00+00:00'
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

## The ugly

IDE support - it is simply non-existent - Vim, Emacs, VS Code and 3rd party plugin for IntelliJ is all you get.
And I have no idea how to debug Rust code.
