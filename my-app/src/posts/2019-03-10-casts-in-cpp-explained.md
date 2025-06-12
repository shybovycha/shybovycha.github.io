---
layout: post
title: 'Type casts in C++, explained'
date: '2019-03-21T10:00:56+10:00'
tags:
- c++
- programming
---

As explained by my colleague, former professional C++ developer (now writes schwifty Java).
Below is a quick explanation of four basic C++ type casts. Just for the matter of shorter post,
consider each cast taking a template type `T` and a parameter value `v` like this: `const_cast<T>(v)`.

* `const_cast` - removes the `const` from the type of `v` (think of casting `const char*` will produce `char*`)
* `static_cast` - C-style, unchecked explicit type casting (just like in old 90s: `(int) 3.14`)
* `reinterpret_cast` - hard low-level treating a block of memory as type `T`, no matter what resides in that memory
* `dynamic_cast` - does the runtime checks of param type and template type
