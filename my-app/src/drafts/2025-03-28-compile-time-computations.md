---
layout: post
title: 'Compile-time computations'
tags: [cpp, game-development, programming, c++, game-programming, git, development-tools, version-control, gamedev]
---

Since `constexpr` and `consteval` were introduced in C++, there have been quite a few posts on LinkedIn about this.
Apparently, this is a game-changer - now even standard library utilizes compile-time.
I have tried my best to come up with a use case for this fancy new feature, but ultimately failed.
And since I am not a C++ professional, I asked the right people on the same LinkedIn posts about this:

> i wonder how often you would write such code in real life? or rather, is there a real-world example where constexpr is actually helpful?

The answers I got were very disappointing, in my opinion:

> One example is when you don't wanna have magic numbers in your code. For example you have a constant which comes from some calculation, you just write the equation and make constexpr (consteval is more appropriate in this case) . Your code will be more readable but no runtime punishment.

And another one pretty useless:

> Please, take a look at:
>
> - `std::array`
> - `std::vector`
> - `std::unique_ptr`
> - `std::string_view`
>
> There are many functions inside those classes that uses `constexpr`.

The code in the original post was this:

```cpp
#include <cstdint>

constexpr uint64_t factorial(int32_t n) {
  uint64_t result = 1;
  for (int i = 2; i <= n; ++i) {
    result *= i;
  }
  return result;
}

int main() {
  return factorial(5);
}
```

Some time ago I have heard [`Zig`](https://ziglang.org/) uses compile-time for template / generic programming. Curious idea, but I never really explored it.

Then my friend, FireMage, suggested me to try [`D`](https://dlang.org/) (which I vaguely explored quite a long time ago). He himself created a [compile-time template parser/compiler for D](https://github.com/katyukha/darktemple), which is a rather cool idea. He has also mentioned was D has access to the file system at compile time.

With that said, there are plethora of things I wish to try out myself!

I have been ~~trying to come up with a simple WYSIWYG editor for my~~ working on my [chicken shooter game](https://github.com/shybovycha/shoot-them/tree/rework-2024).
The idea I have right now is to have OpenGL shaders be defined as C++ classes, with shader code hard-coded as a string literal.
All the shader inputs (uniform variables) have locations which you have to obtain when linking shader, so for performance considerations, each variable location could be persisted in class member at startup and then used by reference in the code via a dedicated setter method.
The issue with this approach, however, is that there is quite a lot of dummy code that has to come with each shader.
But OpenGL has APIs to retrieve all the uniform variables and their locations from a shader.
Putting all things together, my big idea is to have shader classes generated at compile time from the shader code (also available at compile time).

