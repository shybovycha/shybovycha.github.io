---
title: 'Writing fast and beautiful code with C++ and D'
layout: post
date: '2016-03-22T08:51:00'
---

## Introduction

Currently I am writing my *(second)* master's thesis. And it's one of the hardest
work I've been doing ever. It's about image and video processing. And I'm using OpenCV.

Using OpenCV is an interesting decision: if you want to create a beautiful OO architecture
for your program, you'd rather use something like Java. But I didn't manage to run OpenCV
in Java =P

So I decided to write my code in C++. Yeah, tough decision... After spending some time implementing that,
I understood why it was not the best decision: my solution was too heavy *(435 LOC)* and it didn't even
contained four major method' implementations!

Then I sit back and thought: *"Couldn't I use C++ for video/image reading only? And write the rest of the code in a more OOP-friendly language?"*. And that's when I started looking for a language with nice syntax and OOP features *(like interfaces, short array syntax, tuples/maps, etc.)* and found D language.

I've been looking at D a very long time ago, but had never actually tried it for writing anything
more complex than a *"Hello, World!"*  program. *"That's my star time!"* - I thought.

My idea was to:

1. create small C++ library for video IO
2. create D program, which processes video, read using that C++ library

D offeres nice C++ interop, except it requires you to define classes/functions signatures you
are importing from C++. That was not a big deal for me since I had no complex classes written yet.

<!--more-->

## First steps

Standard example from a tutorial on D website works like a charm:

`cpp_lib.cpp`:

{% highlight cpp %}
#include <iostream>

using namespace std;

class Base
{
    public:
        virtual void print3i(int a, int b, int c) = 0;
};

class Derived : public Base
{
    public:
        int field;
        Derived(int field) : field(field) {}

        void print3i(int a, int b, int c)
        {
            cout << "a = " << a << endl;
            cout << "b = " << b << endl;
            cout << "c = " << c << endl;
        }

        int mul(int factor);
};

int Derived::mul(int factor)
{
    return field * factor;
}

Derived *createInstance(int i)
{
    return new Derived(i);
}

void deleteInstance(Derived *&d)
{
    delete d;
    d = 0;
}
{% endhighlight %}

`main.d`:

{% highlight d %}
extern(C++)
{
    interface Base
    {
        void print3i(int a, int b, int c);
    }

    class Derived : Base
    {
        int field;
        @disable this();
        override void print3i(int a, int b, int c);
        final int mul(int factor);
    }

    Derived createInstance(int i);
    void deleteInstance(ref Derived d);
}

void main()
{
    import std.stdio;

    auto d1 = createInstance(5);
    writeln(d1.field);
    writeln(d1.mul(4));

    Base b1 = d1;
    b1.print3i(1, 2, 3);

    deleteInstance(d1);
    assert(d1 is null);

    auto d2 = createInstance(42);
    writeln(d2.field);

    deleteInstance(d2);
    assert(d2 is null);
}
{% endhighlight %}

Compiled this one with `c++ -c cpp_lib.cpp -o cpp_lib.o && dmd cpp_lib.o main.d` and ran - *and it worked*.

Now it's time to add some OpenCV code!

## First troubles

`video_reader.cpp`:

{% highlight cpp %}
#include "opencv2/imgproc/imgproc.hpp"
#include "opencv2/imgcodecs.hpp"
#include "opencv2/videoio.hpp"

#include <string>
#include <iostream>

using namespace cv;

class Video {
protected:
    unsigned int frameCount;

public:
    Video(unsigned int _frameCount) : frameCount(_frameCount) {}

    unsigned int getFrameCount() const {
        return frameCount;
    }
};

class VideoReader {
public:
    VideoReader() {}

    Video* readFile(const std::string &filename) {
        VideoCapture *capture = new VideoCapture(filename);

        if (!capture->isOpened()) {
            std::cout << "(C++) Can not open video file\n";
            return 0;
        }

        size_t framesToRead = (size_t) capture->get(CAP_PROP_FRAME_COUNT);

        Video *video = new Video(framesToRead);

        return video;
    }
};
{% endhighlight %}

`main.d`:

{% highlight d %}
extern(C++)
{
    class Video
    {
        @disable this();
        uint getFrameCount();
    }

    class VideoReader
    {
        Video readFile(string filename);
    }
}

void main()
{
    import std.stdio;

    VideoReader reader = new VideoReader();
    Video video = reader.readFile("video.mp4");

    if (video is null) {
        writeln("(D) Could not read video");
    } else {
        writefln("Video length: %d", video.getFrameCount());
    }
}
{% endhighlight %}

Compiling this code should not cause any troubles:

{% highlight bash %}
c++ -c video_reader.cpp -o video_reader.o -I/usr/local/opt/opencv3/include
dmd video_reader.o main.d -L-lstdc++ -L-lopencv_videoio -L-lopencv_core -L-lopencv_imgproc -L-L/usr/local/opt/opencv3/lib
{% endhighlight %}

Instead, there were errors from D compiler:

{% highlight bash %}
Error: Internal Compiler Error: unsupported type string

[2]    97638 segmentation fault  dmd video_reader.o main.d -L-lstdc++ -L-lopencv_videoio -L-lopencv_core
{% endhighlight %}

WTF?! Internal compiler error?! And what's wrong with strings?

## Fixing everything

Alright, maybe it's caused by us,
using `string` type in `VideoReader::readFile(string filename)` method signature, which D could not
simply understand *(since it's not D's string, rather it's a class from C++'s STL library)*.

If we change `string` to `char*`, which is basically a common way to work with strings since C98, it
should work.

`video_reader.cpp`:

{% highlight cpp %}
class VideoReader {
public:
    VideoReader() {}

    Video* readFile(const char* filename) {
        VideoCapture *capture = new VideoCapture(filename);

        if (!capture->isOpened()) {
            std::cout << "(C++) Can not open video file\n";
            return 0;
        }

        size_t framesToRead = (size_t) capture->get(CAP_PROP_FRAME_COUNT);

        Video *video = new Video(framesToRead);

        return video;
    }
};
{% endhighlight %}

`main.d`:

{% highlight d %}
extern(C++)
{
    class VideoReader
    {
        Video readFile(const char* filename);
    }
}
{% endhighlight %}

Yes, D supports pointers, but for interaction with C++ only. You **can** use pointers to create
arrays, but that's not recommended.

Compiling our code with C++ compiler gives no errors, but D is not happy again...

{% highlight bash %}
Undefined symbols for architecture x86_64:
  "VideoReader::readFile(char const*)", referenced from:
      _D4main11VideoReader6__vtblZ in main.o
  "Video::getFrameCount()", referenced from:
      _D4main5Video6__vtblZ in main.o
ld: symbol(s) not found for architecture x86_64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
--- errorlevel 1
{% endhighlight %}

Wow! That's actually not that interesting, but simple to understand: C++ does not generate
any bytecode, which is not used by C++ program. So we should explicitly tell C++ compiler,
which methods we do want to generate code for:

{% highlight cpp %}
class Video {
protected:
  unsigned int frameCount;

public:
  Video(unsigned int _frameCount) : frameCount(_frameCount) {}

  unsigned int getFrameCount() const;
};

unsigned int Video::getFrameCount() const {
  return frameCount;
}

class VideoReader {
public:
  VideoReader() {}

  Video* readFile(const char* filename);
};

Video* VideoReader::readFile(const char* filename) {
  VideoCapture *capture = new VideoCapture(filename);

  if (!capture->isOpened()) {
    std::cout << "(C++) Can not open video file\n";
    return 0;
  }

  size_t framesToRead = (size_t) capture->get(CAP_PROP_FRAME_COUNT);

  Video *video = new Video(framesToRead);

  return video;
}
{% endhighlight %}

The trick here is to first *declare* the whole class. Note: if we extract this declaration to a separate header file - it
won't be compiled, since headers are not **compile units**. And if we then *define* *(implement)* methods, those implementations
will become compile units and the code for them will be generated even if they are not explicitly used in the program.

Now we've made something like standard C++ code - with a *header* and *implementation*.
But D compiler could not link our program again!

{% highlight bash %}
Undefined symbols for architecture x86_64:
  "Video::getFrameCount()", referenced from:
      _D4main5Video6__vtblZ in main.o
ld: symbol(s) not found for architecture x86_64
clang: error: linker command failed with exit code 1 (use -v to see invocation)
--- errorlevel 1
{% endhighlight %}

What? Why? We've declared that method explicitly! Or maybe not?.. Let's look at our object file using `nm` utility:
`nm video_reader.o`.

{% highlight asm %}
00000000000001d8 s GCC_except_table1
0000000000000258 s GCC_except_table10
0000000000000308 s GCC_except_table12
                 U __Unwind_Resume
0000000000000010 T __ZN11VideoReader8readFileEPKc
                 U __ZN2cv12VideoCaptureC1ERKNS_6StringE
                 U __ZN2cv6String10deallocateEv
                 U __ZN2cv6String8allocateEm
0000000000000350 S __ZN2cv6StringC1EPKc
0000000000000440 S __ZN2cv6StringC2EPKc
0000000000000380 S __ZN2cv6StringD1Ev
00000000000004b0 S __ZN2cv6StringD2Ev
0000000000000410 S __ZN5VideoC1Ej
00000000000004d0 S __ZN5VideoC2Ej
0000000000000000 T __ZNK5Video13getFrameCountEv
                 U __ZNKSt3__16locale9use_facetERNS0_2idE
                 U __ZNKSt3__18ios_base6getlocEv
0000000000001030 S __ZNSt3__111char_traitsIcE11eq_int_typeEii
0000000000001050 S __ZNSt3__111char_traitsIcE3eofEv
0000000000000970 S __ZNSt3__111char_traitsIcE6lengthEPKc
                 U __ZNSt3__112basic_stringIcNS_11char_traitsIcEENS_9allocatorIcEEE6__initEmc
                 U __ZNSt3__112basic_stringIcNS_11char_traitsIcEENS_9allocatorIcEEED1Ev
                 U __ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEE6sentryC1ERS3_
                 U __ZNSt3__113basic_ostreamIcNS_11char_traitsIcEEE6sentryD1Ev
0000000000000990 S __ZNSt3__116__pad_and_outputIcNS_11char_traitsIcEEEENS_19ostreambuf_iteratorIT_T0_EES6_PKS4_S8_S8_RNS_8ios_baseES4_
00000000000004f0 S __ZNSt3__124__put_character_sequenceIcNS_11char_traitsIcEEEERNS_13basic_ostreamIT_T0_EES7_PKS4_m
                 U __ZNSt3__14coutE
                 U __ZNSt3__15ctypeIcE2idE
                 U __ZNSt3__16localeD1Ev
                 U __ZNSt3__18ios_base33__set_badbit_and_consider_rethrowEv
                 U __ZNSt3__18ios_base5clearEj
00000000000003c0 S __ZNSt3__1lsINS_11char_traitsIcEEEERNS_13basic_ostreamIcT_EES6_PKc
                 U __ZSt9terminatev
                 U __ZdlPv
                 U __Znwm
00000000000003a0 S ___clang_call_terminate
                 U ___cxa_begin_catch
                 U ___cxa_end_catch
                 U ___gxx_personality_v0
                 U _memcpy
                 U _strlen
{% endhighlight %}

No, we've got everything in place... `__ZNK5Video13getFrameCountEv` points to that...
Okay, that's the mistake in D method signature:

{% highlight d %}
extern(C++)
{
    class Video
    {
        @disable this();
        uint getFrameCount() const;
    }
}
{% endhighlight %}

The `@disable` annotation marks the method *(in our case - constructor of a `Video` class)* to be excluded from
code generation. So in our case we won't have a default constructor for `Video` class, which D generates for us
implicitly. And when we try to link our code, linker can not find the code for a no-argument constructor. So we then
have two options: either we define a no-argument, "default" constructor in our C++ library, **or** we prevent D
from generating it for us.

Yeah! Our code compiled and linked successfully! Now let's run it:

{% highlight bash %}
[2]    99945 segmentation fault  ./video_reader
{% endhighlight %}

What? Wrong again?! Okay, that's really strange. And no debugging information provided.
Adding some debugging info with `writeln()`'s shows it's caused by the `getFrameCount()` method call:

{% highlight d %}
void main()
{
    import std.stdio;

    VideoReader reader = new VideoReader();
    Video video = reader.readFile("video.mp4");

    if (video is null) {
       writeln("(D) Could not read video");
    } else {
        writeln("Testing getFrameCount()...");
        writefln("Video length: %d", video.getFrameCount());
    }
}
{% endhighlight %}

{% highlight bash %}
Testing getFrameCount()...
[2]    523 segmentation fault  ./video_reader
{% endhighlight %}

Maybe it's because D and C++ have different `unsigned int` type size?

{% highlight cpp %}
std::cout << "(C++) sizeof(unsitned int) = " << sizeof(unsigned int) << "\n";
{% endhighlight %}

{% highlight d %}
writefln("(D) sizeof(uint) = %d", uint.sizeof);
{% endhighlight %}

Seems like no:

{% highlight bash %}
(C++) sizeof(unsigned int) = 4
(D) sizeof(uint) = 4
{% endhighlight %}

The trick here is that signature for constant method in C++ differs from such in D:

`C++`:

{% highlight cpp %}
class Video {
public:
    unsigned int getFrameCount() const;
};
{% endhighlight %}

`D`:

{% highlight d %}
class Video
{
    final uint getFrameCount() const;
}
{% endhighlight %}

If we build and run our code now, everything works fine:

{% highlight bash %}
Video length: 318
{% endhighlight %}

<!--
## Build tools

So we now have quite a complex build process: we need to run C++ compiler for library, then we run DMD to compile
and link our D code... May we use existing tools to simplify this?

I will use Bazel for this purpose.

First of all, lets organize our project's directory structure:

{% highlight tree %}
.
├── WORKSPACE
├── lib
│   ├── BUILD
│   └── video_reader.cpp
└── main
    ├── BUILD
    └── main.d
{% endhighlight %}

The `BUILD` files will contain all the build process steps' definitions. And we'll put project settings into
`WORKSPACE` file.

The first step is to build C++ library. We already have the command which does that:

{% highlight bash %}
c++ -c video_reader.cpp -o video_reader.o -I/usr/local/opt/opencv3/include
{% endhighlight %}

This command could be split into variables, describing compilation:

* compiler is `c++`
* source file is `video_reader.cpp`
* output file is `video_reader.o`
* additional header files are located in the `/usr/local/opt/opencv3/include` directory
* we need object file generation *(set by the `-c` flag)*

Since we are building library, we will use a specific Bazel's task for that:

`lib/BUILD`:

{% highlight bazel %}
cc_library(
    name = "video_reader_lib",
    srcs = ["video_reader.cpp"],
    visibility = ["//main:__pkg__"],
)
{% endhighlight %}

The line `visibility = ["//main:__pkg__"]` indicates that the target `video_reader_lib` will be visible from the
`main/BUILD` file.

We now need to specify the OpenCV header files location:

{% highlight bazel %}
cc_library(
    name = "video_reader_lib",
    srcs = ["video_reader.cpp"],
    copts = [
        "-I/usr/local/opt/opencv3/include"
    ],
    visibility = ["//main:__pkg__"],
)
{% endhighlight %}

Turning on the `-c` flag is just an addition to the `copts` array:

{% highlight bazel %}
cc_library(
    name = "video_reader_lib",
    srcs = ["video_reader.cpp"],
    copts = [
        "-I/usr/local/opt/opencv3/include",
        "-c"
    ],
    visibility = ["//main:__pkg__"],
)
{% endhighlight %}

Now we would like to have our D code compiled. D is not supported out-of-the-box by Bazel. So we will need to
install a corresponding plugin to enable it. Let's add it to our `WORKSPACE` file:

{% highlight bazel %}
http_archive(
    name = "io_bazel_rules_d",
    url = "http://bazel-mirror.storage.googleapis.com/github.com/bazelbuild/rules_d/archive/0.0.1.tar.gz",
    sha256 = "6f83ecd38c94a8ff5a68593b9352d08c2bf618ea8f87917c367681625e2bc04e",
    strip_prefix = "rules_d-0.0.1",
)

load("@io_bazel_rules_d//d:d.bzl", "d_repositories")

d_repositories()
{% endhighlight %}

We compiled our D program and linked it with this command:

{% highlight bash %}
dmd video_reader.o main.d -L-lstdc++ -L-lopencv_videoio -L-lopencv_core -L-lopencv_imgproc -L-L/usr/local/opt/opencv3/lib
{% endhighlight %}

Its parameters are:

* sources: `video_reader.o` and `main.d`
* linker arguments: `-L-lstdc++ -L-lopencv_videoio -L-lopencv_core -L-lopencv_imgproc -L-L/usr/local/opt/opencv3/lib`

Only two of them, huh? Alright then... Let's define the build target in the `main/BUILD` file:

{% highlight bazel %}
load("@io_bazel_rules_d//d/d", "d_source_library")

d_binary(
    name = "main",
    srcs = ["main.d"],
    deps = ["//lib:video_reader_lib"],
    linkopts = [
        "-lstdc++",
        "-lopencv_videoio",
        "-lopencv_core",
        "-lopencv_imgproc",
        "-L/usr/local/opt/opencv3/lib"
    ],
)
{% endhighlight %}
-->

## The end?

These simple errors were hard to find and fix intuitively. But what does it have in common
with beautiful code? Stay tuned for more`!
