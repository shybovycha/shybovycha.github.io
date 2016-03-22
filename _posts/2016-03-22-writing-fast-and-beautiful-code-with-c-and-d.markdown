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

So I decided to write my code in C++. Yeah, tough solution... And in some
time I saw why it was not the best one: my architecture was too bold *(435 LOC)* and it didn't even
contained four method implementations!

Then I sit back and thought: *"Couldn't I use C++ for video/image reading only? And write the rest of the code in a more OOP-friendly language?"*. And that's when I started looking for a language with nice syntax and OOP features *(like interfaces, short array syntax, tuples/maps, etc.)* and found D.

I've been looking at D since loooong time ago, but actually never tried it for writing anything
more complex than a *"Hello, World!"*. *"That's my star time!"* - is what I thought.

My idea was:

1. create small C++ library for video IO
2. create D program, using that C++ library to read and process video

D offeres nice C++ interop, except it requires you to define classes/functions signatures you
are importing from C++. That was not a big deal for me since I had no huge classes written yet.

<!--more-->

## First steps

Standard example from D website works like a charm:

*C++ code:*

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

*D code:*

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

Compiled this one with `c++ -c cpp_lib.cpp -o cpp_lib.o && dmd cpp_lib.o main.d` and ran - *it works*.

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

Instead, there are errors from D compiler:

{% highlight bash %}
Error: Internal Compiler Error: unsupported type string

[2]    97638 segmentation fault  dmd video_reader.o main.d -L-lstdc++ -L-lopencv_videoio -L-lopencv_core
{% endhighlight %}

WTF?! Internal compiler error?! And what's wrong with strings?

## Fixing everything

Alright, maybe it's caused by us,
using `string` type in `VideoReader::readFile(string filename)` method signature. Let's fix it:

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

Wow! That's actually not that interesting, but rather understandable: C++ does not generate
any bytecode, which is not used by C++ program. So we should explicitly tell C++ compiler,
which methods we do want to have code generated:

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

What? Why? We've declared that method explicitly! Or maybe not?.. Let's look at our object file:

{% highlight bash %}
$ nm video_reader.o
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

Yeah! Our code compiled and linked successfully! Let's run it:

{% highlight bash %}
[2]    99945 segmentation fault  ./video_reader
{% endhighlight %}

What? Wrong again?! Okay, that's really strange. And no debugging information.
Adding some debugging `writeln()`'s shows it's caused by the `getFrameCount()` method call:

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

Googling and trying semi-random solutions showed that signatures for constant methods
in C++ are a bit different in D:

{% highlight d %}
extern(C++)
{
    class Video
    {
        @disable this();
        final uint getFrameCount() const;
    }
}
{% endhighlight %}

And only now the code compiles successfully!

{% highlight bash %}
Video length: 318
{% endhighlight %}

## The end?

These simple errors were hard to bypass intuitively. But may there be even more?..
If so - this post will become longer =)
