---
title: Iterating a vector in C++
layout: post
---

Such a simple topic - iterating over a vector, is it even worth discussing?

Interestingly enough, there **is** a difference in **how exactly** you iterate - be it using iterators, `for(:)` sugar or plain old `for(i=0; i<vec.size(); ++i)`.

Let us see what output does a compiler produce in each of these cases.

`sample1` (simple `for` loop):

```cpp
std::vector<int> data{ 5, 3, 2, 1, 4 };

for (auto i = 0; i < data.size(); ++i) {
  moo(data[i]);
}
```

```asm
        mov     DWORD PTR [rbp-20], 0
        jmp     .L3
.L4:
        mov     eax, DWORD PTR [rbp-20]
        movsx   rdx, eax
        lea     rax, [rbp-64]
        mov     rsi, rdx
        mov     rdi, rax
        call    std::vector<int, std::allocator<int> >::operator[](unsigned long)
        mov     eax, DWORD PTR [rax]
        mov     edi, eax
        call    moo(int)
        add     DWORD PTR [rbp-20], 1
.L3:
        mov     eax, DWORD PTR [rbp-20]
        movsx   rbx, eax
        lea     rax, [rbp-64]
        mov     rdi, rax
        call    std::vector<int, std::allocator<int> >::size() const
        cmp     rbx, rax
        setb    al
        test    al, al
        jne     .L4
```

but

`sample2` (reversed `for` loop):

```cpp
for (auto i = data.size() - 1; i >= 0; --i) {
  moo(data[i]);
}
```

```asm
        lea     rax, [rbp-64]
        mov     rdi, rax
        call    std::vector<int, std::allocator<int> >::size() const
        sub     rax, 1
        mov     QWORD PTR [rbp-24], rax
.L3:
        mov     rdx, QWORD PTR [rbp-24]
        lea     rax, [rbp-64]
        mov     rsi, rdx
        mov     rdi, rax
        call    std::vector<int, std::allocator<int> >::operator[](unsigned long)
        mov     eax, DWORD PTR [rax]
        mov     edi, eax
        call    moo(int)
        sub     QWORD PTR [rbp-24], 1
        jmp     .L3
```

also

`sample3` (foreach):

```cpp
for (auto i : data) {
  moo(i)
}
```

```asm
        lea     rax, [rbp-80]
        mov     QWORD PTR [rbp-24], rax
        mov     rax, QWORD PTR [rbp-24]
        mov     rdi, rax
        call    std::vector<int, std::allocator<int> >::begin()
        mov     QWORD PTR [rbp-88], rax
        mov     rax, QWORD PTR [rbp-24]
        mov     rdi, rax
        call    std::vector<int, std::allocator<int> >::end()
        mov     QWORD PTR [rbp-96], rax
        jmp     .L3
.L4:
        lea     rax, [rbp-88]
        mov     rdi, rax
        call    __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator*() const
        mov     eax, DWORD PTR [rax]
        mov     DWORD PTR [rbp-28], eax
        mov     eax, DWORD PTR [rbp-28]
        mov     edi, eax
        call    moo(int)
        lea     rax, [rbp-88]
        mov     rdi, rax
        call    __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator++()
.L3:
        lea     rdx, [rbp-96]
        lea     rax, [rbp-88]
        mov     rsi, rdx
        mov     rdi, rax
        call    bool __gnu_cxx::operator!=<int*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&, __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&)
        test    al, al
        jne     .L4
```

but with `-O1`

`sample1` (simple `for` loop):

```asm
        movabs  rax, 12884901893
        movabs  rdx, 4294967298
        mov     QWORD PTR [r12], rax
        mov     QWORD PTR [r12+8], rdx
        mov     DWORD PTR [r12+16], 4
        mov     QWORD PTR [rsp+8], rbp
        mov     rbx, r12
        jmp     .L10
.L20:
        add     rbx, 4
        cmp     rbp, rbx
        je      .L19
.L10:
        mov     edi, DWORD PTR [rbx]
        call    moo(int)
        jmp     .L20
```

`sample2` (reversed `for` loop):

```asm
        movabs  rax, 12884901893
        movabs  rdx, 4294967298
        mov     QWORD PTR [rbp+0], rax
        mov     QWORD PTR [rbp+8], rdx
        mov     DWORD PTR [rbp+16], 4
        lea     rbx, [rbp+16]
        jmp     .L4
.L8:
        sub     rbx, 4
.L4:
        mov     edi, DWORD PTR [rbx]
        call    moo(int)
        jmp     .L8
```

`sample3` (foreach):

```asm
        mov     r12, rax
        lea     rbp, [rax+20]
        mov     QWORD PTR [rsp+16], rbp
        movabs  rax, 12884901893
        movabs  rdx, 4294967298
        mov     QWORD PTR [r12], rax
        mov     QWORD PTR [r12+8], rdx
        mov     DWORD PTR [r12+16], 4
        mov     QWORD PTR [rsp+8], rbp
        mov     rbx, r12
        jmp     .L10
.L20:
        add     rbx, 4
        cmp     rbx, rbp
        je      .L19
.L10:
        mov     edi, DWORD PTR [rbx]
        call    moo(int)
        jmp     .L20
```

Interesting how iterating forwards adds an extra boundary check and a jump (`cmd rbx, rbp` and `je .L19` in samples #1 and #3),
whereas iterating backwards does not.

But this find actually must come with one pretty big caveat: the cache lines. The number of assembly instructions is a pretty poor measure of performance - after all, CPU instructions are ridiculously fast, compared to any form of IO - specifically memory access.

The code is also available on Github: [https://github.com/shybovycha/iterate-over-vector-in-cpp/]

The benchmarking results seem to prove the assumption: bacwards iteration seems to be faster:

| Test               | Min     | Max     | 50%     | 90%     | 95%     |
|--------------------|---------|---------|---------|---------|---------|
| sample1 (`++i`)    | 1325447 | 3762733 | 1339309 | 1416805 | 1429724 |
| sample2 (`--i`)    | 1290571 | 2596630 | 1308953 | 1334955 | 1342008 |
| sample3 (`for(:)`) | 1438847 | 3034474 | 1460698 | 1506001 | 1508657 |

Out of curiosity, I also benchmarked using the postfix operator instead of prefix: `i++` (postfix) instead of `++i` (prefix), which shows prefix operator is slightly faster:

| Test                    | Min     | Max     | 50%     | 90%     | 95%     | delta |
|-------------------------|---------|---------|---------|---------|---------|-------|
| sample1 prefix (`++i`)  | 1325447 | 3762733 | 1339309 | 1416805 | 1429724 | +11%  |
| sample1 postfix (`i++`) | 1292195 | 4155596 | 1308721 | 1370542 | 1382136 | +7%   |
| sample2 prefix (`--i`)  | 1290571 | 2596630 | 1308953 | 1334955 | 1342008 | +5%   |
| sample2 postfix (`i--`) | 1256929 | 2598426 | 1267088 | 1277779 | 1282787 | 0%    |

Interestingly enough, there is *some* difference:

1. backward iteration with postfix decrement `for (auto i=vec.size()-1; i>0; i--)` is the fastest
2. backward iteration with prefix decrement `--i` is
  - `5%` slower than `i--`
3. forward iteration with postfix increment `i++` is
  - `3%` slower than `--i`
  - `7%` slower than `i--`
4. forward iteration with prefix increment `++i` is the slowest:
  - `11%` slower than `i--`
  - `7%` slower than `--i`
  - `4%` slower than `i++`

The above tests were ran `10000` times on a heap-allocated (`std::vector`) `10000` `int` elements.

## Caching impact

One other assumption is that iterating over a vector backwards affects the memory caching in a pretty poor manner.
This is a rather complex scenario to test, since there are two potential scenarios on how this could happen:

1. swap memory usage - happens when the dataset is so big it does not fit in the available RAM and OS has to switch to using disk instead of RAM; this is the worst-case scenario
2. data locality / CPU cache usage - CPUs usually try to predict memory access patterns and pre-load more data than actually required by a given operation; when the data block is accessed at random points (indices), CPU fails to pre-load the correct slices of memory

Since this blog is about iterating over a vector, it would be seem to be easier to generate absurdely large chunks of data and try to iterate over them. But in reality it would only happen with absurdely large files - my machine, for instance, sports 32GB of RAM, so it would be quite a task to generate *multiple* 32GB files.

Simulating random memory access could actually be easier - we just need to replace integers with structures of variable size and use `std::list` (linked list) instead of `std::vector` (single block of memory):

```cpp
struct MyStruct {
  bool f0;
  long f1;
  float f2;
  int* f3;
  long long f4[10];
};

std::list<MyStruct> a;
```

and generate a large enough number of these objects:

```cpp
#include <random>

std::random_device rd;
std::mt19937_64 gen(rd());

std::uniform_int_distribution<int> bool_dist(0, 1);
std::uniform_int_distribution<long> long_dist(std::numeric_limits<long>::min(), std::numeric_limits<long>::max());
std::uniform_real_distribution<float> float_dist(0.0f, 1000.0f);
std::uniform_int_distribution<int> int_dist(std::numeric_limits<int>::min(), std::numeric_limits<int>::max());
std::uniform_int_distribution<long long> long_long_dist(std::numeric_limits<long long>::min(), std::numeric_limits<long long>::max());

for (auto i = 0; i < 10000; ++i) {
  auto e = MyStruct {
    .f0 = static_cast<bool>(bool_dist(gen)),
    .f1 = lond_dist(gen),
    .f2 = float_dist(gen),
    .f3 = new int(int_dist(gen) & 7),
    .f4 = { 0 }
  };

  for (auto t = 0; t < int_dist(gen) % 10; ++t) {
    e.f4[t] = int_dist(gen);
  }

  a.push_back(e);
}
```

Just to prevent memory leaks, cleanup the test data at the end:

```cpp
for (auto& item : a) {
  delete item.f3;
}

a.clear();
```

These benchmarks take significantly longer time to run and produce big numbers:

| Test               | Min     | Max      | 50%     | 90%     | 95%     | delta |
|--------------------|---------|----------|---------|---------|---------|-------|
| sample1 (`++i`)    | 6594160 | 12230338 | 6672436 | 6728386 | 6758193 | +1%   |
| sample2 (`--i`)    | 6479624 | 11910333 | 6550249 | 6603793 | 6632227 | 0%    |
| sample3 (`for(:)`) | 6688370 | 15104530 | 6770925 | 6813835 | 6843606 | +3%   |

## Conclusion

Even then the locality impact (which is negligible for one-by-one iteration) is within 2..3% difference, with backwards iteration being the fastest, followed by forward iteration and foreach being the slowest of the bunch.
Bear in mind: these numbers are in *nanoseconds*, meaning the worst case scenario is `10000` (ten thousand) elements of variable size being iterated in `6.8 ms` and the fastest being `6.6 ms` - that is **milliseconds**.
For comparison, rendering a frame at `120 FPS` rate would take about `8.3 ms`. And if you were to process some requests (in a client-server system), you would be able to handle `151` request per second while iterating over this list.

So realistically, the way you iterate a **vector** would only really matter if you are working on a really high performance system.
