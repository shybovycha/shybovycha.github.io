---
title: Iterating a vector in C++
layout: post
tags: [cpp, c++, performance, optimization, vectors, assembly, benchmarking, algorithms, memory, cache]
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

| Test               | Min     | Max     | 50%     | 90%     | 95%     | delta |
|--------------------|---------|---------|---------|---------|---------|-------|
| sample1 (`++i`)    | 1325447 | 3762733 | 1339309 | 1416805 | 1429724 | +7%   |
| sample2 (`--i`)    | 1290571 | 2596630 | 1308953 | 1334955 | 1342008 | 0%    |
| sample3 (`for(:)`) | 1438847 | 3034474 | 1460698 | 1506001 | 1508657 | +11%  |

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

## Standard library iterators

Out of sheer curiosity, I decided to test how using an iterator would affect the results (`sample4`):

```cpp
for (auto it = a.begin(); it != a.end(); ++it) {
  moo(*it);
}
```

With both prefix and suffix (postfix) variations:

```cpp
for (auto it = a.begin(); it != a.end(); it++) {
  moo(*it);
}
```

And the reverse iterator (`sample5`):

```cpp
for (auto it = a.end(); it != a.begin(); it--) {
  moo(*it);
}
```

| Test                      | Min     | Max     | 50%     | 90%     | 95%     | delta |
|---------------------------|---------|---------|---------|---------|---------|-------|
| sample1 prefix (`++i`)    | 1325447 | 3762733 | 1339309 | 1416805 | 1429724 | +11%  |
| sample1 postfix (`i++`)   | 1292195 | 4155596 | 1308721 | 1370542 | 1382136 | +7%   |
| sample2 prefix (`--i`)    | 1290571 | 2596630 | 1308953 | 1334955 | 1342008 | +4%   |
| sample2 postfix (`i--`)   | 1256929 | 2598426 | 1267088 | 1277779 | 1282787 | 0%    |
| sample3 (`for(:)`)        | 1438847 | 3034474 | 1460698 | 1506001 | 1508657 | +17%  |
| sample4 prefix (`++it`)   | 1470583 | 2834386 | 1495310 | 1561023 | 1564850 | +22%  |
| sample4 postfix (`it++`)  | 1461634 | 2695552 | 1535905 | 1547865 | 1556815 | +21%  |
| sample5 prefix (`--it`)   | 1462156 | 2802714 | 1470567 | 1478957 | 1512621 | +18%  |
| sample5 postfix (`it--`)  | 1447920 | 2831859 | 1471334 | 1479670 | 1490088 | +16%  |

The results are a bit surprising, looking at the assembly code generated by G++:

```asm
.L25:
        cmp     rbx, r13
        je      .L10
        mov     rbp, r13
        jmp     .L11
.L27:
        add     rbp, 4
        cmp     rbx, rbp
        je      .L26
.L11:
        mov     edi, DWORD PTR [rbp+0]
        call    moo(int)
        jmp     .L27
.L26:
        test    r13, r13
        je      .L18
        mov     rsi, r12
        sub     rsi, r13
.L15:
        mov     rdi, r13
        call    operator delete(void*, unsigned long)
        ; ... cleanup & exit ...
        ret
```

Clang 17.0.1 is a slightly different story:

```asm
.LBB0_1:
        cmp     r14, rax
        jne     .LBB0_2
        sub     r14, r15
        movabs  rax, 9223372036854775804
        cmp     r14, rax
        mov     qword ptr [rsp], r15
        je      .LBB0_11
        mov     rbp, r14
        sar     rbp, 2
        cmp     rbp, 1
        mov     rax, rbp
        adc     rax, 0
        lea     rdx, [rax + rbp]
        mov     rcx, r12
        cmp     rdx, r12
        jbe     .LBB0_14
        add     rax, rbp
        jae     .LBB0_16
.LBB0_17:
        test    r12, r12
        je      .LBB0_18
.LBB0_19:
        lea     rdi, [4*r12]
        call    operator new(unsigned long)@PLT
        mov     r15, rax
        jmp     .LBB0_21
.LBB0_14:
        mov     rcx, rdx
        add     rax, rbp
        jb      .LBB0_17
.LBB0_16:
        mov     r12, rcx
        test    r12, r12
        jne     .LBB0_19
.LBB0_18:
        xor     r15d, r15d
.LBB0_21:
        mov     dword ptr [r15 + 4*rbp], r13d
        test    r14, r14
        mov     rbx, qword ptr [rsp]
        jle     .LBB0_23
        mov     rdi, r15
        mov     rsi, rbx
        mov     rdx, r14
        call    memmove@PLT
.LBB0_23:
        test    rbx, rbx
        je      .LBB0_25
        mov     rdi, rbx
        call    operator delete(void*)@PLT
.LBB0_25:
        lea     rbp, [r15 + 4*rbp]
        lea     rax, [r15 + 4*r12]
        movabs  r12, 2305843009213693951
        jmp     .LBB0_26
.LBB0_3:
        cmp     r15, r14
        je      .LBB0_7
        lea     r14, [r15 - 4]
.LBB0_5:
        mov     edi, dword ptr [r14 + 4]
        call    moo(int)@PLT
        add     r14, 4
        cmp     r14, rbp
        jne     .LBB0_5
.LBB0_7:
        test    r15, r15
        je      .LBB0_9
        mov     rdi, r15
        call    operator delete(void*)@PLT
.LBB0_9:
        xor     eax, eax
        add     rsp, 8
        pop     rbx
        pop     r12
        pop     r13
        pop     r14
        pop     r15
        pop     rbp
        ret
.LBB0_11:
        lea     rdi, [rip + .L.str.1]
        call    std::__throw_length_error(char const*)@PLT
        jmp     .LBB0_30
        jmp     .LBB0_30
        mov     qword ptr [rsp], r15
.LBB0_30:
        mov     r14, rax
        cmp     qword ptr [rsp], 0
        je      .LBB0_32
        mov     rdi, qword ptr [rsp]
        call    operator delete(void*)@PLT
.LBB0_32:
        mov     rdi, r14
        call    _Unwind_Resume@PLT
```

## Caching impact

One other assumption is that iterating over a vector backwards affects the memory caching in a pretty poor manner.
This is a rather complex scenario to test, since there are two potential scenarios on how this could happen:

1. swap memory usage - happens when the dataset is so big it does not fit in the available RAM and OS has to switch to using disk instead of RAM; this is the worst-case scenario
2. data locality / CPU cache usage - CPUs usually try to predict memory access patterns and pre-load more data than actually required by a given operation; when the data block is accessed at random points (indices), CPU fails to pre-load the correct slices of memory

Since this blog is about iterating over a vector, it would be seem to be easier to generate absurdely large chunks of data and try to iterate over them. But in reality it would only happen with absurdely large files - my machine, for instance, sports 32GB of RAM, so it would be quite a task to generate *multiple* 32GB files.

Simulating random memory access will break the purpose of iterating over the elements one by one - in these cases we either use iterators to access a linked list (each element is in random part of RAM) or access elements by index. And there is no way to iterate over them by incrementing an index.
Alternatively, we have to dereference indexes from another block of memory. Either way it has nothing to do comparing `for` loops.

Simulating accessing multiple cache lines could actually be easier - we just need to replace integers with structures of variable size and use `std::list` (linked list) instead of `std::vector` (single block of memory):

```cpp
struct MyStruct {
  bool f0; // 1 byte
  float f1[100]; // sizeof(float) * 100 = 4 * 100 = 400 bytes
  double f2[53]; // sizeof(double) * 53 = 8 * 53 = 424 bytes
  char f3[64]; // sizeof(char) * 64 = 1 * 64 = 64 bytes
}; // total size = 889 bytes, likely to span across multiple cache lines

std::list<MyStruct> a;
```

and generate a large enough number of these objects:

```cpp
#include <random>

std::random_device rd;
std::mt19937_64 gen(rd());

std::uniform_int_distribution<int> bool_dist(0, 1);
std::uniform_real_distribution<float> float_dist(0.0f, 1000.0f);
std::uniform_int_distribution<int> int_dist(std::numeric_limits<int>::min(), std::numeric_limits<int>::max());

for (auto i = 0; i < 10000; ++i) {
  MyStruct e;

  e.f0 = static_cast<bool>(bool_dist(gen));

  for (auto t = 0; t < 100; ++t)
    e.f1[t] = float_dist(gen);

  for (auto t = 0; t < 53; ++t)
    e.f2[t] = static_cast<double>(float_dist(gen));

  for (auto t = 0; t < 64; ++t)
    e.f3[t] = static_cast<char>(int_dist(gen) % 255);

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
| sample1 (`++i`)    | 7882057 | 15385569 | 7948012 | 8004355 | 8031673 | 0%    |
| sample2 (`--i`)    | 7915870 | 16297070 | 8971293 | 9109493 | 9151567 | +13%  |
| sample3 (`for(:)`) | 8094894 | 16496526 | 8159004 | 8229138 | 8259364 | +3%   |

Compare this to a structure which actually fits in one cache line.
Cache line size is apparently `64 B` - that is **sixty-four bytes** - on most CPUs ([AMD Zen](https://www.7-cpu.com/cpu/Zen.html), [Intel Ice Lake](https://www.7-cpu.com/cpu/Ice_Lake.html)).
My Apple M1 laptop has twice as much, `128 B`, as shown by running `sysctl -a | grep 'hw.cachelinesize'`.

If the structure was reorganized to fit in that limit, like so:

```cpp
struct MyStruct {
  bool f0; // 1 byte
  float* f1; // sizeof(float*) = 8 bytes
  double* f2; // sizeof(double*) = 8 bytes
  char* f3; // sizeof(char*) = 8 bytes
}; // total size = 25 bytes, likely to fir in a single cache line
```

The timings are considerably lower for the backwards iteration:

| Test               | Min     | Max      | 50%     | 90%     | 95%     | delta |
|--------------------|---------|----------|---------|---------|---------|-------|
| sample1 (`++i`)    | 7575860 | 20005185 | 7640651 | 7700057 | 7725933 | 0%    |
| sample2 (`--i`)    | 7466816 | 18068148 | 7657415 | 8109531 | 8208603 | +6%   |
| sample3 (`for(:)`) | 7832724 | 14264212 | 7882930 | 7932295 | 7960718 | +3%   |

This highlights how predictive cache loading and data element size that fits into a single CPU cache line actually impacts RAM reads - by a significant margin.

## Conclusion

In simplest case (vector of numbers), the worst case scenario is within 11% difference, with backwards iteration being the fastest, followed by forward iteration and foreach being the slowest of the bunch.
Prefix decrement in the backwards iteration being the fastest.
Bear in mind: these numbers are in *nanoseconds*, meaning the worst case scenario is `10000` (ten thousand) elements of variable size being iterated in `1.5 ms` and the fastest being `1.3 ms` - that is **milliseconds**.
For comparison, rendering a frame at `120 FPS` rate would take about `8.3 ms`. And if you were to process some requests (in a client-server system), you would be able to handle `699` requests per second while iterating over this list.
So realistically, the way you iterate a vector of integers, any of these techniques would only really matter if you are working on a really high performance system.

That being said, this is all true while the vector elements fit in one CPU cache line or cache block, that is L1 and L2 caches.

So if the data element does not fit in a block of `64 B` (or `128 B` on some CPUs), the performance impact of backwards iteration is going to be more significant.
In this case we are talking about `8 ms` vs `9.1 ms`. In terms of FPS, the difference would be `125 FPS` vs `109 FPS`. Or about `15` requests per second more.
