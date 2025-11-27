---
layout: post
title: 'Functional programming in a real world'
date: '2018-01-13T18:01:00+01:00'
tags: [cpp, fp, programming, c++, programming-paradigms, functional-programming, data, database]
---

There has been quite a lot of hype around functional programming kicking off in past few years.
A lot of buzz on how it simplifies developers' lives, making code more straightforward, clean and testable.
But let's dig a bit deeper into what actually lies behind the facade of a neat, testable and mistake-resistant code.

Just a recap: what functional programming is?

When people talk about functional paradigm, they refer to few principles:

1. functions are the first-class citizens in a programming language, so any program is just a function
2. immutability (context isolation), meaning a program / function does not change the state of anything outside of its scope / context

On top of this, people often talk about _pure functional programs_ or _pure functions_, which, according to Wikipedia, described as:

3. programs are referentially transparent, meaning any expression in a program can be replaced with its value and the function will behave exactly the same
4. functions don't have side effects, meaning function' state is only internal, functions do not modify the state of other objects

Let us start with a concrete example of a program.
I could have prepared a real-world one, with an in-memory index of some entities,
converting database query result to a HTTP response or creating a HTTP query string. But all those, despite being relatively simple,
are still a bit too big for what I'm about to show you. So let me go a bit unrealistic here and use the sum of an array of integer numbers as an example.

In C++, this might look something like this:

```c
int sum(std::vector<int> a) {
  int res = 0;

  for (int i : a) {
    res += i;
  }

  return res;
}
```

This program exhibits the concept of scope isolation - it does not change anything (any state) outside of its scope, aka it does not have side-effects.

Using the power of [Godbolt compiler exporer](https://godbolt.org/) we can see what it will be compiled to (using `x86-64 gcc 13.1` with no compiler options):

```asm
sum(std::vector<int, std::allocator<int> >):
        push    rbp
        mov     rbp, rsp
        sub     rsp, 64
        mov     QWORD PTR [rbp-56], rdi
        mov     DWORD PTR [rbp-4], 0
        mov     rax, QWORD PTR [rbp-56]
        mov     QWORD PTR [rbp-16], rax
        mov     rax, QWORD PTR [rbp-16]
        mov     rdi, rax
        call    std::vector<int, std::allocator<int> >::begin()
        mov     QWORD PTR [rbp-32], rax
        mov     rax, QWORD PTR [rbp-16]
        mov     rdi, rax
        call    std::vector<int, std::allocator<int> >::end()
        mov     QWORD PTR [rbp-40], rax
        jmp     .L2
.L3:
        lea     rax, [rbp-32]
        mov     rdi, rax
        call    __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator*() const
        mov     eax, DWORD PTR [rax]
        mov     DWORD PTR [rbp-20], eax
        mov     eax, DWORD PTR [rbp-20]
        add     DWORD PTR [rbp-4], eax
        lea     rax, [rbp-32]
        mov     rdi, rax
        call    __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator++()
.L2:
        lea     rdx, [rbp-40]
        lea     rax, [rbp-32]
        mov     rsi, rdx
        mov     rdi, rax
        call    bool __gnu_cxx::operator!=<int*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&, __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&)
        test    al, al
        jne     .L3
        mov     eax, DWORD PTR [rbp-4]
        leave
        ret
```

This is a rather primitive code.

Same code in a what is considered a pure functional programming language, Haskell, would look like this (with a trick that `sum` is in fact a standard library function, so we have to use a different name, `sum2` sounds good to me):

```hs
sum2 :: [Int] -> Int
sum2 (x:xs) = x + sum2 xs
sum2 [] = 0
```

And it is compiled into the following assembly:

```asm
.LrB_bytes:
        .string "Example"
.Lrz_bytes:
        .string "main"
.LrA_closure:
        .quad   ghczmprim_GHCziTypes_TrNameS_con_info
        .quad   .Lrz_bytes
.LrC_closure:
        .quad   ghczmprim_GHCziTypes_TrNameS_con_info
        .quad   .LrB_bytes
Example_$trModule_closure:
        .quad   ghczmprim_GHCziTypes_Module_con_info
        .quad   .LrA_closure+1
        .quad   .LrC_closure+1
        .quad   3
.LsCD_info:

        leaq -16(%rbp),%rax
        cmpq %r15,%rax
        jb .LcCZ

        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movq %rax,%r14
        addq $-16,%rbp

        jmp Example_sum2_info
.LcCZ:

        jmp *-16(%r13)
Example_sum2_info:

        leaq -24(%rbp),%rax
        cmpq %r15,%rax
        jb .LcD6

        movq $.LcCN_info,-8(%rbp)
        movq %r14,%rbx
        addq $-8,%rbp

        testb $7,%bl
        jne .LcCN

        jmp *(%rbx)
        .quad   0
        .long   30
        .long   Example_sum2_closure-(.LcCN_info)+0
.LcCN_info:
.LcCN:

        movq %rbx,%rax
        andl $7,%eax
        cmpq $1,%rax
        jne .LcD3

        leaq stg_INTLIKE_closure+257(%rip),%rbx
        addq $8,%rbp

        jmp *(%rbp)
.LcD3:

        addq $24,%r12
        cmpq 856(%r13),%r12
        ja .LcDb

        movq 6(%rbx),%rax
        movq 14(%rbx),%rbx
        movq $.LsCD_info,-16(%r12)
        movq %rbx,(%r12)
        leaq -16(%r12),%rbx
        leaq base_GHCziNum_zdfNumInt_closure(%rip),%r14
        movq $stg_ap_pp_info,-16(%rbp)
        movq %rax,-8(%rbp)
        movq %rbx,(%rbp)
        addq $-16,%rbp

        jmp base_GHC.Num_+_info
.LcD6:

        leaq Example_sum2_closure(%rip),%rbx
        jmp *-8(%r13)
.LcDb:

        movq $24,904(%r13)
        jmp stg_gc_unpt_r1
Example_sum2_closure:
        .quad   Example_sum2_info
        .quad   0
```

Trick is: this is a bit of an unfair comparison -
C++ code just mutates the variable and uses iterators whereas Haskell code passes sub-array into the recursive call of a function.

Well, we can make them _very_ similar now that we have C++20, using `std::span`:

```cpp
int sum3(std::span<int> a, int res = 0) {
  if (a.empty()) {
    return res;
  }

  return sum3(a.subspan(1), res + a.front());
}
```

This yields a very similar assembly code too:

```asm
sum3(std::span<int, 18446744073709551615ul>, int):
        push    rbp
        mov     rbp, rsp
        push    rbx
        sub     rsp, 40
        mov     rax, rdi
        mov     rcx, rax
        mov     rbx, rdx
        mov     rbx, rsi
        mov     QWORD PTR [rbp-32], rcx
        mov     QWORD PTR [rbp-24], rbx
        mov     DWORD PTR [rbp-36], edx
        lea     rax, [rbp-32]
        mov     rdi, rax
        call    std::span<int, 18446744073709551615ul>::empty() const
        test    al, al
        je      .L7
        mov     eax, DWORD PTR [rbp-36]
        jmp     .L8
.L7:
        lea     rax, [rbp-32]
        mov     rdi, rax
        call    std::span<int, 18446744073709551615ul>::front() const
        mov     edx, DWORD PTR [rax]
        mov     eax, DWORD PTR [rbp-36]
        lea     ebx, [rdx+rax]
        lea     rax, [rbp-32]
        mov     rdx, -1
        mov     esi, 1
        mov     rdi, rax
        call    std::span<int, 18446744073709551615ul>::subspan(unsigned long, unsigned long) const
        mov     rsi, rax
        mov     rdi, rdx
        mov     rcx, rsi
        mov     rax, rdx
        mov     edx, ebx
        mov     rdi, rcx
        mov     rsi, rax
        call    sum3(std::span<int, 18446744073709551615ul>, int)
        nop
.L8:
        mov     rbx, QWORD PTR [rbp-8]
        leave
        ret
```

---

Before talking high-level, let me briefly remind you about how programs are executed on a processor level.

## Crude reminder of how computers work

An average computer has few levels of memory on board (from fastest to slower):

* CPU registers - a rather limited number of **extremely** fast but very small variables
* L1 cache - a larger chunk of memory, which is still **very** fast
* L2 cache - a bit bigger chunk of a bit slower memory (still fast, though)
* L3 cache - the last big chunk of memory available directly on CPU
* RAM - a comparatively fast set of memory, but rather large on modern machines (16 GB being considered the minimum viable for playing games nowadays)
* permanent storage (HDD / SSD) - an insanely large (compared to what is available directly on CPU) chunk of rather slow memory

If you consider databases, especially located somewhere on the network, potentially on another continent, they are the slowest to work with.

For example, my current desktop PC has the following configuration (given Ryzen 7 7800X3D CPU):

* registers: 64-bit-wide
* L1 cache: 512 KB
* L2 cache: 8 MB
* L3 cache: 96 MB
* RAM: 32 GB
* SSD: 2 TB

A detailed list of how fast each memory is, [I think every programmer should know](https://gist.github.com/hellerbarde/2843375/), June 2023 version:

```
L1 cache reference ......................... 0.5 ns
L2 cache reference ........................... 7 ns
Main memory reference ...................... 100 ns
Send 2 KB over 1 Gbps network ........... 20,000 ns  =  20 µs
SSD random read ........................ 150,000 ns  = 150 µs
Read 1 MB sequentially from memory ..... 250,000 ns  = 250 µs
Round trip within same datacenter ...... 500,000 ns  = 0.5 ms
Read 1 MB sequentially from SSD* ..... 1,000,000 ns  =   1 ms
Send packet CA->Netherlands->CA .... 150,000,000 ns  = 150 ms
```

An average program (application) lifecycle is about the following (considering the contemporary computer architecture):

* CPU (via the operating system abstractions) reads the program code (few megabytes, with all the libraries and actual program code) stored on SSD and stores it in RAM
* (due to the parallel and off-order instruction execution) CPU eventually reads _a portion_ of program code and stores them in different caches (L1/L2/L3), prior to executing them
* when CPU executes a portion of a program, it gets an _instruction_ and stores it (together with its operands) in CPU registers
* some program instructions would load data to RAM, CPU will then copy it from RAM to CPU registers (in order to perform operations on that data)
* some programs might perform network calls to load data into RAM and eventually get the data from RAM to CPU registers (to perform operations on that data)

But why am I even mentioning these latencies and CPU registers? Well, keep on reading - we'll use these concepts later on.

## What is functional programming?

Consider the following program in C:

```c
int square(int n) {
  return n * n;
}
```

Does this program exhibit functional approach?

According to Wikipedia, functional paradigm is described by two main principles:

1. operating on functions (functions as first-class citizens in a language)
2. immutability (context isolation; meaning a program / function does not change the state of anything outside of its scope / context)

## The big lie of functional programming

The example above only shows the context isolation (immutability) aspect of functional program.
It does not really demonstrate or use functions as first-class citizens - `*` is an _operator_ in C.

Now consider a tad more complex program:

```cpp
int sum(std::vector<int> a) {
  int res = 0;

  for (int i : a) {
    res += i;
  }

  return res;
}
```

We will refer to this example a couple of times later.

It compiles to the following assembly code (with `-std=c++1z -O1` compiler flags):

```asm
sum(int*, int): # @sum(int*, int)
  test esi, esi
  jle .LBB0_1
  mov ecx, esi
  xor eax, eax
.LBB0_4: # =>This Inner Loop Header: Depth=1
  add eax, dword ptr [rdi]
  add rdi, 4
  dec rcx
  jne .LBB0_4
  ret
.LBB0_1:
  xor eax, eax
  ret
```

Now, a C-style version:

```c
int sum(int *a, int n) {
  int res = 0;

  for (int i = 0; i < n; ++i) {
    res += a[i];
  }

  return res;
}
```

with `-O2` compiler flag yields the following assembly code:

```asm
sum(int*, int): # @sum(int*, int)
  test esi, esi
  jle .LBB0_1
  mov ecx, esi
  cmp esi, 7
  ja .LBB0_6
  xor r8d, r8d
  xor eax, eax
  jmp .LBB0_4
.LBB0_1:
  xor eax, eax
  ret
.LBB0_6:
  and esi, 7
  mov r8, rcx
  sub r8, rsi
  lea rax, [rdi + 16]
  pxor xmm0, xmm0
  mov rdx, r8
  pxor xmm1, xmm1
.LBB0_7: # =>This Inner Loop Header: Depth=1
  movdqu xmm2, xmmword ptr [rax - 16]
  paddd xmm0, xmm2
  movdqu xmm2, xmmword ptr [rax]
  paddd xmm1, xmm2
  add rax, 32
  add rdx, -8
  jne .LBB0_7
  paddd xmm1, xmm0
  pshufd xmm0, xmm1, 78 # xmm0 = xmm1[2,3,0,1]
  paddd xmm0, xmm1
  pshufd xmm1, xmm0, 229 # xmm1 = xmm0[1,1,2,3]
  paddd xmm1, xmm0
  movd eax, xmm1
  test esi, esi
  je .LBB0_9
.LBB0_4:
  lea rdx, [rdi + 4*r8]
  sub rcx, r8
.LBB0_5: # =>This Inner Loop Header: Depth=1
  add eax, dword ptr [rdx]
  add rdx, 4
  dec rcx
  jne .LBB0_5
.LBB0_9:
  ret
```

As you can see, written imperatively, they are compiled into relatively simple Assembly programs (given that `std::vector` has its own safety checks and is iterated over in a slightly different manner).

Now, what will happen if we were about to write the exactly same programs in a more functional style?

Consider this example in C:

```c
int reduce(int *a, int n, int init, int (*reducer)(int, int)) {
  int result = init;

  for (int i = 0; i < n; ++i) {
    result = reducer(a[i], result);
  }

  return result;
}
```

and in C++:

```cpp
#include <vector>
#include <functional>

int reduce(std::vector<int> a, int init, std::function<int(int, int)> &reducer) {
  int result = init;

  for (auto i : a) {
    result = reducer(i, result);
  }

  return result;
}
```

the first one produces this result:

```asm
reduce(int*, int, int, int (*)(int, int)): # @reduce(int*, int, int, int (*)(int, int))
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov dword ptr [rbp - 12], esi
  mov dword ptr [rbp - 16], edx
  mov qword ptr [rbp - 24], rcx
  mov edx, dword ptr [rbp - 16]
  mov dword ptr [rbp - 28], edx
  mov dword ptr [rbp - 32], 0
.LBB0_1: # =>This Inner Loop Header: Depth=1
  mov eax, dword ptr [rbp - 32]
  cmp eax, dword ptr [rbp - 12]
  jge .LBB0_4
  mov rax, qword ptr [rbp - 24]
  mov rcx, qword ptr [rbp - 8]
  movsxd rdx, dword ptr [rbp - 32]
  mov edi, dword ptr [rcx + 4*rdx]
  mov esi, dword ptr [rbp - 28]
  call rax
  mov dword ptr [rbp - 28], eax
  mov eax, dword ptr [rbp - 32]
  add eax, 1
  mov dword ptr [rbp - 32], eax
  jmp .LBB0_1
.LBB0_4:
  mov eax, dword ptr [rbp - 28]
  add rsp, 32
  pop rbp
  ret
```

Still pretty conscise assebmly code. But the C++ one produces quite a bit of noise due to the use of `std::function` and `std::vector`:

```asm
reduce(std::vector<int, std::allocator<int> >, int, std::function<int (int, int)>&): # @reduce(std::vector<int, std::allocator<int> >, int, std::function<int (int, int)>&)
  push rbp
  mov rbp, rsp
  sub rsp, 64
  mov dword ptr [rbp - 4], esi
  mov qword ptr [rbp - 16], rdx
  mov esi, dword ptr [rbp - 4]
  mov dword ptr [rbp - 20], esi
  mov qword ptr [rbp - 32], rdi
  mov rdi, qword ptr [rbp - 32]
  call std::vector<int, std::allocator<int> >::begin()
  mov qword ptr [rbp - 40], rax
  mov rdi, qword ptr [rbp - 32]
  call std::vector<int, std::allocator<int> >::end()
  mov qword ptr [rbp - 48], rax
.LBB0_1: # =>This Inner Loop Header: Depth=1
  lea rdi, [rbp - 40]
  lea rsi, [rbp - 48]
  call bool __gnu_cxx::operator!=<int*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&, __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&)
  test al, 1
  jne .LBB0_2
  jmp .LBB0_4
.LBB0_2: # in Loop: Header=BB0_1 Depth=1
  lea rdi, [rbp - 40]
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator*() const
  mov ecx, dword ptr [rax]
  mov dword ptr [rbp - 52], ecx
  mov rdi, qword ptr [rbp - 16]
  mov esi, dword ptr [rbp - 52]
  mov edx, dword ptr [rbp - 20]
  call std::function<int (int, int)>::operator()(int, int) const
  mov dword ptr [rbp - 20], eax
  lea rdi, [rbp - 40]
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator++()
  mov qword ptr [rbp - 64], rax # 8-byte Spill
  jmp .LBB0_1
.LBB0_4:
  mov eax, dword ptr [rbp - 20]
  add rsp, 64
  pop rbp
  ret
std::vector<int, std::allocator<int> >::begin(): # @std::vector<int, std::allocator<int> >::begin()
  push rbp
  mov rbp, rsp
  sub rsp, 32
  lea rax, [rbp - 8]
  mov qword ptr [rbp - 16], rdi
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 24], rdi # 8-byte Spill
  mov rdi, rax
  mov rsi, qword ptr [rbp - 24] # 8-byte Reload
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::__normal_iterator(int* const&)
  mov rax, qword ptr [rbp - 8]
  add rsp, 32
  pop rbp
  ret
std::vector<int, std::allocator<int> >::end(): # @std::vector<int, std::allocator<int> >::end()
  push rbp
  mov rbp, rsp
  sub rsp, 32
  lea rax, [rbp - 8]
  mov qword ptr [rbp - 16], rdi
  mov rdi, qword ptr [rbp - 16]
  add rdi, 8
  mov qword ptr [rbp - 24], rdi # 8-byte Spill
  mov rdi, rax
  mov rsi, qword ptr [rbp - 24] # 8-byte Reload
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::__normal_iterator(int* const&)
  mov rax, qword ptr [rbp - 8]
  add rsp, 32
  pop rbp
  ret
bool __gnu_cxx::operator!=<int*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&, __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&): # @bool __gnu_cxx::operator!=<int*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&, __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rdi, qword ptr [rbp - 8]
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::base() const
  mov rax, qword ptr [rax]
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 24], rax # 8-byte Spill
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::base() const
  mov rsi, qword ptr [rbp - 24] # 8-byte Reload
  cmp rsi, qword ptr [rax]
  setne cl
  and cl, 1
  movzx eax, cl
  add rsp, 32
  pop rbp
  ret
__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator*() const: # @__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator*() const
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, qword ptr [rdi]
  pop rbp
  ret
std::function<int (int, int)>::operator()(int, int) const: # @std::function<int (int, int)>::operator()(int, int) const
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov dword ptr [rbp - 12], esi
  mov dword ptr [rbp - 16], edx
  mov rdi, qword ptr [rbp - 8]
  mov rax, rdi
  mov qword ptr [rbp - 24], rdi # 8-byte Spill
  mov rdi, rax
  call std::_Function_base::_M_empty() const
  test al, 1
  jne .LBB5_1
  jmp .LBB5_2
.LBB5_1:
  call std::__throw_bad_function_call()
.LBB5_2:
  lea rdi, [rbp - 12]
  mov rax, qword ptr [rbp - 24] # 8-byte Reload
  mov rcx, qword ptr [rax + 24]
  mov qword ptr [rbp - 32], rcx # 8-byte Spill
  mov qword ptr [rbp - 40], rax # 8-byte Spill
  call int&& std::forward<int>(std::remove_reference<int>::type&)
  lea rdi, [rbp - 16]
  mov qword ptr [rbp - 48], rax # 8-byte Spill
  call int&& std::forward<int>(std::remove_reference<int>::type&)
  mov rdi, qword ptr [rbp - 40] # 8-byte Reload
  mov rsi, qword ptr [rbp - 48] # 8-byte Reload
  mov rdx, rax
  mov rax, qword ptr [rbp - 32] # 8-byte Reload
  call rax
  add rsp, 48
  pop rbp
  ret
__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator++(): # @__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator++()
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, qword ptr [rdi]
  add rax, 4
  mov qword ptr [rdi], rax
  mov rax, rdi
  pop rbp
  ret
__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::__normal_iterator(int* const&): # @__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::__normal_iterator(int* const&)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rsi, qword ptr [rbp - 8]
  mov rdi, qword ptr [rbp - 16]
  mov rdi, qword ptr [rdi]
  mov qword ptr [rsi], rdi
  pop rbp
  ret
__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::base() const: # @__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::base() const
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rax, qword ptr [rbp - 8]
  pop rbp
  ret
std::_Function_base::_M_empty() const: # @std::_Function_base::_M_empty() const
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  cmp qword ptr [rdi + 16], 0
  setne al
  xor al, -1
  and al, 1
  movzx eax, al
  pop rbp
  ret
int&& std::forward<int>(std::remove_reference<int>::type&): # @int&& std::forward<int>(std::remove_reference<int>::type&)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rax, qword ptr [rbp - 8]
  pop rbp
  ret
```

But in the essence it is still 41 LOC:

```asm
reduce(std::vector<int, std::allocator<int> >, int, std::function<int (int, int)>&): # @reduce(std::vector<int, std::allocator<int> >, int, std::function<int (int, int)>&)
  push rbp
  mov rbp, rsp
  sub rsp, 64
  mov dword ptr [rbp - 4], esi
  mov qword ptr [rbp - 16], rdx
  mov esi, dword ptr [rbp - 4]
  mov dword ptr [rbp - 20], esi
  mov qword ptr [rbp - 32], rdi
  mov rdi, qword ptr [rbp - 32]
  call std::vector<int, std::allocator<int> >::begin()
  mov qword ptr [rbp - 40], rax
  mov rdi, qword ptr [rbp - 32]
  call std::vector<int, std::allocator<int> >::end()
  mov qword ptr [rbp - 48], rax
.LBB0_1: # =>This Inner Loop Header: Depth=1
  lea rdi, [rbp - 40]
  lea rsi, [rbp - 48]
  call bool __gnu_cxx::operator!=<int*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&, __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&)
  test al, 1
  jne .LBB0_2
  jmp .LBB0_4
.LBB0_2: # in Loop: Header=BB0_1 Depth=1
  lea rdi, [rbp - 40]
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator*() const
  mov ecx, dword ptr [rax]
  mov dword ptr [rbp - 52], ecx
  mov rdi, qword ptr [rbp - 16]
  mov esi, dword ptr [rbp - 52]
  mov edx, dword ptr [rbp - 20]
  call std::function<int (int, int)>::operator()(int, int) const
  mov dword ptr [rbp - 20], eax
  lea rdi, [rbp - 40]
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator++()
  mov qword ptr [rbp - 64], rax # 8-byte Spill
  jmp .LBB0_1
.LBB0_4:
  mov eax, dword ptr [rbp - 20]
  add rsp, 64
  pop rbp
  ret
```

Let us try *calling* the `reduce` function. For C:

```c
int reduce(int *a, int n, int init, int (*reducer)(int, int)) {
  int result = init;

  for (int i = 0; i < n; ++i) {
    result = reducer(a[i], result);
  }

  return result;
}

int add(int acc, int elt) {
  return acc + elt;
}

int main() {
  int a[] = { 1, 2, 3 };
  reduce(a, 0, 3, &add);
}
```

and for C++:

```cpp
#include <vector>
#include <functional>

int reduce(std::vector<int> a, int init, std::function<int(int, int)> reducer) {
  int result = init;

  for (auto i : a) {
    result = reducer(i, result);
  }

  return result;
}

int main() {
  std::vector<int> a{ 1, 2, 3 };
  reduce(a, 0, [](int a, int b) -> int { return a + b; });
}
```

Which compile into these two Assemblies:

```asm
reduce: # @reduce
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov dword ptr [rbp - 12], esi
  mov dword ptr [rbp - 16], edx
  mov qword ptr [rbp - 24], rcx
  mov edx, dword ptr [rbp - 16]
  mov dword ptr [rbp - 28], edx
  mov dword ptr [rbp - 32], 0
.LBB0_1: # =>This Inner Loop Header: Depth=1
  mov eax, dword ptr [rbp - 32]
  cmp eax, dword ptr [rbp - 12]
  jge .LBB0_4
  mov rax, qword ptr [rbp - 24]
  mov rcx, qword ptr [rbp - 8]
  movsxd rdx, dword ptr [rbp - 32]
  mov edi, dword ptr [rcx + 4*rdx]
  mov esi, dword ptr [rbp - 28]
  call rax
  mov dword ptr [rbp - 28], eax
  mov eax, dword ptr [rbp - 32]
  add eax, 1
  mov dword ptr [rbp - 32], eax
  jmp .LBB0_1
.LBB0_4:
  mov eax, dword ptr [rbp - 28]
  add rsp, 32
  pop rbp
  ret
add: # @add
  push rbp
  mov rbp, rsp
  mov dword ptr [rbp - 4], edi
  mov dword ptr [rbp - 8], esi
  mov esi, dword ptr [rbp - 4]
  add esi, dword ptr [rbp - 8]
  mov eax, esi
  pop rbp
  ret
main: # @main
  push rbp
  mov rbp, rsp
  sub rsp, 16
  xor esi, esi
  mov edx, 3
  movabs rcx, add
  lea rdi, [rbp - 12]
  mov rax, qword ptr [.Lmain.a]
  mov qword ptr [rbp - 12], rax
  mov r8d, dword ptr [.Lmain.a+8]
  mov dword ptr [rbp - 4], r8d
  call reduce
  xor edx, edx
  mov dword ptr [rbp - 16], eax # 4-byte Spill
  mov eax, edx
  add rsp, 16
  pop rbp
  ret
.Lmain.a:
  .long 1 # 0x1
  .long 2 # 0x2
  .long 3 # 0x3
```

and

```asm
reduce(std::vector<int, std::allocator<int> >, int, std::function<int (int, int)>): # @reduce(std::vector<int, std::allocator<int> >, int, std::function<int (int, int)>)
  push rbp
  mov rbp, rsp
  sub rsp, 64
  mov dword ptr [rbp - 4], esi
  mov esi, dword ptr [rbp - 4]
  mov dword ptr [rbp - 8], esi
  mov qword ptr [rbp - 16], rdi
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 48], rdx # 8-byte Spill
  call std::vector<int, std::allocator<int> >::begin()
  mov qword ptr [rbp - 24], rax
  mov rdi, qword ptr [rbp - 16]
  call std::vector<int, std::allocator<int> >::end()
  mov qword ptr [rbp - 32], rax
.LBB0_1: # =>This Inner Loop Header: Depth=1
  lea rdi, [rbp - 24]
  lea rsi, [rbp - 32]
  call bool __gnu_cxx::operator!=<int*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&, __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&)
  test al, 1
  jne .LBB0_2
  jmp .LBB0_4
.LBB0_2: # in Loop: Header=BB0_1 Depth=1
  lea rdi, [rbp - 24]
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator*() const
  mov ecx, dword ptr [rax]
  mov dword ptr [rbp - 36], ecx
  mov esi, dword ptr [rbp - 36]
  mov edx, dword ptr [rbp - 8]
  mov rdi, qword ptr [rbp - 48] # 8-byte Reload
  call std::function<int (int, int)>::operator()(int, int) const
  mov dword ptr [rbp - 8], eax
  lea rdi, [rbp - 24]
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator++()
  mov qword ptr [rbp - 56], rax # 8-byte Spill
  jmp .LBB0_1
.LBB0_4:
  mov eax, dword ptr [rbp - 8]
  add rsp, 64
  pop rbp
  ret
std::vector<int, std::allocator<int> >::begin(): # @std::vector<int, std::allocator<int> >::begin()
  push rbp
  mov rbp, rsp
  sub rsp, 32
  lea rax, [rbp - 8]
  mov qword ptr [rbp - 16], rdi
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 24], rdi # 8-byte Spill
  mov rdi, rax
  mov rsi, qword ptr [rbp - 24] # 8-byte Reload
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::__normal_iterator(int* const&)
  mov rax, qword ptr [rbp - 8]
  add rsp, 32
  pop rbp
  ret
std::vector<int, std::allocator<int> >::end(): # @std::vector<int, std::allocator<int> >::end()
  push rbp
  mov rbp, rsp
  sub rsp, 32
  lea rax, [rbp - 8]
  mov qword ptr [rbp - 16], rdi
  mov rdi, qword ptr [rbp - 16]
  add rdi, 8
  mov qword ptr [rbp - 24], rdi # 8-byte Spill
  mov rdi, rax
  mov rsi, qword ptr [rbp - 24] # 8-byte Reload
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::__normal_iterator(int* const&)
  mov rax, qword ptr [rbp - 8]
  add rsp, 32
  pop rbp
  ret
bool __gnu_cxx::operator!=<int*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&, __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&): # @bool __gnu_cxx::operator!=<int*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&, __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rdi, qword ptr [rbp - 8]
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::base() const
  mov rax, qword ptr [rax]
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 24], rax # 8-byte Spill
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::base() const
  mov rsi, qword ptr [rbp - 24] # 8-byte Reload
  cmp rsi, qword ptr [rax]
  setne cl
  and cl, 1
  movzx eax, cl
  add rsp, 32
  pop rbp
  ret
__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator*() const: # @__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator*() const
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, qword ptr [rdi]
  pop rbp
  ret
std::function<int (int, int)>::operator()(int, int) const: # @std::function<int (int, int)>::operator()(int, int) const
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov dword ptr [rbp - 12], esi
  mov dword ptr [rbp - 16], edx
  mov rdi, qword ptr [rbp - 8]
  mov rax, rdi
  mov qword ptr [rbp - 24], rdi # 8-byte Spill
  mov rdi, rax
  call std::_Function_base::_M_empty() const
  test al, 1
  jne .LBB5_1
  jmp .LBB5_2
.LBB5_1:
  call std::__throw_bad_function_call()
.LBB5_2:
  lea rdi, [rbp - 12]
  mov rax, qword ptr [rbp - 24] # 8-byte Reload
  mov rcx, qword ptr [rax + 24]
  mov qword ptr [rbp - 32], rcx # 8-byte Spill
  mov qword ptr [rbp - 40], rax # 8-byte Spill
  call int&& std::forward<int>(std::remove_reference<int>::type&)
  lea rdi, [rbp - 16]
  mov qword ptr [rbp - 48], rax # 8-byte Spill
  call int&& std::forward<int>(std::remove_reference<int>::type&)
  mov rdi, qword ptr [rbp - 40] # 8-byte Reload
  mov rsi, qword ptr [rbp - 48] # 8-byte Reload
  mov rdx, rax
  mov rax, qword ptr [rbp - 32] # 8-byte Reload
  call rax
  add rsp, 48
  pop rbp
  ret
__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator++(): # @__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator++()
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, qword ptr [rdi]
  add rax, 4
  mov qword ptr [rdi], rax
  mov rax, rdi
  pop rbp
  ret
main: # @main
  push rbp
  mov rbp, rsp
  sub rsp, 144
  lea rax, [rbp - 24]
  mov rdi, rax
  mov qword ptr [rbp - 128], rax # 8-byte Spill
  call std::vector<int, std::allocator<int> >::vector()
  mov dword ptr [rbp - 28], 1
  lea rsi, [rbp - 28]
  mov rdi, qword ptr [rbp - 128] # 8-byte Reload
  call std::vector<int, std::allocator<int> >::push_back(int&&)
  jmp .LBB7_1
.LBB7_1:
  mov dword ptr [rbp - 48], 2
  lea rdi, [rbp - 24]
  lea rsi, [rbp - 48]
  call std::vector<int, std::allocator<int> >::push_back(int&&)
  jmp .LBB7_2
.LBB7_2:
  mov dword ptr [rbp - 52], 3
  lea rdi, [rbp - 24]
  lea rsi, [rbp - 52]
  call std::vector<int, std::allocator<int> >::push_back(int&&)
  jmp .LBB7_3
.LBB7_3:
  lea rdi, [rbp - 80]
  lea rsi, [rbp - 24]
  call std::vector<int, std::allocator<int> >::vector(std::vector<int, std::allocator<int> > const&)
  jmp .LBB7_4
.LBB7_4:
  lea rdi, [rbp - 112]
  call std::function<int (int, int)>::function<main::$_0, void, void>(main::$_0)
  jmp .LBB7_5
.LBB7_5:
  lea rdi, [rbp - 80]
  xor esi, esi
  lea rdx, [rbp - 112]
  call reduce(std::vector<int, std::allocator<int> >, int, std::function<int (int, int)>)
  mov dword ptr [rbp - 132], eax # 4-byte Spill
  jmp .LBB7_6
.LBB7_6:
  lea rdi, [rbp - 112]
  call std::function<int (int, int)>::~function()
  lea rdi, [rbp - 80]
  call std::vector<int, std::allocator<int> >::~vector()
  lea rdi, [rbp - 24]
  call std::vector<int, std::allocator<int> >::~vector()
  xor eax, eax
  add rsp, 144
  pop rbp
  ret
  mov ecx, edx
  mov qword ptr [rbp - 40], rax
  mov dword ptr [rbp - 44], ecx
  jmp .LBB7_11
  mov ecx, edx
  mov qword ptr [rbp - 40], rax
  mov dword ptr [rbp - 44], ecx
  jmp .LBB7_10
  lea rdi, [rbp - 112]
  mov ecx, edx
  mov qword ptr [rbp - 40], rax
  mov dword ptr [rbp - 44], ecx
  call std::function<int (int, int)>::~function()
.LBB7_10:
  lea rdi, [rbp - 80]
  call std::vector<int, std::allocator<int> >::~vector()
.LBB7_11:
  lea rdi, [rbp - 24]
  call std::vector<int, std::allocator<int> >::~vector()
  mov rdi, qword ptr [rbp - 40]
  call _Unwind_Resume
std::vector<int, std::allocator<int> >::vector(): # @std::vector<int, std::allocator<int> >::vector()
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::_Vector_base<int, std::allocator<int> >::_Vector_base()
  jmp .LBB8_1
.LBB8_1:
  add rsp, 16
  pop rbp
  ret
  mov ecx, edx
  mov rdi, rax
  mov dword ptr [rbp - 12], ecx # 4-byte Spill
  call __clang_call_terminate
std::vector<int, std::allocator<int> >::push_back(int&&): # @std::vector<int, std::allocator<int> >::push_back(int&&)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rdi, qword ptr [rbp - 8]
  mov rsi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 24], rdi # 8-byte Spill
  mov rdi, rsi
  call std::remove_reference<int&>::type&& std::move<int&>(int&)
  mov rdi, qword ptr [rbp - 24] # 8-byte Reload
  mov rsi, rax
  call int& std::vector<int, std::allocator<int> >::emplace_back<int>(int&&)
  mov qword ptr [rbp - 32], rax # 8-byte Spill
  add rsp, 32
  pop rbp
  ret
std::vector<int, std::allocator<int> >::vector(std::vector<int, std::allocator<int> > const&): # @std::vector<int, std::allocator<int> >::vector(std::vector<int, std::allocator<int> > const&)
  push rbp
  mov rbp, rsp
  sub rsp, 96
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rsi, qword ptr [rbp - 8]
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 64], rsi # 8-byte Spill
  call std::vector<int, std::allocator<int> >::size() const
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 72], rax # 8-byte Spill
  call std::_Vector_base<int, std::allocator<int> >::_M_get_Tp_allocator() const
  lea rsi, [rbp - 24]
  mov rdi, rsi
  mov qword ptr [rbp - 80], rsi # 8-byte Spill
  mov rsi, rax
  call __gnu_cxx::__alloc_traits<std::allocator<int> >::_S_select_on_copy(std::allocator<int> const&)
  mov rdi, qword ptr [rbp - 64] # 8-byte Reload
  mov rsi, qword ptr [rbp - 72] # 8-byte Reload
  mov rdx, qword ptr [rbp - 80] # 8-byte Reload
  call std::_Vector_base<int, std::allocator<int> >::_Vector_base(unsigned long, std::allocator<int> const&)
  jmp .LBB10_1
.LBB10_1:
  lea rdi, [rbp - 24]
  call std::allocator<int>::~allocator()
  mov rdi, qword ptr [rbp - 16]
  call std::vector<int, std::allocator<int> >::begin() const
  mov qword ptr [rbp - 48], rax
  mov rdi, qword ptr [rbp - 16]
  call std::vector<int, std::allocator<int> >::end() const
  mov qword ptr [rbp - 56], rax
  mov rax, qword ptr [rbp - 64] # 8-byte Reload
  mov rdx, qword ptr [rax]
  mov rdi, rax
  mov qword ptr [rbp - 88], rdx # 8-byte Spill
  call std::_Vector_base<int, std::allocator<int> >::_M_get_Tp_allocator()
  mov rdi, qword ptr [rbp - 48]
  mov rsi, qword ptr [rbp - 56]
  mov rdx, qword ptr [rbp - 88] # 8-byte Reload
  mov rcx, rax
  call int* std::__uninitialized_copy_a<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*, int>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*, std::allocator<int>&)
  mov qword ptr [rbp - 96], rax # 8-byte Spill
  jmp .LBB10_2
.LBB10_2:
  mov rax, qword ptr [rbp - 64] # 8-byte Reload
  mov rcx, qword ptr [rbp - 96] # 8-byte Reload
  mov qword ptr [rax + 8], rcx
  add rsp, 96
  pop rbp
  ret
  lea rdi, [rbp - 24]
  mov ecx, edx
  mov qword ptr [rbp - 32], rax
  mov dword ptr [rbp - 36], ecx
  call std::allocator<int>::~allocator()
  jmp .LBB10_5
  mov ecx, edx
  mov qword ptr [rbp - 32], rax
  mov dword ptr [rbp - 36], ecx
  mov rax, qword ptr [rbp - 64] # 8-byte Reload
  mov rdi, rax
  call std::_Vector_base<int, std::allocator<int> >::~_Vector_base()
.LBB10_5:
  mov rdi, qword ptr [rbp - 32]
  call _Unwind_Resume
std::function<int (int, int)>::function<main::$_0, void, void>(main::$_0): # @"std::function<int (int, int)>::function<main::$_0, void, void>(main::$_0)"
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 16], rdi
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 40], rdi # 8-byte Spill
  call std::_Function_base::_Function_base()
  lea rdi, [rbp - 8]
  call bool std::_Function_base::_Base_manager<main::$_0>::_M_not_empty_function<main::$_0>(main::$_0 const&)
  mov byte ptr [rbp - 41], al # 1-byte Spill
  jmp .LBB11_1
.LBB11_1:
  mov al, byte ptr [rbp - 41] # 1-byte Reload
  test al, 1
  jne .LBB11_2
  jmp .LBB11_5
.LBB11_2:
  lea rdi, [rbp - 8]
  call std::remove_reference<main::$_0&>::type&& std::move<main::$_0&>(main::$_0&)
  mov rdi, qword ptr [rbp - 40] # 8-byte Reload
  mov rsi, rax
  call std::_Function_base::_Base_manager<main::$_0>::_M_init_functor(std::_Any_data&, main::$_0&&)
  jmp .LBB11_3
.LBB11_3:
  movabs rax, std::_Function_base::_Base_manager<main::$_0>::_M_manager(std::_Any_data&, std::_Any_data const&, std::_Manager_operation)
  movabs rcx, std::_Function_handler<int (int, int), main::$_0>::_M_invoke(std::_Any_data const&, int&&, int&&)
  mov rdx, qword ptr [rbp - 40] # 8-byte Reload
  mov qword ptr [rdx + 24], rcx
  mov qword ptr [rdx + 16], rax
  jmp .LBB11_5
  mov ecx, edx
  mov qword ptr [rbp - 24], rax
  mov dword ptr [rbp - 28], ecx
  mov rax, qword ptr [rbp - 40] # 8-byte Reload
  mov rdi, rax
  call std::_Function_base::~_Function_base()
  jmp .LBB11_6
.LBB11_5:
  add rsp, 48
  pop rbp
  ret
.LBB11_6:
  mov rdi, qword ptr [rbp - 24]
  call _Unwind_Resume
std::function<int (int, int)>::~function(): # @std::function<int (int, int)>::~function()
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::_Function_base::~_Function_base()
  add rsp, 16
  pop rbp
  ret
std::vector<int, std::allocator<int> >::~vector(): # @std::vector<int, std::allocator<int> >::~vector()
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, qword ptr [rdi]
  mov rsi, qword ptr [rdi + 8]
  mov qword ptr [rbp - 32], rdi # 8-byte Spill
  mov qword ptr [rbp - 40], rsi # 8-byte Spill
  mov qword ptr [rbp - 48], rax # 8-byte Spill
  call std::_Vector_base<int, std::allocator<int> >::_M_get_Tp_allocator()
  mov rdi, qword ptr [rbp - 48] # 8-byte Reload
  mov rsi, qword ptr [rbp - 40] # 8-byte Reload
  mov rdx, rax
  call void std::_Destroy<int*, int>(int*, int*, std::allocator<int>&)
  jmp .LBB13_1
.LBB13_1:
  mov rax, qword ptr [rbp - 32] # 8-byte Reload
  mov rdi, rax
  call std::_Vector_base<int, std::allocator<int> >::~_Vector_base()
  add rsp, 48
  pop rbp
  ret
  mov ecx, edx
  mov qword ptr [rbp - 16], rax
  mov dword ptr [rbp - 20], ecx
  mov rax, qword ptr [rbp - 32] # 8-byte Reload
  mov rdi, rax
  call std::_Vector_base<int, std::allocator<int> >::~_Vector_base()
  mov rdi, qword ptr [rbp - 16]
  call __clang_call_terminate
std::_Function_base::~_Function_base(): # @std::_Function_base::~_Function_base()
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  cmp qword ptr [rdi + 16], 0
  mov qword ptr [rbp - 16], rdi # 8-byte Spill
  je .LBB14_3
  mov rax, qword ptr [rbp - 16] # 8-byte Reload
  mov rcx, qword ptr [rax + 16]
  mov edx, 3
  mov rdi, rax
  mov rsi, rax
  call rcx
  mov byte ptr [rbp - 17], al # 1-byte Spill
  jmp .LBB14_2
.LBB14_2:
  jmp .LBB14_3
.LBB14_3:
  add rsp, 32
  pop rbp
  ret
  mov ecx, edx
  mov rdi, rax
  mov dword ptr [rbp - 24], ecx # 4-byte Spill
  call __clang_call_terminate
__clang_call_terminate: # @__clang_call_terminate
  push rax
  call __cxa_begin_catch
  mov qword ptr [rsp], rax # 8-byte Spill
  call std::terminate()
__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::__normal_iterator(int* const&): # @__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::__normal_iterator(int* const&)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rsi, qword ptr [rbp - 8]
  mov rdi, qword ptr [rbp - 16]
  mov rdi, qword ptr [rdi]
  mov qword ptr [rsi], rdi
  pop rbp
  ret
__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::base() const: # @__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::base() const
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rax, qword ptr [rbp - 8]
  pop rbp
  ret
std::_Function_base::_M_empty() const: # @std::_Function_base::_M_empty() const
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  cmp qword ptr [rdi + 16], 0
  setne al
  xor al, -1
  and al, 1
  movzx eax, al
  pop rbp
  ret
int&& std::forward<int>(std::remove_reference<int>::type&): # @int&& std::forward<int>(std::remove_reference<int>::type&)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rax, qword ptr [rbp - 8]
  pop rbp
  ret
std::_Vector_base<int, std::allocator<int> >::_Vector_base(): # @std::_Vector_base<int, std::allocator<int> >::_Vector_base()
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::_Vector_base<int, std::allocator<int> >::_Vector_impl::_Vector_impl()
  add rsp, 16
  pop rbp
  ret
std::_Vector_base<int, std::allocator<int> >::_Vector_impl::_Vector_impl(): # @std::_Vector_base<int, std::allocator<int> >::_Vector_impl::_Vector_impl()
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, rdi
  mov qword ptr [rbp - 16], rdi # 8-byte Spill
  mov rdi, rax
  call std::allocator<int>::allocator()
  mov rax, qword ptr [rbp - 16] # 8-byte Reload
  mov qword ptr [rax], 0
  mov qword ptr [rax + 8], 0
  mov qword ptr [rax + 16], 0
  add rsp, 16
  pop rbp
  ret
std::allocator<int>::allocator(): # @std::allocator<int>::allocator()
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call __gnu_cxx::new_allocator<int>::new_allocator()
  add rsp, 16
  pop rbp
  ret
__gnu_cxx::new_allocator<int>::new_allocator(): # @__gnu_cxx::new_allocator<int>::new_allocator()
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  pop rbp
  ret
void std::_Destroy<int*, int>(int*, int*, std::allocator<int>&): # @void std::_Destroy<int*, int>(int*, int*, std::allocator<int>&)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdi, qword ptr [rbp - 8]
  mov rsi, qword ptr [rbp - 16]
  call void std::_Destroy<int*>(int*, int*)
  add rsp, 32
  pop rbp
  ret
std::_Vector_base<int, std::allocator<int> >::_M_get_Tp_allocator(): # @std::_Vector_base<int, std::allocator<int> >::_M_get_Tp_allocator()
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, rdi
  pop rbp
  ret
std::_Vector_base<int, std::allocator<int> >::~_Vector_base(): # @std::_Vector_base<int, std::allocator<int> >::~_Vector_base()
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, qword ptr [rdi]
  mov rcx, qword ptr [rdi + 16]
  sub rcx, rax
  sar rcx, 2
  mov qword ptr [rbp - 32], rdi # 8-byte Spill
  mov rsi, rax
  mov rdx, rcx
  call std::_Vector_base<int, std::allocator<int> >::_M_deallocate(int*, unsigned long)
  jmp .LBB26_1
.LBB26_1:
  mov rdi, qword ptr [rbp - 32] # 8-byte Reload
  call std::_Vector_base<int, std::allocator<int> >::_Vector_impl::~_Vector_impl()
  add rsp, 32
  pop rbp
  ret
  mov ecx, edx
  mov qword ptr [rbp - 16], rax
  mov dword ptr [rbp - 20], ecx
  mov rdi, qword ptr [rbp - 32] # 8-byte Reload
  call std::_Vector_base<int, std::allocator<int> >::_Vector_impl::~_Vector_impl()
  mov rdi, qword ptr [rbp - 16]
  call __clang_call_terminate
void std::_Destroy<int*>(int*, int*): # @void std::_Destroy<int*>(int*, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rdi, qword ptr [rbp - 8]
  mov rsi, qword ptr [rbp - 16]
  call void std::_Destroy_aux<true>::__destroy<int*>(int*, int*)
  add rsp, 16
  pop rbp
  ret
void std::_Destroy_aux<true>::__destroy<int*>(int*, int*): # @void std::_Destroy_aux<true>::__destroy<int*>(int*, int*)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  pop rbp
  ret
std::_Vector_base<int, std::allocator<int> >::_M_deallocate(int*, unsigned long): # @std::_Vector_base<int, std::allocator<int> >::_M_deallocate(int*, unsigned long)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 8]
  cmp qword ptr [rbp - 16], 0
  mov qword ptr [rbp - 32], rdx # 8-byte Spill
  je .LBB29_2
  mov rax, qword ptr [rbp - 32] # 8-byte Reload
  mov rsi, qword ptr [rbp - 16]
  mov rdx, qword ptr [rbp - 24]
  mov rdi, rax
  call std::allocator_traits<std::allocator<int> >::deallocate(std::allocator<int>&, int*, unsigned long)
.LBB29_2:
  add rsp, 32
  pop rbp
  ret
std::_Vector_base<int, std::allocator<int> >::_Vector_impl::~_Vector_impl(): # @std::_Vector_base<int, std::allocator<int> >::_Vector_impl::~_Vector_impl()
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::allocator<int>::~allocator()
  add rsp, 16
  pop rbp
  ret
std::allocator_traits<std::allocator<int> >::deallocate(std::allocator<int>&, int*, unsigned long): # @std::allocator_traits<std::allocator<int> >::deallocate(std::allocator<int>&, int*, unsigned long)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 8]
  mov rsi, qword ptr [rbp - 16]
  mov rdi, qword ptr [rbp - 24]
  mov qword ptr [rbp - 32], rdi # 8-byte Spill
  mov rdi, rdx
  mov rdx, qword ptr [rbp - 32] # 8-byte Reload
  call __gnu_cxx::new_allocator<int>::deallocate(int*, unsigned long)
  add rsp, 32
  pop rbp
  ret
__gnu_cxx::new_allocator<int>::deallocate(int*, unsigned long): # @__gnu_cxx::new_allocator<int>::deallocate(int*, unsigned long)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 16]
  mov rdi, rdx
  call operator delete(void*)
  add rsp, 32
  pop rbp
  ret
__gnu_cxx::new_allocator<int>::~new_allocator(): # @__gnu_cxx::new_allocator<int>::~new_allocator()
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  pop rbp
  ret
int& std::vector<int, std::allocator<int> >::emplace_back<int>(int&&): # @int& std::vector<int, std::allocator<int> >::emplace_back<int>(int&&)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rsi, qword ptr [rbp - 8]
  mov rdi, qword ptr [rsi + 8]
  cmp rdi, qword ptr [rsi + 16]
  mov qword ptr [rbp - 32], rsi # 8-byte Spill
  je .LBB34_2
  mov rax, qword ptr [rbp - 32] # 8-byte Reload
  mov rcx, qword ptr [rbp - 32] # 8-byte Reload
  mov rsi, qword ptr [rcx + 8]
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 40], rax # 8-byte Spill
  mov qword ptr [rbp - 48], rsi # 8-byte Spill
  call int&& std::forward<int>(std::remove_reference<int>::type&)
  mov rdi, qword ptr [rbp - 40] # 8-byte Reload
  mov rsi, qword ptr [rbp - 48] # 8-byte Reload
  mov rdx, rax
  call void std::allocator_traits<std::allocator<int> >::construct<int, int>(std::allocator<int>&, int*, int&&)
  mov rax, qword ptr [rbp - 32] # 8-byte Reload
  mov rcx, qword ptr [rax + 8]
  add rcx, 4
  mov qword ptr [rax + 8], rcx
  jmp .LBB34_3
.LBB34_2:
  mov rdi, qword ptr [rbp - 32] # 8-byte Reload
  call std::vector<int, std::allocator<int> >::end()
  mov qword ptr [rbp - 24], rax
  mov rdi, qword ptr [rbp - 16]
  call int&& std::forward<int>(std::remove_reference<int>::type&)
  mov rsi, qword ptr [rbp - 24]
  mov rdi, qword ptr [rbp - 32] # 8-byte Reload
  mov rdx, rax
  call void std::vector<int, std::allocator<int> >::_M_realloc_insert<int>(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >, int&&)
.LBB34_3:
  mov rdi, qword ptr [rbp - 32] # 8-byte Reload
  call std::vector<int, std::allocator<int> >::back()
  add rsp, 48
  pop rbp
  ret
std::remove_reference<int&>::type&& std::move<int&>(int&): # @std::remove_reference<int&>::type&& std::move<int&>(int&)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rax, qword ptr [rbp - 8]
  pop rbp
  ret
void std::allocator_traits<std::allocator<int> >::construct<int, int>(std::allocator<int>&, int*, int&&): # @void std::allocator_traits<std::allocator<int> >::construct<int, int>(std::allocator<int>&, int*, int&&)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 8]
  mov rsi, qword ptr [rbp - 16]
  mov rdi, qword ptr [rbp - 24]
  mov qword ptr [rbp - 32], rdx # 8-byte Spill
  mov qword ptr [rbp - 40], rsi # 8-byte Spill
  call int&& std::forward<int>(std::remove_reference<int>::type&)
  mov rdi, qword ptr [rbp - 32] # 8-byte Reload
  mov rsi, qword ptr [rbp - 40] # 8-byte Reload
  mov rdx, rax
  call void __gnu_cxx::new_allocator<int>::construct<int, int>(int*, int&&)
  add rsp, 48
  pop rbp
  ret
void std::vector<int, std::allocator<int> >::_M_realloc_insert<int>(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >, int&&): # @void std::vector<int, std::allocator<int> >::_M_realloc_insert<int>(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >, int&&)
  push rbp
  mov rbp, rsp
  sub rsp, 240
  mov qword ptr [rbp - 8], rsi
  mov qword ptr [rbp - 16], rdi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 16]
  mov eax, .L.str
  mov esi, eax
  mov eax, 1
  mov edi, eax
  mov qword ptr [rbp - 88], rdi # 8-byte Spill
  mov rdi, rdx
  mov rcx, qword ptr [rbp - 88] # 8-byte Reload
  mov qword ptr [rbp - 96], rsi # 8-byte Spill
  mov rsi, rcx
  mov r8, qword ptr [rbp - 96] # 8-byte Reload
  mov qword ptr [rbp - 104], rdx # 8-byte Spill
  mov rdx, r8
  call std::vector<int, std::allocator<int> >::_M_check_len(unsigned long, char const*) const
  mov qword ptr [rbp - 32], rax
  mov rdi, qword ptr [rbp - 104] # 8-byte Reload
  call std::vector<int, std::allocator<int> >::begin()
  mov qword ptr [rbp - 48], rax
  lea rdi, [rbp - 8]
  lea rsi, [rbp - 48]
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::difference_type __gnu_cxx::operator-<int*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&, __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&)
  mov qword ptr [rbp - 40], rax
  mov rsi, qword ptr [rbp - 32]
  mov rdi, qword ptr [rbp - 104] # 8-byte Reload
  call std::_Vector_base<int, std::allocator<int> >::_M_allocate(unsigned long)
  mov qword ptr [rbp - 56], rax
  mov rax, qword ptr [rbp - 56]
  mov qword ptr [rbp - 64], rax
  mov rcx, qword ptr [rbp - 40]
  lea rsi, [rax + 4*rcx]
  mov rdi, qword ptr [rbp - 24]
  mov qword ptr [rbp - 112], rsi # 8-byte Spill
  call int&& std::forward<int>(std::remove_reference<int>::type&)
  mov rdi, qword ptr [rbp - 104] # 8-byte Reload
  mov rsi, qword ptr [rbp - 112] # 8-byte Reload
  mov rdx, rax
  call void std::allocator_traits<std::allocator<int> >::construct<int, int>(std::allocator<int>&, int*, int&&)
  jmp .LBB37_1
.LBB37_1:
  mov qword ptr [rbp - 64], 0
  mov rax, qword ptr [rbp - 104] # 8-byte Reload
  mov rdi, qword ptr [rax]
  lea rcx, [rbp - 8]
  mov qword ptr [rbp - 120], rdi # 8-byte Spill
  mov rdi, rcx
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::base() const
  mov rsi, qword ptr [rax]
  mov rdx, qword ptr [rbp - 56]
  mov rdi, qword ptr [rbp - 104] # 8-byte Reload
  mov qword ptr [rbp - 128], rsi # 8-byte Spill
  mov qword ptr [rbp - 136], rdx # 8-byte Spill
  call std::_Vector_base<int, std::allocator<int> >::_M_get_Tp_allocator()
  mov rdi, qword ptr [rbp - 120] # 8-byte Reload
  mov rsi, qword ptr [rbp - 128] # 8-byte Reload
  mov rdx, qword ptr [rbp - 136] # 8-byte Reload
  mov rcx, rax
  call int* std::__uninitialized_move_if_noexcept_a<int*, int*, std::allocator<int> >(int*, int*, int*, std::allocator<int>&)
  mov qword ptr [rbp - 144], rax # 8-byte Spill
  jmp .LBB37_2
.LBB37_2:
  mov rax, qword ptr [rbp - 144] # 8-byte Reload
  mov qword ptr [rbp - 64], rax
  mov rcx, qword ptr [rbp - 64]
  add rcx, 4
  mov qword ptr [rbp - 64], rcx
  lea rdi, [rbp - 8]
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::base() const
  mov rdi, qword ptr [rax]
  mov rax, qword ptr [rbp - 104] # 8-byte Reload
  mov rsi, qword ptr [rax + 8]
  mov rdx, qword ptr [rbp - 64]
  mov qword ptr [rbp - 152], rdi # 8-byte Spill
  mov rdi, rax
  mov qword ptr [rbp - 160], rdx # 8-byte Spill
  mov qword ptr [rbp - 168], rsi # 8-byte Spill
  call std::_Vector_base<int, std::allocator<int> >::_M_get_Tp_allocator()
  mov rdi, qword ptr [rbp - 152] # 8-byte Reload
  mov rsi, qword ptr [rbp - 168] # 8-byte Reload
  mov rdx, qword ptr [rbp - 160] # 8-byte Reload
  mov rcx, rax
  call int* std::__uninitialized_move_if_noexcept_a<int*, int*, std::allocator<int> >(int*, int*, int*, std::allocator<int>&)
  mov qword ptr [rbp - 176], rax # 8-byte Spill
  jmp .LBB37_3
.LBB37_3:
  mov rax, qword ptr [rbp - 176] # 8-byte Reload
  mov qword ptr [rbp - 64], rax
  jmp .LBB37_14
  mov ecx, edx
  mov qword ptr [rbp - 72], rax
  mov dword ptr [rbp - 76], ecx
  mov rdi, qword ptr [rbp - 72]
  call __cxa_begin_catch
  cmp qword ptr [rbp - 64], 0
  mov qword ptr [rbp - 184], rax # 8-byte Spill
  jne .LBB37_9
  mov rax, qword ptr [rbp - 56]
  mov rcx, qword ptr [rbp - 40]
  lea rsi, [rax + 4*rcx]
  mov rdi, qword ptr [rbp - 104] # 8-byte Reload
  call void std::allocator_traits<std::allocator<int> >::destroy<int>(std::allocator<int>&, int*)
  jmp .LBB37_7
.LBB37_7:
  jmp .LBB37_11
  mov ecx, edx
  mov qword ptr [rbp - 72], rax
  mov dword ptr [rbp - 76], ecx
  call __cxa_end_catch
  jmp .LBB37_13
.LBB37_9:
  mov rdi, qword ptr [rbp - 56]
  mov rsi, qword ptr [rbp - 64]
  mov rax, qword ptr [rbp - 104] # 8-byte Reload
  mov qword ptr [rbp - 192], rdi # 8-byte Spill
  mov rdi, rax
  mov qword ptr [rbp - 200], rsi # 8-byte Spill
  call std::_Vector_base<int, std::allocator<int> >::_M_get_Tp_allocator()
  mov rdi, qword ptr [rbp - 192] # 8-byte Reload
  mov rsi, qword ptr [rbp - 200] # 8-byte Reload
  mov rdx, rax
  call void std::_Destroy<int*, int>(int*, int*, std::allocator<int>&)
  jmp .LBB37_10
.LBB37_10:
  jmp .LBB37_11
.LBB37_11:
  mov rsi, qword ptr [rbp - 56]
  mov rdx, qword ptr [rbp - 32]
  mov rdi, qword ptr [rbp - 104] # 8-byte Reload
  call std::_Vector_base<int, std::allocator<int> >::_M_deallocate(int*, unsigned long)
  jmp .LBB37_12
.LBB37_12:
  call __cxa_rethrow
  jmp .LBB37_17
.LBB37_13:
  jmp .LBB37_15
.LBB37_14:
  mov rax, qword ptr [rbp - 104] # 8-byte Reload
  mov rdi, qword ptr [rax]
  mov rsi, qword ptr [rax + 8]
  mov qword ptr [rbp - 208], rdi # 8-byte Spill
  mov rdi, rax
  mov qword ptr [rbp - 216], rsi # 8-byte Spill
  call std::_Vector_base<int, std::allocator<int> >::_M_get_Tp_allocator()
  mov rdi, qword ptr [rbp - 208] # 8-byte Reload
  mov rsi, qword ptr [rbp - 216] # 8-byte Reload
  mov rdx, rax
  call void std::_Destroy<int*, int>(int*, int*, std::allocator<int>&)
  mov rax, qword ptr [rbp - 104] # 8-byte Reload
  mov rdx, qword ptr [rbp - 104] # 8-byte Reload
  mov rsi, qword ptr [rdx]
  mov rdi, qword ptr [rdx + 16]
  mov rcx, qword ptr [rdx]
  sub rdi, rcx
  sar rdi, 2
  mov qword ptr [rbp - 224], rdi # 8-byte Spill
  mov rdi, rax
  mov rdx, qword ptr [rbp - 224] # 8-byte Reload
  call std::_Vector_base<int, std::allocator<int> >::_M_deallocate(int*, unsigned long)
  mov rax, qword ptr [rbp - 56]
  mov rcx, qword ptr [rbp - 104] # 8-byte Reload
  mov qword ptr [rcx], rax
  mov rax, qword ptr [rbp - 64]
  mov qword ptr [rcx + 8], rax
  mov rax, qword ptr [rbp - 56]
  mov rdx, qword ptr [rbp - 32]
  shl rdx, 2
  add rax, rdx
  mov qword ptr [rcx + 16], rax
  add rsp, 240
  pop rbp
  ret
.LBB37_15:
  mov rdi, qword ptr [rbp - 72]
  call _Unwind_Resume
  mov ecx, edx
  mov rdi, rax
  mov dword ptr [rbp - 228], ecx # 4-byte Spill
  call __clang_call_terminate
.LBB37_17:
std::vector<int, std::allocator<int> >::back(): # @std::vector<int, std::allocator<int> >::back()
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::vector<int, std::allocator<int> >::end()
  lea rdi, [rbp - 16]
  mov ecx, 1
  mov esi, ecx
  mov qword ptr [rbp - 16], rax
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator-(long) const
  lea rdi, [rbp - 24]
  mov qword ptr [rbp - 24], rax
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator*() const
  add rsp, 32
  pop rbp
  ret
void __gnu_cxx::new_allocator<int>::construct<int, int>(int*, int&&): # @void __gnu_cxx::new_allocator<int>::construct<int, int>(int*, int&&)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 16]
  mov rdi, qword ptr [rbp - 24]
  mov qword ptr [rbp - 32], rdx # 8-byte Spill
  call int&& std::forward<int>(std::remove_reference<int>::type&)
  mov ecx, dword ptr [rax]
  mov rax, qword ptr [rbp - 32] # 8-byte Reload
  mov dword ptr [rax], ecx
  add rsp, 32
  pop rbp
  ret
std::vector<int, std::allocator<int> >::_M_check_len(unsigned long, char const*) const: # @std::vector<int, std::allocator<int> >::_M_check_len(unsigned long, char const*) const
  push rbp
  mov rbp, rsp
  sub rsp, 96
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 8]
  mov rdi, rdx
  mov qword ptr [rbp - 48], rdx # 8-byte Spill
  call std::vector<int, std::allocator<int> >::max_size() const
  mov rdi, qword ptr [rbp - 48] # 8-byte Reload
  mov qword ptr [rbp - 56], rax # 8-byte Spill
  call std::vector<int, std::allocator<int> >::size() const
  mov rdx, qword ptr [rbp - 56] # 8-byte Reload
  sub rdx, rax
  cmp rdx, qword ptr [rbp - 16]
  jae .LBB40_2
  mov rdi, qword ptr [rbp - 24]
  call std::__throw_length_error(char const*)
.LBB40_2:
  mov rdi, qword ptr [rbp - 48] # 8-byte Reload
  call std::vector<int, std::allocator<int> >::size() const
  mov rdi, qword ptr [rbp - 48] # 8-byte Reload
  mov qword ptr [rbp - 64], rax # 8-byte Spill
  call std::vector<int, std::allocator<int> >::size() const
  lea rdi, [rbp - 40]
  lea rsi, [rbp - 16]
  mov qword ptr [rbp - 40], rax
  call unsigned long const& std::max<unsigned long>(unsigned long const&, unsigned long const&)
  mov rsi, qword ptr [rbp - 64] # 8-byte Reload
  add rsi, qword ptr [rax]
  mov qword ptr [rbp - 32], rsi
  mov rax, qword ptr [rbp - 32]
  mov rdi, qword ptr [rbp - 48] # 8-byte Reload
  mov qword ptr [rbp - 72], rax # 8-byte Spill
  call std::vector<int, std::allocator<int> >::size() const
  mov rsi, qword ptr [rbp - 72] # 8-byte Reload
  cmp rsi, rax
  jb .LBB40_4
  mov rax, qword ptr [rbp - 32]
  mov rdi, qword ptr [rbp - 48] # 8-byte Reload
  mov qword ptr [rbp - 80], rax # 8-byte Spill
  call std::vector<int, std::allocator<int> >::max_size() const
  mov rdi, qword ptr [rbp - 80] # 8-byte Reload
  cmp rdi, rax
  jbe .LBB40_5
.LBB40_4:
  mov rdi, qword ptr [rbp - 48] # 8-byte Reload
  call std::vector<int, std::allocator<int> >::max_size() const
  mov qword ptr [rbp - 88], rax # 8-byte Spill
  jmp .LBB40_6
.LBB40_5:
  mov rax, qword ptr [rbp - 32]
  mov qword ptr [rbp - 88], rax # 8-byte Spill
.LBB40_6:
  mov rax, qword ptr [rbp - 88] # 8-byte Reload
  add rsp, 96
  pop rbp
  ret
__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::difference_type __gnu_cxx::operator-<int*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&, __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&): # @__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::difference_type __gnu_cxx::operator-<int*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&, __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > > const&)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rdi, qword ptr [rbp - 8]
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::base() const
  mov rax, qword ptr [rax]
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 24], rax # 8-byte Spill
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::base() const
  mov rax, qword ptr [rax]
  mov rsi, qword ptr [rbp - 24] # 8-byte Reload
  sub rsi, rax
  sar rsi, 2
  mov rax, rsi
  add rsp, 32
  pop rbp
  ret
std::_Vector_base<int, std::allocator<int> >::_M_allocate(unsigned long): # @std::_Vector_base<int, std::allocator<int> >::_M_allocate(unsigned long)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rsi, qword ptr [rbp - 8]
  cmp qword ptr [rbp - 16], 0
  mov qword ptr [rbp - 24], rsi # 8-byte Spill
  je .LBB42_2
  mov rax, qword ptr [rbp - 24] # 8-byte Reload
  mov rsi, qword ptr [rbp - 16]
  mov rdi, rax
  call std::allocator_traits<std::allocator<int> >::allocate(std::allocator<int>&, unsigned long)
  mov qword ptr [rbp - 32], rax # 8-byte Spill
  jmp .LBB42_3
.LBB42_2:
  xor eax, eax
  mov ecx, eax
  mov qword ptr [rbp - 32], rcx # 8-byte Spill
  jmp .LBB42_3
.LBB42_3:
  mov rax, qword ptr [rbp - 32] # 8-byte Reload
  add rsp, 32
  pop rbp
  ret
int* std::__uninitialized_move_if_noexcept_a<int*, int*, std::allocator<int> >(int*, int*, int*, std::allocator<int>&): # @int* std::__uninitialized_move_if_noexcept_a<int*, int*, std::allocator<int> >(int*, int*, int*, std::allocator<int>&)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov qword ptr [rbp - 32], rcx
  mov rdi, qword ptr [rbp - 8]
  call std::move_iterator<int*> std::__make_move_if_noexcept_iterator<int, std::move_iterator<int*> >(int*)
  mov qword ptr [rbp - 40], rax
  mov rdi, qword ptr [rbp - 16]
  call std::move_iterator<int*> std::__make_move_if_noexcept_iterator<int, std::move_iterator<int*> >(int*)
  mov qword ptr [rbp - 48], rax
  mov rdx, qword ptr [rbp - 24]
  mov rcx, qword ptr [rbp - 32]
  mov rdi, qword ptr [rbp - 40]
  mov rsi, qword ptr [rbp - 48]
  call int* std::__uninitialized_copy_a<std::move_iterator<int*>, int*, int>(std::move_iterator<int*>, std::move_iterator<int*>, int*, std::allocator<int>&)
  add rsp, 48
  pop rbp
  ret
void std::allocator_traits<std::allocator<int> >::destroy<int>(std::allocator<int>&, int*): # @void std::allocator_traits<std::allocator<int> >::destroy<int>(std::allocator<int>&, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rsi, qword ptr [rbp - 8]
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 24], rdi # 8-byte Spill
  mov rdi, rsi
  mov rsi, qword ptr [rbp - 24] # 8-byte Reload
  call void __gnu_cxx::new_allocator<int>::destroy<int>(int*)
  add rsp, 32
  pop rbp
  ret
std::vector<int, std::allocator<int> >::max_size() const: # @std::vector<int, std::allocator<int> >::max_size() const
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::_Vector_base<int, std::allocator<int> >::_M_get_Tp_allocator() const
  mov rdi, rax
  call std::allocator_traits<std::allocator<int> >::max_size(std::allocator<int> const&)
  add rsp, 16
  pop rbp
  ret
std::vector<int, std::allocator<int> >::size() const: # @std::vector<int, std::allocator<int> >::size() const
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, qword ptr [rdi + 8]
  mov rdi, qword ptr [rdi]
  sub rax, rdi
  sar rax, 2
  pop rbp
  ret
unsigned long const& std::max<unsigned long>(unsigned long const&, unsigned long const&): # @unsigned long const& std::max<unsigned long>(unsigned long const&, unsigned long const&)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 16], rdi
  mov qword ptr [rbp - 24], rsi
  mov rsi, qword ptr [rbp - 16]
  mov rsi, qword ptr [rsi]
  mov rdi, qword ptr [rbp - 24]
  cmp rsi, qword ptr [rdi]
  jae .LBB47_2
  mov rax, qword ptr [rbp - 24]
  mov qword ptr [rbp - 8], rax
  jmp .LBB47_3
.LBB47_2:
  mov rax, qword ptr [rbp - 16]
  mov qword ptr [rbp - 8], rax
.LBB47_3:
  mov rax, qword ptr [rbp - 8]
  pop rbp
  ret
std::allocator_traits<std::allocator<int> >::max_size(std::allocator<int> const&): # @std::allocator_traits<std::allocator<int> >::max_size(std::allocator<int> const&)
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call __gnu_cxx::new_allocator<int>::max_size() const
  add rsp, 16
  pop rbp
  ret
std::_Vector_base<int, std::allocator<int> >::_M_get_Tp_allocator() const: # @std::_Vector_base<int, std::allocator<int> >::_M_get_Tp_allocator() const
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, rdi
  pop rbp
  ret
__gnu_cxx::new_allocator<int>::max_size() const: # @__gnu_cxx::new_allocator<int>::max_size() const
  push rbp
  mov rbp, rsp
  movabs rax, 4611686018427387903
  mov qword ptr [rbp - 8], rdi
  pop rbp
  ret
std::allocator_traits<std::allocator<int> >::allocate(std::allocator<int>&, unsigned long): # @std::allocator_traits<std::allocator<int> >::allocate(std::allocator<int>&, unsigned long)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  xor eax, eax
  mov edx, eax
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rsi, qword ptr [rbp - 8]
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 24], rdi # 8-byte Spill
  mov rdi, rsi
  mov rsi, qword ptr [rbp - 24] # 8-byte Reload
  call __gnu_cxx::new_allocator<int>::allocate(unsigned long, void const*)
  add rsp, 32
  pop rbp
  ret
__gnu_cxx::new_allocator<int>::allocate(unsigned long, void const*): # @__gnu_cxx::new_allocator<int>::allocate(unsigned long, void const*)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdi, qword ptr [rbp - 8]
  mov rdx, qword ptr [rbp - 16]
  mov qword ptr [rbp - 32], rdx # 8-byte Spill
  call __gnu_cxx::new_allocator<int>::max_size() const
  mov rdx, qword ptr [rbp - 32] # 8-byte Reload
  cmp rdx, rax
  jbe .LBB52_2
  call std::__throw_bad_alloc()
.LBB52_2:
  mov rax, qword ptr [rbp - 16]
  shl rax, 2
  mov rdi, rax
  call operator new(unsigned long)
  add rsp, 32
  pop rbp
  ret
int* std::__uninitialized_copy_a<std::move_iterator<int*>, int*, int>(std::move_iterator<int*>, std::move_iterator<int*>, int*, std::allocator<int>&): # @int* std::__uninitialized_copy_a<std::move_iterator<int*>, int*, int>(std::move_iterator<int*>, std::move_iterator<int*>, int*, std::allocator<int>&)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov qword ptr [rbp - 32], rcx
  mov rcx, qword ptr [rbp - 8]
  mov qword ptr [rbp - 40], rcx
  mov rcx, qword ptr [rbp - 16]
  mov qword ptr [rbp - 48], rcx
  mov rdx, qword ptr [rbp - 24]
  mov rdi, qword ptr [rbp - 40]
  mov rsi, qword ptr [rbp - 48]
  call int* std::uninitialized_copy<std::move_iterator<int*>, int*>(std::move_iterator<int*>, std::move_iterator<int*>, int*)
  add rsp, 48
  pop rbp
  ret
std::move_iterator<int*> std::__make_move_if_noexcept_iterator<int, std::move_iterator<int*> >(int*): # @std::move_iterator<int*> std::__make_move_if_noexcept_iterator<int, std::move_iterator<int*> >(int*)
  push rbp
  mov rbp, rsp
  sub rsp, 16
  lea rax, [rbp - 8]
  mov qword ptr [rbp - 16], rdi
  mov rsi, qword ptr [rbp - 16]
  mov rdi, rax
  call std::move_iterator<int*>::move_iterator(int*)
  mov rax, qword ptr [rbp - 8]
  add rsp, 16
  pop rbp
  ret
int* std::uninitialized_copy<std::move_iterator<int*>, int*>(std::move_iterator<int*>, std::move_iterator<int*>, int*): # @int* std::uninitialized_copy<std::move_iterator<int*>, int*>(std::move_iterator<int*>, std::move_iterator<int*>, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov byte ptr [rbp - 25], 1
  mov rdx, qword ptr [rbp - 8]
  mov qword ptr [rbp - 40], rdx
  mov rdx, qword ptr [rbp - 16]
  mov qword ptr [rbp - 48], rdx
  mov rdx, qword ptr [rbp - 24]
  mov rdi, qword ptr [rbp - 40]
  mov rsi, qword ptr [rbp - 48]
  call int* std::__uninitialized_copy<true>::__uninit_copy<std::move_iterator<int*>, int*>(std::move_iterator<int*>, std::move_iterator<int*>, int*)
  add rsp, 48
  pop rbp
  ret
int* std::__uninitialized_copy<true>::__uninit_copy<std::move_iterator<int*>, int*>(std::move_iterator<int*>, std::move_iterator<int*>, int*): # @int* std::__uninitialized_copy<true>::__uninit_copy<std::move_iterator<int*>, int*>(std::move_iterator<int*>, std::move_iterator<int*>, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 8]
  mov qword ptr [rbp - 32], rdx
  mov rdx, qword ptr [rbp - 16]
  mov qword ptr [rbp - 40], rdx
  mov rdx, qword ptr [rbp - 24]
  mov rdi, qword ptr [rbp - 32]
  mov rsi, qword ptr [rbp - 40]
  call int* std::copy<std::move_iterator<int*>, int*>(std::move_iterator<int*>, std::move_iterator<int*>, int*)
  add rsp, 48
  pop rbp
  ret
int* std::copy<std::move_iterator<int*>, int*>(std::move_iterator<int*>, std::move_iterator<int*>, int*): # @int* std::copy<std::move_iterator<int*>, int*>(std::move_iterator<int*>, std::move_iterator<int*>, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 8]
  mov qword ptr [rbp - 32], rdx
  mov rdi, qword ptr [rbp - 32]
  call decltype (__miter_base(({parm#1}.base)())) std::__miter_base<int*>(std::move_iterator<int*>)
  mov rdx, qword ptr [rbp - 16]
  mov qword ptr [rbp - 40], rdx
  mov rdi, qword ptr [rbp - 40]
  mov qword ptr [rbp - 48], rax # 8-byte Spill
  call decltype (__miter_base(({parm#1}.base)())) std::__miter_base<int*>(std::move_iterator<int*>)
  mov rdx, qword ptr [rbp - 24]
  mov rdi, qword ptr [rbp - 48] # 8-byte Reload
  mov rsi, rax
  call int* std::__copy_move_a2<true, int*, int*>(int*, int*, int*)
  add rsp, 48
  pop rbp
  ret
int* std::__copy_move_a2<true, int*, int*>(int*, int*, int*): # @int* std::__copy_move_a2<true, int*, int*>(int*, int*, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdi, qword ptr [rbp - 8]
  call int* std::__niter_base<int*>(int*)
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 32], rax # 8-byte Spill
  call int* std::__niter_base<int*>(int*)
  mov rdi, qword ptr [rbp - 24]
  mov qword ptr [rbp - 40], rax # 8-byte Spill
  call int* std::__niter_base<int*>(int*)
  mov rdi, qword ptr [rbp - 32] # 8-byte Reload
  mov rsi, qword ptr [rbp - 40] # 8-byte Reload
  mov rdx, rax
  call int* std::__copy_move_a<true, int*, int*>(int*, int*, int*)
  add rsp, 48
  pop rbp
  ret
decltype (__miter_base(({parm#1}.base)())) std::__miter_base<int*>(std::move_iterator<int*>): # @decltype (__miter_base(({parm#1}.base)())) std::__miter_base<int*>(std::move_iterator<int*>)
  push rbp
  mov rbp, rsp
  sub rsp, 16
  lea rax, [rbp - 8]
  mov qword ptr [rbp - 8], rdi
  mov rdi, rax
  call std::move_iterator<int*>::base() const
  mov rdi, rax
  call int* std::__miter_base<int*>(int*)
  add rsp, 16
  pop rbp
  ret
int* std::__copy_move_a<true, int*, int*>(int*, int*, int*): # @int* std::__copy_move_a<true, int*, int*>(int*, int*, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov byte ptr [rbp - 25], 1
  mov rdi, qword ptr [rbp - 8]
  mov rsi, qword ptr [rbp - 16]
  mov rdx, qword ptr [rbp - 24]
  call int* std::__copy_move<true, true, std::random_access_iterator_tag>::__copy_m<int>(int const*, int const*, int*)
  add rsp, 32
  pop rbp
  ret
int* std::__niter_base<int*>(int*): # @int* std::__niter_base<int*>(int*)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rax, qword ptr [rbp - 8]
  pop rbp
  ret
int* std::__copy_move<true, true, std::random_access_iterator_tag>::__copy_m<int>(int const*, int const*, int*): # @int* std::__copy_move<true, true, std::random_access_iterator_tag>::__copy_m<int>(int const*, int const*, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 16]
  mov rsi, qword ptr [rbp - 8]
  sub rdx, rsi
  sar rdx, 2
  mov qword ptr [rbp - 32], rdx
  cmp qword ptr [rbp - 32], 0
  je .LBB62_2
  mov rax, qword ptr [rbp - 24]
  mov rcx, qword ptr [rbp - 8]
  mov rdx, qword ptr [rbp - 32]
  shl rdx, 2
  mov rdi, rax
  mov rsi, rcx
  call memmove
  mov qword ptr [rbp - 40], rax # 8-byte Spill
.LBB62_2:
  mov rax, qword ptr [rbp - 24]
  mov rcx, qword ptr [rbp - 32]
  shl rcx, 2
  add rax, rcx
  add rsp, 48
  pop rbp
  ret
int* std::__miter_base<int*>(int*): # @int* std::__miter_base<int*>(int*)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rax, qword ptr [rbp - 8]
  pop rbp
  ret
std::move_iterator<int*>::base() const: # @std::move_iterator<int*>::base() const
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, qword ptr [rdi]
  pop rbp
  ret
std::move_iterator<int*>::move_iterator(int*): # @std::move_iterator<int*>::move_iterator(int*)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rsi, qword ptr [rbp - 8]
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rsi], rdi
  pop rbp
  ret
void __gnu_cxx::new_allocator<int>::destroy<int>(int*): # @void __gnu_cxx::new_allocator<int>::destroy<int>(int*)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  pop rbp
  ret
__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator-(long) const: # @__gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::operator-(long) const
  push rbp
  mov rbp, rsp
  sub rsp, 32
  lea rax, [rbp - 8]
  lea rcx, [rbp - 32]
  xor edx, edx
  mov r8d, edx
  mov qword ptr [rbp - 16], rdi
  mov qword ptr [rbp - 24], rsi
  mov rsi, qword ptr [rbp - 16]
  mov rsi, qword ptr [rsi]
  sub r8, qword ptr [rbp - 24]
  shl r8, 2
  add rsi, r8
  mov qword ptr [rbp - 32], rsi
  mov rdi, rax
  mov rsi, rcx
  call __gnu_cxx::__normal_iterator<int*, std::vector<int, std::allocator<int> > >::__normal_iterator(int* const&)
  mov rax, qword ptr [rbp - 8]
  add rsp, 32
  pop rbp
  ret
__gnu_cxx::__alloc_traits<std::allocator<int> >::_S_select_on_copy(std::allocator<int> const&): # @__gnu_cxx::__alloc_traits<std::allocator<int> >::_S_select_on_copy(std::allocator<int> const&)
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov rax, rdi
  mov qword ptr [rbp - 8], rsi
  mov rsi, qword ptr [rbp - 8]
  mov qword ptr [rbp - 16], rax # 8-byte Spill
  call std::allocator_traits<std::allocator<int> >::select_on_container_copy_construction(std::allocator<int> const&)
  mov rax, qword ptr [rbp - 16] # 8-byte Reload
  add rsp, 16
  pop rbp
  ret
std::_Vector_base<int, std::allocator<int> >::_Vector_base(unsigned long, std::allocator<int> const&): # @std::_Vector_base<int, std::allocator<int> >::_Vector_base(unsigned long, std::allocator<int> const&)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 8]
  mov rsi, qword ptr [rbp - 24]
  mov rdi, rdx
  mov qword ptr [rbp - 48], rdx # 8-byte Spill
  call std::_Vector_base<int, std::allocator<int> >::_Vector_impl::_Vector_impl(std::allocator<int> const&)
  mov rsi, qword ptr [rbp - 16]
  mov rdi, qword ptr [rbp - 48] # 8-byte Reload
  call std::_Vector_base<int, std::allocator<int> >::_M_create_storage(unsigned long)
  jmp .LBB69_1
.LBB69_1:
  add rsp, 48
  pop rbp
  ret
  mov ecx, edx
  mov qword ptr [rbp - 32], rax
  mov dword ptr [rbp - 36], ecx
  mov rdi, qword ptr [rbp - 48] # 8-byte Reload
  call std::_Vector_base<int, std::allocator<int> >::_Vector_impl::~_Vector_impl()
  mov rdi, qword ptr [rbp - 32]
  call _Unwind_Resume
std::allocator<int>::~allocator(): # @std::allocator<int>::~allocator()
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call __gnu_cxx::new_allocator<int>::~new_allocator()
  add rsp, 16
  pop rbp
  ret
int* std::__uninitialized_copy_a<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*, int>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*, std::allocator<int>&): # @int* std::__uninitialized_copy_a<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*, int>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*, std::allocator<int>&)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov qword ptr [rbp - 32], rcx
  mov rcx, qword ptr [rbp - 8]
  mov qword ptr [rbp - 40], rcx
  mov rcx, qword ptr [rbp - 16]
  mov qword ptr [rbp - 48], rcx
  mov rdx, qword ptr [rbp - 24]
  mov rdi, qword ptr [rbp - 40]
  mov rsi, qword ptr [rbp - 48]
  call int* std::uninitialized_copy<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*)
  add rsp, 48
  pop rbp
  ret
std::vector<int, std::allocator<int> >::begin() const: # @std::vector<int, std::allocator<int> >::begin() const
  push rbp
  mov rbp, rsp
  sub rsp, 32
  lea rax, [rbp - 8]
  lea rsi, [rbp - 24]
  mov qword ptr [rbp - 16], rdi
  mov rdi, qword ptr [rbp - 16]
  mov rdi, qword ptr [rdi]
  mov qword ptr [rbp - 24], rdi
  mov rdi, rax
  call __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >::__normal_iterator(int const* const&)
  mov rax, qword ptr [rbp - 8]
  add rsp, 32
  pop rbp
  ret
std::vector<int, std::allocator<int> >::end() const: # @std::vector<int, std::allocator<int> >::end() const
  push rbp
  mov rbp, rsp
  sub rsp, 32
  lea rax, [rbp - 8]
  lea rsi, [rbp - 24]
  mov qword ptr [rbp - 16], rdi
  mov rdi, qword ptr [rbp - 16]
  mov rdi, qword ptr [rdi + 8]
  mov qword ptr [rbp - 24], rdi
  mov rdi, rax
  call __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >::__normal_iterator(int const* const&)
  mov rax, qword ptr [rbp - 8]
  add rsp, 32
  pop rbp
  ret
std::allocator_traits<std::allocator<int> >::select_on_container_copy_construction(std::allocator<int> const&): # @std::allocator_traits<std::allocator<int> >::select_on_container_copy_construction(std::allocator<int> const&)
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov rax, rdi
  mov qword ptr [rbp - 8], rsi
  mov rsi, qword ptr [rbp - 8]
  mov qword ptr [rbp - 16], rax # 8-byte Spill
  call std::allocator<int>::allocator(std::allocator<int> const&)
  mov rax, qword ptr [rbp - 16] # 8-byte Reload
  add rsp, 16
  pop rbp
  ret
std::allocator<int>::allocator(std::allocator<int> const&): # @std::allocator<int>::allocator(std::allocator<int> const&)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rsi, qword ptr [rbp - 8]
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 24], rdi # 8-byte Spill
  mov rdi, rsi
  mov rsi, qword ptr [rbp - 24] # 8-byte Reload
  call __gnu_cxx::new_allocator<int>::new_allocator(__gnu_cxx::new_allocator<int> const&)
  add rsp, 32
  pop rbp
  ret
__gnu_cxx::new_allocator<int>::new_allocator(__gnu_cxx::new_allocator<int> const&): # @__gnu_cxx::new_allocator<int>::new_allocator(__gnu_cxx::new_allocator<int> const&)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  pop rbp
  ret
std::_Vector_base<int, std::allocator<int> >::_Vector_impl::_Vector_impl(std::allocator<int> const&): # @std::_Vector_base<int, std::allocator<int> >::_Vector_impl::_Vector_impl(std::allocator<int> const&)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rsi, qword ptr [rbp - 8]
  mov rdi, rsi
  mov rax, qword ptr [rbp - 16]
  mov qword ptr [rbp - 24], rsi # 8-byte Spill
  mov rsi, rax
  call std::allocator<int>::allocator(std::allocator<int> const&)
  mov rax, qword ptr [rbp - 24] # 8-byte Reload
  mov qword ptr [rax], 0
  mov qword ptr [rax + 8], 0
  mov qword ptr [rax + 16], 0
  add rsp, 32
  pop rbp
  ret
std::_Vector_base<int, std::allocator<int> >::_M_create_storage(unsigned long): # @std::_Vector_base<int, std::allocator<int> >::_M_create_storage(unsigned long)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rsi, qword ptr [rbp - 8]
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 24], rdi # 8-byte Spill
  mov rdi, rsi
  mov rax, qword ptr [rbp - 24] # 8-byte Reload
  mov qword ptr [rbp - 32], rsi # 8-byte Spill
  mov rsi, rax
  call std::_Vector_base<int, std::allocator<int> >::_M_allocate(unsigned long)
  mov rsi, qword ptr [rbp - 32] # 8-byte Reload
  mov qword ptr [rsi], rax
  mov rax, qword ptr [rsi]
  mov qword ptr [rsi + 8], rax
  mov rax, qword ptr [rsi]
  mov rdi, qword ptr [rbp - 16]
  shl rdi, 2
  add rax, rdi
  mov qword ptr [rsi + 16], rax
  add rsp, 32
  pop rbp
  ret
int* std::uninitialized_copy<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*): # @int* std::uninitialized_copy<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov byte ptr [rbp - 25], 1
  mov rdx, qword ptr [rbp - 8]
  mov qword ptr [rbp - 40], rdx
  mov rdx, qword ptr [rbp - 16]
  mov qword ptr [rbp - 48], rdx
  mov rdx, qword ptr [rbp - 24]
  mov rdi, qword ptr [rbp - 40]
  mov rsi, qword ptr [rbp - 48]
  call int* std::__uninitialized_copy<true>::__uninit_copy<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*)
  add rsp, 48
  pop rbp
  ret
int* std::__uninitialized_copy<true>::__uninit_copy<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*): # @int* std::__uninitialized_copy<true>::__uninit_copy<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 8]
  mov qword ptr [rbp - 32], rdx
  mov rdx, qword ptr [rbp - 16]
  mov qword ptr [rbp - 40], rdx
  mov rdx, qword ptr [rbp - 24]
  mov rdi, qword ptr [rbp - 32]
  mov rsi, qword ptr [rbp - 40]
  call int* std::copy<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*)
  add rsp, 48
  pop rbp
  ret
int* std::copy<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*): # @int* std::copy<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 64
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 8]
  mov qword ptr [rbp - 40], rdx
  mov rdi, qword ptr [rbp - 40]
  call __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > > std::__miter_base<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > > >(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >)
  mov qword ptr [rbp - 32], rax
  mov rax, qword ptr [rbp - 16]
  mov qword ptr [rbp - 56], rax
  mov rdi, qword ptr [rbp - 56]
  call __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > > std::__miter_base<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > > >(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >)
  mov qword ptr [rbp - 48], rax
  mov rdx, qword ptr [rbp - 24]
  mov rdi, qword ptr [rbp - 32]
  mov rsi, qword ptr [rbp - 48]
  call int* std::__copy_move_a2<false, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*)
  add rsp, 64
  pop rbp
  ret
int* std::__copy_move_a2<false, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*): # @int* std::__copy_move_a2<false, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*>(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 64
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 8]
  mov qword ptr [rbp - 32], rdx
  mov rdi, qword ptr [rbp - 32]
  call int const* std::__niter_base<int const*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >)
  mov rdx, qword ptr [rbp - 16]
  mov qword ptr [rbp - 40], rdx
  mov rdi, qword ptr [rbp - 40]
  mov qword ptr [rbp - 48], rax # 8-byte Spill
  call int const* std::__niter_base<int const*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >)
  mov rdi, qword ptr [rbp - 24]
  mov qword ptr [rbp - 56], rax # 8-byte Spill
  call int* std::__niter_base<int*>(int*)
  mov rdi, qword ptr [rbp - 48] # 8-byte Reload
  mov rsi, qword ptr [rbp - 56] # 8-byte Reload
  mov rdx, rax
  call int* std::__copy_move_a<false, int const*, int*>(int const*, int const*, int*)
  add rsp, 64
  pop rbp
  ret
__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > > std::__miter_base<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > > >(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >): # @__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > > std::__miter_base<__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > > >(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 16], rdi
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 8], rdi
  mov rax, qword ptr [rbp - 8]
  pop rbp
  ret
int* std::__copy_move_a<false, int const*, int*>(int const*, int const*, int*): # @int* std::__copy_move_a<false, int const*, int*>(int const*, int const*, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov byte ptr [rbp - 25], 1
  mov rdi, qword ptr [rbp - 8]
  mov rsi, qword ptr [rbp - 16]
  mov rdx, qword ptr [rbp - 24]
  call int* std::__copy_move<false, true, std::random_access_iterator_tag>::__copy_m<int>(int const*, int const*, int*)
  add rsp, 32
  pop rbp
  ret
int const* std::__niter_base<int const*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >): # @int const* std::__niter_base<int const*, std::vector<int, std::allocator<int> > >(__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >)
  push rbp
  mov rbp, rsp
  sub rsp, 16
  lea rax, [rbp - 8]
  mov qword ptr [rbp - 8], rdi
  mov rdi, rax
  call __gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >::base() const
  mov rax, qword ptr [rax]
  add rsp, 16
  pop rbp
  ret
int* std::__copy_move<false, true, std::random_access_iterator_tag>::__copy_m<int>(int const*, int const*, int*): # @int* std::__copy_move<false, true, std::random_access_iterator_tag>::__copy_m<int>(int const*, int const*, int*)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdx, qword ptr [rbp - 16]
  mov rsi, qword ptr [rbp - 8]
  sub rdx, rsi
  sar rdx, 2
  mov qword ptr [rbp - 32], rdx
  cmp qword ptr [rbp - 32], 0
  je .LBB86_2
  mov rax, qword ptr [rbp - 24]
  mov rcx, qword ptr [rbp - 8]
  mov rdx, qword ptr [rbp - 32]
  shl rdx, 2
  mov rdi, rax
  mov rsi, rcx
  call memmove
  mov qword ptr [rbp - 40], rax # 8-byte Spill
.LBB86_2:
  mov rax, qword ptr [rbp - 24]
  mov rcx, qword ptr [rbp - 32]
  shl rcx, 2
  add rax, rcx
  add rsp, 48
  pop rbp
  ret
__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >::base() const: # @__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >::base() const
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rax, qword ptr [rbp - 8]
  pop rbp
  ret
__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >::__normal_iterator(int const* const&): # @__gnu_cxx::__normal_iterator<int const*, std::vector<int, std::allocator<int> > >::__normal_iterator(int const* const&)
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rsi, qword ptr [rbp - 8]
  mov rdi, qword ptr [rbp - 16]
  mov rdi, qword ptr [rdi]
  mov qword ptr [rsi], rdi
  pop rbp
  ret
std::_Function_base::_Function_base(): # @std::_Function_base::_Function_base()
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov qword ptr [rdi + 16], 0
  pop rbp
  ret
bool std::_Function_base::_Base_manager<main::$_0>::_M_not_empty_function<main::$_0>(main::$_0 const&): # @"bool std::_Function_base::_Base_manager<main::$_0>::_M_not_empty_function<main::$_0>(main::$_0 const&)"
  push rbp
  mov rbp, rsp
  mov al, 1
  mov qword ptr [rbp - 8], rdi
  and al, 1
  movzx eax, al
  pop rbp
  ret
std::_Function_base::_Base_manager<main::$_0>::_M_init_functor(std::_Any_data&, main::$_0&&): # @"std::_Function_base::_Base_manager<main::$_0>::_M_init_functor(std::_Any_data&, main::$_0&&)"
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov rdi, qword ptr [rbp - 8]
  mov rsi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 32], rdi # 8-byte Spill
  mov rdi, rsi
  call std::remove_reference<main::$_0&>::type&& std::move<main::$_0&>(main::$_0&)
  mov rdi, qword ptr [rbp - 32] # 8-byte Reload
  mov rsi, rax
  call std::_Function_base::_Base_manager<main::$_0>::_M_init_functor(std::_Any_data&, main::$_0&&, std::integral_constant<bool, true>)
  add rsp, 32
  pop rbp
  ret
std::remove_reference<main::$_0&>::type&& std::move<main::$_0&>(main::$_0&): # @"std::remove_reference<main::$_0&>::type&& std::move<main::$_0&>(main::$_0&)"
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rax, qword ptr [rbp - 8]
  pop rbp
  ret
std::_Function_handler<int (int, int), main::$_0>::_M_invoke(std::_Any_data const&, int&&, int&&): # @"std::_Function_handler<int (int, int), main::$_0>::_M_invoke(std::_Any_data const&, int&&, int&&)"
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov rdi, qword ptr [rbp - 8]
  call std::_Function_base::_Base_manager<main::$_0>::_M_get_pointer(std::_Any_data const&)
  mov rdi, qword ptr [rbp - 16]
  mov qword ptr [rbp - 32], rax # 8-byte Spill
  call int&& std::forward<int>(std::remove_reference<int>::type&)
  mov esi, dword ptr [rax]
  mov rdi, qword ptr [rbp - 24]
  mov dword ptr [rbp - 36], esi # 4-byte Spill
  call int&& std::forward<int>(std::remove_reference<int>::type&)
  mov edx, dword ptr [rax]
  mov rdi, qword ptr [rbp - 32] # 8-byte Reload
  mov esi, dword ptr [rbp - 36] # 4-byte Reload
  call main::$_0::operator()(int, int) const
  add rsp, 48
  pop rbp
  ret
std::_Function_base::_Base_manager<main::$_0>::_M_manager(std::_Any_data&, std::_Any_data const&, std::_Manager_operation): # @"std::_Function_base::_Base_manager<main::$_0>::_M_manager(std::_Any_data&, std::_Any_data const&, std::_Manager_operation)"
  push rbp
  mov rbp, rsp
  sub rsp, 64
  mov qword ptr [rbp - 8], rdi
  mov qword ptr [rbp - 16], rsi
  mov dword ptr [rbp - 20], edx
  mov edx, dword ptr [rbp - 20]
  mov esi, edx
  mov rdi, rsi
  sub rdi, 3
  mov qword ptr [rbp - 40], rsi # 8-byte Spill
  mov qword ptr [rbp - 48], rdi # 8-byte Spill
  ja .LBB94_5
  mov rax, qword ptr [rbp - 40] # 8-byte Reload
  mov rcx, qword ptr [8*rax + .LJTI94_0]
  jmp rcx
.LBB94_1:
  mov rdi, qword ptr [rbp - 8]
  call std::type_info const*& std::_Any_data::_M_access<std::type_info const*>()
  movabs rdi, typeinfo for main::$_0
  mov qword ptr [rax], rdi
  jmp .LBB94_5
.LBB94_2:
  mov rdi, qword ptr [rbp - 16]
  call std::_Function_base::_Base_manager<main::$_0>::_M_get_pointer(std::_Any_data const&)
  mov rdi, qword ptr [rbp - 8]
  mov qword ptr [rbp - 56], rax # 8-byte Spill
  call main::$_0*& std::_Any_data::_M_access<main::$_0*>()
  mov rdi, qword ptr [rbp - 56] # 8-byte Reload
  mov qword ptr [rax], rdi
  jmp .LBB94_5
.LBB94_3:
  mov rdi, qword ptr [rbp - 8]
  mov rsi, qword ptr [rbp - 16]
  call std::_Function_base::_Base_manager<main::$_0>::_M_clone(std::_Any_data&, std::_Any_data const&, std::integral_constant<bool, true>)
  jmp .LBB94_5
.LBB94_4:
  mov rdi, qword ptr [rbp - 8]
  call std::_Function_base::_Base_manager<main::$_0>::_M_destroy(std::_Any_data&, std::integral_constant<bool, true>)
.LBB94_5:
  xor eax, eax
  mov cl, al
  and cl, 1
  movzx eax, cl
  add rsp, 64
  pop rbp
  ret
.LJTI94_0:
  .quad .LBB94_1
  .quad .LBB94_2
  .quad .LBB94_3
  .quad .LBB94_4
std::_Function_base::_Base_manager<main::$_0>::_M_init_functor(std::_Any_data&, main::$_0&&, std::integral_constant<bool, true>): # @"std::_Function_base::_Base_manager<main::$_0>::_M_init_functor(std::_Any_data&, main::$_0&&, std::integral_constant<bool, true>)"
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 16], rdi
  mov qword ptr [rbp - 24], rsi
  mov rdi, qword ptr [rbp - 16]
  call std::_Any_data::_M_access()
  mov rdi, qword ptr [rbp - 24]
  mov qword ptr [rbp - 32], rax # 8-byte Spill
  call std::remove_reference<main::$_0&>::type&& std::move<main::$_0&>(main::$_0&)
  mov qword ptr [rbp - 40], rax # 8-byte Spill
  add rsp, 48
  pop rbp
  ret
std::_Any_data::_M_access(): # @std::_Any_data::_M_access()
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, rdi
  pop rbp
  ret
std::_Function_base::_Base_manager<main::$_0>::_M_get_pointer(std::_Any_data const&): # @"std::_Function_base::_Base_manager<main::$_0>::_M_get_pointer(std::_Any_data const&)"
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call main::$_0 const& std::_Any_data::_M_access<main::$_0>() const
  mov rdi, rax
  call main::$_0 const* std::__addressof<main::$_0 const>(main::$_0 const&)
  mov qword ptr [rbp - 16], rax
  mov rax, qword ptr [rbp - 16]
  add rsp, 16
  pop rbp
  ret
main::$_0::operator()(int, int) const: # @"main::$_0::operator()(int, int) const"
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov dword ptr [rbp - 12], esi
  mov dword ptr [rbp - 16], edx
  mov edx, dword ptr [rbp - 12]
  add edx, dword ptr [rbp - 16]
  mov eax, edx
  pop rbp
  ret
main::$_0 const* std::__addressof<main::$_0 const>(main::$_0 const&): # @"main::$_0 const* std::__addressof<main::$_0 const>(main::$_0 const&)"
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rax, qword ptr [rbp - 8]
  pop rbp
  ret
main::$_0 const& std::_Any_data::_M_access<main::$_0>() const: # @"main::$_0 const& std::_Any_data::_M_access<main::$_0>() const"
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::_Any_data::_M_access() const
  add rsp, 16
  pop rbp
  ret
std::_Any_data::_M_access() const: # @std::_Any_data::_M_access() const
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, rdi
  pop rbp
  ret
std::type_info const*& std::_Any_data::_M_access<std::type_info const*>(): # @std::type_info const*& std::_Any_data::_M_access<std::type_info const*>()
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::_Any_data::_M_access()
  add rsp, 16
  pop rbp
  ret
main::$_0*& std::_Any_data::_M_access<main::$_0*>(): # @"main::$_0*& std::_Any_data::_M_access<main::$_0*>()"
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::_Any_data::_M_access()
  add rsp, 16
  pop rbp
  ret
std::_Function_base::_Base_manager<main::$_0>::_M_clone(std::_Any_data&, std::_Any_data const&, std::integral_constant<bool, true>): # @"std::_Function_base::_Base_manager<main::$_0>::_M_clone(std::_Any_data&, std::_Any_data const&, std::integral_constant<bool, true>)"
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov qword ptr [rbp - 16], rdi
  mov qword ptr [rbp - 24], rsi
  mov rdi, qword ptr [rbp - 16]
  call std::_Any_data::_M_access()
  mov rdi, qword ptr [rbp - 24]
  mov qword ptr [rbp - 32], rax # 8-byte Spill
  call main::$_0 const& std::_Any_data::_M_access<main::$_0>() const
  mov qword ptr [rbp - 40], rax # 8-byte Spill
  add rsp, 48
  pop rbp
  ret
std::_Function_base::_Base_manager<main::$_0>::_M_destroy(std::_Any_data&, std::integral_constant<bool, true>): # @"std::_Function_base::_Base_manager<main::$_0>::_M_destroy(std::_Any_data&, std::integral_constant<bool, true>)"
  push rbp
  mov rbp, rsp
  sub rsp, 32
  mov qword ptr [rbp - 16], rdi
  mov rdi, qword ptr [rbp - 16]
  call main::$_0& std::_Any_data::_M_access<main::$_0>()
  mov qword ptr [rbp - 24], rax # 8-byte Spill
  add rsp, 32
  pop rbp
  ret
main::$_0& std::_Any_data::_M_access<main::$_0>(): # @"main::$_0& std::_Any_data::_M_access<main::$_0>()"
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::_Any_data::_M_access()
  add rsp, 16
  pop rbp
  ret
.L.str:
  .asciz "vector::_M_realloc_insert"

typeinfo name for main::$_0:
  .asciz "Z4mainE3$_0"

typeinfo for main::$_0:
  .quad vtable for __cxxabiv1::__class_type_info+16
  .quad typeinfo name for main::$_0
```

When all the standard library code is removed, this gives raw of `60` LOC for C program and `41 + 72 = 113` LOC for C++ program.

Now, what will happen if we write the same code but in different languages?

Remember functional programming was introduced in Java 8?

```java
import java.util.*;
import java.lang.*;
import java.io.*;
import java.util.function.*;

public class Sample {
  public static void main (String[] args) throws java.lang.Exception {
    Function<Integer, Integer> f1 = n -> n * 2;
    Function<Integer, Integer> f2 = n -> n * n;
    Function<Integer, Integer> composition = f1.andThen(f2);

    System.out.printf("(2 * n * n)(3) = %d\n", composition.apply(3));
  }
}
```

is compiled into something like this:

```java
public class Sample {
  public Sample();
    Code:
       0: aload_0
       1: invokespecial #1                  // Method java/lang/Object."<init>":()V
       4: return

  public static void main(java.lang.String[]) throws java.lang.Exception;
    Code:
       0: invokedynamic #2,  0              // InvokeDynamic #0:apply:()Ljava/util/function/Function;
       5: astore_1
       6: invokedynamic #3,  0              // InvokeDynamic #1:apply:()Ljava/util/function/Function;
      11: astore_2
      12: aload_1
      13: aload_2
      14: invokeinterface #4,  2            // InterfaceMethod java/util/function/Function.andThen:(Ljava/util/function/Function;)Ljava/util/function/Function;
      19: astore_3
      20: getstatic     #5                  // Field java/lang/System.out:Ljava/io/PrintStream;
      23: ldc           #6                  // String (2 * n * n)(3) = %d\n
      25: iconst_1
      26: anewarray     #7                  // class java/lang/Object
      29: dup
      30: iconst_0
      31: aload_3
      32: iconst_3
      33: invokestatic  #8                  // Method java/lang/Integer.valueOf:(I)Ljava/lang/Integer;
      36: invokeinterface #9,  2            // InterfaceMethod java/util/function/Function.apply:(Ljava/lang/Object;)Ljava/lang/Object;
      41: aastore
      42: invokevirtual #10                 // Method java/io/PrintStream.printf:(Ljava/lang/String;[Ljava/lang/Object;)Ljava/io/PrintStream;
      45: pop
      46: return
}
```


```cpp
auto f1 = [](auto a, auto b) { return a + b; };

auto f2 = [](std::vector<int> a, auto f, auto init) { auto res = init; for (auto i : a) res = f(i, res); return res; };
```

Does this demonstrate the first-class functions in C++? Does it compy with functional paradigm?

```c
int reduce(int n, int *a, int (*f)(int a, int b), int init) {
  int res = init;

  for (int i = 0; i < n; ++i) {
    res = f(res, a[i]);
  }

  return res;
}
```

This last example will be compiled (using Clang 5.0.0) into this:

```asm
reduce(int, int*, int (*)(int, int), int): # @reduce(int, int*, int (*)(int, int), int)
  push rbp
  mov rbp, rsp
  sub rsp, 48
  mov dword ptr [rbp - 4], edi
  mov qword ptr [rbp - 16], rsi
  mov qword ptr [rbp - 24], rdx
  mov dword ptr [rbp - 28], ecx
  mov ecx, dword ptr [rbp - 28]
  mov dword ptr [rbp - 32], ecx
  mov dword ptr [rbp - 36], 0
.LBB0_1: # =>This Inner Loop Header: Depth=1
  mov eax, dword ptr [rbp - 36]
  cmp eax, dword ptr [rbp - 4]
  jge .LBB0_4
  mov rax, qword ptr [rbp - 24]
  mov edi, dword ptr [rbp - 32]
  mov rcx, qword ptr [rbp - 16]
  movsxd rdx, dword ptr [rbp - 36]
  mov esi, dword ptr [rcx + 4*rdx]
  call rax
  mov dword ptr [rbp - 32], eax
  mov eax, dword ptr [rbp - 36]
  add eax, 1
  mov dword ptr [rbp - 36], eax
  jmp .LBB0_1
.LBB0_4:
  mov eax, dword ptr [rbp - 32]
  add rsp, 48
  pop rbp
  ret
```

With `-O2` option:

```asm
reduce(int, int*, int (*)(int, int), int): # @reduce(int, int*, int (*)(int, int), int)
  push r15
  push r14
  push rbx
  mov eax, ecx
  mov r14, rdx
  mov rbx, rsi
  test edi, edi
  jle .LBB0_3
  mov r15d, edi
.LBB0_2: # =>This Inner Loop Header: Depth=1
  mov esi, dword ptr [rbx]
  mov edi, eax
  call r14
  add rbx, 4
  dec r15
  jne .LBB0_2
.LBB0_3:
  pop rbx
  pop r14
  pop r15
  ret
```

Consider this C++17 code for the very same function:

```cpp
#include <vector>
#include <functional>

int reduce(std::vector<int> a, std::function<int (int, int)> f, int init) {
  int res = init;

  for (int i : a) {
    res = f(i, res);
  }

  return res;
}
```

its output (with Clang 5.0.0 and `-std=c++1z -O2` options:

```asm
reduce(std::vector<int, std::allocator<int> >, std::function<int (int, int)>, int): # @reduce(std::vector<int, std::allocator<int> >, std::function<int (int, int)>, int)
  push r15
  push r14
  push r13
  push r12
  push rbx
  sub rsp, 16
  mov eax, edx
  mov r12, rsi
  mov rbx, qword ptr [rdi]
  mov r13, qword ptr [rdi + 8]
  cmp rbx, r13
  je .LBB0_4
  lea r14, [rsp + 12]
  lea r15, [rsp + 8]
.LBB0_2: # =>This Inner Loop Header: Depth=1
  mov ecx, dword ptr [rbx]
  mov dword ptr [rsp + 12], ecx
  mov dword ptr [rsp + 8], eax
  cmp qword ptr [r12 + 16], 0
  je .LBB0_5
  mov rdi, r12
  mov rsi, r14
  mov rdx, r15
  call qword ptr [r12 + 24]
  add rbx, 4
  cmp r13, rbx
  jne .LBB0_2
.LBB0_4:
  add rsp, 16
  pop rbx
  pop r12
  pop r13
  pop r14
  pop r15
  ret
.LBB0_5:
  call std::__throw_bad_function_call()
```

Now consider Haskell example:

```hs
module Example where

sum2 :: [Int] -> Int
sum2 = foldr (+) 0
```

```asm
__stginit_Example:
c10H_str:
  .byte 109
  .byte 97
  .byte 105
  .byte 110
  .byte 0
r10q_closure:
  .quad ghc-prim_GHC.Types_TrNameS_static_info
  .quad c10H_str
c10L_str:
  .byte 69
  .byte 120
  .byte 97
  .byte 109
  .byte 112
  .byte 108
  .byte 101
  .byte 0
r10_closure:
  .quad ghc-prim_GHC.Types_TrNameS_static_info
  .quad c10L_str
Example_$trModule_closure:
  .quad ghc-prim_GHC.Types_Module_static_info
  .quad r10q_closure+1
  .quad r10_closure+1
  .quad 3
s10F_closure:
  .quad ghc-prim_GHC.Types_I#_static_info
  .quad 0
s10E_closure:
  .quad s10E_info
  .quad 0
  .quad 0
  .quad 0
s10E_info:
  leaq -16(%rbp),%rax
  cmpq %r15,%rax
  jb .Lc10Z
  subq $8,%rsp
  movq %r13,%rax
  movq %rbx,%rsi
  movq %rax,%rdi
  xorl %eax,%eax
  call newCAF
  addq $8,%rsp
  testq %rax,%rax
  je .Lc10X
  movq $stg_bh_upd_frame_info,-16(%rbp)
  movq %rax,-8(%rbp)
  movl $base_GHC.Num_$fNumInt_closure,%r14d
  addq $-16,%rbp
  jmp base_GHC.Num_+_info
.Lc10X:
  jmp *(%rbx)
  jmp *-16(%r13)
Example_sum2_closure:
  .quad Example_sum2_info
  .quad 0
  .quad 0
  .quad 0
Example_sum2_info:
  leaq -40(%rbp),%rax
  cmpq %r15,%rax
  jb .Lc11f
  subq $8,%rsp
  movq %r13,%rax
  movq %rbx,%rsi
  movq %rax,%rdi
  xorl %eax,%eax
  call newCAF
  addq $8,%rsp
  testq %rax,%rax
  je .Lc11d
  movq $stg_bh_upd_frame_info,-16(%rbp)
  movq %rax,-8(%rbp)
  movl $base_Data.Foldable_$fFoldable[]_closure,%r14d
  movq $stg_ap_pp_info,-40(%rbp)
  movq $s10E_closure,-32(%rbp)
  movq $s10F_closure+1,-24(%rbp)
  addq $-40,%rbp
  jmp base_Data.Foldable_foldr_info
.Lc11d:
  jmp *(%rbx)
.Lc11f:
  jmp *-16(%r13)
S111_srt:
  .quad base_GHC.Num_$fNumInt_closure
  .quad base_Data.Foldable_$fFoldable[]_closure
  .quad s10E_closure
  .quad s10F_closure
```

and recursive variant, since there are no loops in Haskell:

```hs
module Example where

sum1 :: [Int] -> Int
sum1 (x:xs) = x + sum1 xs
sum1 [] =  0
```

which compiles into

```asm
__stginit_Example:
cHx_str:
  .byte 109
  .byte 97
  .byte 105
  .byte 110
  .byte 0
rxL_closure:
  .quad ghc-prim_GHC.Types_TrNameS_static_info
  .quad cHx_str
cHB_str:
  .byte 69
  .byte 120
  .byte 97
  .byte 109
  .byte 112
  .byte 108
  .byte 101
  .byte 0
ry2_closure:
  .quad ghc-prim_GHC.Types_TrNameS_static_info
  .quad cHB_str
Example_$trModule_closure:
  .quad ghc-prim_GHC.Types_Module_static_info
  .quad rxL_closure+1
  .quad ry2_closure+1
  .quad 3
Example_sum1_closure:
  .quad Example_sum1_info
  .quad 0
sHv_info:
  leaq -16(%rbp),%rax
  cmpq %r15,%rax
  jb .LcI1
  movq $stg_upd_frame_info,-16(%rbp)
  movq %rbx,-8(%rbp)
  movq 16(%rbx),%rax
  movq %rax,%r14
  addq $-16,%rbp
  jmp Example_sum1_info
.LcI1:
  jmp *-16(%r13)
Example_sum1_info:
  leaq -24(%rbp),%rax
  cmpq %r15,%rax
  jb .LcI8
  movq $cHQ_info,-8(%rbp)
  movq %r14,%rbx
  addq $-8,%rbp
  testb $7,%bl
  jne .LcHQ
  jmp *(%rbx)
  .long SIf_srt-(cHQ_info)+0
  .long 0
  .quad 0
  .quad 12884901920
cHQ_info:
.LcHQ:
  movq %rbx,%rax
  andl $7,%eax
  cmpq $1,%rax
  jne .LcI5
  movl $stg_INTLIKE_closure+257,%ebx
  addq $8,%rbp
  jmp *(%rbp)
.LcI5:
  addq $24,%r12
  cmpq 856(%r13),%r12
  ja .LcIe
  movq 6(%rbx),%rax
  movq 14(%rbx),%rbx
  movq $sHv_info,-16(%r12)
  movq %rbx,(%r12)
  leaq -16(%r12),%rbx
  movl $base_GHC.Num_$fNumInt_closure,%r14d
  movq $stg_ap_pp_info,-16(%rbp)
  movq %rax,-8(%rbp)
  movq %rbx,(%rbp)
  addq $-16,%rbp
  jmp base_GHC.Num_+_info
.LcI8:
  movl $Example_sum1_closure,%ebx
  jmp *-8(%r13)
.LcIe:
  movq $24,904(%r13)
  jmp stg_gc_unpt_r1
SIf_srt:
  .quad Example_sum1_closure
  .quad base_GHC.Num_$fNumInt_closure
```

Please note: no compiler options provided!

With `-O2` the last one gives us this:

```asm
__stginit_Example:
cKp_str:
  .byte 109
  .byte 97
  .byte 105
  .byte 110
  .byte 0
Example_$trModule2_closure:
  .quad ghc-prim_GHC.Types_TrNameS_static_info
  .quad cKp_str
cKt_str:
  .byte 69
  .byte 120
  .byte 97
  .byte 109
  .byte 112
  .byte 108
  .byte 101
  .byte 0
Example_$trModule1_closure:
  .quad ghc-prim_GHC.Types_TrNameS_static_info
  .quad cKt_str
Example_$trModule_closure:
  .quad ghc-prim_GHC.Types_Module_static_info
  .quad Example_$trModule2_closure+1
  .quad Example_$trModule1_closure+1
  .quad 3
Example_$wsum1_closure:
  .quad Example_$wsum1_info
Example_$wsum1_info:
  leaq -16(%rbp),%rax
  cmpq %r15,%rax
  jb .LcKW
  movq $cKO_info,-8(%rbp)
  movq %r14,%rbx
  addq $-8,%rbp
  testb $7,%bl
  jne .LcKO
  jmp *(%rbx)
  .quad 65
  .quad 32
cL8_info:
  movq %rbx,%rax
  movq 8(%rbp),%rbx
  addq %rax,%rbx
  addq $16,%rbp
  jmp *(%rbp)
.LcKT:
  movq $cL3_info,-8(%rbp)
  movq 14(%rbx),%rax
  movq 6(%rbx),%rbx
  movq %rax,(%rbp)
  addq $-8,%rbp
  testb $7,%bl
  jne .LcL3
  jmp *(%rbx)
  .quad 1
  .quad 32
cL3_info:
.LcL3:
  movq $cL8_info,(%rbp)
  movq 8(%rbp),%r14
  movq 7(%rbx),%rax
  movq %rax,8(%rbp)
  jmp Example_$wsum1_info
.LcKW:
  movl $Example_$wsum1_closure,%ebx
  jmp *-8(%r13)
  .quad 0
  .quad 32
cKO_info:
.LcKO:
  movq %rbx,%rax
  andl $7,%eax
  cmpq $1,%rax
  jne .LcKT
  xorl %ebx,%ebx
  addq $8,%rbp
  jmp *(%rbp)
Example_sum1_closure:
  .quad Example_sum1_info
Example_sum1_info:
  leaq -8(%rbp),%rax
  cmpq %r15,%rax
  jb .LcLC
  movq $cLw_info,-8(%rbp)
  addq $-8,%rbp
  jmp Example_$wsum1_info
.LcLG:
  movq $16,904(%r13)
  jmp stg_gc_unbx_r1
  .quad 0
  .quad 32
cLw_info:
  addq $16,%r12
  cmpq 856(%r13),%r12
  ja .LcLG
  movq $ghc-prim_GHC.Types_I#_con_info,-8(%r12)
  movq %rbx,(%r12)
  leaq -7(%r12),%rbx
  addq $8,%rbp
  jmp *(%rbp)
.LcLC:
  movl $Example_sum1_closure,%ebx
  jmp *-8(%r13)
```

Effectively, the summing function in Haskell has this Assembly output:

```asm
Example_$wsum1_info:
  leaq -16(%rbp),%rax
  cmpq %r15,%rax
  jb .LcKW
  movq $cKO_info,-8(%rbp)
  movq %r14,%rbx
  addq $-8,%rbp
  testb $7,%bl
  jne .LcKO
  jmp *(%rbx)
  .quad 65
  .quad 32
cL8_info:
  movq %rbx,%rax
  movq 8(%rbp),%rbx
  addq %rax,%rbx
  addq $16,%rbp
  jmp *(%rbp)
.LcKT:
  movq $cL3_info,-8(%rbp)
  movq 14(%rbx),%rax
  movq 6(%rbx),%rbx
  movq %rax,(%rbp)
  addq $-8,%rbp
  testb $7,%bl
  jne .LcL3
  jmp *(%rbx)
  .quad 1
  .quad 32
cL3_info:
.LcL3:
  movq $cL8_info,(%rbp)
  movq 8(%rbp),%r14
  movq 7(%rbx),%rax
  movq %rax,8(%rbp)
  jmp Example_$wsum1_info
.LcKW:
  movl $Example_$wsum1_closure,%ebx
  jmp *-8(%r13)
  .quad 0
  .quad 32
cKO_info:
.LcKO:
  movq %rbx,%rax
  andl $7,%eax
  cmpq $1,%rax
  jne .LcKT
  xorl %ebx,%ebx
  addq $8,%rbp
  jmp *(%rbp)
```

Or, with a more high-level code, CMM (C--, the high-level GHC Assembly-like language):

```hs
Example.sumOverArray_entry() //  [R2]
   { info_tbl: [(cHQ,
                 label: block_cHQ_info
                 rep:StackRep []),
                (cI7,
                 label: Example.sumOverArray_info
                 rep:HeapRep static { Fun {arity: 1 fun_type: ArgSpec 5} })]
     stack_info: arg_space: 8 updfr_space: Just 8
   }
{offset
 cI7:
     _sHr::P64 = R2;
     if ((Sp + 8) - 32 < SpLim) goto cI8; else goto cI9;
 cI8:
     R2 = _sHr::P64;
     R1 = Example.sumOverArray_closure;
     call (stg_gc_fun)(R2, R1) args: 8, res: 0, upd: 8;
 cI9:
     I64[Sp - 8] = cHQ;
     R1 = _sHr::P64;
     Sp = Sp - 8;
     if (R1 & 7 != 0) goto cHQ; else goto cHR;
 cHR:
     call (I64[R1])(R1) returns to cHQ, args: 8, res: 8, upd: 8;
 cHQ:
     _sHs::P64 = R1;
     _cI6::P64 = _sHs::P64 & 7;
     if (_cI6::P64 != 1) goto cI5; else goto cI4;
 cI5:
     Hp = Hp + 24;
     if (Hp > HpLim) goto cIe; else goto cId;
 cIe:
     HpAlloc = 24;
     R1 = _sHs::P64;
     call stg_gc_unpt_r1(R1) returns to cHQ, args: 8, res: 8, upd: 8;
 cId:
     _sHt::P64 = P64[_sHs::P64 + 6];
     _sHu::P64 = P64[_sHs::P64 + 14];
     I64[Hp - 16] = sat_sHv_info;
     P64[Hp] = _sHu::P64;
     _cHW::P64 = Hp - 16;
     R2 = GHC.Num.$fNumInt_closure;
     I64[Sp - 16] = stg_ap_pp_info;
     P64[Sp - 8] = _sHt::P64;
     P64[Sp] = _cHW::P64;
     Sp = Sp - 16;
     call GHC.Num.+_info(R2) args: 32, res: 0, upd: 8;
 cI4:
     R1 = stg_INTLIKE_closure+257;
     Sp = Sp + 8;
     call (P64[Sp])(R1) args: 8, res: 0, upd: 8;
}
```

This makes comparison unfair - we've used recursion in Haskell program and loop in C program so compiler can produce slighty
different assembly code for them. Let's then compare this latest Haskell recursive function with the recursive C function:

```c
int sum(int *a, int n) {
  if (n > 0)
    return a[0] + sum(a + 1, n - 1);

  return 0;
}
```

which yields

```asm
sum(int*, int): # @sum(int*, int)
  push rbp
  mov rbp, rsp
  sub rsp, 32                    # reserve memory for intermediate variables
  mov qword ptr [rbp - 16], rdi  # store function arguments in intermediate variables
  mov dword ptr [rbp - 20], esi
  cmp dword ptr [rbp - 20], 0    # compare `n` and `0`
  jle .LBB0_2                    # if (n <= 0) goto .LBB0_2
  mov rax, qword ptr [rbp - 16]
  mov ecx, dword ptr [rax]       # # store `a[0]` in ECX
  mov rax, qword ptr [rbp - 16]  # # store pointer to `a[0]` in RAX
  add rax, 4                     # store `a + 1` value (pointer to/address of a[1]) in the intermediate variable
  mov edx, dword ptr [rbp - 20]  # # store `n` value in EDX
  sub edx, 1                     # store `n - 1` value in the intermediate variable
  mov rdi, rax
  mov esi, edx
  mov dword ptr [rbp - 24], ecx
  call sum(int*, int)           # recursive call
  mov ecx, dword ptr [rbp - 24] # store recursive call result in the intermediate variable
  add ecx, eax                  # add `a[0]` and the return value of a recursive call
  mov dword ptr [rbp - 4], ecx  # store the sum in the "result" intermediate variable
  jmp .LBB0_3                   # goto .LBB0_3
.LBB0_2:
  mov dword ptr [rbp - 4], 0    # store `0` in the "result" intermediate variable
.LBB0_3:
  mov eax, dword ptr [rbp - 4]  # return the "result" variable' value
  add rsp, 32                   # free the allocated memory
  pop rbp
  ret
```

With `-O2` it produces somewhat more code:

```asm
sum(int*, int): # @sum(int*, int)
  test esi, esi
  jle .LBB0_1
  mov eax, esi
  not eax
  cmp eax, -3
  mov r8d, -2
  cmovg r8d, eax
  lea edx, [r8 + rsi + 1]
  inc rdx
  xor eax, eax
  cmp rdx, 8
  jae .LBB0_4
  mov rcx, rdi
  jmp .LBB0_7
.LBB0_1:
  xor eax, eax
  ret
.LBB0_4:
  add r8d, esi
  add r8d, 2
  and r8d, 7
  sub rdx, r8
  sub esi, edx
  lea rcx, [rdi + 4*rdx]
  add rdi, 16
  pxor xmm0, xmm0
  pxor xmm1, xmm1
.LBB0_5: # =>This Inner Loop Header: Depth=1
  movdqu xmm2, xmmword ptr [rdi - 16]
  paddd xmm0, xmm2
  movdqu xmm2, xmmword ptr [rdi]
  paddd xmm1, xmm2
  add rdi, 32
  add rdx, -8
  jne .LBB0_5
  paddd xmm1, xmm0
  pshufd xmm0, xmm1, 78 # xmm0 = xmm1[2,3,0,1]
  paddd xmm0, xmm1
  pshufd xmm1, xmm0, 229 # xmm1 = xmm0[1,1,2,3]
  paddd xmm1, xmm0
  movd eax, xmm1
  test r8d, r8d
  je .LBB0_9
.LBB0_7:
  inc esi
.LBB0_8: # =>This Inner Loop Header: Depth=1
  add eax, dword ptr [rcx]
  add rcx, 4
  dec esi
  cmp esi, 1
  jg .LBB0_8
.LBB0_9:
  ret
```

Now, we talk about immutability. But the computers we use were not designed with immutability in mind.

What's more important, in a number of cases we have to deal with some data storages (like databases, cookies, etc.), which are mutable by design. So what's the point of having a well-designed immutability in a programming language, when we just throw it out of the window by using the mutable data storage?

## The practical applications

### Case 1: games / heavy-computational applications

Now, let's talk about how we *actually* use FP. Some of its principles are used all around. But what about a real-world
application, made with, say, Haskell?

Assume we have a simple game in C++ with all the physics libraries and graphics engine and sound library being written in C++.
What if *(why in the world a sane person would do this?)* we write it with Haskell?

...

The problem here is that games have to deal with big amounts of data. Would immutability help here?

*TODO: code samples here with memory allocation / deallocation counts*

Each frame we allocate **and** free a huge amount of memory just to handle this and that stuff.

### Case 2: web applications

> Heyyyy! We have Clojure, ClojureScript, Elm, React+Re* and friends! They all are already widely spread!

Welllllll, let's take a closer look: we have to deal with databases, DOM, cookies, browser state & user input **a lot**.
No, <h2>A LOT</h2>. How a typical application in Clojure looks like?

*TODO: an example of Ring/Compojure/Liberator app*

How does the React app looks like?

*TODO: an example of a React component*

Well, this is a more edge-case example. See, we do have a mutable state here - DOM. And in order to update it,
React has to deal with it in a way that it *mutates it* (remove elements, change attributes - *consolidation* algorithms?).

Static typing is great? Well, here's the thing: if you have to deal with JSON - you're doomed.

*TODO: an example of trying to deal with a variable JSON structure*

APIs are everywhere and if one changes - you either handle the change to fill the gaps with default values, or throw an exception. Alternatively, you re-build the whole application on each API change. Now, some might say *"Oh, just use functors/lenses/monads/whatever!"* - but imagine a *trivial task*:

*bad one:* you are given a map of string translations for languages supported by the system. The task is to translate a string in English or just use it as-is when there is no translation.

*better one:* you are given a map of (what)?

Looks really easy, right? But can you implement it? Can you?

```js
JSON = {
  "RU": {
    "hello": "привет",
    "world": "мир"
  },
  "DE": {
    "hello": "guten tag"
  },
  "PL": {
    "hello": "witaj",
    "world": "świat"
  },
}
```

```hs
import Html exposing (text)
import Json.Decode exposing (decodeString, dict, string)

decodeString (dict dict) "{ \"alice\": { \"hello\": \"something\" }, \"bob\": { \"world\": \"something else\" } }"
```

## References

[NASM tutorial](http://cs.lmu.edu/~ray/notes/nasmtutorial/)
[language-to-Assembly online compiler](https://godbolt.org/#)
[PE file description](https://habrahabr.ru/post/266831/)
[multicore assembly](https://stackoverflow.com/questions/980999/what-does-multicore-assembly-language-look-like#991191)
[OpenCL base knowledge](https://habrahabr.ru/post/345984/)
