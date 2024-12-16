---
title: Iterating a vector in C++
layout: post
---

Such a simple topic - iterating over a vector, is it even worth discussing?

Interestingly enough, there **is** a difference in **how exactly** you iterate - be it using iterators, `for(:)` sugar or plain old `for(i=0; i<vec.size(); ++i)`.

Let us see what output does a compiler produce in each of these cases.

`sample1`:

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

`sample2`:

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

`sample3`:

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

`sample1`:

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

`sample2`:

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

`sample3`:

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

Iterating over a vector backwards affects the memory caching in a pretty poor manner _(benchmarking TBD)_.
