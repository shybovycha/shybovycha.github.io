---
layout: post
title: 'Assembly in C++ programs'
date: '2015-04-23T13:18:11+02:00'
tags:
- rtfm
- assembly
- nasm
- linux
- c++
tumblr_url: 'http://shybovycha.tumblr.com/post/117161684891/assembly-in-c-programs'
---

## Foreword

Writing code in assembly language in 2015 seems stupid and meaningless. Yet, this subject has a few huge pros:

1. understanding how computers/compilers/programs work
2. optimizations while writing a complex or performance-critical applications
3. fun

Well, in real life, I’ve never met conditions of such performance requirements when I should be writing some parts of application in ASM. Except, maybe, **a few** ACM ICPC problems.

So, we got two merely huge pros to write in assembly. Thus, if you are not getting fun of writing applications, you may be not interested in this topic.

This article is academical mostly. People who study something like *low-level programming* at their universities may be interested.

<!--more-->

## Simplest function in NASM

And to start off, we will write a very simple program in assembly language. I shall be covering NASM language and compiler under Linux. MASM for Windows is much like that, but you should find your own way of compiling, linking and debugging all this code.

Our first program will do nothing. It will just contain globally available function, named `myfunc`.

```nasm
BITS 32

section .text

    global myfunc

myfunc:

    enter 0, 0

    leave
    ret
```

This is barely a something useful, but that’s how it looks like. A simple function, which does nothing, has no arguments and returns nothing.

Note these instructions: `enter 0, 0` and `leave`. These are dedicated to create a **stack frame**. Stack frame is a part of stack, where we can store variables. This part of stack is isolated, so we barely may hurt system when using stack operations (`push` and `pop`).

Actually, you may create the stack frame yourself, pushing `ESP` and `EBP` to a stack manually, then shifting `ESP` and rolling all this back at function’s end. But these instructions are simpler.

**NOTE:** never forget the `leave` operation when using `enter` one! This may cause a `SEGFAULT` exceptions and you may spend hours searching for an error _(just as I did last night...)_.

To use our function in a C++ program, we need to perform three steps:

1. add en external declaration for our function in C++
2. compile our C++ and NASM programs to object files _(`.o` or `.obj`)_
3. link our object files into a single binary one

So, we need to interference with assembly from within our C++ code. And declare an external function. Here’s how our dummy program may look like:

```c
#include <stdio.h>

extern "C" void myfunc();

int main() {
    myfunc();

    return 0;
}
```

Compiling this contains three steps, as I mentioned above:

```bash
g++ -c -m32 -g test.cpp -o test_c.o
nasm -felf32 -g test.asm -o test_asm.o
g++ -m32 -g test_c.o test_asm.o -o test
```

Let’s take a look over each of these closely.

```bash
g++ -c -m32 -g test.cpp -o test_c.o
```

This tells compiler a few things:

1. `-c`: only compile the code, do not link it (do not search for referenced functions)
2. `-m32`: compile code in a 32-bit mode
3. `-g`: add a debugger information
4. `-o test_c.o`: write output to a `test_c.o` file

_Why 32-bit mode? Why not 64-bit?_ - you may ask. Because some conventions of 64-bit mode are harder to understand and should be compared to 32-bit ones.

Now, compiling assembly code command:

```bash
nasm -felf32 -g test.asm -o test_asm.o
```

This provides compiler with these options:

1. `-felf32`: compile in a 32-bit mode
2. `-g`: add a debugginng info
3. `-o test_asm.o`: write output to an object file `test_asm.o`

Note the difference between `-m32` and `-felf32`. They mean the same, but are spelled differently.

## Passing arguments and returning values

Now let’s make our function do something for a great good. For example, sum-up two numbers. We will end-up with this function declaration:

```c
extern "C" int sum_two_numbers(int a, int b);
```

The values `a` and `b` are integer. This means, each of them is **4-byte wide**. You can find sizes of different C types writing a very simple program:

```c
#include <stdio.h>

int main() {
    printf("size(char) = %d bytes\n", sizeof(char));
    printf("size(short) = %d bytes\n", sizeof(short));
    printf("size(int) = %d bytes\n", sizeof(int));
    printf("size(long) = %d bytes\n", sizeof(long));
    printf("size(long long) = %d bytes\n", sizeof(long long));
    printf("size(float) = %d bytes\n", sizeof(float));
    printf("size(double) = %d bytes\n", sizeof(double));
    printf("size(long double) = %d bytes\n", sizeof(long double));

    printf("size(char*) = %d bytes\n", sizeof(char*));
    printf("size(short*) = %d bytes\n", sizeof(short*));
    printf("size(int*) = %d bytes\n", sizeof(int*));
    printf("size(long*) = %d bytes\n", sizeof(long*));
    printf("size(long long*) = %d bytes\n", sizeof(long long*));
    printf("size(float*) = %d bytes\n", sizeof(float*));
    printf("size(double*) = %d bytes\n", sizeof(double*));
    printf("size(long double*) = %d bytes\n", sizeof(long double*));

    return 0;
}
```

On my laptop this code printed this:

```
size(char) = 1 bytes
size(short) = 2 bytes
size(int) = 4 bytes
size(long) = 8 bytes
size(long long) = 8 bytes
size(float) = 4 bytes
size(double) = 8 bytes
size(long double) = 16 bytes
size(char*) = 8 bytes
size(short*) = 8 bytes
size(int*) = 8 bytes
size(long*) = 8 bytes
size(long long*) = 8 bytes
size(float*) = 8 bytes
size(double*) = 8 bytes
size(long double*) = 8 bytes
```

But when run in **32-bit** mode, these numbers changed:

```
size(char) = 1 bytes
size(short) = 2 bytes
size(int) = 4 bytes
size(long) = 4 bytes
size(long long) = 8 bytes
size(float) = 4 bytes
size(double) = 8 bytes
size(long double) = 12 bytes
size(char*) = 4 bytes
size(short*) = 4 bytes
size(int*) = 4 bytes
size(long*) = 4 bytes
size(long long*) = 4 bytes
size(float*) = 4 bytes
size(double*) = 4 bytes
size(long double*) = 4 bytes
```

The difference in atomic types is really "ghostly", namely `long` and `long double` are `4-byte longer` in 64-bit mode. But when it comes to pointer types, we have twice longer variables.

That is the first, may be not so notable, but really important difference between _32-bit_ and _64-bit_ modes. This will be handy when it comes to **arrays**. But that will be covered later.

Now, the more notable difference hides in how arguments are passed to a function and how the function returns its result.

To show this difference, we will write a few short functions and look at their assembly code. Here they are:

```c
char func1() {
    return 'x';
}

char func2(char x) {
    return x;
}

short func3() {
    return 1;
}

short func4(short x) {
    return x;
}

int func5() {
    return 2;
}

int func6(int x) {
    return x;
}

long long func7() {
    return 2147483647;
}

long long func8(long long x) {
    return x;
}

float func9() {
    return 3.14f;
}

float func10(float x) {
    return x;
}

double func11() {
    return 3.15;
}

double func12(double x) {
    return x;
}

long double func13() {
    return 3.16;
}

long double func14(long double x) {
    return x;
}

int main() {
    func1();
    func2('x');
    func3();
    func4(1);
    func5();
    func6(2);
    func7();
    func8(3);
    func9();
    func10(3.14f);
    func11();
    func12(3.14);
    func13();
    func14(3.14);

    return 0;
}
```

Compile it with `g++` using this command:

```bash
g++ -S -m32 -c -masm=intel test1.cpp -o test1.asm
```

I’ll explain this line’s options:

1. `-S`: generate assembly output
2. `-m32`: generate 32-bit code
3. `-c`: stop after compiling
4. `-masm=intel`: use Intel’ assembly syntax; it is NASM' syntax and thus more readable then GASM' one
5. `-o test1.asm`: write output to a `test1.asm` file

I shall not show the full output of this program, because it’s huge. It takes almost 400 lines of assembly code (370, actually)! Yet, in 64-bit mode it is just a bit more than 300 lines of code (precisely, 318). 60 LOC difference, but still…

These 60 lines of code is caused by a C type sizes. See, in 32-bit mode we have registers of a size `32 / 8 = 4` bytes. This is enough to store `int` or `float` value. Byt when it comes to `long double` or even just `double`, we have 4 bytes more. In 64-bit mode we have 8-byte wide registers. So, even a `long double` variable may be stored in a single register.

But let’s go back and take a look at, let’s say, `func1` function assembly:

```nasm
_Z5func1v:
.LFB0:
    .cfi_startproc
    push    ebp
    .cfi_def_cfa_offset 8
    .cfi_offset 5, -8
    mov ebp, esp
    .cfi_def_cfa_register 5
    mov eax, 120
    pop ebp
    .cfi_restore 5
    .cfi_def_cfa 4, 4
    ret
    .cfi_endproc
.LFE0:
    .size   _Z5func1v, .-_Z5func1v
    .globl  _Z5func2c
    .type   _Z5func2c, @function
```

Yeah, monstrous... Cleaning it up and using `enter` and `leave`, we have only this:

```nasm
_Z5func1v:
    enter 0, 0
    mov eax, 120
    leave
    ret
```

See, the return value is stored in a `EAX` register. That’s how we should return values from our functions. When it comes to a larger data types, we may return values via `EDX:EAX` registers’ pair. Yeah, strange, but that is a **convention**.

Let’s take a look at the assembly code for a `func7` function and compare its variations for 32-bit mode vs 64-bit mode:

**32-bit func7:**

```nasm
_Z5func7v:
    enter 0, 0
    mov eax, 2147483647
    mov edx, 0
    leave
    ret
```

**64-bit func7:**

```nasm
_Z5func8l:
    enter 0, 0
    mov QWORD PTR [rbp-8], rdi
    mov rax, QWORD PTR [rbp-8]
    leave
    ret
```

See, there are two registers used in a 32-bit mode, `EAX = 2147483647` and `EDX = 0`. The second register is used for a sign value. If we’d change the return value for our C++ function to return a negative value:

```c
long long func7() {
    return -2147483647;
}
```

We will end-up with this code in a 32-bit mode:

```nasm
mov eax, -2147483647
mov edx, -1
```

And in 64-bit mode it will have only one operation:

```nasm
mov rax, -2147483647
```

Now let’s take a look over the `func8` function:

```nasm
_Z5func8x:
    enter 0, 0
    sub esp, 8
    mov eax, DWORD PTR [ebp+8]
    mov DWORD PTR [ebp-8], eax
    mov eax, DWORD PTR [ebp+12]
    mov DWORD PTR [ebp-4], eax
    mov eax, DWORD PTR [ebp-8]
    mov edx, DWORD PTR [ebp-4]
    leave
    ret
```

We may clean it up removing all those `DWORD PTR` type hints:

```nasm
_Z5func8x:
    enter 0, 0
    sub esp, 8
    mov eax, [ebp+8]
    mov [ebp-8], eax
    mov eax, [ebp+12]
    mov [ebp-4], eax
    mov eax, [ebp-8]
    mov edx, [ebp-4]
    leave
    ret
```

All the memory “by the negative side” of `EBP` is dedicated to local variables. All the memory “by the positive side” of `EBP` is the one with arguments, passed to our function.

Taking that into account, we may rewrite our assembly function as this:

```nasm
_Z5func8x:
    enter 0, 0

    %define x1 dword[ebp+8]
    %define x2 dword[ebp+12]
    %define tmp1 dword[ebp-8]
    %define tmp2 dword[ebp-4]

    sub esp, 8
    mov eax, x1
    mov tmp1, eax
    mov eax, x2
    mov tmp2, eax
    mov eax, tmp1
    mov edx, tmp2

    leave
    ret
```

Now it became more readable.

Here we have a few really important things:

1. `sub esp, 8` - this allocates 8 bytes of stack memory for our local variables
2. `[ebp+8]` and `[ebp+12]` are two parts, each 4-byte long, of our argument of type `long long`
3. `[ebp-8]` and `[ebp-4]` are two parts of our return value; each 4-byte long; of type `long long`
4. return value is split into two registers, namely, `EAX` (high-order bytes) and `EDX` (low-order bytes)

That is how C passes arguments to a function in 32-bit mode. Arguments here are passed via stack. In 64-bit mode it’s a bit complicated: arguments are passed via registers and if they are not enough - through the stack. Registers are the following (ordered): `RDI`, RSI`, RDX`, RCX`, R8`, `R9`.

And the return values are stored in registers. Always. In both 32-bit and 64-bit modes.

## Working wit arrays

I shall not cover working with arrays in NASM itself, but rather working with already allocated memory in C.

Arrays are transfered to a function as pointers in C and C++. Under the hood, pointer is just an address to a memory block. To its beginning, actually. Knowing the **size of each array element** and **elements count**, we may perform any kind of operations simply iterating through a set of memory addresses.

Let’s for example calculate a sum of an array elements:

```c
#include <stdio.h>

extern "C" int sum(int n, int *a);

int main() {
    int n = 5, a[] = { 1, 2, 7, 9, -4 };

    // 1 + 2 + 7 + 9 - 4 = 15
    printf("sum(a) = %d\n", sum(n, a));

    return 0;
}
```

And let’s create the function `sum` in NASM. To start off, we’ll use a function, receiving two arguments, `int` and `int*` and returning a zero.

```nasm
BITS 32

section .text

global sum

sum:

    enter 0, 0

    %define n dword [ebp + 8]
    %define a dword [ebp + 12]

    mov eax, 0

    leave
    ret
```

Now what we would like to do is to add each element of array to our `result` variable _(oh, we do not have one yet!)_. To do this, we will use two registers: `ECX` to count how many elements we have added and `EAX` to store the sum. Each element’s address is `*a + 4 * i` or address of `a[0]` plus `4 bytes` times `i`, our element number.

The loop we would use is a reverse one: first we assign `ECX = n` and then decrement our `ECX` by one each loop iteration. We are decrementing `ECX` by one because it contains a number of elements at the beginning of our function. We may use even reverse approach (or a straight one in the meanings of C, when we count from the first element to the last): first, we assign `ECX = 0` and before going to the end of a loop we will compare `ECX` to `n` instead of zero.

In NASM we may calculate the address of each array element in the operand itself: `[ebx + 4 * ecx]`.

Now everything what we need is to add all those hints into a single program:

```nasm
BITS 32

section .text

global sum

sum:

    enter 0, 0

    %define n dword [ebp + 8]
    %define a dword [ebp + 12]

    mov eax, 0 ; EAX = sum = 0
    mov ebx, a ; EBX = *a
    mov ecx, n ; ECX = i = n

add_loop:

    add eax, [ebx + 4 * ecx - 4] ; EAX += a[i]

    dec ecx ; ECX --
    cmp ecx, 0

    jg add_loop ; if ECX > 0 then goto add_loop

    ; EAX contains the sum here

    leave
    ret
```

Note that I subtract four bytes in an element address: `[ebx + 4 * ecx - 4]`. That’s because our i'th element starts at `*a + (i * 4)` byte, but we have `i = n` on the beginning. Thus, first iteration will try to add element, starting at `*a + (n * 4)` byte, which does not exist in our array _(the 5th element)_. So, we need to subtract one element’ size from our `[ebx + 4 * ecx]` address.

Now, if we would like to shorten our source a bit, we may use the `loop` operation. What it does, is compares `ECX` with zero and if it is greater than zero - it jumps to a label specified.

These two codes are completely identical for processor:

**manual loop**:

```nasm
mov ecx, n

add_loop:

    ; do something

    dec ecx
    cmp ecx, 0
    jg add_loop
```

**using the `loop` instruction**:

```nasm
mov ecx, n

add_loop:

    ; do something

    loop add_loop
```

We’ve just saved two lines of code!

## Floating-point operations

When working with floating-point data, we have **seven** registers, which could be used to perform operations on a floating-point arguments. We may **store** float data in a **memory** _(but never in registers!)_, but we may not perform operations on a float data contained in a memory. Just as we may not operate on a usual data, stored in a memory - we need to store it in registers first. Same thing here - store data on a **floating-point stack** and perform operations there. Then move results to the memory.

When writing a C++ functions working with floats, arguments are stored on a float stack and results are stored on a top of that stack. Yet, the other six cells of a float stack **should** be cleared when returning a value. Otherwise it may cause hard-to-find errors.

So, the basic operations we may run on a floats are:

1. pushing to stack (`FLD`, `FLDZ`, `FLD1`, etc.)
2. floating-point arithmetics (`FADD`, `FMUL`, `FDIV`, `FSUB`, etc.)
3. arithmetics with popping from a stack into the top stack cell (`ST0`)
4. popping from a stack to a memory (`FST` operations)

Yeah, these are a hell-yeah mix of both arithmetic operations and stack operations!

Let’s write a very short example, showing how to work with floats. Let it be a two-vector dot product function.

We shall write a function of this declaration:

```c
#include <stdio.h>

extern "C" long double dot_product(int n, long double *v1, long double *v2);

int main() {
    long double v1[] = { 3, 5 };
    long double v2[] = { 4, 2 };
    int n = 2;

    // 3*4 + 5*2 = 12 + 10 = 22
    printf("dot_product(v1, v2) = %0.4f\n", dot_product(n, v1, v2));

    return 0;
}
```

This one will calculate a dot product of two _n-element_ vectors.

```nasm
BITS 32

section .text

    global dot_product

dot_product:

    enter 0, 0

    %define n dword[ebp + 8]
    %define v1 dword[ebp + 12]
    %define v2 dword[ebp + 16]

    mov ecx, n
    mov edx, v1
    mov ebx, v2

    fldz ; stack: 0 ( = tail)

add_loop:

    fld tword [edx] ; stack: v1, tail
    fld tword [ebx] ; stack: v2, v1, tail

    fmulp st1, st0 ; stack: v2 * v1, tail
    faddp st1, st0 ; stack: v2 * v1 + tail

    add edx, 12
    add ebx, 12

    loop add_loop

just_exit:

    leave
    ret
```

The algorithm of a code above may be written as follows:

- load zero to a floating stack *(ST: `[0 nan nan nan nan nan nan]`)*
- *in a loop* load *i_th element of `v1` to a floating stack _(ST: `[3 0 nan nan nan nan nan]`)*
- *in a loop* load *i_th element of `v2` to a floating stack _(ST: `[4 3 0 nan nan nan nan]`)*
- *in a loop* multiply first two elements of a floating stack, write the result to `ST1` and pop stack head *(ST: `[12 0 nan nan nan nan nan]`)*
- *in a loop* add first two elements of a stack, write the result to `ST1` and pop stack head *(ST: `[12 nan nan nan nan nan nan]`)*
- *in a loop* add 12 bytes to our `i`
- *in a loop* add 12 bytes to our `t`

At the end of our loop, precisely, at our `just_exit` label, we will have floating stack with the only element on its top, the dot product of our vectors `v1` and `v2`. This value will be returned to our C++ program.

## A few words on debugging

As you remember _(if not - just look above)_ we added a _debugger info_ option when compiling our programs. Now let's use it.

Let's have some buggy program. For example, the one which calculates a rectangular parallelepiped's surface area and volume:

**C program**:

```c
#include <stdio.h>

extern void surface_and_volume(float a, float b, float c, float *v, float *s);

int main() {
  float surface = 0, volume = 0;

  surface_and_volume(4, 5, 6, &volume, &surface);

  printf("volume: %0.3f surface: %0.3f\n", volume, surface);

  return 0;
}
```

**NASM program**:

```nasm
BITS 32

section .text

    global surface_and_volume

surface_and_volume:

    enter 0, 0

    %define a dword[ebp+8]
    %define b dword[ebp+12]
    %define c dword[ebp+16]
    %define vol_ptr dword [ebp+20] ; a*b*c
    %define surf_ptr dword [ebp+24] ; a*a*2 + b*b*2 + c*c*2

    mov eax, vol_ptr
    mov ebx, surf_ptr

    fldz ; st6
    fldz ; st5
    fldz ; st4
    fld c ; st3
    fld b ; st2
    fld a ; st1
    fldz ; st0

    ; calculate volume
    fsub st0, st0 ; st0 = 0
    fadd st1 ; st0 = a
    fmul st2 ; st0 *= b
    fmul st3 ; st0 *= c

    fst dword [eax]

    ; calculate surface

    fsub st0, st0 ; st0 = 0
    fadd st1 ; st0 = a
    fmul st2 ; st0 *= b
    fadd st4, st0 ; st4 = a*b

    fsub st0, st0 ; st0 = 0
    fadd st1 ; st0 = a
    fmul st3 ; st0 *= c
    fadd st5, st0 ; st5 = a*c

    fsub st0, st0 ; st0 = 0
    fadd st2 ; st0 = b
    fmul st3
    fadd st6, st0 ; st6 = b*c

    fsub st0, st0 ; st0 = 0
    fadd st4
    fadd st5
    fadd st6 ; st0 = a*b + a*c + b*c
    fadd st0 ; st0 = 2*(a*b + a*c + b*c)

just_exit:

    fst dword [ebx]

    ; free float stack
    fstp a
    fstp a
    fstp a
    fstp a
    fstp a
    fstp a

    ret
```

Compile it as usual and run with GDB:

```bash
g++ -c -m32 -g test3.c -o test3_c.o
nasm -felf32 -g test3.asm -o test3_asm.o
g++ -m32 -g test3_asm.o test3_c.o -o test3
gdb test3
```

Now, when you're in a debugger' console, you may run debugging commands. Here are a few of them:

- `r` or `run` will run your program, stopping at first breakpoint
- `b func_name` or `break func_name` will set a breakpoint at the first line of `func_name`
- `p var` or `print var` will show the `var` variable contents in decimal format. For registers its `$eax` and so on
- `p /x var` or `print /x var` will show the `var` variable contents in hexadecimal format
- `x mem_addr` will show the contents of memory at `mem_addr`
- `x/4 mem_addr` will show **four** 4-byte pieces of memory at `mem_addr`
- `disassemble` will print out the assembly code for current function
- *(from breakpoint)* `c` or `continue` will run program until it hits end or breakpoint
- *(from breakpoint)* `ni` will step one instruction
- *(from breakpoint)* `n` or `next` will step over the next function *(in C)*; for ASM it's same as `ni`
- *(from breakpoint)* `s` or `step` will step in the next function *(in C)*; for ASM it's same as `ni`
- `info r` shows current registers' state
- `info float` shows current co-processor state
- <kbd>Ctrl+D</kbd> stands for `quit`

Now, using GDB, try to find out what's wrong with the program I've suggested!

## Afterword

This is currently most of important things I’ve learnt at the university. This is pretty much for a beginner. And this information is really for those who have fun writing code or those who are made to write some excercises at university.

As for me, now ASM does not look so scary now =) But I like writing more high-level code _(in C at least!)_ because it takes less time to do more.
