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

This article is academical mostly. People who study something like _low-level programming_ at their universities may be interested.

<!--more-->

## Simplest function in NASM

And to start off, we will write a very simple program in assembly language. I shall be covering NASM language and compiler under Linux. MASM for Windows is much like that, but you should find your own way of compiling, linking and debugging all this code.

Our first program will do nothing. It will just contain globally available function, named <code>myfunc</code>.

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

Note these instructions: <code>enter 0, 0</code> and <code>leave</code>. These are dedicated to create a **stack frame**. Stack frame is a part of stack, where we can store variables. This part of stack is isolated, so we barely may hurt system when using stack operations (<code>push</code> and <code>pop</code>).

Actually, you may create the stack frame yourself, pushing <code>ESP</code> and <code>EBP</code> to a stack manually, then shifting <code>ESP</code> and rolling all this back at function’s end. But these instructions are simpler.

**NOTE:** never forget the <code>leave</code> operation when using <code>enter</code> one! This may cause a <code>SEGFAULT</code> exceptions and you may spend hours searching for an error _(just as I did this night…)_.

To use our function in a C++ program, we need to perform three steps:

1. add en external declaration for our function in C++
1. compile our C++ and NASM programs to object files _(_.o or _.obj)_
1. link our object files into a single binary one

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
$ g++ -c -m32 -g test.cpp -o test_c.o
$ nasm -felf32 -g test.asm -o test_asm.o
$ g++ -m32 -g test_c.o test_asm.o -o test
```

Let’s take a look over each of these closely.

```bash
$ g++ -c -m32 -g test.cpp -o test_c.o
```

This tells compiler a few things:

1. <code>-c</code>: only compile the code, do not link it (do not search for referenced functions)
2. <code>-m32</code>: compile code in a 32-bit mode
3. <code>-g</code>: add a debugger information
4. <code>-o test_c.o</code>: write output to a <code>test_c.o</code> file

_Why 32-bit mode? Why not 64-bit?_ - you may ask. Because some conventions of 64-bit mode are harder to understand and should be compared to 32-bit ones.

Now, compiling assembly code command:

```bash
$ nasm -felf32 -g test.asm -o test_asm.o
```

This provides compiler with these options:

1. <code>-felf32</code>: compile in a 32-bit mode
2. <code>-g</code>: add a debugginng info
3. <code>-o test_asm.o</code>: write output to an object file <code>test_asm.o</code>

Note the difference between <code>-m32</code> and <code>-felf32</code>. They mean the same, but are spelled differently.

## Passing arguments and returning values

Now let’s make our function do something for a great good. For example, sum-up two numbers. We will end-up with this function declaration:

```c
extern "C" int sum_two_numbers(int a, int b);
```

The values <code>a</code> and <code>b</code> are integer. This means, each of them is **4-byte wide**. You can find sizes of different C types writing a very simple program:

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

The difference in atomic types is really “ghostly”, namely <code>long</code> and <code>long double</code> are **4-byte longer** in 64-bit mode. But when it comes to pointer types, we have twice longer variables.

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

Compile it with <code>g++</code> using this command:

```bash
$ g++ -S -m32 -c -masm=intel test1.cpp -o test1.asm
```

I’ll explain this line’s options:

1. <code>-S</code>: generate assembly output
2. <code>-m32</code>: generate 32-bit code
3. <code>-c</code>: stop after compiling
4. <code>-masm=intel</code>: use Intel’ assembly syntax; it is NASM’ syntax and thus more readable then GASM’ one
5. <code>-o test1.asm</code>: write output to a <code>test1.asm</code> file

I shall not show the full output of this program, because it’s huge. It takes almost 400 lines of assembly code (370, actually)! Yet, in 64-bit mode it is just a bit more than 300 lines of code (precisely, 318). 60 LOC difference, but still…

These 60 lines of code is caused by a C type sizes. See, in 32-bit mode we have registers of a size <code>32 / 8 = 4</code> bytes. This is enough to store <code>int</code> or <code>float</code> value. Byt when it comes to <code>long double</code> or even just <code>double</code>, we have 4 bytes more. In 64-bit mode we have 8-byte wide registers. So, even a <code>long double</code> variable may be stored in a single register.

But let’s go back and take a look at, let’s say, <code>func1</code> function assembly:

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

Yeah, monstrous&hellip; Cleaning it up and using <code>enter</code> and <code>leave</code>, we have only this:

```nasm
_Z5func1v:
    enter 0, 0
    mov eax, 120
    leave
    ret
```

See, the return value is stored in a <code>EAX</code> register. That’s how we should return values from our functions. When it comes to a larger data types, we may return values via <code>EDX:EAX</code> registers’ pair. Yeah, strange, but that is a **convention**.

Let’s take a look at the assembly code for a <code>func7</code> function and compare its variations for 32-bit mode vs 64-bit mode:

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

See, there are two registers used in a 32-bit mode, <code>EAX = 2147483647</code> and <code>EDX = 0</code>. The second register is used for a sign value. If we’d change the return value for our C++ function to return a negative value:

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

Now let’s take a look over the <code>func8</code> function:

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

We may clean it up removing all those <code>DWORD PTR</code> type hints:

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

All the memory “by the negative side” of <code>EBP</code> is dedicated to local variables. All the memory “by the positive side” of <code>EBP</code> is the one with arguments, passed to our function.

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

1. <code>sub esp, 8</code> - this allocates 8 bytes of stack memory for our local variables
2. <code>[ebp+8]</code> and <code>[ebp+12]</code> are two parts, each 4-byte long, of our argument of type <code>long long</code>
3. <code>[ebp-8]</code> and <code>[ebp-4]</code> are two parts of our return value; each 4-byte long; of type <code>long long</code>
4. return value is split into two registers, namely, <code>EAX</code> (high-order bytes) and <code>EDX</code> (low-order bytes)

That is how C passes arguments to a function in 32-bit mode. Arguments here are passed via stack. In 64-bit mode it’s a bit complicated: arguments are passed via registers and if they are not enough - through the stack. Registers are the following (ordered): <code>RDI</code>, RSI<code>, RDX</code>, RCX<code>, R8</code>, <code>R9</code>.

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

And let’s create the function <code>sum</code> in NASM. To start off, we’ll use a function, receiving two arguments, <code>int</code> and <code>int*</code> and returning a zero.

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

Now what we would like to do is to add each element of array to our <code>result</code> variable _(oh, we do not have one yet!)_. To do this, we will use two registers: <code>ECX</code> to count how many elements we have added and <code>EAX</code> to store the sum. Each element’s address is <code>*a + 4 * i</code> or address of <code>a[0]</code> plus <code>4 bytes</code> times <code>i</code>, our element number.

The loop we would use is a reverse one: first we assign <code>ECX = n</code> and then decrement our <code>ECX</code> by one each loop iteration. We are decrementing <code>ECX</code> by one because it contains a number of elements at the beginning of our function. We may use even reverse approach (or a straight one in the meanings of C, when we count from the first element to the last): first, we assign <code>ECX = 0</code> and before going to the end of a loop we will compare <code>ECX</code> to <code>n</code> instead of zero.

In NASM we may calculate the address of each array element in the operand itself: <code>[ebx + 4 * ecx]</code>.

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

    jg add_loop ; if ECX &gt; 0 then goto add_loop

    ; EAX contains the sum here

    leave
    ret
```

Note that I subtract four bytes in an element address: <code>[ebx + 4 * ecx - 4]</code>. That’s because our i'th element starts at <code>*a + (i * 4)</code> byte, but we have <code>i = n</code> on the beginning. Thus, first iteration will try to add element, starting at <code>*a + (n * 4)</code> byte, which does not exist in our array _(the 5th element)_. So, we need to subtract one element’ size from our <code>[ebx + 4 * ecx]</code> address.

Now, if we would like to shorten our source a bit, we may use the <code>loop</code> operation. What it does, is compares <code>ECX</code> with zero and if it is greater than zero - it jumps to a label specified.

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

**with <code>loop</code> instruction**:

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

1. pushing to stack (<code>FLD</code>, <code>FLDZ</code>, <code>FLD1</code>, etc.)
2. floating-point arithmetics (<code>FADD</code>, <code>FMUL</code>, <code>FDIV</code>, <code>FSUB</code>, etc.)
3. arithmetics with popping from a stack into the top stack cell (<code>ST0</code>)
4. popping from a stack to a memory (<code>FST</code> operations)

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

1. load zero to a floating stack _(ST: <code>[0 nan nan nan nan nan nan]</code>)_
2. _in a loop_ load _i_th element of <code>v1</code> to a floating stack _(ST: <code>[3 0 nan nan nan nan nan]</code>)_
3. _in a loop_ load _i_th element of <code>v2</code> to a floating stack _(ST: <code>[4 3 0 nan nan nan nan]</code>)_
4. _in a loop_ multiply first two elements of a floating stack, write the result to <code>ST1</code> and pop stack head _(ST: <code>[12 0 nan nan nan nan nan]</code>)_
5. _in a loop_ add first two elements of a stack, write the result to <code>ST1</code> and pop stack head _(ST: <code>[12 nan nan nan nan nan nan]</code>)_
6. _in a loop_ add 12 bytes to our <code>i</code>
7. _in a loop_ add 12 bytes to our <code>t</code>

At the end of our loop, precisely, at our <code>just_exit</code> label, we will have floating stack with the only element on its top, the dot product of our vectors <code>v1</code> and <code>v2</code>. This value will be returned to our C++ program.

## A few words on debugging

As you remember _(if not - just look above)_ we added a _debugger info_ option when compiling our programs. Now let&rsquo;s use it.

Let&rsquo;s have some buggy program. For example, the one which calculates a rectangular parallelepiped&rsquo;s surface area and volume:

**C program**:

```c
#include <stdio.h>

extern void surface_and_volume(float a, float b, float c, float *v, float *s);

int main() {
  float surface = 0, volume = 0;

  surface_and_volume(4, 5, 6, &amp;volume, &amp;surface);

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
$ g++ -c -m32 -g test3.c -o test3_c.o
$ nasm -felf32 -g test3.asm -o test3_asm.o
$ g++ -m32 -g test3_asm.o test3_c.o -o test3
$ gdb test3
```

Now, when you&rsquo;re in a debugger&rsquo; console, you may run debugging commands. Here are a few of them:

* <code>r</code> or <code>run</code> will run your program, stopping at first breakpoint
* <code>b func_name</code> or <code>break func_name</code> will set a breakpoint at the first line of <code>func_name</code>
* <code>p var</code> or <code>print var</code> will show the <code>var</code> variable contents in decimal format. For registers its <code>$eax</code> and so on
* <code>p /x var</code> or <code>print /x var</code> will show the <code>var</code> variable contents in hexadecimal format
* <code>x mem_addr</code> will show the contents of memory at <code>mem_addr</code>
* <code>x/4 mem_addr</code> will show **four** 4-byte pieces of memory at <code>mem_addr</code>
* <code>disassemble</code> will print out the assembly code for current function
* _(from breakpoint)_ <code>c</code> or <code>continue</code> will run program until it hits end or breakpoint
* _(from breakpoint)_ <code>ni</code> will step one instruction
* _(from breakpoint)_ <code>n</code> or <code>next</code> will step over the next function _(in C)_; for ASM it&rsquo;s same as <code>ni</code>
* _(from breakpoint)_ <code>s</code> or <code>step</code> will step in the next function _(in C)_; for ASM it&rsquo;s same as <code>ni</code>
* <code>info r</code> shows current registers&rsquo; state
* <code>info float</code> shows current co-processor state
* <kbd>Ctrl+D</kbd> stands for <code>quit</code>

Now, using GDB, try to find out what&rsquo;s wrong with the program I&rsquo;ve suggested!

## Afterword

This is currently most of important things I’ve learnt at the university. This is pretty much for a beginner. And this information is really for those who have fun writing code or those who are made to write some excercises at university.

As for me, now ASM does not look so scary now =) But I like writing more high-level code _(in C at least!)_ because it takes less time to do more.
