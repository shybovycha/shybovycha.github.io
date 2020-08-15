---
layout: post
title: Memory allocation in ASM
date: '2015-05-06T12:20:04+02:00'
tags:
- rtfm
- tutorial
- assembly
- nasm
- linux
- c++
tumblr_url: http://shybovycha.tumblr.com/post/118273000531/memory-allocation-in-asm
---

Currently I am working on a long arithmetic problem at the university. This problem is much more complicated than I described or than a task I shall be describing now, but here&rsquo;s the thing: I needed some part of memory to be allocated from within my function. And I needed this to be done in assembly.

<!--more-->
Thus, I created this piece of snippet code:

```
; void addition(int* x, int x_len, int* y, int y_len, int* &z, int* z_len);
global _Z8additionPiiS_iRS_S_
_Z8additionPiiS_iRS_S_:

    enter 0, 0

    %define p_x [ebp + 8]
    %define x_len [ebp + 12]
    %define p_y [ebp + 16]
    %define y_len [ebp + 20]
    %define p_z [ebp + 24]
    %define p_z_len [ebp + 28]

addition_allocate_mem:

    ; push x_len * 4 ; bytes to allocate
    push 3 * 4 ; bytes to allocate
    call malloc ; call malloc()
    add esp, 4 ; undo push
    mov edx, eax ; save returned address from malloc
    mov eax, p_z
    mov [eax], edx ; z = malloc(...)
    mov eax, p_z_len
    mov [eax], dword 3 ; *z_len = elements

addition_fill_mem:

    ; fill with sample values
    mov eax, p_z
    mov eax, [eax]
    add eax, 0 * 4
    mov [eax], dword 4

    ; mov eax, p_z
    add eax, 1 * 4
    mov [eax], dword 3

    ; mov eax, p_z
    ; add eax, 2 * 4
    add eax, 1 * 4
    mov [eax], dword 2

    leave
    ret
```

There are, however, a few really interesting things in this code:

* naming of C++ functions, generated from assembly <em>(name mangling)</em>
* memory allocation itself
* returning data from function via pointers&hellip; <strong>in assembly!</strong>

To demonstrate how this stuff works, we need some C++ code which uses our assembly function:

```cpp
#include <stdio.h>
#include <stdlib.h>

// our addition function for BIG integers
// arguments are as follows: number and its length; two first pairs are the operands
// and the last two arguments describe the returned big integer
// thus, the result is z = x + y
extern "C" void addition(int* x, int x_len, int* y, int y_len, int* &z, int* z_len);

// helper function to convert BIG integers to strings
char* bigint2str(int* x, int len) {
    char *res = (char*) malloc((len + 1) * sizeof(char));

    for (int i = 0; i < len; i++) {
        res[i] = x[i] + '0';
    }

    res[len] = '\0';

    return res;
}

int main() {
    int* a = 0;
    int a_len = 0;

    // here we add nothing with nothing
    // and storing the result in a big integer `a`
    addition(0, 0, 0, 0, a, &a_len);

    printf("a = %s\n", bigint2str(a, a_len));

    return 0;
}
```

Comments in the code describe those moments which are important.

To compile these codes and link them into one executable, use these:

```
nasm -g -felf32 test.asm -o test_asm.o
g++ -g test.cpp -c -m32 -o test_c.o
g++ -g -m32 -o test test_asm.o test_c.o
```

Now, let&rsquo;s talk about name mangling. It is really important. I shall not cover all the depths of this, only the parts, related to this article.

We see that our function,

```
void addition(int* x, int x_len, int* y, int y_len, int* &z, int* z_len);
```

is known as `_Z8additionPiiS_iRS_S_` in the assembly code.

<em>What&rsquo;s the..? What are all these strange prefixes?</em>  - you might ask.

Here&rsquo;s the convention:

1. functions are named with the underscore and an uppercase letter
2. function name&rsquo; length and the name itself follows that prefix
3. arguments are stored as their types only

Argument type is encoded as well. For our example, we see these:

1. `Pi` - that means, literally, `pointer to integer`
2. `i` - that stands for `integer`
3. `S_` - that is the same as `Pi`, equal to `signed integer`, <strong>but</strong> for some reason <em>(yes, I do not know why this happens)</em> if you try to replace it with `Pi`, your function will not be found by a linker
4. `RS_` - this is `a reference to a pointer to integer`

To get know those conventions better, you might refer to <a href="http://www.ofb.net/gnu/gcc/gxxint_15.html">g++ internals reference</a>.

You can decode demangled <em>(encoded)</em> function names as well. Just use `c++filt` utility:

```bash
$ c++filt -n _Z8divisionPiiS_iRS_S_
division(int*, int, int*, int, int*&, int*)
```

&lsquo;til next time!
