# Functional programming in a real world

There are lots of talks about how beautiful functional programming is,
how it simplifies our lives, making code more straightforward, clean and testable.
But to what extent is it true?

Before talking high-level, let me ask you one question: do you know how programs are executed
at a processor level? If you do know that - just skip the first section of this article.

## Gentle introduction to how computers work

What happens when we run a program on our PC? There is a little bit of magic which must happen first. You run a program
(most of the time) from an operating system. And this operating system needs to perform some actions
before it can send your program's commands straight to a processor - reserve some memory, make scheduling...
And since most operating systems are meant to be multi-tasked (allow multiple programs to run simultaneously),
OS can not allow your program to just send all of its commands to a processor, one by one - if your program is faulty then
any other program might have never been executed again, even the OS itself! That's why operating system decides
when to send the next command to a processor to execute it. This also allows for optimizations on multi-core
and multi-processor systems - when two operations could be executed simultaneously, whithout overwriting memory used
by another one - it can be run on a different core or processor to speed up the execution.

That's why our program will not contain programmer-defined commands only. Let's have a look at the
standard Windows executable file (PE32) structure:

<img data-src="{{ '/images/functional-programming-in-a-real-world/pe32-diagram.jpg' | prepend: site.baseurl }}" alt="">

You can see it has lots of *headers*, which describe different aspects of a program and
two major *sections* - **text** (containing the code itself) and **data** (containing all
the constant definitions and allocations). Each executable can also contain resources like images,
sounds and others - if needed.

We all know how to write a program in, say, C. But how a program in C (or any other language) is "understood"
(and thus, executed) by a processor? One may have heard of Assembly language. This is a low-level programming
language (in opposite to C, which is a high-level programming language). *"Low-level"* means that a programmer
writes instructions for the processor in the order processor will pick them up and execute, one by one, exactly as
they are set in a program, ommiting syntactic sugar constructions like loops, if-then-else operators and so on.
Usually in such programming languages programmers operate memory addresses and numbers only.
On the other hand, *high-level* programs, containing all the syntactic sugar features, are first translated
to something processor can "understand" and execute - processors commands (instructions). The program is also being
in terms of removing unused and unreacheable code, replacing expressions which can be pre-calculated into the results
of pre-calculation (like the expression `x * 2 * 32` could be simply written as `x * 64` or `x << 4`, which most
of the compilers will do by default because of performance reasons - binary shift operation is way more faster than
multiplication; but for programmer it might be required to stay explicit in order to highlight some calculations or
just to stay clear for other programmers).

Let us have a look at how programs are compiled. Consider this program in C:

```
int product(int a, int b) {
    return a * b;
}
```

It will be compiled into this assembly code:

```
product(int, int): # @product(int, int)
  imul edi, esi
  mov eax, edi
  ret
```

Looks simple, huh?

In essence, each line of a program in assembly language looks like this:

```
label: command operand1, operand2 # comment
```

Both label, operands and comment are optional with the exception that operands requirements
depend on command.

Operands could be either numbers, registers (refer to processor registers) or memory addresses.

Registers represent cells in processor memory, those are very limited in number and size, but are directly
accessed by processor and thus are very, very fast. There are eight general-purpose registers, common for
majority of processors: AX, BX, CX, DX, SP, BP, SI, DI. There are other registers, but we will cover them later.
Each command will store its result in one of
these registers. These registers have size of one word (2 bytes, 16 bits). Each register has "lower byte" and,
correspondingly, "higher byte" - for AX they are AL (low) and AH (high), representing first and last 8 bits of
a register. These registers are available for all 16-bit processors. More modern 32-bit processors have extended
versions of each register, for AX it is EAX, for CX its ECX and so on. They are twice as big as their non-extended
versions (32 bit, 2 words). Nowadays 64-bit systems have extended versions of extended registers, having 4 words
and precended with letter R: RAX, RBX and so on. There are also 16 registers worth 128 bit each, XMM0 .. XMM15.

Addressing memory refers to the computer's RAM; in order to operate with RAM processor first copies the
data from RAM to its registers and then performs operation on data in registers. Processor is unable
to operate with RAM data directly. You can see clearly such operations require additional actions
from processor, namely - copy data from RAM to registers before executing operation and copy the data
back to RAM after the operation execution (usually this step is stated separatedly by a programmer).
This is time-consuming. And since RAM is not that fast as processor's registers, it makes operations slow.
Memory is addressed using one of these syntaxes:

```
[address] ; [0x12345]
[register] ; [RAX] - takes address, contained in register RAX
[register + number] ; [RAX + 2]
[register + register * number] ; [RAX + RBX * 3]
[register + register * number + number] ; [RBX + EAX * 4 + 3]
```

You can also do more tricky thing: define a named constant - give a literal name to a number and then refer
to a value by its name. But first you need to reserve a memory for it:

```
constant_name: db 0x123 ; define one byte value (0x123) and give it a name "constant_name"
constant2: dw 'ab' ; define one word (two bytes) of values 0x61 and 0x62, correspondingly
const3: dd 'abcd' ; define double-word (four bytes)
const4: dq 0x123456789abcdef0 ; 8-byte wide constant
const5: dq 1.234567e20 ; double-precision constant
const6: dt 1.234567e20 ; extended-precision constant
```

Note I'm constantly mentioning numbers, but never characters or strings. That's because eventually all this
data is just a number (character code or a pointer to a memory - effectively, an address of a memory). And
processor can only operate on numbers, nothing else. Compiler will convert all these characters and floating-point
values to hexadecimal integer values anyway.

You can also reserve a bunch of memory cells using `res` command:

```
const1: resb 2 ; reserve 2 bytes
const2: resw 16 ; reserve 16 words
const3: resq 7 ; reserve seven 8-byte cells
```

This way each constant will point to the first of the reserved array' cells in RAM.

Let's take a look at our example again:

```
int product(int a, int b) {
    return a * b;
}
```

Its assembly code is:

```
product(int, int): # @product(int, int)
  imul edi, esi
  mov eax, edi
  ret
```

This program has exactly three effective lines:

```
imul edi, esi ; multiply the data in two registers, EDI and ESI and store the result of multiplication in EAX register
```

```
mov eax, edi ; copy the data from EAX to EDI
```

```
ret ; restore pointer to the currently running command and the values in registers from stack
```

See, assembly is still a somewhat high-level language - one command does a little bit of "magic" like
storing the result somewhere, or restoring the processor state (`ret`) or storing its state and moving to the
different command or sub-routing or function (`call`). Each processor has its own set of commands, which might
be more optimized for this particular processor. That's why it is really hard to get each program run best
on all possible computers when distributing it. But the "basic set" of commands is quite common for majority
of processors and you can be quite confident about them.

So after the program is written in Assembly (which is the lowest reasonable language we can more or less easily
write code in) it is compiled into... something... *Binary code* is what you might have been thinking of.
And you are totally correct - programs are essentially just a set of ones and zeros. But more interesting question
is how a computer executes them?

If we compile our function into binary file - we will get each Assembly command translated into a long
hexadecimal (or binary - if you split a long list of ones and zeros into chunks of 16 digits - you will be able to
write them down as one character, which is much more compach, right?) value, describing the opcode of a command
and its arguments. Each Assembly command has its own code, just like character in a string. And each register has
its own code too. Combine them - and you will get a long number. Convert it to binary - and you will get a line of
machine codes.

Processor then reads the operation code and acts exactly as it says - if it would be `imul RAX, RBX` - processor
will do just exactly as it says - multiply the value in register `RAX` by the value in register `RBX` and then
save the result in register `RAX`. To explain how processor actually multiplies values from registers and stores
them - we need to go one level lower - and get familiar with how processor does that on the electonics level.
But we won't do that. Instead, we will just scratch the surface by describing how *fast* those operations could be.

You might have heard about *processor's frequency* or *clock rate*. This is a number, showing how frequently an
electronic impulse is sent over all the processor's schematic to enforce its state update. Effectively, what it means is each processor operation like multiplication, addition, subtraction, moving the data around and others, need to change the state of different electronic components, mainly - triggers and transistors. When talking about addition of two registers, for example, processor will need to calculate the sum of each bit pair in registers given, handling overflows, different register lengths and clearing the state of the resutl register beforehand. Some processors are designed to do that in one cycle, but mainly addition will be done in several *clocks*. Hence a processor with a clock rate of 1 GHz **will not** make 1 billion additions, subtractions, multiplications or other operations per second. For a reference, here is a diagram, showing how many clocks does each operation takes in average

Remember I have mentioned optimization a while ago? Here's a sample table of how many clocks do some processor
instructions take (in average):

<img data-src="{{ '/images/functional-programming-in-a-real-world/cpu-operations-cost.png' | prepend: site.baseurl }}" alt="">

With this knowledge we can start talking about more high-level things.

## Preamble

What is the functional programming?

```
int square(int n) {
  return n * n;
}
```

Is this a functional approach?

According to Wikipedia, functional paradigm is described by two main principles:

1. operating on functions (functions as first-class citizens in a language)
2. immutability (context isolation)

## The big lie of functional programming

The example above shows only the context isolation approach. But how about the other one?
Given it is written in C/C++, consider these two examples:

```
int sum(std::vector<int> a) {
  int res = 0;

  for (int i : a) {
    res += i;
  }

  return res;
}
```

^ we will refer to this example a couple of times later

Its output in assembly with `-std=c++1z -O1` flags:

```
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

Now, more C-ish version:

```
int sum(int *a, int n) {
  int res = 0;

  for (int i = 0; i < n; ++i) {
      res += a[i];
  }

  return res;
}
```

with `-O2`:

```
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



```
auto f1 = [](auto a, auto b) { return a + b; };

auto f2 = [](std::vector<int> a, auto f, auto init) { auto res = init; for (auto i : a) res = f(i, res); return res; };
```

Does this demonstrate the first-class functions in C++? Does it refer to a functional paradigm?

```
int reduce(int n, int *a, int (*f)(int a, int b), int init) {
  int res = init;

  for (int i = 0; i < n; ++i) {
    res = f(res, a[i]);
  }

  return res;
}
```

This last example will be compiled (using Clang 5.0.0) into this:

```
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

```
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

```
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

```
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

```
module Example where

sum2 :: [Int] -> Int
sum2 = foldr (+) 0
```

```
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

```
module Example where

sum1 :: [Int] -> Int
sum1 (x:xs) = x + sum1 xs
sum1 [] =  0
```

```
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

```
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

```
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

```
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

```
int sum(int *a, int n) {
  if (n > 0)
    return a[0] + sum(a + 1, n - 1);

  return 0;
}
```

```
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

```
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

Now, we talk about im-mutability. But the machines' architecture we used to deal with are not designed for immutability by design.

What's more important, in a number of cases we have to deal with some data storages (like databases, cookies, etc.), which are mutable by design. So what's the point of having a well-designed immutability in a language itself, when we just throw it in a window by using those data storages?

## The practical applications

### Case 1: games / heavy-computational applications

Now, let's talk about how we *actually* use FP. Some of its principles are used all around. But what about a real-world
application, made with, say, Haskell?

Assume we have a simple game in C++ with all the physics libraries and graphics engine and sound library being written in C++.
What if *(why in the world a sane person would do this?)* we write it with Haskell?

...

The problem here is that games have to deal with big amounts of data. Would immutability help here?

*code samples here with memory allocation / deallocation counts*

Each frame we allocate **and** free a huge amount of memory just to handle this and that stuff.

### Case 2: web applications

*Heyyyy! We have Clojure, ClojureScript, Elm, React+Re* and friends! They all are already widely spread!*

Welllllll, let's take a closer look: we have to deal with databases, DOM, cookies, browser state & user input **a lot**.
No, <h1>A LOT</h1>. How a typical application in Clojure looks like?

*an example of Ring/Compojure app*

How does the React app looks like?

*an example of React component*

Well, this is a more edge-case example. See, we do have a mutable state here - DOM. And in order to update it,
React has to deal with it in a way that it *mutates it* (remove elements, change attributes - *consolidation* algorithms?).

Static typing is great? Well, here's the thing: if you have to deal with JSON - you're doomed.

*an example of trying to deal with a variable JSON structure*

APIs are everywhere and if one changes - you either handle the change to fill the gaps with default values, or throw an exception. Alternatively, you re-build the whole application on each API change. Now, some might say *"Oh, just use functors/lenses/monads/whatever!"* - but imagine a *trivial task*: 

*bad one:* you are given a structure of all known languages and known strings translations for each of those languages. The task is to translate a string in English or just use it as-is when there is no translation.

*better one:* you are given a map of 

Looks really easy, right? But can you implement it? Can you?

```
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















## References

[NASM tutorial](http://cs.lmu.edu/~ray/notes/nasmtutorial/)
[language-to-Assembly online compiler](https://godbolt.org/#)
[PE file description](https://habrahabr.ru/post/266831/)
[multicore assembly](https://stackoverflow.com/questions/980999/what-does-multicore-assembly-language-look-like#991191)
[OpenCL base knowledge](https://habrahabr.ru/post/345984/)
