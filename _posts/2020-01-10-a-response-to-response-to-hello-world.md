---
layout: post
title: 'A response to response to hello world'
date: '2020-01-10T15:10:24+11:00'
tags:
  - nasm
  - assembly
  - osx
  - go
  - performance
---

<div class="content-read-marker" data-id="{{page.title | slugify}}" data-fraction="0" data-page="{{page.title | escape}}"></div>

Recently I've received an email from StackOverflow newsletters with a link to a quite controversial (at first glance) blog, [A response to Hello World](https://www.doxsey.net/blog/a-response-to-hello-world) by Caleb Doxsey. This blog is a response to another curious read, [Hello world](https://drewdevault.com/2020/01/04/Slow.html) by Drew DeVault.

In the former article, author compared the performance of a tiny "Hello, World" program in Assembly to the same program in Go.
He then tried to optimize the program in Go to run faster and towards the end of an article comes up with a program that is faster than its Assembly counterpart.

<img alt="" data-src="/images/a-response-to-response-to-hello-world/hello-world-original-chart-min.webp">
<img alt="" data-src="/images/a-response-to-response-to-hello-world/hello-world-new-chart-min.webp">
<img alt="" data-src="/images/a-response-to-response-to-hello-world/hello-world-new-chart2-min.webp">

And this totally makes sense, since if you give it a good read, you will notice that author did optimize the program in Go but did not do that for the Assembly program.

I have decided to burn few hours of my life and jump onto this topic, since, in my opinion, author did not do a fair comparison.

<!--more-->

The original code Caleb has used is (I have added the imports and package declaration required to run the program):

### Go

```go
package main

import "strconv"
import "os"

func main() {
    n, _ := strconv.Atoi(os.Args[1])

    for i := 0; i < n; i++ {
        os.Stdout.Write([]byte("hello world\n"))
    }
}
```

### Assembly

```nasm
_start:
    mov rdi, [rsp + 16]
    call atoi

    mov r12, rax

.helloloop:
    mov rdx, len
    mov rsi, msg
    mov rdi, 1
    mov rax, 1
    syscall
    dec r12
    cmp r12, 0
    jg .helloloop

    mov rdi, 0
    mov rax, 60
    syscall
```

### Optimized Go

```go
package main

import "strconv"
import "os"
import "bufio"

func main() {
    n, _ := strconv.Atoi(os.Args[1])
    w := bufio.NewWriter(os.Stdout)
    defer w.Flush()
    for i := 0; i < n; i++ {
        w.Write([]byte("hello world"))
    }
}
```

What Caleb did was string concatenation in memory, creating a big string in memory and then calling the system function to print it out only once.
Indeed this is way more performant than making an IO syscall every iteration.

Since Caleb did not provide an equal program in Assembly, I decided to fill this gap.
I have spent quite some time (predominantly because I am quite rusty with Assembly at the moment and I forgot that different OSes use [different calling conventions](https://en.wikipedia.org/wiki/X86_calling_conventions)).
I came up with the code in Assembly that does exactly the same - allocates the memory, fills it with the repeated `"hello world"` string and then makes a syscall to print it to the console.

### My optimized Assembly

<div class="content-read-marker" data-id="{{page.title | slugify}}" data-fraction="25" data-page="{{page.title | escape}}"></div>

```nasm
;; compile me on OSX: nasm -fmacho64 helloworld.asm && ld helloworld.o -lSystem -macosx_version_min 10.13
;; compile me on Linux: nasm -felf64 helloworld.asm && ld helloworld.o # AND DO NOT FORGET TO ALIGN WITH THE SYSCALL CONVENTION BY USING STACK INSTEAD OF REGISTERS

global _main

extern _malloc, _puts, _atoi, _free

section .text

_main:
.load_counter:
    cmp rdi, 2
    jne .exit_failure

    mov r12, rdi
    mov r13, [rsi + 8]

    push rdi

    mov rdi, r13
    call _atoi

    mov r13, rax

    pop rdi

.calculate_memory_length:
    mov rdi, r13
    mov rax, len
    imul rdi, rax
    inc rdi
    mov r12, rdi

.allocate_memory:
    push rax

    mov rdi, r12
    xor rax, rax
    call _malloc

    test rax, rax
    jz .exit_failure

    mov r12, rax

    pop rax

.repeat_message:
    mov rcx, r13
    mov rdi, r12

.repeat_message_1:
    push rcx
    lea rsi, [rel msg]
    mov rcx, len
    cld

.copy_message:
    mov rax, [rsi]
    mov [rdi], rax
    inc rsi
    inc rdi
    dec rcx
    jnz .copy_message

.repeat_message_2:
    pop rcx
    dec rcx
    jnz .repeat_message_1

    xor rax, rax
    mov [rdi], rax

.print_string_builder:
    push rdi

    mov rdi, r12
    xor rax, rax
    call _puts

    pop rdi

.free_memory:
    push rdi

    mov rdi, r12
    call _free

    pop rdi

.exit_success:
    xor rdi, rdi
    jmp .exit

.exit_failure:
    mov rdi, 1

.exit:
    mov rax, 0x02000001
    syscall

section .data

msg: db "Hello, world", 10
len equ $ - msg
```

<div class="content-read-marker" data-id="{{page.title | slugify}}" data-fraction="50" data-page="{{page.title | escape}}"></div>

### My version with logs

As Caleb highlights in his blog, a lot of "burden" introduced by many languages (referring to the original blog by Drew DeVault) is the debugging and safety related information.

I went ahead and added few logs, memory deallocation and test for allocation success to the code in Assembly:

```nasm
;; compile me on OSX: nasm -fmacho64 helloworld.asm && ld helloworld.o -lSystem -macosx_version_min 10.13
;; compile me on Linux: nasm -felf64 helloworld.asm && ld helloworld.o # AND DO NOT FORGET TO ALIGN WITH THE SYSCALL CONVENTION BY USING STACK INSTEAD OF REGISTERS

global _main ;; a function declaration - an entry point; use `_start` for Linux

extern _malloc, _puts, _atoi, _printf

section .text

_main:
.load_counter:
    push r12
    push r13

    cmp rdi, 2 ;; check ARGC - program takes exactly ONE argument, so checking if ARGC == 2 (first one being program name)
    jne .exit_failure

;; log
    mov r12, rdi ;; store ARGC in R12
    mov r13, [rsi + 8] ;; store the ARGV[2] (by addressing it with *(ARGV + 1)) in R13

    push rsi ;; preserve all the used registers on stack - required by OSX calling convention
    push rdi
    push rdx

    mov rdi, log1 ;; copy format string to RDI
    mov rsi, r12 ;; copy first argument for _printf to RSI
    mov rdx, r13 ;; copy second argument for _printf to RDX
    xor rax, rax ;; zero out RAX to enable variable function arguments

    call _printf ;; syscall to printf()

    pop rdx ;; restore the registers' values
    pop rdi
    pop rsi

;; end log

    ; mov r12, rsi ;; pointer to argv (which is a pointer on its own)

    push rdi

    mov rdi, r13 ;; move the address of argv[1] (r12 + 0 => argv[0], r12 + 8 => argv[1]) to rdi
    call _atoi ;; convert char* argv[1] to int and store the result in rax

    mov r13, rax ;; store counter as int in r13

    pop rdi

;; log

    push rdi
    push rsi

    mov rdi, log2
    mov rsi, r13
    xor rax, rax
    call _printf

    pop rsi
    pop rdi

;; end log

.calculate_memory_length:
    ;; rdi = counter * len
    mov rdi, r13
    mov rax, len
    imul rdi, rax
    inc rdi ;; +1 for the terminal character (\0)
    mov r12, rdi ;; store total length in r12

.allocate_memory:
    push rax

    mov rdi, r12 ;; number of bytes to allocate
    xor rax, rax
    call _malloc

    test rax, rax ;; test the return code of _malloc syscall
    jz .exit_failure ;; if it is an error - go to exit(1)

    mov r12, rax ;; store address in r12

    pop rax

.repeat_message:
    mov rcx, r13 ;; set outer loop counter to COUNTER
    mov rdi, r12 ;; set destination address to the stored address from malloc call

.repeat_message_1:
    push rcx ;; push outer loop counter onto stack - we don't need it just yet, but will need it after the inner loop is done
    lea rsi, [rel msg] ;; load address of our message to repeat into the source address
    mov rcx, len ;; set inner loop counter to the length of a message
    cld ;; reset the string copying direction flag

.copy_message:
    mov rax, [rsi] ;; load the next byte from the source memory
    mov [rdi], rax ;; copy the loaded byte to the destination memory
    inc rsi ;; advance source pointer
    inc rdi ;; advamce destimation pointer
    dec rcx ;; decrease the inner loop counter
    jnz .copy_message

.repeat_message_2:
    pop rcx ;; restore the outer loop counter
    dec rcx ;; decrement the outer loop counter
    jnz .repeat_message_1

    xor rax, rax
    mov [rdi], rax ;; put \0 at current RDI

.print_string_builder:
    push rdi

    mov rdi, r12 ;; set first argument to the allocated memory start
    xor rax, rax
    call _puts

    pop rdi

.exit_success:
    xor rdi, rdi ;; exit argument - exit status - 0
    jmp .exit

.exit_failure:
    mov rdi, 1

.exit:
    mov rax, 0x02000001 ;; syscall id - exit; use `60` for Linux
    syscall

section .data

msg: db "Hello, world", 10
len equ $ - msg

log1: db "argc = %d, argv[1] = %s", 10, 0
log2: db "atoi = %d", 10, 0
```

## Comparison

Finally, I ran the aforementioned programs with the following arguments: `1M`, `2.5M`, `5M`, `7.5M`, `10M`, `12.5M`, `15M` and `17.5M` (where `M` stands for "million").

The results became more reasonable:

<img alt="" data-src="/images/a-response-to-response-to-hello-world/go_vs_asm_hello_world-min.webp">

<div class="content-read-marker" data-id="{{page.title | slugify}}" data-fraction="100" data-page="{{page.title | escape}}"></div>