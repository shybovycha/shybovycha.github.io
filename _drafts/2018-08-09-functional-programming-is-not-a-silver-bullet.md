---
title: 'Functional programming is not a silver bullet'
layout: post
date: '2018-08-08T22:10:00+10:00'
tags: [fp, cpp, haskell, programming, c++, programming-paradigms, functional-programming]
---

<!--
    TODO: show how same code in C / C++ / D / Haskell / Rust is compiled to ASM and what overhead does immutability and function purity implies.
-->

## Example #1: a sum of a number list

All the sources in the handy assembly view, with highlighting of corresponding code <-> assembly pieces, are [here](https://godbolt.org/g/1wdTVh).

### C, simple `for` loop

```c
int sum(int* a, int n) {
    int result = 0;

    for (int i = 0; i < n; ++i) {
        result += a[i];
    }

    return result;
}
```

```asm
sum: # @sum
  push rbp
  mov rbp, rsp
  mov qword ptr [rbp - 8], rdi
  mov dword ptr [rbp - 12], esi
  mov dword ptr [rbp - 16], 0
  mov dword ptr [rbp - 20], 0
.LBB0_1: # =>This Inner Loop Header: Depth=1
  mov eax, dword ptr [rbp - 20]
  cmp eax, dword ptr [rbp - 12]
  jge .LBB0_4
  mov rax, qword ptr [rbp - 8]
  movsxd rcx, dword ptr [rbp - 20]
  mov edx, dword ptr [rax + 4*rcx]
  add edx, dword ptr [rbp - 16]
  mov dword ptr [rbp - 16], edx
  mov eax, dword ptr [rbp - 20]
  add eax, 1
  mov dword ptr [rbp - 20], eax
  jmp .LBB0_1
.LBB0_4:
  mov eax, dword ptr [rbp - 16]
  pop rbp
  ret
```

### C, functional approach

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

### C++

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
    std::vector<int> a;
    a.push_back(1);
    a.push_back(2);
    a.push_back(3);
    reduce(a, 0, [](int a, int b) -> int { return a + b; });
}
```

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
  call std::vector<int, std::allocator<int> >::vector() [base object constructor]
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
  call std::function<int (int, int)>::~function() [base object destructor]
  lea rdi, [rbp - 80]
  call std::vector<int, std::allocator<int> >::~vector() [base object destructor]
  lea rdi, [rbp - 24]
  call std::vector<int, std::allocator<int> >::~vector() [base object destructor]
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
  call std::function<int (int, int)>::~function() [base object destructor]
.LBB7_10:
  lea rdi, [rbp - 80]
  call std::vector<int, std::allocator<int> >::~vector() [base object destructor]
.LBB7_11:
  lea rdi, [rbp - 24]
  call std::vector<int, std::allocator<int> >::~vector() [base object destructor]
  mov rdi, qword ptr [rbp - 40]
  call _Unwind_Resume
std::vector<int, std::allocator<int> >::vector() [base object constructor]: # @std::vector<int, std::allocator<int> >::vector() [base object constructor]
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::_Vector_base<int, std::allocator<int> >::_Vector_base() [base object constructor]
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
  call std::allocator<int>::~allocator() [base object destructor]
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
  call std::allocator<int>::~allocator() [base object destructor]
  jmp .LBB10_5
  mov ecx, edx
  mov qword ptr [rbp - 32], rax
  mov dword ptr [rbp - 36], ecx
  mov rax, qword ptr [rbp - 64] # 8-byte Reload
  mov rdi, rax
  call std::_Vector_base<int, std::allocator<int> >::~_Vector_base() [base object destructor]
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
  call std::_Function_base::_Function_base() [base object constructor]
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
  call std::_Function_base::~_Function_base() [base object destructor]
  jmp .LBB11_6
.LBB11_5:
  add rsp, 48
  pop rbp
  ret
.LBB11_6:
  mov rdi, qword ptr [rbp - 24]
  call _Unwind_Resume
std::function<int (int, int)>::~function() [base object destructor]: # @std::function<int (int, int)>::~function() [base object destructor]
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::_Function_base::~_Function_base() [base object destructor]
  add rsp, 16
  pop rbp
  ret
std::vector<int, std::allocator<int> >::~vector() [base object destructor]: # @std::vector<int, std::allocator<int> >::~vector() [base object destructor]
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
  call std::_Vector_base<int, std::allocator<int> >::~_Vector_base() [base object destructor]
  add rsp, 48
  pop rbp
  ret
  mov ecx, edx
  mov qword ptr [rbp - 16], rax
  mov dword ptr [rbp - 20], ecx
  mov rax, qword ptr [rbp - 32] # 8-byte Reload
  mov rdi, rax
  call std::_Vector_base<int, std::allocator<int> >::~_Vector_base() [base object destructor]
  mov rdi, qword ptr [rbp - 16]
  call __clang_call_terminate
std::_Function_base::~_Function_base() [base object destructor]: # @std::_Function_base::~_Function_base() [base object destructor]
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
std::_Vector_base<int, std::allocator<int> >::_Vector_base() [base object constructor]: # @std::_Vector_base<int, std::allocator<int> >::_Vector_base() [base object constructor]
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::_Vector_base<int, std::allocator<int> >::_Vector_impl::_Vector_impl() [base object constructor]
  add rsp, 16
  pop rbp
  ret
std::_Vector_base<int, std::allocator<int> >::_Vector_impl::_Vector_impl() [base object constructor]: # @std::_Vector_base<int, std::allocator<int> >::_Vector_impl::_Vector_impl() [base object constructor]
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  mov rax, rdi
  mov qword ptr [rbp - 16], rdi # 8-byte Spill
  mov rdi, rax
  call std::allocator<int>::allocator() [base object constructor]
  mov rax, qword ptr [rbp - 16] # 8-byte Reload
  mov qword ptr [rax], 0
  mov qword ptr [rax + 8], 0
  mov qword ptr [rax + 16], 0
  add rsp, 16
  pop rbp
  ret
std::allocator<int>::allocator() [base object constructor]: # @std::allocator<int>::allocator() [base object constructor]
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call __gnu_cxx::new_allocator<int>::new_allocator() [base object constructor]
  add rsp, 16
  pop rbp
  ret
__gnu_cxx::new_allocator<int>::new_allocator() [base object constructor]: # @__gnu_cxx::new_allocator<int>::new_allocator() [base object constructor]
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
std::_Vector_base<int, std::allocator<int> >::~_Vector_base() [base object destructor]: # @std::_Vector_base<int, std::allocator<int> >::~_Vector_base() [base object destructor]
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
  call std::_Vector_base<int, std::allocator<int> >::_Vector_impl::~_Vector_impl() [base object destructor]
  add rsp, 32
  pop rbp
  ret
  mov ecx, edx
  mov qword ptr [rbp - 16], rax
  mov dword ptr [rbp - 20], ecx
  mov rdi, qword ptr [rbp - 32] # 8-byte Reload
  call std::_Vector_base<int, std::allocator<int> >::_Vector_impl::~_Vector_impl() [base object destructor]
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
std::_Vector_base<int, std::allocator<int> >::_Vector_impl::~_Vector_impl() [base object destructor]: # @std::_Vector_base<int, std::allocator<int> >::_Vector_impl::~_Vector_impl() [base object destructor]
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call std::allocator<int>::~allocator() [base object destructor]
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
__gnu_cxx::new_allocator<int>::~new_allocator() [base object destructor]: # @__gnu_cxx::new_allocator<int>::~new_allocator() [base object destructor]
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
  call std::_Vector_base<int, std::allocator<int> >::_Vector_impl::~_Vector_impl() [base object destructor]
  mov rdi, qword ptr [rbp - 32]
  call _Unwind_Resume
std::allocator<int>::~allocator() [base object destructor]: # @std::allocator<int>::~allocator() [base object destructor]
  push rbp
  mov rbp, rsp
  sub rsp, 16
  mov qword ptr [rbp - 8], rdi
  mov rdi, qword ptr [rbp - 8]
  call __gnu_cxx::new_allocator<int>::~new_allocator() [base object destructor]
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
std::_Function_base::_Function_base() [base object constructor]: # @std::_Function_base::_Function_base() [base object constructor]
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

### Haskell, recursion

```hs
sum' :: [Int] -> Int -> Int
sum' [] acc = acc
sum' (a : as) acc = sum' as (a + acc)

sum :: [Int] -> Int
sum a = sum' a 0
```

yields

```asm
rYy_bytes:
        .asciz "main"
rYS_closure:
        .quad   ghczmprim_GHCziTypes_TrNameS_con_info
        .quad   rYy_bytes
rYT_bytes:
        .asciz "Example"
rYU_closure:
        .quad   ghczmprim_GHCziTypes_TrNameS_con_info
        .quad   rYT_bytes
Example_$trModule_closure:
        .quad   ghczmprim_GHCziTypes_Module_con_info
        .quad   rYS_closure+1
        .quad   rYU_closure+1
        .quad   3
Example_sum'_closure:
        .quad   Example_sum'_info
        .quad   0
s19g_info:
        leaq -40(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc19D
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movq 24(%rbx),%rbx
        movl $base_GHCziNum_zdfNumInt_closure,%r14d
        movq $stg_ap_pp_info,-40(%rbp)
        movq %rbx,-32(%rbp)
        movq %rax,-24(%rbp)
        addq $-40,%rbp
        jmp base_GHC.Num_+_info
.Lc19D:
        jmp *-16(%r13)
Example_sum'_info:
        leaq -16(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc19K
        movq $c19s_info,-16(%rbp)
        movq %r14,%rbx
        movq %rsi,-8(%rbp)
        addq $-16,%rbp
        testb $7,%bl
        jne .Lc19s
        jmp *(%rbx)
        .long   S19R_srt-(c19s_info)+0
        .long   0
        .quad   1
        .quad   12884901918
c19s_info:
.Lc19s:
        movq 8(%rbp),%rax
        movq %rbx,%rcx
        andl $7,%ecx
        cmpq $1,%rcx
        jne .Lc19H
        movq %rax,%rbx
        andq $-8,%rbx
        addq $16,%rbp
        jmp *(%rbx)
.Lc19H:
        addq $32,%r12
        cmpq 856(%r13),%r12
        ja .Lc19Q
        movq 6(%rbx),%rcx
        movq 14(%rbx),%rbx
        movq $s19g_info,-24(%r12)
        movq %rax,-8(%r12)
        movq %rcx,(%r12)
        leaq -24(%r12),%rax
        movq %rax,%rsi
        movq %rbx,%r14
        addq $16,%rbp
        jmp Example_sum'_info
.Lc19K:
        movl $Example_sum'_closure,%ebx
        jmp *-8(%r13)
.Lc19Q:
        movq $32,904(%r13)
        jmp stg_gc_unpt_r1
Example_sum_closure:
        .quad   Example_sum_info
        .quad   0
Example_sum_info:
        movl $stg_INTLIKE_closure+257,%esi
        jmp Example_sum'_info
S19R_srt:
        .quad   base_GHCziNum_zdfNumInt_closure
        .quad   Example_sum'_closure
```

### Haskell, fold

```hs
sum :: [Int] -> Int
sum a = foldl (+) 0 a
```

yields

```asm
r1qE_bytes:
        .asciz "main"
r1qP_closure:
        .quad   ghczmprim_GHCziTypes_TrNameS_con_info
        .quad   r1qE_bytes
r1qQ_bytes:
        .asciz "Example"
r1qR_closure:
        .quad   ghczmprim_GHCziTypes_TrNameS_con_info
        .quad   r1qQ_bytes
Example_$trModule_closure:
        .quad   ghczmprim_GHCziTypes_Module_con_info
        .quad   r1qP_closure+1
        .quad   r1qR_closure+1
        .quad   3
Example_sum_closure:
        .quad   Example_sum_info
        .quad   0
s1qU_info:
        leaq -16(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc1ra
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movl $base_GHCziNum_zdfNumInt_closure,%r14d
        addq $-16,%rbp
        jmp base_GHC.Num_+_info
.Lc1ra:
        jmp *-16(%r13)
Example_sum_info:
        leaq -32(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc1rd
        addq $16,%r12
        cmpq 856(%r13),%r12
        ja .Lc1rg
        movq $s1qU_info,-8(%r12)
        leaq -8(%r12),%rax
        movq %r14,%rbx
        movl $base_DataziFoldable_zdfFoldableZMZN_closure,%r14d
        movq $stg_ap_ppp_info,-32(%rbp)
        movq %rax,-24(%rbp)
        movq $stg_INTLIKE_closure+257,-16(%rbp)
        movq %rbx,-8(%rbp)
        addq $-32,%rbp
        jmp base_Data.Foldable_foldl_info
.Lc1rg:
        movq $16,904(%r13)
.Lc1rd:
        movl $Example_sum_closure,%ebx
        jmp *-8(%r13)
S1rh_srt:
        .quad   base_GHCziNum_zdfNumInt_closure
        .quad   Example_sum_closure
        .quad   base_DataziFoldable_zdfFoldableZMZN_closure
```

### Haskell, manual reduce

```hs
module Example where

reduce :: [Int] -> Int -> (Int -> Int -> Int) -> Int
reduce [] init reducer = init
reduce (x:xs) acc reducer = reduce xs (reducer x acc) reducer
```

```asm
__stginit_Example:
Example_reduce_closure:
  .quad Example_reduce_info
sGs_info:
  leaq -16(%rbp),%rax
  cmpq %r15,%rax
  jb .LcGO
  movq $stg_upd_frame_info,-16(%rbp)
  movq %rbx,-8(%rbp)
  movq 16(%rbx),%rax
  movq 24(%rbx),%rcx
  movq 32(%rbx),%rbx
  movq %rax,%rsi
  movq %rbx,%r14
  movq %rcx,%rbx
  addq $-16,%rbp
  jmp stg_ap_pp_fast
.LcGO:
  jmp *-16(%r13)
Example_reduce_info:
  leaq -24(%rbp),%rax
  cmpq %r15,%rax
  jb .LcGV
  movq $cGD_info,-24(%rbp)
  movq %r14,%rbx
  movq %rsi,-16(%rbp)
  movq %rdi,-8(%rbp)
  addq $-24,%rbp
  testb $7,%bl
  jne .LcGD
  jmp *(%rbx)
  .quad 2
  .quad 32
cGD_info:
.LcGD:
  movq 8(%rbp),%rax
  movq 16(%rbp),%rcx
  movq %rbx,%rdx
  andl $7,%edx
  cmpq $1,%rdx
  jne .LcGS
  movq %rax,%rbx
  andq $-8,%rbx
  addq $24,%rbp
  jmp *(%rbx)
.LcGS:
  addq $40,%r12
  cmpq 856(%r13),%r12
  ja .LcH1
  movq 6(%rbx),%rdx
  movq 14(%rbx),%rbx
  movq $sGs_info,-32(%r12)
  movq %rax,-16(%r12)
  movq %rcx,-8(%r12)
  movq %rdx,(%r12)
  leaq -32(%r12),%rax
  movq %rcx,%rdi
  movq %rax,%rsi
  movq %rbx,%r14
  addq $24,%rbp
  jmp Example_reduce_info
.LcGV:
  movl $Example_reduce_closure,%ebx
  jmp *-8(%r13)
.LcH1:
  movq $40,904(%r13)
  jmp stg_gc_unpt_r1
cHd_str:
  .byte 109
  .byte 97
  .byte 105
  .byte 110
  .byte 0
rw_closure:
  .quad ghczmprim_GHCziTypes_TrNameS_static_info
  .quad cHd_str
cHh_str:
  .byte 69
  .byte 120
  .byte 97
  .byte 109
  .byte 112
  .byte 108
  .byte 101
  .byte 0
rwQ_closure:
  .quad ghczmprim_GHCziTypes_TrNameS_static_info
  .quad cHh_str
Example_$trModule_closure:
  .quad ghczmprim_GHCziTypes_Module_static_info
  .quad rw_closure+1
  .quad rwQ_closure+1
  .quad 3
```

## Example 2: Levenshtein distance

### C, recursion

```c
#define max(a, b) a < b ? b : a
#define min2(a, b) a < b ? a : b
#define min3(a, b, c) min2(min2(a, b), c)

/**
 * @param s1 first string, to calculate distance from
 * @param i length of string s1
 * @param s2 second string, to calculate distance to
 * @param j length of string s2
 * @returns int the Levenshtein distance between strings s1 and s2
 */
int levenshtein(char *s1, int i, char *s2, int j) {
    if (!i) {
        return j;
    }

    if (!j) {
        return i;
    }

    int d1 = levenshtein(s1, i - 1, s2, j) + 1;
    int d2 = levenshtein(s1, i, s2, j - 1) + 1;
    int d3 = levenshtein(s1, i - 1, s2, j - 1) + (s1[i - 1] != s2[j - 1]);

    return min3(d1, d2, d3);
}
```

### C, recursion, with a tiny optimization

```c
#define max(a, b) a < b ? b : a
#define min2(a, b) a < b ? a : b
#define min3(a, b, c) min2(min2(a, b), c)

/**
 * @param s1 first string, to calculate distance from
 * @param i length of string s1
 * @param s2 second string, to calculate distance to
 * @param j length of string s2
 * @returns int the Levenshtein distance between strings s1 and s2
 */
int levenshtein(char *s1, int i, char *s2, int j) {
    if (!i) {
        return j;
    }

    if (!j) {
        return i;
    }

    if (s1[i - 1] == s2[j - 1]) {
        return levenshtein(s1, i - 1, s2, j - 1);
    }

    int d1 = levenshtein(s1, i - 1, s2, j) + 1;
    int d2 = levenshtein(s1, i, s2, j - 1) + 1;
    int d3 = levenshtein(s1, i - 1, s2, j - 1) + (s1[i - 1] != s2[j - 1]);

    return min3(d1, d2, d3);
}
```

In this case we add an early return condition, which adds ~30 lines of assembly output:

```asm
levenshtein(char*, int, char*, int):
        push    rbp
        mov     rbp, rsp
        sub     rsp, 48
        mov     QWORD PTR [rbp-24], rdi
        mov     DWORD PTR [rbp-28], esi
        mov     QWORD PTR [rbp-40], rdx
        mov     DWORD PTR [rbp-32], ecx
        cmp     DWORD PTR [rbp-28], 0
        jne     .L2
        mov     eax, DWORD PTR [rbp-32]
        jmp     .L3
.L2:
        cmp     DWORD PTR [rbp-32], 0
        jne     .L4
        mov     eax, DWORD PTR [rbp-28]
        jmp     .L3
.L4:
        mov     eax, DWORD PTR [rbp-28]
        cdqe
        lea     rdx, [rax-1]
        mov     rax, QWORD PTR [rbp-24]
        add     rax, rdx
        movzx   edx, BYTE PTR [rax]
        mov     eax, DWORD PTR [rbp-32]
        cdqe
        lea     rcx, [rax-1]
        mov     rax, QWORD PTR [rbp-40]
        add     rax, rcx
        movzx   eax, BYTE PTR [rax]
        cmp     dl, al
        jne     .L5
        mov     eax, DWORD PTR [rbp-32]
        lea     ecx, [rax-1]
        mov     eax, DWORD PTR [rbp-28]
        lea     esi, [rax-1]
        mov     rdx, QWORD PTR [rbp-40]
        mov     rax, QWORD PTR [rbp-24]
        mov     rdi, rax
        call    levenshtein(char*, int, char*, int)
        jmp     .L3
.L5:
        mov     eax, DWORD PTR [rbp-28]
        lea     esi, [rax-1]
        mov     ecx, DWORD PTR [rbp-32]
        mov     rdx, QWORD PTR [rbp-40]
        mov     rax, QWORD PTR [rbp-24]
        mov     rdi, rax
        call    levenshtein(char*, int, char*, int)
        add     eax, 1
        mov     DWORD PTR [rbp-4], eax
        mov     eax, DWORD PTR [rbp-32]
        lea     ecx, [rax-1]
        mov     rdx, QWORD PTR [rbp-40]
        mov     esi, DWORD PTR [rbp-28]
        mov     rax, QWORD PTR [rbp-24]
        mov     rdi, rax
        call    levenshtein(char*, int, char*, int)
        add     eax, 1
        mov     DWORD PTR [rbp-8], eax
        mov     eax, DWORD PTR [rbp-32]
        lea     ecx, [rax-1]
        mov     eax, DWORD PTR [rbp-28]
        lea     esi, [rax-1]
        mov     rdx, QWORD PTR [rbp-40]
        mov     rax, QWORD PTR [rbp-24]
        mov     rdi, rax
        call    levenshtein(char*, int, char*, int)
        mov     esi, eax
        mov     eax, DWORD PTR [rbp-28]
        cdqe
        lea     rdx, [rax-1]
        mov     rax, QWORD PTR [rbp-24]
        add     rax, rdx
        movzx   edx, BYTE PTR [rax]
        mov     eax, DWORD PTR [rbp-32]
        cdqe
        lea     rcx, [rax-1]
        mov     rax, QWORD PTR [rbp-40]
        add     rax, rcx
        movzx   eax, BYTE PTR [rax]
        cmp     dl, al
        setne   al
        movzx   eax, al
        add     eax, esi
        mov     DWORD PTR [rbp-12], eax
        mov     eax, DWORD PTR [rbp-4]
        cmp     eax, DWORD PTR [rbp-8]
        jl      .L6
        mov     eax, DWORD PTR [rbp-8]
        cmp     eax, DWORD PTR [rbp-12]
        jge     .L7
        mov     eax, DWORD PTR [rbp-4]
        cmp     eax, DWORD PTR [rbp-8]
        jge     .L8
        mov     eax, DWORD PTR [rbp-4]
        jmp     .L12
.L8:
        mov     eax, DWORD PTR [rbp-8]
        jmp     .L12
.L7:
        mov     eax, DWORD PTR [rbp-12]
        jmp     .L12
.L6:
        mov     eax, DWORD PTR [rbp-4]
.L12:
        nop
.L3:
        leave
        ret
```

But it saves us a lot more calculations

### C, loop

```c
#include <stdlib.h>

int levenshtein(char *s1, int len1, char *s2, int len2) {
    int *v0 = (int*) malloc((len1 + 1) * sizeof(int));
    int *v1 = (int*) malloc((len1 + 1) * sizeof(int));

    for (int i = 0; i <= len1; ++i) {
        v0[i] = i;
    }

    for (int i = 0; i < len2; ++i) {
        v1[0] = i + 1;

        for (int j = 0; j < len1; ++j) {
            int d1 = v0[j + 1] + 1;
            int d2 = v0[j] + 1;
            int d3 = v0[j] + (s1[i] != s2[j]);

            v1[j + 1] = min3(d1, d2, d3);
        }

        int *tmp = v0;
        v0 = v1;
        v1 = tmp;
    }

    return v0[len1];
}
```

### Haskell, recursion

```haskell
levenshtein' :: String -> Int -> String -> Int -> Int
levenshtein' s1 0 s2 j = j
levenshtein' s1 i s2 0 = i
levenshtein' s1 i s2 j = min (min d1 d2) d3
    where
        d1 = 1 + levenshtein' s1 (i - 1) s2 j
        d2 = 1 + levenshtein' s1 i s2 (j - 1)
        d3_delta = if (s1 !! (i - 1)) /= (s2 !! (j - 1)) then 1 else 0
        d3 = d3_delta + levenshtein' s1 (i - 1) s2 (j - 1)

levenshtein :: String -> String -> Int
levenshtein s1 s2 = levenshtein' s1 (length s1) s2 (length s2)
```

Yields

```asm
r2wR_bytes:
        .asciz "main"
r2xp_closure:
        .quad   ghczmprim_GHCziTypes_TrNameS_con_info
        .quad   r2wR_bytes
r2xq_bytes:
        .asciz "Example"
r2xr_closure:
        .quad   ghczmprim_GHCziTypes_TrNameS_con_info
        .quad   r2xq_bytes
Example_$trModule_closure:
        .quad   ghczmprim_GHCziTypes_Module_con_info
        .quad   r2xp_closure+1
        .quad   r2xr_closure+1
        .quad   3
Example_levenshtein'_closure:
        .quad   Example_levenshtein'_info
        .quad   0
s2x_info:
        leaq -40(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2yG
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movl $base_GHCziNum_zdfNumInt_closure,%r14d
        movq $stg_ap_pp_info,-40(%rbp)
        movq %rax,-32(%rbp)
        movq $stg_INTLIKE_closure+273,-24(%rbp)
        addq $-40,%rbp
        jmp base_GHCziNum_zm_info
.Lc2yG:
        jmp *-16(%r13)
s2xX_info:
        leaq -40(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2yN
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movl $base_GHCziNum_zdfNumInt_closure,%r14d
        movq $stg_ap_pp_info,-40(%rbp)
        movq %rax,-32(%rbp)
        movq $stg_INTLIKE_closure+273,-24(%rbp)
        addq $-40,%rbp
        jmp base_GHCziNum_zm_info
.Lc2yN:
        jmp *-16(%r13)
s2y0_info:
        leaq -16(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2yQ
        addq $48,%r12
        cmpq 856(%r13),%r12
        ja .Lc2yT
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movq 24(%rbx),%rcx
        movq 32(%rbx),%rdx
        movq 40(%rbx),%rbx
        movq $s2x_info,-40(%r12)
        movq %rbx,-24(%r12)
        leaq -40(%r12),%rbx
        movq $s2xX_info,-16(%r12)
        movq %rdx,(%r12)
        leaq -16(%r12),%rdx
        movq %rbx,%r8
        movq %rcx,%rdi
        movq %rdx,%rsi
        movq %rax,%r14
        addq $-16,%rbp
        jmp Example_levenshtein'_info
.Lc2yT:
        movq $48,904(%r13)
.Lc2yQ:
        jmp *-16(%r13)
s2xS_info:
        leaq -40(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2z7
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movl $base_GHCziNum_zdfNumInt_closure,%r14d
        movq $stg_ap_pp_info,-40(%rbp)
        movq %rax,-32(%rbp)
        movq $stg_INTLIKE_closure+273,-24(%rbp)
        addq $-40,%rbp
        jmp base_GHCziNum_zm_info
.Lc2z7:
        jmp *-16(%r13)
s2xT_info:
        leaq -16(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2za
        addq $24,%r12
        cmpq 856(%r13),%r12
        ja .Lc2zd
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movq 24(%rbx),%rbx
        movq $s2xS_info,-16(%r12)
        movq %rbx,(%r12)
        leaq -16(%r12),%rbx
        movq %rbx,%rsi
        movq %rax,%r14
        movl $base_GHCziList_znzn_closure,%ebx
        addq $-16,%rbp
        jmp stg_ap_pp_fast
.Lc2zd:
        movq $24,904(%r13)
.Lc2za:
        jmp *-16(%r13)
s2xP_info:
        leaq -40(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2zn
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movl $base_GHCziNum_zdfNumInt_closure,%r14d
        movq $stg_ap_pp_info,-40(%rbp)
        movq %rax,-32(%rbp)
        movq $stg_INTLIKE_closure+273,-24(%rbp)
        addq $-40,%rbp
        jmp base_GHCziNum_zm_info
.Lc2zn:
        jmp *-16(%r13)
s2xQ_info:
        leaq -16(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2zq
        addq $24,%r12
        cmpq 856(%r13),%r12
        ja .Lc2zt
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movq 24(%rbx),%rbx
        movq $s2xP_info,-16(%r12)
        movq %rbx,(%r12)
        leaq -16(%r12),%rbx
        movq %rbx,%rsi
        movq %rax,%r14
        movl $base_GHCziList_znzn_closure,%ebx
        addq $-16,%rbp
        jmp stg_ap_pp_fast
.Lc2zt:
        movq $24,904(%r13)
.Lc2zq:
        jmp *-16(%r13)
s2xV_info:
        leaq -48(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2zB
        addq $64,%r12
        cmpq 856(%r13),%r12
        ja .Lc2zE
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movq 24(%rbx),%rcx
        movq 32(%rbx),%rdx
        movq 40(%rbx),%rbx
        movq $s2xT_info,-56(%r12)
        movq %rcx,-40(%r12)
        movq %rbx,-32(%r12)
        leaq -56(%r12),%rbx
        movq $s2xQ_info,-24(%r12)
        movq %rax,-8(%r12)
        movq %rdx,(%r12)
        leaq -24(%r12),%rax
        movq $c2__info,-24(%rbp)
        movl $ghczmprim_GHCziClasses_zdfEqChar_closure,%r14d
        movq $stg_ap_pp_info,-48(%rbp)
        movq %rax,-40(%rbp)
        movq %rbx,-32(%rbp)
        addq $-48,%rbp
        jmp ghczmprim_GHCziClasses_zsze_info
        .quad   0
        .quad   30
c2__info:
        andl $7,%ebx
        cmpq $1,%rbx
        jne .Lc2zy
        movl $stg_INTLIKE_closure+257,%ebx
        addq $8,%rbp
        jmp *(%rbp)
.Lc2zy:
        movl $stg_INTLIKE_closure+273,%ebx
        addq $8,%rbp
        jmp *(%rbp)
.Lc2zE:
        movq $64,904(%r13)
.Lc2zB:
        jmp *-16(%r13)
s2y1_info:
        leaq -40(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2zM
        addq $96,%r12
        cmpq 856(%r13),%r12
        ja .Lc2zP
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movq 24(%rbx),%rcx
        movq 32(%rbx),%rdx
        movq 40(%rbx),%rbx
        movq $s2y0_info,-88(%r12)
        movq %rax,-72(%r12)
        movq %rcx,-64(%r12)
        movq %rdx,-56(%r12)
        movq %rbx,-48(%r12)
        leaq -88(%r12),%rsi
        movq $s2xV_info,-40(%r12)
        movq %rax,-24(%r12)
        movq %rcx,-16(%r12)
        movq %rdx,-8(%r12)
        movq %rbx,(%r12)
        leaq -40(%r12),%rax
        movl $base_GHCziNum_zdfNumInt_closure,%r14d
        movq $stg_ap_pp_info,-40(%rbp)
        movq %rax,-32(%rbp)
        movq %rsi,-24(%rbp)
        addq $-40,%rbp
        jmp base_GHCziNum_zp_info
.Lc2zP:
        movq $96,904(%r13)
.Lc2zM:
        jmp *-16(%r13)
s2xK_info:
        leaq -40(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2A7
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movl $base_GHCziNum_zdfNumInt_closure,%r14d
        movq $stg_ap_pp_info,-40(%rbp)
        movq %rax,-32(%rbp)
        movq $stg_INTLIKE_closure+273,-24(%rbp)
        addq $-40,%rbp
        jmp base_GHCziNum_zm_info
.Lc2A7:
        jmp *-16(%r13)
s2xL_info:
        leaq -16(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2Aa
        addq $24,%r12
        cmpq 856(%r13),%r12
        ja .Lc2Ad
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movq 24(%rbx),%rcx
        movq 32(%rbx),%rdx
        movq 40(%rbx),%rbx
        movq $s2xK_info,-16(%r12)
        movq %rbx,(%r12)
        leaq -16(%r12),%rbx
        movq %rbx,%r8
        movq %rcx,%rdi
        movq %rdx,%rsi
        movq %rax,%r14
        addq $-16,%rbp
        jmp Example_levenshtein'_info
.Lc2Ad:
        movq $24,904(%r13)
.Lc2Aa:
        jmp *-16(%r13)
s2xM_info:
        leaq -40(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2Af
        addq $48,%r12
        cmpq 856(%r13),%r12
        ja .Lc2Ai
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movq 24(%rbx),%rcx
        movq 32(%rbx),%rdx
        movq 40(%rbx),%rbx
        movq $s2xL_info,-40(%r12)
        movq %rax,-24(%r12)
        movq %rcx,-16(%r12)
        movq %rdx,-8(%r12)
        movq %rbx,(%r12)
        leaq -40(%r12),%rax
        movl $base_GHCziNum_zdfNumInt_closure,%r14d
        movq $stg_ap_pp_info,-40(%rbp)
        movq $stg_INTLIKE_closure+273,-32(%rbp)
        movq %rax,-24(%rbp)
        addq $-40,%rbp
        jmp base_GHCziNum_zp_info
.Lc2Ai:
        movq $48,904(%r13)
.Lc2Af:
        jmp *-16(%r13)
s2xF_info:
        leaq -40(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2Aw
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movl $base_GHCziNum_zdfNumInt_closure,%r14d
        movq $stg_ap_pp_info,-40(%rbp)
        movq %rax,-32(%rbp)
        movq $stg_INTLIKE_closure+273,-24(%rbp)
        addq $-40,%rbp
        jmp base_GHCziNum_zm_info
.Lc2Aw:
        jmp *-16(%r13)
s2xG_info:
        leaq -16(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2Az
        addq $24,%r12
        cmpq 856(%r13),%r12
        ja .Lc2AC
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movq 24(%rbx),%rcx
        movq 32(%rbx),%rdx
        movq 40(%rbx),%rbx
        movq $s2xF_info,-16(%r12)
        movq %rdx,(%r12)
        leaq -16(%r12),%rdx
        movq %rbx,%r8
        movq %rcx,%rdi
        movq %rdx,%rsi
        movq %rax,%r14
        addq $-16,%rbp
        jmp Example_levenshtein'_info
.Lc2AC:
        movq $24,904(%r13)
.Lc2Az:
        jmp *-16(%r13)
s2xH_info:
        leaq -40(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2AE
        addq $48,%r12
        cmpq 856(%r13),%r12
        ja .Lc2AH
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movq 24(%rbx),%rcx
        movq 32(%rbx),%rdx
        movq 40(%rbx),%rbx
        movq $s2xG_info,-40(%r12)
        movq %rax,-24(%r12)
        movq %rcx,-16(%r12)
        movq %rdx,-8(%r12)
        movq %rbx,(%r12)
        leaq -40(%r12),%rax
        movl $base_GHCziNum_zdfNumInt_closure,%r14d
        movq $stg_ap_pp_info,-40(%rbp)
        movq $stg_INTLIKE_closure+273,-32(%rbp)
        movq %rax,-24(%rbp)
        addq $-40,%rbp
        jmp base_GHCziNum_zp_info
.Lc2AH:
        movq $48,904(%r13)
.Lc2AE:
        jmp *-16(%r13)
s2xN_info:
        leaq -40(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2AJ
        addq $96,%r12
        cmpq 856(%r13),%r12
        ja .Lc2AM
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movq 24(%rbx),%rcx
        movq 32(%rbx),%rdx
        movq 40(%rbx),%rbx
        movq $s2xM_info,-88(%r12)
        movq %rax,-72(%r12)
        movq %rcx,-64(%r12)
        movq %rdx,-56(%r12)
        movq %rbx,-48(%r12)
        leaq -88(%r12),%rsi
        movq $s2xH_info,-40(%r12)
        movq %rax,-24(%r12)
        movq %rcx,-16(%r12)
        movq %rdx,-8(%r12)
        movq %rbx,(%r12)
        leaq -40(%r12),%rax
        movl $ghczmprim_GHCziClasses_zdfOrdInt_closure,%r14d
        movq $stg_ap_pp_info,-40(%rbp)
        movq %rax,-32(%rbp)
        movq %rsi,-24(%rbp)
        addq $-40,%rbp
        jmp ghczmprim_GHCziClasses_min_info
.Lc2AM:
        movq $96,904(%r13)
.Lc2AJ:
        jmp *-16(%r13)
Example_levenshtein'_info:
        leaq -32(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2AO
        movq $c2yf_info,-32(%rbp)
        movq %rsi,%rbx
        movq %r14,-24(%rbp)
        movq %rdi,-16(%rbp)
        movq %r8,-8(%rbp)
        addq $-32,%rbp
        testb $7,%bl
        jne .Lc2yf
        jmp *(%rbx)
        .long   S2B0_srt-(c2yf_info)+0
        .long   0
        .quad   3
        .quad   133143986206
c2yf_info:
.Lc2yf:
        movq 24(%rbp),%rax
        movq 7(%rbx),%rcx
        testq %rcx,%rcx
        jne .Lc2AS
        movq %rax,%rbx
        andq $-8,%rbx
        addq $32,%rbp
        jmp *(%rbx)
.Lc2AS:
        movq $c2ym_info,(%rbp)
        movq %rbx,%rcx
        movq %rax,%rbx
        movq %rcx,24(%rbp)
        testb $7,%bl
        jne .Lc2ym
        jmp *(%rbx)
        .long   S2B0_srt-(c2ym_info)+0
        .long   0
        .quad   3
        .quad   133143986206
c2ym_info:
.Lc2ym:
        movq 8(%rbp),%rax
        movq 16(%rbp),%rcx
        movq 24(%rbp),%rdx
        addq $96,%r12
        cmpq 856(%r13),%r12
        ja .Lc2AV
        movq 7(%rbx),%rsi
        testq %rsi,%rsi
        jne .Lc2AX
        addq $-96,%r12
        movq %rdx,%rbx
        andq $-8,%rbx
        addq $32,%rbp
        jmp *(%rbx)
.Lc2AO:
        movl $Example_levenshtein'_closure,%ebx
        jmp *-8(%r13)
.Lc2AV:
        movq $96,904(%r13)
        jmp stg_gc_unpt_r1
.Lc2AX:
        movq $s2y1_info,-88(%r12)
        movq %rax,-72(%r12)
        movq %rcx,-64(%r12)
        movq %rdx,-56(%r12)
        movq %rbx,-48(%r12)
        leaq -88(%r12),%rsi
        movq $s2xN_info,-40(%r12)
        movq %rax,-24(%r12)
        movq %rcx,-16(%r12)
        movq %rdx,-8(%r12)
        movq %rbx,(%r12)
        leaq -40(%r12),%rax
        movl $ghczmprim_GHCziClasses_zdfOrdInt_closure,%r14d
        movq $stg_ap_pp_info,8(%rbp)
        movq %rax,16(%rbp)
        movq %rsi,24(%rbp)
        addq $8,%rbp
        jmp ghczmprim_GHCziClasses_min_info
Example_levenshtein_closure:
        .quad   Example_levenshtein_info
        .quad   0
s2y5_info:
        leaq -32(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2DP
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movl $base_DataziFoldable_zdfFoldableZMZN_closure,%r14d
        movq $stg_ap_p_info,-32(%rbp)
        movq %rax,-24(%rbp)
        addq $-32,%rbp
        jmp base_DataziFoldable_length_info
.Lc2DP:
        jmp *-16(%r13)
s2y4_info:
        leaq -32(%rbp),%rax
        cmpq %r15,%rax
        jb .Lc2DW
        movq $stg_upd_frame_info,-16(%rbp)
        movq %rbx,-8(%rbp)
        movq 16(%rbx),%rax
        movl $base_DataziFoldable_zdfFoldableZMZN_closure,%r14d
        movq $stg_ap_p_info,-32(%rbp)
        movq %rax,-24(%rbp)
        addq $-32,%rbp
        jmp base_DataziFoldable_length_info
.Lc2DW:
        jmp *-16(%r13)
Example_levenshtein_info:
        addq $48,%r12
        cmpq 856(%r13),%r12
        ja .Lc2E2
        movq $s2y5_info,-40(%r12)
        movq %rsi,-24(%r12)
        leaq -40(%r12),%rax
        movq $s2y4_info,-16(%r12)
        movq %r14,(%r12)
        leaq -16(%r12),%rbx
        movq %rax,%r8
        movq %rsi,%rdi
        movq %rbx,%rsi
        jmp Example_levenshtein'_info
.Lc2E2:
        movq $48,904(%r13)
        movl $Example_levenshtein_closure,%ebx
        jmp *-8(%r13)
S2B0_srt:
        .quad   base_GHCziNum_zdfNumInt_closure
        .quad   Example_levenshtein'_closure
        .quad   base_GHCziList_znzn_closure
        .quad   ghczmprim_GHCziClasses_zdfEqChar_closure
        .quad   ghczmprim_GHCziClasses_zdfOrdInt_closure
        .quad   base_DataziFoldable_zdfFoldableZMZN_closure
        .quad   Example_levenshtein_closure
```
