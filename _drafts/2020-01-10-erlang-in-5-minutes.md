---
layout: post
date: '2020-01-10T15:24:24+11:00'
title: 'Erlang in 5 minutes'
---

## X = erlang, Y = 5, io:format("Learn ~s in ~s minutes", [ X, Y ]).

Welcome to a 5-minute Erlang introduction!

First things first:

* Erlang is a functional programming language
* `erl` is the REPL shell
* <kbd>Ctrl + G</kbd> and then <kbd>q</kbd> **or** `q().` to exit the shell
* `c(file_name).` to load the code from file

Now, to the language constructs:

* every statement has to end with a dot (`.`) symbol
* multiple statements are separated by comma (`,`) character
* constants start with a **C**apital **L**etter
* _atoms_ (yes, like in Ruby) start with **l**owercase **l**etter
* atoms are allowed to contain spaces (sic!), but then they have to be quoted: `'atoms are awesome'`
* atoms can also have the at sign (`@`)
* anonymous functions are defined as `fun() -> return_statement end.`
* last statement before `end` **must not** end with comma
* function signature is `function_name/number_of_arguments`: `factorial/1`, `format/2`
* comments start with percent (`%`) symbol
* lists are `[ TheOnlyElement ]` or `[ Head | TailElements ]`, where `TailElements` is also a list
* pattern matching just works `[ Head1 | Head2 | Tails ] = [ 1, 2, 3, 4 ]`
* strings are just `"strings"`
* tuples are quirky `{ elt1, elt2 }`
* maps are even weirder `#{ key => value }`
* operators are almost fine:
  * comparison: `<` (less than), `>` (greater than), `>=` (greater than or equals to), `=<` (equals to or less than), `==` (equals to), `/=` (not equals to), `=:=` (adds type checking on tops, JS equivalent of `===`)
  * assignment / pattern matching: `=`
  * math: `+`, `-`, `*`, `/`, `div`, `rem` (Pascal way)
  * typechecking: `is_atom/1`, `is_function/1`, `is_number/1`, `is_boolean/1`, `is_pid/1`, `is_list/1`, etc.
* list comprehentions are similar to lists: `[ N || N <- [ 1, 2, 3, 4, 5 ], N rem 2 == 0 ].`
* files are modules, defined with few statements:
  * `-module(module_name).`
  * `-export([ list_of_function_signatures ]).`
  * `-import(module_name, [ list_of_functions ]).`
* calling module's export be like `module_name:function_name(arguments).`

Quick recap:

```erlang
%%% module comment
-module(erlang_example).

% exporting the functions from the module
-export([ factorial/1 ]).

% importing the functions from the other module
-import(my_other_module, [ new_factorial/1 ]).

% constants with pattern matching - will assign the value to non-defined constant on success
X = 5.

% redefinition is prohibited
X = 10. % exception

% functions could also be constants
PlusOne = fun(X) -> X + 1 end.

% or, multiline
PlusTwo = fun(X) ->
  X + 2

  end.

% the standard way
bad_factorial(N) ->
  N * factorial(N - 1).

%% function documentation
% functions can have *guargs*, which are basically pattern matching
% also, function overloading is split with semicolon (;)
factorial(N) when N =< 1 -> 1;

% last overload ends with dot (.)
factorial(N) -> N * factorial(N - 1).

% some return types
string_fn() -> "hello, world!".
list_fn() -> [ 1, "elements could be of different types too", -3.14, atom ].
function_function() ->
  fun(X) -> X mod 2 end.
dict_fn() -> #{ "key" => -3.14 }.
tuple_fn() -> { elt1, elt2, "elt 3" }.

%% note how complex_function/1 has different definition than complex_function/2
complex_function(List) ->
  complex_function(List, 0).

% the power of pattern matching!
complex_function([ Head ], Acc) -> Acc + Head;

complex_function([ Head1 | Head2 ], Acc) -> Acc + Head1 + Head2;

complex_function([ Head | Tail ], Acc) ->
  complex_function(Tail, Acc + Head);

complex_function([], Acc) -> Acc.

fst({ First, _ }) -> First.

snd({ _, Second }) -> Second.

evens(List) -> [ X || X <- List, X rem 2 == 0 ].

keys(#{ Key => Value, _ => _ })
```

There also are few good resources on this: [https://learnxinyminutes.com/docs/erlang/](Learn X in Y minutes (where X = erlang)) and [http://erlang.org/doc/apps/inets/http_client.html](Official Erlang docs).