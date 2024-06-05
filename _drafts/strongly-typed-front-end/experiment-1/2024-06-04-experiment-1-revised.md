---
layout: post
title: 'Experiment #1, revised'
date: '2024-06-04T22:13:00+10:00'
---

## TypeScript

TypeScript got pretty much down to zero overhead - the code is very close to ESNext:

```ts
class RGBType {
  constructor(public r: number, public g: number, public b: number) {}
}

const hex2rgb = (hex: string): RGBType | undefined => {
  const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);

  if (!result) {
    return undefined;
  }

  const [ , r, g, b ] = result;

  return new RGBType(parseInt(r, 16),parseInt(g, 16),parseInt(b, 16));
}

export { type RGBType, hex2rgb };
```

The above TypeScript code produces the following ES6:

```js
class RGBType {
    constructor(r, g, b) {
        this.r = r;
        this.g = g;
        this.b = b;
    }
}
const hex2rgb = (hex) => {
    const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
    if (!result) {
        return undefined;
    }
    const [, r, g, b] = result;
    return new RGBType(parseInt(r, 16), parseInt(g, 16), parseInt(b, 16));
};
export { hex2rgb };
```

## ReasonML

ReasonML still exists, but lacks good documentation just as before.
By reading the [documentation](https://reasonml.github.io/docs/en/compiling-to-js-with-melange) trying to re-write the `hex2rgb` function,
I realized it now relies on a new backend, [Melange](https://melange.re/), with its own [documentation](https://melange.re/v4.0.0/api.html).

With a bit of struggling, I came up with the following implementation:

```reason
type rgb = {
  r: int,
  g: int,
  b: int,
};

let parse_hex = s => int_of_string("0x" ++ s);

let re = Js.Re.fromString({|^#?([a-f0-9]{2})([a-f0-9]{2})([a-f0-9]{2})$|})

let fn0: string => option(Js.Re.result) =
  hex_str => Js.Re.exec(hex_str, re);

let fn1: Js.Re.result => array(Js.Nullable.t(string)) =
  re_result => Js.Re.captures(re_result);

let fn2: array(Js.Nullable.t(string)) => array(option(string)) =
  array_of_nullables =>
    Belt.Array.map(array_of_nullables, Js.Nullable.toOption);

let fn3: array(option(string)) => option(array(string)) =
  array_of_options =>
    switch (array_of_options) {
    | [|Some(a), Some(b), Some(c)|] => Some([|a, b, c|])
    | _ => None
    };

let fn4: array(string) => array(int) =
  array_of_hex => Belt.Array.map(array_of_hex, parse_hex);

let fn5: array(int) => option(rgb) =
  array_of_ints =>
    switch (array_of_ints) {
    | [|r, g, b|] => Some({r, g, b})
    | _ => None
    };

let parse_rgb = hex_str =>
  fn0(hex_str)
  ->Belt.Option.map(fn1)
  ->Belt.Option.map(fn2)
  ->Belt.Option.flatMap(fn3)
  ->Belt.Option.map(fn4)
  ->Belt.Option.flatMap(fn5);
```

This code is quite different from OCaml, since this code is compiled to JS. The difference is that one can not use the standard ReasonML library,
but has to rely on Melange and JS-specific APIs (instead of `Str` which is a built-in regex library one has to use `Js.Re`; abundance of `Js.Nullable`, etc.).

Note that it is not required to provide type annotations for functions - OCaml / ReasonML is **very good** at inferring types on its own.
I have done it so that I can see what goes wrong without the need to hover over each function all while trying to demystify the compiler errors in the
[playground 1](https://reasonml.github.io/en/try) or [playground 2](https://melange.re/v2.0.0/playground/).

So this is the very same code, just without type annotations:

```reason
type rgb = {
  r: int,
  g: int,
  b: int,
};

let parse_hex = s => int_of_string("0x" ++ s);

let re = Js.Re.fromString({|^#?([a-f0-9]{2})([a-f0-9]{2})([a-f0-9]{2})$|});

let fn0 = hex_str => Js.Re.exec(hex_str, re);

let fn1 = re_result => Js.Re.captures(re_result);

let fn2 = array_of_nullables =>
  Belt.Array.map(array_of_nullables, Js.Nullable.toOption);

let fn3 = array_of_options =>
  switch (array_of_options) {
  | [|Some(a), Some(b), Some(c)|] => Some([|a, b, c|])
  | _ => None
  };

let fn4 = array_of_hex => Belt.Array.map(array_of_hex, parse_hex);

let fn5 = array_of_ints =>
  switch (array_of_ints) {
  | [|r, g, b|] => Some({r, g, b})
  | _ => None
  };

let parse_rgb = hex_str =>
  fn0(hex_str)
  ->Belt.Option.map(fn1)
  ->Belt.Option.map(fn2)
  ->Belt.Option.flatMap(fn3)
  ->Belt.Option.map(fn4)
  ->Belt.Option.flatMap(fn5);
```

Also, note that there are at least two conflicting libraries - Belt and ReasonML / OCaml standard library.
There are conflicts within the libraries itself (or rather a questionable design decisions, if you ask me):

* `Option.map` has a signature `('a => 'b, option('a)) => option('b)`, meaning it takes a function and an option (in that order)
* `Belt.Option.map` has a signature `(option('a), 'a => 'b) => option('b)` - it takes an option and a function (in that order)
* `Array.map` also takes a function and an array (in that order) whereas `Belt.Array.map` has its arguments reversed
* `Option.flatMap` and `Array.flatMap` (stdlib) do not exist, but exist as `Belt.Option.flatMap` and `Belt.Array.flatMap` effectively making another stdlib
* `Js.Array.map` seems to introduce a third stdlib, specifically for JS; its signature takes a function and then an array (in that order)
* `Str` stdlib module (according to [ReasonML documentation](https://reasonml.github.io/api/Str.html)) does not really provide string functions, but regular expressions; contrary, `String` stdlib module provides string functions; but it does not even exist in [Melange](https://melange.re/v4.0.0/api/re/melange/Stdlib/)
* `Js.String` module provides JS-specific string functions

There is a [helpful table](https://github.com/melange-re/melange?tab=readme-ov-file#how-does-this-project-relate-to-other-tools) to get a tiny bit less confused by all the names used.

Running ReasonML locally (in Docker container) is also a bit of a challenge.
Compiling different pieces of instructions together:

* install OPAM (package manager) - either run it in Docker (`ocaml/opam`) or use a script `bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"` (via [Melange for React devs](https://react-book.melange.re/installation/#opam))
* install Melange and ReasonML with OPAM: `opam install melange reason`
* initialize barebones project with Dune (build tool): `dune init project NAME`
* change the Dune project files as follows (from [early Melange getting started guide](https://melange.re/v1.0.0/build-system/)):

```
project_name/
├── dune-project
├── lib
│   ├── dune
│   └── hex2rgb.re
└── emit
    └── dune
```

`dune-project` file in the project root (via [Melange project template](https://github.com/melange-re/melange-opam-template/blob/main/dune-project)) to include the following:

```lisp
(lang dune 3.15)

;; THIS IS IMPORTANT: this is NOT the version of Melange itself, but rather Dune plugin
;; the only supported version as of June 2024 is 0.1
(using melange 0.1)
```

`emit/dune` file to include the following:

```lisp
(melange.emit
  (target output)
  (libraries hex2rgb)
  (module_systems es6))
```

`lib/dune` to have:

```lisp
(library
  (name hex2rgb)
  (modes melange)
  (libraries melange.belt)
  (preprocess
    (pps melange.ppx)))
```

Put your code in `lib/hex2rgb.re` (file extension is `.re` for ReasonML and `.ml` for OCaml).

If you build the project now, you may encounter the following error:

```
File "lib/hex2rgb.re", line 12, characters 9-19:
12 |   str => Js.Re.exec(str, re);
              ^^^^^^^^^^
Error (warning 6 [labels-omitted]): label str was omitted in the application of this function.
```

This is due to the conflicting declarations of the `Js.Re.exec` function in many stdlibs - it would work in playground, but will fail in this simple project.
Change it to have labelled argument `~str`:

```reason
let fn0: string => option(Js.Re.result) =
  str => Js.Re.exec(~str, re);
```

Finally, run Dune to build the project: `opam exec dune build`.
The generated files will be in the `_build/default/emit/output/`.

Then you can / need to bundle the output without messing up the generated `node_modules` directory, containing Melange' files.
I use `esbuils`:

```shell
$ esbuild lib/hex2rgb.js --bundle --platform=node --outdir=dist

  dist/hex2rgb.js  9.8kb

⚡ Done in 12ms
$ esbuild lib/hex2rgb.js --bundle --platform=node --outdir=dist --minify

  dist/hex2rgb.js  4.1kb

⚡ Done in 14ms
```

If we want a shorter code, we can inline some of the functions:

```reason
let parse_rgb = hex_str =>
  hex_str
  ->Js.Re.exec(re)
  ->Belt.Option.map(Js.Re.captures)
  ->Belt.Option.map(Array.map(Js.Nullable.toOption))
  ->Belt.Option.flatMap(fn3)
  ->Belt.Option.map(Array.map(parse_hex))
  ->Belt.Option.flatMap(fn5);
```

Note how `Belt.Array.map` became `Array.map` - this is where the order of arguments in the stdlib functions starts to matter.

The functions with pattern matching are slightly clunkier to inline:

```reason
let parse_rgb1 = hex_str =>
  hex_str
  ->Js.Re.exec(re)
  ->Belt.Option.map(Js.Re.captures)
  ->Belt.Option.map(Array.map(Js.Nullable.toOption))
  ->(array_of_options =>
  switch (array_of_options) {
  | Some([|Some(a), Some(b), Some(c)|]) => Some([|a, b, c|])
  | _ => None
  })
  ->Belt.Option.map(Array.map(parse_hex))
  ->(array_of_ints =>
  switch (array_of_ints) {
  | Some([|r, g, b|]) => Some({r, g, b})
  | _ => None
  });
```

On top of that, we can reduce this even further by inlining the `parse_hex` call and constructing the `rgb` record:

```reason
let parse_rgb1 = hex_str =>
  hex_str
  ->Js.Re.exec(re)
  ->Belt.Option.map(Js.Re.captures)
  ->Belt.Option.map(Array.map(Js.Nullable.toOption))
  ->(array_of_options =>
  switch (array_of_options) {
  | Some([|Some(a), Some(b), Some(c)|]) => Some({ r: parse_hex(a), g: parse_hex(b), b: parse_hex(c) })
  | _ => None
  });
```

This produces `9.3KB` of code. Interestingly enough, one third of it is parsing the numbers - since there is no stdlib function to parse an integer with a given number base. Replacing it with `parseInt(s, 16)` in the bundle directly cuts it down to `5.7KB`.

All things considered, my first impression of ReasonML is a mixed bag - it is really good at inferring types, has reasonably helpful compile-time error messages (not shown here)
and produces a rather small bundle. On the other hand, dealing with the documentation and trying to get your bearings is extremely clunky - the docs are all
over the place, spread across multiple projects (either not cross-linked or linked to outdated). The standard libraries do make life easier once you adjust,
but they are quite inconsistent.

The generated code exports a `parse_hex(str)` function, so we can use it later for the tests.

## F#

Back in 2021 the bundle produced by F# was around `40KB`. The code looked like this:

```fsharp
module Hex2Rgb

open System.Text.RegularExpressions

type RGBType = { r: int16; g: int16; b: int16 }

let hex2rgb (hex: string) =
    let m = Regex.Match(hex, "^#?([a-f0-9]{2})([a-f0-9]{2})([a-f0-9]{2})$")
    if m.Success then
        m.Groups
        |> Seq.cast<Group>
        |> Seq.skip 1 // zero capture group is always the full string, when it matches
        |> Seq.map (fun m -> m.Value)
        |> Seq.map (fun x -> System.Convert.ToInt16(x, 16))
        |> Seq.toList
        |> (function
            | r :: g :: b :: [] -> Some { r = r; g = g; b = b }
            | _ -> None)
    else None
```

Original solution contained quite a few sequence operations on the match result, so how about streamlining them:

```fsharp
let parse (g: Group) = System.Convert.ToInt16(g.Value, 16)

let hex2rgb (hex: string) =
    let m = Regex.Match(hex, "^#?([a-f0-9]{2})([a-f0-9]{2})([a-f0-9]{2})$")
    if m.Success then
        m.Groups
        |> Seq.cast<Group>
        |> Seq.toList
        |> (function
            | r_g :: g_g :: b_g :: [] -> Some { r = parse(r_g); g = parse(g_g); b = parse(b_g) }
            | _ -> None)
    else None
```

This sheds off barely `5KB` of the bundle size, down to `23.7KB`, which is still a lot.

The output exports contain `hex2rgb` function and the `RGBType` class, they will be used later for the tests.
