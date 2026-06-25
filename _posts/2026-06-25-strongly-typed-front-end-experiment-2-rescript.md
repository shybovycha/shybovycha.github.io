---
layout: post
title: 'Strongly-typed front-end: experiment 2, simple application, in ReasonML'
date: '2021-04-19T09:23:02+09:00'
tags: [reasonml, fsharp, javascript, development-tools, version-control, dotnet, frontend, functional-programming, git, react]
---

### Contents

1. [Introduction](/2021/04/19/strongly-typed-front-end-introduction.html)
2. [Experiment 1, hex2rgb](/2021/04/19/strongly-typed-front-end-experiment-1-experiment-1.html)
3. Experiment 2, simple application
    - [Elm](/2021/04/19/strongly-typed-front-end-experiment-2-experiment-2-elm.html)
    - [F#](/2021/04/19/strongly-typed-front-end-experiment-2-experiment-2-fsharp.html)
    - [PureScript & purescript-react-dom](/2021/04/19/strongly-typed-front-end-experiment-2-experiment-2-purescript.html)
    - [PureScript & Halogen](/2024/05/17/strongly-typed-front-end-experiment-2-experiment-2-purescript-halogen.html)
    - [ReasonML](/2021/04/19/strongly-typed-front-end-experiment-2-experiment-2-reasonml.html)
    - [Gleam](/2024/12/20/strongly-typed-front-end-experiment-2-experiment-2-gleam.html)
    - [ReScript **(you are here)**](/2026/06/25/strongly-typed-front-end-experiment-2-rescript.html)

Previously I made this experimental application in [ReasonML](/2021/04/19/strongly-typed-front-end-experiment-2-experiment-2-reasonml.html).
However, it was previously known as ReScript and there was no clear distinction between them.
There actually is a difference between ReasonML and ReScript, although they seem oh so similar:

* ReScript is a JavaScript/front-end focused language, trying to keep both of OCaml world and be as friendly to front-enders as possible
* ReasonML can compile to JavaScript, but it is a general purpose language (and it can be compiled to native code)

Starting the React Hooks example on [ReScript playground](https://rescript-lang.org/try/) website, you get this code:

```reason
module Counter = {
  @react.component
  let make = (~name) => {
    let (count, setCount) = React.useState(() => 0)

    <div>
      <p> {React.string(`${name} clicked ${Int.toString(count)} times`)} </p>
      <button onClick={_ => setCount(_ => count + 1)}>
        {React.string("Click me")}
      </button>
    </div>
  }
}
```

Starting off by defining the enum type for shape:

```reason
type Shape = Circle | Square;
```

And immediately getting an error:

```
Did you mean `shape` instead of `Shape`?
```

A very clear message, easy fix:

```reason
type shape = Circle | Square;
```

Now, add some markup:

```reason
@react.component
let make = (~name) => {
  let (_shape, setShape) = React.useState(() => None)
  let (value, setValue) = React.useState(() => 0.0)
  let (area, setArea) = React.useState(() => 0.0)

  <div>
    <select>
      <option value=""> Choose shape </option>
      <option value="circle"> Circle </option>
      <option value="square"> Square </option>
    </select>
    <input value={value} />
    <p> {React.string(string_of_float(area))} </p>
    <button>
      {React.string("Calculate")}
    </button>
  </div>
}
```

<!--more-->

And getting hit by another error:

```
[E] Line 12, column 24:

The variant constructor Choose can't be found.
  
  - If it's defined in another module or file, bring it into scope by:
    - Prefixing it with said module name: TheModule.Choose
    - Or specifying its type: let theValue: TheModule.theType = Choose
  - Constructors and modules are both capitalized. Did you want the latter?
    Then instead of let foo = Bar, try module Foo = Bar.
```

Not extremely helpful. But the exact same thing has happened with ReasonML. But this time there is no OCaml code to look at.
Let's just follow the example of ReasonML and use `React.string` here:

```jsx
<option>{React.string("Choose shape")}</option>
```

And having to replace this everywhere in the JSX:

```reason
@react.component
let make = () => {
  let (_shape, setShape) = React.useState(() => None)
  let (value, setValue) = React.useState(() => 0.0)
  let (area, setArea) = React.useState(() => 0.0)

  <div>
    <select>
      <option value="">{React.string("Choose shape")}</option>
      <option value="circle">{React.string("Circle")}</option>
      <option value="square">{React.string("Square")}</option>
    </select>
    <input value={value} />
    <p> {"Area:" ++ React.string(string_of_float(area))} </p>
    <button>
      {React.string("Calculate")}
    </button>
  </div>
};
```

And the next error is

```
We've found a bug for you!
OCaml preview 33:30-34

This has type:
  float
But somewhere wanted:
  string

You can convert a float to a string with string_of_float.
```

Seems that `value={value}` does not work out of the box again, since the `value` variable has the type `float` and this version of React expects it to be `string`.
The `string_of_float` function does not existing:

```
The value string_of_float can't be found
```

The [docs](https://rescript-lang.org/docs/manual/api/stdlib/float#value-toString) point out to the `Float.toString` method though:

```reason
<input value={Float.toString(value)} />
```

Okay, only the unused state setters left. Let’s implement the `shapeChanged` and the `calculateArea` helpers first:

```reason
let shapeChanged = (shapeStr: string): option(shape) =>
      switch shapeStr {
        | "circle" => Some(Circle)
        | "square" => Some(Square)
        | _ => None
      }

let calculateArea = (_shape: shape, value: float) =>
      switch _shape {
        | Circle => Math.Constants.pi * value * value
        | Square => value * value
      }
```

The only problem is the use of braces for type parameters:

```
Type parameters require angle brackets:
  option<shape>
```

Now, the last bit: connecting the component to the state:

```reason
<select onChange={ event => setShape(shapeChanged(event.target.value)) }>
```

This gives an error:

```
You're trying to access the record field target, but the thing you're trying to access it on is not a record. 
  
The type of the thing you're trying to access it on is:
  
  JsxEvent.Form.t
  

Only records have fields that can be accessed with dot notation.
```

The correct way is actually documented and shown in the playground code by default:

```reason
<select onChange={event => {
      let eventTarget = event->ReactEvent.Form.target
      let value = eventTarget["value"]
      setShape(_ => shapeChanged(value))
    }}>
```

Unfortunately, the number input is not as simple as raw React code:

```reason
<input value={Float.toString(area)} onChange={event => {
      let eventTarget = event->ReactEvent.Form.target
      let value = eventTarget["value"]
      setValue(_ => Float.parseFloat(value))
    }} />
```

And finally the actual calculation code:

```reason
<button onClick={_ => setArea(_ => _shape->Option.mapOr(0.0, s => calculateArea(s, value)))}>
```

Moreover, now the enum types can be either marked as `@unboxed` or have a catch-all string constructor to allow for implicit string coercion:

```reason
type shape = Circle | Square | Unknown(string)
```

This allows to drop `shapeChanged` converter altogether and just use `string` value as if it was a value of type `shape`:

```reason
let calculateArea = (_shape: shape, value: float) =>
  switch _shape {
  | Circle => Math.Constants.pi * value * value
  | Square => value * value
  | Unknown(_) => 0.0
  }

  let (_shape, setShape) = React.useState(() => Unknown(""))

  <select
    onChange={event => {
      let value = event->ReactEvent.Form.target(event)["value"]
      setShape(_ => value)
    }}
  >
    <option value=""> {React.string("Choose shape")} </option>
    <option value="circle"> {React.string("Circle")} </option>
    <option value="square"> {React.string("Square")} </option>
  </select>
      
  <button onClick={_ => setArea(_ => calculateArea(_shape, value))}>
    {React.string("Calculate")}
  </button>
```

Compared to the previous revision, it is actually much more consistent and has much fewer issues (like weird BuckleScript / OCaml / ReScript legacy):

```reason
module ShapeCalculator = {
  // semicolons are now optional
  // string conversions are available out-of-the-box
  type shape = Circle | Square | Unknown(string)

  let calculateArea = (_shape: shape, value: float) =>
        switch _shape {
          // Js.Math._PI is now Math.Constants.pi
          // float multiplication is now just `*` instead of `*.`
          | Circle => Math.Constants.pi * value * value
          | Square => value * value
          // this handles the Option.mapOr
          | Unknown(_) => 0.0
        }

  @react.component
  let make = () => {
    let (_shape, setShape) = React.useState(() => Unknown(""))
    let (value, setValue) = React.useState(() => 0.0)
    let (area, setArea) = React.useState(() => 0.0)

    <div>
      <select onChange={event => {
        let eventTarget = event->ReactEvent.Form.target
        let value = eventTarget["value"]
        // type string coercion
        setShape(_ => value)
      }}>
        <option value=""> {React.string("Choose shape")} </option>
        <option value="circle"> {React.string("Circle")} </option>
        <option value="square"> {React.string("Square")} </option>
      </select>
      // Js.Float.toString is now first-class Float.toString
      <input value={Float.toString(area)} onChange={event => {
        let eventTarget = event->ReactEvent.Form.target
        let value = eventTarget["value"]
        // float_of_string is now part of Float - Float.toString
        setValue(_ => Float.parseFloat(value))
      }} />
      <p> {React.string(`Area: ${Float.toString(area)}`)} </p>
      <button onClick={_ => setArea(_ => calculateArea(_shape, value))}>
        {React.string("Calculate")}
      </button>
    </div>
  }
}
```

Apparently, even adding external JS imports now is a breeze:

```reason
type school
type student
type payload = {
  student: student
}

@module external school: school = "school"
@send external getStudentById: (school, int) => student = "getStudentById"
```

TL;DR:

ReScript has become **much** more convenient, since it got rid of its intermediate transition state - with a mix of Belt, BuckleScript, raw OCaml and ReasonML pieces.
