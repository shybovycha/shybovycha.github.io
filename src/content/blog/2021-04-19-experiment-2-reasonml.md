---
layout: post
title: 'Strongly-typed front-end: experiment 2, simple application, in ReasonML'
date: '2021-04-19T09:23:02+09:00'
---

### Contents

1. [Introduction](/strongly-typed-front-end/2021/04/19/introduction.html)
2. [Experiment 1, darken_color](/strongly-typed-front-end/experiment-1/2021/04/19/experiment-1.html)
3. Experiment 2, simple application
    - [Elm](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-elm.html)
    - [F#](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-fsharp.html)
    - [PureScript & purescript-react-dom](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-purescript.html)
    - [PureScript & Halogen](/strongly-typed-front-end/experiment-2/2024/05/17/experiment-2-purescript-halogen.html)
    - [ReasonML **(you are here)**](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-reasonml.html)

For the sake of experiment, I have decided to implement the very same application in ReasonML → ReScript by Facebook.

Starting the React Hooks example on [Try ReasonML](https://reasonml.github.io/en/try) website, you get this code, which resembles some of the React features, just in a slightly weird syntax:

```ocaml
[@bs.config {jsx: 3}];

module Counter = {
  [@react.component]
  let make = (~name) => {
    let (count, setCount) = React.useState(() => 0);

    <div>
      <p> {React.string(name ++ " clicked " ++ string_of_int(count) ++ " times")} </p>
      <button onClick={_ => setCount(_ => count + 1)}>
        {React.string("Click me")}
      </button>
    </div>
  };
};

ReactDOMRe.renderToElementWithId(<Counter name="Counter" />, "preview");
```

Starting off by defining the enum type for shape:

```ocaml
type Shape = Circle | Square;
```

And immediately getting an error:

```
Line 4:8-12 A type name must start with a lower-case letter or an underscore
```

That one is easy to fix:

```ocaml
type shape = Circle | Square;
```

Now, add some markup:

```ocaml
[@react.component]
let make = (~name) => {
  let (_shape, setShape) = React.useState(() => None);
  let (value, setValue) = React.useState(() => 0.0);
  let (area, setArea) = React.useState(() => 0.0);

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
};
```

<!--more-->

And getting hit by another error:

```
We've found a bug for you!
OCaml preview 14:52-57

The variant constructor Choose can't be found.

- If it's defined in another module or file, bring it into scope by:
  - Annotating it with said module name: let food = MyModule.Apple
  - Or specifying its type: let food: MyModule.fruit = Apple
- Constructors and modules are both capitalized. Did you want the latter?
  Then instead of let foo = Bar, try module Foo = Bar.
```

Not extremely helpful. Having to look into the OCaml code compiled from ReasonML:

```ocaml
[@@@bs.config { jsx = 3 }]
module Counter =
  struct
    type shape =
      | Circle
      | Square
    let make ~name  =
      let (_shape,setShape) = React.useState (fun ()  -> None) in
      let (value,setValue) = React.useState (fun ()  -> 0.0) in
      let (area,setArea) = React.useState (fun ()  -> 0.0) in
      ((div
          ~children:[((select
                         ~children:[((option ~value:""
                                        ~children:[Choose; shape] ())
                                   [@JSX ]);
                                   ((option
                                       ~value:(("circle")[@reason.raw_literal
                                                           "circle"])
                                       ~children:[Circle] ())[@JSX ]);
                                   ((option
                                       ~value:(("square")[@reason.raw_literal
                                                           "square"])
                                       ~children:[Square] ())[@JSX ])] ())
                    [@JSX ]);
                    ((input ~value ~children:[] ())[@JSX ]);
                    ((p ~children:[React.string (string_of_float area)] ())
                    [@JSX ]);
                    ((button
                        ~children:[React.string
                                     (("Calculate")[@reason.raw_literal
                                                     "Calculate"])] ())
                    [@JSX ])] ())[@JSX ])[@@react.component ]
  end
let _ =
  ReactDOMRe.renderToElementWithId
    ((Counter.createElement
        ~name:(("Counter")[@reason.raw_literal "Counter"]) ~children:[] ())
    [@JSX ]) (("preview")[@reason.raw_literal "preview"])
```

Seems that this JSX does not work well with strings. Looking into the original example, one can deduct the format:

```jsx
<option>{React.string("Choose shape")}</option>
```

And having to replace this everywhere in the JSX:

```ocaml
[@react.component]
let make = (~name) => {
  let (_shape, setShape) = React.useState(() => None);
  let (value, setValue) = React.useState(() => 0.0);
  let (area, setArea) = React.useState(() => 0.0);

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

Seems that `value={value}` does not work out of the box too, since the `value` variable has the type `float` and this version of React expects it to be `string`. Okay, let’s use that `string_of_float` function:

```ocaml
<input value={string_of_float(value)} />
```

Apparently it is also not good enough:

```
Warning number 3
OCaml preview 33:37-51

deprecated: Pervasives.string_of_float
Please use Js.Float.toString instead, string_of_float generates unparseable floats
```

So it should be

```ocaml
<input value={Js.Float.toString(value)} />
```

Okay, only the unused state setters left. Let’s implement the `shapeChanged` and the `calculateArea` helpers first:

```ocaml
let shapeChanged = (shapeStr: string): option(shape) =>
      switch shapeStr {
        | "circle" => Some(Circle)
        | "square" => Some(Square)
        | _ => None
      };

let calculateArea = (_shape: shape, value: float) =>
      switch _shape {
        | Circle => Js.Math.PI * value * value
        | Square => value * value
      };
```

The `Js.Math.PI` is not accessible, apparently:

```
We've found a bug for you!
OCaml preview 20:23-32

The variant constructor Js.Math.PI can't be found.

- If it's defined in another module or file, bring it into scope by:
  - Annotating it with said module name: let food = MyModule.Apple
  - Or specifying its type: let food: MyModule.fruit = Apple
- Constructors and modules are both capitalized. Did you want the latter?
  Then instead of let foo = Bar, try module Foo = Bar.
```

According to the [docs](https://rescript-lang.org/docs/manual/latest/api/js/math#_pi), it is `Js.Math._PI`. But even after fixing that, there is a mysterious error now:

```
We've found a bug for you!
OCaml preview 20:23-33

This has type:
  float
But somewhere wanted:
  int

You can convert a float to a int with int_of_float.If this is a literal, you want a number without a trailing dot (e.g. 20).
```

The error happens on this OCaml line:

```ocaml
| Circle  -> (Js.Math._PI * value) * value
```

Not helpful at all. The trick is that OCaml uses different operators for integer and floating-point math. This should do the trick:

```ocaml
| Circle -> Js.Math._PI *. value *. value
```

Now, the last bit: connecting the component to the state:

```ocaml
<select onChange={ event => setShape(shapeChanged(event.target.value)) }>
```

As usual, an error:

```
We've found a bug for you!
OCaml preview 26:62-67

The record field target can't be found.

If it's defined in another module or file, bring it into scope by:
- Annotating it with said module name: let baby = {MyModule.age: 3}
- Or specifying its type: let baby: MyModule.person = {age: 3}
```

Apparently, this JSX implementation has [its own ways](https://reasonml.github.io/reason-react/docs/en/event) of accessing event’s props:

```ocaml
ReactEvent.Form.target(event)##value
```

The issue is that this code is a valid BuckleScript, but not ReasonML. These intuitions [described in docs](https://rescript-lang.org/docs/manual/latest/migrate-from-bucklescript-reason) won’t work:

```ocaml
setShape(shapeChanged(ReactEvent.Form.target(event).value))
```

And here we go again:

```
We've found a bug for you!
OCaml preview 27:75-79

The record field value can't be found.

If it's defined in another module or file, bring it into scope by:
- Annotating it with said module name: let baby = {MyModule.age: 3}
- Or specifying its type: let baby: MyModule.person = {age: 3}
setShape(shapeChanged(ReactEvent.Form.target(event)["value"]))

We've found a bug for you!
OCaml preview 27:45-74

This has type:
  < .. > Js.t
But somewhere wanted:
  'a array
```

I am yet to figure out WTF is going on there, but the rough solution would be to just smash some JS code in:

```ocaml
<select onChange={ event => {
        let v: string = [%bs.raw {| event.target.value |}];
        let s: option(shape) = shapeChanged(v);
        setShape(s)
      }}>
```

That worked just enough to show yet another error:

```
We've found a bug for you!
OCaml preview 28:48

This has type:
  shape option
But somewhere wanted:
  'a option -> 'a option
```

This is because state setters take a function, not just a value:

```ocaml
setShape(_ => s)
```

And the whole event handler can be simplified a little bit:

```ocaml
<select onChange={ event => {
        let v: string = [%bs.raw {| event.target.value |}];
        setShape(_ => shapeChanged(v))
      }}>
```

This breaks some of the type checking benefits, but it just works ™️ ©

Back to the other event handlers:

```ocaml
<input value={Js.Float.toString(value)} onChange={ event => {
        let v: string = [%bs.raw {| event.target.value |}];
        setValue(_ => float_of_string(v));
      }} />
```

Surprisingly enough, here ReasonML is totally fine with `float_of_string`.

```ocaml
<button onClick={ _ => {
        setArea(_ => Belt.Option.mapWithDefault(_shape, 0.0, s => calculateArea(s, value)))
      }}>
```

Tricky `Belt` library way to work with `option`s.

Now that the code is done and seems to work in the playground, it is time to introduce yet another issue with raw technology by Facebook: it has actually **four** completely different and incompatible versions:

* OCaml, with all of the libraries provided by Facebook
* BuckleScript, the back-end of the ReasonML ([rebranding announcement](https://rescript-lang.org/blog/bucklescript-is-rebranding), [differences](https://rescript-lang.org/blog/bucklescript-8-1-new-syntax))
* ReasonML, the old version of the language ([blogs up to 2018](https://reasonml.github.io/blog/))
* ReScript, the new version of the language ([differences](https://rescript-lang.org/docs/manual/latest/migrate-from-bucklescript-reason))

Each of them has incompatible syntax, all of them use the docs of one another (in particular, React docs are written for BuckleScript, but refer to ReScript docs).

Despite the bold proclamations like

> // ReScript / old Reason syntax should parse just
> // fine (go to the "Settings" panel for toggling syntax).

And

> What Will Change with ReScript?
>
> Technically, not much. One of our main goals is to keep backwards compatibility for existing BuckleScript codebases and will provide an automated upgrade path from .re (Reason) to .res (ReScript) files.
>
> The BuckleScript compiler toolchain and its new .res syntax will be unified into one platform called ReScript. Upgrading from the bs-platform to the soon-to-be-published ReScript npm package will just be a matter of updating your package.json file. Syntax wise, we believe that previous Reason users will feel right at home.
>
> ReScript will continue shipping the old Reason v3.6 syntax as well and it will be possible to mix .re and .res files in one codebase (same with libraries).

If you try to run the ReasonML code that works in ReasonML playground in ReScript playground, you will notice that it does not even compile:

```
[E] Line 6, column 19:
Missing expression
[E] Line 12, column 49:
Type parameters require angle brackets:
  option<shape>
[E] Line 27, column 32:
Did you forget a `,` here? 
[E] Line 27, column 57:
Did you forget a `}` here? 
[E] Line 36, column 32:
Did you forget a `,` here? 
```

With a few changes following intuition (and not those useless error messages), one can get it to compile:

```ocaml
// no more [%bs] annotations
module Counter = {
  type shape = Circle | Square;
  
  @react.component // annotations look more java-like or typescript-like
  let make = () => {
    let (_shape, setShape) = React.useState(() => None);
    let (value, setValue) = React.useState(() => 0.0);
    let (area, setArea) = React.useState(() => 0.0);
    
    // option(type) is now option<type>
    let shapeChanged = (shapeStr: string): option<shape> =>
      switch shapeStr {
        | "circle" => Some(Circle)
        | "square" => Some(Square)
        | _ => None
      };
    
    let calculateArea = (_shape: shape, value: float) =>
      switch _shape {
        | Circle => Js.Math._PI *. value *. value
        | Square => value *. value
      };

    <div>
      <select onChange={ event => {
        // the issue with event.target.value is somewhat resolved
        let v: string = ReactEvent.Form.target(event)["value"];
        setShape(_ => shapeChanged(v))
      }}>
        <option value="">{React.string("Choose shape")}</option>
        <option value="circle">{React.string("Circle")}</option>
        <option value="square">{React.string("Square")}</option>
      </select>
      
      <input value={Js.Float.toString(value)} onChange={ event => {
        let v: string = ReactEvent.Form.target(event)["value"];
        setValue(_ => float_of_string(v));
      }} />
      
      <p>{React.string("Area:" ++ Js.Float.toString(area))}</p>

      <button onClick={ _ => {
        setArea(_ => Belt.Option.mapWithDefault(_shape, 0.0, s => calculateArea(s, value)))
      }}>
        {React.string("Calculate")}
      </button>
    </div>
  };
};
```

TL;DR:

<img src="/images/strongly-typed-front-end/one-can-not-simply.png" loading="lazy" alt="One can't simply...">
