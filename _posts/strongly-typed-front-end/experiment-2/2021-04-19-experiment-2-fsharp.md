---
layout: post
title: 'Strongly-typed front-end: experiment 2, simple application, in F#'
date: '2021-04-19T09:23:02+09:00'
---

In F# world, there is a framework called [Fable](https://fable.io/). It allows one to compile their F# code to JavaScript. There is a built-in package for React, but Fable developers themselves [suggest](https://fable.io/docs/your-fable-project/use-a-fable-library.html) using [Elmish](https://elmish.github.io/), which is a framework similar to Elm, just suited for F#.

A sample Elmish application in the [online editor](https://fable.io/repl/) looks like this:

```fsharp
module Elmish.SimpleInput

(**
Minimal application showing how to use Elmish
You can find more info about Emish architecture and samples at https://elmish.github.io/
*)

open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React

// MODEL

type Model =
    { Value : string }

type Msg =
    | ChangeValue of string

let init () = { Value = "" }, Cmd.none

// UPDATE

let update (msg:Msg) (model:Model) =
    match msg with
    | ChangeValue newValue ->
        { model with Value = newValue }, Cmd.none

// VIEW (rendered with React)

let view model dispatch =
    div [ Class "main-container" ]
        [ input [ Class "input"
                  Value model.Value
                  OnChange (fun ev -> ev.target?value |> string |> ChangeValue |> dispatch) ]
          span [ ]
            [ str "Hello, "
              str model.Value
              str "!" ] ]

// App
Program.mkProgram init update view
|> Program.withConsoleTrace
|> Program.withReactSynchronous "elmish-app"
|> Program.run
```

One can easily see the similarities to Elm (or so I think).

<!--more-->

Rewriting it to the application from above should not be a problem, right?

```fsharp
module Elmish.SimpleInput

(**
Minimal application showing how to use Elmish
You can find more info about Emish architecture and samples at https://elmish.github.io/
*)

open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open System

// MODEL

type Shape = Rectangle | Circle

let calculateArea (shape: Shape) (value: float) =
    match shape with
    | Circle -> value * value * Math.PI
    | Rectangle -> value * value

type Model =
    { shape : Option<Shape>; value: float; area: float }

type Msg =
    | ShapeChanged of Shape
    | ValueChanged of float
    | CalculateArea

let init () = { value = 0.0; shape = Option.None; area = 0.0 }, Cmd.none

// UPDATE

let update (msg: Msg) (model: Model) =
    match msg with
    | ValueChanged newValue ->
        { model with value = newValue }, Cmd.none
    | ShapeChanged newShape ->
        { model with shape = newShape }, Cmd.none
    | CalculateArea ->
        { model with area = calculateArea model.shape model.value }

// VIEW (rendered with React)

let view model dispatch =
    div []
        [ select [ OnChange (fun evt -> evt.target?value |> string |> ShapeChanged |> dispatch) ] [
            option [ ] [ str "Select shape" ]
            option [ Value "circle" ]
            option [ Value "rectangle" ]
          ]
          input [ Value model.Value
                  OnChange (fun evt -> evt.target?value |> float |> ValueChanged |> dispatch) ]
          button [ OnClick (fun evt -> dispatch CalculateArea) ] [ str "Calculate area" ]
          span [ ]
            [ str "Area: "
              str model.Area
            ]
        ]

// App
Program.mkProgram init update view
|> Program.withConsoleTrace
|> Program.withReactSynchronous "elmish-app"
|> Program.run
```

I deliberately skipped few things to check what errors will I get from the compiler.

Now to the errors:

```
    | Circle -> value * value * Math.PI
The value, constructor, namespace or type 'PI' is not defined.
| ShapeChanged newShape ->
        { model with shape = newShape }, Cmd.none
This expression was expected to have type
    'Shape'    
but here has type
    'Shape option'
| CalculateArea ->
        { model with area = calculateArea model.shape model.value }
All branches of a pattern match expression must return values of the same type as the first branch, which here is 'Model * Cmd<'a>'. This branch returns a value of type 'Model'.
[ select [ OnChange (fun evt -> evt.target.value |> string |> ShapeChanged |> dispatch) ] [
            option [ Value "circle" ]
            option [ Value "rectangle" ]
          ]
          
The type 'EventTarget' does not define the field, constructor or member 'value'.

Type mismatch. Expecting a
    'string -> 'a'    
but given a
    'Shape -> Msg'    
The type 'string' does not match the type 'Shape'

The type ''a -> ReactElement' is not compatible with the type 'ReactElement' (x2)
span [ ]
            [ str "Area: "
              str model.Area
            ]
            
Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.
```

The errors might be a tiny bit mysterious at times, but using simple intuition one can easily fix them all.

```fsharp
module Elmish.SimpleInput

open Fable.Core.JsInterop
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React

open System

// MODEL

type Shape = Rectangle | Circle

let calculateArea (shape: Shape) (value: float) =
    match shape with
    | Circle -> value * value * Math.PI
    | Rectangle -> value * value

let getShape (value: string): Option<Shape> =
    match value with
    | "circle" -> Option.Some Circle
    | "rectangle" -> Option.Some Rectangle
    | _ -> Option.None

type Model =
    { shape : Option<Shape>; value: float; area: float }

type Msg =
    | ShapeChanged of string
    | ValueChanged of string
    | CalculateArea

let init () = { value = 0.0; shape = Option.None; area = 0.0 }, Cmd.none

// UPDATE

let update (msg: Msg) (model: Model) =
    match msg with
    | ValueChanged newValue ->
        { model with value = float newValue }, Cmd.none
    | ShapeChanged newShape ->
        { model with shape = getShape newShape }, Cmd.none
    | CalculateArea ->
        let newArea = 
            Option.map (fun shape -> calculateArea shape model.value) model.shape
            |> Option.defaultValue 0.0
        { model with area = newArea }, Cmd.none

// VIEW

let view model dispatch =
    div []
        [ select [ OnChange (fun evt -> evt.target?value |> ShapeChanged |> dispatch) ] [
            option [ ] [ str "Select shape" ]
            option [ Value "circle" ] [ str "Circle" ]
            option [ Value "rectangle" ] [ str "Rectangle" ]
          ]
          input [ Value model.value
                  OnChange (fun evt -> evt.target?value |> ValueChanged |> dispatch) ]
          button [ OnClick (fun evt -> dispatch CalculateArea) ] [ str "Calculate area" ]
          span [ ]
            [ str "Area: "
              str (string model.area)
            ]
        ]

// App
Program.mkProgram init update view
|> Program.withConsoleTrace
|> Program.withReactSynchronous "elmish-app"
|> Program.run
```

TL;DR:

<img data-src="/images/strongly-typed-front-end/not-bad.png" alt="Not bad">
