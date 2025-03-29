---
layout: post
title: 'Strongly-typed front-end: experiment 2, simple application, in Elm'
date: '2021-04-19T09:23:02+09:00'
---

### Contents

1. [Introduction](/strongly-typed-front-end/2021/04/19/introduction.html)
2. [Experiment 1, hex2rgb](/strongly-typed-front-end/experiment-1/2021/04/19/experiment-1.html)
3. Experiment 2, simple application
    - [**Elm (you are here)**](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-elm.html)
    - [F#](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-fsharp.html)
    - [PureScript & purescript-react-dom](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-purescript.html)
    - [PureScript & Halogen](/strongly-typed-front-end/experiment-2/2024/05/17/experiment-2-purescript-halogen.html)
    - [ReasonML](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-reasonml.html)
    - [Gleam](/strongly-typed-front-end/experiment-2/2024/12/20/experiment-2-gleam.html)

(Heavily over-opinionated statement) Elm forces you to handle error scenarios when writing the code.

[Sandbox](https://codesandbox.io/s/inspiring-diffie-lq0u2)

This is pretty much a translation of a TypeScript code from above:

```haskell
module Main exposing (..)

import Browser
import Html exposing (Html, button, div, text, input, select, option)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)

-- util

type Shape = Circle | Square

calculateArea : Shape -> Float -> Float
calculateArea shape value =
  case shape of
    Circle -> pi * value * value
    
    Square -> value * value
    
-- MAIN

main =
  Browser.sandbox { init = init, update = update, view = view }

-- MODEL

type alias Model = { shape: Shape, value: Float, area: Float }

init : Model
init = { shape = "", value = 0, area = 0 }

-- UPDATE

type Msg
  = ShapeChanged Shape
  | ValueChanged Float
  | CalculateArea

update : Msg -> Model -> Model
update msg model =
  case msg of
    ShapeChanged shape ->
      { model | shape = shape }

    ValueChanged value ->
      { model | value = value }
      
    CalculateArea ->
      { model | area = (calculateArea model.shape model.value) }

-- VIEW

onShapeChanged : String -> Msg
onShapeChanged shape = 
  case shape of
    "circle" -> ShapeChanged Circle
    "square" -> ShapeChanged Square

onValueChanged : String -> Msg
onValueChanged value = ValueChanged (Maybe.withDefault 0 (String.toFloat value))

view : Model -> Html Msg
view model =
  div []
    [ select [ onInput onShapeChanged ] [ 
      option [ value "" ] [ text "Choose shape" ], 
      option [ value "circle" ] [ text "Circle" ],
      option [ value "square" ] [ text "Square" ] ]
    , input [ value (String.fromFloat model.value), onInput onValueChanged ] []
    , button [ onClick CalculateArea ] [ text "Calculate area" ]
    , div [] [ text ("Area: " ++ (String.fromFloat model.area)) ]
    ]
```

Note that it won’t compile:

```
-- TYPE MISMATCH ----------------------------------------------- Jump To Problem

Something is off with the body of the `init` definition:

29| init = { shape = "", value = 0, area = 0 }
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The body is a record of type:

    { area : Float, shape : String, value : Float }

But the type annotation on `init` says it should be:

    Model
```

<!--more-->

You can’t have a default value for a type (the way enums are implemented in Elm / Haskell / ML-like languages) that is outside of the type values' range. You have to either use a valid value or stick to something like `Maybe`:

```haskell
type alias Model = { shape: Maybe Shape, value: Float, area: Float }

init : Model
init = { shape = Nothing, value = 0, area = 0 }

-- UPDATE

type Msg
  = ShapeChanged (Maybe Shape)
  | ValueChanged Float
  | CalculateArea

update : Msg -> Model -> Model
update msg model =
  case msg of
    ShapeChanged shape ->
      { model | shape = shape }

    ValueChanged value ->
      { model | value = value }
      
    CalculateArea ->
      { model | area = (Maybe.withDefault 0 (Maybe.map (\shape -> calculateArea shape model.value) model.shape)) }

-- VIEW

onShapeChanged : String -> Msg
onShapeChanged shape = 
  case shape of
    "circle" -> ShapeChanged (Just Circle)
    "square" -> ShapeChanged (Just Square)
    _ -> ShapeChanged Nothing

onValueChanged : String -> Msg
onValueChanged value = ValueChanged (Maybe.withDefault 0 (String.toFloat value))
```

See how this simple fact changes the whole implementation. Not sure if that is a good news, though.

Now even with these changes the code won’t compile, since there is one code path uncovered - user selecting a value other than `circle` or `square` (the default one):

```
-- MISSING PATTERNS -------------------------------------------- Jump To Problem

This `case` does not have branches for all possibilities:

54|>  case shape of
55|>    "circle" -> ShapeChanged (Just Circle)
56|>    "square" -> ShapeChanged (Just Square)

Missing possibilities include:

    _

I would have to crash if I saw one of those. Add branches for them!

Hint: If you want to write the code for each branch later, use `Debug.todo` as a
placeholder. Read <https://elm-lang.org/0.19.1/missing-patterns> for more
guidance on this workflow.
```

Elm _forces_ you to cover that path.

```haskell
onShapeChanged : String -> Msg
onShapeChanged shape = 
  case shape of
    "circle" -> ShapeChanged (Just Circle)
    "square" -> ShapeChanged (Just Square)
    _ -> ShapeChanged Nothing
```

See how awesome error messages from Elm are and how they really help you figure out what the issues are and fix errors.
