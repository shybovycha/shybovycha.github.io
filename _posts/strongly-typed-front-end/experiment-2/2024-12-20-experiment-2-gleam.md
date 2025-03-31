---
layout: post
title: 'Strongly-typed front-end: experiment 2, simple application, in Gleam / Lustre'
date: '2024-04-19T09:47:48+09:00'
---

### Contents

1. [Introduction](/strongly-typed-front-end/2021/04/19/introduction.html)
2. [Experiment 1, hex2rgb](/strongly-typed-front-end/experiment-1/2021/04/19/experiment-1.html)
3. Experiment 2, simple application
    - [Elm](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-elm.html)
    - [F#](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-fsharp.html)
    - [PureScript & purescript-react-dom](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-purescript.html)
    - [PureScript & Halogen](/strongly-typed-front-end/experiment-2/2024/05/17/experiment-2-purescript-halogen.html)
    - [ReasonML](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-reasonml.html)
    - [**Gleam (you are here)**](/strongly-typed-front-end/experiment-2/2024/12/20/experiment-2-gleam.html)

```gleam
import gleam/float
import gleam/option.{type Option, None, Some}
import gleam/string
import lustre
import lustre/attribute.{value}
import lustre/element.{text}
import lustre/element/html.{button, div, input, p, select}
import lustre/event.{on_click, on_input}

type Shape {
  Circle
  Square
}

type Msg {
  ShapeChanged(s: Option(Shape))
  ValueChanged(x: Float)
  CalculateArea
}

type State {
  State(shape: Option(Shape), value: Float, area: Option(Float))
}

const pi = 3.14

fn calculate_area(shape: Shape, x: Float) -> Float {
  case shape {
    Circle -> pi *. x *. x
    Square -> x *. x
  }
}

fn init(_flags) -> State {
  State(shape: None, value: 0.0, area: None)
}

fn update(model: State, msg: Msg) -> State {
  case msg {
    ShapeChanged(s) -> State(..model, shape: s)
    ValueChanged(v) -> State(..model, value: v)
    CalculateArea ->
      State(
        ..model,
        area: option.map(model.shape, fn(s) { calculate_area(s, model.value) }),
      )
  }
}

fn handle_value_change(s: String) -> Msg {
  case string.is_empty(s) {
    True -> ValueChanged(0.0)
    False ->
      case float.parse(s) {
        Ok(x) -> ValueChanged(x)
        _ -> ValueChanged(0.0)
      }
  }
}

fn handle_shape_change(s: String) -> Msg {
  case s {
    "circle" -> ShapeChanged(Some(Circle))
    "square" -> ShapeChanged(Some(Square))
    _ -> ShapeChanged(None)
  }
}

fn view(model: State) {
  div([], [
    select([on_input(handle_shape_change)], [
      html.option([value("")], "Select shape"),
      html.option([value("circle")], "Circle"),
      html.option([value("square")], "Square"),
    ]),
    input([value(float.to_string(model.value)), on_input(handle_value_change)]),
    button([on_click(CalculateArea)], [text("Calculate area")]),
    p([], [
      text(
        "Area: " <> option.unwrap(option.map(model.area, float.to_string), ""),
      ),
    ]),
  ])
}

pub fn main() {
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
```

Resulting bundle is quite big sitting at a whopping `65.9kb`
