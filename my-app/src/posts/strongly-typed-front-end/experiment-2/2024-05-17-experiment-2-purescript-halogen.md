---
layout: post
title: 'Strongly-typed front-end: experiment 2, simple application, in PureScript / Halogen'
date: '2024-04-19T09:47:48+09:00'
---

### Contents

1. [Introduction](/strongly-typed-front-end/2021/04/19/introduction.html)
2. [Experiment 1, hex2rgb](/strongly-typed-front-end/experiment-1/2021/04/19/experiment-1.html)
3. Experiment 2, simple application
    - [Elm](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-elm.html)
    - [F#](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-fsharp.html)
    - [PureScript & purescript-react-dom](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-purescript.html)
    - [**PureScript & Halogen (you are here)**](/strongly-typed-front-end/experiment-2/2024/05/17/experiment-2-purescript-halogen.html)
    - [ReasonML](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-reasonml.html)
    - [Gleam](/strongly-typed-front-end/experiment-2/2024/12/20/experiment-2-gleam.html)

A more "conventional" way to implement the front-end application in PureScript would be using a framework called [Halogen](https://github.com/purescript-halogen/).

Starting off with a "hello world" example:

```purescript
module Main where

import Prelude

import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = Increment | Decrement

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = 0

  render state =
    HH.div_
      [ HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.div_ [ HH.text $ show state ]
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      ]

  handleAction = case _ of
    Increment -> H.modify_ \state -> state + 1
    Decrement -> H.modify_ \state -> state - 1
```

Adding the utility code akin to the other technologies:

```purescript
data Shape = Circle | Square

calculateArea :: Maybe Shape -> Float -> Float
calculateArea Nothing _ = 0
calculateArea (Just Circle) value = pi * value * value
calculateArea (Just Square) value = value * value

getShape :: String -> Maybe Shape
getShape "circle" = Just Circle
getShape "square" = Just Square
getShape _ = Nothing
```

This resurfaces few differences from Haskell, Elm and others:

* there is no `pi` constant in the `Prelude`, so need to import one of the [available definitions](https://pursuit.purescript.org/search?q=pi), I went with `Data.Number`
* `Float` is not a type; there is `Number`, however
* `0` is not a `Number`, it is `Int`, confusing the audience

These are all minor differences, however.
But this code is not a conventional PureScript either - it is working against the good practices of
functional programming and thus defeats the purpose of these experiments.
Examples of this are the heavy reliance on `String` instead of using the available type system.

Let us change that a bit:

```purescript
import Data.String.Read (class Read)

data Shape = Circle | Square

calculateArea :: Shape -> Number -> Number
calculateArea Circle value = pi * value * value
calculateArea Square value = value * value

instance Read Shape where
  read = case _ of
    "square" -> Just Square
    "circle" -> Just Circle
    _ -> Nothing

instance Show Shape where
  show = case _ of
    Square -> "square"
    Circle -> "circle"
```

Now, to the UI:

```purescript
import Halogen.HTML.Properties as HP

render state =
  HH.div_
    [
      HH.select [] [
        HH.option [ HP.value "" ] [ HH.text "Select shape" ],
        HH.option [ HP.value (show Circle) ] [ HH.text (show Circle) ],
        HH.option [ HP.value (show Square) ] [ HH.text (show Square) ]
      ],
      HH.input [],
      HH.div_ [ HH.text "<area>" ]
    ]
```

In the application state we need to store the selected shape and the value, so we can utilize records for that:

```purescript
initialState _ = { shape: Nothing, value: Nothing }
```

Then we need to modify the possible actions. Let's stick to the same approach of utilizing the type system:

```purescript
data Action = ChangeValue (Maybe Number) | ChangeShape (Maybe Shape)
```

The thing glueing the two together is the `handleAction` function:

```purescript
handleAction = case _ of
  ChangeValue value ->
    H.modify_ \state -> state { value = value }
  ChangeShape shape ->
    H.modify_ \state -> state { shape = shape }
```

Here, unlike Haskell (to my best knowledge), the placeholder variable is being used
for pattern matching against the only function argument.
So instead of a little verbose

```purescript
handleAction action = case action of
  -- ...
```

you can use this placeholder variable and just provide the branches for each of its possible values:

```purescript
handleAction = case _ of
  -- ...
```

Modifying the state is done using the [`Halogen.Hooks.HookM.modify_`](https://pursuit.purescript.org/packages/purescript-halogen-hooks/0.6.3/docs/Halogen.Hooks.HookM#v:modify_) function, which allows us to only use the previous state value and provide a new state value, without the need to mess with monads.
In turn, we modify the state record using the record syntax:

```purescript
state { shape = newShapeValue }
```

Now the only bit left is tying the UI with the actions:

```purescript
import Halogen.HTML.Events as HE
import Data.String.Read (read)
import Data.Number as N
import Data.Tuple (Tuple(..))

render state =
  HH.div_
    [
      HH.select [ HE.onValueChange onShapeChanged ] [
        HH.option [ HP.value "" ] [ HH.text "Select shape" ],
        HH.option [ HP.value (show Circle) ] [ HH.text (show Circle) ],
        HH.option [ HP.value (show Square) ] [ HH.text (show Square) ]
      ],
      HH.input [ HE.onValueChange onValueChanged ],
      HH.div_ [ HH.text "<area>" ]
    ]

onShapeChanged v = ChangeShape (read v)

onValueChanged v = ChangeValue (N.fromString v)

showArea state =
  case res of
    Nothing ->
      HH.text "Choose shape and provide its parameter"

    Just (Tuple shape area) ->
      HH.text $ "Area of " <> (show shape) <> " is " <> (show area)

  where
    res = do
      shape <- state.shape
      value <- state.value
      let area = calculateArea shape value
      pure (Tuple shape area)
```

Here is where most fun and benefit from using PureScript comes into play.

First of all, the [`HE.onValueChange`](https://pursuit.purescript.org/packages/purescript-halogen/7.0.0/docs/Halogen.HTML.Events#v:onValueChange)
event handler (the `onShapeChanged` and `onValueChanged` functions) - it will be called with the new value for the input instead of an entire
event object. This allows us to skip unpacking the raw value from that object.

Then, the action dispatchers take the value from the input and _try_ to parse it, returning a `Maybe a`:

```purescript
onShapeChanged :: String -> Maybe Shape
onShapeChanged v = ChangeShape (read v)

onValueChanged :: String -> Maybe Number
onValueChanged v = ChangeValue (N.fromString v)
```

It is actually a quite important part, since the shape might not be selected (making the `<select>` value an empty string) and the value might be either
a blank string or not a valid number string.
PureScript does not allow us to _not_ handle these cases, so whenever we parse the user input, we get a `Maybe a` value and we _have to_ handle both
scenarios when the value is valid and when it is not.

The function `showArea` is where this neatness comes together - we handle both values as one, using the `Data.Tuple` type to pair them together:

```purescript
res = do
  shape <- state.shape -- unpacks `Shape` from `Maybe Shape`
  value <- state.value -- unpacks `Number` from `Maybe Number`
  let area = calculateArea shape value -- always returns a Number, since both `shape` and `value` are always provided
  pure (Tuple shape area) -- returns a tuple of shape and area, packed in a `Maybe`
```

The above code will shortcircuit whenever at any point it is trying to unpack a value from a `Nothing` and the whole `do` block will return `Nothing`.

Putting it all together:

```purescript
module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number as N
import Data.String.Read (class Read, read)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

data Shape = Circle | Square

calculateArea :: Shape -> Number -> Number
calculateArea Circle value = N.pi * value * value
calculateArea Square value = value * value

instance Read Shape where
  read = case _ of
    "square" -> Just Square
    "circle" -> Just Circle
    _ -> Nothing

instance Show Shape where
  show = case _ of
    Square -> "square"
    Circle -> "circle"

data Action = ChangeValue (Maybe Number) | ChangeShape (Maybe Shape)

component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ = { shape: Nothing, value: Nothing }

  render state =
    HH.div_
      [
        HH.select [ HE.onValueChange onShapeChanged ] [
          HH.option [ HP.value "" ] [ HH.text "Select shape" ],
          HH.option [ HP.value (show Circle) ] [ HH.text (show Circle) ],
          HH.option [ HP.value (show Square) ] [ HH.text (show Square) ]
        ],
        HH.input [ HE.onValueChange onValueChanged ],
        HH.div_ [ showArea state ]
      ]

  onShapeChanged v = ChangeShape (read v)

  onValueChanged v = ChangeValue (N.fromString v)

  showArea state =
    case res of
      Nothing ->
        HH.text "Select shape and provide its value"

      Just (Tuple shape area) ->
        HH.text $ "Area of " <> (show shape) <> " is " <> (show area)
    where
      res = do
        shape <- state.shape
        value <- state.value
        let area = calculateArea shape value
        pure (Tuple shape area)

  handleAction = case _ of
    ChangeValue value ->
      H.modify_ \state -> state { value = value }
    ChangeShape shape ->
      H.modify_ \state -> state { shape = shape }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
```
