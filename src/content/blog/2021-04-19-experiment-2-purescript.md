---
layout: post
title: 'Strongly-typed front-end: experiment 2, simple application, in PureScript'
date: '2021-04-19T09:47:48+09:00'
---

### Contents

1. [Introduction](/strongly-typed-front-end/2021/04/19/introduction.html)
2. [Experiment 1, darken_color](/strongly-typed-front-end/experiment-1/2021/04/19/experiment-1.html)
3. Experiment 2, simple application
    - [Elm](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-elm.html)
    - [F#](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-fsharp.html)
    - [**PureScript & purescript-react-dom (you are here)**](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-purescript.html)
    - [PureScript & Halogen](/strongly-typed-front-end/experiment-2/2024/05/17/experiment-2-purescript-halogen.html)
    - [ReasonML](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-reasonml.html)

In PureScript world there are quite a few libraries for React. And all of them have terrible (or rather non-existent) documentation, so I had to use as much intuition as outdated and barely working code samples. In this case I have picked [purescript-react-dom](https://pursuit.purescript.org/packages/purescript-react-dom/8.0.0).

Initial application structure:

```purescript
module Main where

import Prelude

import Control.Monad.Eff

import Data.Maybe
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)

import Effect (Effect)
import Effect.Console (log)

import DOM (DOM())
import DOM.HTML (window)
import DOM.HTML.Document (body)
import DOM.HTML.Types (htmlElementToElement)
import DOM.HTML.Window (document)

import DOM.Node.Types (Element())

import React

import React.DOM as DOM
import React.DOM.Props as Props

type Shape = Circle | Square

calculateArea :: Maybe Shape -> Float -> Float
calculateArea Nothing _ = 0
calculateArea (Just Circle) value = pi * value * value
calculateArea (Just Square) value = value * value

getShape :: String -> Maybe Shape
getShape "circle" = Just Circle
getShape "square" = Just Square
getShape _ = Nothing

onShapeChanged ctx evt = do
  writeState ctx { shape: getShape ((unsafeCoerce evt).target.value) }

onCalculateAreaClicked ctx evt = do
  { shape, value } <- readState ctx
  writeState ctx { area: calculateArea shape value }

areaCalculator = createClass $ spec { shape: Nothing, value: 0, area: 0 } \ctx -> do
  { shape, value, area } <- readState ctx
  return $ DOM.div [] [
    DOM.div [] [
      DOM.select [ Props.onChange (onShapeChanged ctx) ] [
          DOM.option [ Props.value "" ] [ DOM.text "Select shape" ],
          DOM.option [ Props.value "circle" ] [ DOM.text "Circle" ],
          DOM.option [ Props.value "square" ] [ DOM.text "Square" ]
      ],
      DOM.input [ Props.value (show value) ] [],
      DOM.button [ Props.onClick (onCalculateAreaClicked ctx) ] [ DOM.text "Calculate area" ]
    ],
    DOM.div [] [
      DOM.text ("Area: " ++ (show area))
    ]
    ]

main = container >>= render ui
  where
  ui :: ReactElement
  ui = createFactory areaCalculator {}

  container :: forall eff. Eff (dom :: DOM | eff) Element
  container = do
    win <- window
    doc <- document win
    elt <- fromJust <$> toMaybe <$> body doc
    return $ htmlElementToElement elt
```

<!--more-->

Immediately the flaws of the infrastructure come out:

```
$ spago build                                                                                                                                                                                                                                           1 ↵
Error 1 of 8:

  in module Main
  at src/Main.purs:5:1 - 5:25 (line 5, column 1 - line 5, column 25)

    Module Control.Monad.Eff was not found.
    Make sure the source file exists, and that it has been provided as an input to the compiler.


  See https://github.com/purescript/documentation/blob/master/errors/ModuleNotFound.md for more information,
  or to contribute content related to this error.

Error 2 of 8:

  in module Main
  at src/Main.purs:8:1 - 8:36 (line 8, column 1 - line 8, column 36)

    Module Data.Maybe.Unsafe was not found.
    Make sure the source file exists, and that it has been provided as an input to the compiler.


  See https://github.com/purescript/documentation/blob/master/errors/ModuleNotFound.md for more information,
  or to contribute content related to this error.

Error 3 of 8:

  in module Main
  at src/Main.purs:14:1 - 14:19 (line 14, column 1 - line 14, column 19)

    Module DOM was not found.
    Make sure the source file exists, and that it has been provided as an input to the compiler.


  See https://github.com/purescript/documentation/blob/master/errors/ModuleNotFound.md for more information,
  or to contribute content related to this error.

Error 4 of 8:

  in module Main
  at src/Main.purs:15:1 - 15:25 (line 15, column 1 - line 15, column 25)

    Module DOM.HTML was not found.
    Make sure the source file exists, and that it has been provided as an input to the compiler.


  See https://github.com/purescript/documentation/blob/master/errors/ModuleNotFound.md for more information,
  or to contribute content related to this error.

Error 5 of 8:

  in module Main
  at src/Main.purs:16:1 - 16:32 (line 16, column 1 - line 16, column 32)

    Module DOM.HTML.Document was not found.
    Make sure the source file exists, and that it has been provided as an input to the compiler.


  See https://github.com/purescript/documentation/blob/master/errors/ModuleNotFound.md for more information,
  or to contribute content related to this error.

Error 6 of 8:

  in module Main
  at src/Main.purs:17:1 - 17:45 (line 17, column 1 - line 17, column 45)

    Module DOM.HTML.Types was not found.
    Make sure the source file exists, and that it has been provided as an input to the compiler.


  See https://github.com/purescript/documentation/blob/master/errors/ModuleNotFound.md for more information,
  or to contribute content related to this error.

Error 7 of 8:

  in module Main
  at src/Main.purs:18:1 - 18:34 (line 18, column 1 - line 18, column 34)

    Module DOM.HTML.Window was not found.
    Make sure the source file exists, and that it has been provided as an input to the compiler.


  See https://github.com/purescript/documentation/blob/master/errors/ModuleNotFound.md for more information,
  or to contribute content related to this error.

Error 8 of 8:

  in module Main
  at src/Main.purs:20:1 - 20:34 (line 20, column 1 - line 20, column 34)

    Module DOM.Node.Types was not found.
    Make sure the source file exists, and that it has been provided as an input to the compiler.


  See https://github.com/purescript/documentation/blob/master/errors/ModuleNotFound.md for more information,
  or to contribute content related to this error.


[error] Failed to build.
```

Well, the error messages kind of point you to the source of error - the modules have not been provided to the compiler. 

Had to use documentation for each of the packages to match the types and fix the imports (since the [example](https://github.com/ethul/purescript-react#example) I have relied upon is way out of date):

```bash
$ spago install purescript-web-dom purescript-web-html react-dom
```

And adjust the code itself:

```purescript
module Main where

import Prelude

import Effect (Effect)

import Data.Maybe

import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

import Web.DOM.Element (Element())

import React as React
import ReactDOM as ReactDOM

import React.DOM as DOM
import React.DOM.Props as Props

type Shape = Circle | Square

calculateArea :: Maybe Shape -> Float -> Float
calculateArea Nothing _ = 0
calculateArea (Just Circle) value = pi * value * value
calculateArea (Just Square) value = value * value

getShape :: String -> Maybe Shape
getShape "circle" = Just Circle
getShape "square" = Just Square
getShape _ = Nothing

onShapeChanged ctx evt = do
  writeState ctx { shape: getShape ((unsafeCoerce evt).target.value) }

onCalculateAreaClicked ctx evt = do
  { shape, value } <- React.readState ctx
  writeState ctx { area: calculateArea shape value }

areaCalculator :: React.ReactClass { }
areaCalculator = React.component "AreaCalculator" component
  where
  component ctx = pure { state: { shape: Nothing, value: 0, area: 0 }, render: renderFn ctx }
  where
    renderFn ctx =
      { shape, value, area } <- React.readState ctx
      return $ DOM.div [] [
        DOM.div [] [
          DOM.select [ Props.onChange (onShapeChanged ctx) ] [
              DOM.option [ Props.value "" ] [ DOM.text "Select shape" ],
              DOM.option [ Props.value "circle" ] [ DOM.text "Circle" ],
              DOM.option [ Props.value "square" ] [ DOM.text "Square" ]
          ],
          DOM.input [ Props.value (show value) ] [],
          DOM.button [ Props.onClick (onCalculateAreaClicked ctx) ] [ DOM.text "Calculate area" ]
        ],
        DOM.div [] [
          DOM.text ("Area: " ++ (show area))
        ]
        ]

main = container >>= ReactDOM.render componentInstance
  where
  componentInstance = React.createLeafElement areaCalculator {}

  container :: forall eff. Effect (dom :: DOM | eff) Element
  container = do
    win <- window
    doc <- document win
    elt <- body doc
    return $ toElement elt
```

To get yet another error:

```
Error found:
at src/Main.purs:22:21 - 22:22 (line 22, column 21 - line 22, column 22)

  Unable to parse module:
  Unexpected token '|'
```

That’s my bad, it is more Haskell than Elm or F#:

```diff
- type Shape = Circle | Square
+ data Shape = Circle | Square
```

And yet another one:

```
Error found:
at src/Main.purs:45:3 - 45:8 (line 45, column 3 - line 45, column 8)

  Unable to parse module:
  Unexpected token 'where'
```

Because I did not indent my code enough.

And yet another one:

```
Error found:
at src/Main.purs:47:32 - 47:34 (line 47, column 32 - line 47, column 34)

  Unable to parse module:
  Unexpected "<-" in expression, perhaps due to a missing 'do' or 'ado' keyword
```

Because `React.readState` does not return effect, but rather the value itself, so no need to unpack it:

```diff
- { shape, value, area } <- React.readState ctx
+ let { shape, value, area } = React.readState ctx
```

And yet another one:

```
Error found:
at src/Main.purs:48:9 - 48:15 (line 48, column 9 - line 48, column 15)

  Unable to parse module:
  Unexpected token 'return'
```

Because I have forgot the `do` keyword:

```diff
- renderFn ctx =
+ renderFn ctx = do
```

Few small errors resolved:

```
Compiling Main
Error 1 of 2:

  in module Main
  at src/Main.purs:26:33 - 26:38 (line 26, column 33 - line 26, column 38)

    Unknown type Float

# replace Float with Number, as Float is not really a type in PureScript

Error 2 of 2:

  in module Main
  at src/Main.purs:61:32 - 61:34 (line 61, column 32 - line 61, column 34)

    Unknown operator (++)
    
# replace ++ with + as ++ is not really an operator in PureScript
```

To end up with type errors hell:

```
Error found:
in module Main
at src/Main.purs:60:13 - 60:54 (line 60, column 13 - line 60, column 54)

  Could not match type

    ReactElement

  with type

    Array t0 -> t1


while applying a function input [ value ((...) value)
                                ]
  of type ReactElement
  to argument []
while checking that expression (input [ value (...)
                                      ]
                               )
                               []
  has type ReactElement
in value declaration areaCalculator

where t0 is an unknown type
      t1 is an unknown type
```

What this tells you is that input function [does not take](https://pursuit.purescript.org/packages/purescript-react/8.0.0/docs/React.DOM#v:input) a second argument (which would normally be similar to `React.DOM.div [ attributes ] [ children ]`).

```diff
- DOM.input [ Props.value (show value) ] []
+ DOM.input [ Props.value (show value) ]
```

Earlier I have mentioned type hell. Well, that was just the small example. Here’s the next error in the code:

```
Error found:
in module Main
at src/Main.purs:46:18 - 46:50 (line 46, column 18 - line 46, column 50)

  Could not match type

    Int

  with type

    Number


while trying to match type
                             ( area :: Int
                             , shape :: Maybe t3
                             , value :: Int
                             ...
                             )

  with type
              ( area :: Number
              , shape :: Maybe Shape
              , value :: Number
              ...
              | t1
              )

while solving type class constraint

  Prim.Row.Nub t0
               ( componentDidCatch :: Error
                                      -> { componentStack :: String
                                         }
                                         -> Effect Unit
               , componentDidMount :: Effect Unit
               , componentDidUpdate :: Record ()
                                       -> { area :: Number
                                          , shape :: ...
                                          , value :: Number
                                          | t1
                                          }
                                          -> t2 -> ...
               , componentWillUnmount :: Effect Unit
               , getSnapshotBeforeUpdate :: Record ()
                                            -> { area :: Number
                                               , shape :: ...
                                               , value :: Number
                                               | t1
                                               }
                                               -> Effect t2
               , render :: Effect ReactElement
               , shouldComponentUpdate :: Record ()
                                          -> { area :: Number
                                             , shape :: ...
                                             , value :: Number
                                             | t1
                                             }
                                             -> Effect Boolean
               , state :: { area :: Number
                          , shape :: Maybe Shape
                          , value :: Number
                          | t1
                          }
               , unsafeComponentWillMount :: Effect Unit
               , unsafeComponentWillReceiveProps :: Record () -> Effect Unit
               , unsafeComponentWillUpdate :: Record ()
                                              -> { area :: Number
                                                 , shape :: ...
                                                 , value :: Number
                                                 | t1
                                                 }
                                                 -> Effect Unit
               )

while inferring the type of component "AreaCalculator"
in value declaration areaCalculator

where t1 is an unknown type
      t3 is an unknown type
      t0 is an unknown type
      t2 is an unknown type

See https://github.com/purescript/documentation/blob/master/errors/TypesDoNotUnify.md for more information,
or to contribute content related to this error.
```

Basically compiler is trying to say the initial state provided has a type `( area :: Int, value :: Int, shape :: Maybe t3 )` but what is expected (down in the code) is `( area :: Number, value :: Number, shape :: Maybe Shape )`. Quite an explanation.

```diff
+ initialState :: AreaCalculatorState
+ initialState = { shape: Nothing, value: 0.0, area: 0.0 }

- componentImpl ctx = pure { state: { shape: Nothing, value: 0.0, area: 0.0 }, render: renderFn ctx }
+ componentImpl ctx = pure { state: initialState, render: renderFn ctx }
```

And back to a new error:

```
Error found:
in module Main
at src/Main.purs:70:23 - 70:45 (line 70, column 23 - line 70, column 45)

  No type class instance was found for

    Data.Semiring.Semiring String


while applying a function add
  of type Semiring t0 => t0 -> t0 -> t0
  to argument "Area: "
while inferring the type of add "Area: "
in value declaration areaCalculator

where t0 is an unknown type

See https://github.com/purescript/documentation/blob/master/errors/NoInstanceFound.md for more information,
or to contribute content related to this error.
```

Which means the `+` operator (function) does not apply to strings. The `<>` is the string concatenation operator in this case. Although very hectic, it is described in [the docs](https://pursuit.purescript.org/packages/purescript-prelude/4.1.1/docs/Data.Semigroup#v:(%3C%3E)).

And yet another error:

```
Error found:
in module Main
at src/Main.purs:78:21 - 78:29 (line 78, column 21 - line 78, column 29)

  Could not match type

    Effect

  with type

    Maybe


while trying to match type Effect (Maybe HTMLElement)
  with type Maybe t0
while checking that expression body doc
  has type Maybe t0
in value declaration main

where t0 is an unknown type

See https://github.com/purescript/documentation/blob/master/errors/TypesDoNotUnify.md for more information,
or to contribute content related to this error.
```

Which is all about matching the return types and handling them properly:

```diff
- elt <- toElement elt
+ let elt = fromJust eltMaybe
+ let container = toElement elt
```

And finally, the version that _compiles_:

```purescript
module Main where

import Prelude

import Effect (Effect)

import Data.Maybe

import Math (pi)

import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

import Web.DOM.Element (Element())

import Unsafe.Coerce (unsafeCoerce)

import React as React
import ReactDOM as ReactDOM

import React.DOM as DOM
import React.DOM.Props as Props

data Shape = Circle | Square

calculateArea :: Maybe Shape -> Number -> Number
calculateArea Nothing _ = 0.0
calculateArea (Just Circle) value = pi * value * value
calculateArea (Just Square) value = value * value

getShape :: String -> Maybe Shape
getShape "circle" = Just Circle
getShape "square" = Just Square
getShape _ = Nothing

onShapeChanged ctx evt = do
  React.setState ctx { shape: getShape ((unsafeCoerce evt).target.value) }

onCalculateAreaClicked ctx evt = do
  { shape, value } <- React.getState ctx
  React.setState ctx { area: calculateArea shape value }

type AreaCalculatorState = { shape :: Maybe Shape, value :: Number, area :: Number }

initialState :: AreaCalculatorState
initialState = { shape: Nothing, value: 0.0, area: 0.0 }

areaCalculator :: React.ReactClass { }
areaCalculator = React.component "AreaCalculator" componentImpl
  where
  componentImpl ctx = pure { state: initialState, render: renderFn ctx }
    where
      renderFn ctx' = do
        { shape, value, area } <- React.getState ctx'
        pure $ DOM.div [] [
          DOM.div [] [
            DOM.select [ Props.onChange (onShapeChanged ctx') ] [
                DOM.option [ Props.value "" ] [ DOM.text "Select shape" ],
                DOM.option [ Props.value "circle" ] [ DOM.text "Circle" ],
                DOM.option [ Props.value "square" ] [ DOM.text "Square" ]
            ],

            DOM.input [ Props.value (show value) ],

            DOM.button [ Props.onClick (onCalculateAreaClicked ctx') ] [ DOM.text "Calculate area" ]
          ],
          DOM.div [] [
            DOM.text ("Area: " <> (show area))
          ]
        ]

main = do
  let componentInstance = React.createLeafElement areaCalculator {}
  win <- window
  doc <- document win
  eltMaybe <- body doc
  let elt = fromJust eltMaybe
  let container = toElement elt
  ReactDOM.render componentInstance container
```

Of course, there are few warnings and few sloppy solutions worth fixing:

```
Warning 1 of 7:

  in module Main
  at src/Main.purs:7:1 - 7:18 (line 7, column 1 - line 7, column 18)

    Module Data.Maybe has unspecified imports, consider using the explicit form:

      import Data.Maybe (Maybe(..), fromJust)



  See https://github.com/purescript/documentation/blob/master/errors/ImplicitImport.md for more information,
  or to contribute content related to this warning.

Warning 2 of 7:

  in module Main
  at src/Main.purs:5:1 - 5:23 (line 5, column 1 - line 5, column 23)

    The import of Effect is redundant


  See https://github.com/purescript/documentation/blob/master/errors/UnusedImport.md for more information,
  or to contribute content related to this warning.

Warning 3 of 7:

  in module Main
  at src/Main.purs:3:1 - 3:15 (line 3, column 1 - line 3, column 15)

    Module Prelude has unspecified imports, consider using the explicit form:

      import Prelude (bind, pure, show, ($), (*), (<>))



  See https://github.com/purescript/documentation/blob/master/errors/ImplicitImport.md for more information,
  or to contribute content related to this warning.

Warning 4 of 7:

  in module Main
  at src/Main.purs:16:1 - 16:35 (line 16, column 1 - line 16, column 35)

    The import of Web.DOM.Element is redundant


  See https://github.com/purescript/documentation/blob/master/errors/UnusedImport.md for more information,
  or to contribute content related to this warning.

Warning 5 of 7:

  in module Main
  at src/Main.purs:38:1 - 39:75 (line 38, column 1 - line 39, column 75)

    No type declaration was provided for the top-level declaration of onShapeChanged.
    It is good practice to provide type declarations as a form of documentation.
    The inferred type of onShapeChanged was:

      forall t10 t14 t8.
        ReactThis t10
          { shape :: ...
          | t8
          }
        -> t14 -> Effect Unit


  in value declaration onShapeChanged

  See https://github.com/purescript/documentation/blob/master/errors/MissingTypeDeclaration.md for more information,
  or to contribute content related to this warning.

Warning 6 of 7:

  in module Main
  at src/Main.purs:41:1 - 43:57 (line 41, column 1 - line 43, column 57)

    No type declaration was provided for the top-level declaration of onCalculateAreaClicked.
    It is good practice to provide type declarations as a form of documentation.
    The inferred type of onCalculateAreaClicked was:

      forall t24 t37 t39.
        ReactThis t37
          { area :: Number
          , shape :: ...
          , value :: Number
          | t39
          }
        -> t24 -> Effect Unit


  in value declaration onCalculateAreaClicked

  See https://github.com/purescript/documentation/blob/master/errors/MissingTypeDeclaration.md for more information,
  or to contribute content related to this warning.

Warning 7 of 7:

  in module Main
  at src/Main.purs:74:1 - 81:46 (line 74, column 1 - line 81, column 46)

    No type declaration was provided for the top-level declaration of main.
    It is good practice to provide type declarations as a form of documentation.
    The inferred type of main was:

      Partial => Effect (Maybe ReactComponent)


  in value declaration main

  See https://github.com/purescript/documentation/blob/master/errors/MissingTypeDeclaration.md for more information,
  or to contribute content related to this warning.
```

Essentially,

```
Module Data.Maybe has unspecified imports, consider using the explicit form:

      import Data.Maybe (Maybe(..), fromJust)
```

is fixed with the suggested code:

```
import Data.Maybe (Maybe(..), fromJust)
```

Same with the unnecessary imports.

This one looks neat:

```
Warning 5 of 7:

  in module Main
  at src/Main.purs:38:1 - 39:75 (line 38, column 1 - line 39, column 75)

    No type declaration was provided for the top-level declaration of onShapeChanged.
    It is good practice to provide type declarations as a form of documentation.
    The inferred type of onShapeChanged was:

      forall t10 t14 t8.
        ReactThis t10
          { shape :: ...
          | t8
          }
        -> t14 -> Effect Unit


  in value declaration onShapeChanged

  See https://github.com/purescript/documentation/blob/master/errors/MissingTypeDeclaration.md for more information,
  or to contribute content related to this warning.
```

Compiler asks you to explicitly type the function declaration. But I don’t care about that for now.

In order to run the app, few actions are still needed:

```bash
$ yarn add -D spago purescript parcel
```

Then, one will need an entry point:

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Document</title>
</head>
<body>
  <script src="index.js"></script>
</body>
</html>
```

with a script:

```js
const Main = require('./output/Main');

Main.main();
```

To build that whole thing now:

```bash
$ yarn spago build && yarn parcel index.html
```

And finally, if you run this, nothing will work.

All because the return type of the `main` function is not really `Effect Unit`, but rather a function. To fix this:

```purescript
mountMain :: HTMLElement -> Effect Unit
mountMain elt = do
  let container = toElement elt
  let componentInstance = React.createLeafElement areaCalculator {}
  let componentMaybe = ReactDOM.render componentInstance container
  void componentMaybe

main :: Effect Unit
main = do
  win <- window
  doc <- document win
  eltMaybe <- body doc
  maybe (pure unit) mount eltMaybe
```

And the thing still won’t completely work, since we do not modify the state on input value change:

```purescript
import Data.Float.Parse (parseFloat)

onValueChanged ctx evt = do
  let newValue = fromMaybe 0.0 (parseFloat ((unsafeCoerce evt).target.value))
  React.setState ctx { value: newValue }
```

Final solution is available on [sandbox](https://codesandbox.io/s/purescript-test1-02wf4).
