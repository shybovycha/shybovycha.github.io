---
layout: post
title: 'Experiment #1: mismatching type handling & error helpfulness'
date: '2021-04-19T08:56:24+09:00'
---

### Contents

1. [Introduction](/strongly-typed-front-end/2021/04/19/introduction.html)
2. [**Experiment 1, darken_color  (you are here)**](/strongly-typed-front-end/experiment-1/2021/04/19/experiment-1.html)
3. Experiment 2, simple application
    - [Elm](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-elm.html)
    - [F#](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-fsharp.html)
    - [PureScript & purescript-react-dom](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-purescript.html)
    - [PureScript & Halogen](/strongly-typed-front-end/experiment-2/2024/05/17/experiment-2-purescript-halogen.html)
    - [ReasonML](/strongly-typed-front-end/experiment-2/2021/04/19/experiment-2-reasonml.html)

For a sake 🍶of <del>science</del> experiment, I have [converted](https://github.com/shybovycha/darken_color.js/tree/experiment/strong-typing) **one function** of a [library](https://github.com/shybovycha/darken_color.js) I created long time ago to multiple languages that compile to JS and called it with various values.

The function is simple - it takes a color represented as a HEX string and converts it to `{ r, g, b }` object.

The test is relatively big - it passes various numbers (integer and floating point, negative and positive), booleans, objects, arrays, obvious candidates - `null` and `undefined` and incorrect string.

The implementations are made with:

* Scala.js
* ReasonML
* F#
* PureScript
* TypeScript
* Elm

## Implementations

### Scala.JS

```scala
package darken_color

import scala.scalajs.js
import scala.scalajs.js.annotation._

class RGB(val r: Int, val g: Int, val b: Int) extends js.Object

@JSExportTopLevel("DarkenColor")
object DarkenColor {
  @JSExport
  def hex2rgb(s: String): RGB = {
    val re = """^#?([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$""".r

    val rgbStr = s match {
      case re(rStr, gStr, bStr) => Some((rStr, gStr, bStr))
      case _ => None
    }

    rgbStr.map (x => new RGB(Integer.parseInt(x._1, 16), Integer.parseInt(x._2, 16), Integer.parseInt(x._3, 16))).getOrElse(null)
  }
}
```

### ReasonML

```reason
type rgb = {
  r: int,
  g: int,
  b: int,
}

let parse_hex = s => int_of_string("0x" ++ s)

let hex2rgb = hex =>
  Js.Re.fromString("^#?([a-f0-9]{2})([a-f0-9]{2})([a-f0-9]{2})$")
    -> Js.Re.exec_(hex)
    -> Belt.Option.map (Js.Re.captures)
    -> Belt.Option.map (Js.Array.map (Js.Nullable.toOption))
    -> Belt.Option.map (x => Js.Array.sliceFrom(1, x))
    -> Belt.Option.map (Js.Array.map (x => Belt.Option.map(x, parse_hex)))
    -> (matches => switch matches {
      | Some([ Some(r), Some(g), Some(b) ]) => Some({ r: r, g: g, b: b })
      | _ => None
    })
```

### PureScript

```purescript
module DarkenColor where

import Prelude (join, map, ($), (<#>), (>>=), (>>>))

import Data.Array (catMaybes)
import Data.Array.NonEmpty (drop)
import Data.Int (fromStringAs, hexadecimal)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Either (hush)
import Data.String.Regex (regex, match)
import Data.String.Regex.Flags (ignoreCase)

type RGB =
  {
    r :: Int,
    g :: Int,
    b :: Int
  }

constructRGB :: Array Int -> Maybe RGB
constructRGB [ r, g, b ] = Just { r: r, g: g, b: b }
constructRGB _ = Nothing

hex2rgb :: String -> Nullable RGB
hex2rgb hexString =
  toNullable $
  ((hush >>> join) $ (regex "^#?([0-9a-f]{2})([0-9a-f]{2})([0-9a-f]{2})$" ignoreCase) <#> (\re -> (match re hexString)))
  <#> (drop 1)
  <#> catMaybes
  <#> (map (fromStringAs hexadecimal))
  <#> catMaybes
  >>= constructRGB
```

### F#

```fsharp
module DarkenColor

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

### TypeScript

```ts
interface RGBType {
  r: number;
  g: number;
  b: number;
}

/**
  * Converts a HEX color value to RGB by extracting R, G and B values from string using regex.
  * Returns r, g, and b values in range [0, 255]. Does not support RGBA colors just yet.
  *
  * @param hex The color value
  * @returns The RGB representation or {@code null} if the string value is invalid
  */
const hex2rgb = (hex: string): RGBType => {
  // Expand shorthand form (e.g. "03F") to full form (e.g. "0033FF")
  const shorthandRegex = /^#?([a-f\d])([a-f\d])([a-f\d])$/i;

  hex = hex.replace(shorthandRegex, (_match, r, g, b) => {
    return r + r + g + g + b + b;
  });

  const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);

  if (!result) {
    return undefined;
  }

  return {
    r: parseInt(result[1], 16),
    g: parseInt(result[2], 16),
    b: parseInt(result[3], 16)
  };
}

export { hex2rgb };
```

### Elm

```haskell
module DarkenColor exposing (..)

import List
import Maybe
import Maybe.Extra
import Regex

type alias RGBType = { r: Int, g: Int, b: Int }

hex2rgb : String -> Maybe RGBType
hex2rgb hex =
    Regex.fromString "^#?([a-f0-9]{2})([a-f0-9]{2})([a-f0-9]{2})$"
        |> Maybe.map (\regex -> Regex.find regex hex)
        |> Maybe.map (List.map .match)
        |> Maybe.map (List.map String.toInt)
        |> Maybe.andThen (Maybe.Extra.combine)
        |> Maybe.andThen constructRGB

constructRGB list =
    case list of
        [ r, g, b ] -> Maybe.Just { r = r, g = g, b = b }
        _ -> Maybe.Nothing
```

For fair comparison, the implementation is kept same (no platform-specific code, except `Option` in functional languages) and every single bundle is processed with Webpack 4.

The test checks both the result and the assumes no exception is thrown, even when the input is incorrect.
For the interest sake, the exceptions thrown as well as bundle sizes will be listed below.

<!--more-->

### The test

```js
const FSharp = require('./dist/bundle.fsharp.js');
const Purescript = require('./dist/bundle.purescript.js');
const Rescript = require('./dist/bundle.rescript.js');
const ScalaJS = require('./dist/bundle.scalajs.js');
const Typescript = require('./dist/bundle.typescript.js');

describe('DarkenColor', () => {
  const interpretations = {
    'F#': FSharp,
    'PureScript': Purescript,
    'ReScript': Rescript,
    'ScalaJS': ScalaJS.DarkenColor,
    'TypeScript': Typescript
  };

  Object.entries(interpretations).forEach(([language, DarkenColor]) => {
    describe(`in ${language}`, () => {
      describe('hex2rgb', () => {
        describe('for valid input', () => {
          describe('with hash prefix', () => {
            describe('long HEX', () => {
              const input = '#bede12';

              it('returns correct result', () => {
                expect(DarkenColor.hex2rgb(input)).toMatchObject({ r: 190, g: 222, b: 18 });
              });
            });

            xdescribe('short HEX', () => {
              const input = '#bd7';

              it('returns correct result', () => {
                expect(DarkenColor.hex2rgb(input)).toMatchObject({ r: 187, g: 221, b: 119 });
              });
            });
          });

          describe('no hash prefix', () => {
            describe('long HEX', () => {
              const input = 'bede12';

              it('returns correct result', () => {
                expect(DarkenColor.hex2rgb(input)).toMatchObject({ r: 190, g: 222, b: 18 });
              });
            });

            xdescribe('short HEX', () => {
              const input = 'bd7';

              it('returns correct result', () => {
                expect(DarkenColor.hex2rgb(input)).toMatchObject({ r: 187, g: 221, b: 119 });
              });
            });
          });
        });

        describe('for invalid input', () => {
          describe('invalid HEX string', () => {
            describe('invalid number of digits', () => {
              describe('too few', () => {
                describe('for long HEX', () => {
                  const input = '#bede1';

                  it('does not fail', () => {
                    expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
                  });
                });

                xdescribe('for short HEX', () => {
                  const input = '#be';

                  it('does not fail', () => {
                    expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
                  });
                });
              });

              describe('too many', () => {
                describe('for long HEX', () => {
                  const input = '#bede128';

                  it('does not fail', () => {
                    expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
                  });
                });

                xdescribe('for short HEX', () => {
                  const input = '#bede';

                  it('does not fail', () => {
                    expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
                  });
                });
              });
            });

            describe('invalid letters', () => {
              const input = '#bfghij';

              it('does not fail', () => {
                expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
              });
            });

            describe('invalid characters', () => {
              const input = '#be!?12';

              it('does not fail', () => {
                expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
              });
            });

            describe('spaces', () => {
              const input = '#be de  12';

              it('does not fail', () => {
                expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
              });
            });

            describe('invalid prefix', () => {
              const input = '?bede12';

              it('does not fail', () => {
                expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
              });
            });
          });
        });

        describe('for number', () => {
          describe('positive', () => {
            describe('floating point', () => {
              const input = 3.14;

              it('does not fail', () => {
                expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
              });
            });

            describe('integer', () => {
              const input = 314;

              it('does not fail', () => {
                expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
              });
            });
          });

          describe('negative', () => {
            describe('floating point', () => {
              const input = -3.14;

              it('does not fail', () => {
                expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
              });
            });

            describe('integer', () => {
              const input = -314;

              it('does not fail', () => {
                expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
              });
            });
          });
        });

        describe('for object', () => {
          describe('null', () => {
            const input = null;

            it('does not fail', () => {
              expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
            });
          });

          describe('undefined', () => {
            const input = undefined;

            it('does not fail', () => {
              expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
            });
          });

          describe('NaN', () => {
            const input = NaN;

            it('does not fail', () => {
              expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
            });
          });

          describe('JSON', () => {
            const input = { r: 25, g: 76, b: 120 };

            it('does not fail', () => {
              expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
            });
          });

          describe('array', () => {
            const input = [ 25, 76, 120 ];

            it('does not fail', () => {
              expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
            });
          });

          describe('Boolean', () => {
            const input = true;

            it('does not fail', () => {
              expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
            });
          });

          describe('Map', () => {
            const input = new Map();

            it('does not fail', () => {
              expect(() => DarkenColor.hex2rgb(input)).not.toThrow();
            });
          });
        });
      });
    });
  });
});
```

## Test results

Here is a comparison of the test results:

<table>
  <thead>
    <tr>
      <th>Tool</th>
      <th>Test results</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>F#</td>
      <td>✅</td>
    </tr>
    <tr>
      <td>PureScript</td>
      <td>❌</td>
    </tr>
    <tr>
      <td>ReScript</td>
      <td>✅</td>
    </tr>
    <tr>
      <td>ScalaJS</td>
      <td>❌</td>
    </tr>
    <tr>
      <td>TypeScript</td>
      <td>❌</td>
    </tr>
    <tr>
      <td>Elm</td>
      <td>n/a</td>
    </tr>
  </tbody>
</table>

## Error helpfulness

Some implementations still throw error when the data type is not what the function expects.

### PureScript

```
  ● DarkenColor › in PureScript › hex2rgb › for number › positive › integer › does not fail

    expect(received).not.toThrow()

    Error name:    "TypeError"
    Error message: "u.match is not a function"
    
  ● DarkenColor › in PureScript › hex2rgb › for object › null › does not fail

    expect(received).not.toThrow()

    Error name:    "TypeError"
    Error message: "Cannot read property 'match' of null"
    
  ● DarkenColor › in PureScript › hex2rgb › for object › undefined › does not fail

    expect(received).not.toThrow()

    Error name:    "TypeError"
    Error message: "Cannot read property 'match' of undefined"
```

### ScalaJS

```
 ● DarkenColor › in ScalaJS › hex2rgb › for object › undefined › does not fail

    expect(received).not.toThrow()

    Error name:    "org.scalajs.linker.runtime.UndefinedBehaviorError"
    Error message: "java.lang.ClassCastException: undefined is not an instance of java.lang.String"

  ● DarkenColor › in ScalaJS › hex2rgb › for number › positive › floating point › does not fail

    expect(received).not.toThrow()

    Error name:    "org.scalajs.linker.runtime.UndefinedBehaviorError"
    Error message: "java.lang.ClassCastException: 3.14 is not an instance of java.lang.String"
```

### TypeScript

```
 ● DarkenColor › in TypeScript › hex2rgb › for number › positive › floating point › does not fail

    expect(received).not.toThrow()

    Error name:    "TypeError"
    Error message: "e.replace is not a function"

  ● DarkenColor › in TypeScript › hex2rgb › for object › null › does not fail

    expect(received).not.toThrow()

    Error name:    "TypeError"
    Error message: "Cannot read property 'replace' of null"

  ● DarkenColor › in TypeScript › hex2rgb › for object › undefined › does not fail

    expect(received).not.toThrow()

    Error name:    "TypeError"
    Error message: "Cannot read property 'replace' of undefined"
```

### Elm

```
Detected problems in 1 module.
-- TYPE MISMATCH ------------------------------------------- src/DarkenColor.elm

The 1st argument to `hex2rgb` is not what I expect:

24| main = hex2rgb -12
                   ^^^
This argument is a number of type:

    number

But `hex2rgb` needs the 1st argument to be:

    String

Hint: Try using String.fromInt to convert it to a string?
```

## Bundle size

<table>
  <thead>
    <tr>
      <th>Tool</th>
      <th>Bundle size</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>F#</td>
      <td>40K</td>
    </tr>
    <tr>
      <td>PureScript</td>
      <td>174K</td>
    </tr>
    <tr>
      <td>ReScript</td>
      <td>22K</td>
    </tr>
    <tr>
      <td>ScalaJS</td>
      <td>197K</td>
    </tr>
    <tr>
      <td>TypeScript</td>
      <td>1.4K</td>
    </tr>
    <tr>
      <td>Elm</td>
      <td>n/a</td>
    </tr>
  </tbody>
</table>

<img src="/images/strongly-typed-front-end/chart1.png" loading="lazy" alt="Bundle size comparison">
