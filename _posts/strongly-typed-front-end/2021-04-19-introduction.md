---
layout: post
title: 'Strongly-typed front-end: introduction'
date: '2021-04-19T08:36:15+09:00'
---

In this little research project I describe my journey through a series of experiments trying out a number of technologies and clashing them against each other.

I have always questioned the _real value_ all those languages that compile to JS, especially TypeScript or Flow, give you.

So I have asked Atlassian front-enders a question:

> I need your opinions for my research: what benefits does TypeScript (or Flow, depending on your camp) give you? why do you use it?

The answers I have received varied but the common themes were:

* we like types! 😍
* less errors
* easier refactoring
* tools & IDEs integration (mainly for code navigation and autocomplete)
* self-documented or more readable code

The issues I see with TypeScript and Flow are bit closer to the real world:

* they catch way too few errors - unless your **whole project** (including 3rd party dependencies) is using the thing **correctly**, you will see the errors whenever you end up in the layer between native JS and typed code
* more often than not, error messages are either pointless or hard to read (see the examples below)

I do acknowledge the earlier you catch an error, the cheaper the fix would be. You have to put some effort into writing the typed code, but if it only catches a fraction of errors and only at compile time, then why all the hassle?

## "Benefits" of TypeScript

### Pointless errors

The most issues I have seen so far happen in what is considered an stdlib:

```ts
interface MyClass {
  id: string;
  val: number;
}

const a: MyClass[] = [
  { id : 'moo', val: 1 }, 
  { id: 'foo', val: -1 },
  { id: 'bar', val: 3.14 },
];

const counts = a.reduce((acc, e) => {
  if (acc.has(e.id)) {
    acc.set(e.id, acc.get(e.id) + e.val);
  } else {
    acc.set(e.id, e.val);
  }
  
  return acc;
}, new Map<string, number>());
```

Here we are reducing a list of objects. TS is freaking out every time you are using `Map` (however, it is a natively supported type, IIRC) - calling `map.get()` will always produce `T | undefined` type, unless you explicitly tell TS the type is `T` by using the `as` operator:

```ts
acc.set(e.id, acc.get(e.id) + e.val); // ERROR: Object is possibly 'undefined'.
```

Adding an explicit check does not have any effect:

```ts
if (acc.has(e.id) && acc.get(e.id) !== undefined) {
  acc.set(e.id, acc.get(e.id) + e.val); // TS does not give a damn: Object is possibly 'undefined'.
}
```

whereas

```ts
acc.set(e.id, acc.get(e.id) as number + e.val); // OK
```

But the issue is: if you do not add the check, the value in fact might be `undefined`.

Flow has flaws here too:

```js
/* @flow */

interface MyClass {
  id: string;
  val: number;
}

const a: MyClass[] = [
  { id : 'moo', val: 1 }, 
  { id: 'foo', val: -1 },
  { id: 'bar', val: 3.14 },
];

const counts = a.reduce((acc, e) => {
  if (acc.has(e.id) && acc.get(e.id) !== undefined) { // ERROR: Cannot perform arithmetic operation because undefined [1] is not a number. [unsafe-addition]
    acc.set(e.id, acc.get(e.id) + e.val);
  } else {
    acc.set(e.id, e.val);
  }
  
  return acc;
}, new Map<string, number>());
```

But it provides a bit more context about the issue:

```
    16:     acc.set(e.id, acc.get(e.id) + e.val);
                          ^ Cannot perform arithmetic operation because undefined [1] is not a number. [unsafe-addition]
        References:
        [LIB] ..//static/v0.135.0/flowlib/core.js:617:     get(key: K): V | void;
                                                                            ^ [1]
```

Both Flow and TS work fine if you extract the `.get` call result to a variable and add a check for `undefined`:

```ts
interface MyClass {
    id: string;
    val: number;
}

const a: MyClass[] = [
    { id : 'moo', val: 1 }, 
    { id: 'foo', val: -1 },
    { id: 'bar', val: 3.14 },
];

const counts = a.reduce((acc, e) => {
    const prevVal = acc.get(e.id);

    if (prevVal !== undefined) {
        acc.set(e.id, prevVal + e.val);
    } else {
        acc.set(e.id, e.val);
    }

    return acc;
}, new Map<string, number>());
```

### Implementation-specific errors

In order to _understand_ this error message, you have to know how enums are implemented in TypeScript:

```ts
enum Figure {
  RECTANGLE,
  SQUARE,
  CIRCLE,
}

const area: Record<Figure, Function> = {
  [Figure.RECTANGLE]: (w: number, h: number) => w * h,
  [Figure.CIRCLE]: (r: number) => Math.PI * r * r,
  // ERROR: Property '1' is missing in type '{ 0: (w: number, h: number) => number; 2: (r: number) => number; }' but required in type 'Record<Figure, Function>'.
};

console.log(area);
```

Enums in TS are backed by numbers, _by default_. In order for that error above to make sense, you have to provide some sort of a reasonable (`.toString()`-backed) value for enum values:

```ts
enum Figure {
  RECTANGLE = 'RECTANGLE',
  SQUARE = 'SQUARE',
  CIRCLE = 'CIRCLE',
}

const area: Record<Figure, Function> = {
  [Figure.RECTANGLE]: (w: number, h: number) => w * h,
  [Figure.CIRCLE]: (r: number) => Math.PI * r * r,
  // ERROR: Property 'SQUARE' is missing in type '{ RECTANGLE: (w: number, h: number) => number; CIRCLE: (r: number) => number; }' but required in type 'Record<Figure, Function>'.
};

console.log(area);
```

### Runtime is imperfect

You might have typed every single bit of your project and all the 3rd party dependencies. And you did it right. This still does not guarantee you won’t have `Can not read property XXX of undefined or XXX is not a function` at run time.

Type system won’t really save you, if you only have covered some of the use cases, but the user ended up in uncovered one:

```ts
import React, { useState, useCallback } from 'react';

enum Shape {
  SQUARE = 'SQUARE',
  CIRCLE = 'CIRCLE',
}

const AREA: Record<Shape, Function> = {
  [Shape.SQUARE]: (side: number) => side * side,
  [Shape.CIRCLE]: (r: number) => Math.PI * r * r,
};

export default () => {
  const [shape, setShape] = useState<Shape>(null);
  const [value, setValue] = useState<number>(0);
  const [area, setArea] = useState<number>(0);

  const onShapeChanged = useCallback((e: React.ChangeEvent<HTMLSelectElement>) => {
    setShape(e.target.value);
  }, []);

  const onValueChanged = useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    setValue(parseFloat(e.target.value));
  }, []);

  const onSubmit = useCallback(() => {
    setArea(AREA[shape](value));
  }, [shape, value]);

  return (
    <div>
      <select onChange={onShapeChanged}>
        <option value="">Choose shape</option>

        {Object.keys(AREA).map(shape => (<option value={shape}>{shape}</option>))}
      </select>

      <input value={value} onChange={onValueChanged} />

      <button onClick={onSubmit}>Calculate area</button>

      <div>
        Area: {area}
      </div>
    </div>
  );
};
```

This is a quite simple application ([sandbox](https://codesandbox.io/s/happy-sutherland-1qtwb)), built on top of the previous example with enums and records in TypeScript.

There are at least two uncovered scenarios in this application, resulting in errors:

1. when user does not select a shape and clicks “calculate”, the `TypeError: AREA[shape] is not a function` will be thrown
2. when user types anything but number in the input, the value immediately becomes `NaN`; if user then clicks “calculate”, an error won’t be thrown (since the app does not use the calculation result in any way), but the area calculated will also be `NaN`; for this example this is fine, but imagine using the value further down the line in some financial calculations

This is a trivial synthetic example and the errors might be easy to spot and fix, but the important question is: _did TypeScript help you find those errors?_

If you set up TSLint, you might have _some_ errors caught:

```
Argument of type 'null' is not assignable to parameter of type 'Shape | (() => Shape)'.ts(2345)
Argument of type 'string' is not assignable to parameter of type 'SetStateAction<Shape>'.ts(2345)
Type 'null' cannot be used as an index type.ts(2538)
```

Unless you do the right thing, you _might_ end up fixing those scenarios. But instead, I often see solutions like these ([sandbox](https://codesandbox.io/s/winter-river-dvzwd)):

```ts
const [shape, setShape] = useState<Shape | null>(null);

// ...

setShape(e.target.value as Shape);

// ...

setArea(shape ? AREA[shape](value) : 0);
```

Those solutions do solve a subset of errors, at a cost of readability and potential other errors.

### There are no classes in JavaScript

Found this one recently, apparently TypeScript classes are same as JavaScript (ES6) classes:

```ts
namespace TypeScriptIsGarbage {
  export class A {};

  export class B {};

  export const a = (): A => new B(); // perfectly fine

  export const b = (): B => new A(); // perfectly fine
}

namespace TypeScriptIsJavaScript {
  export enum Types {
    A,
    B,
  }

  export type A = { type: Types.A };

  export type B = { type: Types.B };

  export const a = (): A => ({ type: Types.B }); // Type 'Types.B' is not assignable to type 'Types.A'.

  export const b = (): B => ({ type: Types.A }); // Type 'Types.A' is not assignable to type 'Types.B'.
}
```

This one is actually quite serious, if your application relies on type safety and objective-oriented-design.

Try doing it in C# (which TS tries to inherit from, iirc) and yo'll get sane compile-time errors:

```csharp
using System;

class A {}

class B {}

class Main {
  A createA() {
    return new B(); // Cannot implicitly convert type `B` to `A`
  }

  B createB() {
    return new A(); // Cannot implicitly convert type `A` to `B`
  }

  public static void Main(string[] args) {}
}
```

### IDE integration is awesome

Recently I had to use both Cypress and Jest in a project of mine. Cypress was used for E2E tests and Jest was used for unit-tests. And they both provide some sort of assertion framework (think all those `expect()` calls).

And apparently their definitions are different and are clashing, since my VSCode looks like this:

<img data-src="/images/strongly-typed-front-end/ts-vscode-integration-1.png" alt="TS definitions errors in VSCode">

<img data-src="/images/strongly-typed-front-end/ts-vscode-integration-2.png" alt="TS definitions errors in VSCode">

Apparently, I needed two separate `tsconfig.json` files, for each specific set of tests to even compile the thing. Which is still not recognized by VSCode.

### Errors can be found and eliminated early

The helpfulness of the error messages by TS compiler is far from perfect. And same holds for TSLint.

And in some cases (as with type checks), they are even completely missing, so good luck finding out why the application does not work.

It is possible to add a bunch of `debugger` statements, breakpoints and `console.log()`s to the code. But what is the benefit of that complex setup and extra overhead of typing every single line then?
