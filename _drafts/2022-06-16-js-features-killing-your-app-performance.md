---
title: "JavaScript features killing your application's performance"
layout: post
tags: [javascript, performance, typescript, type-safety, java, benchmarking, optimization, programming, frontend, web-development]
---

Thesis: spread operator when used with array-iterating functions (filter, map, reduce, etc.) is killing your app performance.

Proof:

## Pure JavaScript

```js
const users = [
    { name: "moo", age: 12 },
    { name: "moo1", age: 42 },
    { name: "moo2", age: 7 },
    { name: "moo3", age: 30 },
    { name: "moo4", age: 28 },
    { name: "moo5", age: 24 },
];

const youngsters = users.reduce((acc, user) => user.age > 7 && user.age < 40 ? [...acc, user] : acc, []);
```

## TypeScript

```ts
type User = {
    name: string;
    age: number;
};

const users: Array<User> = [
    { name: "moo", age: 12 },
    { name: "moo1", age: 42 },
    { name: "moo2", age: 7 },
    { name: "moo3", age: 30 },
    { name: "moo4", age: 28 },
    { name: "moo5", age: 24 },
];

const youngsters = users.reduce((acc, user) => user.age > 7 && user.age < 40 ? [...acc, user] : acc, [] as Array<User>);
```

When compiled to ES5:

```js
"use strict";
var __spreadArray = (this && this.__spreadArray) || function (to, from, pack) {
    if (pack || arguments.length === 2) for (var i = 0, l = from.length, ar; i < l; i++) {
        if (ar || !(i in from)) {
            if (!ar) ar = Array.prototype.slice.call(from, 0, i);
            ar[i] = from[i];
        }
    }
    return to.concat(ar || Array.prototype.slice.call(from));
};
var users = [
    { name: "moo", age: 12 },
    { name: "moo1", age: 42 },
    { name: "moo2", age: 7 },
    { name: "moo3", age: 30 },
    { name: "moo4", age: 28 },
    { name: "moo5", age: 24 },
];
var youngsters = users.reduce(function (acc, user) { return user.age > 7 && user.age < 40 ? __spreadArray(__spreadArray([], acc, true), [user], false) : acc; }, []);
```

When compiling to the newer JS versions, it becomes equal to the pure JS version, utilizing the spread operator:


```js
"use strict";
const users = [
    { name: "moo", age: 12 },
    { name: "moo1", age: 42 },
    { name: "moo2", age: 7 },
    { name: "moo3", age: 30 },
    { name: "moo4", age: 28 },
    { name: "moo5", age: 24 },
];
const youngsters = users.reduce((acc, user) => user.age > 7 && user.age < 40 ? [...acc, user] : acc, []);
```

## PureScript

```hs
module Main where

import Prelude
import Effect.Console (log)
import TryPureScript (render, withConsole)
import Data.Foldable (foldl)

type User = {
  name :: String,
  age :: Int
}

showPerson :: User -> String
showPerson o = o.name <> " (" <> (show o.age) <> ")"

users :: Array User
users = [
  { name: "moo", age: 12 },
  { name: "moo1", age: 42 },
  { name: "moo2", age: 7 },
  { name: "moo3", age: 30 },
  { name: "moo4", age: 28 },
  { name: "moo5", age: 24 } ]

isYoungster :: User -> Boolean
isYoungster user = user.age > 7 && user.age < 40

getYoungsters :: Array User -> Array User
getYoungsters us = foldl (\acc user -> if isYoungster user then acc <> [user] else acc) [] us

mapYoungsters :: Array User -> String
mapYoungsters us = foldl (\acc user -> acc <> (showPerson user) <> ", ") "" us

main = render =<< withConsole do
  log $ mapYoungsters (getYoungsters users)
```

## Elm

```hs
import Html exposing (text)
import List exposing (foldl)
import String exposing (fromInt)

type alias User =
  { name : String
  , age : Int
  }

users = [
  { name = "moo", age = 12 },
  { name = "moo1", age = 42 },
  { name = "moo2", age = 7 },
  { name = "moo3", age = 30 },
  { name = "moo4", age = 28 },
  { name = "moo5", age = 24 } ]

showPerson : User -> String
showPerson o = o.name ++ " (" ++ (fromInt o.age) ++ ")"

isYoungster : User -> Bool
isYoungster user = user.age > 7 && user.age < 40

getYoungsters : List User -> List User
getYoungsters us = foldl (\user acc -> if isYoungster user then acc ++ [user] else acc) [] us

mapYoungsters : List User -> String
mapYoungsters us = foldl (\user acc -> acc ++ (showPerson user) ++ ", ") "" us

main =
  text (mapYoungsters (getYoungsters users))
```

## Benchmark

```js
const users = [];

for (let i = 0; i < 10000000; i++) {
  users.push({ name: "moo", age: Math.random() % 30 });
}

console.time("spreadOperator + reduce");
const youngsters = users.reduce(
  (acc, user) => (user.age > 7 && user.age < 40 ? [...acc, user] : acc),
  []
);
console.timeEnd("spreadOperator + reduce");
```

yielding

```
spreadOperator + reduce: 486ms - timer ended
spreadOperator + reduce: 461ms - timer ended
spreadOperator + reduce: 469ms - timer ended
spreadOperator + reduce: 731ms - timer ended
spreadOperator + reduce: 731ms - timer ended
spreadOperator + reduce: 1048ms - timer ended
spreadOperator + reduce: 742ms - timer ended
spreadOperator + reduce: 454ms - timer ended
```

So for 10 million objects this operation takes 486ms in Firefox.

Let's try refactoring this to make it more peformant:

```js
const users = [];

for (let i = 0; i < 10000000; i++) {
  users.push({ name: "moo", age: Math.random() % 30 });
}

console.time("concat + reduce");
const youngsters = users.reduce(
  (acc, user) => (user.age > 7 && user.age < 40 ? acc.concat([user]) : acc),
  []
);
console.timeEnd("concat + reduce");
```

yielding

```
concat + reduce: 219ms - timer ended
concat + reduce: 465ms - timer ended
concat + reduce: 480ms - timer ended
concat + reduce: 466ms - timer ended
concat + reduce: 456ms - timer ended
concat + reduce: 525ms - timer ended
concat + reduce: 210ms - timer ended
concat + reduce: 467ms - timer ended
concat + reduce: 462ms - timer ended

```

`2 ms` difference is pretty negligible.

Let's see if instead of constantly creating a new array we reuse the memory of an existing one:

```js
const users = [];

for (let i = 0; i < 10000000; i++) {
  users.push({ name: "moo", age: Math.random() % 30 });
}

console.time("push + reduce");
const youngsters = users.reduce(
  (acc, user) => {
    if (user.age > 7 && user.age < 40)
        acc.push(user);

    return acc;
  },
  []
);
console.timeEnd("push + reduce");
```

yields

```
push + reduce: 468ms - timer ended
push + reduce: 237ms - timer ended
push + reduce: 467ms - timer ended
push + reduce: 460ms - timer ended
push + reduce: 485ms - timer ended
push + reduce: 439ms - timer ended
push + reduce: 463ms - timer ended
push + reduce: 461ms - timer ended
```


