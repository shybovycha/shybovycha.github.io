---
layout: post
title: 'Algorithmic complexity in real life'
tags: [programming, computer-science, algorithms]
---

Some people still say stuff like _"you don't have to know how a combustion engine works to drive a car, so why study algos?"_.

Real-life scenario: in JS, try running the following code with lodash:

```js
_.uniqWith(largeArrayOfArrays, _.equalsTo)
```

This code actually has a time complexity of `O(n^3)`.

TODO: describe why the complexity is n^3

TODO: add benchmarks

Compare this to the following, arguably more complicated, code:

```js
_.map(_.uniq(_.map(largeArrayOfArrays, JSON.stringify)), JSON.parse)
```

This one has a time complexity of `O(n^2)`.

TODO: describe why the complexity is n^2

TODO: add benchmarks