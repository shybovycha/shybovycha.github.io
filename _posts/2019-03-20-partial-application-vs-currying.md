---
layout: post
title: Partial application vs currying
date: '2019-03-20T14:18:56+10:00'
tags:
- functional programming
- programming
---

Here's a quick explanation of the difference between **currying** and **partial application**.

For a one- or two-argument functions there is little to none difference. But for function with multiple arguments...

Say you have a function `f(a1, a2, a3, a4)` (it does not really matter what it returns or what the arguments are):

* *currying* it will give you a function of one argument, which will return a function of one argument (and so on and so forth), calling which (the last one-argument function) will give you same result as calling the original function `f` with all the arguments of single-argument functions passed at once *(curried)*: `curry(f) = f1(x1) => f2(x2) => f3(x3) => f4(x4) === f(x1, x2, x3, x4)`

* *partially applying* a function *to some N values* will give you a function of smaller amount of arguments, where the first *N* arguments are already defined: `papply(f, 1, 17) => f1(x3, x4) === f(1, 17, x3, x4)`

In Javascript, the two could be implemented like this:

```js
function curry(f) {
    function curry(f) {
    const currySub = function (fn, args) {
        // Surprisingly, `fn.length` returns the number of arguments of a function `fn`
        if (args.length < fn.length) {
            return function (x) {
                return currySub(fn, args.concat([ x ]));
            };
        }

        return f.apply(null, args);
    }

    return currySub(f, []);
}

function papply() {
    const f = arguments[0];
    const args0 = Array.from(arguments).slice(1);

    return function () {
        const args1 = Array.from(arguments);

        const args = args0.concat(args1);

        return f.apply(null, args);
    }
}
```
