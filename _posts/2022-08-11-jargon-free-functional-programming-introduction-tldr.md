---
layout: post
title: 'Jargon-free functional programming. TL;DR'
date: '2022-08-11T17:00:00+10:00'
---

This is a boiled-down version of a much longer read, <a href="/2022/08/24/jargon-free-functional-programming-part1.html"><em>Jargon-free functional programming</em></a>,
giving a brief and visual introduction to the concepts of a real-world functional programming. This blog is aimed at people who already know something
about programming and want to learn what the heck functional programming is, how is it different to "normal" programming and how does it look like
in a real world.

In a non-functional world, the code we write depends on anything - a function, aside from its arguments, is free to use environment variables,
global variables, outer scope, dependency injection - pretty much anything.

<img data-src="/images/jargon-free-functional-programming/Functional programming 1 1.png" alt="" />

Moreover, it can modify all of the above (including outer scope, global and environment variables, etc.).

<img data-src="/images/jargon-free-functional-programming/Functional programming 1 2.1.png" alt="" />

In a functional programming world we restrict a function to only rely on its arguments (or nothing at all).

<img data-src="/images/jargon-free-functional-programming/Functional programming 1 4.png" alt="" />

But what about things like databases, user input, network communication, exceptions?

<img data-src="/images/jargon-free-functional-programming/Functional programming 1 5.png" alt="" />
<img data-src="/images/jargon-free-functional-programming/Functional programming 1 6.png" alt="" />
<img data-src="/images/jargon-free-functional-programming/Functional programming 1 7.png" alt="" />
<img data-src="/images/jargon-free-functional-programming/Functional programming 1 8.png" alt="" />

A typical application involving all of the above could be explained algorithmically as the endless loop, waiting for some _input_ to appear
before doing something (waiting for database query to complete, waiting for user to provide input, waiting for a network request to complete).

<img data-src="/images/jargon-free-functional-programming/Functional programming 1 9.png" alt="" />

And every step of the program is described as a sequence of actions (potentially involving some rather trivial decision making).
This approach is known as "imperative programming" and is very commonly used.

<img data-src="/images/jargon-free-functional-programming/Functional programming 1 10.png" alt="" />

In reality, however, every step of this algorithm can go wrong in many different ways - each step is free to modify some global state (think OS and filesystem),
it can fail terribly with an exception. Moreover, anything from the outside world (think OS or dependency injection) can break into the program and change any value
or state of the program.

<img data-src="/images/jargon-free-functional-programming/Functional programming 1 11.png" alt="" />

In a functional programming world, functions (and programs) are not described as sequences of commands - instead, they are more like recipes
for calculations that will happen once all the requirements are provided.

<div class="content-read-marker" data-fraction="25"></div>

<img data-src="/images/jargon-free-functional-programming/Functional programming 1 12.png" alt="" />

The way to handle all the nifty things such as exceptions, networking, databases, etc. is to wrap a function which works with a _result of the "unsafe" operation_ in a
safe container. This container won't execute the function - just hold it for a while. However, this container would have two special properties: an ability to
run the underlying function when it is deemed safe and an ability to be connected to other containers _of the same type_.

<img data-src="/images/jargon-free-functional-programming/Functional programming 2 3.png" alt="" />

Each container will perform its very specific role - handling exceptions to return a value instead of breaking,
running some input-output operations (incl. networking and databases), etc. We assume containers already do these operations
in a safe manner - meaning they do not change anything in the program outside of themselves (think global variables, outer scope, etc.)
and they always return a value. They only execute the function they wrap once requested explicitly.

<img data-src="/images/jargon-free-functional-programming/Functional programming 2 4.png" alt="" />
<img data-src="/images/jargon-free-functional-programming/Functional programming 2 6.png" alt="" />

By making it so that safe containers of different types can not be chained, we eliminate the chance of unexpected program failure.
And we make sure at any point in time we can say what a program is doing exactly by just looking at its types.

By connecting such containers in a chain, we make programs.

<img data-src="/images/jargon-free-functional-programming/Functional programming 2 7.png" alt="" />

But these chains do not do anything until they are explicitly executed.

<img data-src="/images/jargon-free-functional-programming/Functional programming 2 8.png" alt="" />

A program is a chain of functions, wrapped in "safe" constructs, which is executed "at the end / edge of the world" - meaning program is thought to be executed only once.
If all the blocks of this chain of containers succeed - the entire program succeeds.

<img data-src="/images/jargon-free-functional-programming/Functional programming 2 9.png" alt="" />

If any of the blocks fails - the program does not exit or terminates, the failed block simply returns a different value.

<img data-src="/images/jargon-free-functional-programming/Functional programming 2 11.png" alt="" />

All the logic is hidden in those "safe" constructs - it is isolated from the rest of the world.

<img data-src="/images/jargon-free-functional-programming/Functional programming 1 15.png" alt="" />

Those containers are only allowed access to their direct arguments. It is guaranteed to never break and always
return a value (which might be wrapped in another "safe" construct).

<img data-src="/images/jargon-free-functional-programming/Functional programming 1 14.png" alt="" />

A program made of these safe recipes on how to calculate the result is just another recipe itself - essentially a series of recipes.

<div class="content-read-marker" data-fraction="50"></div>

<img data-src="/images/jargon-free-functional-programming/Functional programming 1 20.png" alt="" />

This safe set of recipes is then thrown together with a bunch of inputs into a grinder called "real world", where nothing is safe and everything can happen (theoretically).

<img data-src="/images/jargon-free-functional-programming/Functional programming 1 17.png" alt="" />

In the grinder, the dish is being cooked from the inputs, following the recipes thrown to the grinder.
The result of this cooking might be another program itself, which can then be recycled by being thrown back into the grinder -
that would happen if a program enters the (infinite) loop, waiting for some inputs - it is essentially becomes a new program,
which also needs to be executed when all the requirements are met.

<img data-src="/images/jargon-free-functional-programming/Functional programming 1 18.png" alt="" />

In order to build one of those containers, one starts by creating a simple class.

<img data-src="/images/jargon-free-functional-programming/Functional programming 3 5.png" alt="" />

The class must hold a function without running it.

<img data-src="/images/jargon-free-functional-programming/Functional programming 3 4.png" alt="" />

There should be a way to link (chain) this container with some other function, creating a new safe container.

<img data-src="/images/jargon-free-functional-programming/Functional programming 3 3.png" alt="" />

And finally there should be a way to execute the function wrapped by this safe container.

<img data-src="/images/jargon-free-functional-programming/Functional programming 3 7.png" alt="" />

The details of each container' implementation is what makes them different. For few examples, the container which
makes an arbitrary function safe (in this case we assume it does some input-output stuff) could look like this:

```ts
class IO <A> {
    constructor(private f: () => A) {
    }

    andThen<B>(g: (_: A) => B) {
        return new IO(() => g(this.f()));
    }

    unsafeRun() {
        this.f();
    }
}
```

<div class="content-read-marker" data-fraction="75"></div>

A container which wraps a function returning a `Promise` might look similar (except all the dancing around `Promise` API):

```ts
class PromiseIO <A> {
    constructor(private readonly f: () => Promise<A>) {}

    andThen<B>(g: (_: A) => B) {
        return new PromiseIO<B>(() => this.unsafeRun().then(g));
    }

    unsafeRun() {
        return this.f();
    }
}
```

You can see the pattern - these classes all have very similar interface. Hence you can extract it:

```ts
interface Container <A> {
    andThen<B>(g: (_: A) => B): Container<B>;
}

class IO <A> implements Container <A> { ... }

class PromiseIO <A> implements Container <A> { ... }
```

Then you can create a container which wraps a function which might throw an exception (together with its error handler):

```ts
class Try <A> implements Container <A> {
    constructor(private readonly f: () => Container<A>, private readonly errorHandler: (_: unknown) => Container<A>) {}

    andThen<B>(g: (_: A) => B) {
        return new Try<B, E>(
            () => this.f().andThen(g),
            (e) => this.errorHandler(e).andThen(g)
        );
    }

    unsafeRun() {
        try {
            return this.f();
        } catch (e) {
            return this.errorHandler(e);
        }
    }
}
```

Then you can write programs using these containers:

```ts
const fetchSomeResponse = () => new PromiseIO(() => fetch('/').then(r => r.text()));

const processResponse = (response: string) =>
    new Try(
        () => new IO(() => console.log('OK', response)),
        (e) => new IO(() => console.error('ERR', e))
    );

const program = fetchSomeResponse()
    .andThen(processResponse)
    .andThen(t => t.unsafeRunTry())
    .andThen(io => (io as IO<void>).unsafeRun())
    .unsafeRun();
```

The <a href="/2022/08/24/jargon-free-functional-programming-part1.html">next article</a> contains a few examples and explains the above in bloody details,
using TypeScript and a (semi-)real-world problem and a step-by-step approach to arrivin at the above concepts.
It also introduces few more of those containers so you can actually go out and understand some (if not most)
of the real-world applications made with functional paradigm. Or even build your own!

<div class="content-read-marker" data-fraction="100"></div>
