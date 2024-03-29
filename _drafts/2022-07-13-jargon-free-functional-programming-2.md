---
layout: post
title: 'Jargon-free functional programming. Part 2'
date: '2022-08-24T00:24:00+00:00'
---

Tips:

having a proper functional programming in JS/TS world requires both automatic checks and strict
discipline so you do not create mutating code or changing the environment or relying on some magic
from the outer world in your functions. also, you can not really use standard library anymore -
welcome to the world of endless custom-made classes for immutability and monads.

----

> Hooks do not add side effects. They are kind of composing the components. component => props => state => hooks => return ...

> Yeah but, we implement any side effects in useEffect which is a hook

> let's forget the hooks for a moment, how would you want to deal with side effects? Even when we only had classes, side effects were happening in functions.
> They give us reusability which I believe trumps the fact that a function is not pure when we are developing.

> the only valid way to deal with side effects (the ones you can't avoid) in a true functional way is hide them in monads and let the runtime handle them ;)

> I heard about monads for the first time here. I think it would be safe to assume that this isn't something we do everyday. Also, if something can be done, doesn't mean it should be done.. right? :).

> well, monad is a concept coming from the world of Haskell (hence the misleading name). essentially, you wrap a _function which does have side-effects_ in a pure construct (in a _something_ that is guaranteed to **not** have any side-effects) and rely on all side-effects to magically happen at some time and your function relying on those side-effects to be executed in a callback-manner. by doing so you essentially admit your program will have side-effects but you defer them as much as possible and make them happen all at once, in a somewhat safe environment. well, at least you claim your code to be "side-effect-less". all this is good and nice (not entirely) in theory, but in the world of JavaScript you can't do any of that, since it _heavily_ relies on side-effects (AJAX calls, UI interaction, state mutation, etc.). whereas the whole idea of pure functional program is to be a chain of pure functions operating on immutable state (only intermediate temporary constants are present in the code). the best we can do is have an endless loop consisting of this chain of functions, yielding new DOM tree every CPU tick. but this ideal program concept would simply destroy any performance consideration, which is undesired. also, monads is what functional programmers have to deal with everyday in a more or less real applications ;)

> I went through some documentation from my end as well. Why don't you write a blog about it? :) You have explained it elegantly over here! Also, some things that I have understood here is that,:- 1. Monads kind of remove async behaviour by using callbacks, which push or defer those events or statements to the end of the event loop. Thus sync behaviour. 2. In real world programming, higher order functions are a type of Monads as per my understanding, since they take a value, a wrapper function and then return us a new value. Without taking async behaviour into context and thinking based on core logic. Am I right with these assumptions?

> yes, you are very close! few things to note:
>
> 1. the execution is theoretically moved to "the edge of the world", e.g. the end of the entire program execution. but that is only theory ;)
> 2. monads are slightly more than just HOFs - they have strictly defined operations on them, mostly to allow for "safe" combinations of monads. so a monad is essentially a class which wraps a value with certain context/rules and has two methods:
> a. `bind(T): Monad<T>`, a constructor, wrapping a value in a monad
> b. `map(Monad<T>, Function<T, Monad<U>>): Monad<U>` - a function that takes a monad wrapping a value of type T, "safely unpacks" the value, applies the function that transforms the unpacked value and returns a new monad, wrapping the new value
> 3. the closest you can get to monads in JS world is Promises (aka Future in some other languages and frameworks) - all those `then` calls are exactly the `map` method of a monad - they take a transformer function (passed to `then`) and a value wrapped in a monad > (Promise) and return a new monad (new Promise) wrapping a new value
>
> also, check out PureScript (Haskell) and Elm (simplified Haskell) 😉
>
> i think the blog might actually be a very good idea! 👍

## What are side-effects?

Very roughly, any function that returns `void` or `Unit` is a side-effect - most likely it relies on a certain state of the system and it changes the state of the system. That fact makes that function very hard to reason about (test, refactor the code using the function, expect defined behaviour of the program using that function).

Let's introduce few definitions first.

Talking about functions in functional programming, here are few things to consider:

1. a function is **pure** when it always produces the same output for the same input and it has all possible outputs defined (no undefined behaviour)
2. we can always inline a function call or extract it out (because it will always give the same result); we can always substitute a variable for the expression it is bound to or introduce a new variable to extract the common sub-expressions; this is called **referential transparency** of an expression

Programs in functional programming are nothing but expressions (functions or combinations of functions). Running a function (or a functional program, for that matter) means we are evaluating an expression.

Every expression (in general) is either referentially transparent or it is a side-effect.

Let's have few examples (by Rob Norris): try guessing if the following programs are same

```scala
val a = 42

(a, a)
```

and

```scala
(42, 42)
```

They apparently are.

How about this?

```scala
val a = println("hello")

(a, a)
```

and

```scala
(println("hello"), println("hello"))
```

They are not - one prints `hello` once, while the other prints it twice.
To make this last example more vivid, try replacing `print("hello")` with something like `UsersTable.insert(name = "hello")`

A slightly simpler example:

```scala
val a = iterator.next()

(a, a)
```

and

```scala
(iterator.next(), iterator.next())
```

These are not the same - the first one will advance iterator once, while the second one will advance iterator twice.

Those programs which are not the same after we have inlined some expressions are not referentially transparent and hence are said to have side effects.

Functional programming sounds reasonably simple with the information presented so far.

Yet, in real world, we have few issues with functional programming:

* what about functions that not always have to return a value? like returning a `null` or `void`? (partiality)
* how to handle exceptions?
* what if a function can return different outputs from the same inputs? like `Math.random()` or `DateTime.now()`? (nondeterminism)
* what about dependency injection and inversion of control?
* how about logging?
* how about mutable state?

The way we deal with those and claim back the features which we have lost by using functional programming?

We use effects. They are different from side-effects - effects are good, side-effects are the reason for bugs.

## Some basic effects

Consider these few effects which solve few aspects of the expressiveness we lose with functional programming as opposed to imperative programming.

* `Option` - solves the issue of partiality, when a function _always_ returns a value (of type `Option`)
* `Either` - the easiest way to handle exceptions and errors by returning either `Left(error)` or `Right(result)`
* `Reader` - allows to retrieve data from some _outer world_ (from a program' perspective); this solves the dependency injection issue
* `Writer` - allows to pass some data to the _outer world_; this solves issues like logging, for instance
* `State` - takes a (_previous_ or _initial_) state, computes the result and a _new_ state; this solves the mutable state issue
* `IO` - produces a value, fails or never terminates

### Reader

Reader allows user to describe a computation of a value which is not _yet_ present.\
Essentially, you define a program of _some_ input parameter (which is not present just _yet_) and when you execute this program, you have to provide this value - then will you be able to get the result.

Example:

```scala
case class Reader[E, A](run: E => A)

type Host = String

def path(s: String): Reader[Host, String] =
    Reader { host => s"https://$host/$s" }

val p = path("foo/bar")
p.run("google.com") // https://google.com/foo/bar
```

### Writer

_TODO_

### State

```scala
case class State[S, A](run: S => (A, S))

type Counter = Int

def greet(name: String): State[Counter, String] =
    State { count => (s"Hello, $name, you are visitor #$count", count + 1) }

val x = greet("Bob")

x.run(1) // (Hello, Bob, you are visitor #1", 2)
x.run(42) // ("Hello, Bob, you are visitor #42", 43)
```

## What is effect and how is it different to side-effect?

The effects presented above have one thing in common: they all compute some "answer" (or "result", or "output") when you evaluate them, but alongside the "answer" they do a little bit extra. This is what we call an "effect".

If you look closely, all these effects have same _shape_, `F[A]`:

```scala
type F[A] = Option[A]
type F[A] = Either[E, A]
type F[A] = Reader[E, A]
type F[A] = Writer[W, A]
type F[A] = State[S, A]
```

An _effect_ in this case is whatever distinguishes `F[A]` from `A`. Think of it as "program of `F` that computes a value of type `A`".

Issue is: these effects do not compose well - you can't easily compose a program of `Option[Char]` with a program of `Option[Int]`:

```scala
// get 10th character of an input string
val char10: String => Option[Char] =
    s => s.lift(10)

val letter: Char => Option[Int] =
    c => if (c.isLetter) Some(c.toInt) else None

char10 andThen letter
// error: type mismatch
```

## Composing effects

You know the famous function composition diagram, right?

```
A (id[A]: A => A) ---- f: A => B ---> B
 \                                    |
  \                                   |
   \                                  |
  (f andThen g): A => C            g: B => C
     \                                |
      \                               |
       \                              v
           C
```

This diageram shows how function composition looks like: given a function (which has an argument) of type `A` (and returns a value) to type `B`, `f: A => B`, and another function of `B` to `C`, `g: B => C`, a composition of those two functions, expressed in Scala as `(f andThen g)`, will be a function
of `A` to `C`: `(f andThen g): A => C`.

With effects, this becomes invalid: a function of `A` to `F[B]` can not be composed with a function of `B` to `F[C]` and produce a function of `A` to `F[C]`.

Let us pretend for a second that there _is_ a way to compose those effects so that the diagram holds: instead of `andThen` we will use the operation `>=>`. It is also known as Kleisli operator. We will also call a function `id` (identity) which used to be `id[A]: A => A` will become `pure[A]: A => F[A]`.

This is a bit funky looking in Scala (a much more clean implementation is, surprisingly, Haskell):

```scala
trait Fishy[F[_]] {
    def pure[A](a: A): F[A]

    // the fishy operator, >=>
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}

// define fishy operator, >=>, as an infix operator using a syntax class
// we are wrapping a function f: A => B and adding a method of B => F[C] given the evidence that there is an instance of Fishy[F], we delegate to that instance and get a function A => F[C]
implicit class FishyFunctionOps[F[_], A, B](f: A => B) {
    def >=>[C](g: B => F[C])(implicit ev: Fishy[F]): A => F[C] =
        a => ev.flatMap(f(a))(g)
}
```

But that needs an instance of this `Fishy` class for each of the effect classes that we want to use:

```scala
implicit val FishyOption: Fishy[Option] =
    new Fishy[Option] {
        def pure[A](a: A) = Some(a)

        def flatMap[A, B](fa: Option[A])(f: A => Option[B]) = fa.flatMap(f)
    }
```

Now the functions `char10` and `letter` we defined before can be composed with `>=>` instead of `andThen`:

```scala
char10 >=> letter
// String => Option[Int]
```

This `Fishy` trait that we have developed is also commonly known as a `Monad`.

## Simple examples

### Reader

Consider a `Reader` effect as a solution for dependency injection.

```scala
type Host = String

def path(p: String): Reader[Host, String] =
    Reader(host => s"https://$host/$p")

val hostLength: Reader[Host, Int] =
    Reader(host => host.length)

val program = for {
    a <- path("foo/bar")
    b <- hostLength
} yield s"Path is $a and its length is $b"
```

Now this program does not return a result - this is just a few functions, composed together.
It will only produce a result once you _run_ it:

```scala
program.run("google.com")
// "Path is https://google.com/foo/bar and its length is 11"
```

Essentially, the string you pass to the `program.run()` function is the "environment", which will be available to all the parts of the program.

### IO

In Cats Effects, an `IO` effect describes _an intention to run a side-effect_. It does not actually run it, hence it is composable and gives referentially transparent expressions once composed.

Consider the following program (example from Gabriel Volpe talk):

```scala
val program: IO[Unit] =
    for {
        _ <- IO(println("Enter your name:"))
        name <- IO(readLine)
        _ <- IO(println(s"Hello, $name"))
    } yield ()
```

It is referentially transparent and is just a composition of pure effects (no side-effects).
This misleads many padavans of functional programming to think that by wrapping all the logic in `IO` they could get functional programs
with referential transparency (hence safe to run, refactor and test).

Ah, yes, testing - think about how would you test such a program (without changing it). I can't think of a better way other than running it (potentially in some sort of sandbox environment) and checking the output (STDOUT).

Yet there is a better way to utilize the `IO` effect in this example, by using something called tagless final.
The idea is to provide that sandbox environment at compile time by extracting the `IO` part into its own effect.

```scala
trait MyEnvironment[F[_]] {
    def println(str: String): F[Unit]

    def readLine: F[String]
}

def program[F[_]: Monad](implicit E: MyEnvironment[F]): F[Unit] =
    for {
        _ <- C.println("Enter your name:")
        name <- C.readLine
        _ <- C.println(s"Hello, $name")
    } yield ()
```

This program abstracts over the instance of `MyEnvironment`, which is a monad (so that you can utilize `flatMap`, which is the underline of `for` expression) and provides two methods - `println` and `readLine`.

The `MyEnvironment`, which abstracts over _some_ type `F[_]` (a wrapper around some other type), is called "algebra". The instance (which we are about to implement) is called an "interpreter".

For instance, an instance that utilizes the `Sync` monad from Cats Effects might look like this:

```scala
class StdioEnvironment[F[_]: Sync] extends MyEnvironment[F] {
    def println(str: String) = Sync[F].delay(println(str))

    def readLine = Sync[F].delay(readLine)
}
```

Alternatively, for testing purposes, you can have an interpreter which keeps track of the results in a list:

```scala
class TestEnvironment[F[_]: Applicative](state: Ref[F, List[String]]) extends MyEnvironment[F] {
    def println(str: String): F[Unit] = state.update(_ :+ str)

    def readLine: F[String] = "testName".pure[F]
}
```

Then, a test for the program above would look something like this:

```scala
test("program") {
    val spec =
        for {
            state <- Ref.of[IO, List[String]](List.empty[String])
            algebra(e: MyEnvironment[IO]) = new TestEnvironment[IO](state)
            _ <- program[IO]
            log <- state.get
            testResult <- IO { assert(log == List("Enter your name:", "Hello, testName")) }
        } yield testResult

    spec.unsafeToFuture()
}
```

In this test, we instantiate the `MyEnvironment[IO]` implicit (required by `program`) with `TestEnvironment`, which, in turn, is initialized with a state (`Ref` in Cats Effects) holding an empty list of strings.

We then describe a test as a sequence of executing the program and returning an `IO` effect of the assertion comparing the output of `program` with a certain expected value (`List("Enter your name:", "Hello, testName")`).

The issue with tagless final approach is more obvious with more complex programs when a program requires multiple behaviours to be implemented in order to perform computation.

Think of a user service that requires database, caching, logging and some other behaviours to be implemented before being able to create or update a user entity.

This issue can't really be solved by a `Reader` monad, as one might think, since the program would need to constantly perform computations of that reader monad and, most importantly, return the reader monad upon each operation.

One might think `ReaderT` (monad transformer) comes to the rescue. But monad transformers are known to be heavy and not "stack-safe" - that is, some monads in Scala do not really implement tail recursion but rely on non-tail recursion instead. That effectively means each recursive call is being pushed onto stack so that deep transformations (or long transformations, like `sum(List(20000)(1))`) can cause `StackOverflowException`, surprisingly.

Hence there is a need for some new monad, which would remind of `Either` where `Left` would be a failure (`IO`-like behaviour), `Right` would be a result of a successful computation and there should be one more param, the `Reader` monad.

In Cats Effects there is no `Reader` or `ReaderT` monad. Instead, it utilizes the [Kleisli](https://typelevel.org/cats/datatypes/kleisli.html#configuration) monad.

----

Resources:

* https://blog.tmorris.net/posts/monads-do-not-compose/
* https://mmhaskell.com/monads/transformers
* https://github.com/JordanMartinez/pure-conf-talk/blob/master/slides/Cheatsheet.md
* https://gcanti.github.io/fp-ts/learning-resources/
* https://dev.to/gcanti/functional-design-tagless-final-332k
* https://stackoverflow.com/questions/6647852/haskell-actual-io-monad-implementation-in-different-language
* https://stackoverflow.com/questions/73032939/typescript-generic-type-constraints-cant-deduct-type/73074420#73074420
* https://github.com/louthy/language-ext
