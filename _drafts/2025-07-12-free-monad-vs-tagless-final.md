---
layout: post
title: 'Free monad vs tagless final'
tags: [monads, rest, javascript, java, fp, backend, programming, programming-paradigms, functional-programming, api]
---

Much like there are design patterns in OOP world, there are design patterns in functional programming world as well.
This article covers two rather popular ones, namely the Free monad and Tagless Final.

Before diving into the specifics of each approach, it might be worth figuring out when and why should you even consider using them.

In functional programming, the application is sort of a recipe on how to perform calculations when all of the ingridients (requirements) are present.
Think of a program in FP land as a function of multiple arguments, such as file on a file system, a user input for a specific operation, a network call response, etc.
For example, a program that gets a currency conversion rate from a Forex API and spits out a HTML might look something like this:

```scala
def forexExchange(from: Currency, to: Currency) =
  makeHttpCall(s"http://forex.com/convert/${from}/${to}")
    .andThen(response => parseForexJson(response).andThen(json => getExchangeRate(json).andThen(rate => makeHtml(rate, from, to))))
```

A more "canonical" way to represent the same program is by using a `do` notation, but we will preserve this for a bit later.

<info>
In a general purpose language such as JavaScript or Java, this would make perfect sense. But there is a little trick hidden in this code, which _might_ be a source of many errors: `makeHttpCall` function only takes one parameter - a URL, and it magically knows how to make HTTP requests and handle errors - it does not only return a value based on its arguments - for instance, if the server is down, it would throw an error. Effectively this function has a side-effect of _sending a HTTP request_ - it is doing something outside of the scope of the function body.
</info>

In a *pure functional* style, `makeHttpCall` would be also a function of _something_ that can make HTTP calls and the request, not just a URL:

```scala
def makeHttpCall(H: HttpClient, url: String) = ???
```

So the program would have an additional requirement - something that can make HTTP calls.
This could be the context (the monad) that the application is working within, returned by some helper function (so that the runtime would know how to make the HTTP call eventually):

```scala
def httpGet(url: String): IO[Either[HttpError, HttpResponse]] = ???

def makeHttpCall(url: String): IO[Either[HttpError, HttpResponse]] = httpGet(url)
```

Note how `IO` type in this made up example saves the hassle of having to specify the `HttpClient`.

Or it could be expressed as an implicit object that can make HTTP requests:

```scala
trait HttpClient[F[_]] {
  def get(url: String): F[Either[HttpError, HttpResponse]]
}

def makeHttpCall(url: String)(implicit H: HttpClient[IO]): IO[Either[HttpError, HttpResponse]] = H.makeHttpCall(url)

// ...

def run(args: List[String]): IO[ExitCode] = {
  implicit val liveHttpClient: HttpClient = ???

  makeHttpRequest("...")
}
```

Since this function call is the first in the application, the entire application would be a function that works in this context (`IO`).
The application would then need to be written in a way so that all of its steps are combined using the proper combination operations (`map` and `flatMap`) to preserve the type integrity:

```scala
def forexExchange(from: Currency, to: Currency) =
  for {
    response <- makeHttpCall(s"http://forex.com/convert/$from/$to")
    json     <- parseForexJson(response)
    rate     <- getExchangeRate(json)
    html     = makeHtml(rate, from, to)
  } yield html

def makeHttpCall(url: String)(implicit H: HttpClient): IO[Either[HttpError, HttpResponse]] = H.get(url)

def parseForexJson(response: HttpResponse): IO[Either[AppError, Json]] = ???

def getExchangeRate(json: Json): IO[Either[AppError, BigDecimal]] = ???

def makeHtml(rate: BigDecimal, from: Currency, to: Currency): String = ???
```

Both Free monad and Tagless Final are two approaches in functional programming to structuring programs to make combining different steps of the application logic easier.

It is totally possible to not use either approach, like the example above, which merely uses `map` and `flatMap` (actually, `andThen`, for sake of simplicity):

```scala
def forexExchange(from: Currency, to: Currency) =
  makeHttpCall(s"http://forex.com/convert/${from}/${to}")
    .andThen(response => parseForexJson(response).andThen(json => getExchangeRate(json).andThen(rate => makeHtml(rate, from, to))))
```

But as application gets more and more complex, it becomes progressively harder to structure the code and combine different parts of application.

For instance, if the above application required some sort of caching and a scheduler (like CRON), `IO` might not be a sufficient context anymore (again, simplifying for the sake of example):

```scala
trait Cache[F[_], K, V] {
  def put(key: K, value: V): F[Unit]

  def get(key: K): F[Option[V]]

  def invalidate(): F[Unit]
}

def forexExchange(from: Currency, to: Currency)(implicit C: Cache[IO, (Currency, Currency), BigDecimal]) = {
  C.get((from, to)) match {
    case Some(rate) => IO.pure(makeHtml(rate, from, to))
    case None =>
      makeHttpCall(s"http://forex.com/convert/${from}/${to}")
        .andThen(response => parseForexJson(response).andThen(json => getExchangeRate(json).andThenThru(rate => C.put((from, to), rate)).andThen(rate => makeHtml(rate, from, to))))
  }
}
```

The only change required here is to pass the implicit values to the "parent" (or top-level) function (the one that combines the program steps):

```scala
def forexExchange(from: Currency, to: Currency)(implicit C: Cache[IO, (Currency, Currency), BigDecimal], H: HttpClient) = {
  C.get((from, to)) match {
    case Some(rate) => IO.pure(makeHtml(rate, from, to))
    case None =>
      H.get(s"http://forex.com/convert/${from}/${to}")
        .andThen(response => parseForexJson(response).andThen(json => getExchangeRate(json).andThenThru(rate => C.put((from, to), rate)).andThen(rate => makeHtml(rate, from, to))))
  }
}
```

The program already becomes a bit of a mess, but still somewhat manageable. Assume there is a need to periodically invalidate the cache:

```scala
trait Scheduler[F[_]] {
  def schedule(delay: FiniteDuration, task: F[Unit]): F[Unit]
}

def forexExchange(from: Currency, to: Currency)(implicit C: Cache[IO, (Currency, Currency), BigDecimal], S: Scheduler[IO]) = {
  C.get((from, to)) match {
    case Some(rate) => IO.pure(makeHtml(rate, from, to))
    case None =>
      H.get(s"http://forex.com/convert/${from}/${to}")
        .andThen(response => parseForexJson(response)
          .andThen(json => getExchangeRate(json)
            .andThenThru(rate => C.put((from, to), rate))
            .andThenThru(rate => S.schedule(1.hour, C.invalidateAll()))
            .andThen(rate => makeHtml(rate, from, to))
          )
        )
  }
}
```

It is quite hard to read a code like that, so usually we would use the `do` notation (`for` in Scala) to simplify it:

```scala
def forexExchange(from: Currency, to: Currency)(implicit C: Cache[IO, (Currency, Currency), BigDecimal], H: HttpClient[IO], S: Scheduler[IO]) =
  for {
    cached <- C.get((from, to))
    rate <- cached match {
      case Some(r) => Monad[F].pure(r)
      case None    =>
        for {
          response <- H.get(s"http://forex.com/convert/${from.code}/${to.code}")
          j        <- parseForexJson(response.body)
          r        <- getExchangeRate(j)
          _        <- C.put((from, to), r)
          _        <- S.schedule(1.hour, C.invalidateAll())
        } yield r
    }
    html <- makeHtml(rate, from, to)
  } yield html
```

<info>
The above code defines a program `forexExchange` which runs in a context `IO`.
It is highly specific - `IO` is generally reserved for situations where literally *any* side-effect could happen in a program.
Usually the programs are generalized over a template parameter `F[_]`:

```scala
def forexExchange[F[_]](from: Currency, to: Currency)(implicit C: Cache[F, (Currency, Currency), BigDecimal], H: HttpClient[F], S: Scheduler[F]) = ???
```

Most of the time the type `F` must meet certaing requirements such as support apply / construct function `()`, `map` or both `map` and `flatMap` functions (the last two requirements make `F` available to be used with `do` notation). This is done by either specifying these requirements in the template parameter itself:

```scala
def forexExchange[F[_]: Monad](from: Currency, to: Currency)(implicit C: Cache[F, (Currency, Currency), BigDecimal], H: HttpClient[F], S: Scheduler[F]) = ???
```

Or in the implicits:

```scala
def forexExchange[F[_]](from: Currency, to: Currency)(implicit C: Cache[F, (Currency, Currency), BigDecimal], H: HttpClient[F], S: Scheduler[F], M: Monad[F]) = ???
```

After specifying this template parameter, there must be a suitable implementation for each of the functions used in the `forexExchange` provided at runtime and each of them must be compatible with the specified requirements of type `F` (such as it must implement `Monad`, for example).
</info>

Since there are a few implicit parameters of the `forexExchange` function, there must be a context defined at runtime, which must have the capabilities of a `Cache`, an `HttpClient` and `Schedule`.

Assuming somehow the types of each call (`HttpClient::get`, `parseForexJson`, `getExchangeRate`, `Cache::put`, `Scheduler::schedule`) can compose, we need to provide an implementation of each capability.

The composition of the effects (or return types of each of the calls listed above) means that we can chain the calls to each of the functions - without the `do` notation, the program would look just like a chain of `flatMap` calls:

```scala
def forexExchange(from: Currency, to: Currency)(implicit C, H, S) =
  H.get("...")
    .flatMap(httpResponse => parseForexJson(httpResponse))
    .flatMap(json => getExchangeRate(json))
    .flatMap(rate => C.put((from, to), rate).flatTap(() => S.schedule(1.hour, () => C.invalidate())))
    .map(rate => makeHtml(rate, from, to))
```

See how each next step of a program is a `flatMap`, `flatTap` (run the operation but return the input value) or `map`. Meaning whatever each operation returns must be composable with `flatMap` or `map` or `flatTap`.

For example, assume the functions are declared as following:

```scala
def makeHttpCall(url: String): IO[Either[HttpError, String]] = ???

def parseForexJson(json: String): Either[JsonParseError, Json] = ???

def getExchangeRate(json: Json): Option[BigDecimal]
```

With these declarations, the program won't compose.

The defined `flatMap` on the `IO[A]` type (returned by `makeHttpCall`) would look like this: `IO[A].flatMap((a: A) -> IO[B]) -> IO[B]`.
The alternative is `map`: `IO[A].map((a: A) -> B): IO[B]`.
But neither `flatMap` nor `map` would work for the types `makeHttpCall` and `parseForexJson` return:

```scala
IO[Either[A, B]].flatMap((a: Either[A, B]) -> Option[C]): IO[???] // this won't work: Option[C] should have been IO[C]

IO[Either[A, B]].map((a: Either[A, B]) -> Option[C]): IO[Option[C]] // this would only work if parseForexJson uses Either[A, B] as an argument
```

This was a long introduction, but what is the problem either Tagless Final or Free Monad are supposed to solve?
Mostly for swappable implementations - think mocking logic for testing or having multiple implementations for different deployment environments.
But also for code organization and abstraction - there could be multiple implementations of the functions and moreover multiple supported effects (`IO`, `Future`, `ZIO` etc.). If you ever wanted to swap one for another - instead of rewriting the original code you really only implement the smallest bits - even just type signatures.

I have actually stumbled upon this problem myself while implementing this very program (forex exchange), when the API was rate limiting me or unavailable. Back in the day, I created a mock server in Ruby - effectively making an integration testing suite. Looking at it retrospectively, I should have just created another implementation.

## Tagless Final

There are two concepts which describe Tagless Final - algebras and interpreters (yes, functional programming is notorious for coming up with theorethically heavy mathematical names, which are not very helpful for us common people).

We already defined algebras:

```scala
trait HttpClient[F[_]] {
  def get(url: String): F[Either[HttpError, HttpResponse]]
}

trait Cache[F[_], K, V] {
  def put(key: K, value: V): F[Unit]

  def get(key: K): F[Option[V]]

  def invalidate(): F[Unit]
}

trait Scheduler[F[_]] {
  def schedule(delay: FiniteDuration, task: F[Unit]): F[Unit]
}
```

These are effectively just interfaces, the declarations of certain capabilities.

Then there are interpreters:

```scala
def mockHttpClient: HttpClient[IO] = new HttpClient[IO] {
  def get(url: String): IO[HttpResponse] = IO.pure(HttpResponse("""{"rate": 1.23}""", 200))
}

def ioCache(ref: Ref[IO, Map[(Currency, Currency), BigDecimal]]): Cache[IO, (Currency, Currency), BigDecimal] = new Cache[IO, (Currency, Currency), BigDecimal] {
  def put(key: (Currency, Currency), value: BigDecimal): IO[Unit] =
    ref.update(_ + (key -> value))

  def get(key: (Currency, Currency)): IO[Option[BigDecimal]] =
    ref.get.map(_.get(key))

  def invalidate(key: (Currency, Currency)): IO[Unit] =
    ref.update(_ - key)
}

def ioScheduler: Scheduler[IO] = new Scheduler[IO] {
  def schedule(delay: FiniteDuration, task: IO[Unit]): IO[Unit] =
    IO.sleep(delay).flatMap(_ => task)
}
```

Remember that we must provide these implementations to the program in order for it to work:

```scala
def run = {
  for {
    cacheRef <- Ref.of[IO, Map[(Currency, Currency), BigDecimal]](Map.empty)

    implicit val httpClient: HttpClient[IO] = mockHttpClient
    implicit val cache: Cache[IO, (Currency, Currency), BigDecimal] = ioCache(cacheRef)
    implicit val scheduler: Scheduler[IO] = ioScheduler

    result <- forexExchange[IO](Currency("USD"), Currency("EUR"))
  } yield result
}
```

If I were to run this program, it would most likely work. If I want to actually send requests to the Forex Exchange API, I would need to implement a `HttpClient[IO]` which would actually send requests and then replace this one single line:

```diff
- implicit val httpClient: HttpClient[IO] = mockHttpClient
+ implicit val httpClient: HttpClient[IO] = realHttpClient
```

If I were ever to replace Cats Effects (or, roughly put, `IO` monad) with ZIO, it would be as simple as implementing new interpreters:

```scala
type AppContext[A] = ZIO[Any, Throwable, A]

def zioHttpClient: HttpClient[AppContext] = new HttpClient[AppContext] {
  def get(url: String): AppContext[HttpResponse] =
    ZIO.succeed(HttpResponse("""{"rate": 1.23}""", 200))
}

def zioCache(ref: zio.Ref[Map[(Currency, Currency), BigDecimal]]): Cache[AppContext, (Currency, Currency), BigDecimal] =
  new Cache[AppContext, (Currency, Currency), BigDecimal] {
    def put(key: (Currency, Currency), value: BigDecimal): AppContext[Unit] =
      ref.update(_ + (key -> value))

    def get(key: (Currency, Currency)): AppContext[Option[BigDecimal]] =
      ref.get.map(_.get(key))

    def invalidate(key: (Currency, Currency)): AppContext[Unit] =
      ref.update(_ - key)
  }

def zioScheduler: Scheduler[AppContext] = new Scheduler[AppContext] {
  def scheduleAfter(delay: FiniteDuration)(task: AppContext[Unit]): AppContext[Unit] =
    ZIO.sleep(delay.toMillis.millis).flatMap(_ => task)
}
```

And in the runner program (`run`):

```diff
- cacheRef <- Ref.of[IO, Map[(Currency, Currency), BigDecimal]](Map.empty)
-
- implicit val httpClient: HttpClient[IO] = mockHttpClient
- implicit val cache: Cache[IO, (Currency, Currency), BigDecimal] = ioCache(cacheRef)
- implicit val scheduler: Scheduler[IO] = ioScheduler
-
- result <- forexExchange[IO](Currency("USD"), Currency("EUR"))
+ cacheRef <- zio.Ref.make(Map.empty[(Currency, Currency), BigDecimal])
+
+ implicit val httpClient: HttpClient[AppContext] = zioHttpClient
+ implicit val cache: Cache[AppContext, (Currency, Currency), BigDecimal] = zioCache(cacheRef)
+ implicit val scheduler: Scheduler[AppContext] = zioScheduler
+
+ result <- forexExchange[AppContext](Currency("USD"), Currency("EUR"))
```

And if you have pre-defined type alias like `AppContext` for Cats' `IO` before:

```scala
type AppContext[F] = IO[F]
```

you would only really have to replace the *summoners* (another beautiful word from the land of functional programming - effectively just a constructor / initializer / factory invocation):

```diff
- cacheRef <- Ref.of[AppContext, Map[(Currency, Currency), BigDecimal]](Map.empty)
-
- implicit val httpClient: HttpClient[AppContext] = mockHttpClient
- implicit val cache: Cache[AppContext, (Currency, Currency), BigDecimal] = ioCache(cacheRef)
- implicit val scheduler: Scheduler[AppContext] = ioScheduler
-
- result <- forexExchange[AppContext](Currency("USD"), Currency("EUR"))
+ cacheRef <- zio.Ref.make(Map.empty[(Currency, Currency), BigDecimal])
+
+ implicit val httpClient: HttpClient[AppContext] = zioHttpClient
+ implicit val cache: Cache[AppContext, (Currency, Currency), BigDecimal] = zioCache(cacheRef)
+ implicit val scheduler: Scheduler[AppContext] = zioScheduler
+
+ result <- forexExchange[AppContext](Currency("USD"), Currency("EUR"))
```

## Free Monad

Whereas Tagless Final is easy to relate to if you know OOP - it is very similar to abstraction and interface implementation,
Free Monad introduces a completely different approach to writing programs - using Algebraic Data Types (ADTs).
And it does rely on the definition of `Free` monad, whereas Tagless Final does not really require anything but the basic language features - it is literally just a glorified abstraction mechanism.

You first define all the possible operations that the program could perform. The program is then defined as a chain (or rather layers) of those operations. Lastly, you define *parsers* which unwrap the operations and effectively interpret the program.

Effectively, the program becomes a glorified state machine. Or a [Redux](https://redux.js.org/) application.

Check out the example of the above program converted to Free Monad step-by-step.

First we define the operations the program can perform:

```scala
sealed trait ForexOp[A]

case class HttpGet(url: String) extends ForexOp[HttpResponse]

case class ParseJson(json: String) extends ForexOp[Json]

case class GetExchangeRate(json: Json) extends ForexOp[BigDecimal]

case class CacheGet(key: (Currency, Currency)) extends ForexOp[Option[BigDecimal]]
case class CachePut(key: (Currency, Currency), value: BigDecimal) extends ForexOp[Unit]
case class CacheInvalidate() extends ForexOp[Unit]

case class Schedule(delay: FiniteDuration, task: ForexProgram[Unit]) extends ForexOp[Unit]

case class RenderHtml(rate: BigDecimal, from: Currency, to: Currency) extends ForexOp[String]
```

Note how all the operations are instances of `ForexOp`, which is **not** an instance of `Monad`, meaning we can **not** chain or combine them yet. To make them chainable (or combineable), we would use an existing tool, the `Free` monad:

```scala
type ForexProgram[A] = Free[ForexOp, A]

object dsl {
  def httpGet(url: String): ForexProgram[HttpResponse] =
    Free.liftF(HttpGet(url))

  def parseJson(response: HttpResponse): ForexProgram[Json] =
    Free.liftF(ParseJson(response))

  def getExchangeRate(json: Json): ForexProgram[BigDecimal] =
    Free.liftF(GetExchangeRate(json))

  def cacheGet(key: (Currency, Currency)): ForexProgram[Option[BigDecimal]] =
    Free.liftF(CacheGet(key))

  def cachePut(key: (Currency, Currency), value: BigDecimal): ForexProgram[Unit] =
    Free.liftF(CachePut(key, value))

  def cacheInvalidate(): ForexProgram[Unit] =
    Free.liftF(CacheInvalidate())

  def schedule(delay: FiniteDuration)(task: ForexProgram[Unit]): ForexProgram[Unit] =
    Free.liftF(Schedule(delay, task))

  def renderHtml(rate: BigDecimal, from: Currency, to: Currency): ForexProgram[String] =
    Free.liftF(RenderHtml(rate, from, to))

  def pure[A](a: A): ForexProgram[A] =
    Free.pure(a)
}
```

These are the constructors for each operation. But instead of creating objects of each operation, they wrap ("lift") the operation in the `Free` monad.

There is also a `pure` constructor, which creates a program of a static value. Whilst each operation wrapped in a `Free` monad is an intermediate state of a program, this is the final state of a program.

Let's now define the program or a chain of these intermediate states:

```scala
def forexExchange(from: Currency, to: Currency): ForexProgram[String] =
  for {
    cached <- cacheGet((from, to))
    rate <- cached match {
      case Some(r) => pure(r)
      case None    =>
        for {
          response <- httpGet(s"http://forex.com/convert/${from.code}/${to.code}")
          j        <- parseJson(response)
          r        <- getExchangeRate(j)
          _        <- cachePut((from, to), r)
          _        <- schedule(1.hour, cacheInvalidate())
        } yield r
    }
    html <- renderHtml(rate, from, to)
  } yield html
```

Lastly, we define an interpreter. Its job is to parse the program by unwrapping each of the intermediate states and executing a corresponding logic. It does that until the program becomes a "pure" value.

```scala
def interpreter(cacheRef: Ref[IO, Map[CurrencyPair, BigDecimal]]): ForexOp ~> IO =
  new (ForexOp ~> IO) {
    def apply[A](op: ForexOp[A]): IO[A] = op match {
      case HttpGet(url) =>
        IO.pure(HttpResponse("""{"rate": 1.23}""", 200))

      case ParseJson(json) =>
        IO.pure(BigDecimal("1.23"))

      case ParseJson(json) =>
        IO.pure(BigDecimal("1.23"))

      case CacheGet(key) =>
        cacheRef.get.map(_.get(key))

      case CachePut(key, value) =>
        cacheRef.update(_ + (key -> value))

      case CacheInvalidate(key) =>
        cacheRef.update(_ - key)

      case Schedule(delay, task) =>
        IO.sleep(delay).flatMap(_ => task.foldMap(this))

      case RenderHtml(rate, from, to) =>
        IO.pure(s"<html><body>1 ${from.code} = $rate ${to.code}</body></html>")
    }
  }
```

To put these units in terms of Redux, case class `ForexOp` defines types of actions, all the functions in the `dsl` object are action creators and `interpreter` is a reducer.

Since all the states are defined as instances of `Free` monad, a valid program would be either a single "pure" value (wrapped in `Free`, of course) or another program, wrapped in `Free`. The task of `interpreter` is to reduce the program down to a "pure" value by unwrapping each of the `Free` layers and performing a corresponding operation. It does so by using `foldMap` function, just like handling `Schedule` action:

```scala
case Schedule(delay, task) =>
  IO.sleep(delay).flatMap(_ => task.foldMap(this))
```

Running the program is very similar:

```scala
def run =
  for {
    cacheRef <- Ref.of[IO, Map[CurrencyPair, BigDecimal]](Map.empty)
    i = interpreter(cacheRef)
    result <- forexExchange(Currency("USD"), Currency("EUR")).foldMap(i)
  } yield result
```
