---
layout: post
title: 'Free monad vs tagless final'
---

Much like there are design patterns in OOP world, there are design patterns in functional programming world as well.
This article covers two rather popular ones, namely the Free monad and Tagless Final.

Before diving into the specifics of each approach, it might be worth figuring out when and why should you even consider using them.

In functional programming, the application is sort of a recipe on how to perform calculations when all of the ingridients (requirements) are present.
Think of a program in FP land as a function of multiple arguments, such as file on a file system, a user input for a specific operation, a network call response, etc.
For example, a program that gets a currency conversion rate from a Forex API and spits out a HTML might look something like this:

```scala
def forexExchange(from: Currency, to: Currency) =
  makeHttpCall("http://forex.com/convert/${from}/${to}")
    .andThen(response => parseForexJson(response).andThen(json => getExchangeRate(json).andThen(rate => makeHtml(rate, from, to))))
```

In a pure functional style, `makeHttpCall` would be a function of _something_ that can make HTTP calls and the request (or just a URL in this example):

```scala
def makeHttpCall(H: HttpClient, url: String) = ???
```

So the program would have an additional requirement - something that can make HTTP calls.
This could be the context (the monad) that the application is working within, returned by some helper function (so that the runtime would know how to make the HTTP call eventually):

```scala
def httpGet(url: String): IO[Either[HttpError, HttpResponse]] = ???

def makeHttpCall(url: String): IO[Either[HttpError, HttpResponse]] = httpGet(url)
```

Or it could be expressed as an implicit object that can make HTTP requests:

```scala
trait HttpClient {
  def get(url: String): IO[Either[HttpError, HttpResponse]]
}

def makeHttpCall(url: String)(implicit H: HttpClient): IO[Either[HttpError, HttpResponse]] = H.makeHttpCall(url)

// ...

def run(args: List[String]): IO[ExitCode] = {
  implicit val liveHttpClient: HttpClient = ???

  makeHttpRequest("...")
}
```

Since this function call is the first in the application, the entire application would be a function that works in this context (`IO`).
The application would then need to be written in a way so that all of its steps are combined using the proper combination operations (`map` and `flatMap`).

Both Free monad and Tagless Final are two approaches in functional programming to structuring programs to make combining different steps of the application logic easier.
It is totally possible to not use either approach, like the example above, which merely uses `map` and `flatMap` (actually, `andThen`, for sake of simplicity).
But as application gets more and more complex, it becomes progressively harder to structure the code and combine different parts of application.

For instance, if the above application required some sort of caching and a scheduler (like CRON), `IO` might not be a sufficient context anymore (again, simplifying for the sake of example):

```scala
trait Cache[F[_], K, V] {
  def put(key: K, value: V): F[Unit]

  def get(key: K): F[Optional[V]]

  def invalidate(): F[Unit]
}

def forexExchange(from: Currency, to: Currency)(implicit C: Cache[IO, (Currency, Currency), Decimal]) = {
  C.get((from, to)) match {
    case Some(rate) => IO.pure(makeHtml(rate, from, to))
    case None =>
      makeHttpCall("http://forex.com/convert/${from}/${to}")
        .andThen(response => parseForexJson(response).andThen(json => getExchangeRate(json).andThenThru(rate => C.put((from, to), rate)).andThen(rate => makeHtml(rate, from, to))))
  }
}
```

The only change required here is the passing of implicits from the parent function (which declares them) to the corresponding :

```scala
def forexExchange(from: Currency, to: Currency)(implicit C: Cache[IO, (Currency, Currency), Decimal], H: HttpClient) = {
  C.get((from, to)) match {
    case Some(rate) => IO.pure(makeHtml(rate, from, to))
    case None =>
      makeHttpCall("http://forex.com/convert/${from}/${to}")
        .andThen(response => parseForexJson(response).andThen(json => getExchangeRate(json).andThenThru(rate => C.put((from, to), rate)).andThen(rate => makeHtml(rate, from, to))))
  }
}
```

And with scheduled cache invalidation:

```scala
trait Schedule[F[_]] {
  def schedule(delay: Duration, task: F[Unit]): F[Unit]
}

def forexExchange(from: Currency, to: Currency)(implicit C: Cache[IO, (Currency, Currency), Decimal], S: Scheduler[IO]) = {
  C.get((from, to)) match {
    case Some(rate) => IO.pure(makeHtml(rate, from, to))
    case None =>
      H.makeHttpCall("http://forex.com/convert/${from}/${to}")
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
def forexExchange(from: Currency, to: Currency)(implicit C: Cache[F, (Currency, Currency), Decimal], H: HttpClient[F], S: Scheduler[F]) = do {
  httpResponse <- H.makeHttpCall("http://forex.com/convert/${from}/${to}")
  json <- parseForexJson(httpResponse)
  rate <- getExchangeRate(json)
  _ <- C.put((from, to), rate)
  _ <- S.schedule(DateTime.now + Duration.ofHours(1), () => C.invalidate())
  html = makeHtml(rate, from, to)
} yield html
```

The above code defines a program `forexExchange` which runs in a context `F` which must be provided at runtime and it must have the capabilities of a cache, an IO and Schedule.
Assuming somehow the types of each call (`IO::makeHttpCall`, `parseForexJson`, `getExchangeRate`, `Cache::put`, `Scheduler::schedule`) can compose, we need to provide an implementation of each capability.
The composition of the effects (or return types of each of the calls listed above) means that we can chain the calls to each of the functions - without the `do` notation, the program would look just like a chain of `flatMap` calls:

```scala
def forexExchange(from: Currency, to: Currency)(implicit C, H, S) =
  H.makeHttpCall("...")
    .flatMap(httpResponse => parseForexJson(httpResponse))
    .flatMap(json => getExchangeRate(json))
    .flatMap(rate => C.put(CurrencyPair(from, to), rate).flatTap(() => S.schedule(1.hour, () => C.invalidate())))
    .map(rate => makeHtml(rate, from, to))
```

See how each next step of a program is a `flatMap`, `flatTap` (run the operation but return the input value) or `map`. Meaning whatever each operation returns must be composable with `flatMap` or `map` or `flatTap`.

For example, assume the functions are declared as following:

```scala
def makeHttpCall(url: String): IO[Either[HttpError, String]] = ???

def parseForexJson(json: String): Either[JsonParseError, Json] = ???

def getExchangeRate(json: Json): Option[Decimal]
```

With these declarations, the program won't compose.

The defined `flatMap` on the `IO[A]` type (returned by `makeHttpCall`) would look like this: `IO[A].flatMap((a: A) -> IO[B]) -> IO[B]`.
The alternative is `map`: `IO[A].map((a: A) -> B): IO[B]`.
But neither `flatMap` nor `map` would work for the types `makeHttpCall` and `parseForexJson` return:

```scala
IO[Either[A, B]].flatMap((a: Either[A, B]) -> Option[C]): IO[???] // this won't work: Option[C] should have been IO[C]

IO[Either[A, B]].map((a: Either[A, B]) -> Option[C]): IO[Option[C]] // this would only work if parseForexJson uses Either[A, B] as an argument
```
