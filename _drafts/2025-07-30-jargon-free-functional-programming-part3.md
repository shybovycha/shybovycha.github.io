---
layout: post
title: 'Jargon-free functional programming. Part 3: structuring application'
date: '2025-07-30T00:15:00+10:00'
tags: [javascript, typescript, type-safety, fp, frontend, programming-paradigms, functional-programming]
---

In the [previous entries](/2022/08/24/jargon-free-functional-programming-part2.html) I have described functional programming patterns in TypeScript based on a somewhat generic helper classes such as `Wrappable`, `Maybe`, `Either` and `IO`.

Let's recap: the basic building blocks for a functional program in this framework is the `Wrappable` class which works together with a single-argument function type `Func`:

```ts
type Func<A, B> = (a: A) => B;

abstract class Wrappable<A> {
  abstract andThenWrap<B>(func: Func<A, B>): Wrappable<B>;

  abstract andThen<B>(func: Func<A, Wrappable<B>>): Wrappable<B>;
}
```

It can then be extended for another helper classes, designed to handle some most common situations (generally known as "effects").
Such as: handling nullable or non-existent value (`null | undefined`) with a `Maybe` helper class:

```ts
abstract class Maybe <A> extends Wrappable <A> {
  abstract override andThenWrap <B>(func: Func<A, B>): Maybe<B>;

  abstract override andThen <B>(func: Func<A, Maybe<B>>): Maybe<B>;

  static fromNullable <A>(value: A | null | undefined): Maybe<A> {
      return (value === null || value === undefined) ? Maybe.none<A>() : Maybe.some<A>(value as A);
  }

  static some <A>(value: A): Some<A> {
      return new Some<A>(value);
  }

  static none <A>(): None<A> {
      return new None<A>();
  }
}

class Some <A> extends Maybe <A> {
  constructor(private readonly value: A) {
      super();
  }

  override andThenWrap <B>(func: Func<A, B>): Maybe<B> {
      return new Some(func(this.value));
  }

  override andThen <B>(func: Func<A, Maybe<B>>): Maybe<B> {
      return func(this.value);
  }
}

class None <A> extends Maybe <A> {
  constructor() {
      super();
  }

  override andThenWrap <B>(_: Func<A, B>): Maybe<B> {
      return new None<B>();
  }

  override andThen <B>(_: Func<A, Maybe<B>>): Maybe<B> {
      return new None<B>();
  }
}
```

Handling potentially erroneous situations with `Either` helper class:

```ts
abstract class Either <A, B> extends Wrappable<B> {
  abstract override andThenWrap <C>(func: Func<B, C>): Either<A, C>;

  abstract override andThen <C>(func: Func<B, Either<A, C>>): Either<A, C>;

  static left <A, B>(value: A): Either<A, B> {
      return new Left<A, B>(value);
  }

  static right <A, B>(value: B): Either<A, B> {
      return new Right<A, B>(value);
  }
}

class Left <A, B> extends Either <A, B> {
  constructor(private readonly value: A) {
      super();
  }

  override andThenWrap <C>(_: Func<B, C>): Either<A, C> {
      return new Left<A, C>(this.value);
  }

  override andThen <C>(func: Func<B, Either<A, C>>): Either<A, C> {
      return new Left<A, C>(this.value);
  }
}

class Right <A, B> extends Either <A, B> {
  constructor(private readonly value: B) {
      super();
  }

  override andThenWrap <C>(func: Func<B, C>): Either<A, C> {
      return new Right(func(this.value));
  }

  override andThen <C>(func: Func<B, Either<A, C>>): Either<A, C> {
      return func(this.value);
  }
}
```

Then there is the base class for postponing running any potentially unsafe (in terms of side-effects or functional programming dogmas) operations, `IO` (and a function with no arguments type, `Func0`):

```ts
type Func0<A> = () => A;

class IO <A> extends Wrappable<A> {
  constructor(private intentionFunc: Func0<A>) {
    super();
  }

  override andThenWrap<B>(func: Func<A, B>): IO<B> {
      return new IO<B>(() => func(this.intentionFunc()));
  }

  override andThen<B>(func: Func<A, IO<B>>): IO<B> {
      return new IO<B>(() => func(this.intentionFunc()).intentionFunc());
  }

  unsafeRun() {
      this.intentionFunc();
  }
}
```

This is then extended to support promises and fit nicely into JS world with `PromiseIO`:

```ts
class PromiseIO <A> extends Wrappable<A> {
  constructor(private readonly task: Func0<Promise<A>>) {
    super();
  }

  override andThenWrap<B>(func: Func<A, B>): PromiseIO<B> {
      return new PromiseIO<B>(() => this.unsafeRun().then(func));
  }

  override andThen<B>(func: Func<A, PromiseIO<B>>): PromiseIO<B> {
      return new PromiseIO<B>(() =>
          new PromiseIO<PromiseIO<B>>(() =>
              this.unsafeRun()
                  .then(func)
          )
          .unsafeRun()
          .then(p => p.unsafeRun())
      );
  }

  unsafeRun(): Promise<A> {
      return this.task();
  }
}
```

For running code that might throw an exception and further accomodate the existing JS APIs, we introduced a `ExceptionW` helper class:

```ts
class ExceptionW <A> extends Wrappable <A> {
  constructor(private readonly task: Func0<Wrappable<A>>, private readonly exceptionHandler: Func<unknown, Wrappable<A>>) {
    super();
  }

  override andThenWrap<B>(func: Func<A, B>): ExceptionW<B> {
      return new ExceptionW<B>(
          () => this.runExceptionW().andThenWrap(func),
          (e) => this.exceptionHandler(e).andThenWrap(func)
      );
  }

  override andThen<B>(func: Func<A, ExceptionW<B>>): ExceptionW<B> {
      return new ExceptionW<B>(
          () => this.runExceptionW().andThen(func),
          (e) => this.exceptionHandler(e).andThen(func)
      );
  }

  runExceptionW(): Wrappable<A> {
      try {
          return this.task();
      } catch (e) {
          return this.exceptionHandler(e);
      }
  }
}
```

Since all of the above classes can be only chained with either `andThen` or `andThenWrap` methods, both of which mandate the return type is always the same wrapper class (e.g. `Either.andThen(f)` always returns `Either` and `Maybe.andThenWrap(f)` always returns `Maybe`), we also introduced a way to chain other wrapper classes with so-called "transformers":

```ts
class PromiseIOT <A> extends Wrappable<A> {
  constructor(private readonly value: PromiseIO<Wrappable<A>>) {
    super();
  }

  override andThenWrap<B>(func: Func<A, B>): PromiseIOT<B> {
    return new PromiseIOT(this.value.andThenWrap(m => m.andThenWrap(func)));
  }

  override andThen<B>(func: Func<A, Wrappable<B>>): PromiseIOT<B> {
    return new PromiseIOT(this.value.andThenWrap(m => m.andThen(func)));
  }

  runPromiseIOT(): PromiseIO<Wrappable<A>> {
      return this.value;
  }
}
```

So that the chaining could look like this:

```ts
const value = new PromiseIOT(PromiseIO)
    .andThen(promise => Maybe)
    .andThen(some => Either)
    .andThen(right => right.value)
    .runPromiseIOT();
```

As the example of using the framework for a somewhat realistic task, we had a program that fetches top 10 board games from [boardgamegeek.com], gets a random entry and prints it out (to the console):

```ts
const fetchAPIResponse = (): PromiseIO<string> =>
  new PromiseIO(() => fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`).then(response => response.text()));

const getResponseXML = (response: string): ExceptionW<XMLDocument> =>
  new ExceptionW(
      () => Either.right<Error, XMLDocument>(new DOMParser().parseFromString(response, "text/xml")),
      () => Either.left<Error, XMLDocument>(new Error('Received invalid XML'))
  );

const createGame = (item: Element): Maybe<Game> => {
  const rank = Maybe.fromNullable(item.getAttribute('rank'));
  const name = Maybe.fromNullable(item.querySelector('name')).andThenWrap(name => name.getAttribute('value'));

  return rank.andThen(r =>
      name.andThenWrap(n => ({ name: n, rank: r } as Game))
  );
};

const extractGames = (doc: XMLDocument): Either<Error, Array<Game>> => {
  const items = Array.from(doc.querySelectorAll('items item'));

  return items.reduce((accEither, item) =>
      accEither.andThen(acc =>
          createGame(item)
              .fold(
                game => Either.right<Error, Array<Game>>([...acc, game]),
                () => Either.left<Error, Array<Game>>(new Error('bad item'))
              )
      ),
      Either.right<Error, Array<Game>>([])
  );
};

const getRandomTop10Game = (games: Array<Game>): Either<Error, Game> => {
  if (games.length < 10) {
      return Either.left<Error, Game>(new Error('Not enough games'));
  }

  return Either.right<Error, Game>(games[(Math.random() * 100) % 10]);
};

const printGame = (game: Game): void => {
  console.log('RESULT', `#${game.rank}: ${game.name}`);
};

const program =
  new PromiseIOT(
      fetchAPIResponse()
          .andThenWrap(response => getResponseXML(response))
          .andThenWrap(w => w.runExceptionW())
  )
  .andThen(doc => extractGames(doc))
  .andThen(games => getRandomTop10Game(games))
  .andThenWrap(game => printGame(game))
  .runPromiseIOT();

program.unsafeRun();
```

What this code shows is one aspect of functional programming - deferring (or postponing) the _execution_ of the code to the very end of the program - compared to the "normal" code, which would be interpreted and executed right after the statement:

```ts
// gets executed right away:
fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`).then(response => response.text());

// nothing happens until the `unsafeRun()` is called:
new PromiseIO(() => fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`).then(response => response.text()));
```

What this allows to do is separate the program description from program execution. As collateral, this approach enforces (to a degree) the developer to handle erroneous situations. This is good, but it does not leverage the full potential of functional programming.

In this blog we are going to discover the bigger advantage of functional programming - specifically the wrapper classes we have been using.
If you structure the program in a specific way (discussed below), you can improve the testability and the separation of the application parts greatly.

Currently the program we came up with is a chain of very specific actions (you can call them "effects") - no matter what, when the `unsafeRun` is called, the program is going to behave in the very same way.

There is a way to structure the application in a slightly more complex way, ironically called ["Free"](https://typelevel.org/cats/datatypes/freemonad.html). The core idea is instead of building program as a chain of specific steps, build a chain of _data structures_ which then would be _interpreted_ by a specific "runner" (or "interpreter"). It has a very similar flavour to what we have right now - a program does not get executed until a specific point - currently it is calling the `unsafeRun()` method, in this new approach it is running the "runner" (or "interpreter"). The big difference is that the runners could be replaced with other runners, making program behave differently. The runners could also be chained to perform modifications of the program itself (for the sake of extra logging, profiling and all the way to optimizing the program by eliminating the redundant operations and caching the expensive calls).
