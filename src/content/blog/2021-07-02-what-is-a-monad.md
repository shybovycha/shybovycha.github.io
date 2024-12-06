---
layout: post
title: "What is a Monad?"
date: '2021-07-02T00:00:00+10:00'
---

## What is a monad?

There is a talk by Gilad Bracha, ["Deconstructing functional programming"](https://www.infoq.com/presentations/functional-pros-cons/). This is a very good introduction to functional programming, monads included.

As per that talk, think about monad as a class like this:

```java
abstract class FlatMappable <A> {
  @Immutable
  private final A a;
  
  FlatMappable(A a) {
    this.a = a;
  }
  
  FlatMappable<B> flatMap<B>(Function<A, FlatMappable<B>> fn) {
    return fn.apply(a);
  }
}
```

Just rename `FlatMappable` to `Monad` and there you go.

Now, if you want more Haskell naming (gonna implement it in C++ for more correct syntax):

```cpp
template <typename A>
cass Monad {
public:
  static Monad<A> return(A a) {
    return Monad(a);
  }
  
  template <typename B> Monad<B> operator>>=(std::function<A, Monad<B>> fn) {
    return fn.apply(a);
  }

private:
  Monad(A a) : m_a(a) {}
  
  immutable<A> m_a;
}
```

Essentially, renaming constructor to `return` and `flatMap` to `operator>>=`

Also, in terms of `Mappable` vs `FlatMappable`:

```java
class Mappable <A> {
  private final A a;
  
  Mappable(A a) { this.a = a; }
  
  Mappable<B> map(Function<A, B> fn) {
    return Mappable(fn.apply(a));
  }
}

class Flattable <A> extends Mappable <A> {
  Flattable(A a) { super(a); }
  
  Flattable<A> flatten() {
    if (a instanceOf Flattable<A>) {
      return a.flatten();
    }
    
    return a;
  }
}

class FlatMappable <A> extends Flattable <A> {
  FlatMappable(A a) { super(a); }
  
  FlatMappable<A> flatMap<B>(Function<A, FlatMappable<B>> fn) {
    return map(fn).flatten();
  }
}
```

## Why do we need monads?

In order to preserve purity of programs in functional programming languages, you can not have side-effects in your program.

But if you need interaction with outside systems (IO, database, system clock, random number generator, etc.), you will need to somehow make these operations without side-effects.

So you write your entire program as a description of a workflow (e.g. how data will be processed). Basically your program becomes a chain of functions calling other functions. No waiting for events, nothing like that.

Then, when you run your program, "at the execution edge" (aka "event horizon"), just before your program finishes, the runtime will do all the "unsafe" (or rather "impure") operations (interaction with IO, system clock, etc.) and will feed all the input to your functions, take the output and modify "the state of the world outside".

Where are monads on this? Monads are basically just wrappers around other values, telling the runtime what to do. So that your code is still pure and all the impure side-effects will be eventually handled by runtime.

For example, reading from STDIN would be something like `IO<String>` - a monad, whose value will be provided eventually. So your program will be defined as a function something like

```scala
def main(IO[String] input): IO[String] =
  input.map { name -> printLn("Hello, ${name}") }
```

As you can see, `main` is a function which simply returns another `IO[String]` by mapping a monad parameter.

When you run it, the program will effectively stop until the `IO[String] input` parameter is filled with value. When the value will be provided, runtime will execute the rest of the code (the `map { ... }` part).

