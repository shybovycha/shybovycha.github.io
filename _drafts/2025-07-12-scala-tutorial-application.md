---
layout: post
title: 'Scala tutorial application'
tags: [rest, fsharp, fp, backend, dotnet, programming-paradigms, frontend, data, functional-programming, api]
---

I have been reading some Rock the JVM articles about [FS2](https://rockthejvm.com/articles/fs2-more-than-functional-streaming-in-scala), [Http4S](https://rockthejvm.com/articles/http4s-unleashing-the-power-of-http-apis-library) and [Doobie](https://rockthejvm.com/articles/learning-doobie-for-the-greater-good).
These articles are tutorials built around a sample IMDB (movie database) clone application.
One thing that struck me is how a lot of tutorials (not just Rock the JVM ones) discuss designing an application around the database.
There is an inherent flaw in developing applications this way: developers tend to focus more on the implementation rather than the business case.
Thinking in terms of database entities rather than the functionality ends up causing a lot of issues at the late stages of development, simply due to the less flexible development process - it is harder to add or change functionality of an existing code, especially if it is already running in production.

I want to suggest a different approach. This is my alternative tutorial.

## What you will learn?

This tutorial is going to focus on three libraries: Fs2, Http4s and Doobie as a way to build a web application in Scala.
It would touch the DDD (domain-driven development) approach to building a software project and elements of functional programming in Scala such as tagless final.

## What we will build?

The application we will be building is a limited clone of IMDB - a movie database.

This will be a simple web application with four main functions: showing a list and details of movies and actors casting in those movies.

Let's define the application in terms of what entities and what operations there will be:

```fsharp
module MyIMDB =
  type Movie = { Title : string, Year: int, Cast: Actor list }

  type Actor = { Name: string, Born: date, Died: date }

  type ListMovies = Movie list

  type ListActors = Actor list

  type MovieCast = Movie -> Actor list
```

Note how there are no IDs for either `Movie` or `Actor` entities - they are the implementation detail, we will figure it out when we start implementing the application.

Note that movie title is just a `string`, meaning it could be anything - an empty string is an interesting case. Same with movie year - it is an integer, so it could easily be `0`, `-1` or `20` - none of which make sense. Both dates for `Actor` entity are also unbound. All of these edge cases could be actually described at this stage, to prevent mishaps later down the line:

```scala
import eu.timepit.refined._
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.api.{RefType, Refined}
import eu.timepit.refined.boolean._
import eu.timepit.refined.char._
import eu.timepit.refined.collection._
import eu.timepit.refined.generic._
import eu.timepit.refined.string._

type NonBlankString = NonEmpty And Not[Forall[Whitespace]]

type MovieTitle = NonBlankString
type ActorName = NonBlankString

type Year = Greater[1894] // first ever film

case class Actor(name: NonBlankString)
case class Movie(title: NonBlankString, year: Year, cast: List[Actor] Refined NonEmpty)

trait Imdb {
  def listMovies(): List[Movie]

  def listActors(): List[Actor]

  def actorMovies(actor: Actor): List[Movie]
}
```

Service then proceeds with an implementation in tagless final manner:

```scala
// ...
```

The storage level will operate with IDs, hence introduce two *distinct* types - `MovieID` and `ActorId` - both come from `Int` or `String`, but they represent different values.

With Doobie, use `IO` from cats to represent a deferred evaluation of a database operation (which is, in fact, I/O), unlike Rock the JVM, where author suggests using `ev.blocking`.

Eventually, build a tagless final interpreter implementation for both `IO` (actual application) and the mock one, for testing.
Potentially, compare tagless final approach with Free monad.
