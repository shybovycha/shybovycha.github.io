---
layout: post
title: 'TypeScript is bad for you'
---

Back in 2011 frontend was a very different thing - JavaScript had no `class`, `Object.entries` / `Object.keys`, promises were a proof of concept idea (unless 3rd party library [bluebird](https://github.com/petkaantonov/bluebird)) and Node was v0.10.

Then came [CoffeeScript](https://coffeescript.org/), which added nice helper features to JavaScript - list comprehensions, classes and if _statements_ (meaning you could use them for variable assignment):

```coffeescript
# if statements
text = if happy and knowsIt
  chaChaCha()
else if sexy
  knowsIt()
else if tooSexy
  removeShirt()
else
  showIt()

# list comprehensions
courses = [ 'greens', 'caviar', 'truffles', 'roast', 'cake' ]
menu = (i, dish) -> "Menu Item #{i}: #{dish}"
menu i + 1, dish for dish, i in courses

# ranges and list comprehensions
countdown = (num for num in [10..1])

# iterating over object entries
yearsOld = max: 10, ida: 9, tim: 11

ages = for child, age of yearsOld
  "#{child} is #{age}"
```

Whilst it was still compiled to an inferior ES5 JavaScript, it helped to organise the code. A good tool for that task, if you ask me.

Then came [Dart](https://dart.dev/) and TypeScript, which were also compiled to ES5 JavaScript, but they added not so much helper syntax features, but _types_, aiming to reduce the number of runtime errors by performing type checks at compile time.

Sounds good on paper, but they both suffered from the same issue as JavaScript itself and most common cause of runtime errors - the `null` and `undefined` still were a thing, causing same runtime errors. Not really a good tool for the job, if you ask me.

The one true benefit offered by TypeScript over the others was that it allowed to seamlessly use existing JavaScript code. And, provided you have the type signatures for that JavaScript code, it could even perform type checking on it too, effectively reducing the requirements for using TypeScript in the existing codebase.

Fast-forward to 2024 (**twelve** years since its first release) and TypeScript dominates the frontend world. While still suffering from the original issues and not bringing too many helpful syntactic features anymore. With the new EcmaScript standards, classes and promises became the first-class citizens in all browsers (even Internet Explorer / Edge, despite not being around anymore), the APIs and syntax became more mature (`Object.entries`, `async / await` to reduce [callback hell](http://callbackhell.com/), `for .. of`, `const / let` and many others). There are still no list comprehensions or conditional expressions though.

TypeScript still does help in a small subset of highly-specific scenarios like navigating in the IDE and refactoring the code, but since IDEs matured as well, it is not so much of a useful feature anymore. It can prevent some really stupid errors at compile time (like using a number I stead of an object), but I don't think developers run into them these days - again, IDEs are really helpful these days.

TODO: expander with a few more real-life issue examples, like lack of syntax for if/case expressions, "similar so good enough" matching types, OpenApi with its "is instance of This? Sure! (even if it's not)".

Let me raise a big question now: is it still worth using TypeScript?

Balance bike is a good tool to get you going - it gets you from walking to moving fast. But if you want to get faster and further, you have to drop it at some stage in favour of a more advanced bike.

Similarly, TypeScript gets you from plain JavaScript code to a slightly better place - you can refactor code faster, it saves you from a few errors at compile time. But if you really want to get far, you will have to solve the undefined and null errors for good.

This is where I'd suggest to use another language altogether, which similarly to CoffeeScript and TypeScript back in the day solved some problems at compile time. And suggest I will.

A rather controversial suggestion: use pure functional language, which does not have a concept of null and undefined in the first place. Check out Elm (dead, but a good starting point) and PureScript. And in that order. Let me explain.

Elm is like a very simplified Haskell - it is a pure functional language with a subset of Haskell syntax. It has a nice compiler with really good error messages. It enforces a structure for your application (redux-like). It gives you a gentle introduction to the functional programming concepts and it targets browsers (web applications). With its architecture, you can look at the message (action from redux) type and see exactly what are all possible operations in the application (which makes reading code and getting to know new codebases much easier).

On a bad note, it is not being developed since 2019, it comes with an entire runtime (saves you from runtime errors, but blows up the bundle size) and it is a all-or-nothing commitment for the project - it is an all-in-one platform and if you want to gradually update your application from React - sorry, you will have to rewrite entire parts of you application entirely in Elm. The good point turned bad, having all possible actions defined in one message type make complex applications _really_ complex (with one massive type definition, an issue very familiar to developers who had to deal with Redux).

Here could have been a trivial Elm code sample, but what good would it do? Here's an entire Elm application!

TODO: show how to use component-like structure with action/message type having a `ChartAction a` as a disjoint union variant. 

The next step on this journey would be PureScript. It is an actively developed language, it has a minimal footprint after compiled to JS (much smaller than Elm), it has a _very_ rich ecosystem and, best of all, it has a very simple interop with JS and it can compile just one module. Top it up with Halogen framework and you effectively got yourself Elm on steroids. The downside is that it is slightly more complex platform (language and framework) compared to Elm, so the learning curve is a bit steeper.

The real deal with this approach is how to migrate from an existing (most likely) React/TypeScript/(webpack | vite) ecosystem to PureScript?

Expanding on Scott Wlaschin's talk, you can (and probably should) separate the pure application logic from IO, potentially utilising the foreign imported functions to interact with the existing JS code (libraries). This way you keep your application logic error-free, and all the errors that can happen are shifted towards the presentation layer (MVC/MVP, remember this concept?).

This would be the best strategy for the most projects, migrating one bit at a time and making the application less and less error prone whilst not wreaking the havok by rewriting everything from scratch (very few businesses will buy into that).

The bigger issue is that most modern frontend apps I have seen are so mangled in mixing the business logic and the presentation layer, it would be challenging (to say the least) to get unmangle it to a reasonable code. Check how we handle UI action, triggering a HTTP request and updating both the UI (to display the request progress/status) and the application state (for other parts of the UI) at the same time.

TODO: add an example of converting _a_ component from React codebase to PureScript (maybe with Halogen).