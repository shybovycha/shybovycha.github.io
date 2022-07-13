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