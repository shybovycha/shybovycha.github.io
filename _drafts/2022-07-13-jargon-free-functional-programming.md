# Jargon-free functional programming

## Basics

Let me introduce you functional programming with as few jargonisms and buzz-words as possible.

Shall we start with a simple problem to solve: get a random board game from top-10 games on BoardGamesGeek website and print out its rank and title.

In JavaScript the solution of this problem might look something like this:

```js
fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`)
    .then(response => response.text())
    .then(response => new DOMParser().parseFromString(response, "text/xml"))
    .then(doc => {
        const items = Array.from(doc.querySelectorAll('items item'));

        return items.map(item => {
            const rank = item.getAttribute('rank');
            const name = item.querySelector('name').getAttribute('value');

            return { rank, name };
        });
    })
    .then(games => {
        const randomRank = Math.floor((Math.random() * 100) % 10);

        return games[randomRank];
    })
    .then(randomTop10Game => {
        const log = `#${randomTop10Game.rank}: ${randomTop10Game.name}`;

        console.log(log);
    });
```

Quick and easy, quite easy to understand - seems good enough.

How about we write some tests for it? Oh, now it becomes a little bit clunky - we need to mock `fetch` call (Fetch API) and the `Math.random`. Oh, and the `DOMParser` with its `querySelector` and `querySelectorAll` calls too. Probably even `console.log` method as well. Okay, we will probably need to modify the original code to make testing easier (if even possible). How about we split the program into separate blocks of code?

```js
const fetchAPIResponse = () =>
    fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`)
        .then(response => response.text());

const getResponseXML = (response) =>
    new DOMParser().parseFromString(response, "text/xml");

const extractGames = (doc) => {
    const items = Array.from(doc.querySelectorAll('items item'));

    return items.map(item => {
        const rank = item.getAttribute('rank');
        const name = item.querySelector('name').getAttribute('value');

        return { rank, name };
    });
};

const getRandomTop10Game = (games) => {
    const randomRank = Math.floor((Math.random() * 100) % 10);

    return games[randomRank];
};

const printGame = (game) => {
    const log = `#${game.rank}: ${game.name}`;

    console.log(log);
};

fetchAPIResponse()
    .then(response => getResponseXML(response))
    .then(doc => extractGames(doc))
    .then(games => getRandomTop10Game(games))
    .then(game => printGame(game));
```

Okay, now we can test some of the bits of the program without _too much_ of a hassle - we could test that every call of `getRandomGame` returns a different value (which might not be true) but within the given list of values. We could test the `extractGames` function on a mock XML document and verify it extracts all the `<item>` nodes and its `<name>` child. Testing `fetchAPIResponse` and `getResponseXML` and `printGame` functions, though, would be a bit tricky without either mocking the `fetch`, `console.log` and `DOMParser` or actually calling those functions.

```js
import {
  fetchAPIResponse,
  getResponseXML,
  extractGames,
  getRandomTop10Game,
  printGame
} from "./index";

describe("fetchAPIResponse", () => {
  describe("bad response", () => {
    beforeEach(() => {
      global.fetch = jest.fn(() => Promise.reject("404 Not Found"));
    });

    it("returns rejected promise", () => {
      expect(fetchAPIResponse()).rejects.toBe("404 Not Found");
    });
  });

  describe("ok response", () => {
    beforeEach(() => {
      global.fetch = jest.fn(() =>
        Promise.resolve({
          text() {
            return `<?xml version="1.0" encoding="utf-8"?><items><item rank="1"><name value="Beyond the Sun"/></item></items>`;
          }
        })
      );
    });

    it("returns rejected promise", () => {
      expect(fetchAPIResponse()).resolves.toBe(
        `<?xml version="1.0" encoding="utf-8"?><items><item rank="1"><name value="Beyond the Sun"/></item></items>`
      );
    });
  });
});

describe("getResponseXML", () => {
  describe("null passed", () => {
    it("returns no <item> nodes", () => {
      const doc = getResponseXML(null);

      const items = Array.from(doc.querySelectorAll("item"));

      expect(items).toHaveLength(0);
    });
  });

  describe("invalid text passed", () => {
    it("returns no <item> nodes", () => {
      const doc = getResponseXML("404 not found");

      const items = Array.from(doc.querySelectorAll("item"));

      expect(items).toHaveLength(0);
    });
  });

  describe("blank document passed", () => {
    it("returns no <item> nodes", () => {
      const doc = getResponseXML('<?xml version="1.0" encoding="utf-8"?>');

      const items = Array.from(doc.querySelectorAll("item"));

      expect(items).toHaveLength(0);
    });
  });

  describe("valid document passed", () => {
    it("returns <item> nodes", () => {
      const doc = getResponseXML(
        '<?xml version="1.0" encoding="utf-8"?><items><item rank="1"><name value="Beyond the Sun"/></item></items>'
      );

      const items = Array.from(doc.querySelectorAll("item"));

      expect(items).toHaveLength(1);
    });
  });
});

describe("extractGames", () => {
  describe("null document", () => {
    it("throws an exception", () => {
      expect(() => extractGames(null)).toThrow();
    });
  });

  describe("empty document", () => {
    it("returns empty array", () => {
      const doc = new DOMParser().parseFromString("", "text/xml");
      expect(extractGames(doc)).toStrictEqual([]);
    });
  });

  describe("valid document", () => {
    it("returns an array of games", () => {
      const doc = new DOMParser().parseFromString(
        `<?xml version="1.0" encoding="utf-8"?><items><item rank="3"><name value="Beyond the Sun"/></item></items>`,
        "text/xml"
      );

      expect(extractGames(doc)).toStrictEqual([
        { name: "Beyond the Sun", rank: "3" }
      ]);
    });
  });
});

describe("getRandomTop10Game", () => {
  describe("null passed", () => {
    it("throws an exception", () => {
      expect(() => getRandomTop10Game(null)).toThrow();
    });
  });

  describe("empty array passed", () => {
    it("returns undefined", () => {
      expect(getRandomTop10Game([])).toStrictEqual(undefined);
    });
  });

  describe("less than 10 element array passed", () => {
    it("returns undefined", () => {
      const games = [
        { name: "game1", rank: 1 },
        { name: "game2", rank: 2 }
      ];
      const randomGames = [...new Array(100)].map(() =>
        getRandomTop10Game(games)
      );

      expect(randomGames).toContain(undefined);
    });
  });

  describe("10 or more element array passed", () => {
    it("never returns undefined", () => {
      const games = [
        { name: "game1", rank: 1 },
        { name: "game2", rank: 2 },
        { name: "game3", rank: 3 },
        { name: "game4", rank: 4 },
        { name: "game5", rank: 5 },
        { name: "game6", rank: 6 },
        { name: "game7", rank: 7 },
        { name: "game8", rank: 8 },
        { name: "game9", rank: 9 },
        { name: "game10", rank: 10 }
      ];

      const randomGames = [...new Array(100)].map(() =>
        getRandomTop10Game(games)
      );

      expect(randomGames).not.toContain(undefined);
    });

    it("returns an instance of each game", () => {
      const games = [
        { name: "game1", rank: "1" },
        { name: "game2", rank: "2" },
        { name: "game3", rank: "3" },
        { name: "game4", rank: "4" },
        { name: "game5", rank: "5" },
        { name: "game6", rank: "6" },
        { name: "game7", rank: "7" },
        { name: "game8", rank: "8" },
        { name: "game9", rank: "9" },
        { name: "game10", rank: "10" }
      ];

      const randomGames = [...new Array(100)].map(() =>
        getRandomTop10Game(games)
      );

      expect(randomGames).toStrictEqual(expect.arrayContaining(games));
    });
  });
});

describe("printGame", () => {
  describe("null passed", () => {
    it("throws an exception", () => {
      expect(() => printGame(null)).toThrow();
    });
  });

  describe("game passed", () => {
    const mockLogFn = jest.fn();

    beforeEach(() => {
      console.log = mockLogFn;
    });

    it("prints it to console", () => {
      printGame({ name: "game 42", rank: "42" });

      expect(mockLogFn).toHaveBeenCalledWith("#42: game 42");
    });
  });
});
```

In a lot of ways, I personally find these tests quite... hacky. But they seem to cover most of the functionality.

Let us talk about corner cases. As in, what would happen if the API does not return the result? Or what would happen if the result is not a valid XML (like `404 Not Found` text)? Or what would happen if the XML is valid, but it does not contain any `items` or `item[rank]>name[value]` nodes? Or what if it only returns `5` results (or any number of results less than `10`, for that matter)?

In most of the cases, the promise will get rejected (since an entire program is a chain of `Promise.then` calls). So you might think this is just fine and rely on the rejection logic handling (maybe even using `Promise.catch`).

If you want to be smart about these error cases, you would need to introduce the checks to each and every step of the chain.

In "classic" JS or TS you might want to return `null` (or, less likely, use Java-style approach, throwing an exception) when the error occurs.
This, however, comes with the need to introduce the `null` checks all over the place.
Consider this refactoring:

```js
const fetchAPIResponse = () =>
    fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`)
        .then(response => response.text());

const getResponseXML = (response) => {
    try {
        return new DOMParser().parseFromString(response, "text/xml");
    } catch {
        return null;
    }
};

const extractGames = (doc) => {
    if (!doc) {
        return null;
    }

    const items = Array.from(doc.querySelectorAll('items item'));

    return items.map(item => {
        const rank = item.getAttribute('rank');
        const name = item.querySelector('name').getAttribute('value');

        return { rank, name };
    });
};

const getRandomTop10Game = (games) => {
    if (!games) {
        return null;
    }

    if (games.length < 10) {
        return null;
    }

    const randomRank = Math.floor((Math.random() * 100) % 10);

    return games[randomRank];
};

const printGame = (game) => {
    if (!game) {
        return null;
    }

    const log = `#${game.rank}: ${game.name}`;

    console.log(log);
};

fetchAPIResponse()
    .then(response => getResponseXML(response))
    .then(doc => extractGames(doc))
    .then(games => getRandomTop10Game(games))
    .then(game => printGame(game));
```

In case you don't want to bother with `null` values or want to have a better logging (not necessarily error handling), you can straight away throw an exception:

```js
const fetchAPIResponse = () =>
    fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`)
        .then(response => response.text());

const getResponseXML = (response) => {
    try {
        return new DOMParser().parseFromString(response, "text/xml");
    } catch {
        throw 'Received invalid XML';
    }
};

const extractGames = (doc) => {
    const items = Array.from(doc.querySelectorAll('items item'));

    return items.map(item => {
        const rank = item.getAttribute('rank');
        const name = item.querySelector('name').getAttribute('value');

        return { rank, name };
    });
};

const getRandomTop10Game = (games) => {
    if (!games) {
        throw 'No games found';
    }

    if (games.length < 10) {
        throw 'Less than 10 games received';
    }

    const randomRank = Math.floor((Math.random() * 100) % 10);

    return games[randomRank];
};

const printGame = (game) => {
    if (!game) {
        throw 'No game provided';
    }

    const log = `#${game.rank}: ${game.name}`;

    console.log(log);
};

fetchAPIResponse()
    .then(response => getResponseXML(response))
    .then(doc => extractGames(doc))
    .then(games => getRandomTop10Game(games))
    .then(game => printGame(game))
    .catch(error => console.error('Failed', error));
```

Alternatively, since an entire program is a chain of promises, you could just return a rejected promise:

```js
const fetchAPIResponse = () =>
    fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`)
        .then(response => response.text());

const getResponseXML = (response) => {
    try {
        return new DOMParser().parseFromString(response, "text/xml");
    } catch {
        return Promise.reject('Received invalid XML');
    }
};

const extractGames = (doc) => {
    const items = Array.from(doc.querySelectorAll('items item'));

    return items.map(item => {
        const rank = item.getAttribute('rank');
        const name = item.querySelector('name').getAttribute('value');

        return { rank, name };
    });
};

const getRandomTop10Game = (games) => {
    if (!games) {
        return Promise.reject('No games found');
    }

    if (games.length < 10) {
        return Promise.reject('Less than 10 games received');
    }

    const randomRank = Math.floor((Math.random() * 100) % 10);

    return games[randomRank];
};

const printGame = (game) => {
    if (!game) {
        return Promise.reject('No game provided');
    }

    const log = `#${game.rank}: ${game.name}`;

    console.log(log);
};

fetchAPIResponse()
    .then(response => getResponseXML(response))
    .then(doc => extractGames(doc))
    .then(games => getRandomTop10Game(games))
    .then(game => printGame(game))
    .catch(error => console.error('Failed', error));
```

That's all good and nice and we seem to have covered most of the edge case scenarios (at least those we could think of).
Now, what if I tell you the program is still not entirely correct? See those `querySelector` calls? They might return `null` if the node
or the attribute is not present. And we do not want those empty objects in our program' output. This might be tricky to catch immediately
while developing the code.

One might even argue that most of those errors would have been caught by the compiler, if we have used something like TypeScript.
And they might be right - for the most part:

```ts
interface Game {
    name: string;
    rank: string;
}

const fetchAPIResponse = () =>
    fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`)
        .then(response => response.text());

const getResponseXML = (response: string) => {
    try {
        return new DOMParser().parseFromString(response, "text/xml");
    } catch {
        throw 'Received invalid XML';
    }
};

const extractGames = (doc: XMLDocument) => {
    const items = Array.from(doc.querySelectorAll('items item'));

    return items.map(item => {
        const rank = item.getAttribute('rank') ?? '';
        const name = item.querySelector('name')?.getAttribute('value') ?? '';

        return { rank, name };
    });
};

const getRandomTop10Game = (games: Array<Game>) => {
    if (!games) {
        throw 'No games found';
    }

    if (games.length < 10) {
        throw 'Less than 10 games received';
    }

    const randomRank = Math.floor((Math.random() * 100) % 10);

    return games[randomRank];
};

const printGame = (game: Game) => {
    if (!game) {
        throw 'No game provided';
    }

    const log = `#${game.rank}: ${game.name}`;

    console.log(log);
};

fetchAPIResponse()
    .then(r => getResponseXML(r))
    .then(doc => extractGames(doc))
    .then(games => getRandomTop10Game(games))
    .then(game => printGame(game))
    .catch(error => console.error('Failed', error));
```

Not too many changes, but those pesky little errors were caught at development time, pretty much.
The testing is still a challenge, though.

There is a application design approach which might be able to solve quite a bit of the aforementioned issues.
Let me introduce you to the world of functional programming without a ton of buzzwords and overwhelming terminology.

## Disclaimer

A big disclaimer before diving too deep: I am going to introduce all of the concepts without relying on any frameworks,
libraries, specific programming languages and whatnot - just sticking to the hand-written TypeScript. This might seem like
a lot of boilerplate and overhead for little benefit, but (with notes of a philosophy) the biggest benefit is in the cost of
detecting and fixing errors in the code:

* IDE highlighting an error (and maybe even suggesting a fix) - mere seconds of developer's time
* Local build (compiling the code locally, before pushing the code to the repository) - minutes, maybe tens of minutes
* CI server build, running all the tests possible - around an hour
* Pre-production environment (manual testing on dedicated QA / staging environment or even testing on production) - around few hours, may involve other people
* Production - measured in days or months and risking the reputation with the customers

_TODO: add a diagram_

Hence if we could detect the errors while writing the code the first time - we could potentially save ourselves a fortune measured in both time and money.

## Fancy-less introduction to functional programming

The ideas of functional programming are quite simple.
In functional programming the assumption is that every function only operates on the arguments
it has been passed and nothing else. It can not change the "outer world" - it can have values
temporarily assigned to internal constants, nothing more - take it as there are no variables.
A function should always return the same result for the same arguments, so functions are
always predictable, no matter how many times you call them.

That sounds good and nice, but how does that solve the issues of the above problem, you might ask.
For the most part the functions we have extracted already comply with the ideas of
functional programming - do they not?

Well, not really. For once, fetching the data from the API is a big questionmark on how it fits into the picture of functional programming. Leaving the fetching aside, we have a random call in the middle.
We also log some output to the console (and thus change the "outer world", outside the `printGame` function).

For a lot of things like those, functional programming tries to separate the "pure functional operations" and "impure operations".

See, in functional programming you operate these "pure functions", which only use constants and inputs to produce their outputs. So a program would be nothing but a chain of function calls. With "simple" functions, returning the values which the next function in the chain can take as an input argument, this is quite easy.

Take the code above as an example:

```js
fetchAPIResponse()
    .then(response => getResponseXML(response))
    .then(doc => extractGames(doc))
    .then(games => getRandomTop10Game(games))
    .then(game => printGame(game));
```

This could have been written as

```js
fetchAPIResponse()
    .then(response =>
        printGame(
            getRandomTop10Game(
                extractGames(
                    getResponseXML(response)
                )
            )
        )
    );
```

Since each next function in the chain accepts exactly the type the previous function has returned, they all combine quite well.

<jfyi>
In other languages and some libraries there are operators to combine functions into one big function:

```js
fetchAPIResponse()
    .then(response =>
        _.flow([ getResponseXML, extractGames, getRandomTop10Game, printGame ])(response)
    );
```

or

```java
fetchAPIResponse()
    .then(response ->
        getResponseXML.andThen(extractGames).andThen(getRandomTop10Game).andThen(printGame).aplly(response)
    );
```
</jfyi>

You will understand why this matters in a minute.

There are also functions which need to interact with the outer world. In that case, functional programming suggests that we wrap them in specific constructions and do not run them immediately.
Instead, we weave them into the program, describing what would happen to the result of the wrapped function call when we get one. This makes programs again, "pure functional", "safe" (as in not operating outside of the boundaries of the program itself, all is contained in the function call chains). Then, once we run the program, we enter the world of "unsafe" and execute all those wrapped functions and run the rest of the code once we get the results in place.

Sounds a bit hard to comprehend.

Let me rephrase this with few bits of code.

For the problem above, we are trying to get the response of an API somewhere in the outer world.
This is said to be an "unsafe" operation, since the data lives outside of the program, so we need to wrap this operation in a "safe" manner. Essentially, we will create an _object_ which describes _an intention to run the fetch call_ and then write our program around this object to describe how this data will be processed down the line, when we actually run the program (and the fetch request together with it).

See it in the code:

```ts
class IO {
    private intentionFunc: Function;

    constructor(func: Function) {
        this.intentionFunc = func;
    }

    andThen(func: Function) {
        return new IO(() => func(this.intentionFunc()));
    }

    unsafeRun() {
        this.intentionFunc();
    }
}
```

Essentially, we save the function we intend to run in the `intentionFunc` member of an `IO` class.
When we want to describe what would happen to the result of the data, we return a new `IO` object with a new function - a combination of a function we will call around the call to the function we saved. This is important to understand why we return a new object: so that we do not mutate the original object.

You might see this new `IO` thing is very similar to the `Promise` available in JS runtime already.
The similarities are obvious: we also have this chaining with the `then` method. The call to the `then` method also returns a new object.

But the main issue with `Promise` is that they start running the code you passed in the constructor immediately. And that is exactly the issue we are trying to resolve.

Now, let us see how we would use this new `IO` class in the original problem:

```js
new IO(() => fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`))
```

That would not work, however, since `fetch` call will return a `Promise`. So we need to somehow work with `Promise` instances instead.
Let me postpone this discussion for a short while.

<spoiler>
We could have tried implementing an `unpromisify` helper which would make the `fetch` call synchronous, but promises start executing not immediately,
but once you leave the context of a currently running function. So having that endless `while` loop, waiting for a promise to get resolved has zero effect
since this loop will be running until the end of days, but unless you exit the function beforehand, the promise won't start executing because JS is single threaded
and the execution queue / event loop prevents you from running the promise immediately.
</spoiler>

For now, let us pretend this code would magically work, so we can talk about one important matter.
As I mentioned above, simple functions combine easily if one function accepts the same argument type the previous function returns.
Think `extractGames` and `getRandomTop10Game` - `getRandomTop10Game` accepts an argument of type `Array<Game>` while `extractGames` returns just that - `Array<Game>`.
But with this new construct of ours, `IO`, combining anything would be tricky:

```ts
// since Function is not a typed interface in TypeScript
// I have extracted a simple 1-argument function type
type Func <A, B> = (_: A) => B;

class IO <A, B> {
    private intentionFunc: Func<A, B>;

    constructor(func: Func<A, B>) {
        this.intentionFunc = func;
    }

    andThen<C>(func: Func<B, C>) {
        return new IO<A, C>((arg: A) => func(this.intentionFunc(arg)));
    }

    unsafeRun(arg: A) {
        this.intentionFunc(arg);
    }
}

interface Game {
    name: string;
    rank: string;
}

const fetchAPIResponse = () =>
    new IO(() => fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`).then(response => response.text()));

const getResponseXML = (response: string) => {
    try {
        return new DOMParser().parseFromString(response, "text/xml");
    } catch {
        throw 'Received invalid XML';
    }
};

const extractGames = (doc: XMLDocument) => {
    const items = Array.from(doc.querySelectorAll('items item'));

    return items.map(item => {
        const rank = item.getAttribute('rank') ?? '';
        const name = item.querySelector('name')?.getAttribute('value') ?? '';

        return { rank, name };
    });
};

const getRandomTop10Game = (games: Array<Game>) => {
    if (!games) {
        throw 'No games found';
    }

    if (games.length < 10) {
        throw 'Less than 10 games received';
    }

    const randomRank = Math.floor((Math.random() * 100) % 10);

    return games[randomRank];
};

const printGame = (game: Game) => {
    if (!game) {
        throw 'No game provided';
    }

    const log = `#${game.rank}: ${game.name}`;

    console.log(log);
};

fetchAPIResponse()
    .andThen(r => getResponseXML(r))
    .andThen(doc => extractGames(doc))
    .andThen(games => getRandomTop10Game(games))
    .andThen(game => printGame(game))
    .unsafeRun(undefined); // needed as `fetchAPIResponse`, the initial `IO` object in the chain, does not take any arguments
```

Except for a tricky class declaration to support strong types by TypeScript, not much is different, huh?

The one big issue with almost any of the functions we have is that they not always produce results.
And that is an issue, since functional programming dictates a function must always produce one.

The way we deal with this situation in functional programming is by using few helper classes, which describe the presence or abscence of a result.
You might actually know them by heart: `T?` also known as `T | null`, `T1 | T2` - TypeScript has it all:

```ts
const getRandomTop10Game = (games: Array<Game> | undefiend): Game | undefined => {
    return games?.[Math.random() * 100 % 10];
};

const printGame = (game: Game | undefined): void => {
    console.log(`#${game?.rank ?? ''}: ${game?.name ?? ''}`);
};
```

Whereas with the former, optional values, you can do chaining to some extent, this becomes a burden with alternate types:

```ts
const createGame3 = (item: Element): Error | Game => {
    const rank = item.getAttribute('rank');
    const name = item.querySelector('name')?.getAttribute('value');

    if (name && rank)
        return { name, rank } as Game;

    return new Error('invalid response');
};

const getResponseXML3 = (response: string): Error | XMLDocument => {
    try {
        return new DOMParser().parseFromString(response, "text/xml");
    } catch {
        return new Error('Received invalid XML');
    }
};

const extractGames3 = (doc: Error | XMLDocument): Error | Array<Game> => {
    if (doc instanceof Error)
        return doc;

    const items = Array.from(doc.querySelectorAll('items item'));

    const games = [] as Array<Game>;

    for (let item of items) {
        const rank = item.getAttribute('rank');
        const name = item.querySelector('name')?.getAttribute('value');

        if (name && rank)
            games.push({ rank, name } as Game);
        else
            return new Error('invalida data in XML');
    }

    return games;
};

const getRandomTop10Game3 = (games: Error | Array<Game>): Error | Game => {
    if (games instanceof Error)
        return games;

    return games[Math.random() * 100 % 10];
};

const printGame3 = (game: Error | Game): void => {
    if (game instanceof Error)
        return;

    console.log(`#${game.rank}: ${game.name}`);
};
```

Observe how all those `instanceof` checks and questionmarks and pipes infested the code.

Let us have more conscise wrappers, very similar to what other functional programming technologies have:

```ts
abstract class Maybe <A> {
    abstract andThen <B>(func: Func<A, B>): Maybe<B>;

    abstract andThenWrap <B>(func: Func<A, Maybe<B>>): Maybe<B>;

    static option <A>(value: A | null | undefined): Maybe<A> {
        return (!value) ? Maybe.none<A>() : Maybe.some<A>(value);
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

    override andThen <B>(func: Func<A, B>): Maybe<B> {
        return new Some(func(this.value));
    }

    override andThenWrap <B>(func: Func<A, Maybe<B>>): Maybe<B> {
        return func(this.value);
    }
}

class None <A> extends Maybe <A> {
    constructor() {
        super();
    }

    override andThen <B>(_: Func<A, B>): Maybe<B> {
        return new None<B>();
    }

    override andThenWrap <B>(_: Func<A, Maybe<B>>): Maybe<B> {
        return new None<B>();
    }
}
```

The simplest (yet not entirely helpful) way to handle exceptions in functional programming world would be using a wrapper like this:

```ts
abstract class Either <A, B> {
    abstract andThen <C>(func: Func<B, C>): Either<A, C>;

    abstract andThenWrap <C>(func: Func<B, Either<A, C>>): Either<A, C>;

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

    override andThen <C>(_: Func<B, C>): Either<A, C> {
        return new Left<A, C>(this.value);
    }

    override andThenWrap <C>(func: Func<B, Either<A, C>>): Either<A, C> {
        return new Left<A, C>(this.value);
    }
}

class Right <A, B> extends Either <A, B> {
    constructor(private readonly value: B) {
        super();
    }

    override andThen <C>(func: Func<B, C>): Either<A, C> {
        return new Right(func(this.value));
    }

    override andThenWrap <C>(func: Func<B, Either<A, C>>): Either<A, C> {
        return func(this.value);
    }
}
```

This might not look as simplistic or clean when defined, but see how it changes the code:

```ts
const extractGames = (doc: Either<Error, XMLDocument>): Either<Error, Array<Game>> => {
    return doc.andThen((d: XMLDocument) => {
        const items = Array.from(d.querySelectorAll('items item'));

        return items.map(item => {
            const rank = item.getAttribute('rank') ?? '';
            const name = item.querySelector('name')?.getAttribute('value') ?? '';

            return { rank, name };
        });
    });
};

const getRandomTop10Game = (games: Either<Error, Array<Game>>): Either<Error, Game> => {
    return games.andThen(gs => gs[Math.random() * 100 % 10]);
};

const printGame = (game: Either<Error, Game>): void => {
    game.andThen(g => console.log(`#${g.rank}: ${g.name}`));
};
```

Now, there are two parts where we still have those ugly questionmarks:

```ts
const extractGames = (doc: Either<Error, XMLDocument>): Either<Error, Array<Game>> => {
    return doc.andThen((d: XMLDocument) => {
        const items = Array.from(d.querySelectorAll('items item'));

        return items.map(item => {
            const rank = item.getAttribute('rank') ?? '';
            const name = item.querySelector('name')?.getAttribute('value') ?? '';

            return { rank, name };
        });
    });
};
```

Let's see if we can utilize the new helper classes to beautify this:

```ts
const createGame = (item: Element): Maybe<Game> => {
    const rank = Maybe.maybe(item.getAttribute('rank'));
    const name = Maybe.maybe(item.querySelector('name')).andThen(name => name.getAttribute('value'));

    return rank.andThenWrap(r =>
        name.andThen(n => ({ name: n, rank: r } as Game))
    );
};
```

Now the issue is: the `createGame` function takes an `Element` and returns a `Maybe<Game>`, but the `extractGames` function should return an `Either<Error, Array<Game>>`.
So we can't just write something like this and expect it to work:

```ts
const extractGames = (doc: Either<Error, XMLDocument>): Either<Error, Array<Game>> => {
    return doc.andThen((d: XMLDocument) => {
        const items = Array.from(d.querySelectorAll('items item'));

        return items.map(item => createGame(item));
    });
};
```

There simply is no constructor to convert from `Maybe<T>` to `Either<A, B>` both technically, in the code, and logically - what would be the `Left` and what would be the `Right` then? One might argue, for `None` we could return `Left<?>` and for `Some<A>` we could return `Right<A>`. That would be the closest to truth, but we still need to come up with a type for `Left`.

This might actually be a good time to consider if these two types have something in common and, potentially, extract few interfaces.

```ts
abstract class Wrappable <A> {
    abstract andThen <B>(func: Func<A, B>): Wrappable<B>;

    abstract andThenWrap <B>(func: Func<A, Wrappable<B>>): Wrappable<B>;
}
```

```ts
abstract class Maybe <A> extends Wrappable <A> {
    abstract override andThen <B>(func: Func<A, B>): Maybe<B>;

    abstract override andThenWrap <B>(func: Func<A, Maybe<B>>): Maybe<B>;

    abstract toEither<B>(func: () => B): Either<B, A>;

    static option <A>(value: A | null | undefined): Maybe<A> {
        return (!value) ? Maybe.none<A>() : Maybe.some<A>(value);
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

    override andThen <B>(func: Func<A, B>): Maybe<B> {
        return new Some(func(this.value));
    }

    override andThenWrap <B>(func: Func<A, Maybe<B>>): Maybe<B> {
        return func(this.value);
    }

    override toEither<B>(_: () => B): Either<B, A> {
        return Either.right<B, A>(this.value);
    }
}

class None <A> extends Maybe <A> {
    constructor() {
        super();
    }

    override andThen <B>(_: Func<A, B>): Maybe<B> {
        return new None<B>();
    }

    override andThenWrap <B>(_: Func<A, Maybe<B>>): Maybe<B> {
        return new None<B>();
    }

    override toEither<B>(func: () => B): Either<B, A> {
        return Either.left<B, A>(func());
    }
}
```

```ts
abstract class Either <A, B> extends Wrappable<B> {
    abstract override andThen <C>(func: Func<B, C>): Either<A, C>;

    abstract override andThenWrap <C>(func: Func<B, Either<A, C>>): Either<A, C>;

    abstract leftToMaybe(): Maybe<A>;

    abstract rightToMaybe(): Maybe<B>;

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

    override andThen <C>(_: Func<B, C>): Either<A, C> {
        return new Left<A, C>(this.value);
    }

    override andThenWrap <C>(func: Func<B, Either<A, C>>): Either<A, C> {
        return new Left<A, C>(this.value);
    }

    leftToMaybe(): Maybe<A> {
        return Maybe.some(this.value);
    }

    rightToMaybe(): Maybe<B> {
        return Maybe.none<B>();
    }
}

class Right <A, B> extends Either <A, B> {
    constructor(private readonly value: B) {
        super();
    }

    override andThen <C>(func: Func<B, C>): Either<A, C> {
        return new Right(func(this.value));
    }

    override andThenWrap <C>(func: Func<B, Either<A, C>>): Either<A, C> {
        return func(this.value);
    }

    leftToMaybe(): Maybe<A> {
        return Maybe.none<A>();
    }

    rightToMaybe(): Maybe<B> {
        return Maybe.some(this.value);
    }
}
```

```ts
const createGame = (item: Element): Maybe<Game> => {
    const rank = Maybe.option(item.getAttribute('rank'));
    const name = Maybe.option(item.querySelector('name')).andThen(name => name.getAttribute('value'));

    return rank.andThenWrap(r =>
        name.andThen(n => ({ name: n, rank: r } as Game))
    );
};

const extractGames = (doc: Either<Error, XMLDocument>): Either<Error, Array<Game>> => {
    // TODO: flatten
    return doc.andThenWrap((d: XMLDocument) => {
        const items = Array.from(d.querySelectorAll('items item'));

        return items.map(item => createGame(item).toEither(() => new Error('bad item')));
    });
};
```

```ts
const extractGames = (doc: Either<Error, XMLDocument>): Either<Error, Array<Game>> => {
    return doc.andThenWrap((d: XMLDocument) => {
        const items = Array.from(d.querySelectorAll('items item'));

        return items.reduce((accEither, item) =>
            accEither.andThenWrap(acc =>
                createGame(item)
                    .toEither(() => new Error('bad item'))
                    .andThen(game => [...acc, game])
            ),
            Either.right<Error, Array<Game>>([])
        );
    });
};
```

```ts
const getResponseXML = (response: string): Either<Error, XMLDocument> => {
    try {
        return Either<Error, XMLDocument>.right(new DOMParser().parseFromString(response, "text/xml"));
    } catch {
        return Either<Error, XMLDocument>.left('Received invalid XML');
    }
};
```

```ts
const fetchAPIResponse = () =>
    new PromiseIO(() => fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`).then(response => response.text()));
```

```ts
const program = fetchAPIResponse()
    .map(r => getResponseXML(r))
    .map(doc => extractGames(doc))
    .map(games => getRandomTop10Game(games))
    .map(game => printGame(game))
    .unsafeRun();
```

But we can do better than this: we can get rid of all those `Either` and `Maybe` in functions which do not throw an error:

```ts
const createGame = (item: Element): Maybe<Game> => {
    const rank = Maybe.maybe(item.getAttribute('rank'));
    const name = Maybe.maybe(item.querySelector('name')).map(name => name.getAttribute('value'));

    return rank.flatMap(r =>
        name.map(n => ({ name: n, rank: r } as Game))
    );
};

const getResponseXML = (response: string): Either<Error, XMLDocument> => {
    try {
        return Either<Error, XMLDocument>.right(new DOMParser().parseFromString(response, "text/xml"));
    } catch {
        return Either<Error, XMLDocument>.left('Received invalid XML');
    }
};

const extractGames = (doc: XMLDocument): Either<Error, Array<Game>> => {
    const items = Array.from(doc.querySelectorAll('items item'));

    return items.reduce((accEither, item) =>
        accEither.flatMap(acc =>
            createGame(item)
                .toEither(() => new Error('bad item'))
                .map(game => [...acc, game])
        ),
        Either.right<Error, Array<Game>>([])
    );
};

const getRandomTop10Game = (games: Array<Game>): Either<Error, Game> => {
    if (games.length < 10) {
        return Either<Error, Game>.left(new Error('Not enough games'));
    }

    return Either<Error, Game>.right(games[Math.random() * 100 % 10]);
};

const printGame = (game: Game): void => {
    console.log(`#${game.rank}: ${game.name}`);
};
```

Now if we try to compose the program, we would get quite a few "type mismatch" errors:

```ts

const program = fetchAPIResponse()
    .map(r => getResponseXML(r))
    .map(doc => extractGames(doc))
    .map(games => getRandomTop10Game(games))
    .map(game => printGame(game))
    .unsafeRun();
```

yielding

```
Argument of type 'Either<Error, XMLDocument>' is not assignable to parameter of type 'XMLDocument'.
    Type 'Either<Error, XMLDocument>' is missing the following properties from type 'XMLDocument': addEventListener, removeEventListener, URL, alinkColor, and 247 more.

Argument of type 'Either<Error, Game[]>' is not assignable to parameter of type 'Game[]'.
    Type 'Either<Error, Game[]>' is missing the following properties from type 'Game[]': length, pop, push, concat, and 25 more.

Argument of type 'Either<Error, Game>' is not assignable to parameter of type 'Game'.
    Type 'Either<Error, Game>' is missing the following properties from type 'Game': name, rank
```

Let us follow the types being passed around:

```ts
const program = fetchAPIResponse() // () ~> PromiseIO<string>
    .map(r => getResponseXML(r)) // r: string -> getResponseXML(XMLDocument): Either<Error, XMLDocument> ~> PromiseIO<Either<Error, XMLDocument>>
    .map(doc => extractGames(doc)) // doc: Either<Error, XMLDocument> -> extractGames(Array<Game>): Either<Error, Array<Game>> ~> PromiseIO<Either<Error, Array<Game>>>
    .map(games => getRandomTop10Game(games)) // games: Either<Error, Array<Game>> -> getRandomTop10Game(Array<Game>): Either<Error, Game> ~> PromiseIO<Either<Error, Game>>
    .map(game => printGame(game)) // game: Either<Error, Game> -> printGame(Game): void ~> PromiseIO<void>
    .unsafeRun(); // () ~> void
```

The issues begin when we try to call `map` on `PromiseIO<Either<Error, XMLDocument>>`:

```ts
fetchAPIResponse().map(r => getResponseXML(r)) // => PromiseIO<Either<Error, XMLDocument>>
```

Let's recall how the method `map` on `PromiseIO<A>` looks like:

```ts
class PromiseIO<A> {
    map<B>(func: Func<A, B>): PromiseIO<B>;
}
```

So it expects a function which takes the type which `PromiseIO` wraps and returns a new type to be wrapped in `PromiseIO`.
But the type current `PromiseIO` wraps is `Either<Error, XMLDocument>`.
While the function we pass, `extractGames` looks like this:

```ts
const extractGames = (doc: XMLDocument): Either<Error, Array<Game>>;
```

See the issue?

We could have fixed it by adding few more nested functions, like this:

```ts
const program = fetchAPIResponse()
    .map(r => getResponseXML(r))
    .map(docE => docE.flatMap(doc => extractGames(doc)))
    .map(gamesE => gamesE.flatMap(games => getRandomTop10Game(games)))
    .map(gameE => gameE.map(game => printGame(game)))
    .unsafeRun();
```

Or by changing the signature of the functions to take `Either<Error, ?>` instead.

But there is a more neat way to handle cases like this in functional programming.

Since the entire program is a chain of `andThen` and `andThenWrap` calls on different
`Wrappable` objects, we can create one which will wrap the `try..catch` expression.

Consider the code which might fail:

```ts
const getResponseXML = (response: string): XMLDocument =>
    new DOMParser().parseFromString(response, "text/xml");
```

This might not return what we need, according to the [`DOMParser` documentation](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser/parseFromString#error_handling).
When the string passed to the `parseFromString` method is not a valid document, the object
returned would be a document with a single node - `<parsererror>`.
Normally, in JS or TS, we would either return an "invalid" value (think `null` or `undefined`) or throw an exception.

In functional programming we could get away with returning a `Maybe` or `Either` object.

However, let us consider throwing exception just to paint the bigger picture:

```ts
const getResponseXML = (response: string): XMLDocument => {
    try {
        const doc = new DOMParser().parseFromString(response, "text/xml");

        // see https://developer.mozilla.org/en-US/docs/Web/API/DOMParser/parseFromString#error_handling
        if (doc.querySelector('parsererror'))
            throw new Error('Parser error');

        return doc;
    } catch (e) {
        // ???
    }
};
```

The above function, once called, will actually _do some work_. Luckily, in this specific case it won't
do anything too "offensive" in terms of functional programming restrictions (like going to the database or making network calls).
But it will _do something_ rather than just _return a value_.

Every time we call this function, the new `DOMParser` will be created and it will parse the string.

In functional programming world, as mentioned above, the program is just a chain of calls.
So the way to handle situations like this would be to somehow wrap the _actual work_ and only
execute it upon request.

The best way we could _wrap work_ is in a function which is yet to be called:

```ts
() => new DOMParser().parseFromString(response, "text/xml")
```

We then should allow for this code to be worked around in a "callback" manner (as you might be familiar with the concept from
NodeJS and event handlers in browser JS) - the entire program should build upon a concept of "when the result of this execution becomes available".

The way to do so is already described above in terms of `Wrappable` class with its `andThen` and `andThenWrap` methods:

```ts
new XWrappable(() => new DOMParser().parseFromString(response, "text/xml"))
    .andThen(document => doSomething(document))
```

Hence we could build a new `Wrappable` which would wrap both the logic in the `try` block and the logic in the `catch` block.
However, it should not call neither of those blocks of logic until explicitly requested - so we should add a new method to this
new wrappable - to call the logic and return a certain value.

```ts
class ExceptionW <A> implements Wrappable <A> {
    constructor(private readonly task: Func0<Wrappable<A>>, private readonly exceptionHandler: Func<unknown, Wrappable<A>>) {}

    andThen<B>(func: Func<A, B>): ExceptionW<B> {
        return new ExceptionW<B>(
            () => this.runExceptionW().andThen(func),
            (e) => this.runExceptionW(e).andThen(func)
        );
    }

    andThenWrap<B>(func: Func<A, ExceptionW<B>>): ExceptionW<B> {
        return new ExceptionW<B>(
            () => this.runExceptionW().andThenWrap(func),
            (e) => this.runExceptionW(e).andThenWrap(func)
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

Now the program from the beginning of the article can be re-written as follows:

```ts
const getResponseXML = (response: string): ExceptionW<XMLDocument> =>
    new ExceptionW(
        () => {
            const doc = new DOMParser().parseFromString(response, "text/xml");

            if (doc.querySelector('parsererror'))
                return Either<Error, XMLDocument>.left(new Error('Parser error'));

            return Either<Error, XMLDocument>.right(doc);
        },
        (e) => Either<Error, XMLDocument>.left(e)
    );
```

Just to confirm once more if you are still asking "why bother with this `ExceptionW` thing? why not to use `Either` right away?":
this whole `ExceptionW` thing allows us to _postpone running the actual logic_ and build the entire program as a function of
the result of this logic. In turn, when we are ready to execute the entire program, we can run this wrapped logic.

Let us see how this new function can be used in the program:

```ts
const program = (getResponseXML('invalid XML')
    .andThenWrap(doc => extractGames(doc))
    .andThenWrap(games => getRandomTop10Game(games))
    .runExceptionW() as Either<Error, XMLDocument>>)
    .andThen(result => console.log('success', result));
```

There is a little bit of a quirk around type casting (`(???.runExceptionW() as Either<Error, XMLDocument>).andThen`).
We can solve it by slightly modifying the `runExceptionW` method:

```ts
runExceptionW<W extends Wrappable<A>>(): W {
    try {
        return this.task() as W;
    } catch (e) {
        return this.exceptionHandler(e) as W;
    }
}
```

With that, the code becomes a little bit cleaner:

```ts
const program = getResponseXML('invalid XML')
    .andThenWrap(doc => extractGames(doc))
    .andThenWrap(games => getRandomTop10Game(games))
    .runExceptionW<Either<Error, XMLDocument>>()
    .andThen(result => console.log('success', result));
```

With this new concept of wrapping an entire blocks of functionality in a `Wrappable`, we can solve a similar problem
with the initial program: promises.

Instead of just creating a promise object together with the `Wrappable`, we can instead hide it in a function.

Let us do this step by step. First, the interface implementation and the overall skeleton of a new class:

```ts
class PromiseIO <A> implements Wrappable<A> {
    andThen<B>(func: Func<A, B>): PromiseIO<B> {
        // ???
    }

    andThenWrap<B>(func: Func<A, PromiseIO<B>>): PromiseIO<B> {
        // ???
    }
}
```

Now, to wrap the promise:

```ts
class PromiseIO <A> implements Wrappable<A> {
    constructor(private readonly task: Func0<Promise<A>>) {}

    andThen<B>(func: Func<A, B>): PromiseIO<B> {
        // ???
    }

    andThenWrap<B>(func: Func<A, PromiseIO<B>>): PromiseIO<B> {
        // ???
    }
}
```

And running the wrapped promise:

```ts
class PromiseIO <A> implements Wrappable<A> {
    constructor(private readonly task: Func0<Promise<A>>) {}

    andThen<B>(func: Func<A, B>): PromiseIO<B> {
        // ???
    }

    andThenWrap<B>(func: Func<A, PromiseIO<B>>): PromiseIO<B> {
        // ???
    }

    unsafeRun(): Promise<A> {
        return this.task();
    }
}
```

Note how I explicitly called it "unsafe" - since promise can (and, most likely, will) work with an outside world,
we should only run it when we are ready to run the whole program, so it immediately produces the result as
a function of whatever the wrapped promise has returned.

Then, when we need to chain the logic off that promise, we do not really need to call the promise - instead,
we create a new `Wrappable` instance which _will_ call the promise somewhere in the future.

So instead of calling `wrappedPromise.then(func)`, which is now wrapped in a function `() => Promise<A>`,
we create a new `PromiseIO` wrappable with a new function, which will do something _when the promise returns_.

This is better explained with the code, in my opinion:

```ts
class PromiseIO <A> implements Wrappable<A> {
    constructor(private readonly task: Func0<Promise<A>>) {
    }

    andThen<B>(func: Func<A, B>): PromiseIO<B> {
        return new PromiseIO<B>(() => this.unsafeRun().then(func));
    }

    andThenWrap<B>(func: Func<A, PromiseIO<B>>): PromiseIO<B> {
        return new PromiseIO<B>(() => this.unsafeRun().then(func));
    }

    unsafeRun(): Promise<A> {
        return this.task();
    }
}
```

Unfortunately, this won't work. The issue is in the `andThenWrap` method.
Recall the interface of `Wrappable<A>`:

```ts
andThen<B>(func: Func<A, B>): Wrappable<B>;

andThenWrap<B>(func: Func<A, Wrappable<B>>): Wrappable<B>;
```

Now the code we have in there right now will work if the function `func` returned `B`,
which would be exactly what `andThen` method does:

```ts
// func: Func<A, B>

this.unsafeRun().then(func)
// => Promise<A>.then((a: A) => func(a)) => Promise<B>

new PromiseIO(() => this.unsafeRun().then(func)) // seems OK, gives PromiseIO<B>
```

But since the function `func` returns `PromiseIO<B>` instead, the result would be slightly different:

```ts
// func: Func<A, PromiseIO<B>>

this.unsafeRun().then(func)
// => Promise<A>.then((a: A) => func(a)) => Promise<PromiseIO<B>>

new PromiseIO(() => this.unsafeRun().then(func))
// this gives PromiseIO<PromiseIO<B>>, can't return from `andThenWrap`
```

But notice how this is a nested `PromiseIO` object - can we maybe "unwrap" and "repack" it?

Turns out, we can - remember how a `Promise<A>.then(() => Promise<B>)` resolves in `Promise<B>`.
We can utilize this property of `Promise`, if the function we wrap will return `Promise<Promise<B>>`.

And that's where our `unsafeRun` method can be utilized within the function we wrap:

```ts
new PromiseIO<B>(
    () => {
        const p1 = new PromiseIO<PromiseIO<B>>(
            () => this.unsafeRun() // gives Promise<A>
                .then(func) // this gives the Promise<PromiseIO<B>>
        );

        const p2: Promise<PromiseIO<B>> = p1.unsafeRun();

        const p3: Promise<B> = p2.then(
            (p: PromiseIO<B>) => p.unsafeRun() // this gives Promise<B>
        );

        // at this stage we have Promise<Promise<B>>, which is automatically converted to Promise<B>
        // passing it to the constructor of PromiseIO, wrapped in a function, will give PromiseIO<B>

        return p3;
    }
)
```

and then

```ts
class PromiseIO <A> implements Wrappable<A> {
    constructor(private readonly task: Func0<Promise<A>>) {
    }

    andThen<B>(func: Func<A, B>): PromiseIO<B> {
        return new PromiseIO<B>(() => this.unsafeRun().then(func));
    }

    andThenWrap<B>(func: Func<A, PromiseIO<B>>): PromiseIO<B> {
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

Now let us build our solution again, using this newly acquired wrappable:

```ts
const fetchAPIResponse = () =>
    new PromiseIO(() => fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`).then(response => response.text()));

const program = fetchAPIResponse()
    .andThen(response => getResponseXML(response))
    .andThen(doc => extractGames(doc))
    .andThen(games => getRandomTop10Game(games))
    .runExceptionW<Either<Error, XMLDocument>>()
    .andThen(game => printGame(game))
    .unsafeRun();
```

Uh-oh, there is an issue with the types again:

```ts
fetchAPIResponse() // PromiseIO<string>
    .andThen(response => getResponseXML(response)) // PromiseIO<ExceptionW<XMLDocument>>
    .andThen(doc => extractGames(doc)) // doc is now ExceptionW<XMLDocument>
```

So we can not really use the existing functions directly - the types mismatch.
The issue is that they are nested twice already - `PromiseIO<ExceptionW<XMLDocument>>`.
And the `extractGames` function expects just the `XMLDocument` as an input.
Is there a way to extract the double-wrapped value?

Well, in functional programming, we use another wrappable (oh really) which ignores the outside
wrappable and operates on the nested one. We call them "transformers":

```ts
new PromiseIOT(fetchAPIResponse()) // PromiseIOT<string>, the transformer
    .andThen(response => getResponseXML(response)) // unpacks the string, runs the function, and packs everything back in PromiseIOT
    .andThen(doc => extractGames(doc)) // unpacks the XMLDocument, runs the function, and packs everything back in PromiseIOT
```

So this new thing is only helpful for dealing with whatever `PromiseIO` wraps.
And, unfortunately, there needs to be a new one for whatever the new wrappable you come up with.
This is understandable to some extent, since each "external" wrappable would handle the functions
`andThen` and `andThenWrap` differently, so there can not really be a universal one.

Let's see how we can create one for `PromiseIO`, step-by-step.

First things first, the skeleton of a transformer:

```ts
class PromiseIOT <A> implements Wrappable<A> {
    andThen<B>(func: Func<A, B>): PromiseIOT<B> {
        // ???
    }

    andThenWrap<B>(func: Func<A, Wrappable<B>>): PromiseIOT<B> {
        // ???
    }
}
```

And the thing we wrap, which is a `PromiseIO<A>`:

```ts
class PromiseIOT <A> {
    constructor(private readonly value: PromiseIO<Wrappable<A>>) {
    }

    andThen<B>(func: Func<A, B>): PromiseIOT<B> {
        // ???
    }

    andThenWrap<B>(func: Func<A, Wrappable<B>>): PromiseIOT<B> {
        // ???
    }
}
```

And, as in case with `PromiseIO` itself, there has to be a way to run the promise (since it is wrapped).
We will simply expose the value we wrap - as unsafe as it might look, the value we wrap is a wrapped promise already:

```ts
class PromiseIOT <A> {
    constructor(private readonly value: PromiseIO<Wrappable<A>>) {
    }

    andThen<B>(func: Func<A, B>): PromiseIOT<B> {
        // ???
    }

    andThenWrap<B>(func: Func<A, Wrappable<B>>): PromiseIOT<B> {
        // ???
    }

    runPromiseIOT(): PromiseIO<Wrappable<A>> {
        return this.value;
    }
}
```

Note how it has to be `PromiseIO<Wrappable<A>>` - so the value `PromiseIO` wraps is a `Wrappable` on its own.
This is where the transformers differ from the "simple" wrappers - if `PromiseIO` (the thing transformer wraps)
would wrap a simple type (`PromiseIO<A>`) - then there is no need for a transformer - just use the wrapper' interface directly.

Now, to the implementation: the function passed to `andThen` and `andThenWrap` methods would be applied
to the thing the `PromiseIO` wraps. And the transformer wraps that `PromiseIO` object.
Hence we need to call `this.value.andThen()` to get to the value `PromiseIO` wraps.
But since that value is a wrapped type itself, we call `andThen` on it too:

```ts
andThen<B>(func: Func<A, B>): PromiseIOT<B> {
    return new PromiseIOT(this.value.andThen(m => m.andThen(func)));
}
```

And when we want to return a new wrappable to be wrapped in a `PromiseIO`, we simply use `andThenWrap` on a nested-nested value:

```ts
andThenWrap<B>(func: Func<A, Wrappable<B>>): PromiseIOT<B> {
    return new PromiseIOT(this.value.andThen(m => m.andThenWrap(func)));
}
```

The whole transformer looks like this now:

```ts
class PromiseIOT <A> {
    constructor(private readonly value: PromiseIO<Wrappable<A>>) {
    }

    andThen<B>(func: Func<A, B>): PromiseIOT<B> {
        return new PromiseIOT(this.value.andThen(m => m.andThen(func)));
    }

    andThenWrap<B>(func: Func<A, Wrappable<B>>): PromiseIOT<B> {
        return new PromiseIOT(this.value.andThen(m => m.andThenWrap(func)));
    }

    runPromiseIOT(): PromiseIO<Wrappable<A>> {
        return this.value;
    }
}
```

To incorporate it in the program:

```ts
const program = new PromiseIOT(fetchAPIResponse())
```

This won't do, since `fetchAPIResponse` returns `PromiseIO<string>`, meaning it wraps the simple type.
And we need a `Wrappable` instead - otherwise we don't need the transformer. Conveniently for us,
the next function in the chain takes that `string` value and returns a wrappable, `ExceptionW<XMLDocument>`.
So we can smash them together and pass to the `PromiseIOT`:

```ts
const program = new PromiseIOT(
    fetchAPIResponse()
        .andThen(response => getResponseXML(response)
)
```

Now, there is one thing to recall from the previous wrappable we made, `ExceptionW` - the logic it wraps
will only be executed upon request. And we need to incorporate this request in the program.

Since our program so far looks the way it looks, it's type is `PromiseIOT`, which does not have
a way to run `ExceptionW` it wraps. In fact, it does not even have an interface to do so,
since we have explicitly designed it to run all the operations on the doubly-nested type.
We do have one option, though - to run the `ExceptionW` right after we have fetched the response:

```ts
const program = new PromiseIOT(
    fetchAPIResponse()
        .andThen(response => getResponseXML(response))
        .andThen(ew => ew.runExceptionW<Either<Error, XMLDocument>>())
)
```

Note how we are not immediately running the logic wrapped by `ExceptionW`, but deferring it to the point in the future
when the `Promise` returns with the response (or failure). So the chain of function calls is still within
the restrictions of functional programming (or rather pure functions) mentioned in the beginning of this article.

Now the only thing left is to put the rest of the calls matching the types to the `andThen` or `andThenWrap` methods
and run the entire program:

```ts
const program = new PromiseIOT(
    fetchAPIResponse()
        .andThen(response => getResponseXML(response))
        .andThen(ew => ew.runExceptionW<Either<Error, XMLDocument>>())
)
    .andThenWrap(doc => extractGames(doc))
    .andThenWrap(games => getRandomTop10Game(games))
    .andThen(game => printGame(game))
    .runPromiseIOT()
    .unsafeRun();
```

Leaving all the "boilerplate" code aside (all those wrappables), this is how our program looks like so far:

```ts
const fetchAPIResponse = (): PromiseIO<string> =>
    new PromiseIO(() => fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`).then(response => response.text()));

const getResponseXML = (response: string): ExceptionW<XMLDocument> =>
    new ExceptionW(
        () => Either<Error, XMLDocument>.right(new DOMParser().parseFromString(response, "text/xml")),
        () => Either<Error, XMLDocument>.left(new Error('Received invalid XML'))
    );

const createGame = (item: Element): Maybe<Game> => {
    const rank = Maybe.option(item.getAttribute('rank'));
    const name = Maybe.option(item.querySelector('name')).andThen(name => name.getAttribute('value'));

    return rank.andThenWrap(r =>
        name.andThen(n => ({ name: n, rank: r } as Game))
    );
};

const extractGames = (doc: XMLDocument): Either<Error, Array<Game>> => {
    const items = Array.from(doc.querySelectorAll('items item'));

    return items.reduce((accEither, item) =>
        accEither.andThenWrap(acc =>
            createGame(item)
                .convert(
                    () => Either<Error, Array<Game>>.left(new Error('bad item')),
                    game => Either<Error, Array<Game>>.right([...acc, game])
                )
        ),
        Either.right<Error, Array<Game>>([])
    );
};

const getRandomTop10Game = (games: Array<Game>): Either<Error, Game> => {
    if (games.length < 10) {
        return Either<Error, Game>.left(new Error('Not enough games'));
    }

    return Either<Error, Game>.right(games[(Math.random() * 100) % 10]);
};

const printGame = (game: Game): void => {
    console.log('RESULT', `#${game.rank}: ${game.name}`);
};

const program =
    new PromiseIOT(
        fetchAPIResponse()
            .andThen(response => getResponseXML(response))
            .andThen(w => w.runExceptionW<Either<Error, XMLDocument>>())
    )
    .andThenWrap(doc => extractGames(doc))
    .andThenWrap(games => getRandomTop10Game(games))
    .andThen(game => printGame(game))
    .runPromiseIOT();

program.unsafeRun();
```

Compare it to the initial implementation:

```ts
const fetchAPIResponse = () =>
    fetch(`https://boardgamegeek.com/xmlapi2/hot?type=boardgame`)
        .then(response => response.text());

const getResponseXML = (response: string) => {
    try {
        return new DOMParser().parseFromString(response, "text/xml");
    } catch {
        throw 'Received invalid XML';
    }
};

const extractGames = (doc: XMLDocument) => {
    const items = Array.from(doc.querySelectorAll('items item'));

    return items.map(item => {
        const rank = item.getAttribute('rank') ?? '';
        const name = item.querySelector('name')?.getAttribute('value') ?? '';

        return { rank, name };
    });
};

const getRandomTop10Game = (games: Array<Game>) => {
    if (!games) {
        throw 'No games found';
    }

    if (games.length < 10) {
        throw 'Less than 10 games received';
    }

    const randomRank = Math.floor((Math.random() * 100) % 10);

    return games[randomRank];
};

const printGame = (game: Game) => {
    if (!game) {
        throw 'No game provided';
    }

    const log = `#${game.rank}: ${game.name}`;

    console.log(log);
};

fetchAPIResponse()
    .then(r => getResponseXML(r))
    .then(doc => extractGames(doc))
    .then(games => getRandomTop10Game(games))
    .then(game => printGame(game))
    .catch(error => console.error('Failed', error));
```

To me they both look quite similar. So was there any value in all this rewrite?
Let's find it out together!

How about testing?

There is a big test suite for the initial implementation at the top of this article.

How do we go about testing this true-to-functional-programming version though?

We can apply it to this new code with few minor changes around assertions - now
instead of expecting an exception to be thrown or a value immediately returned,
we need to check if the value returned by the function is `Left` or `Right` and
check the wrapped value.

How about testing the program as a whole?
Well now, since the program is just a value, we can easily test its value.
Dang, by replacing the functions in the chain we can even test different behaviours
of the program! As in, instead of using `fetchAPIResponse` we can pass in a different
instance of `PromiseIO` wrapping a completely wild value and see how the program would
process it.

Errors are not a problem now - they are values, not breaking the execution of a program.

Interacting with the outer world is a breeze - just substitute the `fetch` with any `Promise` you want
and it still won't break the whole program.

Now, here's the catch: I am not trying to sell you functional programming as the Holy Grail of
developing the software and a silver bullet to every problem you might have.

In fact, there is a bug in the current true-to-functional-programming implementation,
which is way easier to pin-point with the "conventional" error logs in the console.
However, to find it, one first must run the program. With the functional-programming-implementation,
the program will simply result in a `Either<Error, unknown>.Left` value, without providing _exact_
reason for the error.

The bug is tiny but pesky:

```ts
const getRandomTop10Game = (games: Array<Game>): Either<Error, Game> => {
    if (games.length < 10) {
        return Either<Error, Game>.left(new Error('Not enough games'));
    }

    return Either<Error, Game>.right(games[(Math.random() * 100) % 10]);
};
```

Issue is in the `games[(Math.random() * 100) % 10]` expression:
`(Math.random() * 100) % 10` is a floating-point number.
So most often, this will produce an `undefined` value.

And thus we come to the bigger issue, not related to the functional programming
itself, but rather the issue with TypeScript: in order for one to notice this issue,
the code must be run. But it would have not even been an issue if there was a mechanism
in place to prevent accessing the array elements by randomly typed index.

We can mitigate it in a similar manner we have dealt with all the problems in this whole
process of rewriting the program in a true functional programming way: implement a new
class which would wrap an array of elements of a same type and provide a reasonable
interface to access the elements.

Roughly put, if there was a class `List<A>` which would return a `Maybe<A>` when trying
to access an element of the list by index, the program would have looked differently:

```ts
const getRandomTop10Game = (games: List<Game>): Either<Error, Game> => {
    if (games.length() < 10) {
        return Either<Error, Game>.left(new Error('Not enough games'));
    }

    const game: Maybe<Game> = games.at((Math.random() * 100) % 10);

    return game.toEither(
        () => new Error('Bad game index in the list of games'),
        (value) => value
    );
};
```

And the whole program would have resulted in a `Left<Error>` with a message
rather than some misleading exception (with a stack trace, though).

----

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