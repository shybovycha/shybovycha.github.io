---
layout: post
title: 'Jargon-free functional programming. Part 1: problem statement'
date: '2022-08-24T00:00:00+00:00'
tags: [functional-programming, javascript, typescript, programming-paradigms, fp, promises, api, tutorial, programming, code-quality]
---

## Basics

Let me introduce you functional programming with as few jargonisms and buzz-words as possible.

Shall we start with a simple problem to solve: get a random board game from top-10 games on BoardGamesGeek website and print out its rank and title.

BoardGameGeek website has an API: a request `GET https://boardgamegeek.com/xmlapi2/hot?type=boardgame` will return an XML document like this:

```xml
<?xml version="1.0" encoding="utf-8"?>

<items termsofuse="https://boardgamegeek.com/xmlapi/termsofuse">
    <item id="361545" rank="1">
        <thumbnail value="https://cf.geekdo-images.com/lD8s_SQPObXTPevz-aAElA__thumb/img/YZG-deJK2vFm4NMOaniqZwwlaAE=/fit-in/200x150/filters:strip_icc()/pic6892102.png" />
        <name value="Twilight Inscription" />
        <yearpublished value="2022" />
    </item>

    <item id="276182" rank="2">
        <thumbnail value="https://cf.geekdo-images.com/4q_5Ox7oYtK3Ma73iRtfAg__thumb/img/TU4UOoot_zqqUwCEmE_wFnLRRCY=/fit-in/200x150/filters:strip_icc()/pic4650725.jpg" />
        <name value="Dead Reckoning" />
        <yearpublished value="2022" />
    </item>
</items>
```

In JavaScript a solution to this problem might look something like this:

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

<!--more-->

<div class="content-read-marker" data-fraction="25"></div>

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

<div class="content-read-marker" data-fraction="50"></div>

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

<div class="content-read-marker" data-fraction="75"></div>

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

<a href="/2022/08/24/jargon-free-functional-programming-part2.html" class="btn btn-primary">Proceed</a>

<div class="content-read-marker" data-fraction="100"></div>
