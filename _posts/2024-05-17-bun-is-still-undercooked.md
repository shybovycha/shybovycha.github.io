---
layout: post
title: "Bun is still undercooked"
date: '2024-05-17T14:10:00+10:00'
tags: [bun, nodejs, javascript, bundler, package-manager, testing, performance, web-development, build-tools, benchmarking]
---

my Skunkworks project was trying out [Bun](https://bun.sh/). it was not a successful project, but there are some learnings:

TL;DR: I think Bun is still undercooked and despite being super cool and competitive on paper, it is a bit too early to use it in big projects. But hey, it works for my blog!

## What is Bun

Bun is a combined alternative for NodeJS, package manager (`npm` / `yarn` / `pnpm`), bundler (`vite` / `webpack` / `esbuild`) and test runner (`vite` / `jest`).

## Bun is ridiculously fast

| Command        | Yarn      | Bun       |
| -------------- | --------- | --------- |
| `yarn install` | `49 sec`  | `7.5 sec` |
| `vite build`   | `14 sec`  | `0.9 sec` |
| `vite test`    | `forever` | `4.5 sec` |

This most likely has to do with what happens in those tools - Bun went with parsing files as ASTs, applying transformations and running them in memory (to the best of my knowledge, digging through the Bun code)

## Some things work out of the box

Dependency management works like a charm. No questions asked. Bun is just 7x faster.
Comparing the node_modules directories:

```
Only in node_modules_yarn:
    .yarn-state.yml
    @aashutoshrathi
    @isaacs
    @npmcli
    @pkgjs
    @tootallnate
    abbrev
    agentkeepalive
    aggregate-error
    aproba
    are-we-there-yet
    asynciterator.prototype
    cacache
    chownr
    clean-stack
    color-support
    console-control-strings
    deep-equal
    delegates
    depd
    eastasianwidth
    encoding
    env-paths
    err-code
    es-get-iterator
    exponential-backoff
    foreground-child
    fs-minipass
    gauge
    graceful-fs
    has
    has-unicode
    humanize-ms
    ip
    is-lambda
    jackspeak
    jsonc-parser
    make-fetch-happen
    minipass-collect
    minipass-fetch
    minipass-flush
    minipass-pipeline
    minipass-sized
    minizlib
    mkdirp
    negotiator
    node-gyp
    nopt
    npmlog
    object-is
    p-map
    promise-retry
    retry
    set-blocking
    smart-buffer
    socks
    socks-proxy-agent
    ssri
    stop-iteration-iterator
    string-width-cjs
    strip-ansi-cjs
    tar
    unique-filename
    unique-slug
    wide-align
    wrap-ansi-cjs

Only in node_modules_bun:
    confbox
    es-object-atoms
    word-wrap
```

Curious to see if those packages missing in bun's `node_modules` are actually used anywhere.

## Plugins

In Relational Migrator we use few plugins with vite, namely `svgr`, `vanilla-extract` and `sentry`. Bun only supports limited esbuild plugins and does not have the aforementioned plugins. Some of them work with minimal changes, some of them do not work entirely.

### `svgr` plugin

`svgr` worked with minimal alterations:

```ts
import svgrEsbuildPlugin from 'esbuild-plugin-svgr';

Bun.build({
    plugins: [
        svgrEsbuildPlugin() as unknown as BunPlugin,
    ]
})
```

But required to change the imports from

```ts
import { ReactComponent as DatabaseAccessImage } from './assets/database-access-image.svg';
```

to

```ts
import DatabaseAccessImage from './assets/database-access-image.svg';
```

### `vanilla-extract` plugin

This one loads vite server to compile the CSS and does not work no matter what I tried, throwing the following errors all over the place:

```
error: Styles were unable to be assigned to a file. This is generally caused by one of the following:

- You may have created styles outside of a '.css.ts' context
- You may have incorrect configuration. See https://vanilla-extract.style/documentation/getting-started
      at getFileScope (.../frontend/node_modules/@vanilla-extract/css/fileScope/dist/vanilla-extract-css-fileScope.cjs.dev.js:35:11)
      at generateIdentifier (.../frontend/node_modules/@vanilla-extract/css/dist/vanilla-extract-css.cjs.dev.js:175:7)
      at style (.../frontend/node_modules/@vanilla-extract/css/dist/vanilla-extract-css.cjs.dev.js:374:19)
      at .../frontend/src/shared/leafygreen-ui/badge/badge.css.ts:4:28
```

Followed by

```
error: Module._load is not a function. (In 'Module._load(file, parentModule)', 'Module._load' is undefined)
    at .../frontend/src/components/mapping-banner.css.ts:1:0
```

### `sentry` plugin

This one was trivial and did not complain (I did not check if it actually works):

```ts
Bun.build({
    plugins: [
        sentryEsbuildPlugin({
                disable: !process.env.SENTRY_AUTH_TOKEN,
                org: 'mongodb-org',
                project: 'relational-migrator-frontend',
                telemetry: false,
                sourcemaps: {
                        filesToDeleteAfterUpload: '**/*.map',
                },
        }) as unknown as BunPlugin,
    ]
})
```

## Bundling

Bun is great to run bundling, testing or manage packages from CLI when things are relatively simple. When you need plugins (for instance), the interactions become tricky. For specifying and configuring bundle-time plugins one needs to use Bun's JS/TS API and make a custom build script:

```ts
await Bun.build({ ... });
```

By default, Bun does not log anything, which is actually quite inconvenient - not even build failures are logged. One has to get the result of `Bun.build()` and manually process them, which is a bit of a bummer:

```ts
const result = await Bun.build(...);

if (!result.success) {
  console.error('Build failed');

  for (const message of result.logs) {
    console.error(message);
  }
} else {
  console.info('Build succeeded');
}
```

## Configuration

Another inconvenient interaction - some actions require entire scripts (like build configuration, serving files, etc.). Then there is a config file, `bunfig.toml` where users can specify some configurations for Bun.

## Running tests

This one had the most issues on my end.

### `react-testing-library`

Bun declares support for `react-testing-library`, which worked as expected.

### Browser APIs

Had to use `happy-dom` and configure it in the `bunfig.toml` to enable some of the UI testing features (such as access to the `window` object). Yet, `happy-dom` still [lacks support for Canvas API](https://github.com/capricorn86/happy-dom/issues/241), for instance.

### `test.each`

It is one example of Bun's partial compatibility with Jest - with Jest one can use nice-ish string interpolation to generate test name:

```ts
test.each`
    currentStep | lastCompletedStep | progressType
    ${0}        | ${0}              | ${'active'}
    ${1}        | ${0}              | ${'inactive'}
    ${1}        | ${2}              | ${'checked'}
  `(
    'returns $progressType for $currentStep and $lastCompletedStep',
    ({ currentStep, lastCompletedStep, progressType }) => { })
);
```

With `bun:test` it is slightly different - you can't use arguments out of order, nor do you have access to their names. Neither can you use this nice syntactic sugar for defining test cases in a table manner.

```ts
const cases = [
    // currentStep | lastCompletedStep | progressType
    [ 0, 0, 'active' ],
    [ 1, 0, 'inactive' ],
    [ 1, 2, 'checked' ],
  ];

  test.each(cases)(
    'For %p and %p returns %p',
    (currentStep, lastCompletedStep, progressType) => {
      expect(getProgressType(currentStep, lastCompletedStep)).toEqual(
        progressType
      );
    }
  );
```

Oh, and there is no `describe.each()` functionality at all, which makes defining suites of tests more tedious.

### Mocks

Mocks work as expected, out of the box. There are mocks for system clock, which is nice. Had to replace `vi.fn()` with `mock()` and a corresponding `import { mock } from 'bun:test';`.

### `ObjectContaining` matchers

When using nested matchers in the `ObjectContaining`, some of them are missing in Jest compatibility (like `expect.toBeNumber`):

```ts
expect(nodes).toEqual([
        expect.objectContaining({
          id: 'node-1',
          position: { x: expect.toBeNumber(), y: expect.toBeNumber() },
        }),
]);
```

Had to use `expect.any(Number)` instead:

```ts
expect(nodes).toEqual([
        expect.objectContaining({
          id: 'node-1',
          position: { x: expect.any(Number), y: expect.any(Number) },
        }),
]);
```

### Using `Fragment` import alongside `<>`

If a component contains both `import { Fragment } from 'react'` and uses a shorthand `<>`, Bun will yell at test time (but not at build time, interestingly enough):

```
SyntaxError: Cannot declare an imported binding name twice: 'Fragment'.
```

If you specify a different `jsxFragmentFactory` in `tsconfig.json` **and** set `"jsx": "react"` (and **not** `"react-jsx"` or anything), you will get further.

After meddling with Bun source code itself, I figured something (like it parses files' AST and modifies them to add missing imports, like `Fragment` but it ends up with duplicates), but even after applying some crude hacks to prevent it from adding those duplicate statements, I could not get to fix the issues. Left a comment on Bun's [Github issue](https://github.com/oven-sh/bun/issues/7576), but from my experience developers do not pay enough attention to those.

Ended up manually changing sources for the libraries in question in `node_modules` folder directly (just for the test), which did actually help. Might be worth changing it in the libraries directly, but that won't work with everything.

### Ace editor

It still is kinda impossible to use UMD/AMD modules in conjunction with TS in Bun tests - the nature of UMD is that once the file is imported, it uses IIF to define stuff, but Bun does not tolerate this (I presume it only parses the AST of the imported file but does not actually execute it in the right order).

Hence Ace editor, which uses UMDs, can not really be used as intended.

## Bun's meat and potatoes

I did a bit of digging in Bun's source code and it seems... immature - commented out code, ignored tests, thousand-line-functions and files (`js_parser.zig` has `23.3k LOC`). And this is on top of using Zig, which is still at version `0.12` (as of writing of this post, `10 May 2024`) and has quite limited standard library (no `remove` and `find` methods in lists, no hash sets, etc.).

## Bottom line

My experience shows that Bun might fine to be used in new and low-risk projects, but it is not ready for a drop-in replacement in existing or more or less complex projects.
