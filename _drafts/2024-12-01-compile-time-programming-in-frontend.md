---
layout: post
title: "Compile-time programming in frontend"
---

Modern-day frontend is a wild west - no rules, tons of gaps in application resilience, millions of tools for the same job each with its own massive flaws.

React and TypeScript are a de-facto standard in the world of frontend nowadays, pretty much. Choosing anything else is a wasted effort.

React 19 (or rather [React Compiler](https://react.dev/learn/react-compiler/)) promised a way to address some of the performance issues by moving some of the checks (like the use of memoization, which dependencies the memoized entities have, etc.) to the compile-time.

TypeScript has a rather powerful type system, but it only works at compile-time. It has no benefits at the runtime whatsoever.
And the language lacks features which actually improve developer's life.

I wonder what the likes of Next.JS, Angular, Svelte and React 19 have to offer from the perspective of compile-time optimizations and runtime improvements?
