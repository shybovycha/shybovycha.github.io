---
title: 'TypeScript classes are not what you think'
date: '2023-06-05T15:15:00+10:00'
---

There was a talk at CPPCon 2017 by Matt Goldbolt with an inspirational title, [What has my compiler done for me lately?](https://www.youtube.com/watch?v=bSkpMdDe4g4).
It actually resonates with me quite a bit, especially in the world where we try to push so hard for statically typed compiled languages on front-end.

Recently I recalled one of (many) aspects why I think TypeScript is useless (as a way to introduce strong type system to JavaScript world) in many cases.

To be realistic and not just throw bare accusations around, this was inspired by the work on contracts for a new microservice, specifically - the request & response types to be used in both client and the back-end of the microservice. The types were similar to the types used on a database layer and one of the developers just returned objects of that DB layer model type on a controller (endpoint) level, which confused me.

Consider the code below:

```ts
class A {
    constructor(public moo: string, public foo: number) {}
}

class B {
    constructor(public moo: string, public foo: number, public zoo?: string[]) {}
}

const b: B = new B('ololo', -3.14, ['1']);
const a: A = b;
const c: B = a;
```

This works because of a (rather questionable) design decision by TypeScript team called [Type compatibility](https://www.typescriptlang.org/docs/handbook/type-compatibility.html), where any two classes or interfaces that have overlapping public fields are deemed compatible and can be mutually interplaceable.

However, in most languages with reasonable type system, you would expect two different classes to be just that - two different classes - in the case of a code above, an object of class `B` can never be assigned to a variable of type `A` and vice versa.

There is a way to achieve this in TypeScript, however (a bit cumbersome, though): by hiding the properties and only exposing them through non-getter/non-setter methods, when needed:

```ts
class A {
    constructor(private moo: string, private foo: number) {}

    getMoo() {
        return this.moo;
    }

    getFoo() {
        return this.foo;
    }
}

class B {
    constructor(private moo: string, private foo: number, private zoo?: ReadonlyArray<string>) {}

    getMoo() {
        return this.moo;
    }

    getFoo() {
        return this.foo;
    }

    getZoo() {
        return this.zoo;
    }
}

const b: B = new B('ololo', -3.14, ['1']);
const a: A = b; // Type 'B' is not assignable to type 'A'. Types have separate declarations of a private property 'moo'.(2322)
const c: B = a; // Property 'getZoo' is missing in type 'A' but required in type 'B'.(2741)
```

Without knowing this "feature" beforehand, one might end up with an inconsistent code or errors down the line (when somebody decides to modify the DB layer model and gets errors on an API layer). And TypeScript does not really help here.
