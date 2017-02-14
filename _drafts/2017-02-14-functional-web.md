---
layout: post
title: Functional web
date: '2017-02-14T17:44:39+01:00'
---

In last couple of years the functional programming paradigm became very popular.
A huge amount of libraries, tools, tutorials and blog posts appeared.
Although the paradigm itself is rather old _(lambda calculus was developed around 1930 and the Lisp language was introduced in 1950)_, its popularity blowed up rapidly somewhere in 2014-2016 and that's what is happening right now.
Probably one of the most powerfull influencers, giving FP _(functional programming)_ that thrust is web development.
Since Facebook introduced **React**, the community started incorporating many things from FP with React - including **Redux** and **Immutable.js**.
But there are much more interesting things which were invented on this wave of FP popularity. One of them is [Elm](http://elm-lang.org/).

This is a story how I <s>implemented</s> invented yet another <s>web framework</s> wheel.

<!--more-->

## Elm

Elm is ML-like language, which compiles to JS, and provides developer with a runtime, allowing one to build web applications in a functional, strongly-typed language. Elm enforces its way of building web applications, where each web app is a combination of three entities:

1. **model**, which is basically just a structure of application state
2. **update function**, which takes current state and an event _(**action** in terms of Redux; **message** in terms of Elm; an object, representing user action or any other event)_ and provides the new value for app state _(model)_
3. **view function**, which converts current app state into a virtual DOM tree

Elm runtime handles everything else - passing messages to the `update` function, comparing the actual DOM tree with the one, provided by a `view` function and performing all DOM tree manipulations.

Elm can be compared to `React + Redux + ML-language` combination.

## React

The purpose of Facebook's React is to provide an interface to a virtual DOM, which allows users to track updates to the _application views_ and perform DOM operations to update views **with a minimal operation overhead**. In other words, we do not need to remove all the DOM nodes and create new ones from scratch when we can update a single property in a single DOM node.

## Redux

Redux is an ancestor of Facebook's Flux architecture and library. Flux' main goal was to standardize the way application stores its state and handles state updates:

* application state is stored in stores
* to update application state, we dispatch messages to corresponding stores
* to reflect application state changes, we subscribe on the store state changes

Than comes Redux and says _"hey! we probably can do that with just a single store!"_. And whilst Flux did not forbid state updates to modify the state objects themselves, Redux states clearly: _"you may not change state directly - rather you just calculate a new value for it and I take care of everything else"_.

## Mithril

Mithril is a tiny JS framework, whose main feature is virtual DOM. Just take a look at a sample application:

{% highlight js %}
var count$ = 0;

var App = {
    view: function () {
      return m('main', [
        m('button', { class: 'decrement', onclick: function () { count$--; } }, 'Decrement'),
        m('button', { class: 'increment', onclick: function () { count$++; } }, 'Increment'),
        m('p', {}, 'Counter: ' + count$),
      ]);
    }
};

m.mount(document.body, App);
{% endhighlight %}

## Cycle

Cycle is a reactive framework; kind of React + Rx. This means an application is basically a view function, subscribed to all the events allowed in your application. An example straight from [cycle](https://cycle.js.org/) website:

{% highlight js %}
var xs = xstream.default;
var { div, p, button, makeDOMDriver } = CycleDOM;

function App(sources) {
  var decrement$ = sources.DOM
    .select('.decrement')
    .events('click')
    .mapTo(-1);

  var increment$ = sources.DOM
    .select('.increment')
    .events('click')
    .mapTo(+1);

  var action$ = xs.merge(decrement$, increment$);
  var count$ = action$.fold(function (acc, v) { return acc + v; }, 0);

  var vtree$ = count$.map(function (count) {
    return div([
      button('.decrement', 'Decrement'),
      button('.increment', 'Increment'),
      p('Counter: ' + count)
    ])
  });

  return { DOM: vtree$ };
}

Cycle.run(App, { DOM: makeDOMDriver('body') });
{% endhighlight %}

Let me explain this source, since this might not be obvious.

First, Cycle _(or, actually, **RxJS**)_ operates on **event streams**. In this particular application there are two event streams: clicks on the "Increment" button and clicks on the "Decrement" button. So we first make all "click" events on the "Decrement" button (expressed by `sources.DOM.select('.decrement').events('click')`) to become just `-1` number (which is made by mapping each event to a value `-1`, `eventStream.map(-1)`). Then we do the similar thing to the clicks on the "Increment" button, but transform each event to `1` (positive) number.

Now, we merge both streams into a single one (thus both "Increment" and "Decrement" clicks will come from a single event stream, namely `action$`).

Then we transform our unified event stream `action$` into a stream of a single value by adding all the events (values) from `action$` stream.

And lastly, we transform our single-value stream `count$` into a virtual DOM tree and update the real dom, if needed.

### Event streams

Here's what should be explained regarding those event streams: a **stream** is a series of some values. That's it. Nothing more. When a value is added to a stream _(an event is fired)_, all the transformations are run as they are nothing but observers to the "on value appeared" event. Ans since all those transformations can be chained, we can imagine each transforming function is just a new event listener for that "on value appeared" event. And each transformation will provide us with a new stream, what allows us to add many different listeners to the same stream.

Here's a diagram, showing all the transformations:

<img src="{{ '/images/functional_web/CycleJS_example.png' | prepend: site.baseurl }}" alt="Event transformations diagram">

So once a click event _(on one of two buttons, of course)_ appears, it is transformed to either `-1` or `1`, depending on the button which fired that event; then this number is added to the existing _(which might appear to be empty)_ event stream `action$`; then the sum is calculated over all the numbers in that stream; then the sum is transformed into a virtual DOM tree; and lastly, the virtual DOM tree is returned from a view function and Cycle does its job to update the actual DOM. And since there always will be a different value of sum in our example, the DOM will always be updated.

## libc

My own implementation

