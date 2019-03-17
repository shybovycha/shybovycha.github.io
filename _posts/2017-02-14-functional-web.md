---
layout: post
title: Functional web
date: '2017-02-18T17:44:39+01:00'
---

<img data-src="/images/functional_web/bicycle.jpg" class="img-responsive img-sm img-rounded pull-right">

In last couple of years the functional programming paradigm became very popular.
A huge amount of libraries, tools, tutorials and blog posts appeared.
Although the paradigm itself is rather old _(lambda calculus was developed around 1930 and the Lisp language was introduced in 1950)_, its popularity blew up rapidly somewhere in 2014-2016 and that's what is happening right now.
Probably one of the most powerful influencers, giving FP _(functional programming)_ that thrust is web development.
Since Facebook introduced **React**, the community started incorporating many things from FP with React - including **Redux** and **Immutable.js**.
But there are much more interesting things which were invented on this wave of FP popularity. One of them is [Elm](http://elm-lang.org/).

This is a story how I <s>implemented</s> invented yet another <s>web framework</s> wheel.

<!--more-->

## TL;DR

<style>
  .row {
    display: flex;
    flex-wrap: wrap;
  }

  .row .col {
    max-width: 48%;
    margin: 0 1%;
  }
</style>

<div class="row">
  <div class="col">
    <p><a href="https://facebook.github.io/react/" target="_blank">React</a></p>

    ```js
    class App extends React.Component {
      constructor(props) {
        super(props);
        this.state = { counter: 0 };
      }

      increment() {
        this.setState({ counter: this.state.counter + 1 });
      }

      decrement() {
        this.setState({ counter: this.state.counter - 1 });
      }

      render() {
        return (
          <div>
            <button onClick={() => this.increment()}>Increment</button>
            <button onClick={() => this.decrement()}>Decrement</button>
            <div>Counter: {this.state.counter}</div>
          </div>
        );
      }
    }

    ReactDOM.render(<App />, document.body);
    ```

    <p>
      <a href="http://codepen.io/shybovycha/pen/dNErOY" target="_blank" class="btn btn-md">Run this code</a>
    </p>
  </div>

  <div class="col">
    <p><a href="https://facebook.github.io/react/" target="_blank">React</a> + <a href="http://redux.js.org/" target="_blank">Redux</a></p>

    ```js
    function App(props) {
      var store = props.store;

      return (
        <div>
          <button onClick={() => store.dispatch({ type: 'INCREMENT' })}>Increment</button>
          <button onClick={() => store.dispatch({ type: 'DECREMENT' })}>Decrement</button>
          <div>Counter: {store.getState()}</div>
        </div>
      );
    }

    function update(state = 0, action) {
      switch (action.type) {
      case 'INCREMENT':
        return state + 1
      case 'DECREMENT':
        return state - 1
      default:
        return state
      }
    }

    var store = Redux.createStore(update);

    function render() {
      ReactDOM.render(<App store={store} />, document.body);
    }

    render();

    store.subscribe(render);
    ```

    <p>
      <a href="http://codepen.io/shybovycha/pen/RKmYVy" target="_blank" class="btn btn-md">Code</a>
    </p>
  </div>

  <div class="col">
    <p><a href="http://elm-lang.org" target="_blank">Elm</a></p>

    ```haskell
    import Html exposing (beginnerProgram, div, button, text)
    import Html.Events exposing (onClick)


    initialState = 0


    view state =
      div []
        [ button [ onClick Increment ] [ text "Increment" ]
        , button [ onClick Decrement ] [ text "Decrement" ]
        , div [] [ text ("Counter:" ++ (toString state)) ]
        ]


    type Msg = Increment | Decrement


    update msg state =
      case msg of
        Increment ->
          state + 1

        Decrement ->
          state - 1


    main =
      beginnerProgram { model = initialState, view = view, update = update }
    ```

    <p>
      <a href="http://codepen.io/shybovycha/pen/egaLXv" target="_blank" class="btn btn-md">Compiled code</a>
    </p>
  </div>

  <div class="col">
    <p><a href="http://mithril.js.org" target="_blank">Mithril</a></p>

    ```js
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
    ```

    <p>
      <a href="http://codepen.io/shybovycha/pen/WRBgWv" target="_blank" class="btn btn-small">Code</a>
    </p>
  </div>

  <div class="col">
    <p><a href="https://cycle.js.org" target="_blank">Cycle</a></p>

    ```js
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
    ```

    <p>
      <a href="http://codepen.io/shybovycha/pen/XpOQvx" target="_blank" class="btn btn-small">Code</a>
    </p>
  </div>

  <div class="col">
    <p><a href="https://github.com/shybovycha/libc.js" target="_blank">libc</a></p>

    ```js
    var initialState = 0;

    function update(state, message) {
      if (message == 'INCREMENT')
        return state + 1;

      if (message == 'DECREMENT')
        return state - 1;

      return state;
    };

    function view(state, dispatch) {
      return ['div', [
        ['button', { click: () => dispatch('INCREMENT') }, 'Increment'],
        ['button', { click: () => dispatch('DECREMENT') }, 'Decrement'],
        ['div', `Count: ${ state }`]
      ]];
    };

    var app = createApplication(initialState, update, view);

    app.mount(document.body);
    ```

    <p>
      <a href="http://codepen.io/shybovycha/pen/dNEgNa" target="_blank" class="btn btn-md">Code</a>
    </p>
  </div>
</div>

## React + Redux

Let's start-off by writing a simple "counter" application with the well-known React + Redux bundle.

### React

The purpose of Facebook's React is to provide an interface to a virtual DOM, which allows users to track updates to the _application views_ and perform DOM operations to update views **with a minimal operation overhead**. In other words, we do not need to remove all the DOM nodes and create new ones from scratch when we can update a single property in a single DOM node. It might look like this:

```js
class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = 0;
  }

  increment() {
    this.setState(this.state + 1);
  }

  decrement() {
    this.setState(this.state - 1);
  }

  return (
    <div>
      <button onClick={increment}>Increment</button>
      <button onClick={decrement}>Decrement</button>
      <div>Counter: {this.state}</div>
    </div>
  );
}

ReactDOM.render(<App />, document.body);
```

<a href="http://codepen.io/shybovycha/pen/dNErOY" target="_blank" class="btn btn-md">Run this code</a>

### Redux

Redux is an ancestor of Facebook's Flux architecture and library. Flux' main goal was to standardize the way application stores its state and handles state updates:

* application state is stored in stores
* to update application state, we dispatch messages to corresponding stores
* to reflect application state changes, we subscribe on the store state changes

Than comes Redux and says _"hey! we probably can do that with just a single store!"_. And whilst Flux did not forbid state updates to modify the state objects themselves, Redux states clearly: _"you may not change state directly - rather you just calculate a new value for it and I take care of everything else"_.

```js
function App(props) {
  var store = props.store;

  return (
    <div>
      <button onClick={() => store.dispatch({ type: 'INCREMENT' })}>Increment</button>
      <button onClick={() => store.dispatch({ type: 'DECREMENT' })}>Decrement</button>
      <div>Counter: {store.getState()}</div>
    </div>
  );
}

function update(state = 0, action) {
  switch (action.type) {
  case 'INCREMENT':
    return state + 1
  case 'DECREMENT':
    return state - 1
  default:
    return state
  }
}

var store = Redux.createStore(update);

function render() {
  ReactDOM.render(<App store={store} />, document.body);
}

render();

store.subscribe(render);
```

<a href="http://codepen.io/shybovycha/pen/RKmYVy" target="_blank" class="btn btn-md">Run this code</a>

## Elm

Elm is ML-like _(Haskell-like)_ language, which compiles to JS, and provides developer with a runtime, allowing one to build web applications in a functional, strongly-typed language. Elm enforces its way of building web applications, where each web app is a combination of three entities:

1. **model**, which is basically just a structure of application state
2. **update function**, which takes current state and an event _(**action** in terms of Redux; **message** in terms of Elm; an object, representing user action or any other event)_ and provides the new value for app state _(model)_
3. **view function**, which converts current app state into a virtual DOM tree

<img src="{{ '/images/functional_web/elm_architecture.png' | prepend: site.baseurl }}" alt="Elm workflow">

Elm runtime handles everything else - passing messages to the `update` function, comparing the actual DOM tree with the one, provided by a `view` function and performing all DOM tree manipulations.

Elm can be compared to `React + Redux + ML-language` combination.

Let's take a look at our sample application, written in Elm:

```haskell
import Html exposing (beginnerProgram, div, button, text)
import Html.Events exposing (onClick)


initialState = 0


view state =
  div []
    [ button [ onClick Increment ] [ text "Increment" ]
    , button [ onClick Decrement ] [ text "Decrement" ]
    , div [] [ text ("Counter:" ++ (toString state)) ]
    ]


type Msg = Increment | Decrement


update msg state =
  case msg of
    Increment ->
      state + 1

    Decrement ->
      state - 1


main =
  beginnerProgram { model = initialState, view = view, update = update }
```

<a href="http://codepen.io/shybovycha/pen/egaLXv" target="_blank" class="btn btn-md">Run compiled version of this code</a>

In this example, we first define initial application state, which is just integer number of zero.

Then we describe the view of our application, how the application will display the current state in HTML. This is the battle field of a `view` function.

After that we define possible actions _(or messages in terms of Elm)_. It would be either `Increment` or `Decrement` action with no additional parameters.

Right after that we define how each message will affect application state. Since our application' state is only an integer number, the `update` function will return the new state given the current state value and an action.

Lastly we create our application, providing Elm with all the entities we defined above.

I found this code to be more clear, than in case with React + Redux, since in case we'd like to write something more complex, we don't need to use tons of mapping/reducing/selecting functions, split into a huge bunch of files. I'll cover more complex examples later on.

## Mithril

Mithril is a tiny JS framework, whose main feature is virtual DOM. Just take a look at a sample application:

```js
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
```

<a href="http://codepen.io/shybovycha/pen/WRBgWv" target="_blank" class="btn btn-small">Run this code</a>

This is much less code than in case with React + Redux, isn't it?

## Cycle

Cycle is a reactive framework; kind of React + Rx. This means an application is basically a view function, subscribed to all the events allowed in your application. An example straight from [cycle](https://cycle.js.org/) website:

```js
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
```

<a href="http://codepen.io/shybovycha/pen/XpOQvx" target="_blank" class="btn btn-small">Run this code</a>

Let me explain this source, since this might not be obvious.

First, Cycle _(or, actually, **RxJS**)_ operates on **event streams**. In this particular application there are two event streams: clicks on the "Increment" button and clicks on the "Decrement" button. So we first make all "click" events on the "Decrement" button (expressed by `sources.DOM.select('.decrement').events('click')`) to become just `-1` number (which is made by mapping each event to a value `-1`, `eventStream.map(-1)`). Then we do the similar thing to the clicks on the "Increment" button, but transform each event to `1` (positive) number.

Now, we merge both streams into a single one (thus both "Increment" and "Decrement" clicks will come from a single event stream, namely `action$`).

Then we transform our unified event stream `action$` into a stream of a single value by adding all the events (values) from `action$` stream.

And lastly, we transform our single-value stream `count$` into a virtual DOM tree and update the real dom, if needed.

### Event streams

Here's what should be explained regarding those event streams: a **stream** is a series of some values. That's it. Nothing more. When a value is added to a stream _(an event is fired)_, all the transformations are run as they are nothing but observers to the "on value appeared" event. Ans since all those transformations can be chained, we can imagine each transforming function is just a new event listener for that "on value appeared" event. And each transformation will provide us with a new stream, what allows us to add many different listeners to the same stream.

Here's a diagram, showing all the transformations:

<img src="{{ '/images/functional_web/CycleJS_example-compressed.png' | prepend: site.baseurl }}" alt="Event transformations diagram">

So once a click event _(on one of two buttons, of course)_ appears, it is transformed to either `-1` or `1`, depending on the button which fired that event; then this number is added to the existing _(which might appear to be empty)_ event stream `action$`; then the sum is calculated over all the numbers in that stream; then the sum is transformed into a virtual DOM tree; and lastly, the virtual DOM tree is returned from a view function and Cycle does its job to update the actual DOM. And since there always will be a different value of sum in our example, the DOM will always be updated.

## libc

So far I've covered four different implementations of a very simple yet *interactive* web application. Some of them require a whole bunch of libraries to be injected into a page (in case with React+Redux+whatever, Cycle), some of them introduce a whole new approach to create interactive applications (Cycle, Elm, Mithril). Based on that, I created my own micro library, which inherits most interesting features of frameworks discussed above.

I called it [libc](https://github.com/shybovycha/libc.js).

The same "counter" application, written in libc looks like this:

```js
var initialState = 0;

function update(state, message) {
  if (message == 'INCREMENT')
    return state + 1;

  if (message == 'DECREMENT')
    return state - 1;

  return state;
};

function view(state, dispatch) {
  return ['div', [
    ['button', { click: () => dispatch('INCREMENT') }, 'Increment'],
    ['button', { click: () => dispatch('DECREMENT') }, 'Decrement'],
    ['div', `Count: ${ state }`]
  ]];
};

var app = createApplication(initialState, update, view);

app.mount(document.body);
```

<a href="http://codepen.io/shybovycha/pen/dNEgNa" target="_blank" class="btn btn-md">Run this code</a>

All the DOM nodes are created using the array returned, which should match pattern `[tag, {attributes}, (text | [children]))` _(just like Mithril uses `m()` function or Elm uses `div` and other helper functions)_. Originally, `libc` used `c()` helper function to do that _(similar to Mithril's `m()` function)_, and that's why it was called `libC`.

The library makes use of a VirtualDOM approach to create a virtual DOM tree and chech which nodes/node properties need changes and reflect only the needed changes in a real DOM tree.

`libc` uses the `message -> update -> view` chain, just like Elm does. The inner application state is handled similarly to Redux - application has only one store, which contains application state. And the `update()` function just calculates the new value for a state, without actually assigning it. Library then checks whether state needs to be updated or not _(using `Object.deepEqual` method, exposed by a library)_. If the new state value is different from the existing one - the state is replaced with a new value and the `view()` function is being called with a new state value. `view` function returns the new value of a virtual DOM tree. Library then checks whether it needs to update actual DOM tree or not.

There are however couple highlights regarding `libc`:

* it can create arbitrary tags _(DOM nodes)_ within the `view()` function _(the first element in each array is tag name, which could be pretty much anything)_, so it's quite easy to use it in couple with other libraries _(such as Angular-material, React or Polymer, for instance)_
* properties, whose value is a function are automatically mapped to event listeners; taking into account that property names could be arbitrary as well, you may add event listeners to your custom events
* again, property names could be arbitrary, so you can use even angular ones if you want: `['div', { 'ng-repeat': 'item in items' }, '{{item.name}}']`
* both `properties` and `children or text` arguments are optional; thus you can create empty DOM elements: `['hr']`
* to enable communication with your applications from the outside, application instance, returned by the `createApplication()` function, has the `dispatch(message)` method

In later posts I'll provide more examples of using `libc`.
