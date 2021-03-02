---
layout: post
title: "libc.js: component interaction"
date: '2017-03-12T23:22:24+02:00'
---

<img data-src="/images/microverse-battery_optimized.webp" alt="">

# libc.js

Recently I've written a post about functional programming techniques, coming into the world of front-end and
the library I crafted as an experiment. That library, [libc.js](https://github.com/shybovycha/libc.js) was
highly inspired by [Elm](http://elm-lang.org/) and [Mithril](http://mithril.js.org). But it suffered two major features:

1. components were hardly able to be used in other components
2. the interaction between components was nearly impossible _(or, at least, not very transparent)_

What's hidden beneath the next version of the library?

<!--more-->

The next logical step in library development was to make the interaction between components smooth and
natural. I was primarily thinking of two options:

1. inheriting component from a `VirtualDOMNode` class
2. passing both properties and children as arguments to the `view` function of a component

I'll first describe the second approach a bit: since properties and children basically describe
a `VirtualDOMNode` itself, that meant to pass a `VirtualDOMNode` instance to the `view` function.
And if I did so, I'd get `Mithril.js`.

If I inherit a component class from a `VirtualDOMNode`, I'd step away from the initial purpose
of keeping `view` and `update` functions separated and pure.

The library exposes a `Store` class, which is very similar to [Redux](http://redux.js.org).
This class was also used internally to handle components' state changes. But that did not
solve the problem in any way.

I ended up creating a `Component` class, which encapsulated both `view` and
`update` functions, internal component state and the `dispatch` function, which operated on
the component's internal state. I also exposed the `render()` method, which could then be used
to bind a component to an external `Store` object _(which I'll cover in a minute)_.

# Re-using components

The changes made allowed components to be used in other components. To illustrate that, I
created a `Tab` component and the corresponding example:

```js
let Tabs = (function () {
    let update = (state, message) => {
        if (message.type == 'SELECT_TAB')
            return Object.assign({}, state, {
                currentTabIndex: message.tabIndex
            });

        return state;
    };

    let view = (state, children, dispatch) => {
        let currentTabIndex = state.currentTabIndex || 0;

        let tabHeaders = children.map((tab, tabIndex) => {
            return [ 'div', {
                    class: `tab-header ${tabIndex == currentTabIndex ? 'selected' : ''}`,
                    click: () => dispatch({
                        type: 'SELECT_TAB',
                        tabIndex
                    })
                },
                [ tab.children[0] ]
            ];
        });

        let tabs = children.map((tab, tabIndex) => {
            return [ 'div',
                { class: `tab-content ${tabIndex == currentTabIndex ? 'selected' : ''}` },
                tab.children.slice(1)
            ];
        });

        return [ 'div', [
            [ 'div', { class: 'tab-headers' }, tabHeaders ],
            [ 'div', { class: 'tab-container' }, tabs ]
        ]];
    };

    return createComponent(view, update);
})();

let app = (function () {
    let view = (state, children, dispatch) => {
        return [ Tabs, [
            ['div', [
                ['div', { class: 'header' }, 'Tab #1'],
                ['div', 'FIRST TAB CONTENT']
            ]],
            ['div', [
                ['div', { class: 'header' }, 'Tab #2'],
                ['div', 'SECOND TAB CONTENT']
            ]],
            ['div', [
                ['div', { class: 'header' }, 'Tab #3'],
                ['div', 'THIRD TAB CONTENT']
            ]],
        ] ];
    };

    return createComponent(view);
})();

app.init().mount(document.querySelector('#app'));
```

I also use these styles to make tabs look like tabs:

```css
.tab-container {
    display: flex;
    flex-direction: column;
}

.tab-headers {
    display: flex;
    justify-content: space-around;
}

.tab-header {
    text-align: center;
    cursor: pointer;
    flex-grow: 1;
}

.tab-header:hover {
    text-decoration: underline;
}

.tab-header.selected:hover {
    text-decoration: none;
}

.tab-header.selected {
    background: #ddd;
}

.tab-content {
    display: none;
}

.tab-content.selected {
    display: block;
}
```

This example shows how `Tabs` component could be used and, what's more important,
**as a High-Order Component**, passing tabs along with their headers as a set
of children to the `Tabs` component.

# Using external state

Using component's `render()` method and the `createStore(initialState)` function,
exposed by a library, we can also create and use the store as an external state
provider for our component:

```js
var counterStore = createStore(0);

function update(state, message) {
    if (message == 'INCREMENT')
        return state + 1;

    if (message == 'DECREMENT')
        return state - 1;

    return state;
}

counterStore.onAction(update);

function view(state) {
    let store = state.store;

    return ['div', [
        ['button', { click: () => store.dispatch('INCREMENT') }, 'Increment'],
        ['button', { click: () => store.dispatch('DECREMENT') }, 'Decrement'],
        ['div', `Count: ${ store.getState() }`]
    ]];
}

var Counter = createComponent(view);

counterStore.onStateChanged(() => Counter.render());

Counter.init({ store: counterStore }).mount(document.body);
```

Here you can see how to create a store with an initial state. The initial state could be pretty
much anything - a number, a string, an array, an object...

Using store's `onAction(handler)` and `onStateChanged(handler)` methods, we can set
a chain of reducers and a list of observers to state changes, correspondingly.

This example also shows how we can pass the initial state to a component's instance, using
the `init(state, children)` method of a `Component` instance _(in this example - `Counter.init()`)_.

In this example a few method signatures are also shown in action:

* `Component.init()` has two arguments: `initialState` and `children` and both are optional
* `createComponent()` has two arguments: `viewFn` and `updateFn` and, again, both are optional
* `viewFn` has three arguments: `state`, `children` and `dispatchFn`; last one is used to change component's internal state; second one is used for HOCs and will be handy to make configurations or to wrap the children with a markup or logic; first argument is just an internal component's state and, by the first call of the `viewFn` is equal to component's `initialState`, passed by `Component.init()` call
* `createStore(initialState)` is used to create a `Store` instance, where `initialState` is pretty much anything
* `Store.dispatch()` has exactly the same signature as the `dispatchFn`, used in `viewFn`

# Instead of wrap-up

I hope this library is a little bit more than just an experiment and once it will be used for a great good!
