---
layout: post
title: 'Multi page/component web application'
categories: []
tags: []
published: True

---

Some time ago I wrote a post about web components and how we can compose our web pages of custom
pieces, encapsulating their logic and/or styles. Yeah, the idea of page split is great, but
usually we wouldn't have a one-page application with all the controls in place. It's more natural
to divide website into pages, pages into control groups. But how to achieve the effect of transition
between pages using web-components? Do we need to create a separate file for each page? Let's see...

<!--more-->

In my [previous post]({% post_url 2015-07-12-reactjs-introduction %}) I covered React, Riot,
Angular 2.0 and Polymer. Now the time has passed and Angular 1.5 is here. So I decided to
extend my knowledge and fill the gaps in a brand-new methodology.

Let's start with a very simple application, which makes use of web components and provides navigation
between multiple pages.

## Angular 1.4

`[ng-app] > [ng-view]` + `ngRoute + $routeProvider + when('url', { controller: MyCtrl })`

## Angular 1.5

`[ng-app] > [ng-view]` + `ngRoute + $routeProvider + when('url', { template: '<my-page-component></my-page-component>' })`

## New Angular Router

https://github.com/angular/router/blob/master/examples/angular-1/hello/app.js

`$router.config([{ url: '/url', component: 'my-page-component' }])`

## Transclusion

Discuss, what is transclusion and what can be done with it. And why the hell couldn't we use it for routing.

## Riot

`riot.route('/url', function () { currentPage.unmount(true); currentPage = riot.mount('element#selector', 'my-page-component')[0]; })`

## Bonus: CSS encapsulation

http://plnkr.co/edit/Un94LQWwNL2oJmeeXaEC?p=info

Riot with `scoped` CSS!

<!--To proceed, we need some example or input data. Let's assume for a moment we want to create a webshop.
We'll need one page, listing products; one page, displaying product details and one page for checkout.
I even got images of those pages, from [my post about E2E testing]({% post_url 2015-11-26-end-to-end-testing-with-webdriverio %}).

First things first: we need to think, how can we construct each page? What would be the components?
What hierarchy will
-->