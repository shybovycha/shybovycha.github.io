---
layout: post
title: Two sides of web application. Part 1
date: '2015-09-20T20:18:39+01:00'
---

## Prologue

How do we usually create a web application? We run a bootstraping script, which provides
us with a skeleton of our application and then just extend it with the features we need.

So did we at the last hackathon we were attending - we started with `rails new twf` and
spent half of the day integrating our blank app with Angular, Paperclip, creating API methods
and so on. But the effort we needed to accomplish our goal (quite simple web app) was really huge.

So I decided to find the best combination of backend and frontend technologies to cause
less pain.

At the project I was lastly introduced into, the line between frontend and back-end is
distinguished very clearly: we have an API, written in Clojure and thin frontend application,
made with Angular and working on a generated set of static assets - HTMLs, CSS and JS files
*(but under the hood we are using HAML and SCSS)*.

The application I will be implementing throughout the whole article has the same architecture:
it has RESTful API and MVVM on the frontend, made with Angular. You are welcome to the journey
of research and new technologies!

<!--more-->

## Why not go with Rails?

Because Rails is oftenly being overused. Especially if you install all of frontend libraries
*(like Angular, Bootstrap, some Angular plugins, etc.)* as RubyGems. Frontend should stay
on the front end of the application; you should not tight your application at some precise
version of the JS script, provided by a gem and rely on author's way to integrate it with
Rails.

In our case, Rails is too heavy - when all you need is database + routing + a couple of
lightweight controllers, all Rails' features will become a ballast to our app, which
must be small, by design.

## The goal

Before we start, let's think of what we will be creating. Will it be a web shop? Or a blog?
No, we need something outstanding! Something we were never doing before...

After a hour of imaging what it may be, I decided to go with web analytics tool. A prototype,
which will be able to tell how many visitors did your web application gained lastly.

It should not be too complicated, because, you know, it's just a tutorial... So we will be
tracking users' location and browser only. And we will be showing statistics on a chart
*(values like total visitors, browser usage)* and in table *(the same info as on chart)*.

## General architecture

First thing we need to think of is how we will be gathering the information about users.
It's quite easy - we just need to get a request from visitor. Of any kind -
it may be a request to get an image, a file, a stylesheet or a script.

Then we will just parse headers from that request and save the extracted data in the
database. The only problem here is: *how to get unique key from each request?*. We may use
visitor's IP address. It's the easiest way.

Now, let's decide what pages our application will have and how will they look like.

<div class="row">
    <div class="col-xs-12 col-md-3">
        <a href="{{ '/images/two-sides-of-web-application/part1/screen1.png' | prepend:site.baseurl }}">
            <img src="{{ '/images/two-sides-of-web-application/part1/screen1.png' | prepend:site.baseurl }}" alt="page 1" class="image-responsive">
        </a>
    </div>

    <div class="col-xs-12 col-md-3">
        <a href="{{ '/images/two-sides-of-web-application/part1/screen2.png' | prepend:site.baseurl }}">
            <img src="{{ '/images/two-sides-of-web-application/part1/screen2.png' | prepend:site.baseurl }}" alt="page 2" class="image-responsive">
        </a>
    </div>

    <div class="col-xs-12 col-md-3">
        <a href="{{ '/images/two-sides-of-web-application/part1/screen3.png' | prepend:site.baseurl }}">
            <img src="{{ '/images/two-sides-of-web-application/part1/screen3.png' | prepend:site.baseurl }}" alt="page 3" class="image-responsive">
        </a>
    </div>

    <div class="col-xs-12 col-md-3">
        <a href="{{ '/images/two-sides-of-web-application/part1/screen4.png' | prepend:site.baseurl }}">
            <img src="{{ '/images/two-sides-of-web-application/part1/screen4.png' | prepend:site.baseurl }}" alt="page 4" class="image-responsive">
        </a>
    </div>
</div>

## Build with the right tools!

Now, since we separated our frontend part of application from back-end part, we may use different preprocessing
languages to write stylesheets and views. And even controllers! So let's take the most from 2015 and use the newest
tool set: *Jade*, *ES6+* and *SCSS*. And put all them together with *Bower* and *Gulp*.

All those Jade, SCSS and ES6 are not supported by a browser out-of-the box. They must be compiled to HTML, CSS and JS in order to be recognized by a browser. But they are here to help you writing code quickly. I listed some of their key features below.

**Jade** is a template rendering engine with its own markup language. It is somehow similar to Haml and Slim - it
nests XHTML nodes with indentation, closes tags automatically... But it is especially good at writing complex web
pages, consisting of *layouts* and *partials*.

*Layouts* are big templates, containing placeholders, where
concrete partials will be placed. So, for example, you may create a separate layout for your webshop' landing
page, account page and shopping cart. They will be different. And all of them will use different sets of partials.
But, for example, a footer and a quick shopping cart preview or user account widget (the one with a link to user's
account page) will be the same. To prevent duplicating those widgets' code on each of the layouts, we extract them
to a separate files, called *partials* and then just make a reference (a placeholder) in our layouts, saying
*"place that partial's content here"*.

In case with Jade, we may override or extend existing partials in a layout, without touching partial' file itself.
So, for example, if we want to make user's avatar to be shown in a user account widget only on a product page, we just override user widget partial on a product page, removing the part with avatar.

**SCSS** is a way to simplify writing CSS. It is so simple, but so powerful, that you will fall in love with it
after first couple of stylesheets! See, in CSS when you write a long selector, specifying many parents, you may
find your stylesheets ugly and huge, when describing different children of one, deeply nested parent.

So, let's say you are having a user widget. And it may be placed both on page' header, footer and sidebar. But
the avatar image will look differently on each of those - it must be smaller in header and footer. So you
start writing selectors like `.sidebar .user-widget .avatar img` and `.header .user-widget .avatar img`.
That's painful, but not that much, if you have just a couple of those. But as your website grows, you
start getting really, really upset about that.

And here's where SCSS is just a revelation: its great power is in its *selectors*, *variables* and *mixins*.

*Selectors* allow you to describe selector' nesting just as you'd be writing C code:

{% highlight scss %}
.header {
    // header styles

    .user-widget {
        // header user-widget styles

        .avatar {
            // maybe something differs in avatar itself?

            img {
                // ah! here's it!
            }
        }
    }
}
{% endhighlight %}

*Variables* allow you to get rid of all those magic values. So if you use one color many times across your styles,
you just extract it into a named constant and use the pretty named value everywhere!

*Mixins* allow even more: you may extract the parts of the styles into a named and even **parametrized** function!

Relating on all those, you may re-write your user widget as follows:

{% highlight scss %}
$header-avatar-size: 50px;
$sidebar-avatar-size: 150px;

@mixin avatar($avatar_size) {
    max-width: $avatar_size;
    max-height: $avatar_size;
}

.header {
    .user-widget .avatar {
        @include avatar($header-avatar-size);
    }
}

.sidebar {
    .user-widget .avatar {
        @include avatar($sidebar-avatar-size);
    }
}
{% endhighlight %}

I found Gulp to be super-easy for tasks like compiling stylesheets, views and javascripts. But before
we continue with Gulp, let's initialize an NPM project with `npm init` and install the plugins required:

{% highlight bash %}
npm install -g gulp
{% endhighlight %}

And Gulp plugins:

{% highlight bash %}
npm install --save-dev gulp gulp-babel gulp-scss gulp-jade
{% endhighlight %}

I will describe how Gulp works and how we can use it in our project in a minute. For now, let's
just install the frontend dependencies. Let's make them use fixed versions, so when we update our project,
nothing gets broken. To make our development quick, we'll be using *Twitter Bootstrap* and manage all
frontend dependencies with *Bower*. Bower will fill out the `bower.json`, a file, telling Bower
which libraries to use, automatically:

{% highlight bash %}
npm install -g bower
bower init
bower install --save bootstrap angular
{% endhighlight %}

These commands create a directory `bower_components`, containing all the dependencies installed, each in its own sub-directory. With that in mind, we will be referencing our frontend dependencies, relatively to their catalogs within the `bower_components` directory.

Now let's write a build task for Gulp. Gulp is a streamed build tool. That means, that each operation you perform, passes its result to another operation as the input argument. So, for example, if you run `gulp.src('src/styles/*.scss')`, it will return you an object with the list of all the SCSS files and the magic `pipe()` method. And when you call the `gulp.src(...).pipe(scss())`, Gulp will pass that list to the SCSS compiler plugin, so you will get a compiled CSS code. That is, not a CSS file itself, but a compressed, merged, CSS file' content.

And that describes the second important feature of Gulp: it does not store the intermediate operation results. It is almost like a functional programming - you just have the input data. Then you call a chain of functions on it,
passing the result of one function call to the next function as its input. Same happens here, but in not that
strict manner - since we are using Javascript, we can store the intermediate results in a memory. But to store
the results in the files, we should pass them to the `gulp.dest(...)` function. Depending on function, looking
to store its results, `gulp.dest()` may point to a directory or a single file, where the results will be stored.

Below is our first Gulp task. Write this code in the `gulpfile.js`:

{% highlight js %}
var gulp = require('gulp'),
    sass = require('gulp-scss'),
    babel = require('gulp-babel'),
    jade = require('gulp-jade');

gulp.task('build', function () {
    gulp.src('src/views/**/*.jade')
        .pipe(jade())
        .pipe(gulp.dest('public/'));

    gulp.src('src/javascripts/**/*.js')
        .pipe(babel())
        .pipe(gulp.dest('public/javascripts'));

    gulp.src('src/stylesheets/**/*.scss')
        .pipe(sass({ style: 'expanded' }))
        .pipe(gulp.dest('public/stylesheets'));
});
{% endhighlight %}

This tells Gulp to define `build` task, which compiles all the Jade, SCSS and ES6+ files into HTML, CSS
and JS files, correspondingly. Compiled files are placed within the `public/` directory, so we may
easily render them with almost any web-server. But first things first, we need to prepare directory structure
like this for our task:

{% highlight bash %}
.
|____bower.json
|____gulpfile.js
|____package.json
|____public
|____src
| |____javascripts
| |____stylesheets
| |____views
{% endhighlight %}

This may seem odd to the paragraph, dedicated to build tools, but let's check how our tasks work.
To do this, we need to write some test files to check our `build` task. So let's create one of each kind:

*`src/views/index.jade`:*

{% highlight jade %}
html(lang="en")
head
    meta(charset="UTF-8")
    title OurStats
    link(rel="stylesheet" href="/stylesheets/main.css")
    script(type="text/javascript" src="/javascripts/main.js")
body
    h1 Hello, world!
{% endhighlight %}

*`src/stylesheets/main.scss`:*

{% highlight scss %}
h1 {
    padding: 0 10px;
    border-bottom: 1px solid #dedede;
}
{% endhighlight %}

*`src/javascripts/main.js`:*

{% highlight js %}
function timeout(ms) {
  return new Promise((res) => setTimeout(res, ms));
}

async function f() {
  console.log(1);
  await timeout(1000);
  console.log(2);
  await timeout(1000);
  console.log(3);
}

f()
{% endhighlight %}

<a href="https://github.com/shybovycha/two-sides-of-web-application/tree/10f770a5ca64fad1cb6e58bc957e5210bc95bda3" class="btn btn-info">Full code of these steps</a>

And to actually check our task, we need to run it with

{% highlight bash %}
gulp build
{% endhighlight %}

Now, you may open the HTML generated from Jade in a browser, but it will look ugly, because your
browser will doubtedly find stylesheets and javascripts that simply. To make the magic happen, we
will use another Gulp plugin, `gulp-server-livereload`. Generally, the development process with
different build tools looks very similar nowadays: you set up the environment, find the plugins
you need, install and configure them - and vióla!

I've chosen that server plugin because it comes with one handy feature: it automatically reloads
the opened pages in your web-browser if any of the files you are browsing has changed. Here's
the code of our serving task:

{% highlight js %}
gulp.task('serve', function () {
    gulp.src('public/')
        .pipe(server({
            livereload: true,
            directoryListing: false,
            open: true
        }));
});
{% endhighlight %}

But if you remember, we are compiling our sources from SCSS/Jade into the `public/` directory.
So how the server would know if anything changed in our `*.scss` or `*.jade` files? We need to
reload the page in web-browser if anything changes in those files. To do that, we will write one
more task, which will re-build our source files if anything changes:

{% highlight js %}
gulp.task('watch', function () {
    gulp.watch('src/**/*', [ 'build' ])
});
{% endhighlight %}

Here we used two new features of Gulp: *watching for file changes* and *running existing tasks from another task*.
Simple? Yeah, **that** simple! So we just tell Gulp: *keep an eye on those files - if anything happens - run
those tasks immediately!* - and the magic happens.

But why should we run two tasks? Let's merge them into one so we just run `gulp serve` and get both live reload
and live re-compilation:

{% highlight js %}
gulp.task('serve', function () {
    gulp.watch('src/**/*', [ 'build' ])

    gulp.src('public/')
        .pipe(server({
            port: 3000,
            livereload: true,
            directoryListing: false,
            open: true
        }));
});
{% endhighlight %}

This task first defines a watcher for our sources and then starts server, which will react to any changes
in the `public/` directory. To check how this awesomeness works, start `gulp serve`, open the
[`localhost:3000/test.html`](localhost:3000/test.html) page and then change, for example,
color for the `h1` tag to green. Save the SCSS file and just switch to the browser window, **do not reload
it manually**.

<a href="https://github.com/shybovycha/two-sides-of-web-application/tree/07f6cda21da377719861f41f89064a414e808268" class="btn btn-info">Full code of these steps</a>

## Templates

Let's create a set of pages, we described above. They will be used later to display real data, but for now
we will stub the real data with constants.

To make our prototyping smooth and fast, we will use *Twitter Bootstrap*. We've installed it already with
*Bower*, so we will be just using it. We need to implement these pages only:

* landing page
* sign up / sign in page
* account settings page
* apps list page
* application page
* application create / edit page

Six pages, huh? Let's do it quick:

*`src/views/layouts/default.jade`:*

{% highlight jade %}
doctype html
html(lang="en")
    head
        block head
            title OurStats
            link(rel="stylesheet" href="bootstrap/dist/css/bootstrap.css")
            link(rel="stylesheet" href="bootstrap/dist/css/bootstrap-theme.css")
            script(src="bootstrap/dist/js/bootstrap.js")

    body
        block body
{% endhighlight %}

*`src/views/index.jade`:*

{% highlight jade %}
extends layouts/default.jade

block content
    .jumbotron
        h1.display-3 Welcome to OurStats!
        p.lead This is a simple stats application
        hr.m-y-md

        p.lead
            .row
                .col-xs-12.text-center
                    a.btn.btn-primary.btn-lg(href="#" role="button") Sign in
{% endhighlight %}

*`src/views/new-session.jade`:*

{% highlight jade %}
extends layouts/default.jade

block content
    .row
        .col-xs-12.col-md-4.col-md-offset-1.well
            h3 Sign up
            form
                fieldset.form-group
                    input.form-control(type="text" placeholder="Your name")
                fieldset.form-group
                    input.form-control(type="email" placeholder="Email")
                fieldset.form-group
                    input.form-control(type="password" placeholder="Password")
                fieldset.form-group
                    input.form-control(type="password" placeholder="Password confirmation")
                fieldset.form-group.text-center
                    button.btn.btn-primary(type="submit") Sign up

        .col-md-2

        .col-xs-12.col-md-4.well
            h3 Sign in
            form
                fieldset.form-group
                    input.form-control(type="email" placeholder="Email")
                fieldset.form-group
                    input.form-control(type="password" placeholder="Password")
                fieldset.form-group.text-center
                    button.btn.btn-success(type="submit") Sign in
{% endhighlight %}

*`src/views/edit_account.jade`:*

{% highlight jade %}
extends layouts/default.jade

block content
    .row.m-t-md
        .col-xs-12.col-md-4
            h3 Account settings
            form
                fieldset.form-group
                    input.form-control(type="text" placeholder="Your name")
                fieldset.form-group
                    input.form-control(type="password" placeholder="Password")
                fieldset.form-group
                    input.form-control(type="password" placeholder="Password confirmation")
                fieldset.form-group.text-center
                    button.btn.btn-success(type="submit") Save
{% endhighlight %}

*`src/views/application_list.jade`:*

{% highlight jade %}
extends layouts/default.jade

block content
    .row.m-t-md
        .col-xs-12.col-md-4
            h3 Your applications
            ul
                li
                    a(href="/application_details.html") Test application #1
                li
                    a(href="/application_details.html") Test application #2
                li
                    a(href="/application_details.html") Test application #3
                li
                    a(href="/application_details.html") Test application #4
                li
                    a(href="/application_details.html") Test application #5

        .col-md-offset-2

        .col-xs-12.col-md-6
            h3 Application stats

            h4 New users:
            .progress
                progress.progress-bar.progress-bar-success(role="progress" style="width:30%")
{% endhighlight %}

*`src/views/application_details.jade`:*

{% highlight jade %}
extends layouts/default.jade

block content
    .row
        .col-xs-12.col-md-6.col-md-offset-6
            form.form-inline
                .form-group
                    input.form-control(type="text" placeholder="From date")
                .form-group
                    input.form-control(type="text" placeholder="To date")
                .form-group
                    .btn.btn-primary Update

    .row
        .col-xs-12
            .well
                | You can track your application visitors with this URL:
                br
                | http://localhost:3000/track/APP1TOKN

    .row.m-t-md
        .col-xs-12
            h3 Visitors
            .well
                | Chart goes here

        .col-xs-12.col-md-6
            h3 Visitors by country

            table.table.table-bordered
                thead
                    tr
                        th Country
                        th Visitors
                tbody
                    tr
                        td USA
                        td 100
                    tr
                        td Germany
                        td 50
                    tr
                        td North Korea
                        td 42
                    tr
                        td Greenland
                        td 39
                    tr
                        td Spain
                        td 2
{% endhighlight %}

*`src/views/edit_application.jade`:*

{% highlight jade %}
extends layouts/default.jade

block content
    .row.m-t-md
        .col-xs-12.col-md-6
            h3 Application settings
            form
                fieldset.form-group
                    input.form-control(type="text" placeholder="Name")
                fieldset.form-group.text-center
                    .btn.btn-danger Reset stats
                fieldset.form-group.text-center
                    button.btn.btn-success(type="submit") Save
{% endhighlight %}

*`src/views/new_application.jade`:*

{% highlight jade %}
extends layouts/default.jade

block content
    .row.m-t-md
        .col-xs-12.col-md-6
            h3 New application
            form
                fieldset.form-group
                    input.form-control(type="text" placeholder="Name")
                fieldset.form-group.text-center
                    button.btn.btn-success(type="submit") Create
{% endhighlight %}

And to make Bower-managed libraries available in our views, we need to add one more path to
the server configuration:

{% highlight js %}
gulp.src(['public/', 'bower_components/'])
    .pipe(server({
        port: 3000,
        livereload: true,
        directoryListing: false,
        open: true
    }));
{% endhighlight %}

As you can see, we used Jade's block extending and split our templates into one layout and many partials, so
our file tree is clean and changing any of the pages will not be a hard task.

<a href="https://github.com/shybovycha/two-sides-of-web-application/tree/628f216b8e7d904e040035b31c3d0c546d8f1e5e" class="btn btn-info">Full code of these steps</a>

## Architecture details

In our frontend application we will use something called *MVVM*. That is a design pattern, kindly provided
by Angular. So our *views* will be displaying data and transferring it to controllers *(or ViewModels)*, and
all the logic, handling that data will be defined in *controllers* and *services*, representing *Models*.
Actually, our models will be handled on a server-side, and services will only provide an interface to them.
But that is totally another story and will be described later.

For now let's integrate Angular in our application. And we will start, deciding how we will split our
application on the Angular layer. Pages we described above are used for these actions:

<table class="table table-bordered">
    <thead>
        <tr>
            <th>Action</th>
            <th>Resource <em>(model)</em></th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>sign in</td>
            <td rowspan="2">Session</td>
        </tr>
        <tr>
            <td>sign out</td>
        </tr>
        <tr>
            <td>sign up</td>
            <td rowspan="3">User</td>
        </tr>
        <tr>
            <td>edit profile</td>
        </tr>
        <tr>
            <td>update profile</td>
        </tr>
        <tr>
            <td>add application</td>
            <td rowspan="5">Application</td>
        </tr>
        <tr>
            <td>edit application</td>
        </tr>
        <tr>
            <td>update application</td>
        </tr>
        <tr>
            <td>remove application</td>
        </tr>
        <tr>
            <td>show application</td>
        </tr>
        <tr>
            <td>track visitor</td>
            <td>Visitor</td>
        </tr>
    </tbody>
</table>

And so we can define corresponding Angular controllers:

<table class="table table-bordered">
    <thead>
        <tr>
            <th>Url</th>
            <th>Controller</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>/</td>
            <td>LandingPage</td>
        </tr>
        <tr>
            <td>/new-session</td>
            <td>Session</td>
        </tr>
        <tr>
            <td>/applications</td>
            <td>ListApplications</td>
        </tr>
        <tr>
            <td>/applications/new</td>
            <td>NewApplication</td>
        </tr>
        <tr>
            <td>/applications/:id</td>
            <td>ShowApplication</td>
        </tr>
        <tr>
            <td>/applications/:id/edit</td>
            <td>EditApplication</td>
        </tr>
        <tr>
            <td>/edit-account</td>
            <td>EditAccount</td>
        </tr>
    </tbody>
</table>

## Angular

*Angular.js* is a framework by Google for making **SPA**s (*Single-Page Application*).
SPA is a great architecture, where you have a thin back-end server, providing an API
to your slim frontend application. And the most interesting part here, is that you have
all your application' pages in a one place, loaded once. And they are switched by a router
in a user's web-browser. So all the communication with server is stripped to data
manipulation requests (*creation, updating and reading data from server*) and first-time
request, sending the HTML, CSS and JavaScripts to user's browser. And then, all the
interactions are performed in a browser. At maximum, frontend application can request
a partial or some assets (*like images*) from a web-server.

Basically, here's how SPA works:

1. user enters a website
2. server sends application' code (implemented in JavaScript) and HTML to user
3. user, say, clicks on a link to change the page
4. if application needs partial, it requests server for a HTML partial
5. application renders a retrieved (or cached) partial in a browser

Let's start and you will see how easy it is!

## Kickstarting

We'll start, adding Angular to our `bower.json` dependencies:

{% highlight bash %}
bower install --save angular
{% endhighlight %}

Now when we have Angular inside the `bower_components` directory, let's add its references to
our layout:

{% highlight jade %}
head
    script(src="angular/angular.js")
{% endhighlight %}

Now, the only two things we need to start using Angular right away are:

1. Angular application, defined in the JavaScript and referenced in the `<html>`
tag with `ng-app` **directive**
2. Angular controller, which will contain **scope** with variables and functions for
the page' portion

Yeah, we definitely need some clarifications about those two new words.

## Theory bits

Angular defines a few kind of bricks, you may use to build an entire application:

1. **templates** - dynamically changed HTML files
2. **two-way data binding**, used mostly within **interpolations** - a mechanism
for sharing dynamically changed *(by either user of javascript)* data between
views and code
3. **controllers**, holding **scope** - are JavaScript objects, containing variables and
functions to be used in templates *(functions and variables are put inside a scope, which
is an argument to corresponding controller; I'll describe it later)*
4. **factories** (or **services**) - helper objects, containing logic which is not
*directly* related to views *(displaying and retrieving data)*
5. **directive** - is a place, where HTML-related code is placed

On the other hand, in MVVM architecture we have three layers:

1. **Model** - layer, performing manipulations on data; kind of *database layer*
2. **View** - user interface layer
3. **ViewModel** - stores data to be shown on UI or which was retrieved from UI

Accordingly to this scheme, in Angular we have the next logic structure:

1. **Model** - **Services**
2. **Views** - **Templates**, **Directives**
3. **ViewModel** - **Controllers**

Here I did not mention *two-way data binding* and *scopes*, because first one is a part of *templates* and the second one is a part of *controllers*. And they may not be separated one from another.

## Kickstarting. For real

Let's add some Angular logic into our project. We will split our app into all those
controllers and services very soon. But first, we need our application in Angular to be
registered. Create a file `config.js` inside the `src/javascripts/` directory
with the following content:

{% highlight js %}
var OurStatsApp = angular.module('OurStatsApp', [  ]);
{% endhighlight %}

And point the whole layout to use that application with the `ng-app` directive on `<html>` tag:

{% highlight js %}
doctype html
html(lang="en" ng-app="OurStatsApp")
    head
        block head
{% endhighlight %}

Now when we have our app accessible by all our pages, let's create our first controller,
which will handle exactly one page, the landing page. To expose our controllers,
we need to make a module, consisting of controllers and add it as a dependency
to our application. If you take a look at the application definition, it is also
a module. The first parameter to `angular.module()` function is a module name,
the second one is a list of dependencies.

Controllers are defined in a same way, but the dependency list for them contains
module definition as the last element as well. Let's define a `LandingPageCtrl` controller
in a `src/javascripts/controllers.js` file:

{% highlight js %}
var OurStatsControllers = angular.module('OurStatsControllers', []);

OurStatsControllers.controller('LandingPageCtrl', [ '$scope',
    function ($scope) {
        $scope.apps = 142;
    }
]);
{% endhighlight %}

Here we defined a module `OurStatsControllers`, which will contain all the controllers
and a `LandingPageCtrl` controller, which is a module of the same name, with a single
dependency - `$scope` and its implementation. As you can see, the `$scope` appears
here twice - first time as a dependency name and second time as an argument
to controller' definition function. That's exactly how **dependency injection** is made
with Angular. Here we defined a scope variable for our controller too, called `apps`.
We will use it in just a moment.

Now we need to inject our controllers module into our application. So we just add a
dependency entry inside our application declaration:

{% highlight js %}
var OurStatsApp = angular.module('OurStatsApp', [ 'OurStatsController' ]);
{% endhighlight %}

As we already defined the `OurStatsController` module, we do not need to provide
an implementation for it.

But to make our controller work properly, we need either to declare it on frontend,
making it handle static piece of a page, or make our application use different
controller and view, depending on a route in a browser's address line.

The first approach is good when you use Angular for complex widgets on a page. But
we need something more from our application, so we will set up router. It will
parse URL from browser and run the corresponding controller.

Router we will be using is `ngRoute`. This is a third-party plugin for Angular.
And in order to use it, we need to add a Bower dependency to our project:

{% highlight bash %}
bower install --save-dev angular-route
{% endhighlight %}

Now we only need to reference it in our app definition:

{% highlight js %}
var OurStatsApp = angular.module('OurStatsApp', [ 'ngRoute', 'OurStatsController' ]);
{% endhighlight %}

The next step is configuring our application routes, making application to
"understand", which controller and which template it should run. This may
be done in the same file, as the application definition:

{% highlight js %}
OurStatsApp.config([ '$routeProvider',
    function ($routeProvider) {
        $routeProvider.
            when('/home', {
                templateUrl: 'partials/landing-page.html',
                controller: 'LandingPageCtrl'
            }).
            otherwise({
                redirectTo: '/home'
            });
    }
]);
{% endhighlight %}

This configuration makes our application run `LandingPageCtrl` when the `/home` page is
entered. And if the URL entered is not known, application will redirect user to the
`/home` page. All Angular routes are passed as an anchor, so saying *the `/home` page*
we actually mean `http://your.host/#/home`. This makes browser to not send requests
to the server, requesting inexisting pages.

Also, note we point our application to use the `partials/home.html` template.
But we did not tell the Angular where to render it. To fix that, we will
add a `ng-view` directive to the `.container-fluid` element of our `<body>` tag,
right in our layout:

{% highlight jade %}
body
    block body
        .container-fluid(ng-view)
{% endhighlight %}

Currently, when a project is build with the Gulp' `build` task,
we have a `public/home.html` page, but it is not a partial - it is a standalone
page, extending a layout. But what we need is to compile all our Jade templates
into a `public/partials` directory, making them available through the
`/partials/partial_name.html` URL. And they should not extend a layout, because
then we will end up with an incorrect HTML code.

To make the magic happen, we only need to change our `build` task for Gulp
to build views directly to the `public` directory. So the `src/views/index.jade`
file will be the main HTML page of our application, available through
`/index.html` URL. And all the partials would be our Angular controllers' templates,
available through `/partials/template_name.html` URLs.

Now the views part of Gulp' `build` task should look like this:

{% highlight js %}
gulp.src('src/views/**/*.jade')
    .pipe(jade())
    .pipe(gulp.dest('public/'));
{% endhighlight %}

And let's just extract the content of `index.jade` partial as it is now, all the
content into a separate Angular partial, `src/views/partials/home.jade`.

And let's add only one small change - add only one line there:

{% highlight html %}
<h1>Currently running {{ apps }} apps</h1>
{% endhighlight %}

Remember, we have the `$scope` parameter for our `LandingPageCtrl` controller?
And there we set the `apps` variable? That is it, displaying on our home page.
All the other parameters, added to the `$scope` are available on the corresponding
template, by default. You may display them with some formatting, looping through the list
with `ng-repeat` and using them as conditionals with `ng-if` or `ng-show`.
But let's go step-by-step.

<a class="btn btn-primary btn-lg" href="https://github.com/shybovycha/two-sides-of-web-application/commit/b443618ca13179cc33b7ff2e08d0d6209b323113">Code goes here</a>

## Populating our app with stuff

For now, let's add all the other controllers to our app. Just create empty
controllers according to the table from **part one**, using the same approach as
described right above.

We will define each controller in its own file, but all of them will be concatenated
into one big application javacript. We will use Gulp for this purpose with
its `gulp-concat` plugin.

Let's install it:

{% highlight bash %}
npm install --save gulp-concat
{% endhighlight %}

Its usage is super-easy: you just redirect the output of our CSS and JS files compiler
to the `concat(filename)` function, providing it with a resulting file' name. Like this:

{% highlight js %}
var concat = require('gulp-concat');

gulp.src('src/javascripts/**/*.js')
    .pipe(babel())
    .pipe(concat('all.js'))
    .pipe(gulp.dest('public/javascripts'));

gulp.src('src/stylesheets/**/*.scss')
    .pipe(sass({ style: 'expanded' }))
    .pipe(concat('all.css'))
    .pipe(gulp.dest('public/stylesheets'));
{% endhighlight %}

Now, the controllers. First we need to set the routes.
The router configuration for our application will look just like this:

{% highlight js %}
$routeProvider
    .when('/', {
        templateUrl: 'landing-page.html',
        controller: 'LandingPageCtrl'
    })
    .when('/new-session', {
        templateUrl: 'new-session.html',
        controller: 'NewSessionCtrl'
    })
    .when('/edit-account', {
        templateUrl: 'edit-account.html',
        controller: 'EditAccountCtrl'
    })
    .when('/applications/:id', {
        templateUrl: 'show-application.html',
        controller: 'ShowApplicationCtrl'
    })
    .when('/applications', {
        templateUrl: 'list-applications.html',
        controller: 'ListApplicationsCtrl'
    })
    .when('/applications/:id/edit', {
        templateUrl: 'edit-application.html',
        controller: 'EditApplicationCtrl'
    })
    .when('/applications/new', {
        templateUrl: 'new-application.html',
        controller: 'NewApplicationCtrl'
    })
    .otherwise({
        redirectTo: '/'
    });
{% endhighlight %}

And we currently have a lot of pages with broken links. We should fix that,
providing URLs to our *(empty for now)* controllers. Like this one:

{% highlight jade %}
a.btn.btn-primary.btn-lg(href="#/sign-in" role="button") Sign in
{% endhighlight %}

Fix those links for all the views by yourself - that is not a complex task.
All the routes are described both in an app configuration and in a table
from **part 1**.

## First resource

Let's create a Session resource. Since we have no back-end part, we should stub
the data. We will use Angular **Services**. That's easy: a service defines a
function, returning, say, an object. That object will be used every time you
call a service. And you may use not objects only - you may return functions,
constants or literally anything from your services.

We will return an object with two methods: `get(account)` and `create(account)`.
Both methods will take an object, containing user account details - `email`,
`password`, and, in case of `create()`, `name` and `password_confirmation`, additionally.
You might guessed already: the `get(account)` method will log user in and
`create(account)` will register a new account for user.

{% highlight js %}
OurStatsApp.factory('Session', [
    () => {
        var testAccount = {
                    name: "Artem",
                    email: "artem@ourstats.com"
                };

        return {
            get: (account) => {
                var params = {
                    email: account.email,
                    password: account.password
                };
            },

            create: (account) => {
                var params = {
                    name: account.name,
                    email: account.email,
                    password: account.password
                };
            }
        }
    }
]);
{% endhighlight %}

When you inject it into your controller, you may use it via

{% highlight js %}
Session.get({ email: "", password: "" });
{% endhighlight %}

The thing here is that we want to keep user' data somewhere to simulate a
database work. So when user *"signs up"*, he will have his data saved in memory
to not lost it when changing a page. We will use another service for that purpose.
But contra to our `sessions` service, this one will be used in future and even
barely changed.

To store user account data we will use cookies. And there is a plugin for this
already! It is called `ngCookies`. And we will install it right now:

{% highlight bash %}
bower install --save angular-cookies
{% endhighlight %}

This one's configuration is a bit more complicated. But just a bit: it needs
to be added to application dependency list:

{% highlight js %}
var OurStatsApp = angular.module('OurStatsApp', [ 'ngRoute', 'ngCookies', 'OurStatsControllers' ]);
{% endhighlight %}

Now we need to define our `AccountData` service. It will also require a `ngCookies` dependency:

{% highlight js %}
OurStatsApp.factory('AccountData', [ '$cookies',
    ($cookies) => {
        return {
            get: () => {
                return $cookies.getObject('account');
            },
            set: (account) => {
                $cookies.putObject('account', account);
            },
            reset: () => {
                $cookies.remove('account');
            }
        }
    }
]);
{% endhighlight %}

And we need to modify our `Session` service a bit:

{% highlight js %}
OurStatsApp.factory('Session', [ '$http', 'AccountData',
    ($http, AccountData) => {
        var testAccount = {
                    name: "Artem",
                    email: "artem@ourstats.com"
                };

        return {
            get: (account) => {
                var params = {
                    email: account.email,
                    password: account.password
                };

                AccountData.set(testAccount);
            },

            create: (account) => {
                var params = {
                    name: account.name,
                    email: account.email,
                    password: account.password
                };

                AccountData.set(testAccount);
            }
        }
    }
]);
{% endhighlight %}

And the controller nneds to be changed to pick up the newely added service:

{% highlight js %}
OurStatsControllers.controller('SessionCtrl', [ '$scope', 'Session', 'AccountData',
    ($scope, session) => {
        $scope.newAccount = {};
        $scope.existingAccount = {};

        $scope.signIn = () => {
            Session.get($scope.existingAccount);
        };

        $scope.signUp = () => {
            Session.create($scope.newAccount);
        };

        $scope.signOut = () => {
            AccountData.reset();
        };
    }
]);
{% endhighlight %}

So now we have our first member of our **Model** layer. It works with stubbed data right now,
but we will be changing that later. It is not binded to the **ViewModel**, because the values
of our `$scope.newAccount` and `$scope.existingAccount` are constant and do not depend on
the template. To change this, we need to modify our `new_session.jade` template:

{% highlight jade %}
.row.m-t-md
    .col-xs-12.col-md-4.col-md-offset-1.well
        h3 Sign up
        form(ng-submit="signUp()")
            fieldset.form-group
                input.form-control(type="text" placeholder="Your name" ng-model="newAccount.name")
            fieldset.form-group
                input.form-control(type="email" placeholder="Email" ng-model="newAccount.email")
            fieldset.form-group
                input.form-control(type="password" placeholder="Password" ng-model="newAccount.password")
            fieldset.form-group
                input.form-control(type="password" placeholder="Password confirmation")
            fieldset.form-group.text-center
                button.btn.btn-primary(type="submit") Sign up

    .col-md-2

    .col-xs-12.col-md-4.well
        h3 Sign in
        form(ng-submit="signIn()")
            fieldset.form-group
                input.form-control(type="email" placeholder="Email" ng-model="existingAccount.name")
            fieldset.form-group
                input.form-control(type="password" placeholder="Password" ng-model="existingAccount.password")
            fieldset.form-group.text-center
                button.btn.btn-success(type="submit") Sign in
{% endhighlight %}

Note the `ng-model` directive: it performs two-way data binding. It means, when user changes the
value of our, say, `name` input, the corresponding value inside controller, namely `$scope.newAccount.name`,
will also change. What's more, when the value of `$scope.newAccount.name` will be changed from
the JavaScript (or, from the controller itself), the value within the input in a user's browser,
will also be changed.

Now, what's about the `ng-submit` directive. It is added to form tags only. And it works exactly as
you might guessed: when form is submit, the corresponding expression within that directive is called.

<a href="https://github.com/shybovycha/two-sides-of-web-application/commit/9d6b581312df8b3b61af20a26505b11679c84171" class="btn btn-primary btn-lg">Source code</a>

All the changes we did in this paragraph show you how one resourceful controller is made.
You may now proceed creating all the other controllers and views by yourself, but in case
you are stuck, here's the final version of our app, with all the controllers and resources,
created in the same way, as described above:

## Demo

<p data-height="268" data-theme-id="0" data-slug-hash="obQbvX" data-default-tab="result" data-user="shybovycha" class='codepen'>See the Pen <a href='http://codepen.io/shybovycha/pen/obQbvX/'>Simple web analytics. Angular injection. v1</a> by Artem Shoobovych (<a href='http://codepen.io/shybovycha'>@shybovycha</a>) on <a href='http://codepen.io'>CodePen</a>.</p>
<script async src="//assets.codepen.io/assets/embed/ei.js"></script>
