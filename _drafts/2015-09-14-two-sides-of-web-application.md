---
layout: post
title: Two sides of web application
date: '2015-09-14T17:44:39+01:00'
---

# Two sides of web application

## Prologue

How do we usually create a web application? We run a bootstraping script, which provides
us with a skeleton of our application and then just extend it with the features we need.

So did we at the last hackathon we were attending - we started with `rails new twf` and
spent half of the day integrating our blank app with Angular, Paperclip, creating API methods
and so on. But the effort we needed to accomplish our goal (quite simple web app) was really huge.

So I decided to find the best combination of backend and front-end technologies to cause
less pain.

At the project I was lastly introduced into, the line between front-end and back-end is
distinguished really clearly: we have an API, written in Clojure and thin front-end application,
made with Angular and working on a generated set of static assets - HTMLs, CSS and JS files
*(but under the hood we are using HAML and SCSS)*.

The application I will be implementing throughout the whole article has the same architecture:
it has RESTful API and MVVM on the front-end made with Angular. You are welcome to the journey
of research and new technologies!

<!--more-->

## Why not go with Rails?

The way, which I think is the worst. Especially if you install all of front-end libraries
*(like Angular, Bootstrap, some Angular plugins, etc.)* as RubyGems. Front-end should stay
on the front end of the application; you should not tight your application at some precise
version of the JS script, provided by a gem and rely on author's way to integrate it with
Rails.

Moreover, Rails is really heavy - when all you need is database + routing + lightweight controllers,
all Rails' features will become a ballast to your app, which must be small, by design.

## The goal

Before we start, lets think of what we will be creating. Will it be a web shop? Or a blog?
No, we need something outstanding! Something we were never doing before...

After a hour of imaging what it may be, I decided to go with web analytics tool. A prototype,
which will be able to tell how many users did your web or mobile application gained lastly.

It should not be too complicated, because, you know, it's just a tutorial... So we will be
tracking users' location and browser only. And we will be showing statistics on a chart
*(values like total visitors, browser usage)* and in table *(the same info as on chart)*.

## General architecture

First thing we need to think of is how we will be gathering the information about users.
It may be quite easy - we just need to receive a request from visitor. Of any kind!
It may be a request to get an image, a file, a stylesheet or a script.

Then we will just parse headers from that request and save the extracted data in the
database. The only problem here is: *how to get unique key from each request?*. We may use
visitor's IP address. It's the easiest way.

Now, lets decide what pages our application will have and how will they look like.

**TODO: IMAGES GO HERE**

## Build with the right tools!

Now, since we separated our front-end part of application from back-end part, we may use different preprocessing
languages to write stylesheets and views. And even controllers! So lets take the most from 2015 and use the newest
tool set: *Jade*, *ES6+* and *SCSS*. And put all them together with *Bower* and *Gulp*.

All those Jade, SCSS and ES6 are not supported by a browser out-of-the box. They must be compiled to HTML, CSS and JS in order to be recognized by a browser. But they are here to help you writing code quickly. I listed some of their key features below.

**Jade** is a tempalte rendering engine with its own markup language. It is somehow similar to Haml and Slim - it
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

So, lets say you are having a user widget. And it may be placed both on page' header, footer and sidebar. But
the avatar image will look differently on each of those - it must be smaller in header and footer. So you
start writing selectors like `.sidebar .user-widget .avatar img` and `.header .user-widget .avatar img`.
That's painful, but not that much, if you have just a couple of those. But as your website grows, you
start getting really, really upset about that.

And here's where SCSS is just a revelation: its great power is in its *selectors*, *variables* and *mixins*.

*Selectors* allow you to describe selector' nesting just as you'd be writing C code:

```scss
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
```

*Variables* allow you to get rid of all those magic values. So if you use one color many times across your styles,
you just extract it into a named constant and use the pretty named value everywhere!

*Mixins* allow even more: you may extract the parts of the styles into a named and even **parametrized** function!

Relating on all those, you may re-write your user widget as follows:

```scss
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
```

I found Gulp to be super-easy for tasks like compiling stylesheets, views and javascripts. But before
we continue with Gulp, lets initialize an NPM project with `npm init` and install the plugins required:

```bash
npm install -g gulp
```

And Gulp plugins:

```bash
npm install --save-dev gulp gulp-babel gulp-scss gulp-jade
```

I will describe how Gulp works and how we can use it in our project in a minute. For now, lets
just install the front-end dependencies. Let's make them use fixed versions, so when we update our project,
nothing gets broken. To make our development quick, we'll be using *Twitter Bootstrap* and manage all
fron-end dependencies with *Bower*. Bower will fill out the `bower.json`, a file, telling Bower
which libraries to use, automatically:

```bash
npm install -g bower
bower init
bower install --save bootstrap angular
```

These commands create a directory `bower_components`, containing all the dependencies installed, each in its own sub-directory. With that in mind, we will be referencing our front-end dependencies, relatively to their catalogs within the `bower_components` directory.

Now lets write a build task for Gulp. Gulp is a streamed build tool. That means, that each operation you perform, passes its result to another operation as the input argument. So, for example, if you run `gulp.src('src/styles/*.scss')`, it will return you an object with the list of all the SCSS files and the magic `pipe()` method. And when you call the `gulp.src(...).pipe(scss())`, Gulp will pass that list to the SCSS compiler plugin, so you will get a compiled CSS code. That is, not a CSS file itself, but a compressed, merged, CSS file' content.

And that describes the second important feature of Gulp: it does not store the intermediate operation results. It is almost like a functional programming - you just have the input data. Then you call a chain of functions on it,
passing the result of one function call to the next function as its input. Same happens here, but in not that
strict manner - since we are using Javascript, we can store the intermediate results in a memory. But to store
the results in the files, we should pass them to the `gulp.dest(...)` function. Depending on function, looking
to store its results, `gulp.dest()` may point to a directory or a single file, where the results will be stored.

Below is our first Gulp task. Write this code in the `gulpfile.js`:

```js
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
```

This tells Gulp to define `build` task, which compiles all the Jade, SCSS and ES6+ files into HTML, CSS
and JS files, correspondingly. Compiled files are placed within the `public/` directory, so we may
easily render them with almost any web-server. But first things first, we need to prepare directory structure
like this for our task:

```bash
.
|____bower.json
|____gulpfile.js
|____package.json
|____public
|____src
| |____javascripts
| |____stylesheets
| |____views
```

This may seem odd to the paragraph, dedicated to build tools, but lets check how our tasks work.
To do this, we need to write some test files to check our `build` task. So lets create one of each kind:

*`src/views/index.jade`:*

```pug
html(lang="en")
head
    meta(charset="UTF-8")
    title Document
    link(rel="stylesheet", href="/stylesheets/main.css")
    script(type="text/javascript", src="/javascripts/main.js")
body
    h1 Hello, world!
```

*`src/stylesheets/main.scss`:*

```scss
h1 {
    padding: 0 10px;
    border-bottom: 1px solid #dedede;
}
```

*`src/javascripts/main.js`:*

```js
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
```

And to actually check our task, we need to run it with

```bash
gulp build
```

Now, you may open the HTML generated from Jade in a browser, but it will look ugly, because your
browser will doubtly find stylesheets and javascripts that simply. To make the magic happen, we
will use another Gulp plugin, `gulp-server-livereload`. Generally, the development process with
different build tools looks very similar nowadays: you set up the environment, find the plugins
you need, install and configure them - and vióla!

I've chosen that server plugin because it comes with one handy feature: it automatically re-loads
the opened pages in your web-browser if any of the files you are browsing has changed. Here's
the code of our serving task:

```js
gulp.task('serve', function () {
    gulp.src('public/')
        .pipe(server({
            livereload: true,
            directoryListing: false,
            open: true
        }));
});
```

But if you remember, we are compiling our sources from SCSS/Jade into the `public/` directory.
So how the server would know if anything changed in our `*.scss` or `*.jade` files? We need to
reload the page in web-browser if anything changes in those files. To do that, we will write one
more task, which will re-build our source files if anything changes:

```js
gulp.task('watch', function () {
    gulp.watch('src/**/*', [ 'build' ])
});
```

Here we used two new features of Gulp: *watching for file changes* and *running existin tasks from another task*.
Simple? Yeah, **that** simple! So we just tell Gulp: *keep an eye on those files - if anything happens - run
those tasks immediately!* - and the magic happens.

But why shoud we run two tasks? Let's merge them into one so we just run `gulp serve` and get both live reload
and live re-compilation:

```js
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
```

This task first defines a watcher for our sources and then starts server, which will react to any changes
in the `public/` directory. To check how this awesomeness works, start `gulp serve`, open the
[`localhost:3000/test.html`](localhost:3000/test.html) page and then change, for example,
color for the `h1` tag to green. Save the SCSS file and just switch to the browser window, **do not reload
it manually**.

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

```pug
doctype html
html(lang="en")
    head
        block head
            title OurStats
            link(rel="stylesheet", href="bootstrap/dist/css/bootstrap.css")
            link(rel="stylesheet", href="bootstrap/dist/css/bootstrap-theme.css")
            script(src="bootstrap/dist/js/bootstrap.js")

    body
        block body
```

*`src/views/index.jade`:*

```pug
extends layouts/default.jade

block content
    .jumbotron
        h1.display-3 Welcome to OurStats!
        p.lead This is a simple stats application
        hr.m-y-md

        p.lead
            .row
                .col-xs-12.col-md-1
                    a.btn.btn-primary.btn-lg(href="#", role="button") Sign in

                .col-xs-12.col-md-1
                    a.btn.btn-success.btn-lg(href="#", role="button") Sign up
```

*`src/views/new_session.jade`:*

```pug
extends layouts/default.jade

block content
    .row
        .col-xs-12.col-md-4.col-md-offset-1.well
            h3 Sign up
            form
                fieldset.form-group
                    input.form-control(type="text", placeholder="Your name")
                fieldset.form-group
                    input.form-control(type="email", placeholder="Email")
                fieldset.form-group
                    input.form-control(type="password", placeholder="Password")
                fieldset.form-group
                    input.form-control(type="password", placeholder="Password confirmation")
                fieldset.form-group.text-center
                    button.btn.btn-primary(type="submit") Sign up

        .col-md-2

        .col-xs-12.col-md-4.well
            h3 Sign in
            form
                fieldset.form-group
                    input.form-control(type="email", placeholder="Email")
                fieldset.form-group
                    input.form-control(type="password", placeholder="Password")
                fieldset.form-group.text-center
                    button.btn.btn-success(type="submit") Sign in
```

*`src/views/edit_account.jade`:*

```pug
extends layouts/default.jade

block content
    .row.m-t-md
        .col-xs-12.col-md-4
            h3 Account settings
            form
                fieldset.form-group
                    input.form-control(type="text", placeholder="Your name")
                fieldset.form-group
                    input.form-control(type="password", placeholder="Password")
                fieldset.form-group
                    input.form-control(type="password", placeholder="Password confirmation")
                fieldset.form-group.text-center
                    button.btn.btn-success(type="submit") Save
```

*`src/views/application_list.jade`:*

```pug
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
```

*`src/views/application_details.jade`:*

```pug
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
```

*`src/views/edit_application.jade`:*

```pug
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
```

*`src/views/new_application.jade`:*

```pug
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
```

And to make Bower-managed libraries available in our views, we need to add one more path to
the server configuration:

```js
gulp.src(['public/', 'bower_components/'])
    .pipe(server({
        port: 3000,
        livereload: true,
        directoryListing: false,
        open: true
    }));
```

As you can see, we used Jade's block extending and split our templates into one layout and many partials, so
our file tree is clean and changing any of the pages will not be a hard task.

## Architecture details

In our front-end application we will use something called *MVVM*. That is a design pattern, kindly provided
by Angular. So our *views* will be displaying data and transfering it to controllers *(or ViewModels)*, and
all the logic, handling that data will be defined in *controllers* and *services*, representing *Models*.
Actually, our models will be handled on a server-side, and services will only provide an interface to them.
But that is totally another story and will be described later.

For now lets integrate Angular in our application. And we will start, deciding how we will split our
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
            <td>home</td>
        </tr>
        <tr>
            <td>/sign-in</td>
            <td>session</td>
        </tr>
        <tr>
            <td>/apps</td>
            <td>appList</td>
        </tr>
        <tr>
            <td>/app/:id</td>
            <td>appDetails</td>
        </tr>
        <tr>
            <td>/app/:id/edit</td>
            <td>appEdit</td>
        </tr>
        <tr>
            <td>/account/edit</td>
            <td>account</td>
        </tr>
    </tbody>
</table>

## Next time: Angular injection

This is the end of the first part of my huge research article. In the next post I will show
how to add Angular into our front-end application.

-----

## Angular injection

To start using Angular, we need to add it as a dependency.

**DO NOT FORGET TO ADD `ng-app="myAppName"` for `<body>` tag!**

We eliminated almost all our `block` and `extends layouts/default.jade` instructions. So why are we
still using Jade? - Because it still simplifies our job!

We added a lot of dummy data, forgot a few routes, named our resources in a bad manner... But even
counting all those mistakes, it took nearly 2 hrs to write an Angular application fron scratch!
And that is the power of our architecture! All the next parts of this huge-research-post-series
describe how I look for such powerful solution for a server-side.

-----

## Resourceful services

```js
ourStatsApp.factory('session', [ '$http', 'accountData',
    ($http, accountData) => {
        return {
            get: (account) => {
                var params = {
                    email: account.email,
                    password: account.password
                };

                $http.post('/sessions/get', params)
                    .then((response) => {
                        account = response.data;
                        accountData.set(account);
                    })
                    .catch((response) => {
                        accountData.reset();
                    });
            },

            create: (account) => {
                var params = {
                    name: account.name,
                    email: account.email,
                    password: account.password
                };

                $http.post('/sessions/create', params)
                    .then((response) => {
                        account = response.data;
                        accountData.set(account);
                    })
                    .catch((response) => {
                        accountData.reset();
                    });
            }
        }
    }
]);
```

-----

## Why these ones?

I decided to write BDD tests with *Cucumber* and *Jest*. To interoperate with browser we will be using Selenium.
But it needs a webdriver to communicate with browser. For this purpose we will use *Webdriverio*.
Why these ones? Why not using Protractor?

You should not be looking for a reason to stay with selected technology, unless you are running a long-lasting
enterprise solution. For example, keeping protractor with his hell-hard syntax for simple operations only
because it can handle downloading and running webdrivers for selenium out-of-the box... Or using ugly-looking
*expect* from npm because it is popular a bit... Or *chai.js* for its `.eventually()` method of handling promises...

## Promises

```javascript
  this.Then(/^he can (not )?see retina images$/, function (negation, callback) {
       var pattern = (negation ? /^((?!@2x).)*$/ : /@2x/);

       element(by.css('.norton-logo img'))
           .getAttribute('src')
           .then(function (src) {
               expect(src).toMatch(pattern);
           })
           .then(function () {
               element(by.css())
                   .getCssValue('background')
                   .then(function (background) {
                       expect(background).toMatch(pattern);
                   });
           });
   });
```

```js
  this.Then(/^he can (not )?see retina images$/, function (negation, callback) {
        var pattern = (negation ? /^((?!@2x).)*$/ : /@2x/);

        var findNortonLogo = function () {
            return element(by.css('.norton-logo img')).getAttribute('src');
        };

        var checkNortonLogoSrc = function (src) {
            var deferred = protractor.promise.defer();

            expect(src).toMatch(pattern);
            deferred.fulfill();

            return deferred.promise;
        };

        var findHomepageLogo = function () {
            return element(by.css('.homepage')).getCssValue('background');
        };

        var checkHomepageLogoBackground = function (background) {
            var deferred = protractor.promise.defer();

            expect(background).toMatch(pattern);
            deferred.fulfill();

            return deferred.promise;
        };

        browser.waitForAngular()
            .then(findNortonLogo)
            .then(checkNortonLogoSrc)
            .then(findHomepageLogo)
            .then(checkHomepageLogoBackground)
            .then(callback);
    });
```

## Frank

*Sinatra + Sequel*

## Try some Grapes

*Napa = Grape + ActiveRecord*

## Scale it

*Scala + Play + Squeryl | SORM*

## Rethink half of it

*NodeJS + RethinkDB*

## Switch to Java

YEAH!

*Groovy?*

## Add some lists

*Clojure + Compojure + ???*

## Go with the flow

*Go + ??? + ???*

## Performance?

*Performance tests, run from another machine; clean restart server before each test*
