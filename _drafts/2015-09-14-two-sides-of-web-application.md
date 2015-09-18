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

Before we start, let's think of what we will be creating. Will it be a web shop? Or a blog?
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

Now, let's decide what pages our application will have and how will they look like.

**TODO: IMAGES GO HERE**

Pages illustrated above are used for these actions:

<table class="table table-bordered">
    <thead>
        <tr>
            <th>Action</th>
            <th>Resource</th>
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

And so we can set corresponding Angular controllers.

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
            <td>/sign-up</td>
            <td>accountCreate</td>
        </tr>
        <tr>
            <td>/sign-in</td>
            <td>sessionCreate</td>
        </tr>
        <tr>
            <td>/sign-out</td>
            <td>sessionDestroy</td>
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
    </tbody>
</table>

## Build with the right tools!

Now, since we separated our front-end part of application from back-end part, we may use different preprocessing
languages to write stylesheets and views. And even controllers! So let's take the most from 2015 and use the newest
tool set: *Jade*, *ES6+* and *SCSS*. And put all them together with *Bower* and *Gulp*.

I found Gulp to be super-easy for tasks like compiling stylesheets, views and javascripts.

But first of all, let's initialize an NPM project with `npm init`. Then, we need to install our development tools:

{% highlight bash %}
npm install -g gulp bower
{% endhighlight %}

We will be building our Jade, SCSS and ES6+ files with Gulp, so we do not need to install all those tools
globally - only as plugins for Gulp:

{% highlight bash %}
npm install --save-dev gulp gulp-babel gulp-scss gulp-jade gulp-install
{% endhighlight %}

I will describe how Gulp works and how we can use it in our project in a minute. For now, let's just install the project dependencies. Let's make them use fixed versions, so when we update our project,
nothing gets broken. To make our development quick, we'll be using *Twitter Bootstrap*.
Let's fill out the `bower.json` file automatically:

{% highlight bash %}
bower init
bower install --save bootstrap angular
{% endhighlight %}

These commands create a directory `bower_components`, containing all the dependencies installed, each in its own sub-directory. With that in mind, we will be referencing our front-end dependencies, relatively to their catalogs within the `bower_components` directory.

Now let's write a build task for Gulp. Gulp is a streamed build tool. That means, that each operation you perform, passes its result to another operation as the input argument. So, for example, if you run `gulp.src('src/styles/*.scss')`, it will return you an object with the list of all the SCSS files and the magic `pipe()` method. And when you call the `gulp.src(...).pipe(scss())`, Gulp will pass that list to the SCSS compiler plugin, so you will get a compiled CSS code. That is, not a CSS file itself, but a compressed, merged, CSS file' content.

And that describes the second important feature of Gulp: it does not store the intermediate operation results. It is almost like a functional programming.

So, how to store that list of results?

Just write this code in the `gulpfile.js`:

{% highlight js %}
var gulp = require('gulp'),
    sass = require('gulp-scss'),
    babel = require('gulp-babel'),
    jade = require('gulp-jade'),
    install = require('gulp-install');

gulp.task('install', function () {
    gulp.src([ 'bower.json', 'package.json' ])
        .pipe(install());
});

gulp.task('build', function () {
    gulp.src('src/views/**/*.jade')
        .pipe(jade())
        .pipe(gulp.dest('public/views'));

    gulp.src('src/javascripts/**/*.js')
        .pipe(babel())
        .pipe(gulp.dest('public/javascripts'));

    gulp.src('src/stylesheets/**/*.scss')
        .pipe(sass({ style: 'expanded' }))
        .pipe(gulp.dest('public/stylesheets'));
});
{% endhighlight %}

This tells Gulp to define two tasks - `install`, which installs all NPM and Bower dependencies, and
`build`, which compiles all the Jade, SCSS and ES6+ files into HTML, CSS and JS files, correspondingly.
Compiled files are placed within the `public/` directory, so we may easily render them with almost any
web-server. But first things first, we need to prepare directory structure like this for our `build` task:

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

*`src/views/index.jade`*

{% highlight jade %}
html(lang="en")
head
    meta(charset="UTF-8")
    title Document
    link(rel="stylesheet", href="/stylesheets/main.css")
    script(type="text/javascript", src="/javascripts/main.js")
body
    h1 Hello, world!
{% endhighlight %}

*`src/stylesheets/main.scss`*

{% highlight scss %}
h1 {
    padding: 0 10px;
    border-bottom: 1px solid #dedede;
}
{% endhighlight %}

*`src/javascripts/main.js`*

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

And to actually check our task, we need to run it with `gulp build`. Now, you may open the HTML generated
from Jade in a browser, but it will look ugly, because your browser will doubtly find stylesheets and javascripts.

## Why these ones?

I decided to write BDD tests with *Cucumber* and *Jest*. To interoperate with browser we will be using Selenium.
But it needs a webdriver to communicate with browser. For this purpose we will use *Webdriverio*.
Why these ones? Why not using Protractor?

You should not be looking for a reason to stay with selected technology, unless you are running a long-lasting
enterprise solution. For example, keeping protractor with his hell-hard syntax for simple operations only
because it can handle downloading and running webdrivers for selenium out-of-the box... Or using ugly-looking
*expect* from npm because it is popular a bit... Or *chai.js* for its `.eventually()` method of handling promises...

## Promises

{% highlight javascript %}
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
{% endhighlight %}

{% highlight js %}
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
{% endhighlight %}

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
