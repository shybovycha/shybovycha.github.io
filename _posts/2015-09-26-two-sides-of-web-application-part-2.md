---
layout: post
title: Two sides of web application. Part 2
categories: []
tags: []
published: True

---

# Two sides of web application. Part 2

## Angular

*Angular.js* is a framework by Google for making **SPA**s (*Single-Page Application*).
SPA is a great architecture, where you have a thin back-end server, providing an API
to your slim front-end application. And the most interesting part here, is that you have
all your application' pages in a one place, loaded once. And they are switched by a router
in a user's web-browser. So all the communication with server is stripped to data
manipulation requests (*creation, updating and reading data from server*) and first-time
request, sending the HTML, CSS and JavaScripts to user's browser. And then, all the
interactions are performed in a browser. At maximum, front-end application can request
a partial or some assets (*like images*) from a web-server.

<!--more-->

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
2. **two-way data binding**, used mostly within **interpolations** - a mechanizm
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

Accordingly to this scheme, in Angular we have the next logic division:

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
var ourStatsApp = angular.module('ourStatsApp', [  ]);
{% endhighlight %}

And point the whole layout to use that application with the `ng-app` directive on `<html>` tag:

{% highlight js %}
doctype html
html(lang="en" ng-app="ourStatsApp")
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
module definition as the last element as well. Let's define a `homeCtrl` controller
in a `src/javascripts/controllers.js` file:

{% highlight js %}
var ourStatsControllers = angular.module('ourStatsControllers', []);

ourStatsControllers.controller('homeCtrl', [ '$scope',
    function ($scope) {
        $scope.apps = 142;
    }
]);
{% endhighlight %}

Here we defined a module `ourStatsControllers`, which will contain all the controlelrs
and a `homeCtrl` controller, which is a module of the same name, with a single
dependency - `$scope` and its implementation. As you can see, the `$scope` appears
here twice - first time as a dependency name and second time as an argument
to controller' definition function. That's exactly how **dependency injection** is made
with Angular. Here we defined a scope variable for our controller too, called `apps`.
We will use it in just a moment.

Now we need to inject our controlelrs module into our application. So we just add a
dependency entry inside our application declaration:

{% highlight js %}
var ourStatsApp = angular.module('ourStatsApp', [ 'ourStatsController' ]);
{% endhighlight %}

As we already defined the `ourStatsController` module, we do not need to provide
an implementation for it.

But to make our controller work properly, we need either to declare it on front-end,
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
var ourStatsApp = angular.module('ourStatsApp', [ 'ngRoute', 'ourStatsController' ]);
{% endhighlight %}

The next step is configuring our application routes, making application to
"understand", which controller and which template it should run. This may
be done in the same file, as the application definition:

{% highlight js %}
ourStatsApp.config([ '$routeProvider',
    function ($routeProvider) {
        $routeProvider.
            when('/home', {
                templateUrl: 'partials/home.html',
                controller: 'homeCtrl'
            }).
            otherwise({
                redirectTo: '/home'
            });
    }
]);
{% endhighlight %}

This configuration makes our application run `homeCtrl` when the `/home` page is
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

Remember, we have the `$scope` parameter for our `homeCtrl` controller?
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
$routeProvider.
    when('/home', {
        templateUrl: 'partials/home.html',
        controller: 'homeCtrl'
    }).
    when('/sign-in', {
        templateUrl: 'partials/new_session.html',
        controller: 'sessionCtrl'
    }).
    when('/apps', {
        templateUrl: 'partials/application_list.html',
        controller: 'appListCtrl'
    }).
    when('/app/:id', {
        templateUrl: 'partials/application_details.html',
        controller: 'appDetailsCtrl'
    }).
    when('/app/:id/edit', {
        templateUrl: 'partials/edit_application.html',
        controller: 'appEditCtrl'
    }).
    when('/account/edit', {
        templateUrl: 'partials/edit_account.html',
        controller: 'accountCtrl'
    }).
    otherwise({
        redirectTo: '/home'
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
ourStatsApp.factory('session', [
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
session.get({ email: "", password: "" });
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
var ourStatsApp = angular.module('ourStatsApp', [ 'ngRoute', 'ngCookies', 'ourStatsControllers' ]);
{% endhighlight %}

Now we need to define our `accountData` service. It will also require a `ngCookies` dependency:

{% highlight js %}
ourStatsApp.factory('accountData', [ '$cookies',
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

And we need to modify our `session` service a bit:

{% highlight js %}
ourStatsApp.factory('session', [ '$http', 'accountData',
    ($http, accountData) => {
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

                accountData.set(testAccount);
            },

            create: (account) => {
                var params = {
                    name: account.name,
                    email: account.email,
                    password: account.password
                };

                accountData.set(testAccount);
            }
        }
    }
]);
{% endhighlight %}

And the controller nneds to be changed to pick up the newely added service:

{% highlight js %}
ourStatsControllers.controller('sessionCtrl', [ '$scope', 'session', 'accountData',
    ($scope, session) => {
        $scope.newAccount = {};
        $scope.existingAccount = {};

        $scope.signIn = () => {
            session.get($scope.existingAccount);
        };

        $scope.signUp = () => {
            session.create($scope.newAccount);
        };

        $scope.signOut = () => {
            accountData.reset();
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

<a href="https://github.com/shybovycha/two-sides-of-web-application/commit/012593aee2ce391f63b44e9af1bfa54cf9d339fe" class="btn btn-primary btn-lg">Final version of this part</a>

## I'll be back!

In the next part I'll tell you, how we can implement the back-end part of our application and
how to bind it to the existing Angular application.

