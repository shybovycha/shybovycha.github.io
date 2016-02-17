---
layout: post
title: "Two sides of web application. Part 3: Communication Layer"
date: '2016-02-11T14:47:39+01:00'
---

<div class="row">
    <div class="col-md-6 col-xs-12">
        {% include two-sides-of-web-application-reference.html %}
    </div>

    <div class="col-md-6 col-xs-12 text-xs-center text-md-right">
        <img class="img-responsive" style="max-height: 150px" src="{{ '/images/two-sides-of-web-application/a305-1.jpg' | prepend: site.baseurl }}" />
    </div>
</div>

## Foreword

In this section we will implement the communication layer for our application. It'll handle
all the requests to/from our web server. Have no worries - we will create server application
in the next section!

## First resource

Let's create a `Session` resource. Since we have no backend part, we should stub
the data. We'll use Angular **Services**. That's easy: a service defines a
function, returning, say, an object. That object will be used every time you
call a service. And you may use not only objects - you may return functions,
constants or literally anything from your services.

<!--more-->

We'll return an object with two methods: `get(account)` and `create(account)`.
Both methods will take an object, containing user account details - `email`,
`password`, and, in case of `create()`, `name` and `password_confirmation`, additionally.
You might've guessed already: the `get(account)` method will log the user in and
`create(account)` will register a new account for the user.

{% highlight js %}
ourStatsControllers.factory('Session', [
    () => {
        var testAccount = {
                    name: 'Artem',
                    email: 'artem@ourstats.com'
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
Session.get({ email: '', password: '' });
{% endhighlight %}

The thing here is that we want to keep user data somewhere to simulate a
database work. So when user *"signs up"*, he will have his data saved in the memory
not to loose it when changing a page. We'll use another service for that purpose.
But contrary to our `sessions` service, this one will be used in the future and even
barely changed.

Now, here's one more trick: we defined our `Session` factory in the `ourStatsControllers` module.
Why did we do this? Why not just use `ourStatsApp` module? This is done mostly to keep things
in one place, so everything related to controllers and UI processing is kept inside one module.
Doing so we can later separate our modules into separate files and keep our main application
module clean from unnecessary code *(in terms of the whole application)*.

To store user account data we'll use cookies. And there is a plugin for this
already! It is called `ngCookies`. And we'll install it right now:

{% highlight bash %}
bower install --save angular-cookies
{% endhighlight %}

This one's configuration is a bit more complicated. But just a bit: it needs
to be added to application dependency list:

{% highlight js %}
var ourStatsApp = angular.module('ourStatsApp', [ 'ngRoute', 'ngCookies', 'ourStatsControllers' ]);
{% endhighlight %}

But since we use cookies directly in controllers only, we may move the `ngCookies` dependency to
the `ourStatsControllers` module:

{% highlight js %}
var ourStatsControllers = angular.module('ourStatsControllers', [ 'ngCookies' ]);

// ...

var ourStatsApp = angular.module('ourStatsApp', [ 'ngRoute', 'ourStatsControllers' ]);
{% endhighlight %}

Now we need to define our `AccountData` service. It'll also require a `ngCookies` dependency:

{% highlight js %}
ourStatsControllers.factory('AccountData', [ '$cookies',
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
ourStatsControllers.factory('Session', [ '$http', 'AccountData',
    ($http, AccountData) => {
        var testAccount = {
                    name: 'Artem',
                    email: 'artem@ourstats.com'
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

And now we can implement the controller to pick up the newely added service:

{% highlight js %}
ourStatsControllers.controller('NewSessionCtrl', [ '$scope', 'Session', 'AccountData',
    ($scope, Session) => {
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

But here's just one small detail: we need both `Session` and `AccountData` services to perform
similar actions - managing session. We can refactor our code to keep all the session management
tasks in one place - `Session` service:

{% highlight js %}
ourStatsControllers.factory('Session', [ '$http', 'AccountData',
    ($http, AccountData) => {
        var testAccount = {
                    name: 'Artem',
                    email: 'artem@ourstats.com'
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
            },

            reset: () => {
                AccountData.reset();
            }
        }
    }
]);
{% endhighlight %}

Now our `NewSessionCtrl` could be refactored too:

{% highlight js %}
ourStatsControllers.controller('NewSessionCtrl', [ '$scope', 'Session',
    ($scope, Session) => {
        $scope.newAccount = {};
        $scope.existingAccount = {};

        $scope.signIn = () => {
            Session.get($scope.existingAccount);
        };

        $scope.signUp = () => {
            Session.create($scope.newAccount);
        };

        $scope.signOut = () => {
            Session.reset();
        };
    }
]);
{% endhighlight %}

Looks better, huh?

So now we have our first member of our **Model** layer. It works with stubbed data right now,
but we'll be changing that later. It is not bound to the **ViewModel**, because the values
of our `$scope.newAccount` and `$scope.existingAccount` are constant and don't depend on
the template. To change this, we need to modify our `new_session.jade` template:

{% highlight jade %}
.row
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
you might've guessed: when form is submitted, the corresponding expression within that directive is called.

## Making fake calls

Now that we have our stubbed signing in and up routines, we may redirect user to the right page
when he signed in or up successfully. This is the job for `$location` service. It's available
in Angular core module, `ng`, so we don't need to add any new module-wise dependencies.
Just use it in our `NewSessionCtrl`:

{% highlight js %}
ourStatsControllers.controller('NewSessionCtrl', [ '$scope', '$location', 'Session',
    ($scope, $location, Session) => {
        $scope.newAccount = {};
        $scope.existingAccount = {};

        $scope.signIn = () => {
            Session.get($scope.existingAccount);
            $location.path('/applications');
        };

        $scope.signUp = () => {
            Session.create($scope.newAccount);
            $location.path('/applications');
        };

        $scope.signOut = () => {
            Session.reset();
            $location.path('/');
        };
    }
]);
{% endhighlight %}

Here comes new refactoring-able piece of code: we now have our routes declared in both
`ourStatsApp` module and `ourStatsControllers`. So if we decide to change, let's say,
route `/new-session` to `/login`, we will need to change it in both modules. We can easily
extract those URLs into a single service and use it in both `ourStatsControllers`
and `ourStatsApp` modules, because everything we define in `ourStatsControllers` will be
available in the `ourStatsApp` thanks to dependency injection. And since our URLs will
always be the same *(except, maybe, the parametrised ones)*, we may define a constant
instead of factory:

{% highlight js %}
ourStatsControllers.constant('Url', {
        landing: '/',
        newSession: '/new-session',
        editAccount: '/edit-account',
        listApplications: '/applications',
        newApplication: '/applications/new',

        showApplication: (id) => {
            if (!id)
                id = ':id';

            return `/applications/${id}`;
        },

        editApplication: (id) => {
            if (!id)
                id = ':id';

            return `/applications/${id}/edit`;
        }
    })
{% endhighlight %}

## Application startup phases

As you can see, there are a couple of entities you can create with Angular - we've already
dealt with **controllers**, **factories**, **modules** and **constants**. They all are slighty
different from each other and the difference lies under Angular startup process.

When your application *"starts up"* or initializes, there are two kingds of methods, which are
ran first: `config` and `run`. They are user-defined and your application can have more than just
one of those. Those methods define the initialization behaviour of your application. They both
can deal with dependency injection. But their run order is different: `config` blocks are executed
first, at the very beginning of the whole application initialization process. Thus you can inject
only **providers** *(`$routeProvider` for instance)* and **constants** into `config` blocks.
Whilst `run` blocks can only handle **instances** *(for example, `$scope`)* and **constants**.

Since we have our routes defined at the `config` stage, we need to use **constant** to name our
routes. And then use them like this:

{% highlight js %}
ourStatsApp
    .config(['$routeProvider', 'Url',
        ($routeProvider, Url) => {
            $routeProvider
                .when(Url.landing, {
                    templateUrl: 'landing-page.html',
                    controller: 'LandingPageCtrl'
                })
                .when(Url.newSession, {
                    templateUrl: 'new-session.html',
                    controller: 'NewSessionCtrl'
                })
                .when(Url.editAccount, {
                    templateUrl: 'edit-account.html',
                    controller: 'EditAccountCtrl'
                })
                .when(Url.listApplications, {
                    templateUrl: 'list-applications.html',
                    controller: 'ListApplicationsCtrl'
                })
                .when(Url.newApplication, {
                    templateUrl: 'new-application.html',
                    controller: 'NewApplicationCtrl'
                })
                .when(Url.editApplication(), {
                    templateUrl: 'edit-application.html',
                    controller: 'EditApplicationCtrl'
                })
                .when(Url.showApplication(), {
                    templateUrl: 'show-application.html',
                    controller: 'ShowApplicationCtrl'
                })
                .otherwise({
                    redirectTo: Url.landing
                });
        }]);

ourStatsControllers
    .controller('NewSessionCtrl', [ '$scope', '$location', 'Session', 'Url',
        ($scope, $location, Session, Url) => {
            $scope.newAccount = {};

            $scope.existingAccount = {};

            $scope.signIn = () => {
                Session.get($scope.existingAccount);
                $location.path(Url.listApplications);
            };

            $scope.signUp = () => {
                Session.create($scope.newAccount);
                $location.path(Url.listApplications);
            };

            $scope.signOut = () => {
                AccountData.reset();
                $location.path(Url.landing);
            };
        }
    ])
{% endhighlight %}

To show the difference in practice, let's add the authentication verification. We now have our
stubbed account and session management service, so why not? Angular provides us with two options:

1. the `resolve` attribute for the `$routeProvider.when()` method
2. the `$routeChangeStart` event

The first approach is nice, but since we set up routes in the `config` section, we are not allowed
to use services. And that is the problem, because we've got our `Session` service, handling the
tasks we need. And one more drawback of this method: one should set it for each route, which requires
verification:

{% highlight js %}
var verifyAuthentication = () => {
    // ...
};

ourStatsApp
    .config(['$routeProvider', 'Url',
        ($routeProvider, Url) => {
            $routeProvider
                .when(Url.landing, {
                    templateUrl: 'landing-page.html',
                    controller: 'LandingPageCtrl'
                })
                .when(Url.newSession, {
                    templateUrl: 'new-session.html',
                    controller: 'NewSessionCtrl'
                })
                .when(Url.editAccount, {
                    templateUrl: 'edit-account.html',
                    controller: 'EditAccountCtrl',
                    resolve: verifyAuthentication
                })
                .when(Url.listApplications, {
                    templateUrl: 'list-applications.html',
                    controller: 'ListApplicationsCtrl',
                    resolve: verifyAuthentication
                })
                .when(Url.newApplication, {
                    templateUrl: 'new-application.html',
                    controller: 'NewApplicationCtrl',
                    resolve: verifyAuthentication
                })
                .when(Url.editApplication(), {
                    templateUrl: 'edit-application.html',
                    controller: 'EditApplicationCtrl',
                    resolve: verifyAuthentication
                })
                .when(Url.showApplication(), {
                    templateUrl: 'show-application.html',
                    controller: 'ShowApplicationCtrl',
                    resolve: verifyAuthentication
                })
                .otherwise({
                    redirectTo: Url.landing
                });
            }]);
{% endhighlight %}

Handling `$routeChangeStart` event suits us, and what's more interesting: it shows the use of `run`
section. But as a drawback to this, we'll need the list of routes, which need to be checked. So
I modified the definition of `Url` constant like this:

{% highlight js %}
ourStatsApp
    .constant('Url', (() => {
        var urls = {
            landing: '/',
            newSession: '/new-session',
            editAccount: '/edit-account',
            listApplications: '/applications',
            newApplication: '/applications/new',

            showApplication: (id) => {
                if (!id)
                    id = ':id';

                return `/applications/${id}`;
            },

            editApplication: (id) => {
                if (!id)
                    id = ':id';

                return `/applications/${id}/edit`;
            }
        };

        urls.authRequired = [
            urls.editAccount,
            urls.listApplications,
            urls.newApplication,
            urls.showApplication(),
            urls.editApplication()
        ];

        return urls;
    })());
{% endhighlight %}

A bit tricky, does not it? And here's the handler for the `$routeChangeStart` event:

{% highlight js %}
ourStatsApp
    .run(['$rootScope', '$location', 'Session', 'AccountData', 'Url',
        ($rootScope, $location, Session, AccountData, Url) => {
            $rootScope.$on('$routeChangeStart', (event, current, next) => {
                // check if user is authenticated for selected URLs only
                // first, try to restore his session
                // if this failed because user has no AccountData cookie - redirect him to newSession page
                if (Url.authRequired.indexOf($location.path()) > -1 && !Session.get() && !AccountData.get())
                    $location.path(Url.newSession);
            });
        }]);
{% endhighlight %}

## Rocking on

Other services and controllers are much like those we already have, so I'll just put in the code
in the end of the page. The interesting thing here is the `MockData` service that I've used to keep
data, used while we have no server side.

{% highlight js %}
ourStatsApp
    .constant('MockData', {
        account: {
            name: 'Artem',
            email: 'artem@ourstats.com',
            password: 'abc123'
        },
        applications: [
            {
                id: 0,
                name: 'App #1',
                token: 'APP1TOK',
                stats: {
                    byCountry: [
                        {country: 'Poland', amount: 10},
                        {country: 'USA', amount: 190},
                        {country: 'Algeria', amount: 5}
                    ]
                }
            }, {
                id: 1,
                name: 'App #2',
                token: 'APP2TOK',
                stats: {
                    byCountry: [
                        {country: 'Vietnam', amount: 7},
                        {country: 'New Zeland', amount: 19},
                        {country: 'USA', amount: 15}
                    ]
                }
            }, {
                id: 2,
                name: 'App #3',
                token: 'APP3TOK',
                stats: {
                    byCountry: [
                        {country: 'USA', amount: 95}
                    ]
                }
            }
        ]
    });

// ...

ourStatsApp
    .factory('Application', ['$http', 'MockData', ($http, MockData) => {
        return {
            create: (application) => {
                // send data to the server and check if response status == 200; return application (arg)
                return application;
            },

            get: (id, fromDate, toDate) => {
                // fromDate and toDate are used for stats filtering
                // send data to the server and return response
                return MockData.applications[id];
            },

            update: (id, application) => {
                // send data to the server and check if response status == 200; return application (arg)
                return application;
            },

            all: () => {
                // send request to the server and return the response
                return MockData.applications;
            }
        };
    }]);
{% endhighlight %}

And here's the demo of what we've done up until now:

<p data-height="494" data-theme-id="0" data-slug-hash="vLMZmd" data-default-tab="result" data-user="shybovycha" class='codepen'>See the Pen <a href='http://codepen.io/shybovycha/pen/vLMZmd/'>Simple web analytics. Communication layer. v2</a> by Artem Shoobovych (<a href='http://codepen.io/shybovycha'>@shybovycha</a>) on <a href='http://codepen.io'>CodePen</a>.</p>
<script async src="//assets.codepen.io/assets/embed/ei.js"></script>
