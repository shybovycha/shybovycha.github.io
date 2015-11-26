---
layout: post
title: End-to-end testing with WebdriverIO
date: '2015-11-26T17:03:39+01:00'
---

## Small intro

Have you ever heard about end-to-end testing? Or maybe about testing automation?
Those of you who had, may now be imaging Selenium. That's right, in most of cases
you will need to run *Selenium Server* and use *Selenium Webdriver* in your
tests. Those come handy to run a standalone browser window, with no caches,
filled-in fields or cookies and perform some operations in it.

In this article I will tell you my story of writing E2E tests for Angular webapp.

## A brief of history

In my case, we first tried to use _Protractor_ with _Chai.js_. That time we ended
up with almost unsupportable bunch of code, succeeding in 100% of runs.

Next time we eliminated *Chai* and reworked all our tests to use *Protractor* only.
This time tests worked and were more readable *(I did not like the syntax, but it worked...)*,
but after upgrading libraries *(including Protractor)*, success ratio of running tests
falled down to 40%.

We worked two days, trying to fix those tests. And that's how *webdriverio* came to
our project.

<!--more-->

## The requirements

Before we start, we need to initialize an NPM project:

{% highlight bash %}
npm init
{% endhighlight %}

We barely want to manage Selenium versions by ourselves. When it comes to
continuous integration, that is not an option. So we'd better find a way of
downloading the needed Selenium version automatically. And writing a short
script is somehow a bad idea - there is an NPM module for that task already:

{% highlight bash %}
npm install --save selenium-standalone
{% endhighlight%}

After installing that module, you may manage and run your Selenium with

{% highlight bash %}
./node_modules/selenium-standalone/bin/selenium-standalone install
./node_modules/selenium-standalone/bin/selenium-standalone start
{% endhighlight %}

Now, all the tests we will be writing with *Jasmine*, so we need that one:

{% highlight bash %}
npm install --save jasmine
{% endhighlight %}

To initialize bare Jasmine-ready project you may use Jasmine itself:

{% highlight bash %}
./node_modules/jasmine/bin/jasmine.js init
{% endhighlight %}

This will create this directory structure for you:

    .
    ├── package.json
    └── spec
        ├── helpers
        └── support
            ├── config.json
            └── jasmine.json


So now you may place your tests in the `spec` directory, on any level. Just
remember to name your test files with the `spec` postfix.

The last, but not least, we need *webdriverio* itself:

{% highlight bash %}
npm install --save webdriverio
{% endhighlight %}

And now we are ready to make some coding!

## The build process

First of all, we need the helper, initializing our browser client before any
tests are run. Let's define this in the `spec/helpers/settings.js` file:

{% highlight js %}
var webdriverio = require('webdriverio');

var timeout = 10 * 1000;
var config = {
  "desiredCapabilities": {
    "browserName": "chrome",
    "host": "localhost",
    "port": 9876
  },
  "baseUrl": "http://localhost"
};

jasmine.DEFAULT_TIMEOUT_INTERVAL = 2 * timeout;

beforeAll(function (done) {
    this.client = webdriverio
        .remote(config)
        .init()
        .url('/')
        .call(done);
});

afterAll(function (done) {
    this.client.end(done);
});

exports.webdriverio = webdriverio;
exports.config = config;
exports.timeout = timeout;
exports.baseUrl = config.baseUrl;
{% endhighlight %}

This code does three simple, but really important things:

1. it sets up the *webdriverio* **for all the tests**, opens browser window
*(set in the `browserName` option)* and navigates to the page, defined in `baseUrl` option
2. makes Jasmine wait a bit longer for assertions in the test, before making it
**failed on timeout** *(when Jasmine has timed out, waiting for any assertions)*
3. sets *webdriverio* client instance for each test in `this.client` member

Assume our application is a webshop - it has three pages: `/#/products`, `/#/billing-details`
and `/#/order-summary`. Rather simple webshop:

<img src="{{'/images/e2e-testing-with-webdriverio/webshop1.png'|prepend:site.baseurl}}" alt="">
<img src="{{'/images/e2e-testing-with-webdriverio/webshop2.png'|prepend:site.baseurl}}" alt="">
<img src="{{'/images/e2e-testing-with-webdriverio/webshop3.png'|prepend:site.baseurl}}" alt="">

And here is our first scenario: user selects the iPhone, clicks *"Buy"*, fills in
his billing details and waits for his brand-new iPhone to come.

{% highlight js %}
var settings = require('../helpers/settings.js');

describe('buying iPhone', function () {
    describe('when user selects iPhone', function () {
        it('he gets redirected to the billing-details page', function (done) {
            this.client
                .waitForExist('.title*=iPhone 6 White', settings.timeout)
                .element('.title*=iPhone 6 White')
                .element('..')
                .click('button*=Buy')
                .waitForExist('button*=Check out', settings.timeout)
                .url(function (err, res) {
                    expect(res.value).toMatch(/\/#\/billing-details$/);
                })
                .call(done);
        });
    });
});
{% endhighlight %}

Let's run our test: first, start your webserver to handle the webshop; then run a
Selenium instance and at last, start `./node_modules/jasmine/bin/jasmine.js`.

In this test case we do the following things:

1. wait untill all the products are loaded (`waitForExist('.title*=iPhone 6 White', settings.timeout)`)
2. select parent element for *iPhone 6 White* product (`element('.title*=iPhone 6 White').element('..')`)
3. click on the *Buy* button within that product' container (`click('button*=Buy')`)
4. wait until the *Create account* button is shown, signalizing we have been redirected to the next page
5. get page' URL and make an assertion to check if we have been redirected to the correct page

When we have done everything we need in our test case, we call the `done` callback to notify Jasmine
we have finished our test. This is how asynchronous tests are made in Jasmine - you define the test
with `it(...)`, providing it with a callback. And when you finish all the asynchronous things in your
test, you just call that callback so Jasmine could switch to the next test.

What's interesting, Jasmine runs test in the order they are defined. So you may assume that everything
you did in previous test cases defines your current state. For example, after our test case, namely
*'when user selects iPhone he gets redirected to the billing-details page'*, we stay on the page
we finished shortly.

Given that, we will not add additional checks or `waitForExist()`s to get to the next steps. So the
next step is filling out billing details:

{% highlight js %}
describe('when user provides his billing details', function () {
    it('he gets redirected to the order-summary page', function (done) {
        var billing = {
            firstName: 'Test',
            lastName: 'User',
            country: 'United Kingdom',
            city: 'London',
            address: '221B, Baker Street',
            zipCode: '12345',
            phone: '9998882245'
        };

        this.client
            .setValue('[name="firstName"]', billing.firstName)
            .setValue('[name="lastName"]', billing.lastName)
            .setValue('[name="address"]', billing.address)
            .setValue('[name="country"]', billing.country)
            .setValue('[name="city"]', billing.city)
            .setValue('[name="zipCode"]', billing.zipCode)
            .setValue('[name="phone"]', billing.phone)
            .click('button*=Check out')
            .waitForExist('h2*=Order summary', settings.timeout)
            .url(function (err, url) {
                expect(url.value).toMatch(/\/#\/order-summary$/);
            })
            .call(done);
    });
});
{% endhighlight %}

The new thing here is how we fill out input fields with *webdriverio* - using the `setValue()` method.

The last page we need to check is `order-summary`. Here we want to check if the order total
is exactly the same, as our iPhone' price. But this is where things got tricky: we have two
different rows with the same value. How do we check if we are looking at the correct one?

We will use *XPath* to get to the correct element:

{% highlight js %}
describe('order summary page', function () {
    it('contains correct order total', function (done) {
        this.client
            .isVisible('//div[contains(@class, "row") and contains(., "iPhone 6 White") and contains(., "$450.00") and ancestor-or-self::div[@ng-repeat]]')
            .then(function (visibility) {
                expect(visibility).toBe(true);
            })
            .isVisible('//div[contains(@class, "row") and descendant::div[contains(., "Total:")] and contains(., "$300.00") and not(contains(@class, "ng-scope"))]')
            .then(function (visibility) {
                expect(visibility).toBe(true);
            })
            .call(done);
    });
});
{% endhighlight %}

Seems complicated, doesn't it?.. But it's not that hard. We used XPath instead of usual selectors
to find the element needed. And here's how one could read it:

say, we have this XPath: `//div[contains(@class, "row") and contains(., "iPhone 6 White") and contains(., "$450.00") and ancestor-or-self::div[@ng-repeat]]`.

This could be split into three parts:

1. path to the element, `//` - it tells browser's engine to look for this element everywhere,
independently on its parent
2. element tag, `div`; it could be *"any"* selector, `*`, but we precised our search
3. element attributes' matchers, or everything in square brackets

Let's take a closer look on those - they are really interesting ones:

1. first we have `contains(@class, "row")`; it tells engine to look for an element, whose `class`
attribute contains *(or "partially matches" if you wish)* `row` string
2. now we have `contains(., "iPhone 6 White")`; it looks for text content inside tag; including its
children
3. speaking of children, we've also got a parent matcher, `ancestor-or-self::div[@ng-repeat]`, which
looks for `<div>` element with `ng-repeat` attribute in the current element or any of its parents
4. you may noticed a concatenation of all those matchers with `and` keyword

## References

Try reading more on XPath on [w3schools](http://www.w3schools.com/xsl/xpath_intro.asp).

Webdriverio API you may find [on its docs page](http://webdriver.io/api.html).

Webshop application, used in this article is [available on GitHub](https://github.com/shybovycha/e2e-webshop).
