---
layout: post
title: About Bower, WebComponents and the all the good
date: '2015-06-22T14:23:43+02:00'
tags:
- rtfm
- tutorial
tumblr_url: http://shybovycha.tumblr.com/post/122161655426/about-bower-webcomponents-and-the-all-the-good
---

<img src="/images/Pg-02-shakespeare-getty.jpeg" class="img-responsive img-sm img-rounded pull-right">

## BEM

These days, many web developers use different methodologies to make their web page source look structured and clean. But all those methodologies only work well until you use third-party libraries or involve a new person in your project.

<!--more-->

The aim of BEM is great - to make the web page use independent blocks with some structured **CSS class names** and have them all described in a nice way in the CSS files. Whilst it may sound like a Holy Grail for the Web, let's take a look at a real-world example of how BEM is used.

There are, in fact, two approaches of BEM.

**The old approach**

Below is an example of vanilla BEM:

{% highlight css %}
.bwelcome__message.hidden {
    display: none;
}

.bwelcome__message {
    color: yellow;
    border-radius: 5px;
    text-align: center;
}

.bwelcome__label {
    font-size: 12pt;
}

.bwelcome__input {
    display: block;
    width: 100%;
}
{% endhighlight %}

And the HTML:

{% highlight html %}
<div class="bwelcome">
<div class="bwelcome__message hiddent">Hello, username!</div>
<div class="bwelcome__label">Enter your name:</div>
<div class="bwelcome__input"></div>
</div>
{% endhighlight %}

So, here we can see that any styles should not overlap with the defined ones unless they are written in a BEM-way for the same element. Nice idea,implementation not so nice… All these `b#{block}__#{element}` classes are way too long, don’t you think? Writing them everywhere - HTML, CSS and JavaScript - is really painful.

**The new approach**

But what about the newer version of BEM? Currently its authors assume you would use BEMJSON + BEMHTML to describe the whole page:

{% highlight js %}
({
     block: 'page',
     title: 'hello',
     head: [
         { elem: 'css', url: '_hello.css' }
     ],
     scripts: [{ elem: 'js', url: '_hello.js' }],
     mods: { theme: 'islands' },
     content: [
         {
             block: 'hello',
             content: [
                 {
                     elem: 'greeting',
                     content: 'Hello, %username%!'
                  }
             ]
         }
     ]
 })
{% endhighlight %}

This is basically the whole page, described in one JSON file, which will then be compiled into an HTML page. Using this approach allows you not only to pack the blocks and elements into one structure, but also to spice it up with some JavaScript, which handles those components.

Sounds great and may be somewhat helpful for new projects. But I believe there's a better solution.

## Introducing WebComponents

Although the implementation of BEM is clumsy, the idea is really great! This is just like encapsulation in OOP - one of the mightiest principles in programming. But what better options are there than BEM?

Recently we have all seen great changes to the Web. We now have ES6, CSS3, HTML5 and all of them give us ultimate power! Support for old browsers is now provided with hacks called *polyfills*. But why should we stop ourselves using the best of what we have in the name of [3% of Internet users](http://gs.statcounter.com/#browser_version-ww-monthly-201405-201505)? Well, if you really do have to continue supporting all of those, you could just stop reading now. Or you could concentrate all your will and try out all these brand-new features in your pet project.

So, the new approach in web design encapsulation uses WebComponents. Rather than reading the introductory texts, let's just check ‘em out:

{% highlight html %}
<template id="welcome-component">
      <style>
        .message {
          background-color: #fea;
          padding: 10px 20px;
          text-align: left;
          border-radius: 5px;
          font-family: Arial;
        }
      </style>

      <div class="message">Hello, !</div>
    </template>

    <div class="shadow-host"></div>
    <div class="message">Non-component message</div>

    <script type="text/javascript">
    var host = document.querySelector('.shadow-host'),
        template = document.querySelector('#welcome-component'),
        shadow = host.createShadowRoot();

    shadow.appendChild(template.content);
    </script>
{% endhighlight %}

This code may not look so good, but it works like a charm!

<p><a href="http://codepen.io/shybovycha/pen/gpGJOV"><figure data-orig-width="660" data-orig-height="79" class="tmblr-full"><img src="https://40.media.tumblr.com/08701b6b73d8dcf7c78f1ba163d7f44f/tumblr_inline_nqciwfuAqT1qh5oee_540.png" alt="image" data-orig-width="660" data-orig-height="79"/></figure></a></p>

This example is a bit ugly - it has both CSS, HTML and JavaScript mixed in a single file, but we’ll deal with it for a short time. Just think of the power you’ve got! You can define your own… well, it’s certainly kinda components! It is a bit uncomfortable to paste onto a page… and they are hardly ready to be used with something like Angular.

Well, yeah, this is as ugly as BEM.

Now, let’s make it spicy!

## Bower

Earlier I did not need to use any package managers except `gem` and `apt-get`. Yet, I was not so stupid as to install all the javascript libraries my project needs with RubyGems. On my projects I had to download the necessary library versions and store them in the `assets/javascripts` directory. Forever. Or at the very least an upgrade was needed.

But then I tried Bower. It was so easy to manage my assets! So I highly recommend this way to you, dear Reader!

To use Bower from scratch on a brand new project, you need to

1. install Bower with `npm`
2. ininitialize your project with `bower init`
3. define dependencies in the `dependencies` object in your new `bower.json` file
4. install them with `bower install`

Then you should end up with all your libraries in the `bower_components` directory. And that’s it! Every package has its own dir. So you can keep track of all the versions etc.

## Polymer

Let’s start cooking our demo with [Polymer](https://www.polymer-project.org/). First, initialize the Bower project and add this section to your fresh `bower.json` file:

{% highlight json %}
"dependencies": {
    "polymer": "Polymer/polymer#^1.0.0"
}
{% endhighlight %}

Then run `bower install`. This will provide you with all the stuff you need.

Now we will move our pretty `welcome-component` to a new place. Create two files:`welcome-component.html` and `index.html`. Fill the last one with [Emmet](http://docs.emmet.io/):

{% highlight jade %}
html:5
{% endhighlight %}

*(you need to hit the “Expand” key, formerly Tab, at the end of this single line while editing the`index.html`* *file in an Emmet-powered editor)*. And add just a single line within the HTML’ “ tag:

{% highlight html %}
<welcome-component></welcome-component>
{% endhighlight %}

Now let’s define our new component in `welcome-component.html`:

{% highlight html %}
 <dom-module id="welcome-component">
    <style>
        .message {
          background-color: #fea;
          padding: 10px 20px;
          text-align: left;
          border-radius: 5px;
          font-family: Arial;
        }
      </style>

      <template>
        <div class="message">Hello, !</div>
      </template>

    <script>
      Polymer({
        is: "welcome-component"
      });
    </script>
  </dom-module>
{% endhighlight %}

Now, import the component at the ” tag of the `index.html` file:

{% highlight html %}
<link rel="import" href="bower_components/polymer/polymer.html" /> <!-- imports Polymer -->
<link rel="import" href="welcome-component.html" /> <!-- imports our component -->
{% endhighlight %}

To run this quickly, you might want to use `http-server` from `npm`:

{% highlight bash %}
http-server -o --cors
{% endhighlight %}

And voila! We’ve just made a nice web component, which could be used really simply. And the code is totally clean!

If you look at the HTML source, you’d see a beautiful DOM structure: we’ve got our `<div class="message">` within the `<welcome-component>` tag. And its style will never affect any other elements, even when you create another `<div>` outside the `<welcome-component>`. Seriously, you can try it!

Yeah, I know we’ve used Polymer and that’s cheating. But think of it as of a temporary hack you will remove when your browser starts supporting custom tags.

## Custom attributes for custom tags

Now, the last thing I’d like to show is the attributes for our custom components. We’d like to pass some data to our brand new components easily, right? So, let’s define a `name` attribute, which we’ll show instead of it!

To do this, we shall use two Polymer features. First of all, let’s define our attribute, adding this section to our `Polymer()` call:

{% highlight js %}
    properties: {
      name: {
        type: String
      }
    }
{% endhighlight %}

Now that we’ve defined our attribute THAT simply, we should use it somehow. Polymer allows us to define a method, which will be called once the component gets inserted into a webpage:

{% highlight js %}
    ready: function() {
      this.querySelector('.message').innerHTML = `Hello, ${this.name}!`
    }
{% endhighlight %}

As long as you're riding IOJS you can use the ES6 string interpolation feature, as in the example above =) Here, the `this` variable is bound to the `<welcome-component>` tag. So in order to change the `.message` tag’s HTML, we need to find it first. For this purpose I’ve used the HTML5 `querySelector` method.

Cool enough, right?

## Conclusion goes here

To end-up this short tutorial on WebComponents, I would like to suggest you to read some other tutorials, especially the [WebComponents website](http://webcomponents.org/articles). And never stop trying something new!
