---
layout: post
title: What i always forget about when having an interview
date: '2014-04-12T18:52:00+02:00'
tags:
- ruby
- interview
tumblr_url: http://shybovycha.tumblr.com/post/82491993325/what-i-always-forget-about-when-having-an
---

## Ruby interview

I am always being asked at least two questions. Just to verify that I know Ruby basics.

### What is the main difference between `Module` and `Class`?

That is so simple and obvious! Yet it's too easy to forget... The answer is: **you can not instantiate a Module**. See, Modules in Ruby do not have constructors. Yeah, they may contain variables, but they do not have an `initialize` method.


You could define one this way:

```ruby
module Moo
  def initialize(x)
    @x = x
  end
end
```

But when you try to call `Moo.new` you will get a `method missing` error.  When you try to run `Moo#initialize` you will get a `private method called` error.

So yes, there is no way to instantiate Modules.

### What's the difference between `Proc`, `lambda` and `block`?

This is simple enough to remember as the answer contains only a few points:

* `Proc` is an object; `block` is not
* `Proc` does not check the number of arguments; `lambda` does
* `lambda` returns from itself; `Proc` returns from the outer (containing the `Proc` call) method

### What is REST?

The answer on that question hardly depends on what the asking person means.


So, I got two possible _correct answers_:

1. That is the principle of web application development, when the application responds to a request, depending on which HTTP method was provided _(`PUT`, `GET`, `POST`, `DELETE`, `OPTIONS`)_.
2. This is a way of encapsulation Resource and its Handlers. That is a bit hard to explain. Something like _"you have to split your application to Resources"_.

### Is `Module` an ancestor of `Class` or is `Class` a child of `Module`?

This question, actually, may be asked on `Class`, `Module` or `Object` classes.
It proves to be especially tricky when you do not know the answer.

The reality is plain however:

```ruby
irb(main):005:0> Object.superclass
=> BasicObject
irb(main):006:0> Class.superclass
=> Module
irb(main):007:0> Module.superclass
=> Object
irb(main):008:0> BasicObject.superclass
=> nil
```

So, you can even draw a chain:

```haskell
BasicObject => Object => Module => Class
```

## Some hints

* Think out loud. Show an interviewing person how your thought flow. That is the good practice. It shows **your ability to think logically, not your memory**. And you could get to some friendly talk when you say some magic **keyword** or tell something the interviewer is interested in.
* When I am asked about **Rails best practices**, or just creating my web application, I should never forget one core principle: web application controllers _(looking at Rails' MVC)_ should be **thin**. So, the most logic at `Controller`'s action should get or set some data on `Model` and provide a response. Nothing more.
