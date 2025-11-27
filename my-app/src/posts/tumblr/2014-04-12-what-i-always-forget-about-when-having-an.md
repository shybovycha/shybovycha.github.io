---
layout: post
title: What i always forget about when having an interview
date: '2014-04-12T18:52:00+02:00'
tags:
- ruby
- interview
tumblr_url: http://shybovycha.tumblr.com/post/82491993325/what-i-always-forget-about-when-having-an
---
<h2>The Ruby interview</h2>

<p>I am always being asked at least two questions. Just to verify that I know Ruby basics.</p>

<ul><li><strong>What is the main difference between Module and Class?</strong></li>
</ul><p>That is so simple and obvious! Yet it&rsquo;s too easy to forget&hellip; The answer is: <strong>you can not instantiate a Module</strong>. See, Modules in Ruby do not have constructors. Yeah, they may contain variables, but they do not have an <code>initialize</code> method.</p>

<p>You could define one this way:</p>

```ruby
module Moo
  def initialize(x)
    @x = x
  end
end
```

<p>But when you try to call <code>Moo.new</code> you will get a <code>method missing</code> error.  When you try to run <code>Moo.initialize</code> you will get a <code>private method called</code> error.</p>

<p>So yes, there is no way to instantiate Modules.</p>

<ul><li><strong>What&rsquo;s the difference between Proc, lambda and block?</strong></li>
</ul><p>This is simple enough to remember as the answer contains only a few points:</p>

<ol><li><code>Proc</code> is an object; <code>block</code> is not</li>
<li><code>Proc</code> does not check the number of arguments; <code>lambda</code> does</li>
<li><code>lambda</code> returns from itself; <code>Proc</code> returns from the outer (containing the <code>Proc</code> call) method</li>
</ol><ul><li><strong>What is REST (application)?</strong></li>
</ul><p>The answer on that question hardly depends on what the asking person means.</p>

<p>So, I got two possible <em>correct answers</em>:</p>

<p>a. That is the principle of web application development, when the application responds to a request, depending on which HTTP method was provided <em>(PUT, GET, POST, DELETE, OPTIONS)</em>.</p>

<p>b. This is a way of encapsulation Resource and its Handlers. That is a bit hard to explain. Something like <em>&ldquo;you have to split your application to Resources&rdquo;</em>.</p>

<ul><li><strong>Does Module is the ancestor of Class or does the Class is the child of Module?</strong></li>
</ul><p>This question, actually, may be asked on <strong>Class</strong>, <strong>Module</strong> or <strong>Object</strong> classes. This question is interesting when you do not know the answer.</p>

<p>The reality is plain however:</p>

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

<p>So, you can even draw a chain:</p>

```haskell
BasicObject => Object => Module => Class
```

<h2>Some hints</h2>

<ul><li><p>Think oral. Show an interviewing person how your thought flow. That is the good practice. It shows that you <strong>can think</strong> not just <strong>remember</strong>. And you could get to some friendly talk when you say some magic <em>keyword</em> or tell something the interviewer is interested in.</p></li>
<li><p>When I am asked of <strong>Rails best practices</strong>, or just creating my web application, I should never forget one core principle: web application controllers <em>(looking at Rails&rsquo; MVC)</em> should be <strong>thin</strong>. So, the most logic at <strong>Controller</strong>&rsquo;s action should get or set some data on <strong>Model</strong> and provide a response. Nothing more.</p></li>
</ul>
