---
layout: post
title: Rails tips
date: '2015-02-09T10:09:30+01:00'
tags:
- rtfm
- tutorial
- rails
- programming
tumblr_url: http://shybovycha.tumblr.com/post/110527446781/rails-tips
---
<p>Found these on HabraHabr today. Here are some tricks I found usefull.</p>

<h2>Private methods are not actually private</h2>

<p>Let us have class:</p>

{% highlight ruby %}
class Moo
  private

  def self.foo
    puts 'foo'
  end
end
{% endhighlight %}

<p>Here, class method <code>foo</code> is not private:</p>

{% highlight ruby %}
Foo.foo
=> 'foo'
{% endhighlight %}

<h2>Instance with params</h2>

<p>Oftenly there is a need to create a class instance and set it some params (or, maybe, call some methods on it). It's done usually like this:</p>

{% highlight ruby %}
moo = Moo.new
moo.foo = 'foo'
moo.bar
{% endhighlight %}

<p>This can be shortened with the use of `tap` method:</p>

{% highlight ruby %}
moo = Moo.new.tap { |a| a.foo = 'foo'; a.bar }
{% endhighlight %}

<p>Yet, it is more ruby-convenient and ruby-style to do it with the initialization block:</p>

{% highlight ruby %}
class Moo
  attr_accessor :foo

  def initialize(&block)
    yield self if block_given?
  end

  def bar
    puts "bar!"
  end
end

moo = Moo.new do |a|
  a.foo = 'foo'
  a.bar
end

puts moo.foo
{% endhighlight %}

<p>Or even like this:</p>

{% highlight ruby %}
class Moo
  def initialize(&block)
    instance_eval &block if block_given?
  end

  def moo(val = nil)
    @moo = val unless val.nil?
    @moo
  end

  def bar
    puts "bar!"
  end
end

a = Moo.new do
  moo 'moo~!'
  bar
end

puts a.moo
{% endhighlight %}

<h2>Code-dependent migrations</h2>

<p>When you have your migrations using your code, for example, like this:</p>

{% highlight ruby %}
class CreateDataVolumes < ActiveRecord::Migration
  def up
    Data::VOLUMES.times do |volume|
      create_table "data_#{volume}" do |t|
        # ...
      end
    end
  end
end
{% endhighlight %}

<p>you then have a problem when updating your code. In our example, if you remove the constant <code>Data::VOLUMES</code>, you will have to either manually search for all the usages of this constant, or have a really <em>intelliJent</em> IDE ;)</p>

<p>Rather than using your existing code, stub it and copy-and-paste all migration-dependent code to the stubbing class:</p>

{% highlight ruby %}
class CreateDataVolumes < ActiveRecord::Migration
  class Data < AR::Base
    VOLUMES
  end

  def up
    Data::VOLUMES.times do |volume|
      # ...
    end
  end
end
{% endhighlight %}

<p>Example with constant is rather stupid, whilst you may have some more critical code.</p>
