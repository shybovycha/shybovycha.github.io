---
layout: post
title: Two sides of web application. Part 3
categories: []
tags: []
published: True

---

## Preparation

{% highlight bash %}
gem install napa
napa new napa-test1
cd napa-test1
{% endhighlight %}

napa by default sets ruby to `2.0.0` in both `Gemfile` and `.ruby-version`, so if you are using a newer one
you need to fix both files.

{% highlight bash %}
bundle
napa generate model User name:string email:string password_hash:string
napa generate model Visitor ip:string browser:string
napa generate model Application name:string token:string
{% endhighlight %}

napa uses the buggy version of `mysql2` gem, so fix this in your `Gemfile` setting version to `'~> 0.3.20'`.

{% highlight bash %}
rake db:create
rake db:migrate
napa generate api user
napa generate api session
napa generate api application
napa generate api track
{% endhighlight %}

napa creates default API called `HelloApi` and presenter for it, remove them:

{% highlight bash %}
rm app/apis/hello_api.rb
rm app/representers/hello_representer.rb
{% endhighlight %}

Mount our APIs in `app/apis/applicaiton_api.rb`:

{% highlight ruby %}
mount ApplicationsApi => '/apps'
mount SessionsApi => '/session'
mount UsersApi => '/user'
mount TracksApi => '/track'
{% endhighlight %}

