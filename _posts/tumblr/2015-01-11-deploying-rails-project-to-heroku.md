---
layout: post
title: Deploying Rails project to Heroku
date: '2015-01-11T15:45:52+01:00'
tags:
- rails
- tutorials
- heroku
tumblr_url: http://shybovycha.tumblr.com/post/107790765166/deploying-rails-project-to-heroku
---

First of all, set up the **Heroku Toolbelt**. It is required for all the communications with the server.

The first step of communication with the Heroku servers is logging in. Just run <code>heroku login` and provide your credentials when asked.

Then, you need to create your application or go to the folder where it is located and tune it a bit. Tuning is divided into the following steps:

- tuning the `Gemfile` to include the required gems
- fixing the `database.yml` configurations
- setting the server to handle static assets correctly

But let's create out application on the Heroku servers first: `heroku create [app_name]`. If application name is not provided directly - it will be generated automatically.

Now, our application needs two gems to be included for the `production` gems' group:

```ruby
group :production do
    gem 'pg'
    gem 'rails_12factor'
    gem 'puma'
end
```

The third gem is used as a webserver instead of WebRick, which is default (puma is much, much faster!). The second one is required by Heroku. And the first one is used for PostgreSQL connectivity. If you do not wish to use database - skip it and a few next paragraphs.

Then, let's add the database support for our application. It's done simply, running

```bash
heroku addons:add heroku-postgresql:hobby-dev
```

**Note:** Heroku does not support `sqlite` since some times. This means you are forced to use either PostgreSQL or no database at all (yes, it's possible! Yet, it works for simple or static web applications only). You may want to change this if you would pay for your account. But this tutorial covers only free side of the Heroku deployment.

Now, there are two ways to set database connection options in Rails:

1. set them directly at `config/database.yml` file
2. set the environment variable `DATABASE_URL`

We will cover both cases. For the first one, you will need this section within your `database.yml` file:

```yaml
production:
    adapter: postgresql
    encoding: unicode
    pool: 5
    url: URL_GOES_HERE
```

I will show how to get the `<URL_GOES_HERE>` value in a second. Just keep in mind to replace it with the correct value.

The second option, the environment variable, is set via the Heroku Toolbelt (did I tell you, it is used for almost every deploy operation you will perform?).

First you take the database URL from Heroku server:

```bash
heroku config | grep HEROKU_POSTGRESQL
```

Then, you copy the value you got (it starts with `postgres://`) and run the following:

```bash
heroku config:set DATABASE_URL=URL_GOES_HERE
```

Now let's set our application to serve static assets. It is handy, because it is the easiest way to send images, stylesheets and javascripts to clients. Go to the `config/environments/production.rb` and change both `config.serve_static_assets` and `config.assets.compile` to `true`.

**But beware:** if your `app/assets` files directory contains files of other extensions than Rails' **Sprockets** understands - Rails (and Heroku, particularly) will try to precompile them. And that may cause many troubles. You are recommended either to "teach" Sprockets to skip or precompile those files, **or** you should exclude them from project before deploying to Heroku.

And the last two steps separate us from our goal: first, you should push your project to Heroku' Git repository, it created for you with the application (You do not use Git yet?! How dare you?!..).

Now, if you use database, run migrations with `heroku run rake db:migrate`.

And, finally, see your application running in the browser: `heroku open`.

**Note:** if you are using some assets from the outer web (GoogleFonts, for example) and are seeing your website through the `https` protocol, you should replace all those assets' URL protocols with `https` too.
