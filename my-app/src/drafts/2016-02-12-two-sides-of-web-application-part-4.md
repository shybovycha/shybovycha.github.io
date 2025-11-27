---
layout: post
title: "Two sides of web application. Part 4: backend in Ruby"
date: '2016-02-12T15:47:00+01:00'
tags: [rest, backend, frontend, data, api, web-development, database]
---

<div class="row">
    <div class="col-md-6 col-xs-12">
        {% include references/two-sides-of-web-application.html %}
    </div>
    <div class="col-md-6 col-xs-12 text-xs-center text-md-right">
        <img src="/images/two-sides-of-web-application/a305-1-compressed.webp" loading="lazy" class="img-responsive" style="max-height: 150px" alt="Funny image" />
    </div>
</div>

## Overview

In this part we'll build an API which will provide data consumed by the front-end we've built previously. It should work with the database, which we'll design now, so we will have to have some kind of data layer.

## API endpoints

First things first, let's recap: what API endpoints our front-end will be using?

* User
  * sign up (create user account)
  * sign in (verify if the user exists and the credentials sent by front-end are correct)
  * update account details
* Application
  * create application
  * update application details
  * delete application
  * get application details (together with usages data aka analytics)
  * track application usage

We can see two main *resources* here - `User` (or `Account`) and `Application`.
We will probably not make them perfectly RESTful, since we won't have some of the methods (CRUD - Create, Read, Update, Delete) for `User` and we will have an extra method for `Application` (the tracking one, which should possibly be a separate entity). But the API we will build shall respond with a correct HTTP status and the requests shall go to the truly RESTful paths.

## Database schema

According to the resources we've defined previously, it is obvious (hopefully) what entities we will have in database: `User` and `Application` being the larger ones and `Usage` being the tiny, but most important one.

The relationships between entities should also be quite transparent: `User` will have many `Applications` and each `Application` will have many `Usages`.

Hence the database schema will look like this:

```
User:
  - email
  - password_hash
  - name

Application:
  - token
  - name

Usage:
  - country
  - browser
```

To enable the one-to-many relationships, "slaves" or "children" entities will have a reference to the "master" or "parent" entity:

```
User:
  - id
  - email
  - password_hash

Application:
  - id
  - user_id
  - token
  - name

Usage:
  - id
  - application_id
  - country
  - browser
```

We can now define these entities in terms of a database, which could be either a relational one (supported by `*SQL` - MySQL, PostgreSQL, MSSQL, Oracle, you name it) or a NoSQL one (like Redis or Cassandra or MongoDB).

For the sake of simplicity, I will stick to the more conventional relational DBs for now:

```sql
create table users (id int, email varchar(255) not null, password_hash varchar(512) not null, name varchar(255)) primary key (id);

create table applications (id int, user_id int not null, token varchar(255) not null, name varchar(255) not null) primary key (id);

create table usages (id int, application_id int not null, country varchar(255), browser varchar(255), created_at datetime) primary key (id);
```

Here we could also define foreign keys to ensure the integrity of the data, but we will skip it for now and let server care about that as well as the generating unique IDs for entities.

## Data layer with Sequel

One of the simplest database access libraries I've ever used is [sequel](http://sequel.jeremyevans.net/) by incredible Jeremy Evans. I saw his talk on RubyC back in 2015 and he is really awesome guy. Just as his library:

```ruby
require 'sequel'

DB = Sequel.sqlite

DB.create_table :users do
  primary_key :id

  String :email, null: false
  String :name, null: false
  String :password_hash, null: false
end

DB.create_table :applications do
  primary_key :id

  Int :user_id, null: false
  String :token, null: false
  String :name, null: false
end

DB.create_table :usages do
  primary_key :id

  Int :application_id, null: false
  String :country
  String :browser
  DateTime :created_at
end
```

And we've just created a schema for our whole database!

The only trick is: defining a connection to a database also requires you to install the corresponding database adapter gem. In case of `sqlite` it would be `sqlite3` gem. For PostgreSQL you will have to add the `pg` gem.

Good news is: you will only have to add it to your `Gemfile` and specify the connection options as parameters to the `Sequel.<database_name>` call, Sequel will do everything else for you.

Now to use it we will have to get a reference to the table we want to operate with:

```ruby
def encode_password(password)
  SALT = 'my_non_secure_string'

  Digest::SHA256.hexdigest(password + SALT)
end

users = DB[:users]

users.insert(name: 'User #1', email: 'user@users.com', password_hash: encode_password('password'))

# we want to encode passwords the same way when we created them
user1 = users.select(:id).where(email: 'user@users.com', password_hash: encode_password('password'))
```

The syntax is pretty simple. For instance, to get only the `id` column values from the table we use the `table.select(:id)` method.

To select the rows matching multiple column-value conditions (using the equality operator) we use the `table.where(column1: value1, column2: value2)` method.

But to use a more complex query condition we use something called *Virtual rows*: `table.where { column1 > 1 & column2 < 4 }`.
Or with a more verbose syntax: `table.where { |r| r['column1'] > 1 & r['column2'] < 4 }`.

## Sinatra for defining slick APIs

The simplest way (to my knowledge so far) to create an API in Ruby is to use [Sinatra](http://sinatrarb.com/) framework. Check this out:

```ruby
require 'sinatra'
require 'json'

get '/data' do
  content_type :json

  { status: 'success', data: 'Successful data retrieval!' }.to_json
end
```

Running is as simple as `ruby file.rb`. To test this you can use CURL or the mighty `rest-client` gem:

```ruby
require 'rest-client'

RestClient.get 'http://localhost:4567/data'
```

Or with some more details:

```ruby
response = RestClient.get 'localhost:4567/data'

response.body
```

As you can see (the code is pretty much self-explanatory, and that's what I love Ruby for!), we set the `Content-Type` header (the call to `content_type` helper) to `application/json` and respond with a JSON string in a body (the return value of a function).

We can also utilize the `Sinatra::JSON` helpers from the `sinatra-contrib` gem (**do not forget to add it to your `Gemfile`**):

```ruby
require 'sinatra'
require 'sinatra/json'

get '/data' do
  json status: 'success', data: 'Successful data retrieval!'
end
```

Using parameters from the URL is quite simple:

```ruby
get '/data/:id' do
  "Passed ID: #{params['id']}"
end
```

Or using the block' arguments:

```ruby
get '/data/:id1/:id2' do |id1, id2|
  "ID2: #{id2}, ID1: #{id1}"
end
```

Inside the route handler you will have an access to the `request` object, which will hold all the data about the request. The most interesting (for us, in the context of this tutorial) properties are: `user_agent`, `referrer`, `body` and `ip`.

Different HTTP methods could be handled by just using the corresponding method:

```ruby
get '/' do
end

post '/' do
end

put '/' do
end

delete '/' do
end
```

In order to group some endpoints under a parent route we can use the `sinatra/namespace` module from the `sinatra-contrib` gem:

```ruby
require 'sinatra/namespace'

namespace '/users' do
  namespace '/account' do
    get '/' do
      "GET /users/account"
    end

    post '/' do
      "POST /users/account"
    end
  end

  post '/' do
    "POST to /users"
  end
end
```

Now our API can be defined pretty easily:

```ruby
require 'sinatra'
require 'sinatra/json'
require 'sinatra/namespace'

namespace '/users' do
  # GET /users
  # get user account details
  # we rely on the 'email' and 'password' query params
  # we will respond with user object; it includes USER_ID, which we will be used later on to operate on applications
  get '/' do
    # email, password = params['email'], params['password']
  end

  # POST /users
  # update user account details
  # we will also rely on the 'email' and 'password' query params
  post '/' do
    # email, password = params['email'], params['password']
  end

  # PUT /users
  # create user account
  # we will use the params from the query params as well, because that's simply easier
  put '/' do
    # email, password, name = params['email'], params['password'], params['name']
  end
end

namespace '/applications' do
  # PUT /applications
  # create application
  # we will rely on USER_ID param to assign it to a user
  # the response will contain the application' TOKEN
  put '/' do
    # user_id = params['user_id']
    # app_name = params['name']
  end

  # specific application endpoints
  namespace '/:token' do
    # GET /applications/:token/track
    # simplest way to track application usage without the need to use AJAX requests
    get '/track' do
    end

    # GET /applications/:token
    # get application details
    get '/' do
    end

    # POST /applications/:token
    # update application details
    post '/' do
    end

    # DELETE /applications/:token
    # delete application
    delete '/' do
    end
  end
end
```

## Putting it all together

We have already defined the database tables before. We already have the skeleton for our API endpoints. Now the last thing left to do is to actually fill those endpoints with some business logic.

```ruby
namespace '/users' do
  # get user account details
  get '/' do
    email = Sequel.lit(params['email'])
    password_hash = encode_password(params['password'])

    user_ids = DB[:users].select(:id).where(email: email, password_hash: password_hash)

    if user_ids.size != 1
      status 401
    else
      json user_id: user_ids
    end
  end

  # update user account details
  post '/' do
    begin
      email = params['email']
      password_hash = encode_password(params['password'])

      new_email, new_name = Sequel.lit(params['new_email']), Sequel.lit(params['new_name'])
      new_password_hash = encode_password(params['new_password'])

      DB[:users].where(Sequel.lit('email = ? AND password_hash = ?', email, password_hash).update(email: new_email, password_hash: new_password_hash, name: new_name)

      status 200
    rescue
      status 400
    end
  end

  # create user account
  put '/' do
    begin
      # prevent SQL injections
      email, name = Sequel.lit(params['email']), Sequel.lit(params['name'])

      # this one is encoded, so no need to worry about SQL injection
      password_hash = encode_password(params['password'])

      DB[:users].insert(email: email, password_hash: password_hash, name: name)
      status 201
    rescue
      status 400
    end
  end
end

namespace '/applications' do
  # create application
  put '/' do
    begin
      user_id = params['user_id'].to_i
      name = Sequel.lit(params['name'])
      token = generate_token()

      DB[:applications].insert(user_id: user_id, name: name, token: token)

      status 201
      json token: token
    rescue
      status 400
    end
  end

  # specific application endpoints
  namespace '/:token' do
    # simplest way to track application usage without the need to use AJAX requests
    get '/track' do
      begin
        application_ids = DB[:applications].select(:id).where(Sequel.lit('token = ?', params['token']))

        if application_ids.size != 1
          status 400
        else
          DB[:usages].insert(application_id: application_ids[0], name: Sequel.lit(params['name']), created_at: Sequel.lit('NOW()'))

          status 201
        end
      rescue
        status 400
      end
    end

    # get application details
    get '/' do
      begin
        applications = DB[:applications].where(Sequel.lit('token = ?', params['token']))

        if applications.size != 1
          status 400
        else
          json applications[0]
        end
      rescue
        status 400
      end
    end

    # update application details
    post '/' do
      begin
        DB[:applications].where(Sequel.lit('token = ?', params['token'])).update(name: Sequel.lit(params['new_name']))

        status 200
      rescue
        status 400
      end
    end

    # DELETE /applications/:token
    # delete application
    delete '/' do
      begin
        DB[:applications].where(Sequel.lit('token = ?', params['token'])).delete()

        status 200
      rescue
        status 400
      end
    end
  end
end
```
