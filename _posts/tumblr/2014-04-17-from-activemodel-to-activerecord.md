---
layout: post
title: From ActiveModel to ActiveRecord
date: '2014-04-17T12:47:00+02:00'
tags:
- ruby
- metaprogramming
tumblr_url: http://shybovycha.tumblr.com/post/82983429638/from-activemodel-to-activerecord
---

This short article covers some steps you need to implement to get your own `ActiveRecord` implementation.

Sample `ActiveModel` looks like this:

```ruby
class Posting
  include ActiveModel::Validations

  attr_accessor :id, :title, :body, :tags

  validates :title, :presence => true
  validates :body, :presence => true

  def initialize(attributes = {})
    # ...
  end

  def save
    set_default_values

    # ...
  end

  def self.create(attributes = {})
    new(attributes).save
  end

  protected

  def set_default_values
    # ...
  end
end
```

First trouble is that `ActiveModel` does not provide any attribute access API. Or I did not google enough. So we need to create our own!

Let us have an instance variable `@attributes` where we will store our model' data. We need to define getter and setter methods for all the attributes of our model. This may be done with the `attr_accessor` method. But when user sets the value for some attribute, we should store that in our `@attributes` variable. And here is the first step to our black magic: **we will override the `attr_accessor` method**.

```ruby
module ActiveAttributes
  def attr_accessor(*args)
    args.each do |k|
      define_method("#{k}".to_sym) { @attributes[k.to_sym] }
      define_method("#{k}=".to_sym) { |value| @attributes[k.to_sym] = value }
    end
  end
end
```

I just wrapped the code into a single module. Remember: when you include the module in a class, all the module' methods become class methods.

Now we will include this module **before `attr_accessor` calls**. But beware: you need to declare an `@attributes` instance variable in the constructor!

And let's just agree with the following convention: **all our attribute names should be symbols**.

```ruby
class Posting
  include ActiveModel::Validations
  include ActiveAttributes

  attr_accessor :id, :title, :body, :tags

  validates :title, :presence => true
  validates :body, :presence => true

  def initialize(attributes = {})
    @attributes = {}

    # ...
  end

  def save
    set_default_values

    # ...
  end

  def self.create(attributes = {})
    new(attributes).save
  end

  protected

  def set_default_values
    # ...
  end
end
```

Now, we can implement our constructor. We now have all the attributes' getters and setter and thus we can simply call them in our constructor:

```ruby
class Posting
  include ActiveModel::Validations
  include ActiveAttributes

  attr_accessor :id, :title, :body, :tags

  validates :title, :presence => true
  validates :body, :presence => true

  def initialize(attributes = {})
    @attributes = {}

    attributes.symbolize_keys.each do |k, v|
      v.symbolize_keys! if v.is_a? Hash

      send("#{k}=", v) if respond_to?("#{k}=".to_sym)
    end
  end

  def save
    set_default_values

    # ...
  end

  def self.create(attributes = {})
    new(attributes).save
  end

  protected

  def set_default_values
    # ...
  end
end
```

Now let's implement some basic model persisting. First, we should not forget about our **validations** and add `valid?` test to the `save` method.

Let's say our `save` method should return the model instance. Thus, we should put the model' data into the database and get the `id` for that data (if we put the data with the `INSERT` statement).

So there is an important caveat: **in order to get the correct model `id`, you need to get it from database in the same transaction as the update/insert statement**. The `mysql2` gem does support multiple query statements in a single transaction. But to perform such a query, you will need to set the `MULTI_STATEMENTS` flag when creating a `Mysql2::Connection` instance.

```ruby
def save
  set_default_values

  return self unless valid?

  @connection = Mysql2::Client.new({ flags: Mysql2::Client::MULTI_STATEMENTS }.merge(...))

  # ...

  self
rescue
  self
ensure
  @connection.close
end
```

Here I used the instance variable `@connection` to make it available within the `rescue` and `ensure` statements.

Now we will use our instance variable, `@attributes` to create an SQL query:

```ruby
def save
  set_default_values

  return self unless valid?

  @connection = Mysql2::Client.new({ flags: Mysql2::Client::MULTI_STATEMENTS }.merge(...))

    if @attributes[:id].blank?
    columns = @attributes.keys.map { |k| "`#{ k.to_s }`" }.join ','
    values = @attributes.values.map do |v|
      if v.nil?
        'NULL'
      else
        "'#{ ActionController::Base.helpers.sanitize(v.to_s) }'"
      end
    end.join ','

    query = "INSERT INTO postings#{ volume } (#{ columns }) VALUES (#{ values })"
  else
    mapping = @attributes.map { |k, v| "`#{ k.to_s }` = #{ v.nil? ? 'NULL' : "'#{ ActionController::Base.helpers.sanitize(v) }'" }" }.join ','

    query = "UPDATE postings#{ volume } SET #{ mapping } WHERE id = #{ @attributes[:id] }"
  end

  self
rescue
  self
ensure
  @connection.close
end
```

I used the `ActionController::Base.helpers.sanitize` helper method to escape the query parameters.

Now we should simply wrap our query into a transaction and get an `id` from the database.

```ruby
def save
  set_default_values

  return self unless valid?

  @connection = Mysql2::Client.new({ flags: Mysql2::Client::MULTI_STATEMENTS }.merge(...))

  if @attributes[:id].blank?
    columns = @attributes.keys.map { |k| "`#{ k.to_s }`" }.join ','
    values = @attributes.values.map do |v|
      if v.nil?
        'NULL'
      else
        "'#{ ActionController::Base.helpers.sanitize(v.to_s) }'"
      end
    end.join ','

    query = "INSERT INTO postings#{ volume } (#{ columns }) VALUES (#{ values })"
  else
    mapping = @attributes.map { |k, v| "`#{ k.to_s }` = #{ v.nil? ? 'NULL' : "'#{ ActionController::Base.helpers.sanitize(v) }'" }" }.join ','

    query = "UPDATE postings#{ volume } SET #{ mapping } WHERE id = #{ @attributes[:id] }"
  end

  query = "START TRANSACTION; #{ query }; SELECT LAST_INSERT_ID() AS id; COMMIT;"

  @connection.query(query)

  while @connection.next_result
    result = @connection.store_result.to_a rescue nil

    @attributes[:id] = result.first['id'] if result.present? and result.first.present? and result.first.has_key? 'id'
  end

  self
rescue
  self
ensure
  @connection.close
end
```

Quite big method, sure. Yet, it performs all the <em>UPDATEs</em> and <em>INSERTs</em> for us.

Let's add some attribute with the default value, `created_at` and check how the whole class works:

```ruby
require 'date'

# ...

attr_accessor :created_at

# ...

protected

def set_default_values
  @attributes[:created_at] = DateTime.now
end
```

And the test:

```ruby
p = Posting.new title: "Hello, ActiveModel!", body: "Hello, Database!"

p.save

puts p.created_at
```
