---
layout: post
title: 'Erlang example 2.0'
date: '2020-11-07T11:42:24+9:00'
---

Quite some time ago I've published a <a href="{% post_url tumblr/2015-01-28-erlang-practice %}">blogpost about Erlang</a>. It claimed to present a *short intro to distributed programming in Erlang*. But it turned to be a very simple communication application, nothing super-exciting.

In this post I would like to elaborate more on the topic of Erlang, for a number of reasons:

* it is a pretty simple language itself
* the distributed systems topic gets more and more of my attention these days
* when I was looking at Erlang, I would love to see a more advanced tutorial myself _(more practical things and more Erlang platform features showcased)_

<!--more-->

## Language features

Kicking off with the language features, let's discover few data structures available out of the box.

I was thinking for few a while about what could be showcased in Erlang. I think the system we'll develop closer to the end of this blog
is quite nice.

### Tuples

As you might remember from the [quick introduction to Erlang]({% post_url 2020-01-10-erlang-in-5-minutes %}), tuples are defined like this:

```erlang
X = { elt1, elt2, elt3, elt5 }.
```

Now, since there are no datatypes or alike in Erlang, we can use tuples to denote complex data structures, like trees, for example:

```erlang
Tree = { tree, Parent, Left, Right }.
```

With this, let us create a module to work with binary trees:

```erlang
-module(binary_tree).

-export([create_tree/1, insert_into_tree/2]).

create_tree(N) -> { tree, N, nil, nil }.

insert_into_tree({ tree, Parent, Left, nil }, Value)
  when Value >= Parent ->
    NewNode = create_tree(Value),
    { tree, Parent, Left, NewNode };

insert_into_tree({ tree, Parent, nil, Right }, Value)
  when Value < Parent ->
    NewNode = create_tree(Value),
    { tree, Parent, NewNode, Right };

insert_into_tree({ tree, Parent, Left, Right }, Value)
  when Value >= Parent ->
    { tree, Parent, Left, insert_into_tree(Right, Value) };

insert_into_tree({ tree, Parent, Left, Right }, Value)
  when Value < Parent ->
    { tree, Parent, insert_into_tree(Left, Value), Right }.
```

The idea behind `insert_into_tree/2` is that every call has to return the copy of a current tree with a recursively modified leaf.

Now, to test the binary tree, we can define a function that checks if a node is in tree or not:

```erlang
is_in_tree(nil, Value) -> false;

is_in_tree({ tree, nil, Left, Right }, Value) -> false;

is_in_tree({ tree, Parent, Left, Right }, Value)
  when Parent =:= Value -> true;

is_in_tree({ tree, Parent, Left, Right }, Value)
  when Value >= Parent -> is_in_tree(Right, Value);

is_in_tree({ tree, Parent, Left, Right }, Value)
  when Value < Parent -> is_in_tree(Left, Value).
```

You might want to compile the module now, by running

```bash
$erlc binary_tree.erl
```

We can run the program to construct a tree and check if some element is in there:

```erlang
-import(binary_tree).

start() ->
  Tree = insert_into_tree(insert_into_tree(insert_into_tree(insert_into_tree(create_tree(0), 15), 10), 22), 12),
  X1 = 7,
  X2 = 12,
  X1Present = is_in_tree(Tree, 7),
  X2Present = is_in_tree(Tree, 12),
  io:fwrite("Tree: ~p~n", [Tree]),
  io:fwrite("~p in tree (~p)~n", [X1, X1Present]),
  io:fwrite("~p in tree (~p)~n", [X2, X2Present]).
```

This is a bit cumbersome, so let's use the `lists:foldl/3` function together with a list of values to construct the tree in a more convenient way:

```erlang
-import(lists, [foldl/3]).

start() ->
  Values = [15, 12, 20, 22, 10, 3, 5, 1, 4],
  Tree = lists:foldl(fun(Elt, Acc) -> insert_into_tree(Acc, Elt) end, create_tree(0), Values)
  % ...
  .
```

### Records

There is a mechanism in Erlang called _records_. It is basically a syntactic sugar around tuples, just as we have used them above, but it gives a little bit more clarity for pattern matching when writing code:

```erlang
-record(tree, { value, left, right }).

create_tree(N) -> #tree{ value = N, left = nil, right = nil }.

insert_into_tree(#tree{ value = Parent, left = Left, right = nil }, Value)
  when Value >= Parent ->
    NewNode = create_tree(Value),
    #tree{ value = Parent, left = Left, right = NewNode };

insert_into_tree(#tree{ value = Parent, left = nil, right = Right }, Value)
  when Value < Parent ->
    NewNode = create_tree(Value),
    #tree{ value = Parent, left = NewNode, right = Right };

insert_into_tree(#tree{ value = Parent, left = Left, right = Right }, Value)
  when Value >= Parent ->
    #tree{ value = Parent, left = Left, right = insert_into_tree(Right, Value) };

insert_into_tree(#tree{ value = Parent, left = Left, right = Right }, Value)
  when Value < Parent ->
    #tree{ value = Parent, left = insert_into_tree(Left, Value), right = Right }.

is_in_tree(nil, _) -> false;

is_in_tree(#tree{ value = nil, left = _, right = _ }, _) -> false;

is_in_tree(#tree{ value = Parent, left = _, right = _ }, Value)
  when Parent =:= Value -> true;

is_in_tree(#tree{ value = Parent, left = _, right = Right }, Value)
  when Value >= Parent -> is_in_tree(Right, Value);

is_in_tree(#tree{ value = Parent, left = Left, right = _ }, Value)
  when Value < Parent -> is_in_tree(Left, Value).
```

Well, it does make pattern matching more explicit, but the code has just blown up!
Luckily, Erlang does provide a syntactic sugar to read a specific field of a record and to update specific record' fields:

```erlang
insert_into_tree(Tree, Value)
  when Tree#tree.right =:= nil, Value >= Tree#tree.value -> % read specific fields from Tree, namely - right and value
    NewNode = create_tree(Value),
    Tree#tree{ right = NewNode }; % update only the tree.right value, leave the rest values of Tree unchanged
```

This makes a module a bit shorter again:

```erlang
-record(tree, { value, left, right }).

create_tree(N) -> #tree{ value = N, left = nil, right = nil }.

insert_into_tree(Tree, Value)
  when Tree#tree.right =:= nil, Value >= Tree#tree.value ->
    NewNode = create_tree(Value),
    Tree#tree{ right = NewNode };

insert_into_tree(Tree, Value)
  when Tree#tree.left =:= nil, Value < Tree#tree.value ->
    NewNode = create_tree(Value),
    Tree#tree{ left = NewNode };

insert_into_tree(Tree, Value)
  when Value >= Tree#tree.value ->
    Tree#tree{ right = insert_into_tree(Tree#tree.right, Value) };

insert_into_tree(Tree, Value)
  when Value < Tree#tree.value ->
    Tree#tree{ left = insert_into_tree(Tree#tree.left, Value) }.

is_in_tree(nil, _) -> false;

is_in_tree(Tree, Value)
  when Tree#tree.value =:= Value ->
    true;

is_in_tree(Tree, Value)
  when Value >= Tree#tree.value ->
    is_in_tree(Tree#tree.right, Value);

is_in_tree(Tree, Value)
  when Value < Tree#tree.value ->
    is_in_tree(Tree#tree.left, Value).
```

### Maps

Well, having records is good and nice. But what if we want a hashmap?
Consider a trie data structure, where it has a value and a map of characters to nodes.
Technically, we could have a list of nodes and iterate over them whenever we need to access a specific child.
But that would be a waste of time.

Luckily, Erlang does provide the `maps` module:

```erlang
-module(trie).

-import(lists, [foldl/3]).
-import(maps, [is_key/2, get/2, put/3]).

-export([new/0, insert/2, contains/2]).

-record(trie, { stop, children }).

new() -> #trie{ stop = false, children = #{} }.

insert(Trie, []) -> Trie#trie { stop = true };

insert(Trie, [Char|Chars]) ->
  Children = Trie#trie.children,
  IsChild = maps:is_key(Char, Children),
  if
    IsChild == true ->
      SubTrie = maps:get(Char, Children),
      NewSubTrie = insert(SubTrie, Chars),
      NewChildren = maps:put(Char, NewSubTrie, Children),
      Trie#trie { children = NewChildren };
    true ->
      SubTrie = new(),
      NewSubTrie = insert(SubTrie, Chars),
      NewChildren = maps:put(Char, NewSubTrie, Children),
      Trie#trie { children = NewChildren }
  end.

contains(Trie, []) -> Trie#trie.stop;

contains(Trie, [Char|Chars]) ->
  Children = Trie#trie.children,
  IsChild = maps:is_key(Char, Children),
  if
    IsChild == true ->
      SubTrie = maps:get(Char, Children),
      contains(SubTrie, Chars);
    true ->
      false
  end.
```

Quite unfortunately, pretty much none of the handy maps syntax is implemented yet.

## Platform features

Let's start with few simple but practical examples.

### Web-server

Erlang comes packed with a lot of features. One of them is bundled web-server. Some time ago I've published a post about a very simple Tetris game, which I've crafted one evening. The thing is: that game is nothing more than a single HTML page. What we can do now is start a web server, which will listen for connections on `8080` port and serve them a static file with the game.

The code is pretty simple:

```erlang
-module(simple_web_server).
-export([ run_server/0, stop_server/1 ]).

run_server() ->
  inets:start(),

  inets:start(httpd, [{ port, 8080 }, { server_name, "httpd_test" }, { server_root, "." }, { document_root, "." }, { bind_address, any }]).

stop_server(Pid) ->
  inets:stop(httpd, Pid).
```

But modern web applications require a server that will have some sort of an API. Take REST for example - when a URL of a specific format with specific HTTP method (`GET`, `PUT`, `POST`, `DELETE`, etc.) is hit on the server, some behaviour is expected. For instance, `curl -X GET http://server.address/posts` request is expected to return a list of all `Post` entities (whatever that means in the context of an application), whereas `curl -X PUT http://server.address/posts -d '{ "title": "post #1", "content": "blah" }' -H "Content-Type: application/json"` is expected to create a `Post` entity with fields `title` and `content` from the request.

The bundled Erlang HTTP server, `httpd` kind of allows for that, except the URLs will be in a format `cgi-bin/erl/mymodule:mymethod`, which is not exactly what we want.
But just for the sake of example, here's how it would look like implemented with httpd:

```erlang
-module(posts).

-export([get/3, all/3, create/3]).

get(SessionID, Env, Input) ->
  mod_esi:deliver(SessionID, "Content-Type:application/json\r\n\r\n"),
  mod_esi:deliver(SessionID, do_get(Input)).

do_get(Input) ->
  QueryParams = uri_string:dissect_query(Input),

  { _IdParamName, Id } = lists:search(fun({ Key, _Value }) -> Key == "id" end),

  ["{\"id\":1,\"title\": \"post 1\", \"content\": \"blah\"}"].

all(SessionID, Env, Input) ->
  mod_esi:deliver(SessionID, "Content-Type:application/json\r\n\r\n"),
  mod_esi:deliver(SessionID, do_all()).

do_all() ->
  ["[{\"title\": \"post 1\", \"content\": \"blah\"}]"].

create(SessionID, Env, Input) ->
  mod_esi:deliver(SessionID, "Content-Type:application/json\r\n\r\n"),
  mod_esi:deliver(SessionID, do_create(Input)).

do_create(Input) ->
  % here the Input variable will contain the POST request' body, which will most likely be a JSON, which we also will need to parse
  ["Status: 204 No Content"].
```

As you can see, this is a pretty complex example, since the API of `httpd` module is rather low-level - we have to explicitly form a HTTP response and send it back to clients.
Although not impossible, it is just not so pleasant development experience as it could be. Read on to see how it could be significantly improved.

### Mnesia

Erlang is bundled with a distributed real-time database called Mnesia!

```erlang
-module(visit_tracker).
-export([
  init/0,
  create_schema/0,
  create_account/2,
  find_account_by_email/1
]).

-record(visit, { site_id, ip, browser, location }).
-record(site, { id, account_id, name, visits }).
-record(account, { id, email, sites }).

init() ->
  mnesia:create_schema([node()]),
  mnesia:start().

create_schema() ->
  mnesia:create_table(visits, [ { attributes, record_info(fields, visit) }, { record_name, visit } ]),
  mnesia:create_table(sites, [ { type, bag }, { attributes, record_info(fields, site) }, { record_name, site } ]),
  mnesia:create_table(accounts, [ { type, bag }, { attributes, record_info(fields, account) }, { record_name, account } ]).

%% temporarily forcing to put ID by hand
create_account(Id, Email) ->
  Txn = fun() ->
    %% record creation syntax is similar to dictionary / map creation syntax: `#record_name{ key = Value }`
    mnesia:write(accounts, #account{ id = Id, email = Email, sites = [] }, write)
  end,

  mnesia:transaction(Txn).

create_site(Account, Id, Name) ->
  AccId = Account#account.id,

  Txn = fun() ->
    [ _ ] = mnesia:read(accounts, AccId),

    %% mnesia:write(Account),

    mnesia:write(sites, #site{ account_id = AccId, id = Id, name = Name, visits = [] }, write)
  end,

  mnesia:transaction(Txn).

create_visit(Site, Ip) ->
  create_visit(Site, Ip, unknown, unknown).

create_visit(Site, Ip, Browser) ->
  create_visit(Site, Ip, Browser, unknown).

create_visit(Site, Ip, Browser, Location) ->
  %% retrieving ID field from the Site parameter
  SiteId = Site#site.id,

  Txn = fun() ->
    [ _ ] = mnesia:read(sites, SiteId),

    %% since table name is different to record name, we have to pass table name as the first argument
  end,

  mnesia:transaction(Txn).

find_account_by_email(Email) ->
  Txn = fun() ->
    %% select semantics:
    %%
    %%   mnesia:select(Table_name, [ Query, Conditions, ResultMapping ])
    %%
    %% here,
    %%  * Query defines the generic pattern to match agains
    %%  * Conditions or Guard is a list of tuples defining the clauses: { operator, field, value }: { '>', 'id', 10 }
    %%  * ResultMapping is what will be returned; you can use the pattern matching from the Query param here
    %%
    %% map / dictionary creation: #{ key => value }

    [ Acc ] = mnesia:select(accounts, [ { #account{ id='$1', email=Email, sites='$2' }, [], [ #{ id => '$1', email => Email, sites => '$2' } ] } ]),

    Acc
  end,

  mnesia:transaction(Txn).
```

## Ecosystem

Using [Rebar 3](https://hex.pm/) (or `Hex` for Elixir) is relatively easy. Yet it opens access to thousands of packages available out there.

### Creating a project

To create a project with rebar3 support, you can now use `rebar3 new <template> <app-name>`. For just an application, you can use `app` template name.
For libraries - use `lib`. Simple!

To add a dependency, edit the `rebar.config` file in the application directory and change the `deps` dictionary:

```erlang
{deps, [
  {epsql, "~> 4.6.0"} % PostgreSQL package
]}.
```

Then, since a single project can have multiple applications, one needs to add the dependency to each application which requires that dependency. This
is done in the `*.app.src` file in the *application* root directory (for instance, `myapp/appname/appname.app.src`):

```erlang
{application, appname,
 [{description, ""},
  {registered, []},
  {modules, []},
  {applications, [kernel,
                  stdlib,
                  epsql]},
  {mod, {appname_app, []}},
  {env, []}
 ]}.
```

Building the project with dependencies is then done using the `rebar3 compile` command from the *project* root directory (following the example above, `myapp/`).

To run an application, use the `rebar3 shell --apps <comma-separated-app-names>` from the *project* root directory (e.g. `rebar3 shell --apps appname`).

### PostgreSQL

Working with PostgreSQL in Erlang is possible through one of many libraries available in [rebar3 repository](https://hex.pm/packages).
In this blog I will use [`epsql`](https://github.com/epgsql/epgsql) library.

For the most part, when working with the database, you only need a handful of features from the database library:

* connecting to the database with specific parameters (connection pool size, security options, timeouts, etc.)
* executing prepared statements (allowing the DB communication layer - the library - to safely inject query parameters, preventing the SQL injection)
* support for `SELECT`, `INSERT`, `UPDATE` and `DELETE` statements (with very few exceptions, these form 95% of all the use cases, in my experience)
* retrieve query results (as list of dictionaries of sorts)

epsql library provides all of these with the following functions:

* `epgsql:connect/1` (and its variations) in combination with `epgsql:close/1` to close the connection, if ever needed; `epgsql:connect/1` takes a dictionary of options, including
  * `host`
  * `username` and `password`; optionally - `ssl` and `ssl_opts` for secure connections
  * `database`
  * `timeout` - for limiting the connection time
* `epgsql:squery/2` and `epgsql:equery/3` to query the database
  * `epgsql:squery/2` will execute a simple query (takes connection and a query string as parameters, returns a tuple of status - `ok` or `error`, list of columns and list of rows)
  * `epgsql:equery/3` will create an unnamed prepared statement (yes, you can have named prepared statements for future reuse), safely inject the query parameters (specified as `$1`, `$2`, `$3` and so on in query string, the second argument) passed as the third argument (a list of parameters) and execute the statement on a database connection (passed as the first argument)

Here are few simple examples of the above functions:

```erlang
connect_to_blog_db() ->
  { ok, C } = epsql:connect(#{ host => "localhost", username => "root", password => "****", database => "blog" }),

  C.

create_blog(Title, Content) ->
  C = connect_to_blog_db(),

  { ok, Count } = epsql:equery(C, "INSERT INTO posts (title, content) VALUES ($1, $2)", [ Title, Content ]),

  Count == 1.

get_blogs() ->
  C = connect_to_blog_db(),

  { ok, Columns, Rows } = epsql:squery(C, "SELECT title, content FROM posts"),

  Rows.
```

### Mochi HTTP server

As shown above, the default HTTP server bundled with Erlang standard library (OTP) is rather low-level.
There are few options in rebar3 package repository which significantly improve the situation. One of them is [`mochiweb`](https://github.com/mochi/mochiweb/).

The web server with Mochi could look like this (slightly more complex than the one described above with `httpd`):

```erl
-module(http_sample).

-export([ dispatch/1, loop/1, start/0, stop/0 ]).

-define(HTTP_OPTS,
  [ { loop, {?MODULE, dispatch} },
    { port, 4000 },
    { name, http_4000 } ]).

start() ->
  { ok, Http } = mochiweb_http:start(?HTTP_OPTS),

  Pid = spawn_link(fun () -> loop(Http) end),

  register(http_sample, Pid),

  ok.

stop() ->
  http_sample ! stop,

  ok.

dispatch(Req) ->
  case mochiweb_request:get(method, Req) of
    'GET' -> get_resource(Req);

    'PUT' -> put_resource(Req);

    _ -> method_not_allowed(Req)
  end.

get_resource(Req) ->
  Path = mochiweb_request:get(path, Req),

  % note: io:format(FmtString, Params) would print to STDOUT
  % whereas io_lib:format(FmtString, Params) will return a formatted string
  Body = io_lib:format("Hello, Resource '~s'\r\n", [ Path ]),

  Headers = [{ "Content-Type", "text/plain" }],

  mochiweb_request:respond({ 200, Headers, Body }, Req),

  ok.

put_resource(Req) ->
  ContentType = mochiweb_request:get_header_value("Content-Type", Req),

  ReqBody = mochiweb_request:recv_body(Req),

  mochiweb_request:respond({ 201, [], "201 Created\r\n" }, Req),

  ok.

method_not_allowed(Req) ->
  Path = mochiweb_request:get(path, Req),
  Method = mochiweb_request:get(method, Req),

  Body = io_lib:format("Method ~s on path ~s is not supported", [ Method, Path ]),

  mochiweb_request:respond({ 405, [], Body }, Req),

  ok.

loop(Http) ->
  receive
    stop ->
      ok = mochiweb_http:stop(Http),
      exit(normal);

    _ -> ignore
  end,

  (?MODULE):loop(Http).
```

For this code to run, you should do few little extra steps. First, creating the new application with `rebar3` - I prefer `escript` template for something this simple.

Then, add a new dependency to the `rebar.config` file:

```erl
{erl_opts, [debug_info]}.
{deps, [
  {mochiweb, "2.22.0"} % <---- here
]}.

{shell, [
  % {config, "config/sys.config"},
    {apps, [http_sample]}
]}.
```

Then, put the code from above under `src/http_server.erl` file.

Finally, run the interactive shell in the context of the application: `rebar3 shell` and start server by calling `http_server:start().`.

Then you should be able to communicate with this rather simple server by using `curl`, for example:

```sh
curl http://localhost:4000/my/resource -X PUT -d '{ "name": "message", "value": "my message" }'
```

## A more complex distributed system

This might sound super ambitious, but let us build a distributed database. For sake of simplicity, let's make it a reduced
version of Redis - a key-value distributed in-memory storage. Essentially, an over-engineered hashmap.

To draw some boundaries around this, let's focus on these key features:

* simple CRUD actions - get, set and delete a value; this should operate on REST-ish API through HTTP:
  * `GET /{key}` - get key value
  * `PUT /{key}` - use request body for value and associate the value with the key
  * `DELETE /{key}` - remove the key from the storage
* running on multiple nodes (machines)
* synchronizing the data between the nodes; this should support:
  * ability to rotate the nodes in the cluster (switch off nodes and add new ones randomly) without the loss of data
  * distributing the operations across the nodes to keep the data in sync

Sounds unbelievable, but with Erlang this is actually pretty simple.

We will need a "main" process, which will take the requests from the users (REST-ish API) and pass them to the nodes.
Each node will store its own copy of the data in memory. On startup, each new node will receive the copy of the data
from the first available node. If no nodes are available - that means the cluster is fresh and we can safely assume
the data is empty.
