---
layout: post
title: 'Erlang example 2.0'
date: '2020-11-07T11:42:24+9:00'
---

Quite some time ago I've published a <a href="{% post_url tumblr/2015-01-28-erlang-practice %}">blogpost about Erlang</a>. It claimed to present a *short intro to distributed programming in Erlang*. But it turned to be a very simple communication application, nothing super-exciting.

In this post I would like to elaborate more on the topic of Erlang, for a number of reasons:

* it is a pretty simple language itself
* the distributed systems topic disturbs me more and more these days
* when I was looking at Erlang, I would love to see a more advanced tutorial myself _(more practical things and more Erlang platform features showcased)_

<!--more-->

## Language features

Kicking off with the language features itself, let's discover few data structures available out of the box.

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

Erlang comes packed with features. One of them is bundled web-server. Some time ago I've published a post about a very simple Tetris game, which I've crafted one evening. The thing is: that game is nothing more than a single HTML page. What we can do now is start a web server, which will listen for connections on `8080` port and serve them a static file with the game.

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

Using [Hex](https://hex.pm/) (or `rebar3`) is relatively easy. Yet it opens access to thousands of packages available out there.

### CouchDB

### PostgreSQL
