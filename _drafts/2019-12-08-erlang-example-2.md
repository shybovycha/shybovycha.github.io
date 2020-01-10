---
layout: post
title: 'Erlang example 2.0'
date: '2019-12-09T11:42:24+10:00'
---

Some time ago I've published a <a href="{% post_url tumblr/2015-01-28-erlang-practice %}">blogpost about Erlang</a>. It claimed to present a *short intro to distributed programming in Erlang*. But it turned to be a very simple communication application, nothing super-exciting.

In this post I would like to elaborate more on the topic of Erlang, for a number of reasons:

* it is a pretty simple language itself
* the distributed systems topic disturbs me more and more these days
* I would love to see a more advanced tutorial _(more Erlang platform features showcase)_ myself, back in the day

<!--more-->

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

## Database

### Mnesia

Erlang is bundled with a distributed real-time database!

```erlang
-module(visit_tracker).
-export([
  init/0,
  create_schema/0,
  create_account/2,
  create_site/3,
  create_visit/2,
  create_visit/3,
  create_visit/4,
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
    %% then the mnesia:write/3 will be used, so you also have to pass the lock type - `read` or `write` or `sticky_write`
    mnesia:write(visits, #visit{ site_id = SiteId, ip = Ip, browser = Browser, location = Location }, write)
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

### CouchDB

### PostgreSQL
