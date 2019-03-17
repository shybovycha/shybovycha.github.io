---
title: 'Two sides of web application. Part 4: backend'
layout: post
date: '2016-03-10T12:08:39+01:00'
---

<div class="row">
    <div class="col-md-6 col-xs-12">
        {% include references/two-sides-of-web-application.html %}
    </div>
    <div class="col-md-6 col-xs-12 text-xs-center text-md-right">
        <img class="img-responsive" style="max-height: 150px" src="//images/two-sides-of-web-application/tank_back.jpg" />
    </div>
</div>

## Introducing backend part

Previously I described how to build the frontend part of a web application. Now we'll turn backwards,
making the server part for our stats application. And here's one trick: I'll be writing about several
different technologies, allowing one to create an API. And then you'll be able to decide which one
suits you the best.

## Requirements

So what should our backend application do? In the previous part we've stubbed a few calls to the server.
And there's one action required - the one which will track the visitor.

Taking all that into account, these should be implemented:

<table class="table table-bordered">
    <thead>
        <tr>
            <th><strong>Frontend resource</strong></th>
            <th><strong>HTTP request</strong></th>
            <th><strong>Responsibility</strong></th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td rowspan="4">Application</td>
            <td><code>POST /applications</code></td>
            <td>Create an application</td>
        </tr>
        <tr>
            <td><code>GET /applications/:id/:date-from/:date-to</code></td>
            <td>Get application statistics</td>
        </tr>
        <tr>
            <td><code>POST /applications/:id</code></td>
            <td>Update application</td>
        </tr>
        <tr>
            <td><code>GET /</code></td>
            <td>Get application list</td>
        </tr>
        <tr>
            <td rowspan="4">User</td>
            <td><code>POST /users/sign-up</code></td>
            <td>Create user account</td>
        </tr>
        <tr>
            <td><code>POST /users/sign-in</code></td>
            <td>Log in</td>
        </tr>
        <tr>
            <td><code>GET /users/sign-out</code></td>
            <td>Log out</td>
        </tr>
        <tr>
            <td><code>POST /users</code></td>
            <td>Update user account</td>
        </tr>
    </tbody>
</table>

## Web servers' overview

Now we have the list of "actions" we want our server to be able to perform. Now what?
Let's get to know web servers better and find out how they work. See, all the *"HTTP methods"*
we've defined above are the commands to the webserver, which any user can use. They are
**HTTP requests** *(well, almost)* user or browser generates and sends to the server, waiting
for response. Then it processess the response, depending on the request and response itself
*(if the request was to get the HTML file, browser will probably try to show the response to user; if browser has requested an image, it will probably try to put it in place on a web page, etc.)*.

Basically, there are two main HTTP request types: `GET` and `POST`. Both of them have two
fields in common - **HTTP method** (`GET` or `POST`) and **URL** *(or the address, the request is addressed to)*. Both of these methods can send a list *(or a key-value list, actually)* of parameters to the server.
And there's one major difference between `POST` and `GET`: `GET` requests send parameters in the **URL**
address, while `POST` requests send them *"secretely"* - in the **HTTP request body**; e. g. the user
can see what browser sends to the server in an address field of a browser, so he can see all the params
being sent to the server. And `POST` requests hide that information, so user can see params only if he
use developer tools. User can not send `POST` request simply - one should be using JavaScript for that.

## Rapid web development with Ruby

Ruby is a great language. It allows one to write code blazingly fast. It has great community and
a huge repository, full or libraries. For our first backend we will use **Sinatra** framework to
create an API. It's really small framework, but enough to make everything we need.

Let's check it out! First, create a new project with a typical Ruby application structure:

```bash
.
├── Gemfile
└── application.rb
```


