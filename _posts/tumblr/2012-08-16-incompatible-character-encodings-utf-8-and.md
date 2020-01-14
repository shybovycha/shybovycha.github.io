---
layout: post
title: 'incompatible character encodings: UTF-8 and ASCII-8BIT'
date: '2012-08-16T11:20:00+02:00'
tags:
- rails
- rails3
- RTFM
- mysql
tumblr_url: http://shybovycha.tumblr.com/post/29543348338/incompatible-character-encodings-utf-8-and
---

Just got the following error in my Rails project (Rails 3.2.6, Ruby 1.9.3):

```
ActionView::Template::Error (incompatible character encodings: UTF-8 and ASCII-8BIT)
```

That was caused by the case when MySQL record (ActiveReciord object, actually) contained UTF-8 characters and i was trying to output those chars in my template.
But **mysql** gem does not support those. It needs a bit of hacking.

Luckily, there is more convenient way to solve the problem. The solution of this issue now appears too easy:

* Install the `mysql2` gem
* Use `adapter: mysql2` instead of `adapter: mysql` in your `config/database.yml` file
