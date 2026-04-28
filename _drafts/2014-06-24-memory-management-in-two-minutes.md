---
layout: post
title: Memory management in two minutes
date: '2014-06-24T23:14:24+02:00'
tags:
- programming
tumblr_url: http://shybovycha.tumblr.com/post/89792640191/memory-management-in-two-minutes
---

Each data object (or data structure if you want) should have **modificators**, **selectors**, **constructor** and **destructor**.

This is known as **memory management** in C. And this statement is valid for any kind of data.

Thus, if you've allocated memory - you **MUST** clean it up. Created an object - destroy it when it's no longer in need. Filled the database - clean it up lately, when the information stored is useless.

---

*Many thanks for the idea to my former teamlead, Marat Kamenshchikov!*
