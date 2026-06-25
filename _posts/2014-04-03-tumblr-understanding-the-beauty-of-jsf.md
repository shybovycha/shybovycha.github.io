---
layout: post
title: Understanding the beauty of JSF
date: '2014-04-03T13:34:03+02:00'
tags:
- java
- jsf
tumblr_url: http://shybovycha.tumblr.com/post/81577021854/understanding-the-beauty-of-jsf
---

My web framework of choice is Ruby On Rails. It's perfect for me. Not because of its scalability or performance, but because of its architecture. You do not need to do lot of work to create a website or a webservice. Ruby Gems have all the power you will ever need. Ruby itself allows you to do even a black magic nicely.

It's true to say that i am a RoR fan.

So when i started learning JSF i thought _Gosh! It's ugly! It's totally impossible to work with!_. But in a while i realized that small yet mighty concept, i even did not imagine to be thinking of.

See, when you write a website, you need two things to be done:

- **give a user static content**; user just should see something on a display!
- **take user data, process it and perform previous step**; in order to make a dynamic website (which is 90% of all the websites you've seen, i guess) you should use web forms and process them

The last thing i did not mention here (because i did not dive in it yet) is: **just use that javascript**. You would never provide user-friendly interface on the web until you get full control of what's going on client' side. That's the purpose of JavaScript. Different hints and tips, asynchronous operations, messages and other cool stuff making your UI looking great is the JS part.

But in other cases you use **resources**. That's the bundle of data, stored (maybe) in database and being controlled by user via the forms.

Any form (yes, just **any**) could be described as relying on some resource. Login form? It uses **User** resource. Search form? It uses the **SearchQuery** resource. Post creation form? It serves **Post**!

So, that powerful concept i was talking above is the principle **a Managed Java Bean describes the Resource wired to the User Interface**.

See, when you show a form to a user - you get your database row maped onto a Java Bean. When user saves the form with the new data - that data gets stored in the same Bean and the method you've set to `commandButton` or whatever is invoked.

Just think of it! You are doing exactly the same with all those Rails, Play, Django or any other web framework, when you are creating a dynamic website!
