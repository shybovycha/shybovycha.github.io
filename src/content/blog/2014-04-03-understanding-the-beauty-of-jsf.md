---
layout: post
title: Understanding the beauty of JSF
date: '2014-04-03T13:34:03+02:00'
tags:
- java
- jsf
tumblr_url: http://shybovycha.tumblr.com/post/81577021854/understanding-the-beauty-of-jsf
---
<p>My web framework of choice is Ruby On Rails. It&rsquo;s perfect for me. Not because of its scalability or performance, but because of its architecture. You do not need to do lot of work to create a website or a webservice. Ruby Gems have all the power you will ever need. Ruby itself allows you to do even a black magic nicely.</p>

<p>It&rsquo;s true to say that i am a RoR fan.</p>

<p>So when i started learning JSF i thought <em>Gosh! It&rsquo;s ugly! It&rsquo;s totally impossible to work with!</em>. But in a while i realized that small yet mighty concept, i even did not imagine to be thinking of.</p>

<p>See, when you write a website, you need two things to be done:</p>

<ul><li><strong>give a user static content</strong>; user just should see something on a display!</li>
<li><strong>take user data, process it and perform previous step</strong>; in order to make a dynamic website <em>(which is 90% of all the websites you&rsquo;ve seen, i guess)</em> you should use web forms and process them</li>
</ul><p>The last thing i did not mention here (because i did not dive in it yet) is: <strong>just use that javascript</strong>. You would never provide user-friendly interface on the web until you get full control of what&rsquo;s going on client&rsquo; side. That&rsquo;s the purpose of JavaScript. Different hints and tips, asynchronous operations, messages and other cool stuff making your UI looking great is the JS part.</p>

<p>But in other cases you use <strong>resources</strong>. That&rsquo;s the bundle of data, stored (maybe) in database and being controlled by user via the forms.</p>

<p>Any form (yes, just <strong>any</strong>) could be described as relying on some resource. Login form? It uses <strong>User</strong> resource. Search form? It uses the <strong>SearchQuery</strong> resource. Post creation form? It serves <strong>Post</strong>!</p>

<p>So, that powerful concept i was talking above is the principle <strong>a Managed Java Bean describes the Resource wired to the User Interface</strong>.</p>

<p>See, when you show a form to a user - you get your database row maped onto a Java Bean. When user saves the form with the new data - that data gets stored in the same Bean and the method you&rsquo;ve set to <code>commandButton</code> or whatever is invoked.</p>

<p>Just think of it! You are doing exactly the same with all those Rails, Play, Django or any other web framework, when you are creating a dynamic website!</p>
