---
layout: post
title: Decorator pattern in Python
date: '2015-03-24T10:41:35+01:00'
tags:
- python
- rtfm
- design patterns
tumblr_url: http://shybovycha.tumblr.com/post/114483612351/decorator-pattern-in-python
---
<p>Just found some interesting way of implementing Decorator design pattern in Python.</p>

<p>As <strong>Jason Smith</strong> said in his book (<strong>Elemental Design Patterns</strong>), <em>“design patterns may be implemened in different ways in different programming languages”</em>.</p>

<p>That’s said, design patterns are not some set of classes which will be implemented in a very similar way in different languages - they are just a way of doing something.</p>

<p>Thus, Decorator pattern is a way of wrapping some method’s or class’ behaviour. In Python it may be done with <strong>Context Managers:</strong></p>

```python
from contextlib import contextmanager

@contextmanager
def tag(name):
    print "<%s>" % name,
    yield
    print "</%s>" % name,

with tag("h1"):
    print "moo",

print

with tag("div"):
    print "foo",
```

<p>This code will end up wrapping <code>print "foo"</code> and <code>print "moo"</code> methods with printing some HTML tags around &lsquo;em:</p>

```html
<h1>moo</h1>
<div>foo</div>
```

<p>That is interesting as it implements Decorator design pattern in a bit hard-coded way, but using language features, not OOP ones.</p>

<p>Compare it to the <em>"standard"</em> OOP implementation in Python:</p>

```python
# -*-coding:utf-8 -*-
class SimpleText(object):
    def __init__(self, text):
        self.text = text

    def content(self):
        return self.text

    def __str__(self):
        return self.content()

class TagDecorator(SimpleText):
    def __init__(self, text, tag):
        super(TagDecorator, self).__init__(text)
        self.tag = tag

    def content(self):
        return '<{0}>{1}</{0}>'.format(self.tag, super(TagDecorator, self).content())

a = SimpleText('moo')
print('SimpleText: %s' % a)

b = TagDecorator('moo', 'h1')
print('TagDecorator (h1): %s' % b)
```

<p>This one looks a bit&hellip; ugly&hellip; right? And though Python does not really care of which type are <code>a</code> and <code>b</code>, we may not need all this class hierarchy.</p>

<p>This is the might of <strong>context managers</strong>!</p>
