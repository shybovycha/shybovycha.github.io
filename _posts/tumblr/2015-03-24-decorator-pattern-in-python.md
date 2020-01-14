---
layout: post
title: Decorator pattern in Python
date: '2015-03-24T10:41:35+01:00'
tags:
- python
- rtfm
- design-patterns
tumblr_url: http://shybovycha.tumblr.com/post/114483612351/decorator-pattern-in-python
---

Just found some interesting way of implementing Decorator design pattern in Python.

As **Jason Smith** said in his book (**Elemental Design Patterns**),

> design patterns may be implemened in different ways in different programming languages

That’s said, design patterns are not some set of classes which will be implemented in a very similar way in different languages - they are just a way of doing something.

Thus, Decorator pattern is a way of wrapping some method’s or class’ behaviour. In Python it may be done with **Context Managers:**

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

This code will end up wrapping `print "foo"` and `print "moo"` methods with printing some HTML tags around 'em:

```html
<h1>moo</h1>
<div>foo</div>
```

That is interesting as it implements Decorator design pattern in a bit hard-coded way, but using language features, not OOP ones.

Compare it to the "conventional" OOP implementation in Python:

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

This one looks a bit... ugly, right? And though Python does not really care of which type are `a` and `b`, we may not need all this class hierarchy.

This is the might of **context managers**!
