---
layout: post
title: Tiny java code...
date: '2015-03-27T08:10:28+01:00'
tags: []
tumblr_url: http://shybovycha.tumblr.com/post/114739833891/tinyjavacode-this-one-was-super-easy-so-i-did-a
---

<blockquote><p><a href="http://www.reddit.com/r/dailyprogrammer/comments/qlwrc/372012_challenge_19_easy/" rel="noreferrer">This one</a> was super easy so I did a quick comparison of a couple of ways you can do it; the goal is to count all the alphanumeric (a-zA-Z0-9) characters in the text of a Sherlock Holmes book. There’s probably other ways do do it because there’s lots of ways to do literally anything, but the fastest one here is stripping out all non-alphanumeric characters (\W).</p></blockquote>

```java
//*********************************************//
//                                             //
//        EASY DAILY PROGRAMMING TASK #19      //
//        ALPHANUMERIC CHARACTER COUNTING      //
//                                             //
//*********************************************//

public void count1(String text) {
    long start = System.currentTimeMillis();
    int count = 0;

    for (char c : text.toCharArray()) {
        if ((c + "").matches("[a-zA-Z0-9]")) {
            count++;
        }
    }

    long end = System.currentTimeMillis();
    System.out.println("Count 1: " + count + " Time taken: " + (end - start));
}

public void count2(String text) {
    long start = System.currentTimeMillis();
    int count1 = text.length();
    text = text.replaceAll("\\w", "");
    long end = System.currentTimeMillis();
    System.out.println("Count 2: " + (count1 - text.length()) + " Time taken: " + (end - start));
}

public void count3(String text) {
    long start = System.currentTimeMillis();
    text = text.replaceAll("\\W", "");
    long end = System.currentTimeMillis();
    System.out.println("Count 3: " + text.length() + " Time taken: " + (end - start));
}
```

```
Countiung all alphanumeric characters in Sherlock.txt:
Count 1: 432188 Time taken: 269
Count 2: 432188 Time taken: 53
Count 3: 432188 Time taken: 22
```

<a href="http://tinyjavacode.tumblr.com/post/114715031583/this-one-was-super-easy-so-i-did-a-quick" class="tumblr_blog">tinyjavacode</a>:
