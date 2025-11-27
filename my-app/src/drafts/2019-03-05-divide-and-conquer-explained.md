---
title: Divide and conquer explained
layout: post
date: '2019-03-05T16:27:00+10:00'
tags: [performance, java, benchmarking, computer-science, optimization, programming, frontend, jvm, web-development, algorithms]
---

Some time ago, I had to parallelize a bunch (around 10k..70k) of jobs in my project to speed up an operation.
And I had a chance to familiarize myself with the Fork-Join executors in Java. This concept was actually an example
of what is commonly known as "divide and conquer principle".

You might have heard of it at the uni, explained by a solution or a decision tree. But a raw knowledge is barely
valuable without practice.

Occasionally, on the interview, I would offer a candidate to solve a small brain-teaser with the
main solution idea spinning around recursion. It is quite tricky but simple enough for majority
of candidates to be solved in approx. 15 minutes.

And since this task is somewhat boring and gives little insight into candidate's abilities, I started to look for
another problem, which would be simple enough and will cover algorithm or data structure knowledge. I started my
search on the website I had a big crush into some years ago, [e-olymp](https://www.e-olymp.com). Filtering tasks
by _"Simple"_ complexity, I've checked out few from the top of the list. And so happened to be that one of them
was based on a solution tree. And I thought this would be an incredible training. So it was.

Let me start off by describing the problem I was trying to solve on a real project.

## Fork-Join

The idea is that you have a number of tasks and solving each of them is exactly same. You might want to split
the solution process into smaller batches. These batches might also be executed in parallel threads for the
sake of performance. You may repeat the splitting as many times as you wish, recursively, until the size of a
job is small enough. This splitting process is called **"forking"**.

Each smaller job might return some result, which you then process together with the other
results of the jobs of a same size. This will give you a list of results, which you then process as if
it was a result of processing a single batch. You repeat this process until the resulting list is nothing
but a single-element list, which would be the end result. This process or rolling up the recursion is
called **"joining"** (since you join the results together).

Quite blurry explanation, isn't it? Let's illustrate this with a simple example: say, you want to find a maximum of
a huuuuge list of numbers (think billions of billions). You can use a single loop. You can even use SIMD for that.
But that would be less efficient if you have the power of a multi-core processor (or even a multi-processor system).

What we can do is split the initial lists into a list of smaller lists and find a maximum of each sub-list. This will
give you a list of local maximums. Then you find a maximum amongst them. Are you following?

Let me draw this as a solution tree of a small input data:

```
max:

                                    +----------------------------------------+                                       
                                    |                                        |                                       
                 +------------------+     1  2  3  4  5  6  7  8  9  10      +--------------------+                  
                 |                  |                                        |                    |                  
                 |                  +----------------------------------------+                    |                  
                 |                                                                                |                  
                 v                                                                                v                  
+----------------------------------+                                             +----------------------------------+
|                                  |                                             |                                  |
|         1   2   3   4   5        |                                             |        6   7   8   9   10        |
|                                  |                                             |                                  |
+----------------+-----------------+                                             +----------------+-----------------+
                 |                                                                                |
        +--------+--------+                                                              +--------+---------+
        |                 |                                                              |                  |
        v                 v                                                              v                  v
 +--------------+    +--------------+                                            +--------------+    +--------------+
 |              |    |              |                                            |              |    |              |
 |    1  2  3   |    |    4     5   |                                            |    6  7  8   |    |     9   10   |
 |              |    |              |                                            |              |    |              |
 +-------+------+    +-------+------+                                            +-------+------+    +-------+------+
         |                   |                                                           |                   |       
    +----+-----+             |                                                      +----+----+              |       
    |          |             |                                                      |         |              |       
    v          v             v                                                      v         v              v       
+--------+ +--------+   +--------+                                             +--------+ +--------+     +--------+
|        | |        |   |        |                                             |        | |        |     |        |
|  1  2  | |   3    |   |  4  5  |                                             |  6  7  | |   8    |     |  9  10 |
|        | |        |   |        |                                             |        | |        |     |        |
+----+---+ +---+----+   +---+----+                                             +----+---+ +---+----+     +----+---+
     |         |            |                                                       |         |               |
=====================================================================================================================
|                                                                                                                   |
|                                                    JOINING PHASE                                                  |
|                                                                                                                   |
=====================================================================================================================
     |         |            |                                                       |         |               |
     v         v            v                                                       v         v               v
+--------+ +--------+   +--------+                                             +--------+ +--------+     +--------+
|        | |        |   |        |                                             |        | |        |     |        |
|    2   | |   3    |   |   5    |                                             |    7   | |   8    |     |   10   |
|        | |        |   |        |                                             |        | |        |     |        |
+----+---+ +---+----+   +---+----+                                             +----+---+ +---+----+     +----+---+
     |         |            |                                                       |         |               |
     +----+----+            |                                                       +----+----+               |
          |                 |                                                            |                    |
          v                 v                                                            v                    v
    +------------+    +-----------+                                               +--------------+     +--------------+
    |            |    |           |                                               |              |     |              |
    |     3      |    |     5     |                                               |       8      |     |      10      |
    |            |    |           |                                               |              |     |              |
    +-----+------+    +-----+-----+                                               +-------+------+     +-------+------+
          |                 |                                                             |                    |
          +---------+-------+                                                             +---------+----------+
                    |                                                                               |
                    v                                                                               v
                +-------+                                                                       +-------+
                |       |                                                                       |       |
                |   5   |                                                                       |  10   |
                |       |                                                                       |       |
                +-------+                                                                       +-------+
                    |                                                                               |
                    +---------------------------------------+---------------------------------------+
                                                            |
                                                            v
                                                        +--------+
                                                        |        |
                                                        |   10   |
                                                        |        |
                                                        +--------+

```

The way it works is: while the input is larger than two elements - we split it into (in this particular example above)
exactly two sub-tasks and will process them eventually. Then we will process the results of two sub-tasks and return this
result.

In the code it might look like this:

```java
int max(List<Integer> data) {
    if (data.size() == 0) {
        return 0;
    } else if (data.size() == 1) {
        return data.get(0);
    }

    int midPoint = data.size() / 2;
    int x1 = max(data.subList(0, midPoint));
    int x2 = max(data.subList(midPoint, data.size()));

    // here we could have dove into recursion again, but this would be a waste

    if (x1 < x2) {
        return x2;
    } else {
        return x1;
    }
}
```

A huge advantage of such approach is that we can easily run the recursive call in a separate thread.

Consider a slightly modified example, where we can split the task into as many sub-tasks as we need:

```
max:

                                    +----------------------------------------+                                       
                                    |                                        |                                       
                 +------------------+      1  2  3  4  5  6  7  8  9  10     +--------------------+                  
                 |                  |                                        |                    |                  
                 |                  +---------------------+------------------+                    |                  
                 |                                        |                                       |                  
                 |                                        |                                       |                  
                 |                                        |                                       |                  
                 |                                        |                                       |                  
        +--------+----------+                    +--------+---------+                             |                  
        |                   |                    |                  |                             |                  
        v                   v                    v                  v                             v                  
+--------------+    +--------------+     +--------------+   +--------------+               +--------------+          
|              |    |              |     |              |   |              |               |              |          
|    1    2    |    |   3     4    |     |   5    6     |   |   7    8     |               |   9    10    |          
|              |    |              |     |              |   |              |               |              |          
+-------+------+    +-------+------+     +-------+------+   +-------+------+               +------+-------+          
        |                   |                    |                  |                             |
        |                   |                    |                  |                             |
=====================================================================================================================
|                                                                                                                   |
|                                                    JOINING PHASE                                                  |
|                                                                                                                   |
=====================================================================================================================
        |                   |                    |                  |                             |
        |                   |                    |                  |                             |
        v                   v                    v                  v                             v
+--------------+    +--------------+     +--------------+   +--------------+               +--------------+
|              |    |              |     |              |   |              |               |              |
|       2      |    |       4      |     |       6      |   |       8      |               |      10      |
|              |    |              |     |              |   |              |               |              |
+-------+------+    +-------+------+     +-------+------+   +-------+------+               +-------+------+
        |                   |                    |                  |                              |
        +---------+---------+                    +---------+--------+                              |
                  |                                        |                                       |
                  v                                        v                                       v
           +--------------+                         +--------------+                        +--------------+
           |              |                         |              |                        |              |
           |      4       |                         |      8       |                        |      10      |
           |              |                         |              |                        |              |
           +------+-------+                         +------+-------+                        +-------+------+
                  |                                        |                                        |
                  |                                        |                                        |
                  +----------------------------------------+----------------------------------------+
                                                           |
                                                           v
                                                    +--------------+
                                                    |              |
                                                    |      10      |
                                                    |              |
                                                    +--------------+
```

In this example, we split the input data into multiple pieces, until the size of each piece is more than `2`.
This example is actually a little bit complicated, since the input data is being split into **more than 2** branches,
which might end up with a little bit of code:

```java
int max(List<Integer> data) {
    if (data.size() == 0) {
        return 0;
    } else if (data.size() == 1) {
        return data.get(0);
    } else if (data.size() == 2) {
        int x1 = data.get(0);
        int x2 = data.get(1);

        if (x1 < x2) {
            return x2;
        } else {
            return x1;
        }
    }

    StreamSupport.stream(Iterables.partition(data, 2).spliterator(), false);
}
```

The advantage of this approach is that it allows to actually iterate over all possible solutions
