---
layout: post
title: Big O notation
categories: []
tags: []
date: '2016-08-25T13:00:00'
---

The best big O notation explanation I've ever saw I've found on... Google Play Market! I was
hanging around, looking for the suggested software and, for some reason, I've decided to
install some educational application for programmers. And here's what I've found...

<!--more-->

Big O notation shows, how many steps or memory units will the algorithm use to complete,
at its maximum. Here's an example:

{% highlight java %}
void someAlgorithm(int n) {
  // part 1
  doSomething();

  // part 2
  for (int i = 0; i < 10; i++) {
    doSomething();
  }

  // part 3
  for (int i = 0; i < n; i++) {
    doSomething();
  }

  // part 4
  for (int i = 0; i < n; i++) {
    for (int t = 0; t < n; t++) {
      doSomething();
    }
  }

  return n;
}
{% endhighlight %}

Let's take a look at each of four algorithm parts. Part 1 just calls some function, `doSomething()`.
Let's assume it takes some constant amount of time to complete, `C`. The time complexity of calling
a function, which uses constant time to complete is `O(1)`. So part 1 will take `O(1)` time to complete.

Part 2 has a loop, which has exactly 10 iterations, calling `doSomething()` at each iteration. As we've
discussed above, this part takes `10 * C` (ten calls of `doSomething()` function, which takes `C` steps to
complete) steps. This is a constant value too, so part 2 of the function `myAlgorithm()` will have the
complexity of `O(1)`.

Part 3 has a loop, whose number of iterations relies on the input parameter of `myAlgorithm()`, namely `n`.
We do not know, what value the `n` will take. But as it increases, the steps, needed for this part to complete
increases too. So the complexity of this part will be `O(n)`.

Part 4 has two nested loops. As in the previous case, when `n` increases, the steps needed by this part to
complete will increase even faster: for `n = 1` it will take exactly `C` steps to complete (recall:
the complexity of `doSomething()` is `C`); for `n = 2` it will take `2 * 2 * C = 4 * C` steps to complete;
for `n = 10` the amount of steps would be `10 * 10 * C = 100 * C`. One can notice that the complexity
equals to `n * n * C`. This is quadratical complexity, denoted as `O(n^2)`.

The final complexity of an algorithm could be calculated by easily adding all those parts' complexities:
`O(1) + O(1) + O(n) + O(n^2)`. But here's the trick: if the `n` is relatively small (like, `10` or `100`),
the difference between `O(1)` and `O(n)` is huge (noticeable, at least). But if we say the value of `n`
is insanely large (like, billiards), we may not notice the `O(1)` is just nothing, compared to `O(n)`. And
`O(n)` is just nothing, when compared to `O(n^2)`. And here comes the rule: total complexity of an algorithm is
the maximum needed amount of steps to complete. Just as follows:

* `O(C) = O(1)`
* `O(1) + O(n) = O(n)`
* `O(n) + O(n^2) = O(n^2)`

Here comes the comparison of the known complexities: `O(1) < O(log n) < O(n) < O(n*log n) < O(n^m)`.

But we can measure not time consumption only, using the big O notation. It is also handy for memory
complexity measurements.

Here the same rules apply, except of _"steps to complete"_ we use
_"memory cells allocated"_. So we will count the amount of allocated memory. This is mostly used
by lists, not by objects and structures (as they always use the same memory amount). Check this out:

{% highlight c++ %}
struct Moo {
  int a, b, c;
  float d;
};

class Foo {
public:
  int a, b, c;
  float *d;
};
{% endhighlight %}

Both `Moo` and `Foo` will use the same amount of memory initially (since pointers in C++ are just
integer memory addresses' values and floats use same 4 bytes - just as integers do). But depending on
how many memory we will allocate for `Foo.d` we will get the different values. Consider the continuation
of this example below:

{% highlight c++ %}
int myAlgorithm(int n) {
  // part 1
  Foo *foo = new Foo();

  // part 2
  foo->d = new float[10];

  // part 3
  int *a = new int[n];

  // part 4
  int **b = new int*[n];

  for (int i = 0; i < n; i++) {
    b[i] = new int[15];
  }

  return 0;
}
{% endhighlight %}

Here, in part 1, we have just an instance of `Foo` class, which uses
`3 * int + float* = 3 * 1 + 1 = 4` memory cells. As in case with time complexity, this amount is constant,
thus it has `O(1)` memory consumption.

In part 2, however, we extend this amount by placing 10 memory cells into
`foo.d` field, but this does not change much, as `foo` will use constant memory cells anyway. So, part 2 has
memory complexity of `O(1)` too.

In part 3 we create a new array, and its size depends on function's argument `n`, so its memory consumption
is `O(n)`.

In part 4 we create a two-dimensional array, whose size is `n * 15`. We can split its size into two components:
`O(n) * O(15) = O(n)`, because `15` is `15` no matter what.

And the total memory complexity of the algorithm is `O(1) + O(1) + O(n) + O(n) = O(n)`.

For even more simple `O(*)` calculus, replace the `+` operator with the `min` operator:
memory complexity of `myAlgorithm()` is `max(O(1), O(1), O(n), O(n)) = O(n)`.
