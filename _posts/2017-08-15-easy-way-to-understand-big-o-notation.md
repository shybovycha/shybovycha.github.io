---
layout: post
title: Easy way to understand algorithm complexity and big O notation
date: '2017-08-15T11:38:24+02:00'
---

## Foreword

Some time ago I've published an article about bit O notation. I've mentioned its source but never published it. So here it is, the original article.

## The article

Big O notation is the most widely used method which describes algorithm complexity - the execution time required or the space used in memory or in the disk by an algorithm. Often in exams or interviews, you will be asked some questions about algorithm complexity in the following form:

> For an algorithm that uses a data structure of size n, what is the run-time complexity or space complexity of the algorithm? The answer to such questions often uses big O notations to describe the algorithm complexity, such as `O(1)`, `O(n)`, `O(n^2)` or `O(log(n))`.

<!--more-->

## Big O for time complexity

Here we don't want to discuss big O mathematically. Basically, when analyzing the time complexity of an algorithm, big O notation is used to describe the rough estimate of the number of "steps" to complete the algorithm. Let's take the following example:

{% highlight c %}
void fun(int n) {
    // partl
    doSomething();

    // part2
    for (int j = 0; j < n; j++) {
        doSomething();
    }

    // part3
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            doSomething();
        }
    }

    // part4
    return;
}
{% endhighlight %}

Here lets assume that `doSomething()` takes `C` steps to complete. The whole `fun(n)` method has 4 parts. What is the time complexity of each part for different parameters `n`?

For part 1, it does `doSomething()` so it takes constant `C` steps. Here `C` is independent of the parameter `n`. When the time complexity is independent of the parameter, we use `O(1)` to mark it.

For part 2, it does `doSomething()` exactly `n` times. Each time it takes `C` steps. So in total, it takes `C*n` steps to complete part 2. Then as described above, we use `O(1)` to complete the `C` steps. Then for `C*n`, the complexity becomes `n * O(1)`. Here, an important rule is that `a * O(n)` equals `O(a * n)`. In such case, `n * O(1) = O(n)`. So the time complexity of part 2 is `O(n)`.

For part 3, it has two loops. The inner loop is exactly like part 2. The outer loop does part 2 `n` times again so the time complexity of part 3 is `n * O(n)` which is `O(n^2)`.

For part 4, it takes exactly `1` step to return. So the time complexity is `O(1)`.

Then what is the total time complexity of the whole `fun(n)`? It is easy, we just need to add up the time complexity of all the 4 parts:

{% highlight txt %}
O(1) + O(n) + O(n^2) + O(1)
{% endhighlight %}

Now let's assume `n` becomes very-very large. Parts 1 and 4 will still take constant number of steps, `C + 1`, to complete, so the total time of running part 1 and part 4 could be ignored when calculating the execution time for part 2 and part 3, as now both part 2 and part 3 take much more time than part 1 and part 4.

Let's we compare part 2 and part 3:

* when `n` is `1`, both part 2 and part 3 take `C` steps
* when `n` is `3`, part 2 takes `3*C` steps while part 3 takes `9*C` steps
* when `n` is `1000`, part 2 needs `1000 * C` steps but part 3 takes `1000000 * C` steps!

As `n` increases, the time complexity of part 3 increases much quicker than part 2. When `n` becomes extremely large, the time of part 2 can be ignored by the time of part 3.

So here is another rule: when we add up big O notation, the notation with slower increase speed could be ignored by the notation with faster increase speed. As we could see, `O(1)` does not increase at all so it could be ignored by `O(n)`. `O(n)` is slower than `O(n^2)` so it could be ignored by `O(n^2)`. Therefore, the complexity of the whole `fun()` is `O(n^2)`.

## Rules summary of big O notation

From the above examples, we could summarize the following rule: for multiplying two O notations, `O(A)` and `O(B)`, the result is `O(A*B)`. For example:

{% highlight txt %}
O(1) x O(n) = O(n).
O(n) x O(n) = O(n^2).
O(1) + O(n) = O(n).
O(n) + O(n^2) = O(n^2).
O(1) + O(n) + O(n^2) = O(n^2).
{% endhighlight %}

Here is the comparison of the increase speed of different O notations:

{% highlight txt %}
O(1) < O(log(n)) < O(n) < O(n * log(n)) < O(n^2)
{% endhighlight %}

## How to use big O notation to compare algorithm complexity and why

When comparing different algorithms, we often say the one with a "smaller" big O notation is more efficient. However, there are situations that algorithms with "smaller" bit O is faster.

For example, there are two algorithms `f1(n)` and `f2(n)`. `f(1)` takes `100` steps and `f(2)` takes `n` steps so `f(1)` is `O(1)` and `f(2)` is `O(n)`. When `n` is less than `100`, `f1(n)` is more efficient than `f2(n)`. However, big O compares when `n` goes to extremely (or infinitely) large. When `n` goes up to `10000`, `f2(n)` becomes way more efficient. As the nature of large-scale data in computer engineering, it makes more sense to compare algorithms in the cases that the data is very large.

## Big 0 for space complexity

It is easier to understand using big O to estimate space complexity as it is more concrete. For example, in an algorithm, we need to create an array of size `n` to store the temporary results before getting the final result. If we assume that the size of the element in the array is a constant `C` which is independent of `n`, the space complexity of using the array is `C*n` which is

{% highlight txt %}
O(n) x O(C) = O(n) x O(1) = O(n)
{% endhighlight %}

When comparing different algorithms, we often compare how much "extra" space complexity is needed to solve the problem. For example, as an algorithm that needs an extra array of size `n` is not as good as the one which only needs two variables, `O(1)` space complexity is more efficient than `O(n)`.
