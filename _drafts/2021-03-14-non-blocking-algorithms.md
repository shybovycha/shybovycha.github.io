---
layout: post
title: "Lock-free algorithms"
---

## Introduction

Lock-free algorithms refer to concurrent (multi-threaded) algorithms, where threads do not stop
and wait for something. Formally speaking, every iteration of an algorithm is a step
_towards its completion_.

This could be further illustrated by the use of simple locks:

```java
Integer x = 0;
Lock lock;

var runnable = () -> {
    while (x < 100) {
        lock.lock();

        x += 1;

        lock.unlock();
    }
};


Thread t1 = new Thread(runnable);
Thread t2 = new Thread(runnable);
```

In the above example, operations would be sequential (but the order would be pretty much random) -
one thread (either `t1` or `t2`) would stop before incrementing `x` if the other thread
(either `t2` or `t1`, corespondingly) is already incrementing it.

So that both the flows are valid:

```
Thread t1:        Thread 2:
---               ---

lock()            lock() // ?..
x += 1            // wait for lock
unlock()          // wait for lock
lock() // ?       lock()
// wait for lock  x += 1
// wait for lock  unlock()
lock()            lock() // ?
...               ...
```

and

```
Thread t1:        Thread 2:
---               ---

lock() // ?...    lock()
// wait for lock  x += 1
// wait for lock  unlock()
lock()            lock() // ?
x += 1            // wait for lock
unlock()          // wait for lock
lock() // ?       lock()
lock() // ?       x += 1
...               ...
```

The _"wait for lock"_ parts are parts of algorithm which do not contribute to its completion by any means -
processor would simply wait for lock to be released; it would not execute an algorithm itself.

<!-- TODO: replace the above pseudo-code with UML sequence diagrams -->

The need for using locks most naturally arises when multiple threads are sharing the same piece of memory.
In this rough definition, lock-free algorithms do not wait for other threads to finish their operations on the same shared memory.

## Warning

Bear in mind: although this seems like an _extremely_ useful and curious thing to have in any concurrent application
(for obvious performance benefits), this is a **very** specific topic, where developers instruct **both** compiler and processor
to refrain from changing the order of program's instructions (will discuss in a moment). This requires tedious manipulaiton
of the processor instructions themselves, which is far from being an easy task.

This whole concept might be mostly a waste of time for the most of applications and be of a high value only for heavy
computational applications. Think various engines - graphics rendering, physics simulation, database servers, device drivers, etc.

## Real-world scenario

For a more relevant example, let's consider a Queue implementation like below:

```cpp
#include <iostream>
#include <string>

template <class T>
class Entry {
private:
  T* value;
  Entry<T>* next;

public:
  Entry(T* value) : value(value) {}

  T* getValue() const {
    return value;
  }

  Entry* getNext() const {
    return next;
  }

  void setNext(Entry* next) {
    this->next = next;
  }
};

template <class T>
class Queue {
private:
  Entry<T>* head;

public:
  Queue() : head(nullptr) {}

  void push(T value) {
    Entry<T>* newHead = new Entry<T>(&value);

    if (head == nullptr) {
      head = newHead;
    } else {
      newHead->setNext(head);
      head = newHead;
    }
  }

  T poll() {
    Entry<T>* previous = nullptr;
    Entry<T>* current = head;

    while (current != nullptr && current->getNext() != nullptr) {
      previous = current;
      current = current->getNext();
    }

    if (current == nullptr) {
      return nullptr;
    }

    T value = *(current->getValue());

    if (previous == nullptr) {
      head = nullptr;
    } else {
      previous->setNext(nullptr);
    }

    return value;
  }

  bool isEmpty() const {
    return head == nullptr;
  }
};

int main() {
  Queue<std::string> queue;

  queue.push("1");
  queue.push("2");
  queue.push("3");

  while (!queue.isEmpty()) {
    std::cout << queue.poll() << "\n";
  }
}
```

## Off-topic: discussing the queue implementation

The above implementation of a queue is a simple linked list.

The `Entry` class is used to store both the pointer to the value (used pointers only to be able to return `nullptr` when the value does not exist)
and the pointer to the next element of the queue.

The `Queue` class stores both pointers to the first (`head`) and the last (`tail`) elements of the queue - this way.

Adding elements to the queue is as simple as replacing the `head` pointer with the new `Entry` object. If the `head` pointer is not `nullptr`, then we still
have to replace the `head` pointer with a new object, but this new object will need to store a pointer to the previous `head` element.

Removing the elements is not as straightforward - we need to find the pre-last element of the queue and set its `next` pointer to `nullptr`.

Let's analyze algorithmic complexity of `push` and `poll` methods:

* `push` - `O(1)` - simply operates on two variables; regardless of how many elements there are in the queue
* `poll` - `O(n)` - it has to iterate over all the elements of the queue to remove one

This is a relatively poor performance for the `poll` operation. We can do better than that - we can store the pointer to the last element of the queue
and operate on it:

```cpp
template <class T>
class Queue {
private:
  Entry<T>* head;
  Entry<T>* tail;

public:
  Queue() : head(nullptr), tail(nullptr) {}

  void push(T value) {
    Entry<T>* newHead = new Entry<T>(&value);

    if (head == nullptr) {
      head = newHead;
      tail = head;
    } else {
      newHead->setNext(head);
      head = newHead;
    }
  }

  T poll() {
    if (tail == nullptr) {
      return nullptr;
    }

    T value = *(tail->getValue());

    if (head == tail) {
      head = nullptr;
      tail = nullptr;
    } else {
      Entry<T>* next = head;

      while (next != nullptr && next->getNext() != tail) {
        next = next->getNext();
      }

      next->setNext(nullptr);
      tail = next;
    }

    return value;
  }

  bool isEmpty() const {
    return head == nullptr;
  }
};
```

This implemenntation's `poll` method still has a `O(n)` algorithmic complexity, since it still needs to iterate
over the whole queue to remove an element. At least now it knows when to stop.

To further improve this implementation, we can utilize the double linked list: each element of the queue will store
a pointer to *both* previous and the next element:

```cpp
template <class T>
class Entry {
private:
  T* value;
  Entry<T>* next;
  Entry<T>* previous;

public:
  Entry(T* value) : value(value), next(nullptr), previous(nullptr) {}

  T* getValue() const {
    return value;
  }

  Entry* getNext() const {
    return next;
  }

  void setNext(Entry* next) {
    this->next = next;

    if (next != nullptr) {
      next->previous = this;
    }
  }

  Entry* getPrevious() const {
    return previous;
  }
};

template <class T>
class Queue {
private:
  Entry<T>* head;
  Entry<T>* tail;

public:
  Queue() : head(nullptr), tail(nullptr) {}

  void push(T* value) {
    Entry<T>* newHead = new Entry<T>(value);

    if (head == nullptr) {
      head = newHead;
      tail = head;
    } else {
      newHead->setNext(head);
      head = newHead;
    }
  }

  T* poll() {
    if (tail == nullptr) {
      return nullptr;
    }

    T* value = tail->getValue();

    if (head == tail) {
      head = nullptr;
      tail = nullptr;
    } else {
      Entry<T>* previous = tail->getPrevious();

      previous->setNext(nullptr);
      tail = previous;
    }

    return value;
  }

  bool isEmpty() const {
    return head == nullptr;
  }
};
```

As you can see, now it does not need to iterate at all to remove an element. Instead, it stores an extra pointer
(`64 bits` aka `8 bytes`, the common size of any pointer in a 64-bit system) per element.

Although this optimization is really nice to have, it would actually make the example down below more complex to explain at first, so I will start with
the first implementation of the queue.

Simple, single-threaded queue usage could look like this:

```cpp
for (int i = 0; i < 10; ++i) {
  std::ostringstream ss;

  ss << i;

  std::string* s = new std::string(ss.str());

  queue.push(s);
}

while (!queue.isEmpty()) {
  std::string* ptr = queue.poll();

  if (ptr != nullptr) {
    std::cout << *ptr << "\n";
  }
}
```

It does push and pull the elements in order and prints them to the console.

Now, let's throw in some threads: one thread will be pushing elements to the queue ("producer") and the other one will be pulling elements from the queue ("consumer").

This is a very widely used pattern, in general - there is a queue where some data to be processed (like from the user actions) is stored, and there are multiple (usually)
workers, that pull the data from the queue and process it, not blocking the main application thread (user thread). This makes data processing asynchronous, but it
greatly improves the responsiveness of the system, when data processing is a long-lasting process.

```cpp
#include <thread>

int main() {
  Queue<std::string> queue;

  Consumer consumer(&queue);
  std::thread consumerThread(consumer);

  // needed, since consumer is an endless loop and we don't want to wait for it to finish before termination
  consumerThread.detach();

  Producer producer(&queue, 0, 10);
  std::thread producerThread(producer);

  producerThread.join();

  std::cout << "done\n";

  return 0;
}
```

<img data-src="/images/non-blocking-algorithms/queue-threads-0.png" {% imagesize "/images/non-blocking-algorithms/queue-threads-0.png":props %} alt="Queue usage with one producer thread and one consumer thread">

<!-- TODO: convert images to WEBP -->

Now, let's make it more realistic: have one producer (simulating user events) and multiple consumers (2 would be enough to highlight the idea, but 
something like 10 would be more obvious):

```cpp
for (int i = 0; i < 10; ++i) {
  Consumer consumer(&queue);
  std::thread consumerThread(consumer);

  // needed, since consumer is an endless loop and we don't want to wait for it to finish before termination
  consumerThread.detach();
}

Producer producer(&queue, 0, 10);
std::thread producerThread(producer);

producerThread.join();
```

This makes things weird:

<img data-src="/images/non-blocking-algorithms/queue-threads-1.png" {% imagesize "/images/non-blocking-algorithms/queue-threads-1.png":props %} alt="Queue usage with one producer thread and 10 consumer thread">

For more visibility, let's make both producer and consumer print out what they are working with (did not want to do this, since IO is quite a heavyweight process):

```cpp
class Producer {
private:
  Queue<std::string>* queue;
  int value;
  int maxValue;

public:
  Producer(Queue<std::string>* queue, int defaultValue = 0, int maxValue = 10) : queue(queue), value(defaultValue), maxValue(maxValue) {}

  void operator() () {
    while (value < maxValue) {
      std::ostringstream ostream;
      ostream << ++value;

      std::string* str = new std::string(ostream.str());

      queue->push(str);

      std::cout << "<- " << ostream.str() << "\n";
    }
  }
};

class Consumer {
private:
  Queue<std::string>* queue;
  int id;

public:
  Consumer(Queue<std::string>* queue, int id) : queue(queue), id(id) {}

  void operator() () {
    while (true) {
      if (queue->isEmpty()) {
        continue;
      }

      std::string str = *(queue->poll());

      std::cout << "[" << id << "] -> " << str << "\n";
    }
  }
};

int main() {
  Queue<std::string> queue;

  for (int i = 0; i < 10; ++i) {
    Consumer consumer(&queue, i);
    std::thread consumerThread(consumer);

    // needed, since consumer is an endless loop and we don't want to wait for it to finish before termination
    consumerThread.detach();
  }

  Producer producer(&queue, 0, 10);
  std::thread producerThread(producer);

  producerThread.join();

  std::cout << "done\n";

  return 0;
}
```

<img data-src="/images/non-blocking-algorithms/queue-threads-3.png" {% imagesize "/images/non-blocking-algorithms/queue-threads-3.png":props %} alt="Debugging info">

One can see, producer indeed pushes numbers from 1 to 10 to the queue, in order, one by one.

But consumers, on the other hand, sometimes pull out same element or the elements that should have been removed already.

This is a simple example of race condition. Usually it is solved using locks or mutexes, which we will try out now.

## Preventing race condition

The issue happens when one thread is trying to access the same portion of memory as the other thread is currently using.

An easy solution would be to wrap the whole `poll` and `push` methods with `mutex.lock()` and `mutex.unlock()` calls.
This would prevent all other threads from accessing the same section of the code.

But that would essentially draw the concurrency useless in this applicaiton - only one consumer thread out of 10 would be utilized:

<img data-src="/images/non-blocking-algorithms/queue-threads-lock-2.png" {% imagesize "/images/non-blocking-algorithms/queue-threads-lock-2.png":props %} alt="Thread utilization with locks">

This might be caused by the queue implementation itself - the amount of time any thread is waiting for lock whilst some other thread
is occupying it is ridiculous.

Consider the double-linked list implementation: its `poll` operation is much faster than the one with single-linked-list. Wrapping it with the `mutex`:

```cpp
template <class T>
class Queue {
private:
    Entry<T>* head;
    Entry<T>* tail;
    std::mutex lock;

public:
    Queue() : head(nullptr), tail(nullptr) {}

    void push(T* value) {
        lock.lock();

        Entry<T>* newHead = new Entry<T>(value);

        if (head == nullptr) {
            head = newHead;
            tail = head;
        }
        else {
            newHead->setNext(head);
            head = newHead;
        }

        lock.unlock();
    }

    T* poll() {
        lock.lock();

        if (tail == nullptr) {
            lock.unlock();

            return nullptr;
        }


        T* value = tail->getValue();

        if (head == tail) {
            head = nullptr;
            tail = nullptr;
        }
        else {
            Entry<T>* previous = tail->getPrevious();

            previous->setNext(nullptr);
            tail = previous;
        }

        lock.unlock();

        return value;
    }

    bool isEmpty() {
        lock.lock();

        bool result = head == nullptr;

        lock.unlock();

        return result;
    }
};
```

<img data-src="/images/non-blocking-algorithms/queue-double-linked-list-threads-lock-3.png" {% imagesize "/images/non-blocking-algorithms/queue-double-linked-list-threads-lock-3.png":props %} alt="Double-linked list with lock">

<img data-src="/images/non-blocking-algorithms/queue-double-linked-list-threads-lock-2.png" {% imagesize "/images/non-blocking-algorithms/queue-double-linked-list-threads-lock-2.png":props %} alt="Double-linked list with lock">

Much better - now more than one thread is actually doing some work. But now instead of synchronization and sleeping, threads
are mostly busy actively waiting for lock to unlock.

Long story short, locks do the trick.
But the article is called "non-blocking algorithms" and using locks in the example above is a perfect example of a **blocking** algorithm.

## Analyzing the issue

First, we have to determine what is the issue that locks are solving here. Let's look at two examples of how threads are abusing our program without locks.

### Polling head and pushing to queue

Assume queue consists of just one element:

```
head: { value: 42, next: nullptr, previous: nullptr }
tail: { value: 42, next: nullptr, previous: nullptr }

tail = head
```

One thread `t1` is polling the queue and removes the only existing element.
At the same time, another thread `t2` is kicking in and tries to add an element to the queue.

<div class="row">
  <div class="col">

```cpp
if (tail == nullptr) {
  return nullptr;
}

T* value = tail->getValue();

if (head == tail) {
  head = nullptr;
  tail = nullptr;
} else {
  Entry<T>* previous = tail->getPrevious();

  previous->setNext(nullptr);
  tail = previous;
}
```

  </div>
  <div class="col">

```cpp
// this line is not essential for this analysis
// Entry<T>* newHead = new Entry<T>(value);

if (head == nullptr) {
  head = newHead;
  tail = head;
} else {
  newHead->setNext(head);
  head = newHead;
}
```

  </div>
</div>

What can potentially go wrong here (trollface)? Given we have no locks in place?
Well, numerous combinations. Literally. Let's consider simple cases first:

1. `t1` reads `tail` value first (when executing `tail == nullptr` at line `1` or `tail->getValue()` at line `5` of first code snippet)
2. `t1` reads `head` value first (when executing `head == tail` at line `7` of first code snippet)
3. `t1` writes `head` value first (when executing `head = nullptr` at line `8` of first code snippet)
4. `t1` writes `tail` value first (when executing `tail = nullptr` at line `9` of first code snippet)
5. `t2` reads `head` value first (when executing `head == nullptr` at line `4` of second code snippet)
6. `t2` writes `head` value first (when executing `head = newHead` at line `9` of second code snippet)

<!-- 4. `t2` writes `head` value first (when executing `head = newHead` at line `5` of second code snippet)
5. `t2` writes `tail` value first (when executing `tail = head` at line `6` of second code snippet) -->

These are few examples of how processor might execute the program (in what order). They bring various consequences to how other thread might be executed,
and that's why I did not list (yet) the other `if` branch of each code snipped.

All those situations above come down to four possible situations:

1. `t1` reads `head` value and immediately after that `t2` writes `head` value
2. `t1` reads `tail` value and immediately after that `t2` writes `tail` value
3. `t2` reads `head` value and immediately after that `t1` writes `head` value
4. `t2` reads `tail` value and immediately after that `t1` writes `tail` value

This issue comes from the concurrent nature of the program itself.
