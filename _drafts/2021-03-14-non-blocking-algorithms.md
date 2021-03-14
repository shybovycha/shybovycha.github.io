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

## Non-demonstrative example

```cpp
#include <iostream>
#include <string>
#include <sstream>
#include <thread>

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

  void push(T* value) {
    Entry<T>* newHead = new Entry<T>(value);

    if (head == nullptr) {
      head = newHead;
    } else {
      newHead->setNext(head);
      head = newHead;
    }
  }

  T* poll() {
    Entry<T>* previous = nullptr;
    Entry<T>* current = head;

    while (current != nullptr && current->getNext() != nullptr) {
      previous = current;
      current = current->getNext();
    }

    if (current == nullptr) {
      return nullptr;
    }

    T* value = current->getValue();

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
      
      std::string *str = new std::string(ostream.str());

      queue->push(str);
    }
  }
};

class Consumer {
private:
  Queue<std::string>* queue;

public:
  Consumer(Queue<std::string>* queue) : queue(queue) {}

  void operator() () {
    while (true) {
      if (queue->isEmpty()) {
        continue;
      }

      std::string str = *(queue->poll());

      std::cout << "->" << str << "\n";
    }
  }
};

int main() {
  Queue<std::string> queue;

  Producer producer(&queue, 0, 1000);
  std::thread producerThread(producer);

  Consumer consumer1(&queue);
  std::thread consumerThread1(consumer1);

  Consumer consumer2(&queue);
  std::thread consumerThread2(consumer2);

  Consumer consumer3(&queue);
  std::thread consumerThread3(consumer3);

  Consumer consumer4(&queue);
  std::thread consumerThread4(consumer4);

  producerThread.join();
  consumerThread1.join();
  consumerThread2.join();
  consumerThread3.join();
  consumerThread4.join();

  return 0;
}
```

https://replit.com/@ArtemShoobovych/SlipperyColdMinimalsystem#main.cpp

## Simple example

```cpp
#include <iostream>
#include <thread>

class Producer {
private:
    int* value;
    int maxValue;

public:
    Producer(int* value, int maxValue = 10) : value(value), maxValue(maxValue) {}

    void operator() () {
        for (int i = 0; i < maxValue; ++i) {
            *value += 1;
        }
    }
};

int main() {
    int accumulator = 0;

    Producer p1(&accumulator, 1000);
    std::thread t1(p1);

    Producer p2(&accumulator, 1000);
    std::thread t2(p2);

    Producer p3(&accumulator, 1000);
    std::thread t3(p3);

    Producer p4(&accumulator, 1000);
    std::thread t4(p4);

    t1.join();
    t2.join();
    t3.join();
    t4.join();

    std::cout << accumulator << "\n";

    return 0;
}
```
