---
layout: post
title: Abstract classes vs interfaces
date: '2015-02-03T20:31:41+01:00'
tags:
- java
- tutorial
tumblr_url: http://shybovycha.tumblr.com/post/109996204061/abstract-classes-vs-interfaces
---

Lately I was a few times asked a question _"what do we need abstract classes for?"_

And today I've got one more question, which inspired me to write this.

Let us have a task to write an application, which will perform some simple arithmetic operations on numbers, represented in a different numeric systems.

For example, our program should be able to **add** numbers in **roman**, **arabic** and **hexadecimal** systems. But later we should be able to add more operations, like **division**, **multiplication** and **subtraction**.

This is where an **abstract class** comes to help us!

Rather than write an **interface** `Number`, which will define the addition operator and then implementing it for each class like `RomanNumber` and `HexadecimalNumber` we will better use an **abstract class**, which will be able to add decimal numbers and will declare abstract method to convert number itself to decimal system.

Take a look:

```java
public interface INumber {
    public INumber add(INumber other);
}

public class RomanNumber implements INumber {
    public RomanNumber add(INumber other) {
        // when `other` is not a RomanNumber
        // convert other to RomanNumber
        // add `other` to `this`
    }
}

public class HexadecimalNumber implements INumber {
    public HexadecimalNumber add(INumber other) {
        // convert other to HexadecimalNumber and add...
    }
}
```

...and compare it to this:

```java
public abstract class Number {
    public abstract Integer toDecimal();
    public abstract Number fromDecimal(Integer);

    public Number add(Number other) {
        returh fromDecimal(this.toDecimal() + other.toDecimal());
    }
}

public class RomanNumber extends Number {
    public Integer toDecimal() {
        // convert `this` to Integer
    }

    public RomanNumber fromDecimal(Integer n) {
        // convert `n` to RomanNumber
    }
}

public class HexadecimalNumber extends Number {
    public Integer toDecimal() {
        // convert `this` to Integer
    }

    public HexadecimalNumber fromDecimal(Integer n) {
        // convert `n` to HexadecimalNumber
    }
}
```

This is how we can create an abstraction: we can add or perform any other arithmetic operations regardless on which numeric system we use!

Wneh we declare an **interface**, we can't tell how to create an object, implementing that interface. E. g., we can not define even a default constructor.

Thus, when we need to have at least one constructor of a defined signature, we must use **abstract classes**.

Check it out:

```java
public interface IVector {
    public void Vector(); // something like a default constructror
}

public class Vector2d implements IVector {
    // THIS IS NOT A CONSTRUCTOR!
    @Override public void Vector() { }
}
```

and compare it to this:

```java
public abstract class BaseVector {
    public abstract BaseVector() { }
}

public class Vector2d extends BaseVector {
    public Vector2d() {
        // now it DOES HAVE a default constructor super();
    }
}
```
