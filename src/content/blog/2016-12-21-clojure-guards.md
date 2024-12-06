---
layout: post
title: Clojure guards
categories: []
tags: []
published: True
date: '2016-12-21T12:54:00'
---

Once I wanted to have something like a pretty "match" operator from Scala (or Rust, or C# 7), but in Clojure.
In this blog I explore some solutions for that available in Clojure world.

## Use case

Generally, you would use pattern matching as a neat syntax for multiple `if..else` or `switch..case` statements.
Consider this example of transforming values between two enums:

```cpp
if (entityType == DBEntityType::ISSUE) {
    return UIEntityType::ISSUE;
} else if (entityType == DBEntityType::SUBTASK) {
    return UIEntityType::SUBTASK;
} else {
    return UIEntityType::UNKNOWN;
}
```

This could be written neatly with a `switch..case` statement:

```cpp
switch (entityType) {
    case DBEntityType::ISSUE:
        return UIEntityType::ISSUE;
    case DBEntityType::SUBTASK:
        return UIEntityType::SUBTASK;
    default:
        return UIEntityType::UNKNOWN;
}
```

In C# starting with version 7 you could write this using switch expression:

```csharp
return entityType switch {
    DBEntityType.ISSUE => UIEntityType.ISSUE,
    DBEntityType.SUBTASK => UIEntityType.SUBTASK,
    _ => UIEntityType.UNKNOWN
};
```

But what if you have multiple conditions to check?

```cpp
if (entityType == DBEntityType::ISSUE && permissions.canEdit(user, DBEntityType::ISSUE)) {
    return UIEntityType::ISSUE;
} else if (entityType == DBEntityType::SUBTASK && permissions.canEdit(user, DBEntityType::SUBTASK)) {
    return UIEntityType::SUBTASK;
} else {
    return UIEntityType::UNKNOWN;
}
```

This can not be converted to `switch..case` nicely. But with switch expressions (or rather, pattern matching, in general) you can do something like this:

```csharp
return entityType switch {
    DBEntityType::ISSUE when permissions.canEdit(user, DBEntityType::ISSUE) => UIEntityType::ISSUE,
    DBEntityType::SUBTASK when permissions.canEdit(user, DBEntityType::SUBTASK) => UIEntityType::SUBTASK,
    _ => UIEntityType::UNKNOWN,
};
```

## Using `cond`

```clojure
;; Note how this method does not check the `n > 3` case
(defn testFn [n]
    (cond
        (= n 1) "You entered 1!"
        (= n 2) "You entered two!"
        (= n 3) "You entered three!"
        :else "You entered a big number!"
    ))

(defn complexFn [entityType user permissions]
    (cond
        (and (= entityType :db-issue) (can-edit? permissions user :db-issue) :ui-issue)
        (and (= entityType :db-subtask) (can-edit? permissions user :db-subtask) :ui-subtask)
        :else :ui-unknown))
```

## Using `condp`

```clojure
;; Note how this method does not check the `n > 3` case
(defn testFn [n]
    (condp = n
        1 "You entered 1!"
        2 "You entered two!"
        3 "You entered three!"
        "You entered a big number!"))

(defn complexFn [entityType user permissions]
    (condp = [ entityType true ]
        [ :db-issue (can-edit? permissions user :db-issue) ] :ui-issue
        [ :db-subtask (can-edit? permissions user :db-subtask) ] :ui-subtask
        :ui-unknown))
```

## Using `guard` macro

```clojure
(defn generateGuardBody
    "Generates the function body required to support the guard macro"
    [args]
    (let [[thisBranch remainingBranches] (split-at 2 args)
            testCond (first thisBranch)
            testResult (second thisBranch)
            elseFunc (if (empty? remainingBranches)
                false
                (if (=  1 (count remainingBranches))
                    (first remainingBranches)
                    (generateGuardBody remainingBranches)))]
        `(if ~testCond
            ~testResult
            ~elseFunc)))

(defmacro defguardfn
    "Creates a haskell-style guarded function"
    [fnName args & body]
    `(defn ~fnName ~args
        ~(generateGuardBody body)))

(defmacro guard
    "Allows inline guard syntax without surrounding defn"
    [& body]
    `~(generateGuardBody body))


(defguardfn testFn [n]
    (= 1 n) "You entered 1!"
    (= 2 n) "You entered two!"
    (= 3 n) "You entered three!"
    (> n 3) "You entered a big number!")

(println (testFn 5))

(defguardfn fib [n]
    (< n 2) n
    (+ (fib (- n 1)) (fib (- n 2))))


(defn testFn-2 [x]
    (guard
        (= x 1) "One!"
        (= x 2) "Two!"
        true  "Something Else!"))

(println (map fib (range 0 10)))
(println (testFn-2 3)) ; "Something else!"
```

## Using multimethods

```clojure
;; Implementing factorial using multimethods Note that factorial-like function
;; is best implemented using `recur` which enables tail-call optimization to avoid
;; a stack overflow error. This is a only a demonstration of clojure's multimethod

;; identity form returns the same value passed
(defmulti factorial identity)

(defmethod factorial 0 [_]  1)
(defmethod factorial :default [num]
    (* num (factorial (dec num))))

(factorial 0) ; => 1
(factorial 1) ; => 1
(factorial 3) ; => 6
(factorial 7) ; => 5040
```

## Using `core.match`

```clojure
(defn div3? [x] (zero? (rem x 3)))
(let [y [2 3 4 5]]
  (match [y]
    [[_ (a :guard even?) _ _]] :a0
    [[_ (b :guard [odd? div3?]) _ _]] :a1))

(let [x [1 2 3]]
  (match [x]
    [[1 (:or 3 4) 3]] :a0
    [[1 (:or 2 3) 3]] :a1))
;=> :a1

(let [x {:a 3}]
  (match [x]
    [{:a (:or 1 2)}] :a0
    [{:a (:or 3 4)}] :a1))
;=> :a1

(let [v [[1 2]]]
  (match [v]
    [[[3 1]]] :a0
    [[([1 a] :as b)]] [:a1 a b]))
;=> [:a1 2 [1 2]]
```