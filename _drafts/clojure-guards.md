---
layout: post
title: clojure-guards
categories: []
tags: []
published: True

---

# Clojure guards

## Using `guard` macro

{% highlight clojure %}
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
{% endhighlight %}

## Using multimethods

{% highlight clojure %}
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
{% endhighlight %}

## Using `core.match`

{% highlight clojure %}
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
{% endhighlight %}