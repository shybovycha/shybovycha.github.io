---
title: 'RESTful service in Clojure'
date: '2023-05-18T16:25:00+10:00'
---

## Intro

A long time ago I was working on a project aimed to provide a shopping cart for an existing legacy (Java 1.4, IIRC) e-commerce solution.
Since the existing platform was way too outdated even back then, extending the existing solution was out of question - too much effort.
The idea was to create an application (microservices were not a trend back in 2015) in a JVM-compatible language and access it via a REST API.

For that task, Clojure as a language was selected with a suite of libraries (to make life easier):

* Ring
* Compojure
* Liberator
* SpeCLJ

To this day, I find the code we built quite neat. This was mostly thanks to the combination of those libraries, which together created
a nice framework to build a REST API.

For once, the routes were defined in Sinatra / Express.js style:

```clj
(defroutes app
  (GET "/" [] "<h1>Hello World</h1>")
  (route/not-found "<h1>Page not found</h1>"))
```

On top of that, the requests were enriched using middlewares (also very Express.js style):

```clj
(defn wrap-current-user-id [handler]
  (fn [request]
    (let [
            user-id (-> request :session :user-id)
            request2 (assoc request :user-id user-id)
        ]
      (handler request2))))
```

This entire style of defining HTTP routes and request handling was defined by Compojure.

The underlying HTTP framework was Ring, which allowed for all that goodiness.

But the thing I want to touch the most upon is the way each request was handled in a RESTful manner:

```clj
(defroutes app
  (ANY "/secret" []
       (resource :available-media-types ["text/html"]
                 :exists? (fn [ctx]
                            (= "tiger" (get-in ctx [:request :params "word"])))
                 :handle-ok "You found the secret word!"
                 :handle-not-found "Uh, that's the wrong word. Guess again!")))
```

This is the style implemented by Liberator.
It has a state machine, or a flow graph, where request flows through every stage of it and if there is a handler defined for a specific
stage of the graph, it will be executed as a middleware.

It is essentially a big state machine wich each state defined as "continue to next":

<img data-src="/images/restful-service-in-clojure/decision-graph.svg" alt="Liberator request decision graph" />

Hence our entire application looked like this:

```clj
(defn secret-exists? [ctx]
    (= "tiger" (get-in ctx [:request :params "word"])))

(def secret-resource
    (resource
        :available-media-types ["text/html"]
        :exists? secret-exists?
        :handle-ok "You found the secret word!"
        :handle-not-found "Uh, that's the wrong word. Guess again!"))

(defroutes app
  (ANY "/secret" [] secret-resource))
```
