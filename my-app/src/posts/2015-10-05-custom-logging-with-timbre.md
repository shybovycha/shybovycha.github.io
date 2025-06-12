---
layout: post
title: Custom logging with timbre
categories: []
tags: []
published: True

---

## Timbre?

At my job we recently started researching logging tools to make our RESTful API, written in Clojure,
writing logs in JSON format. We were using **Log4j** already, but decided to use another tool for
this task, making it less painful. So we felt into **timbre**. Is seemed so easy to use, but it is
really undocumented.

According to timbre's API, we needed to define our own **appender** for writing to a custom JSON file.
And we found the `output-fn` option to configure this behaviour. But it is not documented at all,
so we started looking for repositories, using timbre, examples and all the stuff. And finally,
we ended up with our own solution.

Underneath you will find description of our way to use timbre from scratch.

<!--more-->

## Getting started

First of all, you will need a **Leiningen** project. We already have one, but you may want to create
a blank one for the test purpose. But beware, you'd better generate it from an `app` template, so
when you run it with `lein run`, you face not the problem I faced. The problem was an error I could
not get rid of, saying `Cannot find anything to run for: my-ns`. So I just re-generated project from
scratch and the problem gone. *I did no research deeper, but if I will, I'll write about it for sure*.

Generating project from an `app` template is done really easily:

```bash
lein new app your-app-name
```

The process will be finished in seconds. Then you'll need to add a few dependencies in your `project.clj`:

```clojure
(defproject your-app-name
    ; ...
    :dependencies [[org.clojure/clojure "1.6.0"]
                 [com.taoensso/timbre "4.1.4"]
                 [org.clojure/data.json "0.2.6"]]
    ; ...
)
```

To install dependencies for your project, simply run `lein deps`.

## Configuring timbre

To use timbre in your project all you need is to require it:

```clojure
(ns timbre-test1.core
  (:require [taoensso.timbre :as timbre :refer (log info warn debug error)]
            [clojure.data.json :as json]))

(defn -main [& args]
  (info {:moo -3.14} {:foo :bar})
  (info {:some-complicated-hash {:inner-hash {:more-inner ["value1" "value2"]}}})
  (info "Hello, World!")
  (error "SOMETHING WENT WELL"))
```

This example shows one very important feature of timbre: **it can log objects, other than strings and
exceptions**. This is really handy! Doing so, timbre is able to log a JSON object simply, almost
out-of-the-box.

## Custom configuration

Now the time has come for something non-trivial. We need to set up our own appender to force timbre
log everything in JSON.

There is a set of default appenders in timbre already. For instance, there is a `spit` appender,
writing logs to a file. And the default timbre's appender, writing logs to `stdout`, is called
`sample`. But there's a trick: each appender in timbre should be configured by its **id**, not
by its **type**. So you may have two different `spit` appenders, for example, writing to different
files.

Timbre provides two methods of configuration:

* **merging**, when you can override only some options of default config
* **setting**, when you override all the default options at once

We used the first way, because some of timbre's options were mystical to us - if we do not set them,
we will get no output at all. And we have not researched which ones we need to set.

At first, we wrote a simple config, forcing logging to be printed onto a screen with some JSON
formatting:

```clojure
(ns timbre-test1.core
  (:require [taoensso.timbre :as timbre :refer (log info warn debug error)]
            [clojure.data.json :as json]))


(defn json-output-fn
  [{:keys [vargs_ hostname_ timestamp_ level] :as args}]
  (let [
        messages (map (fn [msg] { :timestamp @timestamp_
                                  :level     level
                                  :hostname  @hostname_
                                  :message   msg })
                   @vargs_)
        json-messages (map #(json/write-str %) messages)]
    (clojure.string/join "\n" json-messages)))


(defn -main [& args]
  (timbre/merge-config!
    {:appenders {
                 :println {
                    :output-fn json-output-fn
                 }}})
  (info {:moo -3.14} {:foo :bar})
)
```

Note the appender id: `println`. This code overrides `output-fn` for `println` appender *(e. g. the
default one)*. The parameter for `output-fn` is a function, receiving a map with different keys. The
most important ones are `vargs_`, `hostname_`, `timestamp_` and `level`. Those are enough to format
the correct and full message with any format you might want.

The `vargs_` variable contains all the arguments, passed to a logging call *(`info`, `warn`, `error` -
any function from `timbre` namespace, which performs logging)*. And the `clojure.data.json/write-str`
function converts its params to JSON and stringifies it, returning valid JSON as a string.

A function should return a string, representing a message, which will be written to a screen or a file,
depending on appender.

`level` argument is set by a macro, you are logging with. For example, `timbre/info` will set
`level` to `"info"`, `timbre/error` - to `"error"` and so on.

`hostname_` and `timestamp_` are helper arguments, you might not need them in all the use cases.
But the `timestamp_` one is really helpful. Always.

Then we overrided the default file appender, extending it with our `json-output-fn`:

```clojure
(ns timbre-test1.core
  (:require [taoensso.timbre :as timbre :refer (log info warn debug error)]
            [taoensso.timbre.appenders.core :as appenders]
            [clojure.data.json :as json]))


(defn json-output-fn
  [{:keys [vargs_ hostname_ timestamp_ level] :as args}]
  (let [
        messages (map (fn [msg] {:timestamp @timestamp_
                                  :level     level
                                  :hostname  @hostname_
                                  :message   msg})
                   @vargs_)
        json-messages (map #(json/write-str %) messages)]
    (clojure.string/join "\n" json-messages)))


(defn -main [& args]
  (timbre/merge-config!
    {:appenders {
                 :spit (merge (appenders/spit-appender {:fname "timbre.log"}) {:output-fn json-output-fn})
                 }})
  (info {:moo -3.14} {:foo :bar})
)
```

Nothing new here, except of the appender used - now the application will log to both file and screen.
To prevent logging to `stdout`, we disabled the `println` appender:

```clojure
(defn -main [& args]
  (timbre/merge-config!
    {:appenders {
                 :println {:enabled? true}
                 :spit (merge (appenders/spit-appender {:fname "timbre.log"}) {:output-fn json-output-fn})
                 }})
  (info {:moo -3.14} {:foo :bar})
)
```

Here I should mention that we were not made to call our JSON appender `spit` - it was used for convention
reasons only.

And that's basically it - we made our logging to be written to a JSON file only.
