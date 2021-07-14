---
layout: post
title: "Dijkstra algorithm in Clojure"
---

```clj
(def G {:1 [[:2 1], [:3 2]]
        :2 [[:4 4]]
        :3 [[:4 2]]
        :4 []})

(defn reconstruct-path
  ([start end previous] (reconstruct-path start end previous [end]))

  ([start end previous path]
   (if (nil? end)
     nil
     (if (= end start)
       path
       (let [through (get previous end)]
         (recur start through previous (conj path through)))))))

(defn dijkstra
  ([graph start end]
   (let [vertices (keys graph)
         distances (assoc (reduce (fn [acc vertex] (assoc acc vertex Integer/MAX_VALUE)) {} vertices) start 0)
         previous {}]
     (dijkstra graph start start end vertices distances previous)))

  ([graph start end queue distance previous]
   (if (empty? queue)
     {:distance (get distance start) :path (reconstruct-path start end previous)}
     (let [u (peek queue)
           neighbour-edges (get graph u)
           neighbours (map first neighbour-edges)
           new-distance (reduce
                         (fn [acc [v length]]
                           (let [dist-u (get acc u)
                                 dist-v (get acc v)
                                 alt (+ dist-u length)]
                             (if (< alt dist-v) (assoc acc v alt) acc)))
                         distance
                         neighbour-edges)
           new-previous (reduce
                         (fn [acc [v length]]
                           (let [dist-u (get acc u)
                                 dist-v (get acc v)
                                 alt (+ dist-u length)]
                             (if (< alt dist-v) (assoc acc v u) acc)))
                         previous
                         neighbour-edges)]
       (recur graph start end (pop queue) new-distance new-previous)))))

(defn product
  ([list1 list2] (product list1 list2 []))
  ([list1 list2 acc]
   (if (empty? list1)
     acc
     (recur (rest list1) list2 (concat acc (map #(conj [(first list1)] %) list2))))))

(defn to-keyword [n] (keyword (str n)))

(defn edges-to-map [edges]
  (reduce
   (fn [acc [from1 to1 length]]
     (let [from (to-keyword from1)
           to (to-keyword to1)
           existing-edges (get acc from)]
       (-> acc
           (assoc to [])
           (assoc from (conj (if (nil? existing-edges) [] existing-edges) (conj '() length to))))))
   {}
   edges))

(defn generate-graph [n s]
  (if (or (< s (dec n)) (> s (* n (dec n))))
    nil
    (let [vertices (range 1 (inc n))]
      (map
       (fn [x]
         (conj nil (rand-nth (map to-keyword (remove #{x} vertices))) (to-keyword x)))
       vertices))))
    ;; (reduce (fn [acc i] (assoc acc (to-keyword i) [])) {} (range 1 (inc n)))))
    ;; (zipmap (map #(keyword (str %))) ())))
```
