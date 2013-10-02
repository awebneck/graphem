(ns graphem.core
  [require [clojure.set :refer [difference]]])

(defn make-graph
  [& [undirected]]
  (with-meta {} {:undirected (boolean undirected)}))

(defn- undirected?
  [g]
  (:undirected (meta g)))

(defn add-edge
  [g from to weight]
  (let [g (assoc g from (assoc (or (g from) {}) to weight))]
    (if (undirected? g)
      (assoc g to (assoc (or (g to) {}) from weight))
      g)))

(defn delete-edge
  [g from to]
  (let [g (assoc g from (dissoc (or (g from) {}) to))]
    (if (undirected? g)
      (assoc g to (dissoc (or g to) {}) from)
      g)))

(defn neighbors
  [g from]
  (keys (g from)))

(defn dfs
  [g from]
  (loop [frontier [from]
         visited  #{from}
         result   []]
    (if (empty? frontier)
      result
      (let [current   (peek frontier)
            neighbors (neighbors g current)]
        (println frontier)
        (recur (vec (remove visited (into (pop frontier) neighbors))) (conj visited current) (conj result current))))))

(defn bfs
  [g from]
  (loop [frontier [from]
         visited  #{from}
         result   []]
    (if (empty? frontier)
      result
      (let [current   (first frontier)
            neighbors (neighbors g current)]
        (println frontier)
        (recur (into (vec (rest frontier)) (remove visited neighbors)) (into visited neighbors) (conj result current))))))

(defn- calc-tentative
  [current current-dist dists]
  (reduce (fn [acc [skey val]] (assoc acc skey [current (+ val current-dist)])) {} dists))

(defn- replace-smaller
  [dists tdists]
  (reduce (fn [acc [skey [dkey val]]]
            (if (or (nil? (acc skey))
                    (< val ((acc skey) 1)))
              (assoc acc skey [dkey val])
              acc))
          dists tdists))

(defn- next-closest
  [exclude dists]
  ((reduce (fn [acc [skey [_ val]]]
             (if (and (< val (acc 1)) (not (nil? skey)))
               [skey val]
               acc)) [nil Double/POSITIVE_INFINITY] (apply dissoc dists exclude)) 0))

(defn- sp-dists
  [g from]
  (loop [dists   {from [nil 0]}
         visited #{}
         current from]
    (if (= (count visited) (count g))
      dists
      (let [dt    (calc-tentative current ((dists current) 1) (g current))
            dists (replace-smaller dists dt)
            nxt   (next-closest (conj visited current) dists)]
        (recur dists (conj visited current) nxt)))))

(defn sp-tree
  [g from]
  (dissoc (reduce (fn [acc [skey [dkey val]]] (add-edge acc dkey skey val)) (make-graph) (sp-dists g from)) nil))
