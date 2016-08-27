(ns clojure-chess.army.bishop
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessboard :refer :all]
            [clojure-chess.fields :refer :all]))

(defn- get-bishop-possible-up-left-moves
  "Returns list of maps that represents fields from excluded xy to the end of the board over up-left diagonal. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (let [x-values (reverse (take (x-as-num x) x-value))
          x-count (count x-values)]
    (map #(hash-map %1 %2) x-values (take x-count (range (inc y) 8))))))

(defn- get-bishop-possible-up-right-moves
  "Returns list of maps that represents fields from excluded xy to the end of the board over up-right diagonal. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (let [x-values (drop (inc (x-as-num x)) x-value)
          x-count (count x-values)]
    (map #(hash-map %1 %2) x-values (take x-count (range (inc y) 8))))))

(defn- get-bishop-possible-down-left-moves
  "Returns list of maps that represents fields from excluded xy to the end of the board over down-left diagonal. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (let [x-values (reverse (take (x-as-num x) x-value))
          x-count (count x-values)]
    (map #(hash-map %1 %2) x-values (take x-count (reverse (range 0 y)))))))

(defn- get-bishop-possible-down-right-moves
  "Returns list of maps that represents fields from excluded xy to the end of the board over down-right diagonal. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (let [x-values (drop (inc (x-as-num x)) x-value)
          x-count (count x-values)]
    (map #(hash-map %1 %2) x-values (take x-count (reverse (range 0 y)))))))

(defn bishop-moves-vectors
  "Returns vector containing 4 vectors (for each diagonal direction) that represents all possible moves 
   on the empty board from possition xy. If possiton xy is not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (reduce #(conj %1 (%2 x y)) [] [get-bishop-possible-up-left-moves get-bishop-possible-up-right-moves 
                                    get-bishop-possible-down-left-moves get-bishop-possible-down-right-moves])))
