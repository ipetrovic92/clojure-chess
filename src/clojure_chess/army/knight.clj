(ns clojure-chess.army.knight  
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessboard :refer :all]
            [clojure-chess.fields :refer :all]))

(defn- get-knight-possible-up-moves
  "Returns sequence that represents valid fields where knight can potentially move from position xy 
   (first two field up, than one field left or right). If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y) 
    (if (valid-y? (+ 2 y))
      (let [potential-up-moves (filter valid-x? [(dec-x x) (inc-x x)])]
        (map #(hash-map % (+ 2 y)) potential-up-moves))
      [])))

(defn- get-knight-possible-down-moves
  "Returns sequence that represents valid fields where knight can potentially move from position xy 
   (first two field down, than one field left or right). If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y) 
    (if (valid-y? (- y 2))
      (let [potential-down-moves (filter valid-x? [(dec-x x) (inc-x x)])]
        (map #(hash-map % (- y 2)) potential-down-moves))
      [])))

(defn- get-knight-possible-left-moves
  "Returns sequence that represents valid fields where knight can potentially move from position xy 
   (first two field left, than one field up or down). If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y) 
    (if (valid-x? (dec-x (dec-x x)))
      (let [potential-left-moves (filter valid-y? [(inc y) (dec y)])]
        (map #(hash-map (dec-x (dec-x x)) %) potential-left-moves))
      [])))

(defn- get-knight-possible-right-moves
  "Returns sequence that represents valid fields where knight can potentially move from position xy 
   (first two field right, than one field up or down). If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y) 
    (if (valid-x? (inc-x (inc-x x)))
      (let [potential-right-moves (filter valid-y? [(inc y) (dec y)])]
        (map #(hash-map (inc-x (inc-x x)) %) potential-right-moves))
      [])))

(defn knight-moves-vectors
  "Returns vector containing all possible moves on the empty board from possition xy. 
   If possiton xy is not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (flatten (reduce #(conj %1 (%2 x y)) [] [get-knight-possible-up-moves get-knight-possible-down-moves 
                                             get-knight-possible-left-moves get-knight-possible-right-moves]))))
