(ns clojure-chess.rook
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessman :refer :all]
            [clojure-chess.chessboard :refer :all]))

(defn- get-rook-possible-up-moves
  "Returns list of maps that represents fields from excluded xy to the top edge of the board. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (map #(hash-map x %) (range (inc y) 8))))

(defn- get-rook-possible-down-moves
  "Returns list of maps that represents fields from excluded xy to the bottom edge of the board. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (map #(hash-map x %) (reverse (range 0 y)))))

(defn- get-rook-possible-right-moves
  "Returns list of maps that represents fields from excluded xy to the right edge of the board. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (map #(hash-map (x-as-key %) y) (range (if (nil? (inc-x x)) 8 (x-as-num (inc-x x))) 8))))

(defn- get-rook-possible-left-moves
  "Returns list of maps that represents fields from excluded xy to the left edge of the board. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (map #(hash-map (x-as-key %) y) (reverse (range 0 (x-as-num x))))))

(defn get-rook-possible-moves
  
  [chessboard x y]
  (if (and (valid-xy? x y) 
           (occupied? chessboard x y) 
           (is-chessman-type? chessboard x y \r))
    (reduce (fn [new-map possible-moves-func]
              (let [moves (count (:move new-map))
                    eats (count (:eat new-map))
                    possible-moves (possible-moves-func x y)
                    move-fields (take-while #(let [key (get (map->vec %) 0)
                                                   val (get (map->vec %) 1)]
                                               (not-occupied? chessboard key val)) possible-moves)
                    move-count (count move-fields)
                    eat-field (let [to-eat? (first (drop move-count possible-moves))
                                    key (get (map->vec to-eat?) 0)
                                    val (get (map->vec to-eat?) 1)]
                                (if (and (not-nil? to-eat?) (not-same-color? chessboard x y key val))
                                  to-eat?))
                    eat-count (count eat-field)]
                (cond
                  (and (not-nil? move-count) (> move-count 0)
                       (not-nil? eat-count) (> eat-count 0)) 
                  (assoc-in (assoc-in new-map [:move] (vec (flatten (conj (:move new-map) move-fields)))) [:eat] (vec (flatten (conj (:eat new-map) eat-field))))
                  (and (not-nil? move-count) (> move-count 0)) (assoc-in new-map [:move] (vec (flatten (conj (:move new-map) move-fields))))
                  (and (not-nil? eat-count) (> eat-count 0)) (assoc-in new-map [:eat] (vec (flatten (conj (:eat new-map) eat-field))))
                  :else new-map)))
            {:move []
             :eat []}
            [get-rook-possible-up-moves get-rook-possible-down-moves 
             get-rook-possible-right-moves get-rook-possible-left-moves])))
