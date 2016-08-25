(ns clojure-chess.army.rook
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessman :refer :all]
            [clojure-chess.chessboard :refer :all]))

(defn get-rook-possible-up-moves
  "Returns list of maps that represents fields from excluded xy to the top edge of the board. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (map #(hash-map x %) (range (inc y) 8))))

(defn get-rook-possible-down-moves
  "Returns list of maps that represents fields from excluded xy to the bottom edge of the board. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (map #(hash-map x %) (reverse (range 0 y)))))

(defn get-rook-possible-right-moves
  "Returns list of maps that represents fields from excluded xy to the right edge of the board. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (map #(hash-map (x-as-key %) y) (range (if (nil? (inc-x x)) 8 (x-as-num (inc-x x))) 8))))

(defn get-rook-possible-left-moves
  "Returns list of maps that represents fields from excluded xy to the left edge of the board. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (map #(hash-map (x-as-key %) y) (reverse (range 0 (x-as-num x))))))

(defn rook-moves-vectors
  "Returns vector containing 4 vectors (for each direction) that represents all possible moves 
   on the empty board from possition xy. If possiton xy is not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (reduce #(conj %1 (%2 x y)) [] [get-rook-possible-up-moves get-rook-possible-down-moves 
                                    get-rook-possible-right-moves get-rook-possible-left-moves])))

(defn get-rook-possible-moves
  "Returns map {:eat {...} :move {...}} with all possible moves for the rook located on xy field. Moves are separated in two maps: 
   1. :eat - Where rook can eat opponent chessman
   2. :move - Where rook can be placed (fields that are not occupied). 
   If xy is not valid possition or xy is not occupied by rook, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \r)
    (get-chessman-possible-moves chessboard x y (rook-moves-vectors x y))))
