(ns clojure-chess.army.king
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessman :refer :all]
            [clojure-chess.chessboard :refer :all]))

(defn get-king-possible-up-row-moves
  "Returns sequence that represents valid fields where king can potentially move from position xy 
   (up-left, up or up-right field from xy field). If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y) 
    (if (valid-y? (inc y))
      (let [potential-up-row-moves (filter valid-x? [(dec-x x) x (inc-x x)])]
        (map #(hash-map % (inc y)) potential-up-row-moves))
      [])))

(defn get-king-possible-down-row-moves
  "Returns sequence that represents valid fields where king can potentially move from position xy 
   (down-left, down or down-right field from xy field). If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y) 
    (if (valid-y? (dec y))
      (let [potential-down-row-moves (filter valid-x? [(dec-x x) x (inc-x x)])]
        (map #(hash-map % (dec y)) potential-down-row-moves))
      [])))

(defn get-king-possible-same-row-moves
  "Returns sequence that represents valid fields where king can potentially move from position xy 
   (left or right field from xy field). If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y) 
      (let [potential-same-row-moves (filter valid-x? [(dec-x x) (inc-x x)])]
        (map #(hash-map % y) potential-same-row-moves))))

(defn king-moves-vectors
  "Returns vector containing all possible moves on the empty board from possition xy. 
   If possiton xy is not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (flatten (reduce #(conj %1 (%2 x y)) [] [get-king-possible-up-row-moves get-king-possible-down-row-moves 
                                             get-king-possible-same-row-moves]))))

(defn get-king-possible-moves
  "Returns map {:eat {...} :move {...}} with all possible moves for the king located on xy field. Moves are separated in two maps: 
   1. :eat - Where king can eat opponent chessman
   2. :move - Where king can be placed (fields that are not occupied). 
   If xy is not valid possition or xy is not occupied by king, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \k)
    (reduce (fn [new-map possible-move]
              (let [key (get (map->vec possible-move) 0)
                    val (get (map->vec possible-move) 1)]
                (if (not-occupied? chessboard key val)
                  (add-to-move-eat-map new-map :move [possible-move])
                  (if (not-same-color? chessboard x y key val)
                    (add-to-move-eat-map new-map :eat [possible-move])
                    new-map))))
              init-move-eat-map
              (king-moves-vectors x y))))