(ns clojure-chess.army.pawn
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessman :refer :all]
            [clojure-chess.chessboard :refer :all]))

(defn- get-white-pawn-move-to-check
  "Returns one or two fields in front of xy field depends is white pawn on initial row or not. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (if (= y 1) 
      [{x (inc y)} {x (+ y 2)}]
      [{x (inc y)}])))

(defn- get-black-pawn-move-to-check
  "Returns one or two fields in front of xy field depends is black pawn on initial row or not. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (if (= y 6) 
      [{x (dec y)} {x (- y 2)}]
      [{x (dec y)}])))

(defn- get-pawn-possible-moves
  "Returns list of maps that represents fields where white pawn on xy can move. If pawn is on initial pawns row 
   it will be tested for first two forward moves, otherwise only first forward move. 
   If passed in values are not valid or field is not occupied with white pawn, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \p)
    (take-while #(let [key (get (map->vec %) 0)
                       val (get (map->vec %) 1)]
                   (not-occupied? chessboard key val)) (if (white? chessboard x y) 
                                                         (get-white-pawn-move-to-check x y)
                                                         (get-black-pawn-move-to-check x y)))))

(defn- get-pawn-possible-left-eat-move
   "Returns vector with one map that represent potential move or empty vector depends on state of the potential move field 
    (forward left for white/forward right for black). If potential move field is not occupied, empty vector will be returned. 
    If passed in values are not valid or field is not occupied with white pawn, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \p)
  (let [key (dec-x x)
        val (if (white? chessboard x y)
              (inc y)
              (dec y))]
               (if (occupied? chessboard key val)
                 [{key val}]
                 []))))

(defn- get-pawn-possible-right-eat-move
  "Returns vector with one map that represent potential move or empty vector depends on state of the potential move field 
    (forward right for white/forward left for black). If potential move field is not occupied, empty vector will be returned. 
    If passed in values are not valid or field is not occupied with white pawn, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \p)
  (let [key (inc-x x)
        val (if (white? chessboard x y)
              (inc y)
              (dec y))]
               (if (occupied? chessboard key val)
                 [{key val}]
                 []))))

(defn pawn-moves-vectors
  "Returns vector containing 4 vectors (for each direction) that represents all possible moves 
   on the empty board from possition xy. If possiton xy is not valid, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \p)
    (reduce #(conj %1 (%2 chessboard x y)) [] [get-pawn-possible-moves get-pawn-possible-left-eat-move get-pawn-possible-right-eat-move])))

(defn get-pown-possible-moves
  "Returns map {:eat {...} :move {...}} with all possible moves for the rook located on xy field. Moves are separated in two maps: 
   1. :eat - Where rook can eat opponent chessman
   2. :move - Where rook can be placed (fields that are not occupied). 
   If xy is not valid possition or xy is not occupied by rook, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \p)
    (get-chessman-possible-moves chessboard x y (pawn-moves-vectors chessboard x y))))
