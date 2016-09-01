(ns clojure-chess.army.pawn
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessboard :refer :all]
            [clojure-chess.fields :refer :all]))

(defn- get-white-pawn-move-to-check
  "Returns vector of one or two fields in front of xy field depends is white pawn on initial row or not. 
   If passed in values are not valid, nil is returned. "
  [x y]
  (if (valid-xy? x y)
    (if (= y 1) 
      [{x (inc y)} {x (+ y 2)}]
      [{x (inc y)}])))

(defn- get-black-pawn-move-to-check
  "Returns vector of one or two fields in front of xy field depends is black pawn on initial row or not. 
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

(defn- get-pawn-possible-left-captcure-move
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

(defn- get-pawn-possible-right-captcure-move
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

(defn- get-white-en-passant-move
  "Returns possible En Passant move for white player (En Passant - If a pawn moves out two squares on its first move, 
   and by doing so lands to the side of an opponent's pawn, that other pawn has the option of capturing the first pawn
   as it passes by. This special move must be done immediately after the first pawn has moved). If En Passant is not possible from 
   field xy, empty vector is returned. "
  [chessboard x y]
  (if (and (valid-xy? x y) (chessman-type? chessboard x y \p) (= y 4) 
           (= \w (get-chessman-color-short-name chessboard x y)) (not-empty? @last-move))
    (let [last-move-vector @last-move
          last-move-from-x (get last-move-vector 0)
          last-move-from-y (get last-move-vector 1)
          last-move-to-x (get last-move-vector 2)
          last-move-to-y (get last-move-vector 3)] 
      (if (and (chessman-type? chessboard last-move-to-x last-move-to-y \p) 
               (= last-move-from-y 6) (= last-move-to-y 4))
        (if (and (valid-x? (dec-x x)) (= last-move-to-x (dec-x x)))
          [{(dec-x x) (inc y)}]
          (if (and (valid-x? (inc-x x)) (= last-move-to-x (inc-x x)))
            [{(inc-x x) (inc y)}]
            []))
        []))
    []))

(defn- get-black-en-passant-move
  "Returns possible En Passant move for black player (En Passant - If a pawn moves out two squares on its first move, 
   and by doing so lands to the side of an opponent's pawn, that other pawn has the option of capturing the first pawn
   as it passes by. This special move must be done immediately after the first pawn has moved). If En Passant is not possible from 
   field xy, empty vector is returned. "
  [chessboard x y]
  (if (and (valid-xy? x y) (chessman-type? chessboard x y \p) (= y 3) 
           (= \b (get-chessman-color-short-name chessboard x y)) (not-empty? @last-move))
    (let [last-move-from-x (get @last-move 0)
          last-move-from-y (get @last-move 1)
          last-move-to-x (get @last-move 2)
          last-move-to-y (get @last-move 3)] 
      (if (and (chessman-type? chessboard last-move-to-x last-move-to-y \p) 
               (= last-move-from-y 1) (= last-move-to-y 3))
        (if (and (valid-x? (dec-x x)) (= last-move-to-x (dec-x x)))
          [{(dec-x x) (dec y)}]
          (if (and (valid-x? (inc-x x)) (= last-move-to-x (inc-x x)))
            [{(inc-x x) (dec y)}]
            []))
        []))
    []))

(defn pawn-moves-vectors
  "Returns vector containing 4 vectors that represents all possible moves 
   on the board from possition xy. If possiton xy is not valid, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \p)
    (reduce #(conj %1 (%2 chessboard x y)) [] [get-pawn-possible-moves get-pawn-possible-left-captcure-move get-pawn-possible-right-captcure-move
                                               (if (white? chessboard x y)
                                                 get-white-en-passant-move
                                                 get-black-en-passant-move)])))
