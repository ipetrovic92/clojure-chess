(ns clojure-chess.army.king
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessboard :refer :all]
            [clojure-chess.fields :refer :all]
            [clojure-chess.army.rook :refer [get-rook-possible-right-moves get-rook-possible-left-moves]]
            [clojure-chess.move :refer [is-not-check? make-move get-move-fields-from-possible-moves-list]]))

(defn- get-king-possible-up-row-moves
  "Returns sequence that represents valid fields where king can potentially move from position xy 
   (up-left, up or up-right field from xy field). If passed in values are not valid, nil is returned. "
  [chessboard x y]
  (if (valid-xy? x y) 
    (if (valid-y? (inc y))
      (let [potential-up-row-moves (filter valid-x? [(dec-x x) x (inc-x x)])]
        (map #(hash-map % (inc y)) potential-up-row-moves))
      [])))

(defn- get-king-possible-down-row-moves
  "Returns sequence that represents valid fields where king can potentially move from position xy 
   (down-left, down or down-right field from xy field). If passed in values are not valid, nil is returned. "
  [chessboard x y]
  (if (valid-xy? x y) 
    (if (valid-y? (dec y))
      (let [potential-down-row-moves (filter valid-x? [(dec-x x) x (inc-x x)])]
        (map #(hash-map % (dec y)) potential-down-row-moves))
      [])))

(defn- get-king-possible-same-row-moves
  "Returns sequence that represents valid fields where king can potentially move from position xy 
   (left or right field from xy field). If passed in values are not valid, nil is returned. "
  [chessboard x y]
  (if (valid-xy? x y) 
    (let [potential-same-row-moves (filter valid-x? [(dec-x x) (inc-x x)])]
      (map #(hash-map % y) potential-same-row-moves))))

(defn- get-white-king-castling-kingside-move
  "Returns vector with field {:g 0} if white player is able to do kingside castling, otherwise empty vector. "
  [chessboard x y]
  (if (and (= "wk" (get-chessman-short-name chessboard x y))
           @white-king-not-moved @rook-h-0-not-moved
           (= [{:g 0} {:f 0}] (get-move-fields-from-possible-moves-list chessboard (get-rook-possible-left-moves :h 0)))
           (is-not-check? chessboard \w) ;check if white king is not under attack of black player
           (is-not-check? (make-move chessboard x y :f 0) \w) ;check if field {:f 0} is not under attack of black player
           (is-not-check? (make-move chessboard x y :g 0) \w)) ;check if field {:g 0} is not under attack of black player
    [{:g 0}]
    []))

(defn- get-white-king-castling-queenside-move
  "Returns vector with field {:c 0} if white player is able to do queenside castling, otherwise empty vector. "
  [chessboard x y]
  (if (and (= "wk" (get-chessman-short-name chessboard x y))
           @white-king-not-moved @rook-a-0-not-moved
           (= [{:b 0} {:c 0} {:d 0}] (get-move-fields-from-possible-moves-list chessboard (get-rook-possible-right-moves :a 0)))
           (is-not-check? chessboard \w) ;check if white king is not under attack of black player
           (is-not-check? (make-move chessboard x y :d 0) \w) ;check if field {:d 0} is not under attack of black player
           (is-not-check? (make-move chessboard x y :c 0) \w)) ;check if field {:c 0} is not under attack of black player
    [{:c 0}]
    []))

(defn- get-black-king-castling-kingside-move
  "Returns vector with field {:g 7} if black player is able to do kingside castling, otherwise empty vector. "
  [chessboard x y]
  (if (and (= "bk" (get-chessman-short-name chessboard x y))
           @black-king-not-moved @rook-h-7-not-moved
           (= [{:g 7} {:f 7}] (get-move-fields-from-possible-moves-list chessboard (get-rook-possible-left-moves :h 7)))
           (is-not-check? chessboard \b) ;check if white king is not under attack of black player
           (is-not-check? (make-move chessboard x y :f 7) \b) ;check if field {:f 7} is not under attack of white player
           (is-not-check? (make-move chessboard x y :g 7) \b)) ;check if field {:g 7} is not under attack of white player
    [{:g 7}]
    []))

(defn- get-black-king-castling-queenside-move
  "Returns vector with field {:c 7} if black player is able to do queenside castling, otherwise empty vector. "
  [chessboard x y]
  (if (and (= "bk" (get-chessman-short-name chessboard x y))
           @black-king-not-moved @rook-a-7-not-moved
           (= [{:b 7} {:c 7} {:d 7}] (get-move-fields-from-possible-moves-list chessboard (get-rook-possible-right-moves :a 7)))
           (is-not-check? chessboard \b) ;check if white king is not under attack of black player
           (is-not-check? (make-move chessboard x y :d 7) \b) ;check if field {:d 7} is not under attack of white player
           (is-not-check? (make-move chessboard x y :c 7) \b)) ;check if field {:c 7} is not under attack of white player 
    [{:c 7}]
    []))

(defn king-moves-vectors
  "Returns vector containing all possible moves on the empty board from possition xy. 
   If possiton xy is not valid, nil is returned. "
  [chessboard x y]
  (if (valid-xy? x y)
    (flatten (reduce #(conj %1 (%2 chessboard x y)) [] (flatten [get-king-possible-up-row-moves get-king-possible-down-row-moves 
                                                                 get-king-possible-same-row-moves
                                                                 (if (white? chessboard x y)
                                                                   [get-white-king-castling-kingside-move get-white-king-castling-queenside-move]
                                                                   [get-black-king-castling-kingside-move get-black-king-castling-queenside-move])])))))
