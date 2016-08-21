(ns clojure-chess.chessman
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessboard :refer :all]))



(defn- get-chessman-short-name
  "Returns short name of chessman at position xy. Returns '-' for empty field. "  
  [chess-board x y]
  (if (valid-xy? x y)
    (get (get chess-board x) y)))

(defn get-chessman-full-name
  "Returns full name of chessman or :empty if field is not occupied. "
  [chess-board x y]
  (let [chessman-short-name (get (get-chessman-short-name chess-board x y) 1)]
    (cond
      (= chessman-short-name \p) :pawn
      (= chessman-short-name \r) :rook
      (= chessman-short-name \n) :knight
      (= chessman-short-name \b) :bishop
      (= chessman-short-name \q) :queen
      (= chessman-short-name \k) :king
      (nil? chessman-short-name) nil
      :else :empty)))

(defn get-chessman-color-short-name
  "Returns character that represents color of chessman. 
   If field xy is not occupied, nil is returned"
  [chess-board x y]
  (if (and (valid-xy? x y) (occupied? chess-board x y))
    (get (get-chessman-short-name chess-board x y) 0)))