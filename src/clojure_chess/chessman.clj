(ns clojure-chess.chessman
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessboard :refer :all]))

(def chessmans
  "Returns hash-set that represents all chessmans. "
  #{\p \r \n \b \q \k})


(defn get-chessman-short-name
  "Returns short name of chessman at position xy. Returns '-' for empty field. "  
  [chess-board x y]
  (if (valid-xy? x y)
    (get (get chess-board x) y)))

(defn get-chessman-full-name
  "Returns full name of chessman or :empty if field is not occupied. 
   If xy is not valid field on the board nil is returned. "
  [chess-board x y]
  (if (valid-xy? x y)
    (let [chessman-short-name (get (get-chessman-short-name chess-board x y) 1)]
      (cond
        (= chessman-short-name \p) :pawn
        (= chessman-short-name \r) :rook
        (= chessman-short-name \n) :knight
        (= chessman-short-name \b) :bishop
        (= chessman-short-name \q) :queen
        (= chessman-short-name \k) :king
        :else :empty))))

(defn get-chessman-color-short-name
  "Returns character that represents color of chessman. 
   If field xy is not occupied, nil is returned"
  [chess-board x y]
  (if (and (valid-xy? x y) (occupied? chess-board x y))
    (get (get-chessman-short-name chess-board x y) 0)))

(defn get-chessman-color-full-name
  "Returns full name of chessman color. 
   If field xy is not occupied or xy is not valid field, nil is returned. "
  [chess-board x y]
  (if (not-valid-xy? x y)
    nil
    (let [chessman-color-short-name (get-chessman-color-short-name chess-board x y)]
      (cond
        (= chessman-color-short-name \b) :black
        (= chessman-color-short-name \w) :white
        :else nil))))

(defn white?
  "Returns true if chassman on field xy is white piece, otherwise false. "
  [chessboard x y]
  (= \w (get-chessman-color-short-name chessboard x y)))

(defn black?
  "Returns true if chassman on field xy is black piece, otherwise false. "
  [chessboard x y]
  (= \b (get-chessman-color-short-name chessboard x y)))

(defn color? 
  "Returns true if character represents eny color (w - white, b - black), otherwise false. "
  [short-color-name]
  (or (= \b short-color-name)(= \w short-color-name)))

(defn chessman? 
  "Returns true if character represents eny chessman (p-pawn, r-rook, n-knight, b-bishop, q-queen, k-king), otherwise false. "
  [short-chessman-name]
  (not-nil? (get chessmans short-chessman-name)))

(defn same-color?
  "Returns true if chassman on field x1y1 is the same color as piece on x2y2, otherwise false. "
  [chessboard x1 y1 x2 y2]
  (let [color-1 (get-chessman-color-short-name chessboard x1 y1)
        color-2 (get-chessman-color-short-name chessboard x2 y2)]
    (= color-1 color-2)))

(defn not-same-color?
  "Returns true if chassman on field x1y1 is NOT the same color as piece on x2y2, otherwise false. "
  [chessboard x1 y1 x2 y2]
  (not (same-color? chessboard x1 y1 x2 y2)))

(defn set-chessman
  "Function set chessman on position xy. 
   Method accepst color (e.g. b, w) and name (e.g. p, r) to represent chessman, or chessman short name (e.g. wp, wr, wq...). 
   Returns changed board on success, otherwise nil. "
  ([chessboard x y chessman]
    (if (valid-xy? x y)
  (assoc chessboard x (assoc (x chessboard) y chessman))))
  ([chessboard x y chessman-short-color chessman-short-name]
    (set-chessman chessboard x y (str chessman-short-color chessman-short-name))))

(defn remove-chessman
  "Function remove chessman from xy field (set field value to '-'). 
   Returns changed board on success, otherwise nil.  "
  [chessboard x y]
  (if (and (valid-xy? x y) (occupied? chessboard x y))
    (set-chessman chessboard x y "-")))

(defn make-move 
  "Function move chessman from field from-xfrom-y to to-xto-y and returns changed board on success, otherwise nil. "
  [chessboard from-x from-y to-x to-y]
  (if (field-valid-for-move? chessboard from-x from-y to-x to-y)
    (let [chessman-short-name (get-chessman-short-name chessboard from-x from-y)]
        (set-chessman (remove-chessman chessboard from-x from-y) to-x to-y chessman-short-name))))
    
(defn is-chessman-type?
  "Returns true if chessman on position xy is of the same type as chessman-type (p, r, q, k...), otherwise false. "
  [chessboard x y chessman-type]
  (and (chessman? chessman-type)
       (= chessman-type (get (get-chessman-short-name chessboard x y) 1))))