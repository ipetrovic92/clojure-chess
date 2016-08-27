(ns clojure-chess.fields
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessboard :refer :all]))

(def chessmans
  "Returns hash-set that represents all chessmans. "
  #{\p \r \n \b \q \k})

(defn not-occupied? 
  "Returns true if field xy in NOT occupied, otherwise false. "
  [chess-board x y]
  (let [val (get (get chess-board x) y)]
    (and (valid-xy? x y) (= (count val) 1)(= empty-field-val (get val 0)))))

(defn occupied? 
  "Returns true if field xy in occupied, otherwise false. "
  [chess-board x y]
  (and (valid-xy? x y) (not (not-occupied? chess-board x y))))

(defn get-chessman-short-name
  "Returns short name of chessman at position xy. Returns '-' for empty field. "  
  [chessboard x y]
  (if (occupied? chessboard x y)
    (get (x chessboard) y)))

(defn get-chessman-color-short-name
  "Returns character that represents color of chessman. 
   If field xy is not occupied, nil is returned"
  [chessboard x y]
  (if (occupied? chessboard x y)
    (get (get-chessman-short-name chessboard x y) 0)))

(defn color? 
  "Returns true if character represents eny color (w - white, b - black), otherwise false. "
  [short-color-name]
  (or (= \b short-color-name)(= \w short-color-name)))

(defn get-opposite-color
  "Returns color opposite of passed in color (eg. w is passed in, b is returned). If passed in value is not valid color, nil is returned. "  
  [color]
  (if (color? color)
    (if (= color \w)
      \b
      \w)))

(defn chessman? 
  "Returns true if character represents eny chessman (p-pawn, r-rook, n-knight, b-bishop, q-queen, k-king), otherwise false. "
  [short-chessman-name]
  (not-nil? (get chessmans short-chessman-name)))

(defn is-chessman-short-name? 
  "Returns true if passed in value is valid chessmen name (e.g. -, wr, bp, br, wq...), otherwise false. "
  [chessman-short-name]
  (or (and (= 1 (count chessman-short-name)) (= \- (get chessman-short-name 0)))
      (and (= 2 (count chessman-short-name)) (color? (get chessman-short-name 0)) (chessman? (get chessman-short-name 1)))))

(defn white?
  "Returns true if chessman on field xy is white piece, otherwise false. "
  [chessboard x y]
  (and (occupied? chessboard x y)(= \w (get-chessman-color-short-name chessboard x y))))

(defn black?
  "Returns true if chessman on field xy is black piece, otherwise false. "
  [chessboard x y]
  (and (occupied? chessboard x y) (= \b (get-chessman-color-short-name chessboard x y))))

(defn same-color?
  "Returns true if chessman on field x1y1 is the same color as piece on x2y2, otherwise false. "
  [chessboard x1 y1 x2 y2]
  (let [color-1 (get-chessman-color-short-name chessboard x1 y1)
        color-2 (get-chessman-color-short-name chessboard x2 y2)]
    (and (occupied? chessboard x1 y1) (occupied? chessboard x2 y2) (= color-1 color-2))))

(defn not-same-color?
  "Returns true if chessman on field x1y1 is NOT the same color as piece on x2y2, otherwise false. "
  [chessboard x1 y1 x2 y2]
  (and (occupied? chessboard x1 y1) (occupied? chessboard x2 y2) (not (same-color? chessboard x1 y1 x2 y2))))

(defn valid-fields-map?
  "Returns true if map contains one or more x y value pairs only, otherwise false. "
  [m]
  (and (valid-map? m) (not-nil? (keys m)) (not-nil? (vals m))
       (every? #(valid-x? %) (keys m))
       (every? #(valid-y? %) (vals m))))

(defn field-valid-for-move?
  "Returns true if fields from-xfrom-y and to-xto-y are valid and if field from-xfrom-y is occupied. "
  [chessboard from-x from-y to-x to-y]
  (and (occupied? chessboard from-x from-y)
       (not-same-fields? from-x from-y to-x to-y)
       (or (not-occupied? chessboard to-x to-y)
           (not-same-color? chessboard from-x from-y to-x to-y))))

(defn chessman-type?
  "Returns true if chessman on position xy is some of the chessman types (p, r, q, k...), otherwise false. "
  [chessboard x y chessman-type]
  (and (occupied? chessboard x y)
       (chessman? chessman-type)
       (= chessman-type (get (get-chessman-short-name chessboard x y) 1))))

(defn not-chessman-type?
  "Returns true if chessman on position xy is some of the chessman types (p, r, q, k...), otherwise false. "
  [chessboard x y chessman-type]
  (and (occupied? chessboard x y)
       (chessman? chessman-type)
       (not (chessman-type? chessboard x y chessman-type))))