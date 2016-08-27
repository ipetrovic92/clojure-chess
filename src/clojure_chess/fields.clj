(ns clojure-chess.fields
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessboard :refer :all]))

(def chessmans
  "Returns hash-set that represents all chessmans. "
  #{\p \r \n \b \q \k})

(def rook-a-0-not-moved (atom true))
(def rook-h-0-not-moved (atom true))
(def rook-a-7-not-moved (atom true))
(def rook-h-7-not-moved (atom true))
(def white-king-not-moved (atom true))
(def black-king-not-moved (atom true))

(def last-input
  "Represents last input of the player (e.g. [:a 1]). If input is empty, means that no chassman is selected. "
  (atom []))

(def last-move
  "Represents last move played in the game (e.g. [:a 1 :a 2]). "
  (atom []))

(def player-on-move (atom \w))

(defn change-player-turn
  "Change value of the atom that represents player on move. If atom value was w, it will be changed to b and vica verse. "
  []
  (if (compare-and-set! player-on-move \w \b)
    true
    (compare-and-set! player-on-move \b \w)))

(defn set-last-input
  "set last-input atom vlaue to vector created from passed in values. "
  [value]
  (reset! last-input value))

(defn set-last-move
  "Set last-move atom value to vector created from passed in values. "
  [from-x from-y to-x to-y]
  (reset! last-move [from-x from-y to-x to-y]))

(defn change-atom-value-from-false-to-true
  "Change atom value from false to true. If the value is already true, it will not be changed (operation fails). 
   If value is successfully changed true is returned, otherwise false. If passed in value is not instance of Atom, nil is returned. "
  [a]
  (if (instance? clojure.lang.Atom a)
    (compare-and-set! a false true)))

(defn change-atom-value-from-true-to-false
  "Change atom value from true to false. If the value is already false, it will not be changed (operation fails). 
   If value is successfully changed true is returned, otherwise false. If passed in value is not instance of Atom, nil is returned. "
  [a]
  (if (instance? clojure.lang.Atom a)
    (compare-and-set! a true false)))

(defn sort-chessboard-map-by-key
  [chessboard]
  (into (sorted-map) chessboard))

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


(defn get-color-full-name
  "Returns full name for the color. If passed in value is not valid color, nil is returned. "
  [color]
  (if (color? color)
    (if (= color \w)
      "White"
      "Black")))

(defn get-chessman-full-name
  "Returns full chessman name (e.g. white pawn, black rook, etc..). If passed in values are not valid xy values, nil is returned. "
  [chessboard x y]
  (if (valid-xy? x y)
    (let [chessman-short-name (get (get-chessman-short-name chessboard x y) 1)
          chessman-color (get-color-full-name (get-chessman-color-short-name chessboard x y))]
      (cond
        (= chessman-short-name \p) (str chessman-color " pawn")
        (= chessman-short-name \r) (str chessman-color " rook")
        (= chessman-short-name \n) (str chessman-color " knight")
        (= chessman-short-name \b) (str chessman-color " bishop")
        (= chessman-short-name \q) (str chessman-color " queen")
        (= chessman-short-name \k) (str chessman-color " king")
        :else "unknown cheesman"))
    ))

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

(defn valid-input? 
  "Returns true if passed in value is valid string for one field on the board (e.g. a1 b2 c0...), otherwise false. "
  [in]
  (and (= 2 (count in)) (valid-x? (keyword (str (get in 0)))) (is-digit? (get in 1)) (valid-y? (Integer/parseInt (str (get in 1))))))

(defn input->vec
  "Returns input (e.g. 'a0' 'h7' 'b3'...) as vector with key value pair (e.g. [:a 0], [:h 7], [:b 3]...). If passed in value is not valid input value, nil is returned. "
  [in]
  (if (valid-input? in)
    [(keyword (str (get in 0))) (Integer/parseInt (str (get in 1)))]))

(defn key->str
  "Returns key value as string suitable for presenting one column on the chessboard (e.g. a, b, c...). If passed in value is not valid x-key, nil is returned. "
  [x]
  (if (valid-x? x) 
   (str (get (str x) 1))))

(defn y->str
  "Returns y value as string suitable for presenting one row on the chessboard (e.g. 1, 2, 3, 4...). If passed in value is not valid y-key, nil is returned. "
  [y]
  (if (valid-y? y) 
   (str (inc y))))

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