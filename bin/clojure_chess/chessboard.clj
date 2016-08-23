(ns clojure-chess.chessboard
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]))

(def init-board
  "Defines initial chess board. Every chessman is represented with two letters. 
   First letter represents color (w-white, b-black). 
   Second leter represent chessman type (p-pawn, r-rook, n-knight, b-bishop, q-queen, k-king). 
   Empty field is represented with '-'. "
  {:a ["wr" "wp" "-" "-" "-" "-" "bp" "br"]
   :b ["wn" "wp" "-" "-" "-" "-" "bp" "bn"]
   :c ["wb" "wp" "-" "-" "-" "-" "bp" "bb"]
   :d ["wq" "wp" "-" "-" "-" "-" "bp" "bq"]
   :e ["wk" "wp" "-" "-" "-" "-" "bp" "bk"]
   :f ["wb" "wp" "-" "-" "-" "-" "bp" "bb"]
   :g ["wn" "wp" "-" "-" "-" "-" "bp" "bn"]
   :h ["wr" "wp" "-" "-" "-" "-" "bp" "br"]
   })

(def empty-field-val
  "Constant that represents empty field value. "
  \-)

(defn map->vec
  "Returns vector containings sequence of map-key map-value. 
   If passed in parameter is not map, nil is returned. "
  [m]
  (if(instance? clojure.lang.PersistentArrayMap m)
  (vec (flatten (seq m)))))

(def x-value 
  "Vector that represents valid values for x. 
  Vector is used instead of hash-map because order of the values matters. "
  [:a :b :c :d :e :f :g :h])

(def not-nil? 
  "Returns true if value is not nil, otherwise false. "
  (complement nil?))

(defn- is-digit? 
  "Returns true if passed in value is digit, otherwise false. "
  [ch]
  (and (instance? Character ch)
       (Character/isDigit ch)))

(defn- is-str-num? 
  "Returns ture if passed in string is number, otherwise false. "
  [s]
  (and (not (= (count s) 0)) 
       (= (count s)(count (apply str (filter #(is-digit? %) s))))))

(defn valid-x?
  "Retruns true if parameter is valid x position, otherwise nil. "
  [x]
  (not-nil? (some #(= % x) x-value)))

(defn valid-y? 
  "Retruns true if parameter is valid y position, otherwise false. "
  [y]
  (and (instance? Long y) (<= 0 y 7)))

(defn valid-xy?
  "Returns true if position xy represent field on the board, othervise false. "
  [x y]
  (and (valid-x? x)
       (valid-y? y)))

(defn not-valid-xy?
  "Returns true if position xy is NOT VALID field on the board, othervise false. "
  [x y]
  ((complement valid-xy?) x y))

(defn same-field? 
  "Returns true if position x1y1 is equal x2y2, otherwise false. "
  [x1 y1 x2 y2]
  (and (= x1 x2) (= y1 y2)))

(defn not-same-fields? 
  "Returns true if position x1y1 is NOT equal x2y2, otherwise false. "
  [x1 y1 x2 y2]
  ((complement same-field?) x1 y1 x2 y2))


(defn not-occupied? 
  "Returns true if field xy in NOT occupied, otherwise false. "
  [chess-board x y]
  (let [field-value (get (get chess-board x) y)]
    (and (= (count field-value) 1)(= empty-field-val (get field-value 0)))))

(defn occupied? 
  "Returns true if field xy in occupied, otherwise false. "
  [chess-board x y]
  (not (not-occupied? chess-board x y)))

(defn field-valid-for-move?
  "Returns true if fields from-xfrom-y and to-xto-y are valid and if field from-xfrom-y is occupied. "
  [chessboard from-x from-y to-x to-y]
  (and (valid-xy? from-x from-y)
       (valid-xy? to-x to-y)
       (not-same-fields? from-x from-y to-x to-y)
       (occupied? chessboard from-x from-y)))

(defn x-as-num 
  "Returns x as number, based on key (e.g. :a = 0, :b = 1, :c = 2, etc...). "
  [x-key]
  (x-key (zipmap x-value (range 8))))

(defn x-as-key
  "Returns x as key, based on number (e.g. 0 = :a, 1 = :b, 2 = :c, etc...). "
  [x-num]
  (get x-value x-num))

(defn inc-x
  "Returns x key incremented by one if posible, otherwise nil. "
  [x-key]
  (if (valid-x? x-key)
  (x-as-key (inc (x-as-num x-key)))))

(defn dec-x
  "Returns x key decremented by one if posible, otherwise nil. "
  [x-key]
  (if (valid-x? x-key)
  (x-as-key (dec (x-as-num x-key)))))
