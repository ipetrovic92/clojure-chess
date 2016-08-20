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

(def x-value 
  [:a :b :c :d :e :f :g :h])

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
  "Retruns true if parameter is valid x position, otherwise false. "
  [x]
  (contains? x-value x))

(defn valid-y? 
  "Retruns true if parameter is valid y position, otherwise false. "
  [y]
  (<= 0 y 7))

(defn valid-xy?
  "Returns true if position xy represent field on the board, othervise false. "
  [x y]
  (and (valid-x? x)
       (valid-y? y)))

(defn- not-occupied? 
  "Returns true if field xy in NOT occupied, otherwise false. "
  [chess-board x y]
  (= \- (get (get-chessman-short-name chess-board x y) 0)))

(defn occupied? 
  "Returns true if field xy in occupied, otherwise false. "
  [chess-board x y]
  (not (not-occupied? chess-board x y)))

(defn x-as-num 
  "Returns x as number (e.g. :a = 0, :b = 1, :c = 2, etc...). "
  [x-key]
  (x-key (zipmap x-value (range 8))))

(defn x-as-key
  "Returns x as key, based on number (e.g. 0 = :a, 1 = :b, 2 = :c, etc...). "
  [x-num]
  (get x-value x-num))
