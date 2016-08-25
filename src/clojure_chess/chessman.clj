(ns clojure-chess.chessman
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessboard :refer :all]))

(def chessmans
  "Returns hash-set that represents all chessmans. "
  #{\p \r \n \b \q \k})

(def init-move-eat-map
  "Defines map that should be populated with possible chessman move fields and fields where chessman can eat. "
  {:move []
   :eat []})

(defn map->vec
  "Returns vector containing sequence of map-key map-value. 
   If passed in parameter is not map, nil is returned. "
  [m]
  (if (valid-map? m)
    (vec (flatten (seq m)))))

(defn get-chessman-short-name
  "Returns short name of chessman at position xy. Returns '-' for empty field. "  
  [chessboard x y]
  (if (occupied? chessboard x y)
    (get (x chessboard) y)))

(defn get-chessman-full-name
  "Returns full name of chessman or :empty if field is not occupied. 
   If xy is not valid field on the board nil is returned. "
  [chessboard x y]
  (if (occupied? chessboard x y)
    (let [chessman-short-name (get (get-chessman-short-name chessboard x y) 1)]
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
  [chessboard x y]
  (if (occupied? chessboard x y)
    (get (get-chessman-short-name chessboard x y) 0)))

(defn get-chessman-color-full-name
  "Returns full name of chessman color. 
   If field xy is not occupied or xy is not valid field, nil is returned. "
  [chessboard x y]
  (if (occupied? chessboard x y)
    (let [chessman-color-short-name (get-chessman-color-short-name chessboard x y)]
      (if (= chessman-color-short-name \w)
        :white
        :black))))

(defn color? 
  "Returns true if character represents eny color (w - white, b - black), otherwise false. "
  [short-color-name]
  (or (= \b short-color-name)(= \w short-color-name)))

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

(defn valid-fields-map?
  "Returns true if map contains one or more x y value pairs only, otherwise false. "
  [m]
  (and (valid-map? m) (not-nil? (keys m)) (not-nil? (vals m))
       (every? #(valid-x? %) (keys m))
       (every? #(valid-y? %) (vals m))))

(defn field->vec
  "Returns vector containing a field-key field-value pair. 
   If field is not valid field (Map that contains only one x y pair), nil is returned. "
  [field]
  (if (and (valid-fields-map? field) (= 1 (count field)))
    (map->vec field)))

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

(defn field-valid-for-move?
  "Returns true if fields from-xfrom-y and to-xto-y are valid and if field from-xfrom-y is occupied. "
  [chessboard from-x from-y to-x to-y]
  (and (occupied? chessboard from-x from-y)
       (not-same-fields? from-x from-y to-x to-y)
       (or (not-occupied? chessboard to-x to-y)
           (not-same-color? chessboard from-x from-y to-x to-y))))

(defn set-chessman
  "Function set chessman on position xy. Method accepst color (e.g. b, w) and name (e.g. p, r) to represent chessman, 
   or chessman short name (e.g. wp, wr, wq...). Returns changed board on success, otherwise nil. "
  ([chessboard x y chessman]
    (if (and (valid-xy? x y) (is-chessman-short-name? chessman))
      (assoc chessboard x (assoc (x chessboard) y chessman))))
  ([chessboard x y chessman-short-color chessman-short-name]
    (if (and (valid-xy? x y) (chessman? chessman-short-name) (color? chessman-short-color))
      (set-chessman chessboard x y (str chessman-short-color chessman-short-name)))))

(defn remove-chessman
  "Function remove chessman from xy field (set field value to '-'). 
   Returns changed board on success, otherwise nil.  "
  [chessboard x y]
  (if (valid-xy? x y)
    (set-chessman chessboard x y "-")))

(defn make-move 
  "Function move chessman from field from-xfrom-y to to-xto-y and returns changed board on success, otherwise nil. "
  [chessboard from-x from-y to-x to-y]
  (if (field-valid-for-move? chessboard from-x from-y to-x to-y)
    (let [chessman-short-name (get-chessman-short-name chessboard from-x from-y)]
      (set-chessman (remove-chessman chessboard from-x from-y) to-x to-y chessman-short-name))))

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

(defn add-to-move-eat-map
  "Adds values (Vector of fields) to :move or :eat vector in m. If key is not :move nor :eat, nil is returned. "
  [m k values]
  (if 
    (not-nil? (k m))
    (assoc-in m [k] (vec (concat (k m) values)))))

(defn- get-move-fields-from-possible-moves
  "Returns first n fields from field-list that are not occupied on the board. "
  [chessboard field-list]
  (take-while #(let [key (get (map->vec %) 0)
                     val (get (map->vec %) 1)]
                  (not-occupied? chessboard key val)) field-list))

(defn merge-two-move-eat-maps
  "Returns move-eat map as result of merging two passed in move-eat-maps. If passed in maps doesn't have :move and :eat keys, nil is returned. "
  [m1 m2]
  (if (and (not-nil? (:move m1)) (not-nil? (:eat m1))
           (not-nil? (:move m2)) (not-nil? (:eat m2)))
    (add-to-move-eat-map (add-to-move-eat-map m1 :move (:move m2)) :eat (:eat m2))))

(defn- get-eat-fields-from-possible-moves
  "Returns first occupied field from field-list if chessman on that field 
  is NOT the same color as chessman on xy field on chessboard, otherwise empty vector.  "
  [chessboard x y field-list]
  (let [to-eat (first (drop-while #(let [key (get (map->vec %) 0)
                                         val (get (map->vec %) 1)]
                                      (not-occupied? chessboard key val)) field-list))
        to-eat-key (get (map->vec to-eat) 0)
        to-eat-val (get (map->vec to-eat) 1)]
    (if (not-same-color? chessboard x y to-eat-key to-eat-val)
      [to-eat]
      [])))

(defn get-chessman-possible-moves
  "Returns map {:eat {...} :move {...}} with all possible moves of the chessman located on xy field. Moves are separated in two maps: 
   1. :eat - Where rook can eat opponent chessman
   2. :move - Where rook can be placed (fields that are not occupied).
   Moves vector contains vectors for each direction that represents all possible moves 
   on the empty board from possition xy. If xy is not valid possition or xy is not occupied, nil is returned. "
  [chessboard x y possible-moves-vector]
  (if  (occupied? chessboard x y)
    (reduce (fn [new-map possible-moves]
              (let [move-fields (get-move-fields-from-possible-moves chessboard possible-moves)
                    eat-field (get-eat-fields-from-possible-moves chessboard x y possible-moves)]
                (add-to-move-eat-map (add-to-move-eat-map new-map :move move-fields) :eat eat-field)))
            init-move-eat-map
            possible-moves-vector)))
