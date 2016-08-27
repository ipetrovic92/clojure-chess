(ns clojure-chess.move
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessboard :refer :all]
            [clojure-chess.fields :refer :all]
            [clojure-chess.army.pawn :refer [pawn-moves-vectors]]
            [clojure-chess.army.bishop :refer [bishop-moves-vectors]]
            [clojure-chess.army.knight :refer [knight-moves-vectors]]
            [clojure-chess.army.rook :refer [rook-moves-vectors]]))

(def init-move-captcure-map
  "Defines map that should be populated with possible chessman move fields and fields where chessman can captcure opponent chessman. "
  {:move []
   :captcure []})

(defn move-capture-map->vec
  "Returns all fields from :move and :capture vectors as one vector. "
  [move-capture-map]
  (reduce #(flatten (into %1 %2)) [] (vals move-capture-map)))

(defn set-chessman-to-xy-field
  "Function set chessman on position xy. Method accepst color (e.g. b, w) and name (e.g. p, r) to represent chessman, 
   or chessman short name (e.g. wp, wr, wq...). Returns changed board on success, otherwise nil. "
  ([chessboard x y chessman]
    (if (and (valid-xy? x y) (is-chessman-short-name? chessman))
      (assoc chessboard x (assoc (x chessboard) y chessman))))
  ([chessboard x y chessman-short-color chessman-short-name]
    (if (and (valid-xy? x y) (chessman? chessman-short-name) (color? chessman-short-color))
      (set-chessman-to-xy-field chessboard x y (str chessman-short-color chessman-short-name)))))

(defn remove-chessman
  "Function remove chessman from xy field (set field value to '-'). 
   Returns changed board on success, otherwise nil.  "
  [chessboard x y]
  (if (valid-xy? x y)
    (set-chessman-to-xy-field chessboard x y "-")))

(defn make-move 
  "Function move chessman from field from-xfrom-y to to-xto-y and returns changed board on success, otherwise nil. "
  [chessboard from-x from-y to-x to-y]
  (if (field-valid-for-move? chessboard from-x from-y to-x to-y)
    (let [chessman-short-name (get-chessman-short-name chessboard from-x from-y)]
      (set-chessman-to-xy-field (remove-chessman chessboard from-x from-y) to-x to-y chessman-short-name))))

(defn get-king-location
  "Returns possition of king-color king on the chessboard as map (e.g. {:d 0}). If color is not b or w or king is not on the chessboard, nil is returned. "
  [chessboard king-color]
  (if (color? king-color)
    (loop [init-key :a]
      (if (valid-x? init-key)
        (let [king-short-name (str king-color \k)
              y-values (init-key chessboard)]
          (if (some #(= % king-short-name) y-values)
            (hash-map init-key (.indexOf y-values king-short-name))
            (recur (inc-x init-key))))))))

(defn chessman-legal-move? 
  "Returns true if chessman-move-capture-map contains field (in :move or :capture vector), otherwise false. "
  [chessman-move-capture-map field]
  (not-nil? (some #(= % field) (move-capture-map->vec chessman-move-capture-map))))

(defn add-to-move-captcure-map
  "Adds values (Vector of fields) to :move or :captcure vector in m. If key is not :move nor :captcure, nil is returned. "
  [m k values]
  (if 
    (not-nil? (k m))
    (assoc-in m [k] (vec (concat (k m) values)))))

(defn merge-two-move-captcure-maps
  "Returns move-captcure map as result of merging two passed in move-captcure-maps. If passed in maps doesn't have :move and :captcure keys, nil is returned. "
  [m1 m2]
  (if (and (not-nil? (:move m1)) (not-nil? (:captcure m1))
           (not-nil? (:move m2)) (not-nil? (:captcure m2)))
    (add-to-move-captcure-map (add-to-move-captcure-map m1 :move (:move m2)) :captcure (:captcure m2))))

(defn get-move-fields-from-possible-moves-list
  "Returns first n fields from field-list that are not occupied on the board. "
  [chessboard field-list]
  (take-while #(let [key (get (map->vec %) 0)
                     val (get (map->vec %) 1)]
                 (not-occupied? chessboard key val)) field-list))

(defn get-captcure-fields-from-possible-moves-list
  "Returns first occupied field from field-list if chessman on that field 
  is NOT the same color as chessman on xy field on chessboard, otherwise empty vector.  "
  [chessboard x y field-list]
  (let [to-captcure (first (drop-while #(let [key (get (map->vec %) 0)
                                         val (get (map->vec %) 1)]
                                     (not-occupied? chessboard key val)) field-list))
        to-captcure-key (get (map->vec to-captcure) 0)
        to-captcure-val (get (map->vec to-captcure) 1)]
    (if (not-same-color? chessboard x y to-captcure-key to-captcure-val)
      [to-captcure]
      [])))

(defn get-chessman-possible-moves-from-moves-vectors
  "Returns map {:captcure {...} :move {...}} with all possible moves of the chessman located on xy field. Moves are separated in two maps: 
   1. :captcure - Where chessman can captcure opponent chessman
   2. :move - Where chessman can be placed (fields that are not occupied).
   Moves vector contains vectors for each direction that represents all possible moves 
   on the empty board from possition xy. If xy is not valid possition or xy is not occupied, nil is returned. "
  [chessboard x y possible-moves-vectors]
  (if  (occupied? chessboard x y)
    (reduce (fn [new-map possible-moves]
              (let [move-fields (get-move-fields-from-possible-moves-list chessboard possible-moves)
                    captcure-field (get-captcure-fields-from-possible-moves-list chessboard x y possible-moves)]
                (add-to-move-captcure-map (add-to-move-captcure-map new-map :move move-fields) :captcure captcure-field)))
            init-move-captcure-map
            possible-moves-vectors)))

(defn get-pown-possible-moves
  "Returns map {:captcure {...} :move {...}} with all possible moves for the rook located on xy field. Moves are separated in two maps: 
   1. :captcure - Where rook can captcure opponent chessman
   2. :move - Where rook can be placed (fields that are not occupied). 
   If xy is not valid possition or xy is not occupied by rook, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \p)
    (get-chessman-possible-moves-from-moves-vectors chessboard x y (pawn-moves-vectors chessboard x y))))

(defn get-rook-possible-moves
  "Returns map {:captcure {...} :move {...}} with all possible moves for the rook located on xy field. Moves are separated in two maps: 
   1. :captcure - Where rook can captcure opponent chessman
   2. :move - Where rook can be placed (fields that are not occupied). 
   If xy is not valid possition or xy is not occupied by rook, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \r)
    (get-chessman-possible-moves-from-moves-vectors chessboard x y (rook-moves-vectors x y))))

(defn get-knight-possible-moves
  "Returns map {:captcure {...} :move {...}} with all possible moves for the knight located on xy field. Moves are separated in two maps: 
   1. :captcure - Where knight can captcure opponent chessman
   2. :move - Where knight can be placed (fields that are not occupied). 
   If xy is not valid possition or xy is not occupied by knight, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \n)
    (reduce (fn [new-map possible-move]
              (let [key (get (map->vec possible-move) 0)
                    val (get (map->vec possible-move) 1)]
                (if (not-occupied? chessboard key val)
                  (add-to-move-captcure-map new-map :move [possible-move])
                  (if (not-same-color? chessboard x y key val)
                    (add-to-move-captcure-map new-map :captcure [possible-move])
                    new-map))))
              init-move-captcure-map
              (knight-moves-vectors x y))))

(defn get-bishop-possible-moves
  "Returns map {:captcure {...} :move {...}} with all possible moves for the bishop located on xy field. Moves are separated in two maps: 
   1. :captcure - Where bishop can captcure opponent chessman
   2. :move - Where bishop can be placed (fields that are not occupied). 
   If xy is not valid possition or xy is not occupied by bishop, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \b)
    (get-chessman-possible-moves-from-moves-vectors chessboard x y (bishop-moves-vectors x y))))

(defn get-queen-possible-moves
  "Returns map {:captcure {...} :move {...}} with all possible moves for the queen located on xy field. Moves are separated in two maps: 
   1. :captcure - Where rook can captcure opponent chessman
   2. :move - Where rook can be placed (fields that are not occupied). 
   If xy is not valid possition or xy is not occupied by queen, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \q)
    (let [as-rook-moves-map (get-chessman-possible-moves-from-moves-vectors chessboard x y (rook-moves-vectors x y))
          as-bishop-moves-map (get-chessman-possible-moves-from-moves-vectors chessboard x y (bishop-moves-vectors x y))]
      (merge-two-move-captcure-maps as-rook-moves-map as-bishop-moves-map))))

(defn get-chessman-at-xy-possible-moves-excluded-king
  "Returns map {:captcure {...} :move {...}} with all possible moves of the chessman located on xy field. Moves are separated in two maps: 
   1. :captcure - Where chessman can captcure opponent chessman
   2. :move - Where chessman can be placed (fields that are not occupied).
   If xy is not valid possition or xy is not occupied, initial move-captcure map is returned. "
  [chessboard x y]
  (if (occupied? chessboard x y)
    (let [chessman-short-name (get (get-chessman-short-name chessboard x y) 1)]
      (cond
        (= chessman-short-name \p) (get-pown-possible-moves chessboard x y)
        (= chessman-short-name \r) (get-rook-possible-moves chessboard x y)
        (= chessman-short-name \n) (get-knight-possible-moves chessboard x y)
        (= chessman-short-name \b) (get-bishop-possible-moves chessboard x y)
        (= chessman-short-name \q) (get-queen-possible-moves chessboard x y)
        :else init-move-captcure-map))
    init-move-captcure-map))


(defn get-player-checked-field-king-excluded
  "Returns map {:captcure {...} :move {...}} with all possible moves of all chessmans EXCEPT KING moves (method is used to determine is check or not, and king cannot check opponent king)
   on the board with chessman-color color. Moves are separated in two maps: 
   1. :captcure - Where chessman can captcure opponent chessman
   2. :move - Where chessman can be placed (fields that are not occupied).
   If chessman-color in not valid color (w or b), nil is returned. "
  [chessboard chessman-color]
  (if (color? chessman-color)
    (loop [init-result-map init-move-captcure-map
           init-key :a]
      (if (valid-x? init-key)
        (let [y-values (init-key chessboard)]
         (recur (loop [init-result-map-2 init-result-map
                        init-val-pos 0]
                   (if (valid-y? init-val-pos)
                       (if (and (= chessman-color (get-chessman-color-short-name chessboard init-key init-val-pos)) 
                                (not= \k (get (get-chessman-short-name chessboard init-key init-val-pos) 1)))
                         (recur (merge-two-move-captcure-maps init-result-map-2 (get-chessman-at-xy-possible-moves-excluded-king chessboard init-key init-val-pos)) (inc init-val-pos))
                         (recur init-result-map-2 (inc init-val-pos)))
                     init-result-map-2)) (inc-x init-key)))
        init-result-map))))

(defn is-check? 
  "Returns true if king-color king is under check, otherwise false. If passed in value is not valid color (b or w), nil is returned. "
  [chessboard king-color]
  (if (color? king-color)
     (let [king-location-field (get-king-location chessboard king-color)
           opposite-player-moves (get-player-checked-field-king-excluded chessboard (get-opposite-color king-color))]
       (not-nil? (some #(= % king-location-field) (:captcure opposite-player-moves))))))

(defn is-not-check? 
  "Returns true if king-color king is NOT under check, otherwise false. If passed in value is not valid color (b or w), nil is returned. "
  [chessboard king-color]
  (and (color? king-color) (not (is-check? chessboard king-color))))