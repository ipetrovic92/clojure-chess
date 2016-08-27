(ns clojure-chess.game
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessboard :refer :all]
            [clojure-chess.fields :refer :all]
            [clojure-chess.move :refer :all]
            [clojure-chess.army.king :refer [king-moves-vectors]]))

(defn get-king-possible-moves
  "Returns map {:captcure {...} :move {...}} with all possible moves for the king located on xy field. Moves are separated in two maps: 
   1. :captcure - Where king can captcure opponent chessman
   2. :move - Where king can be placed (fields that are not occupied). 
   If xy is not valid possition or xy is not occupied by king, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \k)
    (reduce (fn [new-map possible-move]
              (let [key (get (map->vec possible-move) 0)
                    val (get (map->vec possible-move) 1)
                    color (get-chessman-color-short-name chessboard x y)]
                (if (and (not-occupied? chessboard key val)
                         (is-not-check? (make-move chessboard x y key val) color))
                  (add-to-move-captcure-map new-map :move [possible-move])
                  (if (and (not-same-color? chessboard x y key val)
                           (is-not-check? (make-move chessboard x y key val) color))
                    (add-to-move-captcure-map new-map :captcure [possible-move])
                    new-map))))
            init-move-captcure-map
            (king-moves-vectors chessboard x y))))

(defn get-king-possible-moves-ignore-check
  "Returns map {:captcure {...} :move {...}} with all possible moves for the king located on xy field including also moves that will lead to check. Moves are separated in two maps: 
   1. :captcure - Where king can captcure opponent chessman
   2. :move - Where king can be placed (fields that are not occupied). 
   If xy is not valid possition or xy is not occupied by king, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \k)
    (reduce (fn [new-map possible-move]
              (let [key (get (map->vec possible-move) 0)
                    val (get (map->vec possible-move) 1)
                    color (get-chessman-color-short-name chessboard x y)]
                (if (not-occupied? chessboard key val)
                  (add-to-move-captcure-map new-map :move [possible-move])
                  (if (not-same-color? chessboard x y key val)
                    (add-to-move-captcure-map new-map :captcure [possible-move])
                    new-map))))
            init-move-captcure-map
            (king-moves-vectors chessboard x y))))


(defn get-chessman-at-xy-possible-moves
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
        (= chessman-short-name \k) (get-king-possible-moves chessboard x y)
        :else init-move-captcure-map))
    init-move-captcure-map))

(defn attacked-by-opponent-king? 
  "Returns true if king on xy is attacked by opponent king, otherwise false. "
  [chessboard x y]
  (and (chessman-type? chessboard x y \k)
       (let [opponent-king-location (get-king-location chessboard (get-opposite-color (get-chessman-color-short-name chessboard x y)))
             key (get (map->vec opponent-king-location) 0)
             val (get (map->vec opponent-king-location) 1)]
         (chessman-legal-move? (get-king-possible-moves-ignore-check chessboard key val) (hash-map x y)))))

(defn not-attacked-by-opponent-king?
  "Returns true if king on xy is NOT attacked by opponent king, otherwise false. "
  [chessboard x y]
  (and (chessman-type? chessboard x y \k) (not (attacked-by-opponent-king? chessboard x y))))

(defn filter-chessman-moves-that-will-lead-to-check
  "Returns sequence of fields that are legal moves (moving from xy to all result fields will not lead to check of own king). "
  [chessboard x y move-vector]
  (filter #(let [key (get (map->vec %) 0)
                 val (get (map->vec %) 1)
                 color (get-chessman-color-short-name chessboard x y)]
             (and (valid-xy? key val)
                  (is-not-check? (make-move chessboard x y key val) color)
                  (or (not-chessman-type? chessboard x y \k)
                      (not-attacked-by-opponent-king? (make-move chessboard x y key val) key val)))) move-vector))

(defn get-chessman-at-xy-all-legal-moves
  "Returns map {:captcure {...} :move {...}} with all possible moves of the chessman located on xy field. 
   Moves that will lead to check player's own king will be excluded. Moves are separated in two maps: 
   1. :captcure - Where chessman can captcure opponent chessman
   2. :move - Where chessman can be placed (fields that are not occupied).
   If xy is not valid possition or xy is not occupied, initial move-captcure map is returned. "
  [chessboard x y]
  (if (valid-xy? x y)
    (let [chessman-all-possible-moves (get-chessman-at-xy-possible-moves chessboard x y)]
      {:move (vec (filter-chessman-moves-that-will-lead-to-check chessboard x y (:move chessman-all-possible-moves)))
       :captcure (vec (filter-chessman-moves-that-will-lead-to-check chessboard x y (:captcure chessman-all-possible-moves)))})))

(defn get-player-all-possible-legal-moves
  "Returns map {:captcure {...} :move {...}} with all possible moves of all chessmans on the board with chessman-color color. 
   Moves that will lead to check of player's own king are excluded. Moves are separated in two maps: 
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
                     (if (= chessman-color (get-chessman-color-short-name chessboard init-key init-val-pos))
                       (recur (merge-two-move-captcure-maps init-result-map-2 (get-chessman-at-xy-all-legal-moves chessboard init-key init-val-pos)) (inc init-val-pos))
                       (recur init-result-map-2 (inc init-val-pos)))
                     init-result-map-2)) (inc-x init-key)))
        init-result-map))))



