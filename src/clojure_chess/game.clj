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

(defn can-chessman-be-moved-from-xy?
  "Returns true if chessman on xy has avaliable moves, otherwise false. If passed in values are not valid xy values, nil is returned. "
  [chessboard x y]
  (if (valid-xy? x y)
    (not-empty? (move-capture-map->vec (get-chessman-at-xy-all-legal-moves chessboard x y)))))

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

(defn checkmate?
  "Returns true if player with chessman-color chesmans is checkmated, otherwise false. "
  [chessboard chessman-color]
  (and (color? chessman-color) 
       (is-check? chessboard chessman-color) 
       (empty? (move-capture-map->vec (get-player-all-possible-legal-moves chessboard chessman-color)))))

(defn not-checkmate?
  "Returns true if player with chessman-color chesmans is NOT checkmated, otherwise false. "
  [chessboard chessman-color]
  (and (color? chessman-color) 
       (not (checkmate? chessboard chessman-color))))

(defn stalemate?
  "Returns true if player with chessman-color chesmans is in stalemate, otherwise false. "
  [chessboard chessman-color]
  (and (color? chessman-color) 
       (is-not-check? chessboard chessman-color) 
       (empty? (move-capture-map->vec (get-player-all-possible-legal-moves chessboard chessman-color)))))

(defn not-stalemate?
  "Returns true if player with chessman-color chesmans is NOT in stalemate, otherwise false. "
  [chessboard chessman-color]
  (and (color? chessman-color) 
       (not (stalemate? chessboard chessman-color))))

(defn do-castling
  
  [chessboard from-x from-y to-x to-y]
  (let [chessman-short-name (get-chessman-short-name chessboard from-x from-y)
        chessman-name (get chessman-short-name 1)
        chessman-color (get chessman-short-name 0)]
    (if (= chessman-name \k)
      (cond 
        (and (= from-x :e) (= from-y 0) (= to-x :g) (= to-y 0)) (do (change-atom-value-from-true-to-false rook-h-0-not-moved)
                                                                  (make-move chessboard :h 0 :f 0))
        (and (= from-x :e) (= from-y 0) (= to-x :c) (= to-y 0)) (do (change-atom-value-from-true-to-false rook-a-0-not-moved)
                                                                  (make-move chessboard :a 0 :d 0))
        (and (= from-x :e) (= from-y 7) (= to-x :g) (= to-y 7)) (do (change-atom-value-from-true-to-false rook-h-7-not-moved)
                                                                  (make-move chessboard :h 7 :f 7))
        (and (= from-x :e) (= from-y 7) (= to-x :c) (= to-y 7)) (do (change-atom-value-from-true-to-false rook-a-7-not-moved)
                                                                  (make-move chessboard :a 7 :f 7))
        :else nil)
      [1])))

(defn do-promotion
  "If move from-xfrom-y to-xto-y is the move that is setting pawn on the opposite side of the board, pawn will be promoted to queen. "
  [chessboard from-x from-y to-x to-y]
  (let [chessman-short-name (get-chessman-short-name chessboard from-x from-y)
        chessman-name (get chessman-short-name 0)
        chessman-color (get chessman-short-name 1)]
    (if (= chessman-name \p)
      (cond
        (and (= chessman-color \w) (= to-y 7)) (set-chessman-to-xy-field chessboard to-x to-y "wq")
        (and (= chessman-color \b) (= to-y 0)) (set-chessman-to-xy-field chessboard to-x to-y "bq")
        :else chessboard)
      chessboard)))

(defn change-variables-state
  
  [chessboard from-x from-y to-x to-y]
  (let [chessman-short-name (get-chessman-short-name chessboard from-x from-y)
        chessman-name (get chessman-short-name 0)
        chessman-color (get chessman-short-name 1)
        ]
    (do 
      (set-last-input [])
      (set-last-move from-x from-y to-x to-y)
      (if @rook-a-0-not-moved (and (= from-x :a) (= from-y 0))
        (change-atom-value-from-true-to-false rook-a-0-not-moved))
      (if @rook-h-0-not-moved (and (= from-x :h) (= from-y 0))
        (change-atom-value-from-true-to-false rook-h-0-not-moved))
      (if @rook-a-7-not-moved (and (= from-x :a) (= from-y 7))
        (change-atom-value-from-true-to-false rook-a-7-not-moved))
      (if @rook-h-7-not-moved (and (= from-x :h) (= from-y 7))
        (change-atom-value-from-true-to-false rook-h-7-not-moved))
      (if @white-king-not-moved (and (= from-x :e) (= from-y 0))
        (change-atom-value-from-true-to-false white-king-not-moved))
      (if @black-king-not-moved (and (= from-x :e) (= from-y 7))
        (change-atom-value-from-true-to-false black-king-not-moved))
      (change-player-turn)
      (do-promotion (do-castling (make-move chessboard from-x from-y to-x to-y) chessboard from-x from-y to-x to-y) from-x from-y to-x to-y)
      )))

(defn process-first-part-of-the-move
  "Function checks if passed in field is occupied by player-on-the-move chessman. If yes, method saves passed in field. In case of the invalid field 
   (field is not occupied by player on the move chessman or if chessman on the field doesn't have available moves), messagge is created. In both cases, 
   vector containing board and messagge will be returned. "
  [chessboard from-x from-y]
  (if (= @player-on-move (get-chessman-color-short-name chessboard from-x from-y))
    (if (can-chessman-be-moved-from-xy? chessboard from-x from-y)
      (do (set-last-input [from-x from-y])
        [chessboard (str "Chessman on field " (key->str from-x) (y->str from-y) " (" (get-chessman-full-name chessboard from-x from-y) ") successfully selected. Choose field to move it! " )])
      [chessboard "Selected chessman on field " (key->str from-x) (y->str from-y) " doesn't have any available moves. Please select another chessman. "])
    [chessboard (str (get-color-full-name @player-on-move) " player is on the move. " (get-color-full-name (get-opposite-color @player-on-move)) " chessman cannot be selected. Please select another chessman. ")]))

(defn process-second-part-of-the-move
  
  [chessboard to-x to-y]
  (let [from-x (get @last-input 0)
        from-y (get @last-input 1)]
    (if (and (= from-x to-x) (= from-y to-y))
      (do (set-last-input [])
        [chessboard (str "Moving chessman " (get-chessman-full-name chessboard from-x from-y) " from field " (key->str from-x) (y->str from-y) " canceled. Select new figure to move. ")])
      (if (chessman-legal-move? (get-chessman-at-xy-all-legal-moves chessboard from-x from-y) (hash-map to-x to-y))
        [(change-variables-state chessboard from-x from-y to-x to-y) (str (get-chessman-full-name chessboard from-x from-y) " successfully moved from field " (key->str from-x) (y->str from-y) 
                                                                          "to field" (key->str to-x) (y->str to-y) ". " (get-color-full-name (get-opposite-color @player-on-move)) " is on the move. ")]
        [chessboard (str "Field " (key->str to-x) (y->str to-y) " is not valid move field for " (get-chessman-full-name chessboard from-x from-y) 
                         " located on field " (key->str from-x) (y->str from-y) ". Please select another destination field. ")]))))

(defn process-move
  
  [chessboard input]
  (if (valid-input? input)
    (let [input-key (get (input->vec input) 0)
          input-val (get (input->vec input) 1)]
      (if (empty? @last-input) 
        (if (occupied? chessboard input-key input-val)
          (process-first-part-of-the-move chessboard input-key input-val)
          [chessboard "You need to select chessman first! "])
        (process-second-part-of-the-move chessboard input-key input-val)))
    [chessboard "Passed in value is not valid field on the board. "]))


