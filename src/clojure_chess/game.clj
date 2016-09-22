(ns clojure-chess.game
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessboard :refer :all]
            [clojure-chess.fields :refer :all]
            [clojure-chess.move :refer :all]
            [clojure-chess.army.king :refer [king-moves-vectors]]))

(defn print-all-atoms-value
  "Function print current state of all atom value (e.g. rook-a-0-not-moved, rook-h-7-not-moved, black-king-not-moved, etc...). "
  []
  (do (println (str "Rook a0: " @rook-a-0-not-moved))
    (println (str "Rook h0: " @rook-h-0-not-moved))
    (println (str "Rook a7: " @rook-a-7-not-moved))
    (println (str "Rook h7: " @rook-h-7-not-moved))
    (println (str "White king: " @white-king-not-moved))
    (println (str "Black king: " @black-king-not-moved))
    (println (str "On the move: " @player-on-move))))

(defn new-game 
      "Reset all atom value to initial value. "
      []
      (do (set-current-chessboard init-board)
        (change-atom-value-from-false-to-true rook-a-0-not-moved)
        (change-atom-value-from-false-to-true rook-h-0-not-moved)
        (change-atom-value-from-false-to-true rook-a-7-not-moved)
        (change-atom-value-from-false-to-true rook-h-7-not-moved)
        (change-atom-value-from-false-to-true white-king-not-moved)
        (change-atom-value-from-false-to-true black-king-not-moved)
        (set-last-input [])
        (set-last-move [])
        (reset! player-on-move \w)
        (print-all-atoms-value)
        "Chessboard is ready for new game! "))

(defn new-en-passant-game 
      "Reset all atom value to en passant suitable value. "
      []
      (do (set-current-chessboard en-passant-board)
        (change-atom-value-from-true-to-false rook-a-0-not-moved)
        (change-atom-value-from-true-to-false rook-h-0-not-moved)
        (change-atom-value-from-true-to-false rook-a-7-not-moved)
        (change-atom-value-from-true-to-false rook-h-7-not-moved)
        (change-atom-value-from-false-to-true white-king-not-moved)
        (change-atom-value-from-false-to-true black-king-not-moved)
        (set-last-input [])
        (set-last-move [])
        (reset! player-on-move \w)
        (print-all-atoms-value)
        "Chessboard is ready for en passant move! "))

(defn new-promotion-game 
      "Reset all atom value to promotion suitable value. "
      []
      (do (set-current-chessboard promotion-board)
        (change-atom-value-from-true-to-false rook-a-0-not-moved)
        (change-atom-value-from-true-to-false rook-h-0-not-moved)
        (change-atom-value-from-true-to-false rook-a-7-not-moved)
        (change-atom-value-from-true-to-false rook-h-7-not-moved)
        (change-atom-value-from-true-to-false white-king-not-moved)
        (change-atom-value-from-true-to-false black-king-not-moved)
        (set-last-input [])
        (set-last-move [])
        (reset! player-on-move \w)
        (print-all-atoms-value)
        "Chessboard is ready for promotion move! "))

(defn new-castling-game 
      "Reset all atom value to castling suitable value. "
      []
      (do (set-current-chessboard castling-board)
        (change-atom-value-from-true-to-false rook-a-0-not-moved)
        (change-atom-value-from-false-to-true rook-h-0-not-moved)
        (change-atom-value-from-false-to-true rook-a-7-not-moved)
        (change-atom-value-from-true-to-false rook-h-7-not-moved)
        (change-atom-value-from-false-to-true white-king-not-moved)
        (change-atom-value-from-false-to-true black-king-not-moved)
        (set-last-input [])
        (set-last-move [])
        (reset! player-on-move \w)
        (print-all-atoms-value)
        "Chessboard is ready for castling move! "))

(defn new-checkmate-game 
      "Reset all atom value to checkmate suitable value. "
      []
      (do (set-current-chessboard checkmate-board)
        (change-atom-value-from-false-to-true rook-a-0-not-moved)
        (change-atom-value-from-false-to-true rook-h-0-not-moved)
        (change-atom-value-from-false-to-true rook-a-7-not-moved)
        (change-atom-value-from-false-to-true rook-h-7-not-moved)
        (change-atom-value-from-false-to-true white-king-not-moved)
        (change-atom-value-from-false-to-true black-king-not-moved)
        (set-last-input [])
        (set-last-move [])
        (reset! player-on-move \b)
        (print-all-atoms-value)
        "Chessboard is ready for checkmate game! "))

(defn new-stalemate-game 
      "Reset all atom value to stalemate suitable value. "
      []
      (do (set-current-chessboard stalemate-board)
        (change-atom-value-from-true-to-false rook-a-0-not-moved)
        (change-atom-value-from-true-to-false rook-h-0-not-moved)
        (change-atom-value-from-true-to-false rook-a-7-not-moved)
        (change-atom-value-from-true-to-false rook-h-7-not-moved)
        (change-atom-value-from-true-to-false white-king-not-moved)
        (change-atom-value-from-true-to-false black-king-not-moved)
        (set-last-input [])
        (set-last-move [])
        (reset! player-on-move \w)
        (print-all-atoms-value)
        "Chessboard is ready for stalemate move! "))

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

(defn do-en-passant
  "Helper function that checks if last turn was 'En Passant' move, and if yes, remove opposite pawn that should be eaten. 
   In all other cases, passed in board is returned. "
  [chessboard from-x from-y to-x to-y]
  (let [chessman-short-name (get-chessman-short-name chessboard to-x to-y)
        chessman-name (get chessman-short-name 1)
        chessman-color (get chessman-short-name 0)
        last-move-from-x (get @last-move 0)
        last-move-from-y (get @last-move 1)
        last-move-to-x (get @last-move 2)
        last-move-to-y (get @last-move 3)
        last-move-chessman-color (get-opposite-color chessman-color)
        last-move-chessman-name (get (get-chessman-short-name chessboard to-x to-y) 1)]
    (if (and (= chessman-name \p) (= last-move-chessman-name \p) 
             (= to-x last-move-to-x) (= from-y last-move-to-y))
      (if (or (and (= last-move-chessman-color \b) (= last-move-from-y 6) (= last-move-to-y 4))
              (and (= last-move-chessman-color \w) (= last-move-from-y 1) (= last-move-to-y 3)))
        (remove-chessman chessboard last-move-to-x last-move-to-y)
        chessboard)
      chessboard)))

(defn do-castling
  "Helper function that checks if queen or king side castling is the player, and if that is the case, rook is moved to appropriate field. 
   In all other cases, passed in board is returned. "
  [chessboard from-x from-y to-x to-y]
  (let [chessman-short-name (get-chessman-short-name chessboard to-x to-y)
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
        :else chessboard)
      chessboard)))

(defn do-promotion
  "If move from-xfrom-y to-xto-y is the move that is setting pawn on the opposite side of the board, pawn will be promoted to the queen. 
   In all other cases, passed in chessboard is returned. "
  [chessboard from-x from-y to-x to-y]
  (let [chessman-short-name (get-chessman-short-name chessboard to-x to-y)
        chessman-name (get chessman-short-name 1)
        chessman-color (get chessman-short-name 0)]
    (if (= chessman-name \p)
      (cond
        (and (= chessman-color \w) (= to-y 7)) (set-chessman-to-xy-field chessboard to-x to-y "wq")
        (and (= chessman-color \b) (= to-y 0)) (set-chessman-to-xy-field chessboard to-x to-y "bq")
        :else chessboard)
      chessboard)))

(defn change-variables-state
  "Functions checks which move is played, and sets variable base on move. If some of the rooks or kings are moved, it will be marked as moved. 
   Also, last input is set to empty vector and last move is saved. Player turn is changed. Function also makes move, and call functions 
   that checks does promotions of pawn needs to be done, or does rook needs to be moved in case of castling. Method returns chessboard with move played. "
  [chessboard from-x from-y to-x to-y]
  (let [chessman-short-name (get-chessman-short-name chessboard from-x from-y)
        chessman-name (get chessman-short-name 1)
        chessman-color (get chessman-short-name 0)
        result-chessboard (do-castling (do-en-passant (do-promotion (make-move chessboard from-x from-y to-x to-y) from-x from-y to-x to-y) from-x from-y to-x to-y) from-x from-y to-x to-y)]
    (do 
      (set-last-input [])
      (set-last-move from-x from-y to-x to-y)
      (if (and @rook-a-0-not-moved (= chessman-short-name "wr") (= from-x :a) (= from-y 0))
        (change-atom-value-from-true-to-false rook-a-0-not-moved))
      (if (and @rook-h-0-not-moved (= chessman-short-name "wr") (= from-x :h) (= from-y 0))
        (change-atom-value-from-true-to-false rook-h-0-not-moved))
      (if (and @rook-a-7-not-moved (= chessman-short-name "br") (= from-x :a) (= from-y 7))
        (change-atom-value-from-true-to-false rook-a-7-not-moved))
      (if (and @rook-h-7-not-moved (= chessman-short-name "br") (= from-x :h) (= from-y 7))
        (change-atom-value-from-true-to-false rook-h-7-not-moved))
      (if (and @white-king-not-moved (= chessman-short-name "wk") (= from-x :e) (= from-y 0))
        (change-atom-value-from-true-to-false white-king-not-moved))
      (if (and @black-king-not-moved (= chessman-short-name "bk") (= from-x :e) (= from-y 7))
        (change-atom-value-from-true-to-false black-king-not-moved))
      (change-player-turn)
      result-chessboard)))


(defn process-second-part-of-the-move
  "Function checks if passed in field is same field as last-input field and, if yes, remove last-input information (unselect selected field). 
   If passed in field is legal move from last-input field, function will make move and set all needed information. If move is made, new chessboard will be returned, otherwase passed in board. "
  [chessboard to-x to-y]
  (let [from-x (get @last-input 0)
        from-y (get @last-input 1)]
    (if (and (= from-x to-x) (= from-y to-y))
      (do (set-last-input [])
        [chessboard (str (get-chessman-full-name chessboard from-x from-y) " on the field " (key->str from-x) (y->str from-y) " is not selected any more. Please select chessman that you want to move. ")])
      (if (chessman-legal-move? (get-chessman-at-xy-all-legal-moves chessboard from-x from-y) (hash-map to-x to-y))
        [(change-variables-state chessboard from-x from-y to-x to-y) (str (get-chessman-full-name chessboard from-x from-y) " successfully moved from field " (key->str from-x) (y->str from-y) 
                                                                          " to field " (key->str to-x) (y->str to-y) ". " (get-color-full-name @player-on-move) " player is on the move. ")]
        [chessboard (str "Field " (key->str to-x) (y->str to-y) " is not valid move field for " (get-chessman-full-name chessboard from-x from-y) 
                         " located on field " (key->str from-x) (y->str from-y) ". Please select another destination field. ")]))))

(defn process-first-part-of-the-move
  "Function checks if passed in field is occupied by player-on-the-move chessman. If yes, method saves passed in field. In case of the invalid field 
   (field is not occupied by player on the move chessman or if chessman on the field doesn't have available moves), messagge is created. In both cases, 
   vector containing board and messagge will be returned. "
  [chessboard from-x from-y]
  (if (= @player-on-move (get-chessman-color-short-name chessboard from-x from-y))
    (if (can-chessman-be-moved-from-xy? chessboard from-x from-y)
      (do (set-last-input [from-x from-y])
        [chessboard (str "Chessman on field " (key->str from-x) (y->str from-y) " (" (get-chessman-full-name chessboard from-x from-y) ") successfully selected. Choose field to move it! " )])
      [chessboard (str "Selected chessman on field " (key->str from-x) (y->str from-y) " doesn't have any available moves. Please select another chessman. ")])
    [chessboard (str (get-color-full-name @player-on-move) " player is on the move. " (get-color-full-name (get-opposite-color @player-on-move)) " chessman cannot be selected. Please select another chessman. ")]))

(defn process-move
  "Function receive move, and precess move in particural way. "
  [chessboard input]
  (let [input-key (get (input->vec input) 0)
        input-val (get (input->vec input) 1)]
    (if (empty? @last-input) 
      (if (occupied? chessboard input-key input-val)
        (process-first-part-of-the-move chessboard input-key input-val)
        [chessboard (str "Selected field is empty. Please select chessman that you want to move. " (get-color-full-name @player-on-move) " is on the move. ")])
      (process-second-part-of-the-move chessboard input-key input-val))))

(defn play-move
  "Method receive string that represent field on the board (e.g. 'a1', 'h8', 'd3') and call function to process that move. 
   Chessboard and the message are printed as the result. "
  [input]
  (if (valid-input? input)
    (let [input-key (get (input->vec input) 0)
          input-val-dec (get (input->vec input) 1)
          result (process-move @current-chessboard (str (key->str input-key) input-val-dec))
          chessboard (sort-chessboard-map-by-key  (get result 0))
          message (str (get result 1))]
      (do (set-current-chessboard chessboard)
        (cond 
        (checkmate? @current-chessboard @player-on-move) [@current-chessboard (str message  (get-color-full-name (get-opposite-color @player-on-move)) " player won! Game is over! ")]
        (stalemate? @current-chessboard @player-on-move) [@current-chessboard (str message (get-color-full-name @player-on-move) " player cannot move! Stalemate! Game is over! ")]
        (is-check? @current-chessboard @player-on-move) [@current-chessboard (str message (get-color-full-name  @player-on-move) " player is under the check! ")] 
        :else [@current-chessboard message])))
    [(sort-chessboard-map-by-key @current-chessboard) "Passed in value is not valid field on the board. "]))

(defn process-input
  "Initital method that process possible move on the player on turn. If it is not checkmate or stalemate, play-move function will be called and result will be returned back as [chessboard message]. "
  [input]
  (cond 
        (checkmate? @current-chessboard @player-on-move) [@current-chessboard (str (get-color-full-name (get-opposite-color @player-on-move)) " player won! Game is over! ")]
        (stalemate? @current-chessboard @player-on-move) [@current-chessboard (str (get-color-full-name  @player-on-move) " player cannot move! Stalemate! Game is over! ")]
        :else (play-move input)))

(defn console-play-input
  "Function print result of call in more friendly way. "
  [input]
  (let [result (process-input input)
        result-chessboard (get result 1)
        result-message (get result 0)]
    (print-call-result result-chessboard result-message)))
