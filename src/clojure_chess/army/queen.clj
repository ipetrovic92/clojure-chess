(ns clojure-chess.army.queen
  (:require [clojure.repl :refer :all]
            [clojure.core :refer :all]
            [clojure-chess.chessboard :refer :all]
            [clojure-chess.chessman :refer :all]
            [clojure-chess.army.rook :refer [rook-moves-vectors]]
            [clojure-chess.army.bishop :refer [bishop-moves-vectors]]))

(defn get-queen-possible-moves
  "Returns map {:eat {...} :move {...}} with all possible moves for the queen located on xy field. Moves are separated in two maps: 
   1. :eat - Where rook can eat opponent chessman
   2. :move - Where rook can be placed (fields that are not occupied). 
   If xy is not valid possition or xy is not occupied by queen, nil is returned. "
  [chessboard x y]
  (if (chessman-type? chessboard x y \q)
    (let [as-rook-moves-map (get-chessman-possible-moves chessboard x y (rook-moves-vectors x y))
          as-bishop-moves-map (get-chessman-possible-moves chessboard x y (bishop-moves-vectors x y))]
      (merge-two-move-eat-maps as-rook-moves-map as-bishop-moves-map))))
