(ns clojure-chess.rutes.view
  (:use hiccup.page hiccup.element)
  (:require 
    [clojure-chess.rutes.layout :as layout]
    [clojure-chess.fields :as fields]
    [clojure-chess.chessboard :as board]
    [clojure-chess.game :as game]
    [hiccup.core :refer [h]]))

(defn field-color
  "Function determine background color of the chess field base on field position"
  [key val]
  (let [x (board/x-as-num key)
        y val]
    (if(or (and (= 0 (mod x 2)) (= 0 (mod y 2)))
           (and (= 1 (mod x 2)) (= 1 (mod y 2))))
      (str "black")
      (str "white"))))


(defn display-cheesboard 
  "Function creates HTML that will show chessboard and the message. "
  [chessboard message]
  [:div 
   [:table  {:class "table"}
    [:th {:class "table-header"} ""]
    [:th {:class "table-header"} "1"]
    [:th {:class "table-header"} "2"]
    [:th {:class "table-header"} "3"]
    [:th {:class "table-header"} "4"]
    [:th {:class "table-header"} "5"]
    [:th {:class "table-header"} "6"]
    [:th {:class "table-header"} "7"]
    [:th {:class "table-header"} "8"]
    (map 
      (fn [[key val]]
        (let [x-string (fields/key->str key)]
          [:tr {:call "row"} 
           [:td {:class "letters"} (h x-string)]
           [:td {:class "cell"} [:a {:href (str "/" x-string 0)} [:img {:src (str "/img/" (field-color key 0) "-" (get val 0) ".png")} ]]]
           [:td {:class "cell"} [:a {:href (str "/" x-string 1)} [:img {:src (str "/img/" (field-color key 1) "-" (get val 1) ".png")} ]]]
           [:td {:class "cell"} [:a {:href (str "/" x-string 2)} [:img {:src (str "/img/" (field-color key 2) "-" (get val 2) ".png")} ]]]
           [:td {:class "cell"} [:a {:href (str "/" x-string 3)} [:img {:src (str "/img/" (field-color key 3) "-" (get val 3) ".png")} ]]]
           [:td {:class "cell"} [:a {:href (str "/" x-string 4)} [:img {:src (str "/img/" (field-color key 4) "-" (get val 4) ".png")} ]]]
           [:td {:class "cell"} [:a {:href (str "/" x-string 5)} [:img {:src (str "/img/" (field-color key 5) "-" (get val 5) ".png")} ]]]
           [:td {:class "cell"} [:a {:href (str "/" x-string 6)} [:img {:src (str "/img/" (field-color key 6) "-" (get val 6) ".png")} ]]]
           [:td {:class "cell"} [:a {:href (str "/" x-string 7)} [:img {:src (str "/img/" (field-color key 7) "-" (get val 7) ".png")} ]]]
           ])) (into (sorted-map) chessboard))]
   [:p2 {:id "message"} (str message)]])

(defn create-header 
  "Creates link for new game chessboard and test chessboards. "
  []
  [:ul {:class "tab"}
   [:li [:a {:href "/" :class "tablinks"} (h "New Game")]]
   [:li [:a {:href "/en-passant/" :class "tablinks"} (h "En Passant")]]
   [:li [:a {:href "/promotion/" :class "tablinks"} (h "Promotion")]]
   [:li [:a {:href "/castling/" :class "tablinks"} (h "Castling")]]
   [:li [:a {:href "/checkmate/" :class "tablinks"} (h "Checkmate")]]
   [:li [:a {:href "/stalemate/" :class "tablinks"} (h "Stalemate")]]])

(defn new-game-page
  "Shows new game page. "
  []
  (layout/common-layout (create-header) (do (game/new-game)
                                          (display-cheesboard (fields/sort-chessboard-map-by-key board/init-board) "Chees board is ready! White is on the move! ")))) 

(defn make-move
  "Function process payer move and return new chessboard and message. "
  [move]
  (let [result (game/process-input move)
        chessboard (get result 0)
        message (get result 1)]
    (layout/common-layout (create-header) (display-cheesboard (fields/sort-chessboard-map-by-key chessboard) message))))

(defn en-passan-test-possition
  "Shows en passant board possition. "
  []
  (layout/common-layout (create-header) (do (game/new-en-passant-game)
                                          (display-cheesboard (fields/sort-chessboard-map-by-key board/en-passant-board) "En passant board is ready! White is on the move! "))))

(defn promotion-test-possition
  "Shows promotion board possition. "
  []
  (layout/common-layout (create-header) (do (game/new-promotion-game)
                                          (display-cheesboard (fields/sort-chessboard-map-by-key board/promotion-board) "En passant board is ready! White is on the move! "))))

(defn castling-test-possition
  "Shows castling board possition. "
  []
  (layout/common-layout (create-header) (do (game/new-castling-game)
                                          (display-cheesboard (fields/sort-chessboard-map-by-key board/castling-board) "Castling board is ready! White is on the move! "))))

(defn checkmate-test-possition
  "Shows checkmate board possition. "
  []
  (layout/common-layout (create-header) (do (game/new-checkmate-game)
                                          (display-cheesboard (fields/sort-chessboard-map-by-key board/checkmate-board) "Checkmate board is ready! Black is on the move! (Queen d8-h4)"))))

(defn stalemate-test-possition
  "Shows stalemate board possition. "
  []
  (layout/common-layout (create-header) (do (game/new-stalemate-game)
                                          (display-cheesboard (fields/sort-chessboard-map-by-key board/stalemate-board) "Stalemate board is ready! White is on the move! (King e8-f8)"))))

