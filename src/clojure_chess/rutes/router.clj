(ns clojure-chess.rutes.router
  (:require [compojure.core :refer [defroutes GET]]
            [compojure.route :as route]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [clojure-chess.rutes.view :as view]
            [ring.adapter.jetty :as jetty]))

(defroutes app-routes
  (GET "/" [] (view/new-game-page))
  (GET "/:move" [move] (view/make-move move))
  (GET "/en-passant/" [] (view/en-passan-test-possition))
  (GET "/promotion/" [] (view/promotion-test-possition))
  (GET "/castling/" [] (view/castling-test-possition))
  (GET "/checkmate/" [] (view/checkmate-test-possition))
  (GET "/stalemate/" [] (view/stalemate-test-possition)))

(def app
  (wrap-defaults app-routes site-defaults))