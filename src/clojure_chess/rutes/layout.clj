(ns clojure-chess.rutes.layout
  (:require [hiccup.page :as h-page]
            [hiccup.page :refer [html5 include-css]]))

(defn common-layout [header & body]
  (h-page/html5
    [:head
     [:title "Chess"]
     (include-css "/css/chess.css")]
    [:body
     
     [:div {:class "header"} header]
     [:div {:id "content" :class "container"} body]]))