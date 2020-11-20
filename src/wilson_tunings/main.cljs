(ns wilson-tunings.main
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [wilson-tunings.cps :as cps]
            [wilson-tunings.mos :as mos]))

(defonce state (r/atom {:view :nil
                        :generators "1,3,5,7"
                        :set-size "2"
                        :mos/submos-representation-mode :unique-mos
                        :mos/period 12
                        :mos/generator 5}))

(defn app []
  (case (@state :view)
    :cps (cps/main state)
    :mos (mos/main state)
    [:div {:class "wt__main-screen"}
     [:h1 "Tools for exploring some of Erv Wilsons scale concepts"]
     [:p "What do you want to see?"]
     [:button {:on-click #(swap! state assoc :view :mos)} "Moments of symmetry calculator"]
     [:button {:on-click #(swap! state assoc :view :cps)} "CPS calculator"]]))

(defn start [element-id]
  (dom/render [app] (. js/document (getElementById element-id))))

(defn stop []
  (println "Restarting"))

(comment
  (reset! app-state))

(defn ^:export init [opts]
  (if-let [view (-> opts js->clj (get "view") keyword)]
    (swap! state assoc :view view))
  (let [element-id (-> opts js->clj (get "elementId" "app"))]
    (start element-id)))
