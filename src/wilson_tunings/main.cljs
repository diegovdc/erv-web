(ns wilson-tunings.main
  (:require [reagent.dom :as dom]
            [taoensso.timbre :as timbre]
            [wilson-tunings.cps.core :as cps]
            [wilson-tunings.cps-colors :as cps-colors]
            [wilson-tunings.marwa :as marwa]
            [wilson-tunings.mos :as mos]
            [wilson-tunings.mos2 :as mos2]
            [wilson-tunings.state :refer [state]]))

(timbre/set-level! :info)

(defn app []
  (case (@state :view)
    :cps (cps/main state)
    :cps-colors (cps-colors/main state)
    :mos (mos/main state)
    :mos.v2 (mos2/main state)
    :marwa (marwa/main state)
    [:div {:class "wt__main-screen"}
     [:h1 "Tools for exploring some of Erv Wilsons scale concepts"]
     [:p "What do you want to see?"]
     [:button {:on-click #(swap! state assoc :view :mos)} "Moments of symmetry calculator"]
     [:button {:on-click #(swap! state assoc :view :mos.v2)} "Moments of symmetry calculator (V2)"]
     [:button {:on-click #(swap! state assoc :view :marwa)} "Marwa Permutations"]
     [:button {:on-click #(swap! state assoc :view :cps)} "CPS calculator"]
     [:button {:on-click #(swap! state assoc :view :cps-colors)} "CPS Colors (experiment)"]]))

(defn start
  ([] (start "wilson-tunings-calculator"))
  ([element-id]
   (dom/render [app] (. js/document (getElementById element-id)))))

(defn stop []
  (println "Restarting"))

(comment
  (reset! app-state))

(defn ^:export init [opts]
  (if-let [view (-> opts js->clj (get "view") keyword)]
    (swap! state assoc :view view))
  (let [element-id
        (-> opts js->clj
            (get "elementId" "wilson-tunings-calculator"))]
    (println "ELEMTNID" element-id)
    (start element-id)))
