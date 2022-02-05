(ns wilson-tunings.cps-colors
  (:require [goog.string :as gstr]
            [goog.string.format]
            ["react-color" :refer [ChromePicker]]
            ["color-blend" :as blend]
            [erv.cps.core :as cps]
            [reagent.core :as r]
            [clojure.set :as set]))


(defn factor-names [total-factors]
  (map (comp keyword js/String.fromCharCode)
       (range 97 (+ 97 total-factors))))


(defonce color-atom (r/atom
                     (->> 26
                          factor-names
                          (map (fn [k] [k {"r" 0 "g" 0 "b" 0}]))
                          (into (sorted-map)))))


(defn get-color-string [color]
  (gstr/format "rgba(%s, %s,  %s, %s)"
               (get color "r")
               (get color "g")
               (get color "b")
               (get color "a" 1)))

(defn color-picker [id color]
  [:div
   [:> ChromePicker
    {:color (clj->js (get @color-atom id color))
     :on-change #(swap! color-atom assoc
                        id (-> % js->clj (get "rgb")))}]])



(defn square [color &
              {:keys [on-click]
               :or {on-click (fn [_]
                               (println "cl")
                               nil)}}]
  (when color
    [:div
     {:style {:height 30
              :width 30
              :background-color (get-color-string color)}
      :on-click on-click}]))

(defonce cps-config (r/atom {:size 1 :num-factors 1}))

(def pickers (r/atom {}))

(comment
  (-> @pickers)
  (swap! pickers update-in [:a :show?] (comp not boolean)))

(defn blend-colors [colors]
  (let [cs (map clj->js colors)]
    (js->clj (reduce
              (fn [c1 c2] ((.-difference blend) c1 c2))
              (first cs)
              (rest cs)))))

#_(blend-colors '({"r" 138, "g" 67, "b" 67, "a" 1}
                  {"r" 171, "g" 89, "b" 89, "a" 1}))

(do
  (defn combinations []
    (let [factors (map first (subvec (vec (seq @color-atom))
                                     0
                                     (@cps-config :num-factors)))
          groups (map sort (cps/->cps (dec (@cps-config :size)) factors))
          combos (map sort (cps/->cps (@cps-config :size) factors))
          product-squares (->> combos
                               (map (fn [factors]
                                      [(set factors)
                                       [:div {:style {:margin-left 10}}
                                        [:h3 (map name factors)]
                                        (->> factors
                                             (select-keys @color-atom)
                                             vals
                                             blend-colors
                                             square)]]))
                               (into {}))]
      (map
       (fn [g]
         [:div {:style {:display "flex"}}
          [:h2 (map name g)]
          (map second
               (filter (fn [[fs _]]
                         (set/subset? g fs))
                       product-squares))])
       groups)))

  (combinations))
(defn main [state]
  [:div
   [:h1 "Combination Product Sets Color Maker"]
   [:div
    [:div
     [:label "Set size "
      [:input
       {:min 1
        :type :number
        :value (@cps-config :size)
        :on-change #(swap! cps-config assoc :size
                           (-> % .-target .-value js/Number))}]]]
    [:div
     [:label "Number of factors "
      [:input {:min 1
               :max 26
               :type :number
               :value (@cps-config :num-factors)
               :on-change #(swap! cps-config assoc :num-factors
                                  (-> % .-target .-value js/Number))} ]]]]
   [:div {:style {:display "flex"
                  :flex-wrap "wrap"}}
    (doall
     (map
      (fn [[k color]]
        (let [on-click #(swap! pickers update-in [k :show?] (comp not boolean)) ]
          [:div
           [:h2 (name k)]
           [:div {:position "relative"}
            (square color
                    :on-click on-click)
            [:div {:position "absolute"}
             (if (-> @pickers k :show?)
               [:div (color-picker k color)
                [:button {:on-click on-click}
                 "Hide"]]
               [:button {:on-click on-click}
                "Change"])]]]))
      (subvec (vec (seq @color-atom))
              0
              (@cps-config :num-factors))))]
   [:div {:style {:display "flex"
                  :flex-wrap "wrap"
                  :gap 20}}
    (combinations)]])
