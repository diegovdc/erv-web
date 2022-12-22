(ns wilson-tunings.cps-colors
  (:require [goog.string :as gstr]
            [goog.string.format]
            ["react-color" :refer [ChromePicker]]
            ["color-blend" :as blend]
            ["rgb2hex" :as rgb2hex]
            [erv.cps.core :as cps]
            [reagent.core :as r]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn factor-names [total-factors]
  (map (comp keyword js/String.fromCharCode)
       (range 97 (+ 97 total-factors))))
(defonce color-atom (r/atom
                     (->> 26
                          factor-names
                          (map (fn [k] [k {"r" 0 "g" 0 "b" 0}]))
                          (into (sorted-map)))))

(comment
  (-> @color-atom))

(defonce selected-blend-mode (r/atom "multiply"))

(defn get-color-string [color]
  (gstr/format "rgba(%s, %s,  %s, %s)"
               (get color "r")
               (get color "g")
               (get color "b")
               (get color "a" 1)))

(defn rgb->hex [color]
  (rgb2hex (get-color-string color)))

(defn color-picker [id color]
  [:div
   [:> ChromePicker
    {:color (clj->js (get @color-atom id color))
     :on-change #(swap! color-atom assoc
                        id (-> % js->clj (get "rgb")))}]])

(defn square [color &
              {:keys [on-click]
               :or {on-click (fn [_] nil)}}]
  (when color
    [:div
     {:style {:height 50
              :width 50
              :background-color (get-color-string color)}
      :on-click on-click}]))

(defonce cps-config (r/atom {:size 1 :num-factors 1}))

(def pickers (r/atom {}))

(comment
  (-> @pickers)
  (swap! pickers update-in [:a :show?] (comp not boolean)))

(def blend-modes
  {"normal" (.-normal blend)
   "multiply" (.-multiply blend)
   "screen" (.-screen blend)
   "overlay" (.-overlay blend)
   "darken" (.-darken blend)
   "lighten" (.-lighten blend)
   "colorDodge" (.-colorDodge blend)
   "colorBurn" (.-colorBurn blend)
   "hardLight" (.-hardLight blend)
   "softLight" (.-softLight blend)
   "difference" (.-difference blend)
   "exclusion" (.-exclusion blend)
   "hue" (.-hue blend)
   "saturation" (.-saturation blend)
   "color" (.-color blend)
   "luminosity" (.-luminosity blend)})
(comment
  (blend-modes @selected-blend-mode))
(defn blend-colors [colors]
  (let [cs (map clj->js colors)]
    (js->clj (reduce
              (fn [c1 c2] ((blend-modes @selected-blend-mode) c1 c2))
              (first cs)
              (rest cs)))))
(reverse (cps/archi-subcps-sets (@cps-config :size) 6))

(defn combinations [factors combos]
  (let [groups (map sort (cps/->cps (dec (@cps-config :size)) factors))
        archi-sets (reverse (cps/archi-subcps-sets (@cps-config :size) (count factors)))
        product-squares (->> combos
                             (map
                              (fn [factors]
                                (let [color-data (->> factors
                                                      (select-keys @color-atom)
                                                      vals
                                                      blend-colors)]
                                  [(set factors)
                                   {:html
                                    [:div {:key factors :style {:margin-left 10}}
                                     [:h3 (map name factors)]
                                     (square color-data)]
                                    :color-data color-data}])))
                             (into {}))]
    (->> archi-sets
         (remove (fn [set]
                   (str/includes? (:name set)
                                  "1)1")))
         (map
          (fn [{:keys [name set]}]
            [:div {:key name :style {:display "flex"}}
             [:h2 name]
             (->> set
                  (map (fn [factors]
                         (:html (product-squares factors)))))])))))

(defn main []
  (let [factors
        (map first (take (@cps-config :num-factors) (sort-by first (seq @color-atom))))
        combos (map sort (cps/->cps (@cps-config :size) factors))
        color-data (map (fn [factors*]
                          [(set factors*)
                           (->> factors*
                                (select-keys @color-atom)
                                vals
                                blend-colors)])
                        combos)]
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
                                    (-> % .-target .-value js/Number))}]]]

      [:div
       [:lable "Color blend mode"]
       [:select {:value @selected-blend-mode
                 :on-change (fn [v] (reset! selected-blend-mode (.. v -target -value)))}
        (map (fn [[mode]]
               [:option {:key mode :value mode} mode])
             blend-modes)]]
      [:div
       [:button {:on-click (fn [_]
                             (swap! color-atom
                                    #(into {}
                                           (map (fn [[k _]]
                                                  [k {"r" (rand-int 256)
                                                      "g" (rand-int 256)
                                                      "b" (rand-int 256)
                                                      "a" 1}])
                                                %))))}
        "Randomize colors"]
       [:button {:on-click (fn [_]
                             (reset! selected-blend-mode
                                     (-> blend-modes keys rand-nth)))}
        "Randomize blend mode"]]]

     [:div {:style {:display "flex"
                    :flex-wrap "wrap"}}
      (doall
       (map
        (fn [[k color]]
          (let [on-click #(swap! pickers update-in [k :show?] (comp not boolean))]
            [:div {:key (name k)}
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
        (take (@cps-config :num-factors) (sort-by first (seq @color-atom)))))]
     [:div (combinations factors combos)]
     [:div (map (fn [[fs color-data]]
                  [:p (str fs " \"" (.-hex (rgb->hex color-data)) "\"")])
                color-data)]]))
