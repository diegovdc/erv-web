(ns wilson-tunings.mos
  (:require [clojure.string :as str]
            [erv.mos.mos :as mos]
            [erv.mos.submos :as submos]
            [goog.string :refer [format]]))

(defn calculate-mos [state]
  (let [state-map @state]
    (swap! state assoc :mos/mos
           (mos/make-mos (int (state-map :mos/period))
                         (int (state-map :mos/generator))))))

(defn calculate-sub-mos [i state]
  (let [state-map @state
        selected-mos (nth (state-map :mos/mos) i)]
    (swap! state assoc
           :mos/selected-mos selected-mos
           :mos/submos-data (submos/make-all-submos selected-mos
                                                    (int (state-map :mos/generator))
                                                    (state-map :mos/mos)))))

(defn render-mos-table [state mos]
  (let [unit (->> mos first (apply +) (/ 500) Math/round)]
    (map-indexed
     (fn [i intervals]
       [:table {:key i}
        [:thead [:tr [:td] [:td]]]
        [:tbody
         [:tr
          [:td {:class "wt__mos-table-interval"}
           (map-indexed
            (fn [i interval]
              [:span {:key i
                      :style {:display "inline-block"
                              :width (* unit interval)
                              :border-bottom "1px solid black"}}
               [:span {:style {:border-left "1px solid black"
                               :padding-left 3}}
                interval]])
            intervals)]
          [:td [:span {:style {:display "inline-block"
                               :text-align "center"
                               :width 20}}
                (count intervals)]
           [:button {:class "wt__mos-table-generate-secondary-mos-button"
                     :on-click #(calculate-sub-mos i state)}
            "Generate Secondary MOS"]]]]])
     mos)))

(defn order-submos-using-generator-strategy
  "Attemps to order the submos modes with the generator
   by using the `mos/get-mos-points` function"
  [generator submos]
  (let [submos-size (count submos)
        indexes (drop-last 1 (mos/get-mos-points submos-size  generator))
        res (map #(nth submos %) indexes)]
    (if (= (count res) submos-size) res submos)))


(defn render-submos-data*
  [state-map i {:keys [pattern generator submos submos-by-mos period]}]
  [:div {:key i :class "wt__mos-secondary-data-detail"}
   [:div
    [:div [:b {:class "wt__mos-secondary-data-detail-description"}
           (str (count pattern) ")" generator "/" period)]]
    [:small {:class "wt__mos-secondary-data-detail-explanation"}
     "Generator: " generator ", "]
    [:small {:class "wt__mos-secondary-data-detail-explanation"}
     "MOS: [" (str/join ", " pattern) "]"]]
   (case (state-map :mos/submos-representation-mode)
     :all-modes (->> submos
                     (order-submos-using-generator-strategy generator)
                     (map-indexed
                      (fn [i {:keys [mos degree mos-degrees]}]
                        [:div {:key i :class "wt__mos-secondary-data-main-data"}
                         [:code
                          (format "mode %s: [%s], degrees: [%s]"
                                  degree
                                  (str/join ", " mos)
                                  (str/join ", " mos-degrees))]])))
     :unique-mos (map (fn [[mos group]]
                        (let [zero-rotation
                              (->> group
                                   (filter #(-> % :rotation :at-zero?))
                                   first
                                   :rotation
                                   :mos)]
                          [:div {:key mos
                                 :class "wt__mos-secondary-data-main-data wt__mos-secondary-data-main-data--unique"}
                           [:code (when-not zero-rotation {:style {:color "red"}})
                            (format "[%s]"
                                    (str/join "," (or zero-rotation mos)))
                            (when-not zero-rotation
                              [:small " this secondary MOS has no rotation to the first degree"])]]))
                      submos-by-mos)
     nil)])

(defn render-submos-data [state state-map]
  (let [ pattern (@state :mos/selected-mos)
        submos (filter :true-submos? (state-map :mos/submos-data))
        neighboring-submos (remove :true-submos? (state-map :mos/submos-data))
        nothing-to-see (fn [] [:small "nil"])]
    [:div {:class "wt__mos-secondary-data"}
     [:h4 (str "Viewing data for: ") (count pattern) ")" (apply + pattern)]
     [:small {:class "wt__mos-secondary-data-mos-description" }
      "Row: " (count pattern) ", MOS: " (str/join ", " pattern) ]
     [:div [:button {:class (str "wt__mos-secondary-data-button "
                                 (when (= :all-modes (state-map :mos/submos-representation-mode))
                                   "selected"))
                     :on-click #(swap! state assoc
                                       :mos/submos-representation-mode :all-modes)
                     :style (when (= :all-modes (state-map :mos/submos-representation-mode))
                              {:background-color "#ffc107"}) }
            "Show modes"]
      [:button {:class (str "wt__mos-secondary-data-button "
                            (when (= :unique-mos (state-map :mos/submos-representation-mode))
                              "selected"))
                :on-click #(swap! state assoc
                                  :mos/submos-representation-mode :unique-mos)}
       "Show unique secondary MOS" [:small " (rotated to the first degree of the MOS)"]]]
     [:div {:class "wt__mos-secondary-data-main"}
      [:h3 "Secondary MOS"]
      (if (seq submos)
        (map-indexed (partial render-submos-data* @state) submos)
        (nothing-to-see))
      [:h3 "Traverse MOS"]
      (if (seq neighboring-submos)
        (map-indexed (partial render-submos-data* @state) neighboring-submos)
        (nothing-to-see))]]))

(defn main [state]
  [:div
   [:h1 {:class "wt__title wt__mos-title"} "Moments of symmetry calculator"]
   [:div {:class "wt__credits" }
    "Developed by: " [:a {:href "https://echoic.space"} "Diego Villaseñor"] " with guidance by Kraig Grady and Billy Stiltner. "
    [:a {:href "https://github.com/diegovdc/erv-web"} "Source code"]]
   [:div {:class "wt__mos-form"}
    [:label [:span "Period" [:small " (determines the number of degrees in the scale)"]]
     [:input {:class "wt__mos-form-input"
              :placeholder "12"
              :value (@state :mos/period)
              :on-change #(swap! state assoc :mos/period (-> % .-target .-value js/Number))}]]
    [:label [:span "Generator"]
     [:input {:class "wt__mos-form-input"
              :placeholder "4"
              :value (@state :mos/generator)
              :on-change #(swap! state assoc :mos/generator (-> % .-target .-value js/Number))}]]
    [:button {:class "wt__mos-form-button"
              :style {:margin-top 5}
              :on-click #(calculate-mos state)} "Calculate"]]
   (when (@state :mos/mos)
     (render-mos-table state (@state :mos/mos)))
   (when (@state :mos/submos-data)
     (render-submos-data state @state))])
