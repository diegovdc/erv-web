(ns wilson-tunings.mos2
  (:require [clojure.string :as str]
            [erv.mos.mos :as mos]
            [erv.mos.v2.submos :as submos]
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
           :mos/submos-data (submos/make-all-submos (int (state-map :mos/generator))
                                                    selected-mos))))

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
   (->> submos
        (map-indexed
         (fn [i {:keys [mos degree mos-degrees]}]
           [:div {:key i :class "wt__mos-secondary-data-main-data"}
            [:code
             (format "mode %s: [%s], degrees: [%s]"
                     degree
                     (str/join ", " mos)
                     (str/join ", " mos-degrees))]])))])

(defn get-submos-data [state-map]
  #_(state-map :mos/submos-data)
  (if (state-map :mos/remove-generator-1)
    (filter #(not= 1 (:generator %))
            (state-map :mos/submos-data))
    (state-map :mos/submos-data)))

(defn render-submos-data [state state-map]
  (let [pattern (@state :mos/selected-mos)
        submos-data (get-submos-data state-map)
        submos (filter :true-submos? submos-data)
        neighboring-submos (remove :true-submos? submos-data)
        nothing-to-see (fn [] [:small "nil"])]
    [:div {:class "wt__mos-secondary-data"}
     [:h4 (str "Viewing data for: ") (count pattern) ")" (apply + pattern)]
     [:small {:class "wt__mos-secondary-data-mos-description"}
      "Row: " (count pattern) ", MOS: " (str/join ", " pattern)]
     [:div
      [:div [:label "Hide MOS with generator 1"
             [:input
              {:type "checkbox"
               :checked (@state :mos/remove-generator-1)
               :on-click #(swap! state update :mos/remove-generator-1 not)}]]]]
     [:div {:class "wt__mos-secondary-data-main"}
      [:h3 "Secondary MOS"]
      (if (seq submos)
        (reverse (map-indexed (partial render-submos-data* @state) submos))
        (nothing-to-see))]]))

(defn main [state]
  [:div
   [:h1 {:class "wt__title wt__mos-title"} "Moments of symmetry calculator (V2)"]
   [:div {:class "wt__credits"}
    "This is an version of the MOS calculator that has been optimized for calculating secondary MOS for large MOS. Therefore it does not show traverse MOS because at present they are only found in a sort of brute force way. At the moment this version does not have buttons for switching between different kinds of representation of the secondary MOS, and so this it shows all modes for any given MOS."]
   [:div {:class "wt__credits"}
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
   (when (@state :mos/selected-mos)
     (render-submos-data state @state))])
