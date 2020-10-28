(ns browser.main
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [erv.cps.core :as cps]
            [clojure.edn :as edn]
            [erv.mos.mos :as mos]
            [erv.mos.submos :as submos]
            [erv.scale.core :as scale]
            [erv.utils.conversions :refer [cps->midi]]
            [erv.utils.core :as utils]
            [reagent.core :as r]
            [reagent.dom :as dom]))

                                        ; The output will be loadable through the `<script>` tag in any
                                        ; webpage, making it ideal for client-side code (e.g. React).
                                        ;
                                        ; More details at https://shadow-cljs.github.io/docs/UsersGuide.html#target-browser

(defonce state (r/atom {:view :nil
                        :generators "1,3,5,7"
                        :set-size "2"
                        :mos/period 12
                        :mos/generator 5}))

(defn parse-generators [generators]
  (-> generators (str/split #",")
      (->> (map (comp js/Number str/trim))
           (remove #(or (= % nil?)
                        (= % 0)
                        (js/Number.isNaN %))))))

(defn make-tidal-scale [generators set-size]
  (let [gens (parse-generators generators)
        scale (->> gens
                   (cps/->cps (js/Number set-size))
                   cps/set->maps
                   (cps/bound-ratio 2)
                   (cps/maps->data :bounded-ratio)
                   :scale)
        freqs (map #(cps->midi (scale/deg->freq scale 10 %)) (range (count scale)))]
    (str "[" (->> (map #(utils/round2 2 (- % (first freqs))) freqs)
                  (str/join ","))
         "]")

    ))
(defn cps []
  [:div
   [:h1 "Combination Product Sets Maker"]
   [:p "A tool for creating scales inspired by Erv Wilson's theories. "
    [:a {:href "http://anaphoria.com/wilsoncps.html"} "http://anaphoria.com/wilsoncps.html"]]
   [:small "This is a work in progress, currenlty focusing on creating scales for use on Tidal Cycles."]
   [:form
    [:h2 "Input:"]
    [:label [:span "Generators" [:small " (comma separated numbers, prefably prime or co-prime)"]]
     [:input {:style {:display "block"}
              :placeholder "1,3,5,7"
              :value (@state :generators)
              :on-change #(swap! state assoc :generators (-> % .-target .-value))}]]
    [:label [:span "Set size" [:small " (determines the number of degrees in the scale)"]]
     [:input {:style {:display "block"}
              :placeholder "2"
              :value (@state :set-size)
              :on-change #(swap! state assoc :set-size (-> % .-target .-value))}]]]
   [:h2 "Output scale (in midi, for an octave):"]
   (let [{:keys [generators set-size]} @state]
     (if (and (> (count generators) 0)
              (> (count set-size) 0))
       [:code {:style {:background-color "lightgray"}} (make-tidal-scale generators set-size)]
       [:small "Incomplete input"]))])


(defn calculate-mos [state-map]
  (swap! state assoc :mos/mos
         (mos/make-mos (int (state-map :mos/period))
                       (int (state-map :mos/generator)))))

(defn calculate-sub-mos [i state-map]
  (println "submos" (state-map :mos/mos))
  (let [selected-mos (nth (state-map :mos/mos) i)]
  (println "selected sub" (state-map :mos/mos))
    (swap! state assoc
           :mos/selected-mos selected-mos
           :mos/submos-data (-> selected-mos
                                (submos/make-all-submos (state-map :mos/mos))))))

(defn render-mos-table [mos]
  (let [size (->> mos first (apply +))]
    (println "table" mos)
    (map-indexed (fn [i intervals]
                   [:table {:key i}
                    [:thead [:tr [:td] [:td]]]
                    [:tbody
                     [:tr
                      [:td {:style {:background-color "#eee"}}
                       (map-indexed (fn [i interval]
                                      [:span {:key i
                                              :style {:display "inline-block"
                                                      :width (* 500 (/ interval size))
                                                      :border-left "1px solid black"
                                                      :border-bottom "1px solid black"}}
                                       interval])
                                    intervals)]
                      [:td {:style {:display "inline-block"}}
                       (count intervals) [:button {:on-click #(calculate-sub-mos i @state)} "Generate Secondary MOS"]]]]])
                 mos)))

(defn render-submos-data [state-map]
  (println (state-map :mos/submos-data))
  (conj [:div
         (let [ pattern (@state :mos/selected-mos)]
           [:h4 "Viewing data for row: " (count pattern) ", pattern: " (str/join ", " pattern) ])]
        (map-indexed (fn [i {:keys [pattern generator submos]}]
                       [:div {:key i}
                        [:h5 {:style {:margin-bottom "3px"}}
                         "Based on pattern: [" (str/join ", " pattern) "], "
                         "generator: " generator]
                        (map-indexed (fn [i sm] [:div {:key i} (str/join ", " sm)]) submos)])
                     (state-map :mos/submos-data))))
(comment
  (@state :mos/submos-data)
  (@state :mos/selected-mos)
  (@state :mos/mos))

(defn mos []
  [:div
   [:h1 "Moments of symmetry"]
   [:label [:span "Period" [:small " (determines the number of degrees in the scale)"]]
    [:input {:style {:display "block"}
             :placeholder "12"
             :value (@state :mos/period)
             :on-change #(swap! state assoc :mos/period (-> % .-target .-value))}]]
   [:label [:span "Generator" [:small "An integer number"]]
    [:input {:style {:display "block"}
             :placeholder "4"
             :value (@state :mos/generator)
             :on-change #(swap! state assoc :mos/generator (-> % .-target .-value))}]]
   [:button {:on-click #(calculate-mos @state)} "Calculate"]
   (when (@state :mos/mos)
     (render-mos-table (@state :mos/mos)))
   (when (@state :mos/submos-data)
     (render-submos-data @state))])

(defn app []
  (case (@state :view)
    :cps (cps)
    :mos (mos)
    [:div [:h1 "Tools for exploring some of Erv Wilsons scale concepts"]
     [:p "What do you want to see?"]
     [:button {:on-click #(swap! state assoc :view :mos)} "Moments of symmetry calculator"]
     [:button {:on-click #(swap! state assoc :view :cps)} "CPS calculator"]]))

(defn start []
  (dom/render [app]
              (. js/document (getElementById "app"))))
(comment
  (reset! app-state))

(defn ^:export init [opts]
  (println "Restarting")
  (start))

(defn stop [])
