(ns wilson-tunings.cps
  (:require [clojure.string :as str]
            [erv.cps.core :as cps]
            [erv.scale.core :as scale]
            [erv.scale.scl :as scl]
            [erv.utils.conversions :refer [cps->midi]]
            [erv.utils.core :as utils]))

(defn parse-generators [generators]
  (-> generators (str/split #",")
      (->> (map (comp js/Number str/trim))
           (remove #(or (= % nil?)
                        (= % 0)
                        (js/Number.isNaN %)))
           sort)))

(defn make-tidal-scale [set-size generators period]
  (let [gens (parse-generators generators)
        scale (->> (cps/make set-size gens :period period :norm-gen (last gens)) :scale)
        freqs (map #(cps->midi (scale/deg->freq scale 10 %)) (range (count scale)))]
    (str "[" (->> (map #(utils/round2 2 (- % (first freqs))) freqs)
                  (str/join ","))
         "]")))
(
defn make-scala-file [set-size generators period]
  (let [gens (parse-generators generators)
        scale-data (cps/make set-size gens :period period
                             :norm-gen (last gens))]
    (scl/make-scl-file scale-data)))

(defn main [state]
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
              :type "number"
              :value (@state :set-size)
              :on-change #(swap! state assoc :set-size (-> % .-target .-value))}]]
    [:label [:span "Period" [:small " (harmonic at which the scale repeats - use 2 for octave"]]
     [:input {:style {:display "block"}
              :placeholder "2"
              :type "number"
              :value (@state :period)
              :min "1"
              :on-change #(swap! state assoc :period (-> % .-target .-value js/Number))}]]]
   [:h2 "Scale:"]
   (let [{:keys [generators set-size period]} @state]
     (if (and (> (count generators) 0)
              (> (count set-size) 0)
              (> period 1))
       (let [{:keys [content filename]} (make-scala-file set-size generators period)]
         [:div
          [:div [:h3 "MIDI tuning (for use with SuperCollider or Tidal Cycles)"]
           [:pre {:style {:background-color "lightgray"}} (make-tidal-scale set-size generators period) ]]
          [:div
           [:h3 "Scala file"]
           [:a {:href (str "data:text/plain;charset=utf-8," content)
                :download filename} (str "Download: " filename)]
           [:pre {:style {:background-color "lightgray"}} content]]])
       [:small "Incomplete input"]))])
