(ns wilson-tunings.cps
  (:require [clojure.string :as str]
            [erv.cps.core :as cps]
            [erv.scale.core :as scale]
            [erv.utils.conversions :refer [cps->midi]]
            [erv.utils.core :as utils]))

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
         "]")))

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
              :value (@state :set-size)
              :on-change #(swap! state assoc :set-size (-> % .-target .-value))}]]]
   [:h2 "Output scale (in midi, for an octave):"]
   (let [{:keys [generators set-size]} @state]
     (if (and (> (count generators) 0)
              (> (count set-size) 0))
       [:code {:style {:background-color "lightgray"}} (make-tidal-scale generators set-size)]
       [:small "Incomplete input"]))])
