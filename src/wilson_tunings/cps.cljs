(ns wilson-tunings.cps
  (:require
   [com.gfredericks.exact :as e]
   [clojure.string :as str]
   [erv.cps.core :as cps]
   [erv.scale.core :as scale]
   [erv.scale.scl :as scl]
   [erv.utils.conversions :refer [cps->midi]]
   [erv.utils.core :as utils]
   [erv.utils.conversions :as conv]))

(defn parse-generators [generators]
  (-> generators (str/split #",")
      (->> (map (comp js/Number str/trim))
           (remove #(or (= % nil?)
                        (= % 0)
                        (js/Number.isNaN %)))
           sort)))

(defn get-scale-data [set-size generators period]
  (let [gens (parse-generators generators)]
    (cps/make (int set-size) gens :period period
              :norm-gen (->> gens (take-last (int set-size)) (apply *)))))

(defn make-tidal-scale [scale-data]
  (let [scale (scale-data :scale)
        freqs (map (comp conv/ratio->cents :bounded-ratio) scale)]
    (str "[" (->> (map #(utils/round2 2 (- % (first freqs))) freqs)
                  (str/join ","))
         "]")))
(:scale (cps/make 3 [1 3 5 7]))

(defn make-scala-file [scale-data] (scl/make-scl-file scale-data))

(def ONE (e/native->integer 1))
(defn float->ratio [num]
  (let [total-decimals (-> (str num)
                           (str/split ".")
                           second
                           count)
        tens (Math/pow 10 total-decimals)]
    (e// (e/native->integer (* num tens)) (e/native->integer tens))))

(defn to-e-number [num]
  (if (= (int num) num)
    (e/native->integer num)
    (float->ratio num)))


(defn within-bounding-period
  "Transposes a ratio withing a bounding-period.
  The octave is a `bounding-period` of 2,the tritave of 3, etc."
  [bounding-period ratio]
  {:pre [(> bounding-period 1)]}
  (let [bounding-period (to-e-number bounding-period)
        ratio (to-e-number ratio)
        ratio*  (loop [r ratio]
                  (cond
                    (e/> r bounding-period) (recur (e// r bounding-period))
                    (e/< r ONE) (recur (e/* r bounding-period))
                    (= bounding-period r) ONE
                    :else r))]
    (if (e/ratio? ratio*)
      (str (e/integer->string (e/numerator ratio*)) "/"
           (e/integer->string (e/denominator ratio*)))
      (str (e/integer->string ratio*) "/1"))))

(do
  (defn scale-table [scale]
    [:table {:border 1}
     [:thead
      [:tr
       [:th "Degree"]
       [:th "Cents"]
       [:th "Ratio"]
       [:th "Factors"]]]
     [:tbody
      (map-indexed
       (fn [i {:keys [set ratio bounded-ratio bounding-period]}]
         [:tr {:key set}
          [:td (inc i)]
          [:td (utils/round2 4 (conv/ratio->cents bounded-ratio))]
          [:td (str (within-bounding-period bounding-period (apply * set)))]
          [:td (str/join ", " set)]
          ]) scale)]])
  (scale-table (:scale (cps/make 3 [1 3 5 7]))))

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
       (let [scale-data (get-scale-data set-size generators period)
             {:keys [content filename]} (make-scala-file scale-data)]
         [:div
          (scale-table (:scale scale-data))
          [:div [:h3 "MIDI tuning (for use with SuperCollider or Tidal Cycles)"]
           [:pre {:style {:background-color "lightgray"}}
            (make-tidal-scale scale-data) ]]
          [:div
           [:h3 "Scala file"]
           [:a {:href (str "data:text/plain;charset=utf-8," content)
                :download filename} (str "Download: " filename)]
           [:pre {:style {:background-color "lightgray"}} content]]])
       [:small "Incomplete input"]))])
