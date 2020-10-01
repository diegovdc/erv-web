(ns browser.main
  (:require [reagent.core :as r]
            [reagent.dom :as dom]
            [erv.utils.core :as utils]
            [erv.utils.conversions :refer [cps->midi]]
            [erv.cps.core :as cps]
            [clojure.string :as str]
            [erv.scale.core :as scale]
            [clojure.spec.alpha :as s]))

                                        ; The output will be loadable through the `<script>` tag in any
                                        ; webpage, making it ideal for client-side code (e.g. React).
                                        ;
                                        ; More details at https://shadow-cljs.github.io/docs/UsersGuide.html#target-browser

(def state (r/atom {:generators "1,3,5,7"
                    :set-size "2"}))

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

(defn app []
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

(defn start []
  (dom/render [app]
              (. js/document (getElementById "app"))))
(comment
  (reset! app-state))

(defn ^:export init [opts]
  (println "Restarting")
  (start))

(defn stop [])

(println @state)
