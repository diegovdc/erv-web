(ns wilson-tunings.constant-structure-analyzer.core
  #_(:require
     [clojure.edn :as edn]
     [clojure.string :as str]
     [erv.constant-structures.graphics :as csg]
     [erv.cps.core :as cps]
     [erv.utils.conversions :as convo]
     [wilson-tunings.modal :refer [modal]]
     [quil.core :as q :include-macros true]
     [reagent.core :as r]))

#_(defn parse-cents
    [cents-string]
    (convo/cents->ratio cents-string))

#_(defn parse-ratio [ratio-string]
    (-> ratio-string
        str/trim
        (str/split #"/")
        (->> (map edn/read-string)
             (apply /))))

#_(defn parse-number
    [num-string]
    (if (str/includes? num-string "/")
      (parse-ratio num-string)
      (parse-cents num-string)))

#_(defn remove-comments [strings]
    (js/console.log "rm" strings)
    (remove
     #(str/starts-with? % ";")
     strings))

#_(defn parse-numbers
    [ratios-string]
    (println "pn" ratios-string)
    (->> (str/split ratios-string #"\n")
         (map str/trim)
         remove-comments
         (map parse-number)))

#_(def scale-ratios (r/atom (str/replace "1 80/77 12/11 8/7 96/77 10/7 16/11 120/77 12/7 20/11"
                                         #" "
                                         "\n")))
#_(def added-notes (r/atom "11/9
11/8
;; remove the \";;\" from line below to add an 8/5 note
;; 8/5"))

#_(defonce show-instructions? (r/atom false))

#_(defn regenerate-sketch*
    [scale-ratios added-notes]
    (println (parse-numbers added-notes))
    (csg/make-state (map (fn [n] {:bounded-ratio n}) (parse-numbers scale-ratios))
                    (parse-numbers added-notes)))

#_(def sketch-state (atom (regenerate-sketch* @scale-ratios @added-notes)))

#_(defn regenerate-sketch
    []
    (reset! sketch-state
            (regenerate-sketch* @scale-ratios @added-notes)))

#_(defn canvas []
    (r/create-class
     {:component-did-mount (fn []
                             (q/defsketch cs-tool
                               :title "CS Tool"
                               :host "cs-analyzer-canvas"
                               :settings #(q/smooth 80)
                               :setup csg/setup
                               :draw (csg/draw sketch-state)
                               :size [csg/width csg/height]))
      :reagent-render
      (fn []
        [:div {:id "cs-analyzer-canvas"}])}))

(def instructions
  [:div [:p "This tools allows you to visualize if a scale is a constant structure or not and figure out which notes to add to convert your scale into one. When a scale is not a constant structure red arcs will appear in the scale circle. Usually you will want to add notes to bissect some of these arcs. Added notes will appear as green radius lines. When all arcs disappear then you have a constant structure."]
   [:p "Notes can be specified as ratios (i.e. 3/2) or as cents (i.e. 700)."]
   [:p "All notes should be written in a new line. And you can remove a note or add a comment by prefacing a line with a \";\" symbol."]])

(defn main
  []
  [:div
   [:h1 "Constant Structure Analyzer"]
   #_[:button {:on-click
               (fn [] (reset! show-instructions? true))}
      "Instructions"]
   #_(modal @show-instructions?
            (fn [] (reset! show-instructions? false))
            instructions)
   #_[:div {:style {:display "flex"
                    :gap 20}}
      [:div
       [:label "Scale Ratios"]
       [:div
        [:textarea {:style {:width 100}
                    :rows 12
                    :value @scale-ratios
                    :on-change (fn [ev]
                                 (reset! scale-ratios (-> ev .-target .-value)))}]]
       [:button {:on-click regenerate-sketch} "Regenerate Graph"]]
      [:div
       [:label "Added Notes"]
       [:div
        [:textarea {:style {:width 400}
                    :rows 12
                    :value @added-notes
                    :on-change (fn [ev]
                                 (println ev)
                                 (reset! added-notes (-> ev .-target .-value)))}]]]
      [canvas]]])
