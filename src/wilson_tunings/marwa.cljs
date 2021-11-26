(ns wilson-tunings.marwa
  (:require [erv.marwa.core :as marwa]
            [erv.edo.core :as edo]
            [erv.scale.core :as scale]
            [clojure.string :as str]))

#_(scale/demo! (:scale (edo/from-pattern [2 2 1 2 2 2 1])))
#_(marwa/get-possible-interval-sequences)

(defn parse-scale [input-value]
  (-> input-value
      (str/split ",")
      (->> (map (comp js/Number str/trim) ))))

(def scale-input-id "marwa-input-scale")
(defn scale-input [state]
  [:div
   [:h2 "Input a MOS scale"]
   [:small "Note: if the scale is not a MOS, you may get unisons in your permutations (intervals of size 0)" ]
   [:div
    [:label "Scale: "
     [:input {:id scale-input-id
              :defaultValue "2,2,1,2,2,2,1"
              :placeholder "scale i.e. 2,2,1,2,2,2,1"}]]
    [:button
     {:on-click
      (fn [_]
        (let [scale (->
                     (js/document.getElementById scale-input-id)
                     .-value
                     parse-scale)]
          (swap! state assoc
                 :marwa/input-scale scale
                 :marwa/possible-generator-sequences
                 (marwa/get-possible-generator-sequences scale))))}
     "Calculate generators"]]])

(defn generator-sequences [state]
  (if-let [seqs (filter :best-sequence? (@state :marwa/possible-generator-sequences))]
    [:div
     [:h2 "Choose a generator sequence for the permutations"]
     [:table
      [:thead
       [:tr
        [:th
         [:div "Scale Generator"]
         [:small (count (@state :marwa/input-scale)) "-tone generator"]]
        [:th
         [:div "Generator List"]
         [:small "Counting all " (apply + (@state :marwa/input-scale))
          " tones of the scale"]]
        [:th]]]
      [:tbody
       (map
        (fn [{:keys [generator sequence best-sequence?] :as data}]
          [:tr
           {:key sequence
            :style {:font-weight
                    (if best-sequence? "bold" "normal")}}
           [:td generator]
           [:td (str sequence)]
           [:td [:button {:on-click
                          #(swap! state assoc
                                  :marwa/selected-generator-sequence-data
                                  data)}
                 "Permutate"]]])
        seqs)]]]))

(do
  (defn original-generator-sequence [generator-freqs]
    (->> generator-freqs (sort-by second >)
         (mapcat (fn [[gen freq]] (repeat freq gen)))))
  (original-generator-sequence {7 1, 6 5}))

(defn demo-button [scale]
  [:button
   {:on-click
    (fn [_]
      (scale/demo!
       (:scale (edo/from-pattern scale)))) }
   "Demo"])


(defn permutations [state]
  (let [input-scale (@state :marwa/input-scale)
        remove-unisons? (@state :marwa/remove-unisons?)
        scale-size (apply + input-scale)
        {:keys [sequence
                generator-freqs
                sorted-generators-by-high-freq]
         :as data}
        (@state :marwa/selected-generator-sequence-data {})

        ;; TODO fix this, for generator lists that are not `best-sequence?`
        [gen-interval closing-interval] sorted-generators-by-high-freq
        base-perms (marwa/base-permutations (count sequence)
                                            gen-interval
                                            closing-interval)
        scale-rotation (marwa/intervals->scale-2 scale-size sequence)
        rotated? (not= input-scale scale-rotation)
        perms* (marwa/mos-permutations scale-size base-perms)
        perms (if remove-unisons?
                (remove #(some zero? (:scale %)) perms*)
                perms*)]
    (println remove-unisons?)
    (when sequence
      [:div
       [:h2 "Permutations" [:small " (" (count perms) ")"]]
       [:div [:label "Remove unisons: "
              [:input {:type "checkbox"
                       :checked (@state :marwa/remove-unisons?)
                       :on-change #(swap! state assoc
                                          :marwa/remove-unisons?
                                          (-> % .-target .-checked))}]]]
       (when rotated?
         [:div "Warning: The scale has been rotated to "
          (str scale-rotation)])
       [:table
        [:thead
         [:tr [:th "Scale"] [:th "Generator List"] [:th] [:th]]]
        [:tbody
         (concat
          [[:tr {:key "original"}
            [:td (str scale-rotation)]
            [:td (str sequence)]
            [:td (demo-button scale-rotation)]
            [:td (when rotated? "Original scale (rotated)")]]]
          (map (fn [{:keys [scale generator-seq]}]
                 [:tr {:key generator-seq}
                  [:td (str scale)]
                  [:td (str generator-seq)]
                  [:td (demo-button scale)]
                  [:td]])
               perms))]]])))

(defn main
  [state]
  (println (:marwa/input-scale @state) @state)
  [:div
   [:h1 "Marwa permutations"]
   (scale-input state)
   (generator-sequences state)
   (permutations state)])


(comment
  (require '[wilson-tunings.state :refer [state]])
  (-> @wilson-tunings.state/state
      :marwa/input-scale
      marwa/get-possible-generator-sequences)
  )
