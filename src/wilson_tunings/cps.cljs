(ns wilson-tunings.cps
  (:require ["tone/build/esm/index" :as Tone]
            [clojure.string :as str]
            [com.gfredericks.exact :as e]
            [erv.cps.core :as cps]
            [erv.scale.core :as scale]
            [erv.scale.scl :as scl]
            [erv.utils.conversions :as conv :refer [cps->midi]]
            [erv.utils.core :as utils]
            [wilson-tunings.state :refer [state]]
            [reagent.core :as r]
            [time-time.dynacan.players.gen-poly :as gp]))

(defn parse-generators [generators]
  (-> generators (str/split #",")
      (->> (map (comp js/Number str/trim))
           (remove #(or (= % nil?)
                        (= % 0)
                        (js/Number.isNaN %)))
           sort)))
(defn parse-degrees [degrees]
  (-> degrees (str/split #",")
      (->> (map (comp js/Number str/trim))
           (remove #(or (= % nil?)
                        (js/Number.isNaN %))))))
(defn parse-durs [durs]
  (let [durs (-> durs (str/split #",")
                 (->> (map (comp js/Number str/trim))
                      (remove #(or (= % nil?)
                                   (= % 0)
                                   (js/Number.isNaN %)))))]
    (or (seq durs) [1])))

(defn get-scale-data [set-size generators period]
  (let [gens (parse-generators generators)]
    (cps/make (int set-size) gens
              :period period
              :norm-fac (->> gens
                             (take (int set-size))
                             (apply *)))))

(defn make-tidal-scale [scale-data]
  (let [scale (scale-data :scale)
        freqs (map (comp conv/ratio->cents :bounded-ratio) scale)]
    (str "[" (->> (map #(utils/round2 2 (- % (first freqs))) freqs)
                  (str/join ","))
         "]")))

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
  [bounding-period ratio norm-fac]
  #_{:pre [(> bounding-period 1)]}

  (let [bounding-period (to-e-number bounding-period)
        ratio (e// (to-e-number ratio)
                   (to-e-number norm-fac))
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

(defn scale-table [{:keys [scale meta]}]
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
        [:td i]
        [:td (utils/round2 4 (conv/ratio->cents bounded-ratio))]
        [:td (str (within-bounding-period bounding-period
                                          (apply * set)
                                          (meta :cps/normalized-by)))]
        [:td (str/join ", " set)]])
     scale)]])



(defonce player (r/atom {:mode :seq :durs "1, 0.5" :degrees "0, 1, 2"}))

(defn set-degrees! [degrees] (swap! player assoc :degrees (str/join ", " (sort degrees))))

(defn factors-btns [scale]
  (let [degrees-per-factor
        (->> scale
             (map-indexed (fn [i deg] (assoc deg :degree i)))
             (reduce
              (fn [acc deg]
                (reduce (fn [acc factor] (update acc factor conj (:degree deg)))
                        acc
                        (:set deg)))
              {}))
        factors (sort (keys degrees-per-factor))]
    (map (fn [fac]
           [:button
            {:key fac
             :on-click #(set-degrees! (degrees-per-factor fac))}
            fac])
         factors))
  )
(def synth (.toDestination (Tone/Synth.)))

(defn start-playing []
  (let [scale (-> @state :cps/cps-scale :scale)
        degrees (parse-degrees (@player :degrees))
        durs (parse-durs (@player :durs))]
    (if (and scale (seq degrees))
      (do (Tone/start)
          (gp/ref-rain
           :id ::cps-demo
           :tempo (int (@player :tempo 90))
           :durs durs
           :on-event (fn [ev]
                       (let [index (-> ev :data :index)
                                        ; deg (utils/wrap-at index degrees)
                             deg (if (= (@player :mode) :rand)
                                   (rand-nth degrees)
                                   (utils/wrap-at index degrees))
                             durs (-> ev :data :durs)
                             dur (utils/wrap-at index durs)
                             freq (scale/deg->freq scale (@player :fundamental 200) deg)]
                         (js/console.log freq dur)
                         (.triggerAttackRelease synth
                                                freq
                                                dur
                                                js/undefined
                                                0.5))))
          (swap! player assoc :playing? true))
      (js/console.error "No scale")))

  (js/console.debug "start-playing"))

(defn demo [state]
  (let [scale-data (:cps/cps-scale @state)
        scale (:scale scale-data)]
    [:div
     [:h2 "Try this CPS:"]
     [:div [:label "Degrees: "
            [:input
             {:on-change #(swap! player assoc :degrees (-> % .-target .-value))
              :value (@player :degrees)}]]]
     [:div "Set factor chord: " (factors-btns scale)]
     [:div [:label "Base frequency:  "
            [:input
             {:type "number"
              :min "20"
              :on-change #(swap! player assoc :fundamental
                                 (-> % .-target .-value js/Number))
              :value (@player :fundamental "200")}]
            [:small " Frequency of the 0th degree."]]]
     [:div [:label "Tempo: "
            [:input
             {:type "number"
              :min "33"
              :on-change #(swap! player assoc :tempo
                                 (-> % .-target .-value js/Number))
              :value (@player :tempo "90")}]]]
     [:div [:label "Durs: "
            [:input
             {:on-change #(swap! player assoc :durs (-> % .-target .-value))
              :value (@player :durs)}]
            [:small " Comma separated list of numbers. 1 = quarter note, 0.5 = eight note, etc.  This will loop."]]]
     [:div "Arpeggiation Mode: "
      [:label {:style {:margin-left 10 :margin-right 20}}
       "Sequential"
       [:input {:type "radio" :name "mode" :value :seq
                :checked (= :seq (@player :mode))
                :on-change #(swap! player assoc :mode :seq)}]]
      [:label "Random"
       [:input {:type "radio" :name "mode" :value :rand
                :checked (= :rand (@player :mode))
                :on-change #(swap! player assoc :mode :rand)}]]]
     [:button {:style {:background-color "#696" :color "white"}
               :on-click #(start-playing)}
      (if (@player :playing?) "Update" "Start playing")]
     (when (@player :playing?)
       [:button
        {:style {:background-color "#FF6666" :color "white"}
         :on-click #(do (gp/stop) (swap! player assoc :playing? false))}
        "Stop"])]))

(defn show-scale-data [state]
  [:div
   (let [scale-data (:cps/cps-scale @state)]
     (if scale-data
       (let [{:keys [content filename]} (make-scala-file scale-data)]
         [:div
          [:div {:style {:display "flex" :gap 20}}
           [:div [:h2 "Scale:"] (scale-table scale-data)]
           (demo state)]
          [:div [:h3 "MIDI tuning (for use with SuperCollider or Tidal Cycles)"]
           [:pre {:style {:background-color "lightgray"}}
            (make-tidal-scale scale-data) ]]
          [:div
           [:h3 "Scala file"]
           [:a {:href (str "data:text/plain;charset=utf-8," content)
                :download filename} (str "Download: " filename)]
           [:pre {:style {:background-color "lightgray"}} content]]])
       [:small "Incomplete input. Click on the \"Generate\" button or check that the input data makes sense."]))])


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
              :on-change #(swap! state assoc :period (-> % .-target .-value js/Number))}]]
    [:button
     {:on-click #(let [{:keys [generators set-size period]} @state]
                   (.preventDefault %)
                   (when (and (> (count generators) 0)
                              (> (count set-size) 0)
                              (> period 1))
                     (swap! state assoc :cps/cps-scale
                            (get-scale-data set-size generators period))))}
     "Generate CPS"]]
   (show-scale-data state)])
