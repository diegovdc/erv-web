(ns wilson-tunings.cps
  (:require ["tone/build/esm/index" :as Tone]
            [clojure.math.combinatorics :as combo]
            [clojure.string :as str]
            [com.gfredericks.exact :as e]
            [erv.cps.core :as cps]
            [erv.scale.core :as scale]
            [erv.scale.scl :as scl]
            [erv.utils.conversions :as conv :refer [cps->midi]]
            [erv.utils.core :as utils]
            [reagent.core :as r]
            [time-time.dynacan.players.gen-poly :as gp]
            [wilson-tunings.state :refer [state]]
            [clojure.set :as set]))

(defn parse-factors [factors]
  (-> factors (str/split #",")
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

(defn get-scale-data [set-size factors period]
  (let [gens (parse-factors factors)]
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
(do
  (defn factors-btns [{:keys [scale meta]}]
    (let [size (meta :cps/size)
          factors (meta :cps/factors)
          scale* (->> scale (map-indexed (fn [i deg] (assoc deg :degree i))))]
      (->> (range size)
           (mapcat #(combo/combinations factors %))
           (filter seq)
           (map (fn [factors*]
                  [factors*
                   (->> scale*
                        (filter (fn [deg]
                                  (empty? (set/difference (set factors*) (:set deg)))))
                        (map :degree))]))

           (map (fn [[factors degrees]]
                  [:button
                   {:key factors
                    :on-click #(set-degrees! degrees)}
                   (str "{" (str/join ", " factors) "}")]))
           ((fn [btns] (conj btns [:button {:key "all"
                                           :on-click #(set-degrees! (range 0 (count scale)))}
                                  "all"])))))
    )
  (factors-btns (cps/make 3 [1 3 4 5 6 7])))


(defn get-rand-degree [scale selected-degrees current-deg shared-factors]
  (let [current (utils/wrap-at current-deg scale)
        options (->> selected-degrees
                     (remove #(= % current-deg))
                     (map #(assoc (utils/wrap-at % scale) :degree %))
                     (filter #(<= shared-factors
                                  (count (set/intersection (:set %)
                                                           (:set current))))))]
    (or (:degree (rand-nth options))
        current-deg)))

(comment
  (let [scale (->> @state :cps/cps-scale :scale)
        degrees (->> @player :degrees parse-degrees)
        current-deg (->> @player :current-degree)
        shared-factors 2]
    (get-rand-note scale degrees current-deg shared-factors))
  )

(def synth (.toDestination (Tone/Synth.)))

(defn start-playing []
  (let [scale (-> @state :cps/cps-scale :scale)
        degrees (parse-degrees (@player :degrees))
        durs (parse-durs (@player :durs))
        fundamental (@player :fundamental 200)
        tempo (int (@player :tempo 90))
        min-shared-factors (@player :min-shared-factors 0)]
    (if (and scale (seq degrees))
      (do (Tone/start)
          (gp/ref-rain
           :id ::cps-demo
           :tempo tempo
           :durs durs
           :on-event (fn [ev]
                       (let [index (-> ev :data :index)
                             current-deg (@player :current-degree (rand-nth degrees))
                                        ; deg (utils/wrap-at index degrees)
                             deg (if (= (@player :mode) :rand)
                                   (get-rand-degree scale
                                                    degrees
                                                    current-deg
                                                    min-shared-factors)
                                   (utils/wrap-at index degrees))
                             durs (-> ev :data :durs)
                             dur (utils/wrap-at index durs)
                             freq (scale/deg->freq scale fundamental deg)]
                         (swap! player assoc
                                :current-degree deg
                                :current-note (utils/wrap-at deg
                                                             scale))
                         (.triggerAttackRelease synth
                                                freq
                                                dur
                                                js/undefined
                                                0.3))))
          (swap! player assoc :playing? true))
      (js/console.error "No scale")))

  (js/console.debug "start-playing"))

(comment
  (start-playing))
(defn demo [state]
  (let [scale-data (:cps/cps-scale @state)
        scale (:scale scale-data)
        size (-> scale-data :meta :cps/size)]
    [:div
     [:h2 "Try this CPS:"]
     [:div [:label "Degrees: "
            [:input
             {:on-change #(swap! player assoc :degrees (-> % .-target .-value))
              :value (@player :degrees)}]]]
     [:div [:div"Set factor chord" [:small " (Select all the degrees that share the same set of factors) "] ":"]
      (factors-btns scale-data)]
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
            [:small " Comma separated list of numbers. 1 = quarter note, 0.5 = eighth note, etc.  This will loop."]]]
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
     (when (= :rand (@player :mode))
       [:div
        [:label "Max different factors: "
         [:select {:value (- size (@player :min-shared-factors))
                   :on-change
                   (fn [ev] (-> ev .-target .-value int
                               (->> (- size)
                                    (swap! player assoc :min-shared-factors))))}
          (map (fn [val] [:option {:key val :value val} val])
               (range 1 (inc size)))]]])
     [:button {:style {:background-color "#696" :color "white"}
               :on-click start-playing}
      (if (@player :playing?) "Update" "Start playing")]
     (when (@player :playing?)
       [:button
        {:style {:background-color "#FF6666" :color "white"}
         :on-click #(do (gp/stop) (swap! player assoc :playing? false))}
        "Stop"])
     (when (@player :playing?)
       [:div "Current note: set {"
        (str (->> @player :current-note :set (str/join ", ")))
        "}, degree " (@player :current-degree)])]))
(comment
  (-> @player))
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
           [:a {:href (str "data:text/plain;charset=utf-8," (js/encodeURIComponent content))
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
    [:label [:span "Factors" [:small " (comma separated numbers, prefably prime or co-prime)"]]
     [:input {:style {:display "block"}
              :placeholder "1,3,5,7"
              :value (@state :factors)
              :on-change #(swap! state assoc :factors (-> % .-target .-value))}]]
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
     {:on-click #(let [{:keys [factors set-size period]} @state]
                   (js/console.log @state)
                   (.preventDefault %)
                   (when (and (> (count factors) 0)
                              (> (count set-size) 0)
                              (> period 1))
                     (let [scale-data (get-scale-data set-size
                                                      factors
                                                      period)]
                       (swap! state assoc :cps/cps-scale scale-data)
                       (swap! player assoc :min-shared-factors 0))))}
     "Generate CPS"]]
   (show-scale-data state)])
