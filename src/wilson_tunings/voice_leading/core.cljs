(ns wilson-tunings.voice-leading.core
  (:require
   [clojure.string :as str]
   [erv.edo.voice-leading :refer [find-closest-chord]]
   [reagent.core :as r]
   [taoensso.timbre :as timbre]))

(def ^:private local-state ;; just being quick and dirty
  (r/atom {:scale-size 12
           :chord [0 4 7]
           :target-chord [0 3 7]}))

(-> "0 4 5 "
    str/trim
    (str/split " ")
    (->> (mapv #(when-not (= "" %) (js/Number %)))
         (remove nil?)))
(->> @local-state)
(defn form
  []
  [:div
   [:div
    [:label "Scale Size"
     [:input {:type "number"
              :defaultValue 12
              :on-change #(swap! local-state assoc :scale-size (-> % .-target .-value js/Number))}]]]
   [:div [:label "Initial Chord"
          [:input {:type "text"
                   :defaultValue "0 4 7"
                   :on-change #(swap! local-state assoc :chord (-> % .-target
                                                                   .-value
                                                                   str/trim
                                                                   (str/split " ")
                                                                   (->> (mapv (fn [degree] (when-not (= "" degree) (js/Number degree))))
                                                                        (remove nil?))))}]]]
   [:div
    {:style {:margin-bottom 16}}
    [:label "Target Chord"

     [:input {:type "text"
              :defaultValue "0 3 7"
              :on-change #(swap! local-state assoc :target-chord (-> % .-target .-value str/trim (str/split " ")))}]]
    [:small "if the target chord has the intervals this can be left empty. Make sure that the target chord has the same amount of tones."]]
   [:button {:on-click (fn []
                         (let [{:keys [scale-size chord target-chord]} @local-state
                               voice-leading-data (->> (find-closest-chord scale-size
                                                                           chord
                                                                           (timbre/spy :info  (if (seq target-chord)
                                                                                                target-chord
                                                                                                chord)))
                                                       (take 15))]
                           (println "local-state" scale-size chord target-chord)
                           #_(println "voice-leading-data" voice-leading-data)
                           (swap! local-state assoc :voice-leading-data voice-leading-data)))}
    "Calculate Closest Chords"]])

(def ^:private tdhead-style {:style {:text-align "center"}})
(def ^:private td-style {:style {:text-align "center"}})

(defn format-chord [chord]
  (str "[" (str/join ", " chord) "]"))

(defn voice-leading-table
  [voice-leading-data]

  (when (seq voice-leading-data)
    [:table
     [:thead
      [:tr
       [:td "Initial Chord"]
       [:td "Target Chord"]
       [:td "Balance"]
       [:td "Voice Movement"]
       [:td "Initial Intervals"]
       [:td "Target Intervals"]]]
     (let [initial-chord* (:chord @local-state)]
       (->> voice-leading-data
            (remove (fn [{:keys [target-chord]}]
                      (= (seq target-chord) (seq initial-chord*))))
            (map (fn [{:keys [initial-chord
                              target-chord
                              total-movement
                              voice-movement
                              initial-intervals
                              target-intervals]}]
                   [:tbody {:key (concat initial-chord target-chord)}
                    [:tr {:style {:border-bottom "1px solid black"}}
                     [:td td-style (format-chord initial-chord)]
                     [:td td-style (format-chord target-chord)]
                     [:td td-style (str total-movement)]
                     [:td td-style (format-chord voice-movement)]
                     [:td td-style (format-chord initial-intervals)]
                     [:td td-style (format-chord target-intervals)]]]))))]))

(defn main []
  [:div
   [:h1 "Voice leading tool"]
   [:p "Based and inspired by Dmitri Tymozcko's \"A Geometry of Music\""]
   [:p [:small "See below for the description of the tool the instructions."]]
   [:div {:style {:margin-bottom 40}}
    [:div {:style {:margin-bottom 20}} (form)]

    (voice-leading-table (:voice-leading-data @local-state))]
   [:div
    [:h2 "Instructions"]
    [:p "To function this tool requires three parameters. A scale size (often the size of an EDO), the initial chord (in degrees) and the target chord (in degrees). The chords should be both have the same amount of notes, otherwise the results will not be correct."]
    [:p "Note that the target chord's root degree is actually unimportant, as the algorithm will return the  closest progression for that chord type sorted by the voice leading balance."]
    [:p "To build a chord progression simply choose a chord from the results and feed it back as the initial chord in the calculator."]
    [:p "To try out your chord progressions you can use " [:a {:href "https://luphoria.com/xenpaper/"} "Xenpaper"] ", which uses the same chord syntax as the target chords produced by this calculator."]]

   [:div
    [:h2 "Purpose a theory"]
    [:p "The followig algorithm will provide the 15 closest target chords to any chord. The closeness of one chord to another is judged by the sum of the movements of each individual voice (positive if the voice moves to a higher pitch, and negative if it moves to a lower pitch). According to Tymozcko balanced chord progressions, where the voice movment is near 0 leads to smooth voice leading and if often a desired characteristic in common practie music styles (obviously this is a matter of taste)."]
    [:p "For Tymozcko, a particular type of voice progression that is of interest is a progression where a chord leads to another chord of the same type. To facilitate finding such progressions one can leave the \"target chord\" field empty. Another chord progression of interest according to Tymozcko would be one where the intervals of a target chord are a permutation of the initial chord (i.e. in 12 edo a major chord (4 3 5) leading to a minor chord (3 4 5). So the user of this tool may find it interesting to try out such chords."]
    [:p "Lastly for Tymozcko the best progressions for a given EDO usually revolve around chords whose intervals are almost an equal division of such EDO. So for example, in 12EDO (4 4 4) divides the scale in three equal parts, this chords like (4 3 5) and (3 4 5) which are near the symmetrical division will work very well. This of course poses a problem of what to do with prime EDOs. As Tymozcko only deals with 12EDO, then the question remains open. Regardless of this, the tool will provide the most balanced options for a give chord pair."]]])
