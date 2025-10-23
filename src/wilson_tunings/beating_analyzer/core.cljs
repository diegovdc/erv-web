(ns wilson-tunings.beating-analyzer.core
  (:require
   [clojure.string :as str]
   [erv.beating-analyzer.v1 :as ba]
   [erv.utils.exact :refer [parse-ratios]]
   [erv.utils.exact :as eu]
   [reagent.core :as r]))

(defonce state (r/atom {:ratios "67/64
 279/256
 9/8
 75/64
 39/32
 5/4
 167/128
 87/64
 45/32
 187/128
 3/2
 25/16
 417/256
 27/16
 7/4
 233/128
 15/8
 125/64
 2/1
"}))

(comment
  (-> @state
      (dissoc :beat-data)))

(defn factors->hiccup
  "Outputs hiccup with factors in power notation"
  [factors]
  (if-not (seq factors)
    [:span 1]
    (->> (frequencies factors)
         (sort-by first)
         (map (fn [[factor power]] [:span
                                    {:key (str factors (eu/make-readable factor) power)}
                                    (eu/make-readable factor)
                                    [:sup power]])))))

(defn beat-factors-hiccup
  [{:keys [numer denom]}]
  [:span  (factors->hiccup numer) "/" (factors->hiccup denom)])

(defn empty-cell [] [:span])

(def get-beat-freqs
  (memoize
   (fn [beat-data]
     (->> beat-data
          (group-by #(select-keys % [:beat-freq.hz :beat-freq.factors]))
          (map (fn [[m vs]] (assoc m :instances (count vs))))
          (sort-by :beat-freq.hz)))))

(defn beat-freqs-filter
  [beat-data]
  (let [beat-freqs (get-beat-freqs beat-data)
        state* @state]
    [:div
     [:h3 "Beat Frequencies filter"]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
      [:button
       {:on-click (fn []
                    (swap! state
                           update-in [:filters :beat-freqs] merge
                           (->> beat-freqs
                                (map (fn [data] [(data :beat-freq.hz) false]))
                                (into {}))))}
       "Deselect All"]
      [:button
       {:on-click (fn []
                    (swap! state
                           update-in [:filters :beat-freqs] merge
                           (->> beat-freqs
                                (map (fn [data] [(data :beat-freq.hz) true]))
                                (into {}))))}
       "Select All"]]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
      (map (fn [bf]
             (let [id (:beat-freq.hz bf)]

               [:label {:key id
                        :style {:width 200
                                :flex-shrink 0}}
                [:input {:type :checkbox
                         :checked (get-in state* [:filters :beat-freqs id] true)
                         :on-change (fn [ev]
                                      (swap! state
                                             assoc-in
                                             [:filters :beat-freqs id]
                                             (-> ev .-target .-checked)))}]
                (:beat-freq.hz bf) "hz - "
                (beat-factors-hiccup  (:beat-freq.factors bf))
                " - (" (:instances bf) ")"]))
           beat-freqs)]]))

(defn degrees-filter
  [ratios]
  (let [degrees (range (count ratios))
        state* @state]
    [:div
     [:h3 "Degrees filter"]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
      [:button
       {:on-click (fn []
                    (swap! state
                           update-in [:filters :degrees] merge
                           (->> degrees
                                (map (fn [degree] [degree false]))
                                (into {}))))}
       "Deselect All"]
      [:button
       {:on-click (fn []
                    (swap! state
                           update-in [:filters :degrees] merge
                           (->> degrees
                                (map (fn [degree] [degree true]))
                                (into {}))))}
       "Select All"]]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
      (map (fn [degree]
             [:label {:key degree
                      :style {:width 40
                              :flex-shrink 0}}
              [:input {:type :checkbox
                       :checked (get-in state* [:filters :degrees degree] true)
                       :on-change (fn [ev]
                                    (swap! state
                                           assoc-in
                                           [:filters :degrees degree]
                                           (-> ev .-target .-checked)))}]
              degree])
           degrees)]]))

(defn get-periods
  [beat-data]
  (sort (set (map :period beat-data))))

(defn periods-filter
  [beat-data]
  (let [periods (get-periods beat-data)
        state* @state]
    [:div
     [:h3 "Period filter"]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
      [:button
       {:on-click (fn []
                    (swap! state
                           update-in [:filters :periods] merge
                           (->> periods
                                (map (fn [period] [period false]))
                                (into {}))))}
       "Deselect All"]
      [:button
       {:on-click (fn []
                    (swap! state
                           update-in [:filters :periods] merge
                           (->> periods
                                (map (fn [period] [period true]))
                                (into {}))))}
       "Select All"]]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
      (map (fn [period]
             [:label {:key period
                      :style {:width 40
                              :flex-shrink 0}}
              [:input {:type :checkbox
                       :checked (get-in state* [:filters :periods period] true)
                       :on-change (fn [ev]
                                    (swap! state
                                           assoc-in
                                           [:filters :periods period]
                                           (-> ev .-target .-checked)))}]
              period])
           periods)]]))

(defn thead
  [beat-data]
  (let [beat-freqs (get-beat-freqs beat-data)
        state* @state]
    (into [:tr
           {} ;; do not remove this, even if unstyled, is necessary for the column filtering to take place in the `table` component. Otherwise the vector size will differ from that of the rows.
           [:th "Period"]
           [:th "Degrees"]]
          (keep (fn [bf]
                  (let [hz (:beat-freq.hz bf)]
                    (when-not (false? (get-in state* [:filters :beat-freqs hz]))
                      [:th {:key hz} hz "hz " "(" (:instances bf) ")"
                       [:br]
                       (beat-factors-hiccup  (:beat-freq.factors bf))])))
                beat-freqs))))
(comment
  (not (false? (get-in @state [:filters :degrees 0]))))

(do
  (defn non-empty-columns [rows]
    (let [col-indexes (->> rows
                           (drop-while nil?)
                           first
                           count
                           range)]
      (reduce
       (fn [acc i]
         (let [is-empty? (reduce
                          (fn [is-empty? row]
                            (let [td (nth row i)]
                              (and is-empty? (:empty? (meta td)))))
                          true
                          rows)]
           (if is-empty? acc (conj acc i))))
       []
       col-indexes)))
  (let [e (with-meta [] {:empty? true})
        f [1]]
    (non-empty-columns [[e f e e]
                        [f f e e]])))
(defn table
  [beat-data]
  (let [data-by-deg-pairs (group-by (comp (juxt :degree-1 :degree-2 :period)) beat-data)
        beat-freqs (get-beat-freqs beat-data)
        state* @state
        rows (->> data-by-deg-pairs
                  (sort-by (comp (juxt #(nth % 2 nil) first second) first))
                  (keep
                   (fn [[deg-pair pair-beat-data]]
                     (when (and (not (false? (get-in state* [:filters :degrees (first deg-pair)])))
                                (not (false? (get-in state* [:filters :degrees (second deg-pair)])))
                                (not (false? (get-in state* [:filters :periods (nth deg-pair 2)]))))
                       (let [data-by-hz (group-by :beat-freq.hz pair-beat-data)
                             beating-harmonics-at-beat-hz-index
                             (keep
                              (fn [{hz :beat-freq.hz}]
                                (when-not (false? (get-in state* [:filters :beat-freqs hz]))
                                  (let [content (->> (data-by-hz hz)
                                                     (map (juxt :ratio-1-partial :ratio-2-partial)))]
                                    (with-meta
                                      [:td {:id hz :key hz :style {:white-space "nowrap"}}
                                       (->> content
                                            (map #(str/join "," %))
                                            (interpose [:span {:style {:padding-left 10}}]))]
                                      {:empty? (nil? (seq content))}))))
                              beat-freqs)
                             root-label (str (nth deg-pair 2) "@" (:root-hz (first pair-beat-data)) "hz")
                             deg-pair-label (str/join "," (take 2 deg-pair))]
                         (when (and (some (fn [td] (not (:empty? (meta td))))
                                          beating-harmonics-at-beat-hz-index))
                           (into [:tr {:key (str root-label "-" deg-pair-label)}
                                  [:td root-label]
                                  [:td deg-pair-label]]
                                 beating-harmonics-at-beat-hz-index)))))))
        header (thead (@state :beat-data))
        non-empty-cols (non-empty-columns rows)
        header* (mapv #(nth header %) non-empty-cols)
        rows* (map (fn [row] (mapv #(nth row %) non-empty-cols)) rows)]
    (when (seq header*)
      [:table {:class "beating-analyzer__table"}
       [:thead header*]
       [:tbody rows*]])))

(comment

  (-> @state :beat-data get-beat-freqs eu/make-readable)
  (swap! state update :page inc)
  (count (ba/get-beat-data (eu/->exact 2)
                           (eu/->exact 1)
                           (map eu/->exact (range 1 7))
                           ratios))
  (js/console.log (clj->js ratios)))

(defn ratios-input []
  [:div
   [:div [:label "Scale"]]
   [:textarea {:rows 30
               :value (:ratios @state)
               :on-change (fn [ev]
                            (swap! state assoc :ratios
                                   (-> ev .-target .-value)))}]
   [:button {:on-click (fn []
                         (swap! state assoc :beat-data
                                (ba/get-beat-data (eu/->exact 2)
                                                  (eu/->exact 1)
                                                  (map eu/->exact (range 1 6))
                                                  (parse-ratios (:ratios @state)))))}
    "Analyze Scale Beating"]])

(defn main []
  (let [beat-data (@state :beat-data)]
    [:div [:h1 "Beating Analyzer"]
     (ratios-input)
     (when (seq beat-data)
       [:div {:style {:margin-bottom 16}}
        (beat-freqs-filter beat-data)
        (degrees-filter (parse-ratios (:ratios @state)))
        (periods-filter beat-data)])

     (table beat-data)
     #_[:div
        [:p
         [:button {:on-click (fn [] (swap! state update :page dec))} "Previous Page"]
         (:page @state)
         [:button {:on-click (fn [] (swap! state update :page inc))} "Next Page"]]
        [:p (count (:beat-data @state))]
        [:p (count (:beat-data @state))]]]))
