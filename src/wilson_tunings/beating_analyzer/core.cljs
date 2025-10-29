(ns wilson-tunings.beating-analyzer.core
  (:require
   [clojure.string :as str]
   [erv.beating-analyzer.v1 :as ba]
   [erv.cps.core :refer [format]]
   [erv.utils.exact :refer [parse-ratios]]
   [erv.utils.exact :as eu]
   [re-frame.core :as rf]
   [wilson-tunings.beating-analyzer.synths :as bas]
   [wilson-tunings.modal :refer [modal]]
   [wilson-tunings.synthesis :as wt.s]
   [wilson-tunings.utils :refer [parse-integers]]))

(def default-ratios "67/64
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
")

(def default-state
  {::ratios default-ratios
   ::partials [1 2 3 4 5]
   ::partial-amps [1 0.9 0.8 0.7 0.6]})

(rf/reg-sub
 ::beat-freqs
 (fn [db]
   (->> db
        ::beat-data
        (group-by #(select-keys % [:beat-freq.hz :beat-freq.factors]))
        (map (fn [[m vs]] (assoc m :instances (count vs))))
        (sort-by :beat-freq.hz))))

(rf/reg-sub
 ::filters
 (fn [db _]
   (::filters db)))

(rf/reg-sub
 ::parsed-ratios
 (fn [db _]
   (::parsed-ratios db)))

(rf/reg-sub
 ::synths
 (fn [db _]
   (::synths db)))

(rf/reg-sub
 ::ratios
 (fn [db _]    ;; :<---- trivial boilerplate we might want to skip over
   (::ratios db)))

(rf/reg-sub
 ::beat-data
 (fn [db _]    ;; :<---- trivial boilerplate we might want to skip over
   (::beat-data db)))

(rf/reg-sub
 ::periods
 :<- [::beat-data]
 (fn [beat-data]
   (sort (set (map :period beat-data)))))

(rf/reg-sub
 ::sorted-data-by-degree-pairs
 :<- [::beat-data]
 (fn [beat-data]
   (->> beat-data
        (group-by (comp (juxt :degree-1 :degree-2 :period)))
        (sort-by (comp (juxt #(nth % 2 nil) first second) first)))))

(rf/reg-sub
 ::partials
 (fn [db]
   (::partials db)))

(rf/reg-event-db
 ::set-partials
 (fn [db [_ data]]
   (println data)
   (assoc db ::partials data)))

(rf/reg-event-fx
 ::set-ratios
 (fn [{:keys [db]} ratios]
   {:db (assoc db ::ratios ratios)}))

(rf/reg-event-fx
 ::set-beat-data
 (fn [{:keys [db]}]
   (wt.s/init!)
   (let [parsed-ratios (parse-ratios (::ratios db))
         beat-data (map-indexed
                    #(assoc %2 ::id %1)
                    (ba/get-beat-data (eu/->exact 2)
                                      (eu/->exact 1)
                                      (map eu/->exact (::partials db))
                                      parsed-ratios))]
     {:db (assoc db
                 ::parsed-ratios parsed-ratios
                 ::beat-data beat-data)})))

(rf/reg-event-fx
 ::toggle-harmonic-pair-synth
 (fn
   [{:keys [db]} [_ {:as data-k
                     :keys [root-hz
                            ratio-1
                            ratio-1-partial
                            ratio-2
                            ratio-2-partial]}]]
   (let [data-k* (::id data-k)
         synths (get-in db [::synths data-k*])
         updated-db (if synths
                      (do
                        (doseq [synth synths] (bas/release synth))
                        (update db ::synths dissoc data-k*))

                      (let [env {:attack 102 :decay 5 :sustain 0.8 :release 102}
                            freq-1 (* root-hz
                                      (eu/->native ratio-1)
                                      (eu/->native ratio-1-partial))
                            freq-2 (* root-hz
                                      (eu/->native ratio-2)
                                      (eu/->native ratio-2-partial))
                            params {:env env, :db -32}
                            synths [(bas/play (bas/make-sine (assoc params :freq freq-1)))
                                    (bas/play (bas/make-sine (assoc params :freq freq-2)))]]
                        (println freq-1 freq-2)

                        (update db ::synths assoc data-k* synths)))]
     {:db updated-db})))
(rf/reg-sub
 ::partial-amps
 (fn [db]
   (::partial-amps db)))
(rf/reg-event-fx
 ::toggle-degree-pair-synth
 (fn [{:keys [db]}
      [_ id {:as _data
             :keys [root-hz
                    ratio-1
                    ratio-2]}]]
   (let [data-k* id
         synths (get-in db [::synths data-k*])
         updated-db (if synths
                      (do
                        (doseq [synth synths] (bas/release synth))
                        (update db ::synths dissoc data-k*))

                      (let [env {:attack 102 :decay 5 :sustain 0.8 :release 102}
                            freq-1 (* root-hz (eu/->native ratio-1))
                            freq-2 (* root-hz (eu/->native ratio-2))
                            params {:partial-amps @(rf/subscribe [::partial-amps])
                                    :env env
                                    :db -32}
                            synths [(bas/play (bas/make-additive (assoc params :freq freq-1)))
                                    (bas/play (bas/make-additive (assoc params :freq freq-2)))]]
                        (println freq-1 freq-2)

                        (update db ::synths assoc data-k* synths)))]
     {:db updated-db})))

(rf/reg-event-fx
 ::update-filter
 (fn [{:keys [db]} [_ filter-key filter-data]]
   {:db (update-in db [::filters filter-key] merge filter-data)}))

(defn factors->hiccup
  "Outputs hiccup with factors in power notation"
  [factors]
  (if-not (seq factors)
    [:span 1]
    (->> (frequencies factors)
         (sort-by first)
         (map (fn [[factor power]] [:span
                                    {:key (str (eu/make-readable factors) (eu/make-readable factor) power)}
                                    (eu/make-readable factor)
                                    [:sup power]])))))

(defn beat-factors-hiccup
  [{:keys [numer denom]}]
  [:span  (factors->hiccup numer) "/" (factors->hiccup denom)])

(defn update-all-filters [source-data filter-key on?]
  (rf/dispatch [::update-filter filter-key
                (->> source-data
                     (map (fn [data] [data  on?]))
                     (into {}))]))

(defn beat-freqs-filter
  []
  (let [beat-freqs @(rf/subscribe [::beat-freqs])
        beat-hz (map :beat-freq.hz beat-freqs)
        filters @(rf/subscribe [::filters])]
    [:div
     [:h3 "Beat Frequencies filter"]
     [:p "Note: no filters are selected by default. To populate the table select something. Using \"Select All\" can slow down the page."]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
      [:button
       {:on-click (fn [] (update-all-filters beat-hz :beat-freqs false))}
       "Deselect All"]
      [:button
       {:on-click (fn [] (update-all-filters beat-hz :beat-freqs true))}
       "Select All"]]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
      (map (fn [bf]
             (let [id (:beat-freq.hz bf)]

               [:label {:key id
                        :style {:width 200
                                :flex-shrink 0}}
                [:input {:type :checkbox
                         :checked (get-in filters [:beat-freqs id] false)
                         :on-change (fn [ev]
                                      (rf/dispatch [::update-filter
                                                    :beat-freqs
                                                    {id (-> ev .-target .-checked)}]))}]
                (:beat-freq.hz bf) "hz - "
                (beat-factors-hiccup  (:beat-freq.factors bf))
                " - (" (:instances bf) ")"]))
           beat-freqs)]]))

(defn degrees-filter
  []
  (let [ratios @(rf/subscribe [::parsed-ratios])
        degrees (range (count ratios))
        filters @(rf/subscribe [::filters])]
    [:div
     [:h3 "Degrees filter"]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
      [:button
       {:on-click (fn [] (update-all-filters degrees :degrees false))}
       "Deselect All"]
      [:button
       {:on-click (fn [] (update-all-filters degrees :degrees true))}
       "Select All"]]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
      (map (fn [degree]
             [:label {:key degree
                      :style {:width 40
                              :flex-shrink 0}}
              [:input {:type :checkbox
                       :checked (get-in filters [:degrees degree] true)
                       :on-change (fn [ev]
                                    (rf/dispatch [::update-filter
                                                  :degrees
                                                  {degree (-> ev .-target .-checked)}]))}]
              degree])
           degrees)]]))

(defn periods-filter
  []
  (let [periods @(rf/subscribe [::periods])
        filters @(rf/subscribe [::filters])]
    [:div
     [:h3 "Period filter"]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
      [:button
       {:on-click (fn [] (update-all-filters periods :periods false))}
       "Deselect All"]
      [:button
       {:on-click (fn [] (update-all-filters periods :periods true))}
       "Select All"]]
     [:div {:style {:display "flex" :gap 8 :flex-wrap "wrap"}}
      (map (fn [period]
             [:label {:key period
                      :style {:width 40
                              :flex-shrink 0}}
              [:input {:type :checkbox
                       :checked (get-in filters [:periods period] true)
                       :on-change (fn [ev]
                                    (rf/dispatch [::update-filter
                                                  :periods
                                                  {period (-> ev .-target .-checked)}]))}]
              period])
           periods)]]))

(defn thead
  []
  (let [beat-freqs @(rf/subscribe [::beat-freqs])
        filters @(rf/subscribe [::filters])]
    (into [:tr
           [:th "Period"]
           [:th "Degrees"]]
          (keep (fn [bf]
                  (let [hz (:beat-freq.hz bf)]
                    (when-not (false? (get-in filters [:beat-freqs hz]))
                      [:th {:key hz} (str hz "hz " "(" (:instances bf) ")")
                       [:br]
                       (beat-factors-hiccup  (:beat-freq.factors bf))])))
                beat-freqs))))
(rf/reg-sub
 ::beat-freqs-by-hz
 :<- [::beat-freqs]
 (fn [beat-freqs]
   (reduce
    (fn [acc {hz :beat-freq.hz
              :as data}]
      (assoc acc hz data))
    {}
    beat-freqs)))
(comment
  @(rf/subscribe [::beat-freqs-by-hz])
  @(rf/subscribe [::thead-data]))
(declare playing-synth-notifier)
(rf/reg-sub
 ::thead-data
 :<- [::beat-freqs-by-hz]
 :<- [::non-empty-columns]
 :<- [::beat-freqs-active]
 (fn [[beat-freqs-by-hz columns-hz beat-freqs-active]]
   {:period-label "Period"
    :degrees-label "Degrees"
    :hz-columns (->> columns-hz sort
                     (map (fn [hz]
                            (let [bf (beat-freqs-by-hz hz)]
                              [:th {:key hz :position "relative"}
                               (str hz "hz " "(" (:instances bf) ")")
                               [:br]
                               (beat-factors-hiccup  (:beat-freq.factors bf))
                               (when (beat-freqs-active hz)
                                 (playing-synth-notifier))]))))})
 #_(let [beat-freqs @(rf/subscribe [::beat-freqs])
         filters @(rf/subscribe [::filters])]
     (into [:tr
            {} ;; do not remove this, even if unstyled, is necessary for the column filtering to take place in the `table` component. Otherwise the vector size will differ from that of the rows.
            [:th "Period"]
            [:th "Degrees"]]
           (keep (fn [bf]
                   (let [hz (:beat-freq.hz bf)]
                     (when-not (false? (get-in filters [:beat-freqs hz]))
                       [:th {:key hz} hz "hz " "(" (:instances bf) ")"
                        [:br]
                        (beat-factors-hiccup  (:beat-freq.factors bf))])))
                 beat-freqs))))

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

(comment
  (let [e (with-meta [] {:empty? true})
        f [1]]
    (non-empty-columns [[e f e e]
                        [f f e e]])))

(defn keep-row?
  [degrees-filter periods-filter  deg-pair-k]
  (and (not (false? (get degrees-filter (first deg-pair-k))))
       (not (false? (get degrees-filter (second deg-pair-k))))
       (not (false? (get periods-filter (nth deg-pair-k 2))))))

(defn make-beat-cell [data-by-hz hz]
  (let [data (data-by-hz hz)
        is-empty? (nil? (seq data))
        synths @(rf/subscribe [::synths])
        id (str (str/join (eu/make-readable data)) hz)]
    (with-meta
      [:td {:key id
            :style {:white-space "nowrap"}}
       (->> data
            (map (fn [{:as pair-data
                       :keys [ratio-1-partial ratio-2-partial]}]
                   (let [id (::id pair-data)]
                     [:span {:key id
                             :style {:border "1px solid orange"
                                     :cursor "pointer"
                                     :background-color (if (get synths id) "cyan" "transparent")}
                             :on-click (fn [] (rf/dispatch [::toggle-harmonic-pair-synth pair-data]))}
                      (str (eu/make-readable ratio-1-partial)
                           ","
                           (eu/make-readable ratio-2-partial))]))))]
      {:empty? is-empty?})))

(comment
  @(rf/subscribe [::beat-freqs]))

(defn hidden-column?
  [beat-freqs-filter hz]
  (not (beat-freqs-filter hz)))

(defn row-not-empty?
  [beat-cells]
  (some #(not (:empty? (meta %))) beat-cells))

(defn maybe-make-row
  [beat-freqs-filter beat-freq-columns [deg-pair pair-beat-data]]
  (let [data-by-hz (group-by :beat-freq.hz pair-beat-data)
        ;; the beating harmonic pair at the given beating frequency
        beat-cells (into [] (comp (remove #(hidden-column? beat-freqs-filter (:beat-freq.hz %)))
                                  (map #(make-beat-cell data-by-hz (:beat-freq.hz %))))
                         beat-freq-columns)
        root-label (str (nth deg-pair 2) "@" (:root-hz (first pair-beat-data)) "hz")
        synths @(rf/subscribe [::synths])
        {:keys [degree-1 ratio-1 degree-2 ratio-2]} (first pair-beat-data)
        deg-pair-label (format "%s(%s),%s(%s)"
                               degree-1 (eu/make-readable ratio-1)
                               degree-2 (eu/make-readable ratio-2))
        synth-id [:degree-pair (::id (first pair-beat-data))]]
    (when (row-not-empty? beat-cells)
      (into [:tr {:key (str root-label "-" deg-pair-label)}
             [:td root-label]
             [:td
              [:span {:style {:border "1px solid orange"
                              :cursor "pointer"
                              :background-color (if (get synths synth-id) "cyan" "transparent")}
                      :on-click (fn []
                                  (rf/dispatch
                                   [::toggle-degree-pair-synth
                                    synth-id
                                    (first pair-beat-data)]))}
               deg-pair-label]]]
            beat-cells))))

(defn make-beat-cell-data
  [synths data-by-hz hz]
  (let [data (data-by-hz hz)
        is-empty? (nil? (seq data))
        id (str (str/join (eu/make-readable data)) hz)]
    {:id id
     :beat-hz hz
     :is-empty? is-empty?
     :pairs (->> data
                 (map (fn [{:as pair-data
                            :keys [ratio-1-partial ratio-2-partial]}]
                        {:id (::id pair-data)
                         :pair-data pair-data
                         :playing? (get synths id)
                         :label (str (eu/make-readable ratio-1-partial)
                                     ","
                                     (eu/make-readable ratio-2-partial))})))}))

(defn row-not-empty?2
  [beat-cells]
  (some #(not (:is-empty? %)) beat-cells))
#_(row-not-empty?2 [{:id "1.875", :is-empty? true, :pairs ()}])

(defn maybe-make-row-data
  [beat-freqs-filter beat-freq-columns synths [deg-pair pair-beat-data]]
  (let [data-by-hz (group-by :beat-freq.hz pair-beat-data)
        ;; the beating harmonic pair at the given beating frequency
        beat-cells (into [] (comp (remove #(hidden-column? beat-freqs-filter (:beat-freq.hz %)))
                                  (map #(make-beat-cell-data synths data-by-hz (:beat-freq.hz %))))
                         beat-freq-columns)
        root-label (str (nth deg-pair 2) "@" (:root-hz (first pair-beat-data)) "hz")
        {:keys [degree-1 ratio-1 degree-2 ratio-2]} (first pair-beat-data)
        deg-pair-label (format "%s,%s (%s,%s)"
                               degree-1 degree-2
                               (eu/make-readable ratio-1)
                               (eu/make-readable ratio-2))
        synth-id [:degree-pair (::id (first pair-beat-data))]]
    (when (row-not-empty?2 beat-cells)
      {:row-key (str root-label "-" deg-pair-label)
       :root-label root-label
       :synth-id synth-id
       :beat-cells beat-cells
       :deg-pair-label deg-pair-label
       :pair-beat-data pair-beat-data})))

(rf/reg-sub
 ::rows-data
 :<- [::sorted-data-by-degree-pairs]
 :<- [::beat-freqs]
 :<- [::filters]
 :<- [::synths]
 (fn [[data-by-deg-pairs beat-freq-column-data filters synths] _]
   (let [beat-freqs-filter (:beat-freqs filters {})
         degrees-filter (:degrees filters {})
         periods-filter (:periods filters {})]
     (->> data-by-deg-pairs
          (filter #(keep-row? degrees-filter periods-filter (first %)))
           ;; NOTE `maybe-make-row` may return `nil` when all cells are empty (i.e. can occur when filtering columns)
          (keep (fn [kv-data] (maybe-make-row-data beat-freqs-filter beat-freq-column-data synths kv-data)))))))
#_(defn non-empty-columns [rows]
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

(rf/reg-sub
 ::non-empty-columns
 :<- [::rows-data]
 (fn [rows-data]
   (->> rows-data
        (mapcat :beat-cells)
        (mapcat :pairs)
        (map (comp :beat-freq.hz :pair-data))
        set)))

(comment
  (->> @(rf/subscribe [::rows-data])
       (mapcat :beat-cells)
       (mapcat :pairs)
       first)
  @(rf/subscribe [::non-empty-columns]))

(defn table
  []
  (let [data-by-deg-pairs @(rf/subscribe [::sorted-data-by-degree-pairs]) #_(get-sorted-data-by-degree-pairs beat-data)
        beat-freq-column-data @(rf/subscribe [::beat-freqs]) #_(get-beat-freqs beat-data)
        filters @(rf/subscribe [::filters])
        beat-freqs-filter (:beat-freqs filters {})
        degrees-filter (:degrees filters {})
        periods-filter (:periods filters {})
        rows (->> data-by-deg-pairs
                  (filter #(keep-row? degrees-filter periods-filter (first %)))
                  ;; NOTE `maybe-make-row` may return `nil` when all cells are empty (i.e. can occur when filtering columns)
                  (keep (fn [kv-data] (maybe-make-row beat-freqs-filter beat-freq-column-data kv-data))))
        header (thead)
        non-empty-cols (non-empty-columns rows)
        header* (mapv #(nth header %) non-empty-cols)
        rows* (map (fn [row] (mapv #(nth row %) non-empty-cols)) rows)]
    (when (seq header*)
      [:table {:class "beating-analyzer__table"}
       [:thead header*]
       [:tbody rows*]])))

(declare get-degree-pair-id)

(defn playing-synth-notifier
  []
  [:span {:style {:display "block"
                  :height 8
                  :width 8
                  :background-color "red"
                  :position "absolute"
                  :top 0
                  :right 0}}])

(defn thead2
  [thead-data]
  (into [:tr
         {} ;; do not remove this, even if unstyled, is necessary for the column filtering to take place in the `table` component. Otherwise the vector size will differ from that of the rows.
         [:th (:period-label thead-data)]
         [:th (:degrees-label thead-data)]]
        (:hz-columns thead-data)))

(defn make-cell
  [synths
   non-empty-columns
   {:keys [id beat-hz pairs]}]
  (when (non-empty-columns beat-hz)
    [:td {:key id
          :style {:white-space "nowrap"}}
     (->> pairs
          (map (fn [{:keys [id label pair-data]}]
                 [:span {:key id
                         :style {:border "1px solid orange"
                                 :cursor "pointer"
                                 :background-color (if (get synths id) "cyan" "transparent")}
                         :on-click (fn [] (rf/dispatch [::toggle-harmonic-pair-synth pair-data]))}
                  label])))]))
(defn make-rows
  [synths
   non-empty-columns
   degree-pairs-active
   {:keys [row-key
           root-label
           synth-id
           pair-beat-data
           deg-pair-label
           beat-cells]}]
  (let [pair-data (first pair-beat-data)]
    (into [:tr {:key row-key}
           [:td root-label]
           [:td
            {:position "relative"}
            [:span {:style {:border "1px solid orange"
                            :cursor "pointer"
                            :background-color (if (get synths synth-id) "cyan" "transparent")}
                    :on-click (fn []
                                (rf/dispatch
                                 [::toggle-degree-pair-synth
                                  synth-id
                                  pair-data]))}
             deg-pair-label]
            (when (degree-pairs-active (get-degree-pair-id pair-data))
              (playing-synth-notifier))]]
          (keep #(make-cell synths non-empty-columns %) beat-cells))))

(defn table2
  []
  (let [thead-data @(rf/subscribe [::thead-data])
        synths @(rf/subscribe [::synths])
        rows-data @(rf/subscribe [::rows-data])
        non-empty-columns @(rf/subscribe [::non-empty-columns])
        degree-pairs-active @(rf/subscribe [::degree-pairs-active])]
    [:table {:class "beating-analyzer__table"}
     [:thead (thead2 thead-data)]
     [:tbody (map #(make-rows synths non-empty-columns degree-pairs-active %) rows-data)]]))

(defn ratios-input []
  (let [ratios @(rf/subscribe [::ratios])
        partials @(rf/subscribe [::partials])]
    [:div
     [:div [:label "Scale"]]
     [:textarea {:rows 19
                 :value ratios
                 :on-change (fn [ev]
                              (rf/dispatch [::set-ratios (-> ev .-target .-value)]))}]
     [:div [:label
            [:b "Partials to consider: "]
            [:input {:type "text"
                     :defaultValue (str/join ", " partials)
                     :on-blur (fn [ev]
                                (rf/dispatch [::set-partials (-> ev .-target .-value parse-integers)]))}]]]
     [:div
      [:button {:on-click (fn [] (rf/dispatch [::set-beat-data]))}
       "Analyze Scale Beating"]]]))

(rf/reg-fx
 ::release-synths
 (fn [[synths]]
   (doseq [s synths]
     (bas/release s))))

(rf/reg-event-fx
 ::stop-all-synths
 (fn [{:keys [db]}]
   {::release-synths [(-> db ::synths vals flatten)]
    :db (dissoc db ::synths)}))

(rf/reg-fx
 ::update-synths-partial-amp
 (fn [[partial amp synths]]
   (doseq [s synths]
     (bas/set-partial-amp s (inc partial) amp))))

(rf/reg-event-fx
 ::set-default-partial-amp
 (fn [{:keys [db]} [_ partial amp synths]]
   {:db (assoc-in db [::partial-amps partial] amp)
    ::update-synths-partial-amp [partial amp synths]}))

(rf/reg-sub
 ::additive-synths
 (fn [db]
   (->> db ::synths vals flatten (filter bas/additive-synth?))))

(rf/reg-sub
 ::beat-data-pairs-by-id
 (fn [db]
   (->> db ::beat-data
        (reduce (fn [acc pair]
                  (assoc acc (::id pair) pair))
                {}))))
(comment
  @(rf/subscribe [::beat-data-pairs-by-id]))
(rf/reg-sub
 ::beat-freqs-active
 :<- [::beat-data-pairs-by-id]
 :<- [::synths]
 (fn [[beat-data-pairs-by-id synths]]
   (->> synths keys (map (comp :beat-freq.hz beat-data-pairs-by-id))
        set)))

(def get-degree-pair-id (juxt :degree-1 :degree-2 :period))
(rf/reg-sub
 ::degree-pairs-active
 :<- [::beat-data-pairs-by-id]
 :<- [::synths]
 (fn [[beat-data-pairs-by-id synths]]
   (->> synths keys
        (map #(-> (if (vector? %) (second %) %)
                  beat-data-pairs-by-id
                  get-degree-pair-id))
        set)))

(comment
  @(rf/subscribe [::degree-pairs-active])
  @(rf/subscribe [::beat-freqs-active])
  @(rf/subscribe [::synths])
  @(rf/subscribe [::additive-synths])

  @(rf/subscribe [::partial-amps]))

(rf/reg-event-fx
 ::update-total-partial-amps
 (fn [{:keys [db]} [_ new-total-partials]]
   (let [partial-amps (::partial-amps db)
         total-partials (count partial-amps)
         diff (int (- new-total-partials total-partials))
         new-partial-amps (cond
                            (zero? diff) partial-amps
                            (pos? diff) (into partial-amps (repeat diff 0))
                            :else (into [] (take (+ total-partials diff) partial-amps)))]

     {:db (assoc db ::partial-amps new-partial-amps)})))

(defn synth-controls []
  (let [partial-amps @(rf/subscribe [::partial-amps])
        synths @(rf/subscribe [::additive-synths])
        partials @(rf/subscribe [::partials])]
    [:div
     [:h3 "Synth Partials " [:small "Only applies when playing a pair of degrees (not the beating partials)"]]
     [:label "Total Partials: "
      [:input {:type "number"
               :step 1
               :min 1
               :default-value (count partial-amps)
               :on-change (fn [ev]
                            (rf/dispatch [::update-total-partial-amps
                                          (-> ev .-target .-valueAsNumber)]))}]
      [:small " Only new instances will be affected."]]
     [:div {:style {:display "flex"}}
      (map-indexed (fn [i amp]
                     (let [partial (inc i)]
                       [:label {:style {:display "flex" :flex-direction "column" :align-items "center"}
                                :key i}
                        [:input {:type "range"
                                 :min 0
                                 :max 1
                                 :default-value amp
                                 :step 0.01
                                 :style {:writing-mode "vertical-lr" :direction "rtl"}
                                 :on-change (fn [ev]
                                              (rf/dispatch [::set-default-partial-amp
                                                            i
                                                            (-> ev .-target .-valueAsNumber)
                                                            synths]))}]
                        partial]))
                   partial-amps)]
     [:p "Note: the partials considered in table are " (str/join ", " partials)]]))

(defn beating-synth-controls
  []
  (modal true (fn [])
         [:div {:style {:display "flex" :flex-direction "column"}}
          [:label [:span  "Amp 1"] [:input {:type "range"
                                            :min 0
                                            :max 1
                                            :default-value 1
                                            :step 0.01
                                            :on-change (fn [ev]
                                                         (println "setting amp")
                                                         #_(rf/dispatch [::set-default-partial-amp
                                                                         i
                                                                         (-> ev .-target .-valueAsNumber)
                                                                         synths]))}]]
          [:label "Pan 1" [:input {:type "range"
                                   :min 0
                                   :max 1
                                   :default-value 1
                                   :step 0.01
                                   :on-change (fn [ev]
                                                (println "setting amp")
                                                #_(rf/dispatch [::set-default-partial-amp
                                                                i
                                                                (-> ev .-target .-valueAsNumber)
                                                                synths]))}]]
          [:label "LFO 1 Depth" [:input {:type "range"
                                         :min 0
                                         :max 1
                                         :default-value 1
                                         :step 0.01
                                         :on-change (fn [ev]
                                                      (println "setting amp")
                                                      #_(rf/dispatch [::set-default-partial-amp
                                                                      i
                                                                      (-> ev .-target .-valueAsNumber)
                                                                      synths]))}]]
          [:label "LFO 1 Freq" [:select
                                [:option 1]
                                [:option 2]]]
          [:label "Amp 2" [:input {:type "range"
                                   :min 0
                                   :max 1
                                   :default-value 1
                                   :step 0.01
                                   :on-change (fn [ev]
                                                (println "setting amp")
                                                #_(rf/dispatch [::set-default-partial-amp
                                                                i
                                                                (-> ev .-target .-valueAsNumber)
                                                                synths]))}]]
          [:label "Pan 2" [:input {:type "range"
                                   :min 0
                                   :max 1
                                   :default-value 1
                                   :step 0.01
                                   :on-change (fn [ev]
                                                (println "setting amp")
                                                #_(rf/dispatch [::set-default-partial-amp
                                                                i
                                                                (-> ev .-target .-valueAsNumber)
                                                                synths]))}]]
          [:label "LFO 2 Depth" [:input {:type "range"
                                         :min 0
                                         :max 1
                                         :default-value 1
                                         :step 0.01
                                         :on-change (fn [ev]
                                                      (println "setting amp")
                                                      #_(rf/dispatch [::set-default-partial-amp
                                                                      i
                                                                      (-> ev .-target .-valueAsNumber)
                                                                      synths]))}]]]))

(defn main []
  (let [beat-data @(rf/subscribe [::beat-data]) #_(@state :beat-data)]

    [:div [:h1 "Beating Analyzer"]
     (beating-synth-controls)
     [:style

      "

"
      #_#_"
      .beating-analyzer__table thead th:nth-child(1),
     .beating-analyzer__table tbody td:nth-child(1) {
         position: sticky;
         left: 0;
         background: #fff;
         z-index: 1;
         width: var(--first-col-width);
         min-width: 101px;
border-right: 1px solid red;
     }

     .beating-analyzer__table thead th:nth-child(2),
     .beating-analyzer__table tbody td:nth-child(2) {
         position: sticky;
         left: 101px;
         background: #fff;
         z-index: 1;
         width: var(--second-col-width);
         min-width: var(--second-col-width);
     }
"

        "
      .beating-analyzer__table thead th {
                                         position: sticky ; /* Makes the header cells sticky */
                                         top: 0           ;
                                         background: #fff ; /* Prevents content from showing through */
                                         z-index: ;       /* Ensures the header stays above the scrolling rows */
                                         }

 .beating-analyzer__table thead th:nth-child(1),
 .beating-analyzer__table thead th:nth-child(2){
z-index: 2;
}

"]

     (ratios-input)
     (when (seq beat-data)
       [:div {:style {:margin-bottom 16}}
        (beat-freqs-filter)
        (degrees-filter)
        (periods-filter)
        (synth-controls)
        [:div [:button {:on-click (fn [] (rf/dispatch [::stop-all-synths]))}  "Stop all synths"]]
        #_(table)
        (table2)])
     #_[:div
        [:p
         [:button {:on-click (fn [] (swap! state update :page dec))} "Previous Page"]
         (:page @state)
         [:button {:on-click (fn [] (swap! state update :page inc))} "Next Page"]]
        [:p (count (:beat-data @state))]
        [:p (count (:beat-data @state))]]]))
