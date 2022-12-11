(ns wilson-tunings.cps.analysis
  (:require
   [clojure.edn :as edn]
   [clojure.set :as set]
   [clojure.string :as str]
   [erv.cps.core :as cps]
   [erv.cps.utils :refer [subcps-degrees]]
   [reagent.core :as r]
   [wilson-tunings.modal :refer [modal]]))

(defonce modal-data (r/atom nil))
(defonce factors-filter-set (r/atom #{}))
(defonce degrees-filter-set (r/atom #{}))

(defn find-supersets
  "Get all subcps that include all the given `sets`.
  Note that `sets` is a set of sets."
  [cps factors-set]
  (->> cps :subcps
       (filter (fn [[_ {:keys [scale]}]]
                 (= factors-set (set/intersection
                                 (set (map :set scale))
                                 factors-set))))))

(defn tidal-cps-name [cps-name]
  (-> cps-name
      (str/replace #"\)" "oo")
      (str/replace #" "  "x")
      (str/replace #"\."  "i")
      (str/replace #"-"  "t")))

(def cell-style {:style {:padding 10}})

(defn- subset-row
  [cps [subcps-name subcps]]
  (let [degrees (str/join ", " (:degrees subcps))]
    [:tr {:key subcps-name}
     [:td cell-style subcps-name]
     [:td cell-style degrees]
     [:td cell-style (tidal-cps-name subcps-name)]
     [:td cell-style [:button {:on-click
                               (fn []
                                 (reset! modal-data {:cps cps
                                                     :subcps-name subcps-name
                                                     :subcps subcps}))}
                      "Show Details"]]]))
(defn filter-factors
  "Remove sets from a `subcps-list` if they do not contain the required factors"
  [factors-set subcps-list]
  (if-not (seq factors-set)
    subcps-list
    (filter (fn [[_ {:keys [scale]}]]
              (= factors-set (set/intersection
                              (set (mapcat :set scale))
                              factors-set)))
            subcps-list)))

(defn filter-degrees
  "Remove sets from a `subcps-list` if they do not contain the required factors"
  [degrees-set subcps-list]
  (if-not (seq degrees-set)
    subcps-list
    (filter (fn [[_ {:keys [degrees]}]]
              (= degrees-set (set/intersection
                              (set degrees)
                              degrees-set)))
            subcps-list)))

(defn- add-degrees
  [cps [name* subcps]]
  [name*
   (assoc subcps :degrees (subcps-degrees cps subcps))])

(defn cps-table
  [cps subcps-list]
  (let [subset-rows (->> subcps-list
                         (map (partial add-degrees cps))
                         (filter-factors @factors-filter-set)
                         (filter-degrees @degrees-filter-set)
                         (map (partial subset-row cps)))]
    [:table
     [:thead
      [:tr
       [:th cell-style "Set Name"]
       [:th cell-style "Degrees"]
       [:th cell-style "Tidal Name"]
       [:th cell-style ""]]]
     [:tbody subset-rows]]))

(defn- main-subset-rows
  [cps]
  (->> cps :subcps
       (sort-by (juxt (comp count :scale second) first))
       reverse))

(defn- factor-filters-input
  []
  [:div
   [:label "Filter by containing factors "
    [:input {:placeholder "e.g. 1,7"
             :on-change (fn [ev]
                          (->> ev
                               .-target
                               .-value
                               (re-seq #"\d+")
                               (map edn/read-string)
                               set
                               (reset! factors-filter-set)))}]]])
(defn- degrees-filters-input
  []
  [:div
   [:label "Filter by containing degrees "
    [:input {:placeholder "e.g. 0,2,3"
             :on-change (fn [ev]
                          (->> ev
                               .-target
                               .-value
                               (re-seq #"\d+")
                               (map edn/read-string)
                               set
                               (reset! degrees-filter-set)))}]]])

(defn- make-subcps-details
  [{:keys [cps subcps-name subcps]}]
  (let [supersets  (->> subcps
                        :scale
                        (map :set)
                        set
                        (find-supersets cps)
                        (remove (fn [[set-name]]
                                  (= set-name subcps-name)))
                        (sort-by (juxt (comp count :scale second) first))
                        reverse)
        subsets (->> subcps cps/+all-subcps :subcps
                     (sort-by (juxt (comp count :scale second) first))
                     reverse
                     (remove (fn [[set-name]]
                               (= set-name subcps-name))))]
    [:div
     [:h2 subcps-name]
     (factor-filters-input)
     (degrees-filters-input)
     [:div
      [:h3 "Supersets (sets that contain this set)"]
      (if-not (seq supersets)
        [:p "This set is not contained by any other set"]
        [:div (cps-table cps supersets)])]
     [:div
      [:h3 "Subsets (sets contained by this set)"]
      (if-not (seq subsets)
        [:p "This set does not contain any other set"]
        [:div (cps-table cps subsets)])]]))

(defn main
  [state]
  (when-let [cps* (:cps/cps-scale @state)]

    (let [cps (cps/+all-subcps cps*)]
      [:div
       [:h1 "Analysis"]
       (modal @modal-data
              (fn [] (reset! modal-data nil))
              (make-subcps-details @modal-data))
       (factor-filters-input)
       (degrees-filters-input)
       [:h2 "Subsets"]
       (cps-table cps (main-subset-rows cps))])))
