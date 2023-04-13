(ns wilson-tunings.cps.common
  (:require
   [clojure.string :as str]
   [com.gfredericks.exact :as e]
   [erv.cps.core :as cps]
   [erv.utils.conversions :as conv]
   [erv.utils.core :as utils]
   [erv.utils.ratios :refer [float->ratio]]))

(def ONE (e/native->integer 1))

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

(defn scale-table [{:keys [scale meta degrees]}]
  [:div
   (when (:cps/deduped? meta)
     [:p {:style {:margin-bottom 16, :max-width 400}}
      [:small
       "Note: This scale has duplicate notes that have been removed. Some rows may contain notes with multiple sets of factors that produce the same ratio. These sets of factors have been separated by the \"&\" symbol."]])
   [:table {:border 1}
    [:thead
     [:tr
      [:th "Degree"]
      [:th "Cents"]
      [:th "Ratio"]
      [:th "Factors"]]]
    [:tbody
     (map-indexed
      (fn [i {:keys [set sets bounded-ratio bounding-period]}]
        (let [set* (if sets
                     (->> sets
                          (map #(str/join ", " %))
                          (str/join " & "))
                     (str/join ", " set))]
          [:tr {:key set}
           [:td (nth degrees i i)]
           [:td (utils/round2 4 (conv/ratio->cents bounded-ratio))]
           [:td (str (within-bounding-period bounding-period
                                             (apply * set)
                                             (meta :cps/normalized-by)))]
           [:td set*]]))
      scale)]]])
