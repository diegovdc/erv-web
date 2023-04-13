(ns wilson-tunings.lattice-maker.core
  (:require
   [clojure.string :as str]
   [erv.lattice.v2 :refer [base-coords ratios->lattice-data]]
   [quil.core :as q :include-macros true]
   [reagent.core :as r]
   [wilson-tunings.modal :as modal]
   [wilson-tunings.utils :refer [set-body-scrolling!]]))

(def scale-string (r/atom "45/44
35/33
12/11
9/8
7/6
105/88
5/4
14/11
4/3
15/11
140/99
35/24
3/2
14/9
35/22
5/3
56/33
7/4
20/11
15/8
21/11
2/1"))

(def lattice-data (r/atom (ratios->lattice-data base-coords #_["1/1" "15/14" "5/4" "10/7" "3/2" "12/7"]
                                                #_["1/1"
                                                   "80/77"
                                                   "12/11"
                                                   "8/7"
                                                   "96/77"
                                                   "10/7"
                                                   "16/11"
                                                   "120/77"
                                                   "12/7"
                                                   "20/11"]
                                                (str/split
                                                 @scale-string
                                                 #"\s+"))))

(comment
  (-> js/window.innerWidth)
  (-> @lattice-data))
(/ (-> js/window.innerWidth) (+ 29 29))
(/ (-> js/window.innerHeight) (+ 54 13))
(defn draw [width height lattice-data]
  (fn []
    (let [{:keys [data edges min-x max-x min-y max-y]} @lattice-data
          x-length (->> [min-x max-x]
                        (map Math/abs)
                        (apply + 10))
          y-length (->> [min-y max-y]
                        (map Math/abs)
                        (apply + 10))
          cx (- (/ width 2) (/ x-length 2))
          cy (- (/ height 2) (/ y-length 2))
          zoom (* 0.8 (min (/ width x-length)
                           (/ height y-length)))]
      (q/background 0)
      (q/translate cx cy)

      (q/stroke 255)
      (q/fill 255)

      (q/scale zoom)
      (q/stroke-weight 2.5)
      (q/fill 255)
      (doseq [{:keys [coords]} data]
        (q/point (:x coords) (:y coords)))
      (q/stroke-weight 0.2)
      (doseq [edge edges]
        (let [[coords-1 coords-2] edge]
          (q/line (:x coords-1)
                  (:y coords-1)
                  (:x coords-2)
                  (:y coords-2))))
      (q/text-font "Montserrat" 5)
      (q/stroke-weight 0)
      #_(q/fill 255 0 0)
      (doseq [{:keys [ratio coords]} data]
        (q/text ratio (+ (:x coords) 2) (- (:y coords) 0.4))))))

(defn lattice []
  (r/create-class
   {:component-did-mount (fn []
                           (let [width (- js/window.innerWidth 16)
                                 height js/window.innerHeight]

                             (q/defsketch lattice-tool
                               :title "Lattice Tool"
                               :host "lattice-canvas"
                               :settings #(q/smooth 80)
                               :setup (fn []
                                        #_(q/pixel-density 2)
                                        (q/frame-rate 1))
                               :draw (draw width height lattice-data)
                               :size [width height])))
    :reagent-render
    (fn []
      [:div {:id "lattice-canvas"}])}))

(defonce scale-modal-open? (r/atom false))

(defn main
  []
  [:div
   [:h1 "Lattice Maker"]
   [:button {:on-click (fn [] (reset! scale-modal-open? true))} "Edit Scale"]
   (modal/modal @scale-modal-open?
                (fn [] (reset! scale-modal-open? false))
                [:div
                 [:div [:label "Scale"]]
                 [:textarea {:rows 30
                             :value @scale-string
                             :on-change (fn [ev]
                                          (reset! scale-string
                                                  (-> ev .-target .-value)))}]
                 [:button {:on-click (fn []
                                       (try
                                         (reset! lattice-data
                                                 (ratios->lattice-data base-coords
                                                                       (str/split @scale-string #"\s+")))
                                         (catch js/Error _ (js/alert "There was an error generating the lattice")))
                                       (reset! scale-modal-open? false)
                                       (set-body-scrolling! true))}
                  "Create Lattice"]])
   [:div {:display "flex"}]
   [lattice]])
