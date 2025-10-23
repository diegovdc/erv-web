(ns wilson-tunings.lattice-maker.core
  #_(:require
     [clojure.string :as str]
     [erv.lattice.v2 :refer [base-coords ratios->lattice-data]]
     [quil.core :as q :include-macros true]
     [reagent.core :as r]
     [wilson-tunings.modal :as modal]
     [wilson-tunings.utils :refer [set-body-scrolling!]]))

#_(defonce scale-string (r/atom "45/44
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

#_(defonce lattice-data (r/atom (ratios->lattice-data base-coords #_["1/1" "15/14" "5/4" "10/7" "3/2" "12/7"]
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

#_(defonce text-type (r/atom :ratio))

#_(comment
    (-> js/window.innerWidth)
    (-> @lattice-data)
    (/ (-> js/window.innerWidth) (+ 29 29))
    (/ (-> js/window.innerHeight) (+ 54 13)))

#_(defn draw [text-type width height lattice-data]
    (fn []
      (let [{:keys [data edges min-x max-x min-y max-y period]} @lattice-data
            x-length (->> [min-x max-x]
                          (map Math/abs)
                          (apply +))
            y-length (->> [min-y max-y]
                          (map Math/abs)
                          (apply +))
            cx (/ width 2)
            cy (/ height 2)
            zoom (* 0.7 (min (/ width x-length)
                             (/ height y-length)))]
        (q/background 0)
        (q/translate cx cy)
        (q/scale zoom)
        (comment (q/stroke 255 255)

                 (q/rect min-x min-y x-length y-length))

        (q/stroke 255 255)

        (comment (q/fill 255 0)
                 (q/stroke-weight 20)
                 (q/point 0 0))

        (q/fill 255 255)
        (q/push-matrix)
        (q/translate (- (/ (+ max-x min-x) 2))
                     (- (/ (+ max-y min-y) 2)))

        (comment
          (q/stroke 255 255)
          (q/fill 255 255 0 200)
          (q/stroke-weight 2.5)
          (q/rect min-x min-y x-length y-length))

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
        (doseq [{:keys [ratio coords numer-factors denom-factors]} data]
          (q/text (let [denom-factors* (str/join "*" (remove #(= period %) denom-factors))]
                    (if (= :factors @text-type)
                      (str (str/join "*" (let [ns (remove #(= period %) numer-factors)]
                                           (if (seq ns) ns [1])))
                           (when (seq denom-factors*)
                             (str "/" denom-factors*)))
                      ratio))
                  (+ (:x coords) 2) (- (:y coords) 0.4))))))

#_(defn lattice []
    (r/create-class
     {:component-did-mount (fn []
                             (let [width (- js/window.innerWidth 16)
                                   height js/window.innerHeight
                                   #_#_#_#_width 200
                                       height 200]
                               (q/defsketch lattice-tool
                                 :title "Lattice Tool"
                                 :host "lattice-canvas"
                                 :settings #(q/smooth 80)
                                 :setup (fn []
                                          #_(q/pixel-density 2)
                                          (q/frame-rate 24))
                                 :draw (draw text-type width height lattice-data)
                                 :size [width height])))
      :reagent-render
      (fn []
        [:div {:id "lattice-canvas"}])}))

#_(defonce scale-modal-open? (r/atom false))

(defn main
  []
  #_(js/console.log "Lattice data" (clj->js @lattice-data))
  #_[:div
     [:h1 "Lattice Maker"]
     [:button {:on-click (fn [] (reset! scale-modal-open? true))} "Edit Scale"]
     [:span {:style {:margin-left 16
                     :margin-right 16}}
      "Labels  "
      [:label [:input {:type "radio"
                       :name "text"
                       :checked (= :ratio @text-type)
                       :on-change (fn [_] (reset! text-type :ratio))}] "Ratios"]
      [:label [:input {:type "radio"
                       :name "text"
                       :checked (= :factors @text-type)
                       :on-change (fn [_] (js/console.log "cick") (reset! text-type :factors))}] "Factors"]]
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
                                                                         (filter seq (str/split @scale-string #"\s+"))))
                                           (catch js/Error e
                                             (js/alert "There was an error generating the lattice")
                                             (js/console.error e)))
                                         (reset! scale-modal-open? false)
                                         (set-body-scrolling! true))}
                    "Create Lattice"]])
     [:div {:display "flex"}]
     [lattice]])
