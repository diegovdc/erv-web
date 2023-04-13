(ns wilson-tunings.modal
  (:require
   [wilson-tunings.utils :refer [set-body-scrolling!]]))

(defn modal [open? close-fn children]
  (when open?
    (set-body-scrolling! false)
    [:div {:style {:position "fixed"
                   :top 0
                   :left 0
                   :z-index 999
                   :background-color "#000a"
                   :height "100vh"
                   :width  "100vw"
                   :padding 16}}
     [:div {:style {:width "100%"
                    :background-color "white"
                    :padding 16
                    :max-height "100%"
                    :position "relative"
                    :overflow-y "auto"}}
      [:div {:style {:position "absolute"
                     :top 4
                     :right 8
                     :font-size 16}
             :on-click (fn []
                         (set-body-scrolling! true)
                         (close-fn))}
       "X"]
      children]]))
