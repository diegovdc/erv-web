(ns wilson-tunings.modal
  (:require
   [wilson-tunings.utils :refer [set-body-scrolling!]]))

(defn modal [{:keys [open? close-fn children max-width]
              :or {max-width "100%"}}]
  (let [close (fn []
                (set-body-scrolling! true)
                (close-fn))]
    (when open?
      (set-body-scrolling! false)
      [:div {:style {:position "fixed"
                     :top 0
                     :left 0
                     :z-index 999
                     :background-color "#000a"
                     :height "100vh"
                     :width  "100vw"
                     :padding 16}
             :on-click close}
       [:div {:style {:width "100%"
                      :max-width max-width
                      :margin "0 auto"
                      :background-color "white"
                      :padding 16
                      :max-height "100%"
                      :position "relative"
                      :overflow-y "auto"}
              :on-click (fn [ev] (.stopPropagation ev))}
        [:div {:style {:position "absolute"
                       :top 4
                       :right 8
                       :font-size 16}
               :on-click close}
         "X"]
        children]])))
