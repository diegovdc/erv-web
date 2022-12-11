(ns wilson-tunings.utils)

(defn set-body-scrolling!
  [enable?]
  (set! js/document.body.style.overflow (if enable? "initial" "hidden")))

(defn y-space []
  [:div {:style {:height 16}}])
