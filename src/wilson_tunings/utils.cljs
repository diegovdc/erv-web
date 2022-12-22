(ns wilson-tunings.utils
  (:require [reitit.frontend.easy :as rfe]))

(defn href
  "Return relative url for given route. Url can be used in HTML links."
  ([k]
   (href k nil nil))
  ([k params]
   (href k params nil))
  ([k params query]
   (rfe/href k params query)))

(defn set-body-scrolling!
  [enable?]
  (set! js/document.body.style.overflow (if enable? "initial" "hidden")))

(defn y-space []
  [:div {:style {:height 16}}])
