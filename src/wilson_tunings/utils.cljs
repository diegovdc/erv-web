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
  (set! js/document.body.style.overflow (if enable? "auto" "hidden")))

(defn y-space []
  [:div {:style {:height 16}}])

(defn parse-integers [s]
  (->> s
       (re-seq #"-?\d+")  ;; Match optional '-' followed by digits
       (map #(js/parseInt % 10))))
