(ns wilson-tunings.main
  (:require
   [re-frame.core :as rf]
   [reagent.dom :as dom]
   [taoensso.timbre :as timbre]
   [wilson-tunings.router.routes :as routes]
   [wilson-tunings.router.setup :as router.setup]
   [wilson-tunings.state :refer [state]]
   [wilson-tunings.registrations]))

(timbre/set-level! :info)

(defn root-component []
  (let [current-route @(rf/subscribe [:current-route])]
    [:div
     (when current-route
       [(-> current-route :data :view)])]))

(defn ^:dev/after-load start
  ([] (start "wilson-tunings-calculator"))
  ([element-id]
   (rf/clear-subscription-cache!)
   (let [root-el (.getElementById js/document element-id)
         _router (router.setup/init! routes/routes)]
     #_(dom/unmount-component-at-node root-el)
     (dom/render [root-component] root-el))))

(defn stop []
  (println "Restarting"))

(defn ^:export init [opts]
  (let [element-id
        (-> opts js->clj
            (get "elementId" "wilson-tunings-calculator"))]
    (start element-id)))
