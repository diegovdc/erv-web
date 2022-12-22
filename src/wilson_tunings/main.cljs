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

#_(defn app []
    (case (@state :view)
      :cps (cps/main state)
      :cps-colors (cps-colors/main state)
      :mos (mos/main state)
      :mos.v2 (mos2/main state)
      :marwa (marwa/main state)
      [:div {:class "wt__main-screen"}
       [:h1 "Tools for exploring some of Erv Wilsons scale concepts"]
       [:p "What do you want to see?"]
       [:button {:on-click #(swap! state assoc :view :mos)} "Moments of symmetry calculator"]
       [:button {:on-click #(swap! state assoc :view :mos.v2)} "Moments of symmetry calculator (V2)"]
       [:button {:on-click #(swap! state assoc :view :marwa)} "Marwa Permutations"]
       [:button {:on-click #(swap! state assoc :view :cps)} "CPS calculator"]
       [:button {:on-click #(swap! state assoc :view :cps-colors)} "CPS Colors (experiment)"]]))

(comment
  (-> @re-frame.db/app-db))

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

(comment
  (reset! app-state))

(defn ^:export init [opts]
  (let [element-id
        (-> opts js->clj
            (get "elementId" "wilson-tunings-calculator"))]
    (start element-id)))
