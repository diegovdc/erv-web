(ns wilson-tunings.main
  (:require
   [re-frame.core :as rf]
   [reagent.dom :as dom]
   [taoensso.timbre :as timbre]
   [wilson-tunings.router.routes :as routes]
   [wilson-tunings.router.setup :as router.setup]
   [wilson-tunings.state :refer [state]]
   [wilson-tunings.registrations]
   ["webmidi" :as midi]))

(timbre/set-level! :info)

(defn root-component []
  (let [current-route @(rf/subscribe [:current-route])]
    [:div
     (when current-route
       [(-> current-route :data :view)])]))

;; WIP
(defn start-webmidi
  []
  #_(let [on-enabled (fn [] (if (< (.. midi/WebMidi -inputs -length) 1)
                              (set! (.. js/document -body -innerHTML) "No device detected.")
                              (.forEach
                               (.-inputs midi/WebMidi)
                               (fn [device index]
                                 (set!
                                  (.. js/document -body -innerHTML)
                                  (str "" index ": " (.-name device) " <br>"))))))]
      (-> (.enable midi/WebMidi)
          (.then on-enabled)
          (.catch (fn [err] (js/alert err))))))

(defn ^:dev/after-load start
  ([] (start "wilson-tunings-calculator"))
  ([element-id]
   (rf/clear-subscription-cache!)
   (let [root-el (.getElementById js/document element-id)
         _router (router.setup/init! routes/routes)]
     #_(dom/unmount-component-at-node root-el)
     (start-webmidi)
     (dom/render [root-component] root-el))))

(defn stop []
  (println "Restarting"))

(defn ^:export init [opts]
  (rf/dispatch [:initialize])
  (let [element-id
        (-> opts js->clj
            (get "elementId" "wilson-tunings-calculator"))]
    (start element-id)))
