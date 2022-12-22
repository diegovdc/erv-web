(ns wilson-tunings.router.setup
  (:require
   [re-frame.core :as rf]
   [reitit.coercion.spec :as rss]
   [reitit.frontend :as rtf]
   [reitit.frontend.easy :as rtfe]))

(defn on-navigate [new-match history]
  (when new-match
    (rf/dispatch [:router/navigated new-match history])))

(defonce router (atom nil))

(defn make-router [routes]
  (if-not @router
    (reset! router
            (rtf/router
             routes
             {:data {:coercion rss/coercion}}))
    @router))

(defn init! [routes]
  (let [router (make-router routes)]
    (rtfe/start! router on-navigate {:use-fragment false})
    router))
