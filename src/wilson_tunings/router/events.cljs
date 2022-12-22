(ns wilson-tunings.router.events
  (:require
   [re-frame.core :as rf]
   [reitit.frontend.controllers :as rtfc]))

(rf/reg-event-db
 :router/navigated
 (fn [db [_ new-match _history]]
   (let [old-match   (:current-route db)
         controllers (rtfc/apply-controllers (:controllers old-match) new-match)]
     (assoc db :current-route (assoc new-match :controllers controllers)))))
