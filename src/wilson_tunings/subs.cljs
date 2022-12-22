(ns wilson-tunings.subs
  (:require
   [re-frame.core :as rf :refer [reg-sub]]))

(reg-sub
 :current-route
 (fn [db]
   (:current-route db)))
