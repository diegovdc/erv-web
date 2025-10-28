(ns wilson-tunings.events
  (:require
   [re-frame.core :as rf]
   [wilson-tunings.beating-analyzer.core :as ba]))

(rf/reg-event-fx
 :initialize
 (fn [{:keys [db]} _]
   (when-not (:initialized? db)
     {:db (merge {:initialized? true}
                 ba/default-state)})))
