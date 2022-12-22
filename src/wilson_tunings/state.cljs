(ns wilson-tunings.state
  (:require [reagent.core :as r]))

(defonce state (r/atom {:view :nil
                        :factors "1,3,5,7"
                        :set-size "2"
                        :period 2
                        :mos/submos-representation-mode :unique-mos
                        :mos/period 12
                        :mos/generator 5
                        :mos/remove-generator-1 true
                        :marwa/input-scale [2 2 1 2 2 2 1]
                        :marwa/remove-unisons? true}))
