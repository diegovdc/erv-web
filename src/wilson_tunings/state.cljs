(ns wilson-tunings.state
  (:require [reagent.core :as r]
            [wilson-tunings.marwa :as marwa]
            [wilson-tunings.mos :as mos]))

(defonce state (r/atom {:view :nil
                        :generators "1,3,5,7"
                        :set-size "2"
                        :period 2
                        :mos/submos-representation-mode :unique-mos
                        :mos/period 12
                        :mos/generator 5
                        :marwa/input-scale [2 2 1 2 2 2 1]}))
