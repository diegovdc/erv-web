(ns wilson-tunings.router.routes
  (:require
   [reitit.coercion.malli]
   [wilson-tunings.home :as home]
   [wilson-tunings.mos :as mos]
   [wilson-tunings.mos2 :as mos2]
   [wilson-tunings.marwa :as marwa]
   [wilson-tunings.cps.core :as cps]
   [wilson-tunings.cps-colors :as cps-colors]))

(defn test-route []
  [:div "hola"])

(def routes
  [["/"
    {:name      :routes/home
     :view      #'home/screen
     :controllers
     [{:start (fn [_params])
       :stop  (fn [_params])}]}]
   ["/moments-of-symmetry"
    {:name      :routes/mos
     :view      #'mos/main
     :controllers
     [{:start (fn [_params])

       :stop  (fn [_params])}]}]
   ["/moments-of-symmetry-v2"
    {:name      :routes/mos-v2
     :view      #'mos2/main
     :controllers
     [{:start (fn [_params])
       :stop  (fn [_params])}]}]
   ["/marwa-permutations"
    {:name      :routes/marwa
     :view      #'marwa/main
     :controllers
     [{:start (fn [_params])
       :stop  (fn [_params])}]}]
   ["/cps"
    {:name      :routes/cps
     :view      #'cps/main
     :controllers
     [{:start (fn [_params])
       :stop  (fn [_params])}]}]
   ["/cps-colors"
    {:name      :routes/cps-colors
     :view      #'cps-colors/main
     :controllers
     [{:start (fn [_params])
       :stop  (fn [_params])}]}]])
