(ns wilson-tunings.router.routes
  (:require
   [reitit.coercion.malli]
   [wilson-tunings.cps-colors :as cps-colors]
   [wilson-tunings.cps.core :as cps]
   [wilson-tunings.constant-structure-analyzer.core :as cs-analyzer]
   [wilson-tunings.lattice-maker.core :as lattice-maker]
   [wilson-tunings.voice-leading.core :as voice-leading]
   [wilson-tunings.home :as home]
   [wilson-tunings.marwa :as marwa]
   [wilson-tunings.mos :as mos]
   [wilson-tunings.mos2 :as mos2]
   [wilson-tunings.state :refer [state]]))

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
     :coercion reitit.coercion.malli/coercion
     :parameters {:query [:map
                          [:tidal-names {:optional true} boolean?]
                          [:factors {:optional true} string?]
                          [:set-size {:optional true} string?]]}
     :controllers [{:start (fn [params]
                             (swap! state
                                    (fn [state*]
                                      (-> state*
                                          (update :factors #(or (:factors params) %))
                                          (update :set-size #(or (:set-size params) %))
                                          (update :period #(or (:period params) %))))))
                    :stop  (fn [_params])
                    :identity (fn [match]
                                (-> match :parameters :query))}]}]
   ["/cps-colors"
    {:name      :routes/cps-colors
     :view      #'cps-colors/main
     :controllers
     [{:start (fn [_params])
       :stop  (fn [_params])}]}]
   ["/constant-structure-analyzer"
    {:name      :routes/constant-structure-analyzer
     :view      #'cs-analyzer/main
     :controllers
     [{:start (fn [_params])
       :stop  (fn [_params])}]}]
   ["/lattice-maker"
    {:name      :routes/lattice-maker
     :view      #'lattice-maker/main
     :controllers
     [{:start (fn [_params])
       :stop  (fn [_params])}]}]
   ["/voice-leading"
    {:name      :routes/voice-leading
     :view      #'voice-leading/main
     :controllers
     [{:start (fn [_params])
       :stop  (fn [_params])}]}]])
