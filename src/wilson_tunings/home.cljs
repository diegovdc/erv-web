(ns wilson-tunings.home
  (:require
   [wilson-tunings.utils :refer [href]]))

(def href-style {:color "#070"
                 :display "block"
                 :margin-bottom 16
                 :font-size 20})

(defn screen []
  [:div {:class "wt__main-screen"}
   [:h1 "Tools for exploring some of Erv Wilson's scale concepts"]
   [:p "What do you want to see?"]
   [:a {:style href-style :href (href :routes/mos)} "Moments of symmetry calculator"]
   [:a {:style href-style :href (href :routes/mos-v2)} "Moments of symmetry calculator (V2)"]
   [:a {:style href-style :href (href :routes/marwa)} "Marwa Permutations"]
   [:a {:style href-style :href (href :routes/cps)} "CPS Calculator"]
   [:a {:style href-style :href (href :routes/cps-colors)} "CPS Color (experiment)"]
   [:a {:style href-style :href (href :routes/constant-structure-analyzer)} "Constant Structure Analyzer"]
   [:a {:style href-style :href (href :routes/lattice-maker)} "Lattice Maker"]
   [:div {:style {:margin-top 40}}
    [:h2 "Non-wilsonian tools"]
    [:a {:style href-style :href (href :routes/voice-leading)} "Voice Leading Calculator"]]])
