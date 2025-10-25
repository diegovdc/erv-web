(ns wilson-tunings.beating-analyzer.synths
  (:require
   ["tone" :as Tone]
   [taoensso.timbre :as timbre]))

(defprotocol ISynth
  (play [this])
  (release [this])
  (dispose [this])
  (set-freq [this freq])
  (set-db [this db]))

(defrecord Synth [^js osc ^js env ^js amp freq db rel dispose-on-release?]
  ISynth
  (play [this]
    (if (.-disposed osc)
      (timbre/warn "Synth already disposed")
      (.triggerAttack env))
    this)

  (release [this]
    (.triggerRelease env)
    (assoc this :disposed? dispose-on-release?)
    (when dispose-on-release?
      (js/setTimeout
       (fn [] (.dispose osc))
       (+ 100 (* 1000 rel))))
    this)

  (dispose [this]
    (.dispose osc)

    this)

  (set-freq [this new-freq]
    (set! (.-frequency osc) new-freq)
    (assoc this :freq new-freq))

  (set-db [this new-db]
    (.rampTo (.-volume amp) new-db 2)
    (assoc this :db new-db)))

(defn connect-and-output
  "Sets up a signal chain linearly connecting the given nodes."
  [signal-chain]
  (let [pairs (partition 2 1 signal-chain)]
    (doseq [[node-1 node-2] pairs]
      (.connect node-1 node-2))
    (.toDestination (last signal-chain))))

  ;; More flexible constructor
(defn make-synth
  [& {:keys [freq db type env]
      :or {freq 440
           db -12
           type "sine"}}]
  (let [default-envelope {:attack 0.01 :decay 5 :sustain 0.8 :release 2}
        env-params (merge default-envelope env)
        rel (:release env-params)
        env* (Tone/AmplitudeEnvelope. (clj->js env-params))
        amp (Tone/Volume. db)
        osc-params (clj->js {:frequency freq :type type})
        osc (Tone/Oscillator. osc-params)]

    (connect-and-output [osc amp env*])
    (.start osc)

    (->Synth osc env* amp freq db rel true)))

(def s (make-synth))
(play s)
(set-db s -0)
(release s)
