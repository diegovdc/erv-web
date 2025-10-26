(ns wilson-tunings.synthesis
  "Mostly a playground, but initializes Tone.js"
  (:require
   ["tone" :as Tone]
   [taoensso.timbre :as timbre]))

(defonce inited? (atom false))
(defn init! []
  (when-not @inited?
    (set! (.-Tone js/window) Tone)
    (Tone/start)
    (reset! inited? true)))

;;;;;;;;;;;;;;
;; Playground
;;;;;;;;;;;;;;;

(comment
  (js/console.log (Tone/getTransport))
  (doseq [s ((Tone/getTransport))]
    (js/console.log s))
  (js/console.log Tone)

  (def s (.toDestination (Tone/Synth.)))
  (.triggerAttackRelease s "C4" "8n")

  (defn make-synth
    [&  {:keys [freq db rel]
         :or {freq 440
              db -12
              rel 2}}]
    (let [env (.. (Tone/AmplitudeEnvelope.
                   #js {:attack 1 :decay 0.5 :sustain 0.8 :release rel})
                  (toDestination))
          synth (.. (Tone/Oscillator. #js {:frequency freq
                                           :type "sine"
                                           :volume db})
                    (connect env)
                    (start))]
      {:env env
       :synth synth
       :release (fn []
                  (.triggerRelease env)
                  (js/setTimeout (fn [] (.dispose synth)) (+ 100 (* 1000 rel))))
       :play (fn [] (.triggerAttack env))}))
  (def s (make-synth))
  (play s)
  ((:play s))
  ((:release s))
  (.stop (:synth s))
  (.start s))

(comment
  ;; used for the beating analyzer synth
  (defprotocol ISynth
    (play [this])
    (release [this])
    (dispose [this])
    (set-freq [this freq])
    (set-db [this db]))

  (defrecord Synth [osc env amp freq db rel dispose-on-release?]
    ISynth
    (play [this]
      (println this)
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
    [& {:keys [freq db rel type envelope]
        :or {freq 440
             db -12
             rel 2
             type "sine"
             envelope {:attack 1 :decay 0.5 :sustain 0.8 :release rel}}}]
    (let [env-params (clj->js envelope)
          env (Tone/AmplitudeEnvelope. env-params)
          amp (Tone/Volume. db)
          osc-params (clj->js {:frequency freq :type type})
          osc (Tone/Oscillator. osc-params)]

      (connect-and-output [osc amp env])
      (.start osc)

      (->Synth osc env amp freq db rel true)))

  (def s (make-synth))
  (play s)
  (set-db s -0)
  (release s))
