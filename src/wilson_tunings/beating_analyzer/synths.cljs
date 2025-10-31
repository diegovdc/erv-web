(ns wilson-tunings.beating-analyzer.synths
  (:require
   ["tone" :as Tone]
   [taoensso.timbre :as timbre]))

(defprotocol ISynth
  (play [this])
  (release [this])
  (dispose [this])
  ;; (set-freq [this freq])
  (set-db [this db])
  (set-pan [this pan])
  (ramp-db [this db ramp-dur]))

(defprotocol IAmpLFO
  (set-amp-lfo-freq [this freq])
  (set-amp-lfo-depth [this freq]))

(defprotocol IAdditiveSynth
  (set-partial-db [this partial-index db])
  (set-partial-amp [this partial-index db])
  (ramp-partial-db [this partial-index db ramp-dur])
  (ramp-partial-amp [this partial-index db ramp-dur]))

(defrecord Synth
           [^js osc
            ^js env
            ^js panner
            ^js amp
            ^js amp-lfo
            freq db rel dispose-on-release?]

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

  (set-db [this new-db]
    (set! (.. amp -volume -value) new-db)
    (assoc this :db new-db))

  (ramp-db [this new-db dur]
    (.rampTo (.-volume amp) new-db dur)
    (assoc this :db new-db))

  (set-pan [this pan]
    (set! (.. panner -pan -value) pan)
    this)

  IAmpLFO
  (set-amp-lfo-freq [this freq]
    (println "Setting")
    (set! (.. amp-lfo -frequency -value) freq)
    this)

  (set-amp-lfo-depth [this depth]
    (set! (.. amp-lfo -min) (-> (- 1 depth)
                                (min 1)
                                (max 0)))
    this))

(defrecord AdditiveSynth [partial-oscs ^js add-node ^js env ^js amp freq db rel dispose-on-release?]
  ISynth
  (play [this]
    (if (.-disposed add-node)
      (timbre/warn "Synth already disposed")
      (.triggerAttack env))
    this)

  (release [this]
    (.triggerRelease env)
    (assoc this :disposed? dispose-on-release?)
    (when dispose-on-release?
      (js/setTimeout
       (fn [] (.dispose add-node))
       (+ 100 (* 1000 rel))))
    this)

  (dispose [this]
    (.dispose add-node)
    this)

  #_(set-freq [this new-freq]
              (set! (.-frequency osc) new-freq)
              (assoc this :freq new-freq))

  (set-db [this new-db]
    (set! (.. amp -volume -value) new-db)
    (assoc this :db new-db))

  (ramp-db [this new-db dur]
    (.rampTo (.-volume amp) new-db dur)
    (assoc this :db new-db))

  IAdditiveSynth
  (ramp-partial-db [this partial-index db ramp-dur]
    (when (zero? partial-index)
      (timbre/warn "`partial-index` start at one"))
    (if-let [partial ^js (nth partial-oscs (dec partial-index) nil)]
      (.rampTo (.-volume partial) db ramp-dur)
      (timbre/warn "Partial not found"))
    this)
  (ramp-partial-amp [this partial-index amp ramp-dur]
    (ramp-partial-db this partial-index (Tone/gainToDb amp) ramp-dur))

  (set-partial-db [this partial-index db]
    (when (zero? partial-index)
      (timbre/warn "`partial-index` start at one"))
    (if-let [partial ^js (nth partial-oscs (dec partial-index) nil)]
      (set! (.. partial -volume -value) db)
      (timbre/warn "Partial not found"))
    this)

  (set-partial-amp [this partial-index amp]
    (set-partial-db this partial-index (Tone/gainToDb amp))))

(defn connect-and-output
  "Sets up a signal chain linearly connecting the given nodes."
  [signal-chain]
  (let [pairs (partition 2 1 signal-chain)]
    (doseq [[node-1 node-2] pairs]
      (.connect node-1 node-2))
    (.toDestination (last signal-chain))))

(defn add-signals
  "Adds a sequence of signals"
  [add-node signals]
  (doseq [sig signals]
    (.connect sig add-node 0 0)))

(comment
  (def s (.. (Tone/Oscillator. {:freq 200 :db -20})
             (toDestination)))
  (.play s)
  (. s))

;; More flexible constructor
(defn make-sine
  [& {:keys [freq db type env lfo-freq pan eq-node]
      :or {freq 440
           db -12
           type "sine"
           pan 0
           lfo-freq 0}}]
  (let [default-envelope {:attack 0.01 :decay 5 :sustain 0.8 :release 2}
        env-params (merge default-envelope env)
        rel (:release env-params)
        env* (Tone/AmplitudeEnvelope. (clj->js env-params))
        panner (Tone/Panner. pan)
        amp (Tone/Volume. db)
        lfo-gain (Tone/Gain.)
        amp-lfo (Tone/LFO. #js {:frequency lfo-freq
                                :type "sine"
                                :min 1
                                :max 1})

        osc-params (clj->js {:frequency freq :type type})
        osc (Tone/Oscillator. osc-params)]

    (connect-and-output [osc amp lfo-gain panner env* eq-node])
    (.connect amp-lfo (.-gain lfo-gain))
    (.start amp-lfo)
    (.start osc)

    (->Synth osc env* panner amp amp-lfo freq db rel true)))
(comment
  (def s (make-sine {:pan -1 :env {:attack 2}}))
  (play s)
  (set-pan s 1)
  (set-amp-lfo-depth s 1)
  (set-amp-lfo-freq s 1)
  (ramp-db s -12 2)
  (release s))

(defn make-additive
  [& {:keys [freq partial-amps db env eq-node]
      :or {freq 440 db -12}}]
  (let [default-envelope {:attack 0.01 :decay 5 :sustain 0.8 :release 2}
        env-params (merge default-envelope env)
        rel (:release env-params)
        env* (Tone/AmplitudeEnvelope. (clj->js env-params))
        amp (Tone/Volume. db)
        add-node (Tone/Add.)
        partial-oscs (map-indexed (fn [partial amp]
                                    (Tone/Oscillator.
                                     (clj->js {:frequency (* (inc partial) freq)
                                               :type "sine"
                                               :volume (Tone/gainToDb amp)})))
                                  partial-amps)]
    (add-signals add-node partial-oscs)

    (connect-and-output [add-node amp env* eq-node])

    (doseq [osc partial-oscs]
      (.start osc))

    (->AdditiveSynth partial-oscs add-node env* amp freq db rel true)))

(defn additive-synth?
  [x]
  (instance? AdditiveSynth x))

(comment

  (Tone/gainToDb 0)
  (def eq (Tone/EQ3. 3 -6 3))
  (set! (.. eq -low -value) 0)
  (def s3 (make-additive {:partial-amps [1 0.4 0.6 0.5 0.3 0.3]
                          :eq-rack eq
                          :freq 220
                          :env {:attack 20}}))
  (-> s2 type)
  (instance? AdditiveSynth s2)
  (play s3)
  (ramp-db s2 -0 2)
  (release s2)
  (set-partial-amp s2 1 0)
  (set-partial-db s2 7 -60))

(defn make-eq
  []
  (Tone/EQ3. 3 -3 1))

(comment
  (def eq (make-eq))
  (set! (.. (:eq-rack eq) -gain -value) 0.5)
  (set! (.. (:low-eq eq) -frequency -value) 10000)
  (set! (.. eq -frequency -value) 100)
  (set! (.. eq -gain -value) 1))
