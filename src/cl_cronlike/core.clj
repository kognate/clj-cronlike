(ns cl-cronlike.core
  (:require clojure.string)
  (:use clojure.test))

(import 'java.util.Calendar)
(import 'java.text.SimpleDateFormat)

(defrecord Schedule [minute hour dom mon dow])
(defrecord ScheduledTask [schedule runfunction])

(def ^{:dynamic true} *taskdb* (atom #{}))
(def ^{:dynamic true} *runner* (atom nil))

(def integermap {"Mon" 2
                 "Tue" 3
                 "Wed" 4
                 "Thu" 5
                 "Fri" 6
                 "Sat" 7
                 "Sun" 1})

(defn ^{:no-doc true} integerize
  "helper function to parse integers from strings and handle special cases
of substrings that are really number-like representations (for days of week, mostly)"
  [v]
  (if (integermap v)
    (integermap v)
    (java.lang.Integer/parseInt v)))

(defn ^{:no-doc true} split-or-splat
  "For cron-like strings,  if the value is a splat (which is a *) then don't split
it on commas. Otherwise, split the field on commas"
[strfrag]
    (if (= "*" strfrag)
      :splat
      (map integerize
           (map
            clojure.string/trim
            (clojure.string/split strfrag #"\s*,\s*")))))

(defn schedule-from-string
  "Takes a string formatted in crontab style and returns a Schedule object.
Format is:  minutes hours day-of-month month day-of-week.
Example:  0,15,30,45 12,13,14 * * 2,4,6
          That format means run every 15 minutes between 12:00 and 15:00 on Monday, Wednesday and Friday
NOTE: each set of comma-seperated values MUST NOT have spaces
" [schedulestring]
  (let [tokens (clojure.string/split schedulestring #"[\s]+")
        [minutes hours dom mon dow] (map split-or-splat tokens)]
    (Schedule. minutes hours dom mon dow)))

(defn run-function-with-cron
  "Adds a task to the queue using a cron-style string and a function name"
  [schedulestring functorun]
  (swap! *taskdb* (fn [v]
                    (conj v (ScheduledTask. (schedule-from-string schedulestring) functorun)))))

(defn ^{:no-doc true} match-field
  [field fval sched]
  (or (= :splat (field sched))
      (not (= nil (some (fn [v] (= fval v)) (field sched))))))

(defn ^{:no-doc true} runs-now?
  "Check to see if a given schedule runs at this moment."
  ([sched]
     (runs-now? sched (Calendar/getInstance)))
  ([sched cal]
     (let [minmatch (match-field :minute (.get cal (Calendar/MINUTE)) sched)
           hourmatch (match-field :hour (.get cal (Calendar/HOUR)) sched)
           dommatch (match-field :dom (.get cal (Calendar/DAY_OF_MONTH)) sched)
           monmatch (match-field :mon (.get cal (Calendar/MONTH)) sched)
           dowmatch (match-field :dow (.get cal (Calendar/DAY_OF_WEEK)) sched)]
       (and hourmatch minmatch dommatch monmatch dowmatch))))

(defn ^{:no-doc true} get-runable [tasks]
  (filter (fn [r] (runs-now? (:schedule r))) tasks))

(defn ^{:no-doc true} do-run-func [task]
  (future ((:runfunction task))))

(defn ^{:no-doc true} get-sleep-until-next-minute []
  (let [ci (Calendar/getInstance)]
    (* 1000 (- 60 (.get ci (Calendar/SECOND))))))

(defn running?
  "returns true if the runner is running"
  []
  (not (= nil @*runner*)))

(defn start-runner
  "Starts a background thread, running every minute and checking the list of functions"
  []
  (swap! *runner* (fn [v]
                    (if v (future-cancel v))
                    (future (loop []
                              (Thread/sleep (get-sleep-until-next-minute))
                              (doall (map do-run-func (get-runable @*taskdb*)))
                              (recur))))))

(defn stop-runner
  "Stops the loop"
  []
  (swap! *runner* (fn [v]
                    (if v (future-cancel v))
                    nil)))
