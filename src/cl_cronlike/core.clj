(ns cl-cronlike.core
  (:require
    [clojure.string])
  (:import
    [java.util Calendar]
    [java.text SimpleDateFormat]))

(defrecord Schedule [minute hour dom mon dow])
(defrecord ScheduledTask [schedule runfunction])

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
    (->Schedule minutes hours dom mon dow)))

(defn add-function
  "Adds a task to the queue using a cron-style string and a function name"
  [{:keys [task-db] :as instance} schedulestring func-id func]
  (swap! task-db (fn [db]
                   (assoc db func-id (->ScheduledTask (schedule-from-string schedulestring) func)))))

(defn remove-function
  "Removes a function from the queue"
  [{:keys [task-db] :as instance} func-id]
  (swap! task-db dissoc func-id))

(defn ^{:no-doc true} match-field
  [field fval sched]
  (or (= :splat (field sched))
      (not (= nil (some (fn [v] (= fval v)) (field sched))))))

(defn runs-now?
  "Check to see if a given schedule runs at this moment."
  [sched cal]
  (let [minmatch (match-field :minute (.get cal Calendar/MINUTE) sched)
        hourmatch (match-field :hour (.get cal Calendar/HOUR_OF_DAY) sched)
        dommatch (match-field :dom (.get cal Calendar/DAY_OF_MONTH) sched)
        monmatch (match-field :mon (+ (.get cal Calendar/MONTH) 1) sched)
        dowmatch (match-field :dow (- (.get cal Calendar/DAY_OF_WEEK) 1) sched)]
    (and hourmatch minmatch dommatch monmatch dowmatch)))

(defn ^{:no-doc true} get-runnable [cal tasks]
  (filter (fn [r] (runs-now? (:schedule r) cal)) tasks))

(defn ^{:no-doc true} do-run-func [task]
  (future ((:runfunction task))))

(defn get-sleep-until-next-minute 
  "Returns amount of milliseconds to sleep until the beginnong of the next minute on the calendar"
  [cal acceleration-factor]
  (let [acceleration-factor (or acceleration-factor 1)  
        cur-sec (.get cal Calendar/SECOND)
        cur-msec (.get cal Calendar/MILLISECOND)
        wait-secs (max (- 59 cur-sec) 0) ; leap second's cur-sec=60
        wait-msecs (+ (* 1000 wait-secs) (- 1000 cur-msec))]
    (long (/ wait-msecs acceleration-factor))))

(defn running?
  "Returns true if the runner is running"
  [{:keys [runner] :as instance}]
  (not (= nil @runner)))

(defn- calendar-factory [acceleration-factor start-ts]
  (if (or (some? acceleration-factor) (some? start-ts))
    (let [acceleration-factor (if (some? acceleration-factor) acceleration-factor 1)
          real-start-ts (System/currentTimeMillis)
          start-ts (if (some? start-ts) start-ts real-start-ts)]
      (fn []
        (let [cal (Calendar/getInstance)
              now (.getTimeInMillis cal)
              sim-ts (+ start-ts (long (* (- now real-start-ts) acceleration-factor)))]
          (.setTimeInMillis cal sim-ts)
          cal)))
    #(Calendar/getInstance)))

(defn start-runner
  "Starts a background thread, running every minute and checking the list of functions"
  [{:keys [runner task-db time-acceleration start-ts] :as instance}]
  (let [get-calendar (calendar-factory time-acceleration start-ts)]
    (swap! runner (fn [v]
                    (if v (future-cancel v))
                    (future (loop []
                              (Thread/sleep (get-sleep-until-next-minute (get-calendar) time-acceleration))
                              (doall (map do-run-func (get-runnable (get-calendar) (vals @task-db))))
                              (recur)))))))

(defn stop-runner
  "Stops the loop"
  [{:keys [runner] :as instance}]
  (swap! runner (fn [v]
                  (if v (future-cancel v))
                  nil)))

(defn create-runner
  ([] (create-runner {}))
  ([{:keys [time-acceleration start-ts]}]
   "Options are
    :time-acceleration float or nil : simulate accelerated time by a factor of n
    :start-ts simulated start time (unix timestamp in ms)"
  {:runner (atom nil)
   :task-db (atom {})
   :time-acceleration time-acceleration
   :start-ts start-ts}))
