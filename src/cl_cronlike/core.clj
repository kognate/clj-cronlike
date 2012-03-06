(ns cl-cronlike.core
  (:require clojure.string)
  (:use clojure.test))

(import 'java.util.Calendar)
(import 'java.text.SimpleDateFormat)

(defrecord Schedule [minute hour dom mon dow])
(defrecord ScheduledTask [schedule runfunction])

(def ^{:dynamic true} *taskdb* (atom #{}))
(def ^{:dynamic true} *shouldrun* (atom true))
(def ^{:dynamic true} *runner* (atom nil))

(def integermap {"Mon" 2
                 "Tue" 3
                 "Wed" 4
                 "Thu" 5
                 "Fri" 6
                 "Sat" 7
                 "Sun" 1})

(defn integerize
  "helper function to parse integers from strings and handle special cases
of substrings that are really number-like representations (for days of week, mostly)"
  [v]
  (if (integermap v)
    (integermap v)
    (java.lang.Integer/parseInt v)))

(defn split-or-splat
  "For cron-like strings,  if the value is a splat (which is a *) then don't split
it on commas. Otherwise, split the field on commas"
[strfrag]
    (if (= "*" strfrag)
      :splat
      (map integerize
           (map
            clojure.string/trim
            (clojure.string/split strfrag #"\s*,\s*")))))

(defn schedule-from-string [fstring]
  (let [tokens (clojure.string/split fstring #"[\s]+")
        [minutes hours dom mon dow] (map split-or-splat tokens)]
    (Schedule. minutes hours dom mon dow)))

(defn match-field [field fval sched]
  (or (= :splat (field sched))
      (not (= nil (some (fn [v] (= fval v)) (field sched))))))

(defn runs-now?
  ([sched]
     (runs-now? sched (Calendar/getInstance)))
  ([sched cal]
     (let [minmatch (match-field :minute (.get cal (Calendar/MINUTE)) sched)
           hourmatch (match-field :hour (.get cal (Calendar/HOUR)) sched)
           dommatch (match-field :dom (.get cal (Calendar/DAY_OF_MONTH)) sched)
           monmatch (match-field :mon (.get cal (Calendar/MONTH)) sched)
           dowmatch (match-field :dow (.get cal (Calendar/DAY_OF_WEEK)) sched)]
       (and hourmatch minmatch dommatch monmatch dowmatch))))

(defn get-runable [tasks]
  (filter (fn [r] (runs-now? (:schedule r))) tasks))

(defn do-run-func [task]
  ((:runfunction task)))

(defn get-sleep-until-next-minute []
  (let [ci (Calendar/getInstance)]
    (* 1000 (- 60 (.get ci (Calendar/SECOND))))))

(defn start-runner []
  (swap! *runner* (fn [v]
                    (if v (future-cancel v))
                    (future (loop []
                              (doall (map do-run-func (get-runable @*taskdb*)))
                              (Thread/sleep (get-sleep-until-next-minute))
                              (recur))))))

(defn stop-runner []
  (swap! *runner* (fn [v]
                    (if v (future-cancel v))
                    nil)))
