(ns cl-cronlike.core)

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

(with-test
  (defn integerize [v]
    (if (integermap v)
      (integermap v)
      (java.lang.Integer/parseInt v)))
  (is (= 1 (integerize "1")))
  (is (= 1 (integerize "Sun")))
  (is (= 17 (integerize "17")))
  (is (= 3 (integerize "Tue"))))

(with-test
  (defn split-or-splat [strfrag]
    (if (= "*" strfrag)
      :splat
      (map integerize
           (map
            clojure.string/trim
            (clojure.string/split strfrag #"\s*,\s*")))))
  (is (= :splat (split-or-splat "*")))
  (is (= [2 3 4] (split-or-splat "Mon, Tue , Wed"))))

(with-test
  (defn schedule-from-string [fstring]
    (let [tokens (clojure.string/split fstring #"[\s]+")
          [minutes hours dom mon dow] (map split-or-splat tokens)]
      (Schedule. minutes hours dom mon dow)))
  (is (schedule-from-string "* 1,2,3,5 * * * *")))

(defn match-field [field fval sched]
  (or (= :splat (field sched))
      (not (= nil (some (fn [v] (= fval v)) (field sched))))))

(with-test
  (defn runs-now?
    ([sched]
       (runs-now? sched (Calendar/getInstance)))
    ([sched cal]
       (let [minmatch (match-field :minute (.get cal (Calendar/MINUTE)) sched)
             hourmatch (match-field :hour (.get cal (Calendar/HOUR)) sched)
             dommatch (match-field :dom (.get cal (Calendar/DAY_OF_MONTH)) sched)
             monmatch (match-field :mon (.get cal (Calendar/MONTH)) sched)
             dowmatch (match-field :dow (.get cal (Calendar/DAY_OF_WEEK)) sched)]
         (and hourmatch minmatch dommatch monmatch dowmatch)))))

                                        ; (schedule-from-string "* * * * 2,3,4,1"))
(defn get-runable [tasks]
  (filter (fn [r] (runs-now? (:schedule r))) tasks))

(defn do-run-func [task]
  ((:runfunction task)))

(defn start-runner []
  (swap! *runner* (fn [v]
                    (if v (future-cancel v))
                    (future (loop []
                              (doall (map do-run-func (get-runable @*taskdb*)))
                              (Thread/sleep 6000)
                              (recur))))))

(defn stop-runner []
  (swap! *runner* (fn [v]
                    (if v (future-cancel v))
                    nil)))
