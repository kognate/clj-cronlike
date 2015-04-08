(ns cl-cronlike.test.core
  (:require 
    [clojure.test :refer :all]
    [cl-cronlike.core :refer :all])
  (:import
    [java.util TimeZone]))

(deftest integerizeTest
  (is (= 1 (integerize "1")))
  (is (= 1 (integerize "Sun")))
  (is (= 17 (integerize "17")))
  (is (= 3 (integerize "Tue"))))

(deftest split-or-splatTest
  (is (= :splat (split-or-splat "*")))
  (is (= [2 3 4] (split-or-splat "Mon, Tue , Wed"))))

(deftest fromStringTest
  (is (schedule-from-string "* 1,2,3,5 * * *"))
  (is (schedule-from-string "* 1,2,3,5 * * Mon,Tue,Wed")))

(deftest start-stop-runner
  (let [runner (create-runner)]
    (start-runner runner)
    (add-function runner "* * * * * *" :foo (fn [] nil))
    (remove-function runner :foo)
    (stop-runner runner)))

(deftest run-with-accelerated-time
  (TimeZone/setDefault (TimeZone/getTimeZone "UTC"))
  (let [runner (create-runner {:time-acceleration 600 ; 600 minutes in 1 real minute
                               :start-ts 1427875199000}) ; 2015-04-01T07:59:59+00:00}
        counter (atom 0)
        increment! (fn [] (swap! counter inc))]
    (start-runner runner)
    (add-function runner "0,1,2 8 1 4 3" :increment increment!)
    (Thread/sleep 500) ; approx 5 minutes in sim-time
    (is (= 3 @counter))
    (remove-function runner :increment)
    (stop-runner runner)))
