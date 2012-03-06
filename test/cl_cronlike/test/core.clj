(ns cl-cronlike.test.core
  (:use [cl-cronlike.core])
  (:use [clojure.test]))

(deftest integerizeTest
  (is (= 1 (integerize "1")))
  (is (= 1 (integerize "Sun")))
  (is (= 17 (integerize "17")))
  (is (= 3 (integerize "Tue"))))

(deftest split-or-splatTest
  (is (= :splat (split-or-splat "*")))
  (is (= [2 3 4] (split-or-splat "Mon, Tue , Wed"))))

(deftest fromStringTest
  (is (schedule-from-string "* 1,2,3,5 * * * *"))
  (is (schedule-from-string "* 1,2,3,5 * * * Mon,Tue,Wed")))
