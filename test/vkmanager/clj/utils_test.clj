(ns vkmanager.clj.utils_test
  (:require [vkmanager.clj.test  :refer :all]
            [clojure.test        :refer :all]
            [vkmanager.clj.utils :refer :all])
  (:import  [vkmanager.clj.test Test]))

(deftest test-flatten1
  (let [tests [(Test.
                 [ [1 2 [3 4]] ]
                 [1 2 3 4])
               (Test.
                 [ [[1 2] 3 4] ]
                 [1 2 3 4])
               (Test.
                 [ [1 [2 3] 4] ]
                 [1 2 3 4])
               (Test.
                 [ ["a" "b" [1 2]] ]
                 ["a" "b" 1 2])]]
  (run-all flatten1 tests)))

(deftest test-add-missing-keys
  (let [tests [(Test. ; нормальный случай
                 [{:a 1 :b 2} #{:c :d}]
                 {:a 1 :b 2 :c nil :d nil})
               (Test. ; другой тип шаблона
                 [{:a 1 :b 2} [:c :d]]
                 {:a 1 :b 2 :c nil :d nil})
               (Test. ; пустой шаблон
                 [{:a 1 :b 2} #{}]
                 {:a 1 :b 2})
               (Test. ; совпадение с шаблоном
                 [{:a 1 :b 2} #{:a :b}]
                 {:a 1 :b 2})
               (Test. ; недостаточный шаблон
                 [{:a 1 :b 2 :c 3} #{:a :b}]
                 {:a 1 :b 2 :c 3})
               (Test. ; все пустое
                 [{} #{}]
                 {}) ]]
    (run-all add-missing-keys tests)))
