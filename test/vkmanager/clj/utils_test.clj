(ns vkmanager.clj.utils_test
  (:require [clojure.test        :refer :all]
            [vkmanager.clj.utils :refer :all]))

(deftest test-flatten1
  (let [test1 [1 2 3 4]
        test2 ["a" "b" 1 2]]
    (is (= test1 (flatten1 [1 2 [3 4]])))
    (is (= test1 (flatten1 [[1 2] 3 4])))
    (is (= test1 (flatten1 [1 [2 3] 4])))
    (is (= test2 (flatten1 ["a" "b" [1 2]])))))

(deftest test-align-by ; TODO: 1 failure
  (let [str1 "something | stuff"
        str2 "anything | stuff"
        test1 "something | stuff"
        test2 "anything  | stuff"
        delimiter "|"]
    (is (= (quote (test1 test2))
           (align-by delimiter str1 str2)))))
