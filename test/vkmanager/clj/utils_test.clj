(ns vkmanager.clj.utils_test
  (:require [vkmanager.clj.test  :refer :all]
            [vkmanager.clj.utils :refer :all])
  (:import  [vkmanager.clj.test Test]))

(def-test flatten1
  [(test [1 2 [3 4]]     [1 2 3 4]     :one true)
   (test [[1 2] 3 4]     [1 2 3 4]     :one true)
   (test [1 [2 3] 4]     [1 2 3 4]     :one true)
   (test ["a" "b" [1 2]] ["a" "b" 1 2] :one true)])

(def-test add-missing-keys
  [(test [{:a 1 :b 2} #{:c :d}]      {:a 1 :b 2 :c nil :d nil})
   (test [{:a 1 :b 2} [:c :d]]       {:a 1 :b 2 :c nil :d nil})
   (test [{:a 1 :b 2} #{}]           {:a 1 :b 2})
   (test [{:a 1 :b 2} #{:a :b}]      {:a 1 :b 2})
   (test [{:a 1 :b 2 :c 3} #{:a :b}] {:a 1 :b 2 :c 3})
   (test [{} #{}]                    {}) ])

(def-test lstrip
  [(test "-test" "test" :one true)
   (test ""      ""     :one true)
   (test "a"     ""     :one true)])

(def-test positive-integer?
  [(test "0"  false :one true)
   (test "1"  true  :one true)
   (test "-1" false :one true)])

(def-test update-if-contains
  [(test [{:a 1 :b 2 :c 3} :a str] {:a "1" :b 2 :c 3})
   (test [{:a 1 :b 2 :c 3} :d str] {:a 1 :b 2 :c 3})
   (test [{} :a str]               {})])

(def-test remove-tenth
  [(test 10   9 :one true)
   (test 0    0 :one true)
   (test -10 -9 :one true)
   (test 11   9 :one true)])

(def-test separate-by-commas
  [(test [1 2 3]       "1,2,3" :one true)
   (test ["a" "b" "c"] "a,b,c" :one true)
   (test []            ""      :one true)
   (test [1 "a"]       "1,a"   :one true)
   (test 5             5       :one true)
   (test "a"           "a"     :one true)])

(def-test normalize-text
  [(test "123
          456
          789" "123456789" :one true)
   (test ""    ""          :one true)
   (test ["Hello,
           howdy?" :delimiter " "] "Hello, howdy?" )])

(def-test align-by
  [(test ["|" "first|" "second|"]           ["first |"
                                             "second|"])
   (test [" " "first " "second "]           ["first  "
                                             "second "])
   (test ["|" "first|" "second|" "third|"]  ["first |"
                                             "second|"
                                             "third |"])
   (test ["|" "first|one" "second|one"]     ["first |one"
                                             "second|one"])
   (test ["|" "|first" " |second"]          [" |first"
                                             " |second"])])
