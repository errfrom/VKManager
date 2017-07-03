(ns vkmanager.clj.test
  (:require [clojure.test :refer :all]))

(defrecord Test [input output])

(defn run [fun test']
  (= (apply fun (.input test')) (.output test')))

(defn run-all [fun tests]
  (is (every? true? (map (partial run fun) tests))))
