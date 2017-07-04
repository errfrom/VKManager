(ns vkmanager.clj.test
  (:require [clojure.test :refer :all]))

(defrecord Test [input output])

(defn test [input output & {:keys [one] :or {one false}}]
  "':one true' подразумевает передачу функции единственного
   аргумента. Это всего лишь синтаксический сахар, чтобы не обрамлять
   квадратными скобками input, если аргумент единственнен."
  (if one (Test. [input] output)
          (Test.  input  output)))

(defn run [fun test']
  (let [input  (.input test')
        output (.output test')

        ; equiv выбирает операции на основе
        ; принадлежности выходного значения
        ; числовому типу, т.к. = проверяет
        ; хеш объектов, который для одинаковых
        ; чисел может быть различным
        equiv? (if (number? output) == =)]
  (equiv? (apply fun input) output)))

(defn run-all [fun tests]
  (let [result (is (every? true? (map (partial run fun) tests)))]
    (when (not result)
      (println (str "Тест функции " fun ".")))))

(defmacro def-test [fun tests]
  (let [test-name (symbol (str "test" (hash fun)))]
    `(deftest ~test-name
       (run-all ~fun ~tests))))
