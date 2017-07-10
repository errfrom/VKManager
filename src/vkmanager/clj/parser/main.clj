(ns vkmanager.clj.parser.main
  "Входная точка парсера.
   Организовывает работу последовательную работу
   модулей 'receiver' и 'handler'."
  (:require [vkmanager.clj.parser.receiver :as receiver]
            [vkmanager.clj.parser.handler  :as handler]
            [vkmanager.clj.db              :as db]
            [vkmanager.clj.data            :refer [write-db!]])
  (:import [vkmanager.clj.data User]
           java.lang.System))

(declare get-query-length)

(def ^:private inc-counter
  "Счетчик обработанных записей."
  (let [counter (atom 0)
        out     (System/out)
        pattern "\rОбработано записей: "]
    #(let [swap   (swap! counter inc)
           value  (dec swap)]
       (.print out (str pattern value)))))

(def ^:private basic-query-quantity 500)

(defn- get-query-length [interval]
  (->> interval (map (comp count str))
                (apply +)))

(defn- adapt-query-length [start-uid query-quantity]
  "Динамически изменяет запрашиваемое количество записей,
   тем самым не допуская возникновения ошибки из-за слишком
   длинного запроса к VK API."
  (let [basic-query-length (get-query-length (range basic-query-quantity))
        interval           (range start-uid (+ start-uid query-quantity))
        go                 (fn worker [interval]
                             (if (<= (get-query-length interval) basic-query-length)
                                 interval
                                 (worker (drop-last interval))))]
    (go interval)))

(defn- failover-write-db!
  "Обертка вокруг метода 'write-db!' протокола 'DbInteractional',
  реализующая отказоустойчивость записи в базу данных."
  [obj statmt]
  (try (write-db! obj statmt)
  (catch Exception e
    ; TODO: создать нормальную систему логгирования
    (do (spit "log.txt" e :append true)))))

(defn- build-cities-json [access-token valid-data]
  (receiver/receive receiver/cities-url access-token
   (for [data-chunk valid-data]
     (let [user (first (filter #(instance? User %) data-chunk))
           city-id (.city user)]
       city-id))))

(defn- build-countries-json [access-token valid-data]
  (receiver/receive receiver/countries-url access-token
   (for [data-chunk valid-data]
     (let [user (first (filter #(instance? User %) data-chunk))
           country-id (.country user)]
       country-id))))

(defn- handle-data [access-token users-json]
  (let [data           (map handler/transform users-json)
        valid-data     (filter #(not (false? %)) data)
        countries-json (build-countries-json access-token valid-data)
        cities-json    (build-cities-json access-token valid-data)]
    (for [data-chunk valid-data]
      (let [user? #(instance? User %)]
        (map #(if (user? %)
                  (handler/adapt-user % countries-json cities-json)
                  %) data-chunk)))))

(defn parse!
  "Является точкой входа,
   делегируя работу функциям получения, обработки и сохранения
   информации."
  [statmt access-token start-uid max-records]
  (defonce ^:private limit (+ start-uid max-records))
  (if (>= start-uid limit)
      (println "\nГотово")
      (let [query-quantity (if (> max-records basic-query-quantity)
                               basic-query-quantity
                               max-records)
            interval       (adapt-query-length start-uid query-quantity)
            until          (last interval)
            users-json     (receiver/receive receiver/users-url access-token interval)
            data           (handle-data access-token users-json)]
        (dorun
         (for [data-chunk data]
           (do (dorun (map #(failover-write-db! % statmt) data-chunk))
               (inc-counter))))
        (parse! statmt access-token until max-records))))
