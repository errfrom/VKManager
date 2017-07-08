(ns vkmanager.clj.parser.main
  (:require [vkmanager.clj.parser.receiver :as receiver]
            [vkmanager.clj.parser.handler  :as handler]
            [vkmanager.clj.db              :as db]
            [vkmanager.clj.data            :refer [write-db!]])
  (:import java.lang.System))

(declare get-query-length)

(def ^:private inc-counter
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
  [user statmt]
  (try (write-db! user statmt)
  (catch Exception e
    ; TODO: создать нормальную систему логгирования
    (do (spit "log.txt" e :append true)))))

(defn- handle-users [access-token users-json]
  (let [users          (map handler/juxtapose-json-user users-json)
        valid-users    (filter #(not (false? %)) users)
        country-ids    (map #(.country %) valid-users)
        city-ids       (map #(.city %) valid-users)
        countries-json (receiver/receive-countries-json access-token country-ids)
        cities-json    (receiver/receive-cities-json access-token country-ids)]
    (map #(handler/adapt-user % countries-json cities-json) valid-users)))

(defn parse!
  "Является точкой входа,
   делегируя работу функциям получения, обработки и сохранения
   информации."
   ;TODO: неправильно работают страны и города
   ; нужно брать их из users
  [statmt access-token start-uid max-records]
  (defonce ^:private limit (+ start-uid max-records))
  (if (>= start-uid limit)
      (println "\nГотово")
      (let [query-quantity (if (> max-records basic-query-quantity)
                               basic-query-quantity
                               max-records)
            interval       (adapt-query-length start-uid query-quantity)
            until          (last interval)
            users-json     (receiver/receive-user-jsons access-token interval)
            users          (handle-users access-token users-json)]
      (print (count interval))
      (dorun (for [user users]
               (do (failover-write-db! user statmt)
                   (inc-counter))))
      (parse! statmt access-token until max-records))))
