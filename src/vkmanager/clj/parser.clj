(ns vkmanager.clj.parser
  (:require [vkmanager.clj.data  :refer [write-db!
                                         read-db!
                                         remove-from-db!
                                         map->User]]
            [vkmanager.clj.db    :as db]
            [vkmanager.clj.utils :as utils]
            [clojure.string      :as str]
            [org.httpkit.client  :as http]
            [clojure.data.json   :as json])
  (:import  [vkmanager.clj.data User]))

(defn handle-types [user]
  (let [all-vals-to-str #(apply merge (for [[k v] %]
                                           {k (str v)}))]
  (->> user (all-vals-to-str) ; сначала приводим все значения к строковому типу
            ; после приводим отдельные значения к определенным типам
            (#(update % :uid read-string)))))

(defn handle-values [user]
  (let [genger         (:genger user)
        handled-genger  (cond (= genger 0) nil
                              (= genger 1) "Женский"
                              (= genger 2) "Мужской")]
              ; обновляем значения пола
              ; TODO: реализовать также обработку значений
              ; местоположения, среднего образования, высшего образования,
              ; а также наличие пользователя в иных социальных сетях
    (->> user (#(assoc % :genger handled-genger)))))

(defn handle-user [user]
  (->> user (handle-values)
            (handle-types)))

(defn build-user [json-elem]
  "Принимает json строку пользователя и возвращает
   запись типа User."
  (if (contains? json-elem "deactivated") false ; Если страница удалена
    (let [matching-tree   {"uid"          :uid
                           "first_name"   :fname
                           "last_name"    :sname
                           "sex"          :genger
                           "bdate"        :dob
                           "country"      :country
                           "city"         :city
                           "mobile_phone" :phone}
          ; последовательность ключей записи User
          expected-keys    (set (vals matching-tree))
          ; заменяет ключ по умолчанию на ключ, соответствующий одному из
          ; полей записи User
          substitute-key   (fn [key] (when (contains? matching-tree key)
                                        {(matching-tree key) (json-elem key)}))
          ; возвращает ассоциативный массив с отсутствующими ключами,
          ; значения которых указаны как nil
          ; (подробнее в документации к функции utils/add-missing-keys)
          add-missing      #(utils/add-missing-keys % expected-keys)]
      (->> json-elem (keys) ; получаем ключи по умолчанию
                     (map substitute-key) ; заменяем все ключи
                     ; объединяем полученные ключи в единый
                     ; ассоциативный массив, представляющий запись User
                     (apply merge)
                     ; добавляем недостающие элементы для реализации
                     ; записи User
                     (add-missing)
                     ; приводим все значения к необоходимым для отношения
                     ; USERS типам и обрабатываем некоторые значения,
                     ; полученные по умолчанию
                     (handle-user)
                     (map->User)))))

(def user-agent (utils/normalize-text
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko)
     Chrome/58.0.3029.110 Safari/537.36 OPR/45.0.2552.888" :delimiter " "))

(def options   {:user-agent user-agent
                :headers {
  "accept" (utils/normalize-text "text/html,application/xhtml+xml,application/xml;
                                  q=0.9,image/webp,*/*;q=0.8")
  "accept-Encoding"     "gzip, deflate, sdch, br"
  "accept-language"     "en-US,en;q=0.8"
  "cache-control"       "max-age=0"}})

(defn receive-get-url [access_token start end]
  "Формирование ссылки получения
   информации первых (end - start) пользователей."
  (let [url-pattern (utils/normalize-text
                    "https://api.vk.com/method/users.get?
                     user_ids=%s
                     &access_token=%s
                     &fields=deactivated,uid,first_name,last_name,
                     sex,bdate,country,city,contacts")
        ids          (utils/separate-by-commas (range start end))]
    (format url-pattern ids access_token)))

(defn one-iter! [access-token start-uid quantity]
  (let [url      (receive-get-url access-token start-uid (+ start-uid quantity))
        response ((json/read-str (:body @(http/get url options))) "response")]
    (filter #(not (false? %)) (map build-user response))))

(defn parse! [statmt access-token start-uid max-records]
  (let [num-max-query 800] ; Максимальное число пользователей в одном запросе
    (map #(one-iter! % access-token num-max-query)
         (range start-uid (+ start-uid max-records) num-max-query))))
