(ns vkmanager.clj.parser
  (:require [vkmanager.clj.data  :refer [write-db!
                                         read-db!
                                         remove-from-db!
                                         map->User]]
            [vkmanager.clj.db    :as db]
            [vkmanager.clj.utils :as utils]
            [clojure.string      :as str]
            [clj-http.client     :as http]
            [clojure.data.json   :as json])
  (:import  [vkmanager.clj.data User]
            java.lang.Math))
; TODO: создать папку 'parser' и создать модули, реализующие
;       определенный функциональный блок
; TODO: нужно много тестов

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

(defn build-user
  "Принимает json строку пользователя и возвращает
   запись типа User."
  [json-elem]
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

; Функции формирования API.VK ссылок TODO: query params
;-------------------------------------------------------------------------------
(defn receive-users-url
  "Формирование ссылки получения
   информации первых (end - start) пользователей."
  [access_token start end]
  (let [url-pattern (utils/normalize-text
                    "https://api.vk.com/method/users.get?
                     user_ids=%s
                     &access_token=%s
                     &fields=deactivated,uid,first_name,last_name,
                     sex,bdate,country,city,contacts")
        ids          (utils/separate-by-commas (range start end))]
    (format url-pattern ids access_token)))

(defn receive-countries-url
  "Формирование ссылки получения
  названий стран по их идентификаторам."
  [access-token country-ids]
  (let [url-pattern (utils/normalize-text
                    "https://api.vk.com/method/database.getCountriesById?
                     country_ids=%s
                     &access_token=%s")
        country-ids  (utils/separate-by-commas (set country-ids))]
    (format url-pattern country-ids access-token)))

(defn receive-cities-url
  "Формирование ссылки получения
  названий городов по их идентификаторам."
  [access-token city-ids]
  (let [url-pattern (utils/normalize-text
                    "https://api.vk.com/method/database.getCitiesById?
                    city_ids=%s
                    &access_token=%s")
        city-ids    (utils/separate-by-commas (set city-ids))]
    (format url-pattern city-ids access-token)))
;-------------------------------------------------------------------------------

(defn get-json [url]
  (let [http-response (:body (http/get url {:cookie-policy :standard}))
        json          (json/read-str http-response)
        json-response (json "response")]
    json-response))

(defn parse-users! [access-token start-uid quantity]
  (let [start         start-uid
        end           (+ start-uid quantity)
        url           (receive-users-url access-token start end)
        json-response (get-json url)]
    (map build-user json-response)))

(defn update-counters!
  "Инкрементирует определенный счетчик,
  в зависимости от значения бинарного параметра 'valid'
  и выводит формитированный output."
  [valid? out out-pattern counter-valid counter-invalid]
  (if valid? (reset! counter-valid   (inc @counter-valid))
             (reset! counter-invalid (inc @counter-invalid)))
  (.print out (format out-pattern @counter-valid @counter-invalid)))

(defn failover-write-db!
  "Обертка вокруг метода 'write-db!' протокола 'DbInteractional',
  реализующая отказоустойчивость записи в базу данных."
  [user statmt]
  (try (write-db! user statmt)
  (catch Exception e
    ; TODO: создать нормальную систему логгирования
    (do (spit "log.txt" e :append true)))))

; NOTE: should be refactored later
; TODO: реализовать механизм обработки идентификторов местоположения
;       при грубом прекращении работы
;-------------------------------------------------------------------------------
(defn get-name-by-cid [cid json]
  (if (= cid 0)
      "NULL"
      (format "'%s'"
       ((first (filter #(= cid (% "cid")) json)) "name"))))

(defn get-user-location-ids! [uid statmt location-pattern]
  (->> location-pattern (#(str % uid))
                          (.executeQuery statmt)
                          (resultset-seq)
                          (first)))

(defn update-location-worker!
  [statmt user-uids
   update-pattern location-pattern json-countries json-cities]
  (for [uid user-uids]
    (let [{:keys [city country]} (get-user-location-ids! uid statmt location-pattern)
          country-name (get-name-by-cid (read-string country) json-countries)
          city-name    (get-name-by-cid (read-string city) json-cities)
          update-query (format update-pattern country-name city-name uid)]
      (.execute statmt update-query))))

(defn update-location-data!
  "Замещает идентификаторы стран и городов
  соответствующими названия."
  [statmt access-token user-uids]
  (let [country-ids    (db/get-countries! statmt user-uids)
        countries-url  (receive-countries-url access-token country-ids)
        json-countries (get-json countries-url)

        city-ids       (db/get-cities! statmt user-uids)
        cities-url     (receive-cities-url access-token city-ids)
        json-cities    (get-json cities-url)

        update-pattern    "UPDATE USERS SET COUNTRY = %s, CITY = %s WHERE UID = %s"
        location-pattern  "SELECT COUNTRY, CITY FROM USERS WHERE UID = "]
  (update-location-worker! statmt user-uids update-pattern location-pattern
                           json-countries json-cities)))
;-------------------------------------------------------------------------------

(defn handle-db-update!
  "Делигирует задачи
  функциям 'failover-write-db!' и 'update-counters!',
  опираясь на результат функции 'one-iter', возвращающей
  список 'User' записей."
  [statmt users
   out out-pattern counter-valid counter-invalid]
  (let [update-counters! #(update-counters!
                          % out out-pattern counter-valid counter-invalid)]
    (for [user users]
      (if (false? user)
          (update-counters! false)
          (do (failover-write-db! user statmt)
              (update-counters! true))))))

(defn worker!
  "Рекурсивно обрабатывает информацию,
  вследствие чего не происходит нехватки места в куче."
  [statmt access-token start-uid query-quantity limit
   out out-pattern counter-valid counter-invalid]

  (if (>= start-uid limit)
      (println "\nГотово")
      (let [updated-start-uid (+ start-uid query-quantity)
            users             (parse-users! access-token start-uid query-quantity)
            valid-users       (filter #(not (false? %)) users)
            user-uids         (map #(.uid %) valid-users)]

        (dorun (handle-db-update! statmt users out out-pattern counter-valid counter-invalid))
        (dorun (update-location-data! statmt access-token user-uids))
        (worker! statmt access-token updated-start-uid query-quantity limit
                 out out-pattern counter-valid counter-invalid))))

(defn parse! [statmt access-token start-uid max-records]
  (let [query-quantity  (if (> max-records 500) 500 max-records)
        limit           (+ max-records start-uid)
        out             (System/out)
        out-pattern     (str "\rОбработано и записано в базу данных: %s. "
                             "Несуществующих аккаунтов: %s.")
        counter-valid   (atom 0)
        counter-invalid (atom 0)]

    (worker! statmt access-token start-uid query-quantity limit ; parse params
             out out-pattern counter-valid counter-invalid)))   ; out params
