(ns vkmanager.clj.parser.handler
  "Модуль, который связывает вывод модуля 'receiver'
   со входом модуля 'recorder'. Т.е. реализует
   поддержку преобразования json-представления, полученного
   через VK-API в записи, определенные в модуле data, которые
   имеют интерфейс взаимодействия непосредственно с базой данных."
  (:require [vkmanager.clj.utils :as utils]
            [vkmanager.clj.data  :refer [map->User]]))

(defn- find-name-by-cid
  "Возвращает название страны/города
   по его идентификатору."
  [cid json]
  (let [suit-pred #(== (read-string cid) (% "cid"))
        suitable  (filter suit-pred json)]
    (if (empty? suitable)
        nil
        ((first suitable) "name"))))

(defn- check-phone-number [user] ; TODO: сделать конкретный regexp
  (cond (nil? (:phone user))                      user
        (re-matches #"^[+()\d ]+$" (:phone user)) user
        :else                                     (assoc user :phone nil)))

;-------------------------------------------------------------------------------
(defn juxtapose-json-user
  "Принимает json строку пользователя и возвращает
   запись типа User."
  [user-json]
  (if (contains? user-json "deactivated") false ; если страница удалена
    (let [matching-tree   {"uid"          :uid
                           "first_name"   :fname
                           "last_name"    :sname
                           "sex"          :genger
                           "bdate"        :dob
                           "country"      :country
                           "city"         :city
                           "mobile_phone" :phone}
          user-keys        (set (vals matching-tree))
          ; заменяет ключ по умолчанию на ключ, соответствующий одному из
          ; полей записи User
          substitute-key   #(when (contains? matching-tree %)
                                  {(matching-tree %) (user-json %)})
          add-missing      #(utils/add-missing-keys % user-keys)
          all-vals-to-str  #(utils/hash-fmap str %)
          uid-to-number    #(update % :uid read-string)]

          (->> user-json (keys)
                         (map substitute-key)
                         (apply merge)
                         (all-vals-to-str)
                         (uid-to-number)
                         (add-missing)
                         (map->User)))))

(defn adapt-user
  "Заменяет значения некоторых полей User."
  [user countries-json cities-json]
  (let [country-id   (:country user)
        country-name (find-name-by-cid country-id countries-json)
        city-id      (:city user)
        city-name    (find-name-by-cid city-id cities-json)
        genger       (case (:genger user) "1" "Женский"
                                          "2" "Мужской"
                                          nil)]
    (-> user (assoc :country country-name)
             (assoc :city    city-name)
             (assoc :genger  genger)
             (check-phone-number))))
