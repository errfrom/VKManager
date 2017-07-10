(ns vkmanager.clj.parser.handler
  "Модуль, реализующий поддержку преобразования json-представления,
   полученного через VK-API, в записи определенные в модуле data, которые
   имеют интерфейс взаимодействия непосредственно с базой данных."
  (:require [vkmanager.clj.utils :as utils]
            [vkmanager.clj.data  :refer [map->User
                                         map->School
                                         map->University
                                         map->UserUniver
                                         map->UserSchool]]))

(defn- find-name-by-cid
  "Возвращает название страны/города
   по его идентификатору."
  [cid json]
  (let [suit-pred #(== (read-string cid) (% "cid"))
        suitable  (filter suit-pred json)]
    (if (empty? suitable)
        nil
        ((first suitable) "name"))))

(defn- check-phone-number ; NOTE: пока что реализована легкая
                          ;       проверка, отбрасывающая большинство
                          ;       неправильных представлений
                          ; TODO: конкретизировать проверку
  "Проверка, является ли указанный пользователем
   телефон настоящим."
  [user]
  (cond (nil? (:phone user))                      user
        (re-matches #"^[+()\d ]+$" (:phone user)) user
        :else                                     (assoc user :phone nil)))

(defn- exists-user-page?
  "Если страница пользователя заблокирована
   или удалена, возвращает логическую ложь и наоборот."
  [user-json]
  (not (contains? user-json "deactivated")))

(defn- substitute-key ; NOTE: вынести в utils?
  "Подменяет ключ ассоциативного массива 'h'
   на значение такого же ключа в 'matching-hash'.

   > (substitute-key {:a :b} {:a 5 :c 6} :a)
     {:b 5}"
  [matching-hash h key]
  (when (contains? matching-hash key)
    {(matching-hash key) (h key)}))

;-------------------------------------------------------------------------------
(defn- transform-json-to-schools
  [user-json]
  (if (not (exists-user-page? user-json))
      false
      (let [schools-val   (user-json "schools")
            schools-json  (cond (nil? schools-val)            false
                                ((comp nil? seq) schools-val) false
                                :else                         schools-val)
            matching-hash {"id"   :sch_id
                           "name" :title}
            worker        (fn [school-json]
                            (->> school-json (keys)
                                             (map (partial substitute-key
                                                           matching-hash
                                                           school-json))
                                             (apply merge)
                                             (map->School)))]
        (if (false? schools-json)
            false
            (map worker schools-json)))))

(defn- transform-json-to-university
  [user-json]
  (if (not (exists-user-page? user-json))
      false
      (let [university-json (if (contains? user-json "universities")
                                (user-json "universities")
                                user-json)
            matching-hash   {"university"      :un_id
                             "university_name" :title}
            university-keys (set (vals matching-hash))]
        (->> university-json (keys)
                             (map (partial substitute-key matching-hash university-json))
                             (apply merge)
                             (map->University)))))

(defn- transform-json-to-user
  "Принимает json строку пользователя и возвращает
   запись типа User."
  [user-json]
  (if (not (exists-user-page? user-json))
      false
      (let [matching-hash {"uid"          :uid
                           "first_name"   :fname
                           "last_name"    :sname
                           "sex"          :genger
                           "bdate"        :dob
                           "country"      :country
                           "city"         :city
                           "mobile_phone" :phone}
            user-keys     (set (vals matching-hash))]
        (->> user-json (keys)
                       (map (partial substitute-key matching-hash user-json))
                       (apply merge)
                       (utils/hash-fmap str)
                       (#(update % :uid read-string))
                       (#(utils/add-missing-keys % user-keys))
                       (map->User)))))

(defn- adapt-schools [schools]
  (if (false? schools)
      []
      (->> schools (filter #(not (false? %)))
                   (map #(update % :sch_id read-string)))))

(defn- adapt-univer [university]
  (if (false? university)
      []
      (->> university (vector)
                      (filter #(not (false? %)))
                      (filter #(not= 0 (:un_id %)))
                      (filter #(not (nil? (.un_id %)))))))

(defn transform
  [user-json]
  (if (not (exists-user-page? user-json))
      false
      (let [user         (transform-json-to-user user-json)
            uid          (.uid user)
            schools      (adapt-schools (transform-json-to-schools user-json))
            university   (adapt-univer (transform-json-to-university user-json))
            school-ids   (map #(.sch_id %) schools)
            univer-id    (map #(.un_id %) university)
            result       (transient [])
            to-result    #(conj! result %)]
        (dorun (map to-result (map #(map->UserSchool {:uid uid :sch_id %}) school-ids)))
        (dorun (map to-result (map #(map->UserUniver {:uid uid :un_id %}) univer-id)))
        (dorun (map to-result schools))
        (dorun (map to-result university))
        (to-result user)
        (persistent! result))))

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
