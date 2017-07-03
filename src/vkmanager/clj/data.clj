(ns vkmanager.clj.data
  (:require [vkmanager.clj.db    :as db]
            [vkmanager.clj.utils :as utils]
            [clojure.string      :as str]))

(defprotocol DbInteractional
  "Протокол, определяющий свойство объекта
   взаимодействовать с базой данных."
  (write-db!  [self statmt]
    "Обновление базы."))

(defmulti read-db!
  "Чтение записи, принадлежащей отношению
   с первичным ключем primary-key."
  (fn [rec statmt primary-key] rec))

(defmulti remove-from-db!
  "Удаление записи, принадлежащей отношению
   с первичным ключем primary-key."
   (fn [rec statmt primary-key] rec))


(defn adapt-values [values]
  (->> values (map #(if (nil? %) "NULL" %))  ; обработка nil значений
              (map #(if (instance? String %) ; обработка строк
                      (format "'%s'" %)
                      %))))

(defn adapt-keys [keys']
  (->> keys' (map str)               ; превращение всех ключей в строки
             (map utils/lstrip)      ; удаление префикса :
             (map str/upper-case)))  ; перевод в верхний регистр

(defn adapt [x]
  "Обобщенно адаптирует записи (records) для взаимодействия
   с базой данных sql."
  (assert (record? x))
  (let [sql-values (adapt-values (vals x))
        sql-keys   (adapt-keys   (keys x))]
    [sql-keys sql-values]))

; USER
;-------------------------------------------------------------------------------
(defrecord User [uid fname sname genger dob country city phone])

(extend-protocol DbInteractional User
  (write-db! [user statmt]
    (let [[sql-keys sql-values] (adapt user)]
      (db/insert-db! statmt "USERS" sql-keys sql-values))))

(defmethod read-db! User
  [rec statmt primary-key]
  (map->User (db/select-by-primary-key! statmt "USERS" "UID" primary-key)))

(defmethod remove-from-db! User
  [rec statmt primary-key]
  (db/delete-by-primary-key! statmt "USERS" "UID" primary-key))
;-------------------------------------------------------------------------------
