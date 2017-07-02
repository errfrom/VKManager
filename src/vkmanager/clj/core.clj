(ns vkmanager.clj.core
  (:gen-class)
  (:require [vkmanager.clj.db    :as db]
            [vkmanager.clj.utils :as utils]
            [clojure.string      :as str]
            [clojure.tools.cli   :refer [parse-opts]])
  (:import java.lang.System))

(defn usage [summary]
  (utils/join-by-newline (utils/flatten1
    ["vk-manager"
     "Добро пожаловать!"
     ""
     "Использование: ./run [опции] действие"
     "Если действие не указано напрямую, выполняется 'collect'."
     ""
     "Действия:"
     ""
     (utils/align-by "|"
     "collect | Выгрузка информации, формирование базы данных."
     "relations | Формирует sql-таблицу отношений между определенными пользователями.")
     ""
     "Опции:"
     "Для разных действий можно указать определенные опции!"
     "(название опции, полное название опции, значение по умолчанию, описание)"
     ""
     summary
     ])))

(def options ; TODO: вынести в отдельную функции выставление ограничителей |
  [
  [:short-opt "-h" :long-opt "--help" :required "" :default ""
   :default-desc ""
   :desc "| Выводит небольшую справку или описание определенных действий, опций."]

  [:short-opt "-p" :long-opt "--path-to-db" :required "" :default false
   :default-desc "(текущая директория)"
   :desc "| Путь к базе данных, существующей или нет."]

  [:short-opt "-t" :long-opt "--access-token" :required "" :default false
   :default-desc "(без access-token'а)"
   :desc "| [Рекомендуется!] Ключ доступа для получения расширенных результатов."]

  [:short-opt "-s" :long-opt "--start" :required "" :default false
   :default-desc "(автоматически)"
   :desc (utils/normalize-text
         "| [только для 'collect'] Идентификатор пользователя,
          c которого следует начинать выгрузку." :delimiter " ")]

  [:short-opt "-i" :long-opt "--interval" :required "" :default false
   :default-desc "(без интервала)"
   :desc (utils/normalize-text
         "| [только для 'collect'] Интервал обновления базы данных.
          (Формат значения - hh:mm)" :delimiter " ")]
  ])

(defn interval? [interval]
  (let [interval-regexp #"^[0-9][0-9]:[0-5][0-9]"]
    (cond (not (instance? String interval))            false
          (nil? (re-matches interval-regexp interval)) false
    :else true)))

(def msg-validate-interval
  (utils/join-by-newline
    ["Значения опции 'interval' должно быть представлено"
     "в форме: часы:минуты, где"
     "(00 <= часы <= 99) и (00 <= минуты <= 59)"]))

(defn handle-help! [options summary]
  (cond (options :help) (do (println (usage summary)) ; TODO: расширить cond
                            (System/exit 0))
        :else true))

(defn handle! [args] ; TODO: интерактивные подсказки
  (let [{:keys [options arguments errors summary]} (parse-opts args options)]
    (handle-help! options summary)))

(defn -main
  [& args]
  (handle! args)
  #_(let [[conn statmt] (db/init-db! "test.db")]
    (db/close-db! conn statmt)))
