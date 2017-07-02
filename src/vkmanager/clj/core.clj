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

(defn align-descs [options]
  "parse-opts подразумевает выравнивание,
   однако для сохранения единого стиля,
   описание от остальных параметров должно
   быть отделено прямой чертой '|'.

   Добавляет перед описанием каждой опции, ограничитель."
  (let [delimiter "| "
        ; parse-opts принимает вектор опций, где каждая
        ; опция предаставлена вектором, поэтому приходится
        ; приводить сначала все опции в ассоц. массив, а потом
        ; обратно в вектор
        to-hash-map   (partial map (partial apply hash-map))
        from-hash-map (partial map (comp utils/flatten1 vec))
        add-delimiter (fn [option]
                        (if (contains? option :desc)
                          (update option
                                  :desc
                                  #(str delimiter %))
                          option))]
  (from-hash-map (map add-delimiter
                      (to-hash-map options)))))

(def options ; TODO Добавить опцию для relations, idишки в форме интервала
  (align-descs
  [[:short-opt "-p" :long-opt "--path-to-db" :required "" :default "default.db"
   :default-desc "(текущая директория)"
   :desc "Путь к базе данных, существующей или нет."]

  [:short-opt "-t" :long-opt "--access-token" :required "" :default false
   :default-desc "(без access-token'а)"
   :desc "[Рекомендуется!] Ключ доступа для получения расширенных результатов."]

  [:short-opt "-s" :long-opt "--start" :required "" :default false
   :default-desc "(автоматически)"
   :desc (utils/normalize-text
         "[только для 'collect'] Идентификатор пользователя,
          c которого следует начинать выгрузку." :delimiter " ")]

  [:short-opt "-i" :long-opt "--interval" :required "" :default false
   :default-desc "(без интервала)"
   :desc (utils/normalize-text
         "[только для 'collect'] Интервал обновления базы данных.
          (Формат значения - hh:mm)" :delimiter " ")]

  [:short-opt "-h" :long-opt "--help" :required "" :default false
   :default-desc ""
   :desc "Выводит небольшую справку или описание определенных действий, опций."]
  ]))

(defn interval? [interval]
  (let [interval-regexp #"^[0-9][0-9]:[0-5][0-9]"]
    (cond (not (instance? String interval))            false
          (nil? (re-matches interval-regexp interval)) false
    :else true)))

(def msg-validate-interval
  "Совет, выводящийся при несоответствии значения интервала,
   вводимого пользователем необходимой определенной форме."
  (utils/join-by-newline
    ["Значения опции 'interval' должно быть представлено"
     "в форме: часы:минуты, где"
     "(00 <= часы <= 99) и (00 <= минуты <= 59)."]))

(defn handle-help [args options]
  "Отдельная обработка опции 'help', связанная
   с различным поведением в зависимости от переданного ей
   значения."
  (let [args (set args)
        contains-help? (or (contains? args "-h")
                           (contains? args "--help"))]
    (cond (not contains-help?) false
          (false? (options :help))  true
          :else                     true)))

(defn handle! [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args options)
        advice-format "\n\nСоветы:"
        advices!      (transient [])
        to-advices!   (partial conj! advices!)]

    (when (true? (handle-help args options))
      (do (println (usage summary))
          (System/exit 0)))

    (when-let [interval (options :interval)]
      (when (not (interval? interval))
        (to-advices! msg-validate-interval)))

    (let [advices          (persistent! advices!)
          pretty-advices   (->> advices (map #(str "\n* " %))
                                        (apply str advice-format)
                                        (str (usage summary)))]
      (if ((comp not nil? seq) advices)
        (do (println pretty-advices)
            (System/exit 0))
        [args options]))))

(defn collect [options])   ; TODO
(defn relations [options]) ; TODO

(defn -main [& args]
  "Делегирует работу указанным пользователем действиям,
   распараллеливания процессы."
  (let [[arguments' options] (handle! args)
        arguments            (set arguments')
        map-arg-action       {"collect"   collect
                              "relations" relations}
        specified-actions    (filter (partial contains?
                                              ((comp set keys) map-arg-action))
                                     arguments)]
    (pmap #((map-arg-action %) options)
          specified-actions)))
