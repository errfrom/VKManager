(ns vkmanager.clj.core
  "Точка входа программы.
   Представляет консольный пользовательский
   интерфейс."
  (:gen-class)
  (:require [vkmanager.clj.db          :as db]
            [vkmanager.clj.utils       :as utils]
            [vkmanager.clj.parser.main :as parser]
            [clojure.string            :as str]
            [clojure.tools.cli         :refer [parse-opts]])
  (:import java.lang.System))

(defn- usage [summary]
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
     "relations | [В разработке!] Формирует sql-таблицу отношений между определенными пользователями.")
     ""
     "Опции:"
     "Для разных действий можно указать определенные опции!"
     "(название опции, полное название опции, значение по умолчанию, описание)"
     ""
     summary
     ""
     "Пожалуйста, для безопасного срочного завершения выполнения, используете сочетание 'Ctrl+C'."
     "Своевременно запрашиваете новый access-token для предсказуемой работы программы."
     ])))

(defn- align-descs
  "parse-opts подразумевает выравнивание,
  однако для сохранения единого стиля,
  описание от остальных параметров должно
  быть отделено прямой чертой '|'.

  Добавляет перед описанием каждой опции, ограничитель."
  [options]
  (let [delimiter "| "
        ; parse-opts принимает вектор опций, где каждая
        ; опция предаставлена вектором, поэтому приходится
        ; приводить сначала все опции в ассоц. массив, а потом
        ; обратно в вектор
        to-hash-map    (partial map (partial apply hash-map))
        from-hash-map  (partial map (comp utils/flatten1 vec))
        add-delimiter  #(str delimiter %)
        add-delimiters (fn [option]
                         (->> option
                          (#(utils/update-if-contains % :desc add-delimiter))
                          (#(utils/update-if-contains % :default-desc add-delimiter))))]

  (from-hash-map
   (map add-delimiters
        (to-hash-map options)))))

(def ^:private options
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
         "[В разработке!][только для 'collect'] Интервал обновления базы данных.
         (Формат значения - hh:mm)" :delimiter " ")]

  [:short-opt "-m" :long-opt "--max-records" :required "" :default "100000"
   :default-desc "(100 тысяч)"
   :desc "Максимальное число записей за один вызов."]

  [:short-opt "-h" :long-opt "--help" :required "" :default false
   :default-desc ""
   :desc "Выводит небольшую справку или описание определенных действий, опций."]
  ]))

(defn- interval? [interval]
  (let [interval-regexp #"^[0-9][0-9]:[0-5][0-9]"]

    (cond (not (instance? String interval))            false
          (nil? (re-matches interval-regexp interval)) false
          :else                                        true)))

(def ^:private msg-validate-interval
  "Совет, выводящийся при несоответствии значения интервала,
  вводимого пользователем необходимой определенной форме."
  (utils/normalize-text
    "Значения опции 'interval' должно быть представлено
    в форме: часы:минуты, где
    (00 <= часы <= 99) и (00 <= минуты <= 59)." :delimiter " "))

(defn- validate-start [start-value]
  (cond (false? start-value)                                  true
        ((comp not nil? utils/positive-integer?) start-value) true
        :else                                                 false))

(def ^:private msg-validate-start
  "Значение опции 'start' должно быть представлено целым положительным числом.")

(def ^:private msg-validate-max-records
  "Значение опции 'max-records' должно быть представлено целым положительным числом.")

(defn- handle-help
  "Отдельная обработка опции 'help', связанная
  с различным поведением в зависимости от переданного ей
  значения."
  [args options]
  (let [args (set args)
        contains-help? (or (contains? args "-h")
                           (contains? args "--help"))]

    (cond (not contains-help?)      false
          (false? (options :help))  true
          :else                     true)))

(defn- handle! [args]
  "Проверяет значение каждого входного параметра на совпадение
   с определенной требуемой моделью."
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

    (when-let [start-value (not (validate-start (options :start)))]
      (to-advices! msg-validate-start))

    (when (not (utils/positive-integer? (options :max-records)))
          (to-advices! msg-validate-max-records))

    (let [advices        (persistent! advices!)
          pretty-advices (->> advices (map #(str "\n* " %))
                                      (apply str advice-format)
                                      (str (usage summary)))]
      (if ((comp not nil? seq) advices)
          (do (println pretty-advices)
              (System/exit 0))
        [args options]))))

(defn- set-break-handler!
  "Регистрирует сигнал 'грубого' завершения процесса.
  Безопасно завершает все важные процессы."
  ; NOTE: проверить на Windows.
  [conn statmt]
  (let [script-terminated-code 130]
    (sun.misc.Signal/handle
      (sun.misc.Signal. "INT")
      (proxy [sun.misc.SignalHandler] []
        (handle [signal]
          (db/close-db! conn statmt)
          (System/exit script-terminated-code))))))

(defn- collect! [options]
  (let [[conn statmt] (db/init-db! (options :path-to-db))
        access-token  (if-let [access-token (options :access-token)]
                       access-token
                       "")
        max-records   (read-string (options :max-records))
        start-value   (if-let [start-value (options :start)]
                       (read-string start-value)
                       (db/get-start-value! statmt))]

    (set-break-handler! conn statmt)
    (parser/parse! statmt access-token start-value max-records)
    (db/close-db! conn statmt)))

(defn- relations [options]
  (println "В разработке!"))

(defn- advice-help! []
  (println "Неверный запрос. Для получения справки по использованию, введите './run -h'."))

(defn -main
  "Делегирует работу указанным пользователем действиям,
  распараллеливания процессы."
  [& args]
  (let [[arguments' options] (handle! args)
        arguments            (set arguments')
        map-arg-action       {"collect"   collect!
                              "relations" relations}
        execute-action       #((map-arg-action %) options)
        specified-actions    (filter (partial contains?
                                              ((comp set keys) map-arg-action))
                                     arguments)]

    (cond ((comp nil? seq) specified-actions) (advice-help!)
          (= 1 (count specified-actions))     (execute-action (first specified-actions))
          :else                               (pmap (partial execute-action)
                                                    specified-actions))))
