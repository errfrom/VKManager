(ns vkmanager.clj.db
  (:require [clojure.string  :as str]
            [clojure.java.io :as io])
  (:import  java.lang.System
            java.sql.DriverManager
            java.sql.SQLException))

(defn file-exists? [path-to-file]
  (.exists (io/file path-to-file)))

(defn refactor-db-path [path-to-db]
  (let [name-default "default"
        db-format    ".db"]
    (cond (.isDirectory (io/file path-to-db)) (str path-to-db name-default db-format)
          (not (str/ends-with? path-to-db db-format)) (str path-to-db db-format)
          :else path-to-db)))
          
(defn close-db! [conn statmt]
  (.close statmt)
  (.close conn))

(defn handle-sql-exception! [exc-msg conn statmt]
  (let [mis-use "[SQLITE_MISUSE]"]
    (cond (str/starts-with? exc-msg mis-use) nil ; cond для расширяемости
           :else (do (close-db! conn statmt)
                     (println "Ошибка SQLite. Завершение работы...")
                     (System/exit 1)))))

(defn init-db! [path-to-db]
  "Инициализация базы данных.
   Создает локуальную sqlite базу данных
   в корне проекта со структурой, описанной
   в 'init.sql'."
    (let [path-to-db    (refactor-db-path path-to-db)
          exists?       (file-exists? path-to-db)
          conn-name     (str "jdbc:sqlite:" path-to-db)
          conn          (DriverManager/getConnection conn-name)
          statmt        (.createStatement conn)
          sql-init-path "src/vkmanager/sql/init.sql"
          sql-commands  (str/split (slurp sql-init-path) #";")]
    (when (not exists?)
      (do (println (str "Инициализация новой базы данных - " path-to-db "..."))
          ; создание сразу всех отношений
          (try (doall (map #(.execute statmt %) sql-commands))
               (catch SQLException
                      exception
                      (handle-sql-exception! (.getMessage exception) conn statmt)))
          (println "Инициализировано.")))
    [conn statmt]))
