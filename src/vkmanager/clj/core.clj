(ns vkmanager.clj.core
  (:gen-class)
  (:require [vkmanager.clj.db  :as db]
            [clojure.tools.cli :refer [parse-opts]]))

(defn -main
  [& args]
  (let [[conn statmt] (db/init-db! "test.db")]
    (db/close-db! conn statmt)))
