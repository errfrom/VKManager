(ns vkmanager.clj.utils
  (:require [clojure.string :as str]))

(defn normalize-text [text & {:keys [delimiter] :or {delimiter ""}}]
  (-> text (#(str/split % #" |\n"))
           (#(filter (comp not nil? seq) %))
           (#(str/join delimiter %))))
