(ns vkmanager.clj.utils
  (:require [clojure.string :as str]))

(defn normalize-text [text & {:keys [delimiter] :or {delimiter ""}}]
  (-> text (#(str/split % #" |\n"))
           (#(filter (comp not nil? seq) %))
           (#(str/join delimiter %))))

(def join-by-newline
  (partial str/join "\n"))

(defn align-by [delimiter & strings] ; TODO: не проходит тесты!
  "Выравнивает строки по ограничителю."
    (let [space   " "
          pattern (re-pattern (format "[%s]" delimiter))
          get-index #(str/last-index-of % delimiter)
          max-index  (apply max
                       (map (partial get-index)
                             strings))

          add-spaces #(str/join
                        (str (str/join (repeat (- max-index (get-index %))
                                               space))
                              delimiter)
                        (str/split % pattern))]
    (vec (map (partial add-spaces) strings))))

(defn flatten1 [x]
  "Удаляет один уровень вложенности.
   > flatten1 [a1 a2 [a3 a4]]
   [a1 a2 a3 a4]"
  (if (nil? (seq x)) []
    (let [head (first x)
          tail (rest  x)]
      (if (sequential? head)
        ; рекурсивно раскладывается двумя путями
        ; если мы имеем [a1 a2 [a3 a4] a5]
        ; получим  (cons a1
        ;            (cons a2
        ;              (into [a3 a4]
        ;                (cons a5 [])
        ; что сворачивается в [a1 a2 a3 a4 a5]
        (into head (flatten1 tail))
        (cons head (flatten1 tail))))))

(defn separate-by-commas [x]
  "Соединяет последовательность в строку по разделителю запятой."
  (if (sequential? x)
    (str/join "," x)
    x))

(defn rstrip [x]
  "Удаляет первый элемент строки."
  (str/join "" (rest x)))
