(ns vkmanager.clj.utils
  "Набор вспомогательных частоиспользуемых функций."
  (:require [clojure.string :as str]
            [clojure.set    :refer [difference]]))

(defn normalize-text [text & {:keys [delimiter] :or {delimiter ""}}]
  (-> text (#(str/split % #" |\n"))
           (#(filter (comp not nil? seq) %))
           (#(str/join delimiter %))))

(def join-by-newline
  (partial str/join "\n"))

(defn insert-seq [index s values]
  (let [before (vec (take index s))
        after  (vec (drop index s))]
    (reduce into [before values after])))

(defn align-by
  "Выравнивает строки по ограничителю."
  [delimiter & strings]
  (let [space      " "
        pattern    (re-pattern (format "[%s]" delimiter))
        max-index  (apply max
                          (map #(str/last-index-of % delimiter)
                               strings))]
    (vec
     (for [string strings]
       (let [index         (str/last-index-of string delimiter)
             vec-string    (vec string)
             spaces-amount (- max-index index)
             spaces        (vec (repeat spaces-amount space))]
         (str/join (insert-seq index vec-string spaces)))))))

(defn flatten1
  "Удаляет один уровень вложенности.
   > (flatten1 [a1 a2 [a3 a4]])
   [a1 a2 a3 a4]"
  [x]
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

(defn separate-by-commas
  "Соединяет элементы коллекции в строку по разделителю запятой."
  [x]
  (if (coll? x)
    (str/join "," x)
    x))

(defn lstrip
  "Удаляет первый элемент строки."
  [x]
  (str/join "" (rest x)))

(defn add-missing-keys
  "Сравнивает hashmap с шаблоном (последовательность ключей).
   И, если в hashmap'е отсутствуют некоторые ключи из шаблона,
   добавляет их со значением nil.
   > (add-missing-keys {:a 1 :b 2} #(:a :b :c :d)
   {:a 1 :b 2 :c nil :d nil}."
  [x pattern]
  (let [excepted     (if (instance? java.util.Set pattern) pattern (set pattern))
        existing     (set (keys x))
        missing      (difference excepted existing)
        missing-nils (apply merge (map #(hash-map % nil) missing))]
    (merge x missing-nils)))

(defn positive-integer?
  "Определяет, является ли число, представленное строкой
   целым и положительным."
  [x]
  (if (nil? (re-matches #"^[1-9][0-9]*$" x)) false
                                             true))

(defn update-if-contains
  "Обертка вокруг встроенной функции update.
   Отличие заключается в обработке исключительной ситуации
   при передаче несуществующего в ассоциативном массиве ключа:
   обычная функция добавляет ключ, эта же возвращает первоначальный
   hashmap"
  [x key' fun]
  (if (contains? x key') (update x key' fun)
                         x))

(defn remove-tenth
  "Вернуть 9/10 числа."
  [num]
  (Math/floor (* num 9/10)))

(defn hash-fmap
  "Реализация функтора для ассоциативных массивов."
  [f m]
  (apply merge (for [[k v] m] {k (f v)})))
