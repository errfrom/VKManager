(ns vkmanager.clj.parser
  (:require [vkmanager.clj.data  :refer [write-db!
                                         read-db!
                                         remove-from-db!]]
            [vkmanager.clj.db    :as db]
            [vkmanager.clj.utils :as utils]
            [clojure.string      :as str]
            [org.httpkit.client  :as http])
  (:import  [vkmanager.clj.data User]))

(defn parse! [statmt access-token start-uid]) ; TODO
