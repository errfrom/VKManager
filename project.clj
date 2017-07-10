(defproject vkmanager "0.2.0"
  :description "Structuring, processing and filtering public VK data."
  :url "https://bitbucket.org/errfrom/vk-manager"
  :license {:name "MIT License"
            :url "https://choosealicense.com/licenses/mit/"}
  :dependencies [[org.clojure/clojure    "1.8.0"]
                 [proto-repl             "0.3.1"]      ; repl
                 [org.clojure/tools.cli  "0.3.5"]      ; console args parser
                 [org.xerial/sqlite-jdbc "3.7.15-M1"]  ; jdbc driver
                 [clj-http "3.6.1"]                    ; http handler
                 [org.clojure/data.json  "0.2.6"]]     ; json handler
  :main vkmanager.clj.core
  :aot [vkmanager.clj.core]
  :target-path "target/%s")
