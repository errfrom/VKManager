(defproject vkmanager "0.1.0"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure    "1.8.0"]
                 [proto-repl             "0.3.1"]      ; repl
                 [org.clojure/tools.cli  "0.3.5"]      ; console args parser
                 [org.xerial/sqlite-jdbc "3.7.15-M1"]] ; jdbc driver
  :main vkmanager.sql.core
  :aot [vkmanager.sql.core]
  :target-path "target/%s")
