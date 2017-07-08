(ns vkmanager.clj.parser.receiver
  "Модуль, задачей которого является
   получение информации. Интерфейс
   представляется набором функций,
   принимающих определенные параметры
   запросы и возвращающих json представление."
   (:require [vkmanager.clj.utils :as utils]
             [clj-http.client     :as http]
             [clojure.data.json   :as json])
   (:import  [java.net UnknownHostException]))

(def ^:private fields
  (utils/separate-by-commas ["deactivated" "uid" "first_name" "last_name"
                             "sex" "bdate" "country" "city" "contacts"
                             "education" "schools"]))

(def ^:private headers
  {"accept"          (utils/normalize-text
                     "text/html,application/xhtml+xml,application/xml;
                      q=0.9,image/webp,*/*;q=0.8")
   "user-agent"      (utils/normalize-text
                     "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36
                      (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36
                      OPR/45.0.2552.888")
   "accept-encoding" "gzip, deflate, sdch, br"
   "accept-language" "en-US,en;q=0.8"
   "cache-control"   "max-age=0"})

(def ^:private no-connection-msg
  (utils/normalize-text "Ошибка соединения.
                         Проверьте подключение к интернету.
                         Повтор через 15 секунд." :delimiter " "))

(defn- receive-users-url
  [access-token user-ids]
  (let [url  "https://api.vk.com/method/users.get"
        params {"user_ids"     (utils/separate-by-commas user-ids)
                "access_token" access-token
                "fields"       fields}]
    [url params]))

(defn- receive-countries-url
  [access-token country-ids]
  (let [url "https://api.vk.com/method/database.getCountriesById"
        params {"country_ids"   (utils/separate-by-commas (set country-ids))
                "access_token" access-token}]
    [url params]))

(defn- receive-cities-url
  [access-token city-ids]
  (let [url "https://api.vk.com/method/database.getCitiesById"
        params {"city_ids" (utils/separate-by-commas (set city-ids))
                "access_token" access-token}]
    [url params]))

(defn- get-response! [url params]
  (try (http/get url {:query-params  params
                      :headers       headers
                      :cookie-policy :standard})
  (catch UnknownHostException e
    (do (println no-connection-msg)
        (Thread/sleep 15000)
        (get-response! url params)))))

(defn- get-response-body! [url params]
 (let [response  (get-response! url params)
       status-ok 200
       status    (:status response)]
   (if (== status status-ok)
       (:body response)
       false)))

(defn- get-json [http-response-body]
  (if (not http-response-body)
      false
      (let [json          (json/read-str http-response-body)
            json-response (json "response")]
        json-response)))

;-------------------------------------------------------------------------------
(defn receive-user-jsons [access-token user-ids]
  (let [[url params] (receive-users-url access-token user-ids)
        resp-body    (get-response-body! url params)
        json         (get-json resp-body)]
    json))

(defn receive-cities-json [access-token city-ids]
  (let [[url params] (receive-cities-url access-token city-ids)
        resp-body    (get-response-body! url params)
        json         (get-json resp-body)]
    json))

(defn receive-countries-json [access-token country-ids]
  (let [[url params] (receive-countries-url access-token country-ids)
        resp-body    (get-response-body! url params)
        json         (get-json resp-body)]
    json))
