{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.Receiver
-- Copyright   :  (c) Ivanov Dmitry, 2017
-- License     :  MIT license
-- Maintainer  :  errfrom@yandex.ru
--
-- Модуль, задачей которого является
-- получение информации, посредством VK API.
-- Интерфейс представлен функциями,
-- возвращающими JSON представления
-- полученых данных.
-----------------------------------------------------------------------------

module Parser.Receiver
       ( receiveUsers
       , receiveCities
       , receiveCountries ) where

import           Control.Lens                    ((.~), (^?))
import           Control.Exception               (SomeException(..), catch)
import           Control.Concurrent              (threadDelay)
import qualified Internal.Utils       as Utils
import qualified Data.Vector          as Vec     (toList)
import qualified Data.Maybe           as M       (fromJust)
import qualified Data.Text            as Text    (pack)
import qualified Data.ByteString.Lazy as LBS     (ByteString(..))
import qualified Data.Map.Strict      as Map     (Map(..), toList, fromList)
import qualified Network.HTTP.Client  as Client  (HttpException(..)
                                                 ,HttpExceptionContent(..))
import qualified Network.Wreq         as Network (Options(..), Response(..)
                                                 ,responseBody
                                                 ,getWith, headers, params
                                                 ,defaults)

-- Types & Types Constructors
-----------------------------------------------------------------------------
data Protocol =
    HTTP
  | HTTPS

data URL =
  URL { protocol    :: Protocol
      , body        :: String
      , queryParams :: Map.Map String String }

type AccessToken = String
type Id          = Int

-- URL builders
-----------------------------------------------------------------------------

-- | Формирует ссылку получения информации пользователей 'users.get'
usersURL :: [Id] -> AccessToken -> URL
usersURL uids accessToken =
  let body      = "api.vk.com/method/users.get"
      fields    = ["deactivated", "uid", "first_name", "last_name"
                  ,"sex", "bdate", "country", "city", "contacts"
                  ,"education", "schools", "connections"]
      strUids   = map show uids
      reqParams = Map.fromList [("user_ids",(Utils.separateByCommas strUids))
                               ,("access_token",accessToken)
                               ,("fields",(Utils.separateByCommas fields))]
  in URL HTTPS body reqParams

-- | Формирует ссылку получения названий стран по их идентификаторам
countriesURL :: [Id] -> AccessToken -> URL
countriesURL cids accessToken =
  let body      = "api.vk.com/method/database.getCountriesById"
      strCids   = Utils.mapToString cids
      reqParams = Map.fromList [("country_ids",(Utils.separateByCommas strCids))
                               ,("access_token",accessToken)]
  in URL HTTPS body reqParams

-- | Формирует ссылку получения названий городов по их идентификаторам
citiesURL :: [Id] -> AccessToken -> URL
citiesURL cids accessToken =
  let body =  "api.vk.com/method/database.getCitiesById"
      strCids = Utils.mapToString cids
      reqParams = Map.fromList [("city_ids", (Utils.separateByCommas strCids))
                               ,("access_token", accessToken)]
  in URL HTTPS body reqParams

-- Request setters
-----------------------------------------------------------------------------

setHeaders :: Network.Options -> Network.Options
setHeaders =
  let hdrsList = [("Accept",         (Utils.joinNoDel
                                     ["text/html,application/"
                                     ,"xhtml+xml,application/xml;"
                                     ,"q=0.9,image/webp,*/*;q=0.8"]))
                 ,("User-Agent"     ,(Utils.joinNoDel
                                     ["Mozilla/5.0 (X11; Linux x86_64) "
                                     ,"AppleWebKit/537.36 (KHTML, like Gecko) "
                                     ,"Chrome/58.0.3029.110 Safari/537.36 "
                                     ,"OPR/45.0.2552.888"]))
                 ,("Accept-Charset" ,"utf-8")
                 ,("Accept-Encoding","gzip, deflate, sdch, br")
                 ,("Accept-Language","en-US,en;q=0.8")]
  in do Network.headers .~ hdrsList

setParams :: URL -> Network.Options -> Network.Options
setParams URL{..}  =
  let convertedMap = [Utils.mapTuple Text.pack param
                     |param <- (Map.toList queryParams)]
  in do Network.params .~ convertedMap

-- Builders
-----------------------------------------------------------------------------

-- | Создает строку вида {protocol}{body},
-- чтобы в дальнейшем указать параметры запроса
-- с помощью 'setParams'
buildUrlStringWithoutParams :: URL -> String
buildUrlStringWithoutParams URL{..} =
  let strProtocol = case protocol of
                      HTTP  -> "http://"
                      HTTPS -> "https://"
  in strProtocol ++ body

handleRequest :: Network.Options -> String -> IO (Network.Response LBS.ByteString)
handleRequest opts url =
  let checkErrors :: SomeException -> IO (Network.Response LBS.ByteString)
      checkErrors e = do
        threadDelay 15000000
        doAttempt
      doAttempt = (Network.getWith opts url) `catch` checkErrors
  in doAttempt 

receive :: ([Id] -> AccessToken -> URL) -> AccessToken -> [Id]
                                      -> IO (Maybe LBS.ByteString)
receive funReceiveURL accessToken ids =
  let url        = funReceiveURL ids accessToken
      setParams' = setParams url
      opts       = (setParams' . setHeaders) Network.defaults
  in do
      req <- handleRequest opts (buildUrlStringWithoutParams url)
      return (req ^? Network.responseBody)

-- Receivers
-----------------------------------------------------------------------------

receiveUsers     = receive usersURL
receiveCities    = receive citiesURL
receiveCountries = receive countriesURL
