{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Core.Receiver
-- Copyright   :  (c) Ivanov Dmitry, 2017
-- License     :  MIT license
-- Maintainer  :  errfrom@yandex.ru
--
-- Модуль, задачей которого является
-- получение информации, посредством VK API.
-- Интерфейс представлен функцией receive,
-- возвращающей JSON представление
-- полученных данных.
-----------------------------------------------------------------------------

module TReceiver where


import qualified System.IO               as IO
import           Types.Receive
import qualified Types.DataBase          as DBTypes (User(..))
import           Data.Monoid                        ((<>))
import qualified Data.Text               as Text    (pack)
import qualified Data.Text.Lazy          as LText   (Text(..))
import qualified Data.Text.Lazy.Encoding as BSUTF8  (decodeUtf8)
import qualified Data.ByteString.Lazy    as LBS     (ByteString(..))
import qualified Utils                   as Utils   (mapToString, separateByCommas
                                                    ,mapTuple)
import qualified Data.Map.Strict         as Map     (toList, fromList)
import           Control.Lens                       ((.~), (^.), (^?))
import qualified Data.Aeson              as Aeson   (decode)
import qualified Network.Wreq            as Network (Options(..), getWith
                                                    ,defaults, responseBody
                                                    ,headers, params)

-- | Формирует ссылку получения информации пользователей 'users.get'
usersURL :: [Id] -> AccessToken -> URL
usersURL uids accessToken =
  let body      = "api.vk.com/method/users.get"
      fields    = ["deactivated", "uid", "first_name", "last_name"
                  ,"sex", "bdate", "country", "city", "contacts"
                  ,"education", "schools"]
      strUids   = map show uids
      reqParams = Map.fromList [("user_ids"    , (Utils.separateByCommas strUids))
                               ,("access_token", accessToken)
                               ,("fields"      , (Utils.separateByCommas fields)) ]
  in URL HTTPS body reqParams

-- | Формирует ссылку получения названий стран по их идентификаторам
countriesURL :: [Id] -> AccessToken -> URL
countriesURL cids accessToken =
  let body      = "api.vk.com/method/database.getCountriesById"
      strCids   = Utils.mapToString cids
      reqParams = Map.fromList [("country_ids" , (Utils.separateByCommas strCids))
                               ,("access_token", accessToken)]
  in URL HTTPS body reqParams

-- | Формирует ссылку получения названий городов по их идентификаторам
citiesURL :: [Id] -> AccessToken -> URL
citiesURL cids accessToken =
  let body =  "api.vk.com/method/database.getCitiesById"
      strCids = Utils.mapToString cids
      reqParams = Map.fromList [("city_ids"    , (Utils.separateByCommas strCids))
                               ,("access_token", accessToken)]
  in URL HTTPS body reqParams

setHeaders :: Network.Options -> Network.Options
setHeaders =
  let hdrsList = [("Accept"         , "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8")
                 ,("Accept-Charset" , "utf-8")
                 ,("Accept-Encoding", "gzip, deflate, sdch, br")
                 ,("Accept-Language", "en-US,en;q=0.8")
                 ,("User-Agent"     , "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36 OPR/45.0.2552.888")]
  in do Network.headers .~ hdrsList

setParams :: URL -> Network.Options -> Network.Options
setParams URL{..}  =
  let convertedMap = [Utils.mapTuple Text.pack param | param <- (Map.toList queryParams)]
  in do Network.params .~ convertedMap

-- | Создает строку вида {protocol}{body},
-- чтобы в дальнейшем указать параметры запроса
-- с помощью 'setParams'
buildUrlStringWithoutParams :: URL -> String
buildUrlStringWithoutParams URL{..} =
  let strProtocol = case protocol of
                      HTTP  -> "http://"
                      HTTPS -> "https://"
  in strProtocol <> body

receive :: ([Id] -> AccessToken -> URL) -> [Id] -> AccessToken -> IO (Maybe LBS.ByteString)
receive funReceiveURL ids accessToken =
  let url        = funReceiveURL ids accessToken
      setParams' = setParams url
      opts       = (setParams' . setHeaders) Network.defaults
  in do
      req <- Network.getWith opts (buildUrlStringWithoutParams url)
      return (req ^? Network.responseBody)
    --  return (BSUTF8.decodeUtf8 <$> req ^? Network.responseBody)

getUser :: IO (Maybe LBS.ByteString) -> User
getUser json = Aeson.decode json :: Maybe User
