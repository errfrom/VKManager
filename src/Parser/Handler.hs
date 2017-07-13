{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Parser.Handler
-- Copyright   :  (c) Ivanov Dmitry, 2017
-- License     :  MIT license
-- Maintainer  :  errfrom@yandex.ru
--
-- Задачей данного модуля является
-- обработка полученных JSON представлений
-- и преобразование сих в определенные модулем
-- DataBase типы.
-----------------------------------------------------------------------------

module Parser.Handler
       (parseUser
       ,parseCity
       ,parseCountry
       ,handle) where

import           Types.DataBase
import           Control.Lens                   ((^?))
import           Data.Aeson                     ((.:), (.:?), (.!=))
import qualified Data.Aeson           as Aeson  (Value(Object))
import qualified Data.Aeson.Lens      as ALens  (key)
import qualified Data.Aeson.Types     as ATypes (Value(..), Parser
                                                ,Array, parseMaybe)
import           Data.Aeson.Types               (FromJSON(..), parseJSON)
import qualified Data.HashMap.Strict  as HM     (lookup)
import qualified Data.ByteString.Lazy as LBS    (ByteString(..))
import qualified Data.Maybe           as M      (fromJust)
import qualified Data.Vector          as Vec    (toList)

-----------------------------------------------------------------------------

instance FromJSON Country where
  parseJSON (Aeson.Object obj) =
    Country <$> (obj .: "cid")
            <*> (obj .: "name")

instance FromJSON User where
  parseJSON (Aeson.Object obj) =
    User <$> (obj .:  "uid")
         <*> (obj .:  "first_name")
         <*> (obj .:  "last_name")
         <*> (obj .:  "sex")
         <*> (obj .:? "bdate")
         <*> (obj .:? "country")
         <*> (obj .:? "city")
         <*> (obj .:? "mobile_phone")

instance FromJSON City where
  parseJSON (Aeson.Object obj) =
    City <$> (obj .: "cid")
         <*> (obj .: "name")

checkDeactivated :: ATypes.Value -> Maybe ATypes.Value
checkDeactivated (Aeson.Object val) =
  let deactivated = HM.lookup "deactivated" val
  in deactivated

getResponseJSON :: LBS.ByteString -> Maybe [ATypes.Value]
getResponseJSON json =
  let responseJson = M.fromJust $ json ^? ALens.key "response"
  in case responseJson of
     ATypes.Array arr -> Just (Vec.toList arr)
     _                -> Nothing

defaultParse :: (FromJSON a) => ATypes.Value -> Maybe a
defaultParse val = ATypes.parseMaybe parseJSON val

parseUser :: ATypes.Value -> Maybe User
parseUser val =
  let deactivated = checkDeactivated val
  in case deactivated of
    Just "deleted" -> Just Deleted
    Just "banned"  -> Just Banned
    Nothing        -> defaultParse val

parseCountry :: ATypes.Value -> Maybe Country
parseCountry = defaultParse

parseCity :: ATypes.Value -> Maybe City
parseCity = defaultParse

handle :: (ATypes.Value -> Maybe a) -> LBS.ByteString -> [Maybe a]
handle parseFun json =
  let vals = (M.fromJust . getResponseJSON) json
  in map parseFun vals
