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
       ,parseCountry
       ,parseCity
       ,handle) where

import           Types.DataBase
import           Control.Lens                   ((^?))
import           Data.Aeson                     ((.:), (.:?), (.!=))
import qualified Data.Aeson           as Aeson  (Value(Object))
import qualified Data.Aeson.Lens      as ALens  (key)
import qualified Data.Aeson.Types     as ATypes (Value(..), Parser
                                                ,Array, parseMaybe)
import qualified Data.HashMap.Strict  as HM     (lookup)
import qualified Data.ByteString.Lazy as LBS    (ByteString(..))
import qualified Data.Maybe           as M      (fromJust)
import qualified Data.Vector          as Vec    (toList)

-----------------------------------------------------------------------------

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

parseUser :: ATypes.Value -> Maybe User
parseUser val =
  let deactivated = checkDeactivated val
  in case deactivated of
    Just "deleted" -> Just Deleted
    Just "banned"  -> Just Banned
    Nothing        -> ATypes.parseMaybe handleUser val
  where handleUser :: ATypes.Value -> ATypes.Parser User
        handleUser (Aeson.Object obj) =
          User <$> (obj .:  "uid")
               <*> (obj .:  "first_name")
               <*> (obj .:  "last_name")
               <*> (obj .:  "sex")
               <*> (obj .:? "bdate")
               <*> (obj .:? "country")
               <*> (obj .:? "city")
               <*> (obj .:? "mobile_phone")

parseCountry :: ATypes.Value -> Maybe Country
parseCountry val = ATypes.parseMaybe handleCountry val
  where handleCountry :: ATypes.Value -> ATypes.Parser Country
        handleCountry (Aeson.Object obj) =
          Country <$> (obj .: "cid")
                  <*> (obj .: "name")

parseCity :: ATypes.Value -> Maybe City
parseCity val = ATypes.parseMaybe handleCity val
  where handleCity :: ATypes.Value -> ATypes.Parser City
        handleCity (Aeson.Object obj) =
          City <$> (obj .: "cid")
               <*> (obj .: "name")

handle :: LBS.ByteString -> (ATypes.Value -> Maybe a) -> [Maybe a]
handle json parseFun =
  let vals = (M.fromJust . getResponseJSON) json
  in map parseFun vals
