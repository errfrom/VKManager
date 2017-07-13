{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleContexts  #-}

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
import           Internal.Phones
import qualified Internal.Phones      as Phones (parsePhoneNumber, fromType)
import           Text.Regex.Posix               ((=~))
import           Control.Lens                   ((^?))
import           Data.Aeson                     ((.:), (.:?), (.!=))
import           Data.Aeson.Types               (FromJSON(..), parseJSON)
import qualified Data.Aeson.Types     as ATypes (Value(..), Parser
                                                ,Array, parseMaybe)
import qualified Data.Aeson           as Aeson  (Value(Object))
import qualified Data.Aeson.Lens      as ALens  (key)
import qualified Data.HashMap.Strict  as HM     (lookup)
import qualified Data.ByteString.Lazy as LBS    (ByteString(..))
import qualified Data.Maybe           as M      (fromJust)
import qualified Data.Vector          as Vec    (toList)
import qualified Data.Text            as Text   (Text(..), unpack)

-----------------------------------------------------------------------------

instance FromJSON User where
  parseJSON (Aeson.Object obj) = do
    let genger = getGenger $ getValueOf "sex"
        dob    = getDOB    $ getValueOf "bdate"
        phone  = getPhone  $ getValueOf "mobile_phone"
    uid     <- (obj .:  "uid")
    fName   <- (obj .:  "first_name")
    sName   <- (obj .:  "last_name")
    country <- (obj .:? "country")
    city    <- (obj .:? "city")
    return User{..}
    where getValueOf :: Text.Text -> Maybe ATypes.Value
          getValueOf key = HM.lookup key obj

          getGenger :: Maybe ATypes.Value -> Genger
          getGenger sex  = case sex of
                             Just (ATypes.Number 1.0) -> Female
                             Just (ATypes.Number 2.0) -> Male
                             _                        -> Unknown

          getDOB :: Maybe ATypes.Value -> Maybe DateOfBirth
          getDOB Nothing = Nothing
          getDOB (Just (ATypes.String bdate)) = (go_getDOB . Text.unpack) bdate
          -- getDOB распаковывает полученное значение даты рождения
          -- и передает функции go_getDOB распакованное значение в виде строки
          -- go_getDOB представляет логику, тогда как getDOB - интерфейс
          go_getDOB bdate
           |isMatches dmyMatch = Just $ (applyGroups3 . getGroups) dmyMatch
           |isMatches dmMatch  = Just $ (applyGroups2 . getGroups) dmMatch
           |otherwise          = Nothing
            where dmReg     = "^([1-2][0-9]|0[1-9]|3[0-1]).(0[1-9]|1[0-2])$"
                  dmyReg    = (init dmReg) ++ ".(19[0-9][0-9]|200[0-3])$"
                  isMatches = (not . null)
                  dmyMatch  = (bdate =~ dmyReg :: [[String]])
                  dmMatch   = (bdate =~ dmReg  :: [[String]])
                  getGroups match =
                    let groups = (tail . head) match
                    in map (read :: String -> Int) groups
                  applyGroups2 (d:m:_)   = DateOfBirth d m Nothing
                  applyGroups3 (d:m:y:_) = DateOfBirth d m (Just y)

          getPhone :: Maybe ATypes.Value -> Maybe PhoneNumber
          getPhone Nothing = Nothing
          getPhone (Just (ATypes.String val)) = (go_getPhone . Text.unpack) val
          go_getPhone phone =
            let parsedPhone = Phones.parsePhoneNumber phone
            in case parsedPhone of
               Unrecognized -> Nothing
               _            -> Just $ Phones.fromType parsedPhone

instance FromJSON City where
  parseJSON (Aeson.Object obj) =
    City <$> (obj .: "cid")
         <*> (obj .: "name")

instance FromJSON Country where
  parseJSON (Aeson.Object obj) =
    Country <$> (obj .: "cid")
            <*> (obj .: "name")

-----------------------------------------------------------------------------

checkDeactivated :: ATypes.Value -> Maybe ATypes.Value
checkDeactivated (Aeson.Object obj) =
  let deactivated = HM.lookup "deactivated" obj
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

handle :: (ATypes.Value -> Maybe a) -> Maybe LBS.ByteString -> Maybe [Maybe a]
handle parseFun Nothing     = Nothing
handle parseFun (Just json) =
  let vals = (M.fromJust . getResponseJSON) json
  in Just $ map parseFun vals
