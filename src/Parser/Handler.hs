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
       ( handleChunk
       , handleViaFun
       , parseCity
       , parseCountry ) where

-- В данном модуле не используются конструкторы-коннекторы и строители
import           Types.DataBase       hiding    (SchoolConnect(..)
                                                ,UniversityConnect(..)
                                                ,SNetworkConnect(..)
                                                ,connectUserSchools
                                                ,connectUserUniver)
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
import qualified Data.Maybe           as M      (fromJust, catMaybes)
import qualified Data.Vector          as Vec    (toList)
import qualified Data.Text            as Text   (Text(..), unpack)

-- FromJSON instances
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

instance FromJSON School where
  parseJSON (Aeson.Object obj) =
    School <$> (obj .: "id")
           <*> (obj .: "name")

instance FromJSON University where
  parseJSON (Aeson.Object obj) =
    University <$> (obj .: "university")
               <*> (obj .: "university_name")

instance FromJSON City where
  parseJSON (Aeson.Object obj) =
    City <$> (obj .: "cid")
         <*> (obj .: "name")

instance FromJSON Country where
  parseJSON (Aeson.Object obj) =
    Country <$> (obj .: "cid")
            <*> (obj .: "name")

instance FromJSON SocialNetworks where
  parseJSON (Aeson.Object obj) =
    SocialNetworks <$> (obj .:? "instagram")
                   <*> (obj .:? "twitter")
                   <*> (obj .:? "facebook")

-- Parse functions
-----------------------------------------------------------------------------

defaultParse :: (FromJSON a) => ATypes.Value -> Maybe a
defaultParse val = ATypes.parseMaybe parseJSON val

parseUser :: ATypes.Value -> Maybe User
parseUser val =
  let deactivated = checkDeactivated val
  in case deactivated of
    Just "deleted" -> Just Deleted
    Just "banned"  -> Just Banned
    Nothing        -> defaultParse val
  where checkDeactivated :: ATypes.Value -> Maybe ATypes.Value
        checkDeactivated (Aeson.Object obj) =
          let deactivated = HM.lookup "deactivated" obj
          in deactivated

parseCountry :: ATypes.Value -> Maybe Country
parseCountry = defaultParse

parseCity :: ATypes.Value -> Maybe City
parseCity = defaultParse

parseSchool :: ATypes.Value -> Maybe School
parseSchool = defaultParse

parseSNetworks :: ATypes.Value -> Maybe SocialNetworks
parseSNetworks = defaultParse

parseSchools :: ATypes.Value -> Maybe [Maybe School]
parseSchools (Aeson.Object obj) =
  case (HM.lookup "schools" obj) of
    Just arr -> let schoolObjs = (M.fromJust . fromArray) arr
                in Just $ map parseSchool schoolObjs
    Nothing  -> Nothing

parseUniversity :: ATypes.Value -> Maybe University
parseUniversity = defaultParse

-- Handle functions
-----------------------------------------------------------------------------

-- | По переданному JSON представлению пользователей
-- возвращает список DataChunk, где представлен
-- пользователь, его школ(а/ы), университет и другие
-- социальные сети, в которых он зарегистрирован
handleChunk :: LBS.ByteString -> Maybe [DataChunk]
handleChunk json =
  let respJSON = getResponseJSON json
  in case respJSON of
     Nothing   -> Nothing
     Just vals -> Just $ worker vals
  where worker :: [ATypes.Value] -> [DataChunk]
        worker []       = []
        worker (val:xs) =
          let mUser     = parseUser val
              schools   = M.catMaybes <$> parseSchools val
              univer    = parseUniversity val
              sNetworks = parseSNetworks val
          in case mUser of
            Just user -> DataChunk{..} : (worker xs)
            Nothing   -> worker xs

-- | Обработка, основанная на логике parseFun
-- Полиморфная сущность этой функции, позволяет
-- обобщенно обрабатывать JSON представления
-- городов и стран
handleViaFun :: (ATypes.Value -> Maybe a) -> LBS.ByteString -> Maybe [Maybe a]
handleViaFun parseFun json =
  case (getResponseJSON json) of
    Nothing   -> Nothing
    Just vals -> Just $ map parseFun vals

-- Additional functions
-----------------------------------------------------------------------------

fromArray :: ATypes.Value -> Maybe [ATypes.Value]
fromArray (ATypes.Array arr) = Just (Vec.toList arr)
fromArray _                  = Nothing

getResponseJSON :: LBS.ByteString -> Maybe [ATypes.Value]
getResponseJSON json =
  let responseJson =  json ^? ALens.key "response"
  in case responseJson of
     Just arr -> fromArray arr
     _        -> Nothing
