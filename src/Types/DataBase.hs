{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Types.DataBase where

import           Data.Aeson                 (FromJSON(..), Value(Object)
                                            ,withObject, (.:), (.:?), (.!=))
--import qualified Data.Aeson.Types as ATypes (Parser(..))

data Genger =
  Male
 |Female
 |Unknown

data DateOfBirth =
  DateOfBirth {year  :: Maybe Int
              ,month :: Maybe Int
              ,day   :: Maybe Int }

data PhoneNumber =
  PhoneNumber {countryCode :: Int
              ,phoneNumber :: Int }

data User = -- TODO: сделать проверку для вложенных конструкторов и обработку JSON
  User {uid     :: Int
       ,fName   :: String
       ,sName   :: String
       ,genger  :: String         -- Genger
       ,dob     :: Maybe String   -- DateOfBirth
       ,country :: Maybe String
       ,city    :: Maybe String
       ,phone   :: Maybe String } -- PhoneNumber
 |Deleted
 |Banned

data University =
  University {universityId    :: Int
             ,universityTitle :: String }

data School =
  School {schoolId    :: Int
         ,schoolTitle :: String }

data SocialNetwork =
  SocialNetwork {socialNetworkId    :: Int
                ,sociakNetworkTitle :: String }

class DbInteractional t where
  writeDb ::  t -> IO ()

instance FromJSON User where
  parseJSON = withObject "user" $ \obj -> do
    deactivated <- obj .:? "deactivated"
    case deactivated of
      "deleted" -> pure Deleted
      "banned"  -> Banned
      _         -> User <$> (obj .:  "uid")
                        <*> (obj .:  "first_name")
                        <*> (obj .:  "last_name")
                        <*> (obj .:  "sex")
                        <*> (obj .:? "bdate"        .!= Nothing)
                        <*> (obj .:? "country"      .!= Nothing)
                        <*> (obj .:? "city"         .!= Nothing)
                        <*> (obj .:? "mobile_phone" .!= Nothing)
