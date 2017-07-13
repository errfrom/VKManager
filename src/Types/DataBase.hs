{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Types.DataBase where

type PhoneNumber = String

data Genger =
  Male
 |Female
 |Unknown
  deriving (Show)

data DateOfBirth =
  DateOfBirth {day   :: Int
              ,month :: Int
              ,year  :: Maybe Int}
  deriving (Show)

data User =
  User {uid     :: Int
       ,fName   :: String
       ,sName   :: String
       ,genger  :: Genger
       ,dob     :: Maybe DateOfBirth
       ,country :: Maybe Int
       ,city    :: Maybe Int
       ,phone   :: Maybe PhoneNumber}
 |Deleted
 |Banned
  deriving (Show)

data University =
  University {universityId    :: Int
             ,universityTitle :: String}

data School =
  School {schoolId    :: Int
         ,schoolTitle :: String}

data SocialNetwork =
  SocialNetwork {socialNetworkId    :: Int
                ,sociakNetworkTitle :: String}

data Country =
  Country {cid  :: Int
          ,name :: String}
  deriving (Show)

data City =
  City {cid  :: Int
       ,name :: String}
  deriving (Show)
