{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Types.DataBase
-- Copyright   :  (c) Ivanov Dmitry, 2017
-- License     :  MIT license
-- Maintainer  :  errfrom@yandex.ru
--
-- Описывает используемые типы
-- данных, по своей семантике
-- разделяющиеся на вспомогательные,
-- основные и связующие.
-- Основные типы данных описывают
-- полученные с помощью API данные,
-- связующие описывают схему ДБ,
-- вспомогательные некоторым
-- способом расширяют функционал
-- не изменяя основной логики
-- программы.
-----------------------------------------------------------------------------

module Types.DataBase
       ( -- * Types
         PhoneNumber
         -- * General data constructors
       , DataChunk(..)
         -- * Data constructors that wraps VK API
       , User(..)
       , University(..)
       , School(..)
       , Country(..)
       , City(..)
       , SocialNetwork(..)
         -- * 'Connectors' data constructors
       , SchoolConnect(..)
       , UniversityConnect(..)
       , SNetworkConnect(..)
         -- * Internal data constructors
       , Genger(..)
       , BDate(..)
         -- * Data constructor builders
       , connectUserSchools
       , connectUserUniver ) where

-- Types & Types Constructors
-----------------------------------------------------------------------------

type PhoneNumber = String

data DataChunk a = DataChunk
  { dcUser      :: User a
  , dcSchools   :: Maybe [School]
  , dcUniver    :: Maybe University
  , dcSNetworks :: Maybe [SocialNetwork] }
  deriving (Show)

data Genger =
    Male
  | Female
  | Unknown
  deriving (Show)

-- ^ Используется полиморфный тип a,
-- т.к. изначально страны и города
-- представлены их идентификаторами типа Int,
-- а потом заменяются на их названия строкового типа.
data User a = User
  { userId      :: Int
  , userFName   :: String
  , userSName   :: String
  , userGenger  :: Genger
  , userBDate   :: Maybe BDate
  , userCountry :: Maybe a
  , userCity    :: Maybe a
  , userPhone   :: Maybe PhoneNumber }
  | Deleted
  | Banned
  deriving (Show)

data BDate = BDate
  { bdDay   :: Int
  , bdMonth :: Int
  , bdYear  :: Maybe Int }
  deriving (Show)

data University = University
  { universityId    :: Int
  , universityTitle :: String }
  deriving (Show)

data School = School
  {schoolId    :: Int
  ,schoolTitle :: String }
  deriving (Show)

data SchoolConnect = SchoolConnect
  { scUid      :: Int
  , scSchoolId :: Int }
  deriving (Show)

data UniversityConnect = UniversityConnect
  { ucUid          :: Int
  , ucUniversityId :: Int }
  deriving (Show)

data SocialNetwork =
    Instagram String
  | Twitter   String
  | Facebook  String
  deriving (Show)

data SNetworkConnect = SNetworkConnect
  { sncUid      :: Int
  , sncSNetwork :: SocialNetwork }
  deriving (Show)

data Country = Country
  { countryId   :: Int
  , countryName :: String }
  deriving (Show)

data City = City
  { cityId   :: Int
  , cityName :: String }
  deriving (Show)

-- Data constructor builders
-----------------------------------------------------------------------------
connectUserSchools :: User a -> [School] -> [SchoolConnect]
connectUserSchools _ [] = []
connectUserSchools user@User{..} (School{..}:schools) =
  SchoolConnect userId schoolId : connectUserSchools user schools

connectUserUniver :: User a -> University -> UniversityConnect
connectUserUniver User{..} University{..} =
  UniversityConnect userId universityId
