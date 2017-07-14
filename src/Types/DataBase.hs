{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
       , SocialNetworks(..)
         -- * 'Connectors' data constructors
       , SchoolConnect(..)
       , UniversityConnect(..)
       , SNetworkConnect(..)
         -- * Internal data constructors
       , Genger(..)
       , DateOfBirth(..)
         -- * Data constructor builders
       , connectUserSchools
       , connectUserUniver ) where

-- Types & Types Constructors
-----------------------------------------------------------------------------

type PhoneNumber = String

data DataChunk = DataChunk
  { user      :: User
  , schools   :: Maybe [School]
  , univer    :: Maybe University
  , sNetworks :: Maybe SocialNetworks }
  deriving (Show)

data Genger =
   Male
 | Female
 | Unknown
   deriving (Show)

data User =
   User { uid     :: Int
        , fName   :: String
        , sName   :: String
        , genger  :: Genger
        , dob     :: Maybe DateOfBirth
        , country :: Maybe Int
        , city    :: Maybe Int
        , phone   :: Maybe PhoneNumber }
 | Deleted
 | Banned
   deriving (Show)

data DateOfBirth = DateOfBirth
  { day   :: Int
  , month :: Int
  , year  :: Maybe Int }
  deriving (Show)

data University = University
  { universityId    :: Int
  , universityTitle :: String }
  deriving (Show)

data School = School
  {schoolId    :: Int
  ,schoolTitle :: String}
  deriving (Show)

data SchoolConnect = SchoolConnect
  { uid      :: Int
  , schoolId :: Int }
  deriving (Show)

data UniversityConnect = UniversityConnect
  { uid          :: Int
  , universityId :: Int}
  deriving (Show)

data SocialNetworks = SocialNetworks
  { instagram :: Maybe String
  , twitter   :: Maybe String
  , facebook  :: Maybe String }
  deriving (Show)

data SNetworkConnect = SNetworkConnect
  { uid           :: Int
  , socialNetwork :: SocialNetworks}
  deriving (Show)

data Country = Country
  { cid  :: Int
  , name :: String }
  deriving (Show)

data City = City
  { cid  :: Int
  , name :: String }
  deriving (Show)

-- Data constructor builders
-----------------------------------------------------------------------------
connectUserSchools :: User -> [School] -> [SchoolConnect]
connectUserSchools _ [] = []
connectUserSchools user@User{..} (School{..}:schools) =
  SchoolConnect{..} : connectUserSchools user schools

connectUserUniver :: User -> University -> UniversityConnect
connectUserUniver User{..} University{..} = UniversityConnect{..}
