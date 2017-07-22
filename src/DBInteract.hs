{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module DBInteract
       ( initDB
       , getStartValue
       , buildInsertQuery ) where

import           Types.DataBase
import qualified Internal.Utils         as Utils  (endsWith, join)
import qualified System.IO              as IO     (FilePath, readFile)
import qualified System.FilePath.Posix  as Posix  (pathSeparator)
import qualified System.Directory       as Dir    (doesFileExist
                                                  ,doesDirectoryExist
                                                  ,getCurrentDirectory)
import qualified Data.Text              as Text   (Text(..), pack)
import qualified Data.Int               as Int    (Int64)
import qualified Data.List.Split        as Split  (endBy)
import qualified Database.SQLite.Simple as SQLite (ResultError(..), Connection
                                                  ,open, close, execute_
                                                  ,query_)
import           Database.SQLite.Simple           (ToRow(..), SQLData(..))
import           Database.SQLite.Simple.Types     (Query(..))
import           Control.Exception                (catch)
import           Text.Printf                      (printf)

-- Basic functional
-----------------------------------------------------------------------------
refactorDBPath :: IO.FilePath -> IO (IO.FilePath)
refactorDBPath pathToDB = do
   isDirectory <- Dir.doesDirectoryExist pathToDB
   -- Если является директорией, то значение по умолчанию
   case isDirectory of
     True  -> defaultPath
     False -> return (checkFormat pathToDB)
  where defaultName = Posix.pathSeparator : "default"
        formatDB    = ".db"
        defaultPath = do
          currentDir <- Dir.getCurrentDirectory
          return (currentDir ++ defaultName ++ formatDB)
        checkFormat pathToDB
         |Utils.endsWith formatDB pathToDB = pathToDB
         |otherwise                        = pathToDB ++ formatDB

getInitSQL :: IO [Query]
getInitSQL = do
  currentDir <- Dir.getCurrentDirectory
  let pathToSQLInit = currentDir ++ (Posix.pathSeparator : "sql")
                                 ++ (Posix.pathSeparator : "init.sql")
  initSQL <- IO.readFile pathToSQLInit
  let delimiter = ";"
      splitted  = Split.endBy delimiter initSQL
      queries   = map (Query . Text.pack) splitted
  return (init queries)

-- | Инициализация базы данных.
-- Создает локуальную sqlite базу данных
-- в корне проекта со структурой, описанной
-- в 'init.sql'.
initDB :: IO.FilePath -> IO (SQLite.Connection)
initDB pathToDB = do
  rPathToDB <- refactorDBPath pathToDB
  isExist   <- Dir.doesFileExist rPathToDB
  conn      <- SQLite.open rPathToDB
  case isExist of
    True  -> return ()
    False -> do
      sqlInitCommands <- getInitSQL
      mapM_ (SQLite.execute_ conn) sqlInitCommands
  return conn

-- | Получает максимальное значение UID уже записанных в базу пользователей.
-- Возвращает значение, с которого следует продолжать поиск.
getStartValue :: SQLite.Connection -> IO Int
getStartValue conn = do
  [[maxUID]] <- catch (SQLite.query_ conn "SELECT MAX(UID) FROM USERS")
                      (\(SQLite.ConversionFailed _ _ _) -> do return [[0]])
  return (succ maxUID)

-----------------------------------------------------------------------------
buildInsertQuery :: String -> Int -> Query
buildInsertQuery tableName numColumns =
  let insertPattern = "INSERT INTO %s VALUES (%s)"
      columnSymbols = Utils.join "," ["?" | _ <- [1..numColumns]] :: String
  in (Query . Text.pack) $ printf insertPattern tableName columnSymbols

-- ToRow instances
-----------------------------------------------------------------------------

instance ToRow (User String) where
  toRow User{..} =
    let strCountry = maybeText userCountry
        strCity    = maybeText userCity
        strPhone   = maybeText userPhone
        strGenger  = case userGenger of
                     Male    -> SQLText "Мужской"
                     Female  -> SQLText "Женский"
                     Unknown -> SQLNull
        (d, m, y)  = case userBDate of
                     Nothing  -> (SQLNull, SQLNull, SQLNull)
                     Just val -> (toSQLInt (bdDay val)
                                 ,toSQLInt (bdMonth val)
                                 ,case (bdYear val) of
                                  Nothing -> SQLNull
                                  Just y  -> toSQLInt y)

    in toRow (userId, userFName, userSName, strGenger, d, m, y
             ,strCountry, strCity, strPhone)
    where maybeText :: Maybe String -> SQLData
          maybeText Nothing    = SQLNull
          maybeText (Just val) = SQLText (Text.pack val)

          toSQLInt :: Int -> SQLData
          toSQLInt val = SQLInteger (fromIntegral val :: Int.Int64)

instance ToRow SNetworkConnect where
  toRow SNetworkConnect{..} = worker sncUid sncSNetwork
    where worker userId (Instagram a) = toRow (userId, (SQLText "Instagram"), a)
          worker userId (Twitter   a) = toRow (userId, (SQLText "Twitter"),   a)
          worker userId (Facebook  a) = toRow (userId, (SQLText "Facebook"),  a)

instance ToRow University where
  toRow University{..} = toRow (universityId, universityTitle)

instance ToRow UniversityConnect where
  toRow UniversityConnect{..} = toRow (ucUid, ucUniversityId)

instance ToRow School where
  toRow School{..} = toRow (schoolId, schoolTitle)

instance ToRow SchoolConnect where
  toRow SchoolConnect{..} = toRow (scUid, scSchoolId)
