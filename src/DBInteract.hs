{-# LANGUAGE OverloadedStrings #-}

module DBInteract
       (initDB
       ,getStartValue) where

import qualified Internal.Utils         as Utils  (endsWith)
import qualified System.IO              as IO     (FilePath, readFile)
import qualified System.FilePath.Posix  as Posix  (pathSeparator)
import qualified System.Directory       as Dir    (doesFileExist
                                                  ,doesDirectoryExist
                                                  ,getCurrentDirectory)
import qualified Data.Text              as Text   (pack)
import qualified Data.List.Split        as Split  (endBy)
import qualified Database.SQLite.Simple as SQLite (ResultError(..), Connection
                                                  ,open, close, execute_
                                                  ,query_)
import           Database.SQLite.Simple.Types     (Query(..))
import           Control.Exception                (catch)

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
