{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE FlexibleInstances #-}

module DBInteract
       ( initDB
       , getStartValue
       , buildInsertQuery
       , failoverExecute
       , setKeyboardInterruptHandler ) where

import           Types.DataBase
import qualified Internal.Utils         as Utils   (endsWith, join
                                                   ,cleanAndPrint)
import qualified System.IO              as IO      (FilePath, readFile)
import qualified System.Exit            as Exit    (ExitCode(..))
import qualified System.FilePath.Posix  as Posix   (pathSeparator)
import qualified System.Directory       as Dir     (doesFileExist
                                                   ,doesDirectoryExist
                                                   ,getCurrentDirectory)
import qualified Data.Text              as Text    (Text(..), pack)
import qualified Data.Int               as Int     (Int64)
import qualified Data.List.Split        as Split   (endBy)
import qualified Database.SQLite.Simple as SQLite  (ResultError(..), Statement(..)
                                                   ,Connection
                                                   ,open, close
                                                   ,closeStatement, bind
                                                   ,withStatement, execute_
                                                   ,execute, query_)
import qualified Control.Exception      as Exc     (throwTo)
import qualified Control.Concurrent     as Conc    (myThreadId)
import           Control.Monad                     (void)
import qualified System.Posix.Signals   as Signals (Handler(..)
                                                   ,installHandler
                                                   ,keyboardSignal)
import           Database.SQLite.Simple            (ToRow(..), SQLData(..))
import           Database.SQLite.Simple.Types      (Query(..))
import qualified Database.SQLite3       as Base    (step)
import           Database.SQLite3                  (SQLError(..), Error(..))
import           Control.Exception                 (catch, handle)
import           Text.Printf                       (printf)

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
      putStrLn "Инициализация базы данных..."
      sqlInitCommands <- getInitSQL
      mapM_ (SQLite.execute_ conn) sqlInitCommands
      putStrLn "Инициализировано."
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

failoverExecute :: (ToRow t) => SQLite.Connection -> Query -> t -> IO ()
failoverExecute conn query params =
   withStatementParams conn query params $ \statmt@(SQLite.Statement baseStatmt) -> do
    setKeyboardInterruptHandler conn statmt
    checkErrors `handle` (void . Base.step $ baseStatmt)
  where withStatementParams :: (ToRow params)
                               => SQLite.Connection
                               -> Query
                               -> params
                               -> (SQLite.Statement -> IO a)
                               -> IO a
        withStatementParams conn query params action =
          SQLite.withStatement conn query $ \statmt ->
            SQLite.bind statmt (toRow params) >> action statmt

        checkErrors :: SQLError -> IO ()
        checkErrors (SQLError ErrorConstraint _ _) = return ()
        checkErrors _ = do
          Utils.cleanAndPrint "Ошибка записи в базу. Завершение работы..." True
          SQLite.close conn

-- NOTE: При безопасном завершении, возникает 'Segmentation fault: 11'.
--       Однако, не замечено связанных с этим проблем.
-- Обрабатывает завершение нажатием сочетиния клавиш 'Ctrl+C'.
-- Закрывает последний statement и соединение с базой данных, тем самым
-- обеспечивая сохранность данных при импровизированном завершении.
setKeyboardInterruptHandler :: SQLite.Connection -> SQLite.Statement
                                                 -> IO Signals.Handler
setKeyboardInterruptHandler conn statmt =
  let catchExit = Signals.Catch (safeExit conn statmt)
  in Signals.installHandler Signals.keyboardSignal catchExit Nothing
  where terminatedCode = 130
        safeExit conn statmt  = do
          putStrLn "Безопасное завершение..."
          SQLite.closeStatement statmt
          SQLite.close conn
          tid <- Conc.myThreadId
          Exc.throwTo tid (Exit.ExitFailure terminatedCode)

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
