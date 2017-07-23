{-# LANGUAGE RecordWildCards #-}

module Collect
       ( CollectOptions(..)
       , collect ) where

import qualified Parser.Receiver          as Receiver
import qualified Parser.Handler           as Handler
import qualified Internal.Utils           as Utils     (cleanAndPrint)
import           Types.DataBase
-- DataBase interaction
import qualified DBInteract               as DB
import qualified Database.SQLite.Simple   as SQLite    (Connection, execute
                                                       ,close)
import           Database.SQLite.Simple
-- Data modules
import qualified Data.Text                as Text      (pack)
import qualified Data.List                as List      (nub)
import qualified Data.Maybe               as M         (catMaybes, maybe)
import qualified Data.ByteString.Lazy     as LBS       (ByteString(..))
import qualified Data.IORef               as IORef     (IORef(..), newIORef, readIORef
                                                       ,modifyIORef')

data CollectOptions = CollectOpts
  { pathDB      :: String
  , accessToken :: String
  , startUID    :: Int
  , maxRecords  :: Int }

-- | Оставляет только те наборы данных,
-- значение user которых представляет
-- действительного пользователя.
removeInvalidDataChunks :: [DataChunk Int] -> [DataChunk Int]
removeInvalidDataChunks [] = []
removeInvalidDataChunks (dc@(DataChunk{..}):dataChunks) =
  case dcUser of
    Deleted -> checkNext
    Banned  -> checkNext
    _       -> dc : checkNext
  where checkNext = removeInvalidDataChunks dataChunks

-- | По данным наборам данным и функции, возвращающей
-- значение определенного поля user,
-- возвращает множество значений требуемого поля.
getUserFieldsFromDataChunks :: [DataChunk Int] -> (User Int -> Maybe Int) -> [Int]
getUserFieldsFromDataChunks dataChunks field = (List.nub . M.catMaybes . worker) dataChunks
  where worker (DataChunk{..}:chunks) = (field dcUser) : worker chunks
        worker [] = []

getCityIds :: [DataChunk Int] -> [Int]
getCityIds chunks = getUserFieldsFromDataChunks chunks userCity

getCountryIds :: [DataChunk Int] -> [Int]
getCountryIds chunks = getUserFieldsFromDataChunks chunks userCountry

-- | Обновляет значения city и country поля user в каждом валидном
-- наборе данных. Заменяет идентификаторы на названия, т.е.
-- числовые значения на строковые.
updateChunks :: [DataChunk Int] -> [City] -> [Country] -> [DataChunk String]
updateChunks [] _ _ = []
updateChunks (dc@(DataChunk{dcUser = u@User{..}}):chunks) cities countries =
  let cityName    = getCityNameById userCity cities
      countryName = getCountryNameById userCountry countries
      updatedDC   = dc {dcUser = u {userCity = cityName, userCountry = countryName}}
  in updatedDC : (updateChunks chunks cities countries)
  where getCityNameById Nothing _ = Nothing
        getCityNameById _ []      = Nothing
        getCityNameById v@(Just cid) (City{..}:cities)
         |cid == cityId = Just cityName
         |otherwise     = getCityNameById v cities

        getCountryNameById Nothing _ = Nothing
        getCountryNameById _ []      = Nothing
        getCountryNameById v@(Just cid) (Country{..}:countries)
         |cid == countryId = Just countryName
         |otherwise        = getCountryNameById v countries

-- TODO: refactor
-- | Подготавливает набор данных для записи,
-- создавая промежуточные экземпляры типов
-- и, непосредственно, осуществляет запись в базу данных.
-- По окончанию записи, обновляет счетчик.
writeToDB :: (Enum a, Show a) => SQLite.Connection -> IORef.IORef a -> DataChunk String -> IO()
writeToDB conn counterRef DataChunk{..} =
  let usersQuery      = DB.buildInsertQuery "USERS"         10
      schoolQuery     = DB.buildInsertQuery "SCHOOLS"       2
      univerQuery     = DB.buildInsertQuery "UNIVERSITIES"  2
      schoolConnQuery = DB.buildInsertQuery "USERS_SCHOOLS" 2
      univerConnQuery = DB.buildInsertQuery "USERS_UNIVERS" 2
      userSNConnQuery = DB.buildInsertQuery "USERS_SNS"     3
      uid             = userId dcUser
      univerConn = UniversityConnect uid <$> (universityId <$> dcUniver)
      schoolConn = case dcSchools of
                     Nothing  -> Nothing
                     (Just a) -> Just $ map (SchoolConnect uid . schoolId) a
      snConn     = case dcSNetworks of
                     Nothing -> Nothing
                     (Just a) -> Just $ map (SNetworkConnect uid) a
  in do
    DB.failoverExecute conn usersQuery                       dcUser
    maybeDoNothing (executeMany conn schoolQuery)            dcSchools
    maybeDoNothing (DB.failoverExecute conn univerQuery)     dcUniver
    maybeDoNothing (DB.failoverExecute conn univerConnQuery) univerConn
    maybeDoNothing (executeMany conn schoolConnQuery)        schoolConn
    maybeDoNothing (executeMany conn userSNConnQuery)        snConn
    updateCounter
  where updateCounter  = counter counterRef
        maybeDoNothing = M.maybe (return())
        executeMany conn query a = mapM_ (DB.failoverExecute conn query) a

getValidChunks :: Maybe (LBS.ByteString) -> Maybe [DataChunk Int]
getValidChunks mRespUsers = do
  respUsers   <- mRespUsers
  mixedChunks <- Handler.handleChunks respUsers
  return (removeInvalidDataChunks mixedChunks)

-- | Простой счетчик, при каждом вызове обновляющий переменную.
counter :: (Enum a, Show a) => IORef.IORef a -> IO ()
counter a = do
  IORef.modifyIORef' a succ
  value <- IORef.readIORef a
  let outMsg = "Обработано записей: " ++ (show value)
  Utils.cleanAndPrint outMsg False

-- | Является точкой входа команды 'collect'.
collect :: CollectOptions -> IO()
collect opts@CollectOpts{..} = do
  conn  <- DB.initDB pathDB
  start <- if (startUID == 0)
             then (DB.getStartValue conn)
             else (return startUID)
  cRef  <- IORef.newIORef 0
  let query  = 500
      limit  = start + maxRecords
      ranges = getRanges query start limit
  mapM_ (worker cRef conn accessToken) ranges
  putStrLn "\nГотово."
  where worker ref conn accessToken range = do
          -- ^ Порядок выполнения:
          -- 1. Получение JSON представлений пользователей.
          -- 2. Формирование необработанных наборов данных.
          -- 3. Удаление невалидных наборов данных.
          -- 4. Получение идентификаторов стран и городов.
          -- 5. Получение названий по идентификаторам.
          -- 6. Обновление наборов данных.
          -- 7. Передача управления функции записи в базу.
          respUsers   <- Receiver.receiveUsers accessToken range

          let mValidChunks = getValidChunks respUsers
              idCities    = M.maybe [] getCityIds    mValidChunks
              idCountries = M.maybe [] getCountryIds mValidChunks

          mRespCities    <- Receiver.receiveCities    accessToken idCities
          mRespCountries <- Receiver.receiveCountries accessToken idCountries

          let updatedChunks = do
               respCities    <- mRespCities
               respCountries <- mRespCountries
               validChunks   <- mValidChunks
               cities        <- Handler.handleCities respCities
               countries     <- Handler.handleCountries respCountries
               return (updateChunks validChunks cities countries)

          M.maybe (putStrLn "Возникла ошибка во время выполнения.")
                  (mapM_ (writeToDB conn ref)) updatedChunks

        getRanges :: (Enum a, Num a, Ord a) => a -> a -> a -> [[a]]
        getRanges oneRange start limit =
          -- ^ Возвращает список промежутков по oneRange
          -- элементов в каждом от start до limit.
          let until' = start + oneRange
            in (if (until' < limit)
                then [start..until'] : (getRanges oneRange (succ until') limit)
                else [[start..limit]])
