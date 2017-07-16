{-# LANGUAGE RecordWildCards #-}

module Collect
       (CollectOptions(..)
       ,collect) where

import qualified Parser.Receiver as Receiver
import qualified Parser.Handler  as Handler
import qualified DBInteract      as DB
import           Types.DataBase
import qualified Data.List       as List (nub)
import qualified Data.Maybe      as M    (catMaybes, fromJust)

data CollectOptions =
  CollectOpts { pathDB      :: String
              , accessToken :: String
              , startUID    :: Int
              , maxRecords  :: Int }

-- | Оставляет только те наборы данных,
-- значение user которых представляет
-- действительного пользователя.
removeInvalidDataChunks :: [DataChunk Int] -> [DataChunk Int]
removeInvalidDataChunks [] = []
removeInvalidDataChunks (dc@(DataChunk{..}):dataChunks) =
  case user of
    Deleted -> checkNext
    Banned  -> checkNext
    _       -> dc : checkNext
  where checkNext = removeInvalidDataChunks dataChunks

-- | По данным наборам данным и функции, возвращающей
-- значение определенного поля user,
-- возвращает множество значений требуемого поля.
getUserFieldsFromDataChunks :: [DataChunk Int] -> (User Int -> Maybe Int) -> [Int]
getUserFieldsFromDataChunks dataChunks field = (List.nub . M.catMaybes . worker) dataChunks
  where worker (DataChunk{..}:chunks) = (field user) : worker chunks
        worker [] = []

getCityIds :: [DataChunk Int] -> [Int]
getCityIds chunks = getUserFieldsFromDataChunks chunks city

getCountryIds :: [DataChunk Int] -> [Int]
getCountryIds chunks = getUserFieldsFromDataChunks chunks country

-- | Обновляет значения city и country поля user в каждом валидном
-- наборе данных. Заменяет идентификаторы на названия, т.е.
-- числовые значения на строковые.
updateChunks :: [DataChunk Int] -> [City] -> [Country] -> [DataChunk String]
updateChunks [] _ _ = []
updateChunks (dc@(DataChunk{user = u@User{..}}):chunks) cities countries =
  let cityName    = getCityNameById city cities
      countryName = getCountryNameById country countries
      updatedDC   = dc {user = u {city = cityName, country = countryName}}
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

writeToDB :: DataChunk String -> IO()
writeToDB dataChunk = print "TODO + обновление счетчика"

collect :: CollectOptions -> IO()
collect opts@CollectOpts{..} =
  let limit         = startUID + maxRecords
      queryQuantity = 500
      ranges        = getRanges queryQuantity startUID limit
  --in map (worker opts) ranges
  in print "work"
  where worker opts@CollectOpts{..} range = do
          -- ^ Порядок выполнения:
          -- 1. Получение JSON представлений пользователей.
          -- 2. Формирование необработанных наборов данных.
          -- 3. Удаление невалидных наборов данных.
          -- 4. Получение идентификаторов стран и городов.
          -- 5. Получение названий по идентификаторам.
          -- 6. Обновление наборов данных.
          -- 7. Передача управления функции записи в базу.
          respUsers <- Receiver.receive Receiver.usersURL range accessToken

          let validChunks = M.fromJust $ do -- TODO: Избавиться от fromJust.
                                            --       Может быть причиной многих ошибок.
                respUsers'  <- respUsers
                mixedChunks <- Handler.handleChunks respUsers'
                return (removeInvalidDataChunks mixedChunks)
              idCities    = getCityIds validChunks
              idCountries = getCountryIds validChunks

          respCities    <- Receiver.receive Receiver.citiesURL idCities accessToken
          respCountries <- Receiver.receive Receiver.countriesURL idCountries accessToken

          let updatedChunks = M.fromJust $ do
                respCities'    <- respCities
                respCountries' <- respCountries
                cities         <- (Handler.handleViaFun Handler.parseCity)    respCities'
                countries      <- (Handler.handleViaFun Handler.parseCountry) respCountries'
                return (updateChunks validChunks cities countries)
          -- Запись в базу
          mapM_ writeToDB updatedChunks

        getRanges :: (Enum a, Num a, Ord a) => a -> a -> a -> [[a]]
        getRanges oneRange start limit =
          -- ^ Возвращает список промежутков по oneRange
          -- элементов в каждом от start до limit.
          let until' = start + oneRange
            in (if (until' < limit)
                then [start..until'] : (getRanges oneRange (succ until') limit)
                else [[start..limit]])
