{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Parser.Receiver
import           Parser.Handler
import           Data.Semigroup                    ((<>))
import           Control.Applicative               ((<**>))
import qualified Internal.Utils      as Utils      (joinByNewline, joinNoDel)
import qualified Collect             as Collect    (collect)
import           Collect                           (CollectOptions(..))
import qualified Options.Applicative as OptsParser (-- * Конструкторы
                                                     ParserInfo(..), Parser(..)
                                                    ,ParserPrefs(..)
                                                    -- * Строители 'ParserInfo'
                                                    ,fullDesc, progDesc, header
                                                    ,footer, info
                                                    -- * Строители 'Parser'
                                                    ,strOption, option, auto
                                                    -- * Строители 'ParserPrefs'
                                                    ,disambiguate, prefs
                                                    ,columns
                                                    -- * Модификторы
                                                    ,short, long, help, helpDoc
                                                    ,value, metavar, helper
                                                    ,showDefault, noArgError
                                                    ,command, subparser
                                                    -- * Другие функции
                                                    ,customExecParser)

type Options = Collect.CollectOptions

collectOptions :: OptsParser.Parser Options
collectOptions =
  let pathDBHelp      = "Путь к базе данных, существующей или нет."
      accessTokenHelp = "Ключ доступа для получения данных."
      startUIDHelp    = "Идентификатор пользователя, c которого следует начинать выгрузку."
      maxRecordsHelp  = "Максимальное число записей за один запуск."

  in CollectOpts <$> OptsParser.strOption
                       (OptsParser.short       'p'
                     <> OptsParser.long        "path-to-db"
                     <> OptsParser.help        pathDBHelp
                     <> OptsParser.value       "default.db"
                     <> OptsParser.showDefault)

                 <*> OptsParser.strOption
                       (OptsParser.short       't'
                     <> OptsParser.long        "access-token"
                     <> OptsParser.help        accessTokenHelp)

                 <*> OptsParser.option OptsParser.auto
                       (OptsParser.short       's'
                     <> OptsParser.long        "start-uid"
                     <> OptsParser.help        startUIDHelp
                     <> OptsParser.metavar     "INT"
                     <> OptsParser.value       0)

                 <*> OptsParser.option OptsParser.auto
                       (OptsParser.short       'm'
                     <> OptsParser.long        "max-records"
                     <> OptsParser.help        maxRecordsHelp
                     <> OptsParser.metavar     "INT"
                     <> OptsParser.value       100000
                     <> OptsParser.showDefault)

-- | Дополнительные настройки парсера
parserPrefs :: OptsParser.ParserPrefs
parserPrefs = OptsParser.prefs (OptsParser.disambiguate
                             <> OptsParser.columns 100)

-- | Общее описание программы, а также
-- некоторые советы по ее использованию
parserInfo :: OptsParser.ParserInfo Options
parserInfo =
  let hdr  = "WFurem - обработка и структуризация общедоступных данных."
      ftr  = Utils.joinByNewline [ Utils.joinNoDel
                                  [ "Пожалуйста, для безопасного завершения "
                                  , "выполнения, используете сочетание клавиш 'Ctrl+C'."]
                                 , Utils.joinNoDel
                                  [ "Своевременно запрашиваете новый "
                                  , "access-token для предсказуемой работы программы." ]]
  in OptsParser.info (collectOptions <**> OptsParser.helper)
                     (OptsParser.fullDesc
                   <> OptsParser.header   hdr
                   <> OptsParser.footer   ftr)

main :: IO ()
main = do
  collectOptions <- OptsParser.customExecParser parserPrefs parserInfo
  Collect.collect collectOptions
