{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Internal.Phones
-- Copyright   :  (c) Ivanov Dmitry, 2017
-- License     :  MIT license
-- Maintainer  :  errfrom@yandex.ru
--
-- Модуль, представляющий интерфейс
-- проверки корректности полученного
-- номера телефона. Также предоставляет
-- возможность определить, в какой
-- стране данный номер зарегистрирован.
-----------------------------------------------------------------------------

module Internal.Phones
       ( PhoneType(..)
       , parsePhoneNumber
       , fromType ) where

import qualified Internal.Utils   as Utils (removeSpaces, removeManyFromString
                                           ,joinNoDel)
import           Types.DataBase            (PhoneNumber)
import           Text.Regex       as Regex (Regex(..)
                                           ,mkRegexWithOpts, matchRegex)

data PhoneType =
   -- ^ Наиболее распространены номера телефонов,
   --   соостветствующие стандартам стран СНГ (ВКонтакте)

   -- | Российский номер
   -- Может присутствовать '+' в начале
   -- Перед номером может быть или цифра '7', или '8'
   -- Номер в целом состоит из 10 цифр (не меньше)
   -- Строка номера может содержать скобочки '()' или дефисы '-'
   RussianPhone     PhoneNumber
   -- | Украинский номер
   -- Может присутствовать '+' в начале
   -- Перед номером может стоять число '380' или '0'
   -- Должен быть обязательно указан номер оператора
   -- Номер в целом состоит из 9 символов (не меньше)
   -- Строка номера может содержать скобочки '()' или дефисы '-'
 | UkrainianPhone   PhoneNumber
   -- | Белорусский номер
   -- Может присутствовать '+' в начале
   -- Перед номером может стоять число '375'
   -- Должен быть обязательно указан номер оператора
   -- Номера операторов: 24, 25, 29, 33, 44
   -- Номер в целом состоит из 9 символов (не меньше)
   -- Строка номера может содержать скобочки '()' или дефисы '-'
 | BelorussianPhone PhoneNumber
 | Unrecognized
   deriving (Show)

data PhoneParserParams =
  PhoneParserParams {allowedSymbols :: String
                    ,regex          :: Regex}

fromType :: PhoneType -> PhoneNumber
fromType (RussianPhone a)     = a
fromType (UkrainianPhone a)   = a
fromType (BelorussianPhone a) = a 

-- | На основе переданной строки, решает, является
--   ли данный номер телефона существующим.
parsePhoneNumber :: PhoneNumber -> PhoneType
parsePhoneNumber phone
 |isMatches rus = RussianPhone     (getClean rus phoneNoSpaces)
 |isMatches bel = BelorussianPhone (getClean bel phoneNoSpaces)
 |isMatches ukr = UkrainianPhone   (getClean ukr phoneNoSpaces)
 |otherwise = Unrecognized
  where phoneNoSpaces  = Utils.removeSpaces phone
        getClean :: PhoneParserParams -> PhoneNumber -> PhoneNumber
        getClean params = Utils.removeManyFromString (allowedSymbols params)

        mkRegex :: String -> Regex
        mkRegex = (\reg -> Regex.mkRegexWithOpts reg True True)

        isMatches :: PhoneParserParams -> Bool
        isMatches params@PhoneParserParams{..} =
          let go = (Regex.matchRegex regex . getClean params)
          in case (go phoneNoSpaces) of
             Just _   -> True
             Nothing  -> False

        -- | Символы, наличие которых является допустимым
        --   по умолчанию
        symsAllowedByDef = "()-"

        -- | Российский номер
        rus = let reg = "(^[+][7-8][0-9]{10}$)|(^[7-8]?[0-9]{10}$)"
              in PhoneParserParams symsAllowedByDef (mkRegex reg)
        -- | Белорусский номер
        bel = let reg = Utils.joinNoDel [ "(^[+]375(24|25|29|33|44)[0-9]{7}$)|"
                                        , "(^(375)?(24|25|29|33|44)[0-9]{7}$)" ]
              in PhoneParserParams symsAllowedByDef (mkRegex reg)
        -- | Украинский номер
        ukr = let reg = "(^[+](380|0)[0-9]{9}$)|(^(380|0)?[0-9]{9}$)"
              in PhoneParserParams symsAllowedByDef (mkRegex reg)
