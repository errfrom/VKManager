{-# LANGUAGE OverloadedStrings #-}

module Internal.Utils
       (separateByCommas
       ,mapToString
       ,mapTuple
       ,join
       ,joinNoDel
       ,joinByNewline
       ,removeFromString
       ,removeSpaces
       ,removeManyFromString
       ,endsWith
       ,cleanAndPrint) where

import           Data.Monoid        (Monoid(..), (<>))
import           Control.Concurrent (threadDelay)
import qualified Data.Text as Text  (isSuffixOf, pack)
import qualified System.IO as Sys   (stdout, hPutStr, hFlush)

separateByCommas :: [String] -> String
separateByCommas l = tail $ foldl (\ x y -> x ++ "," ++ y) "" l

mapToString :: (Show a) => [a] -> [String]
mapToString = map show

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

join :: (Monoid mon) => mon -> [mon] -> mon
join delimiter (x:xs) =
  let folded = foldl (\a b -> a <> delimiter <> b) (mempty delimiter) xs
  in x <> folded

joinNoDel :: (Monoid mon) => [mon] -> mon
joinNoDel mons@(mon:_) = join (mempty mon) mons

joinByNewline :: [String] -> String
joinByNewline mons = join "\n" mons

removeFromString :: Char -> String -> String
removeFromString _ [] = []
removeFromString ch (x:xs)
 |x == ch   = removeFromString ch xs
 |otherwise = x : (removeFromString ch xs)

removeSpaces :: String -> String
removeSpaces = (\string -> removeFromString ' ' string)

removeManyFromString :: String -> String -> String
removeManyFromString []      string = string
removeManyFromString (ch:cs) string =
  let nstring = removeFromString ch string
  in removeManyFromString cs nstring

endsWith :: String -> String -> Bool
endsWith sub str = Text.isSuffixOf (Text.pack sub) (Text.pack str)

cleanAndPrint :: String -> Bool -> IO ()
cleanAndPrint text withAdditionalSpace = do
  Sys.hPutStr Sys.stdout ("\r" ++ text ++ (case withAdditionalSpace of
                                           True  -> ([1..100] >> " ")
                                           False -> ""))
  Sys.hFlush Sys.stdout
