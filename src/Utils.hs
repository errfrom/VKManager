{-# LANGUAGE OverloadedStrings #-}

module Utils
       (separateByCommas
       ,mapToString
       ,mapTuple
       ,join
       ,joinNoDel) where

import Data.Monoid (Monoid(..), (<>))

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
