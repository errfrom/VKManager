module Utils where

separateByCommas :: [String] -> String
separateByCommas l = tail $ foldl (\ x y -> x ++ "," ++ y) "" l

mapToString :: (Show a) => [a] -> [String]
mapToString = map show

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)
