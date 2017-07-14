module Collect
       ( CollectOptions(..)
       , collect) where

data CollectOptions =
  CollectOpts { pathDB         :: String
              , accessToken    :: String
              , startUID       :: Int
              , maxRecords     :: Int }

collect :: CollectOptions -> IO ()
collect collectOptions = print "blank"
