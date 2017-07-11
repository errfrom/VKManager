{-# LANGUAGE OverloadedStrings #-}

module Types.Receive where

import qualified Data.Map.Strict as Map  (Map (..))

data Protocol =
  HTTP
 |HTTPS

data URL =
  URL {protocol    :: Protocol
      ,body        :: String
      ,queryParams :: Map.Map String String}

type AccessToken = String
type Id          = Int
