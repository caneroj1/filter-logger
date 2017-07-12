{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString                     as BS (length)
import           Network.Wai.Middleware.FilterLogger
import           Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  middleware filteringMiddleware
  post "/" $ text "SUCCESS"

filteringMiddleware =
  mkFilterLogger True keepShortBodies
  where keepShortBodies bs
          | BS.length bs < 10 = Just bs
          | otherwise         = Nothing
