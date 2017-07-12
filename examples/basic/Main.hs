{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import qualified Data.ByteString                     as BS (elem, length)
import           Data.Char
import           Data.Word
import           Network.Wai.Middleware.FilterLogger
import           Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  middleware filteringMiddleware
  post "/" $ text "SUCCESS"

filteringMiddleware =
  mkFilterLogger True (keepShortBodies >=> containing 'c')
  where keepShortBodies bs
          | BS.length bs < 10 = Just bs
          | otherwise         = Nothing
        containing c bs
          | BS.elem (fromIntegral $ ord c) bs = Just bs
          | otherwise                         = Nothing
