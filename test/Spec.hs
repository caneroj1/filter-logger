{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad                                (void)
import qualified Data.ByteString                              as BS
import           Data.Monoid
import           Network.Wai.Middleware.FilterLogger.Internal
import           Test.HUnit.Base
import           Test.HUnit.Text

main :: IO ()
main = void . runTestTT . TestList $ basicTests ++ advancedTests

-- basic tests for log filtering functionality
basicTests :: [Test]
basicTests = map TestCase [
    noFilter
  , simpleFilterKeep
  , simpleFilterDiscard
  ]

-- slightly more complicated filters
advancedTests :: [Test]
advancedTests = map TestCase [
    doubleFilterKeep
  , doubleFilterDiscard
  ]

runFilterUnitTest :: Maybe BS.ByteString -> BS.ByteString -> LogFilter BS.ByteString -> Assertion
runFilterUnitTest expectation input lf = expectation @=? unwrap lf input

invert :: LogFilter a -> LogFilter a
invert lf = LogFilter (\a -> maybe (Just a) (const Nothing) $ operate lf a)

contains :: BS.ByteString -> BS.ByteString -> Bool
contains t = not . BS.null . snd . BS.breakSubstring t

simpleFilter :: BS.ByteString -> BS.ByteString -> Maybe BS.ByteString
simpleFilter t bs
  | contains t bs = Just bs
  | otherwise         = Nothing

noFilter :: Assertion
noFilter = runFilterUnitTest (Just "MyByteString") "MyByteString" mempty

simpleFilterKeep :: Assertion
simpleFilterKeep = runFilterUnitTest (Just "MyByteString") "MyByteString" testFilter
  where testFilter = mempty <> mkFilter (simpleFilter "Str")

simpleFilterDiscard :: Assertion
simpleFilterDiscard = runFilterUnitTest Nothing "MyByteString" testFilter
  where testFilter = mempty <> invert (mkFilter $ simpleFilter "Str")

doubleFilter :: LogFilter BS.ByteString
doubleFilter = mkFilter (simpleFilter "Test") <>
               mkFilter lenCheck
  where lenCheck s
          | BS.length s > 4 = Just s
          | otherwise       = Nothing

doubleFilterKeep :: Assertion
doubleFilterKeep = runFilterUnitTest (Just "UnitTest") "UnitTest" doubleFilter

doubleFilterDiscard :: Assertion
doubleFilterDiscard = runFilterUnitTest Nothing "UnitTest" (invert doubleFilter)
