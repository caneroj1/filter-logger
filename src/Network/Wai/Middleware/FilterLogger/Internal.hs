{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Wai.Middleware.FilterLogger.Internal where

import           Control.Monad
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS hiding (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString, fromStrict)
import           Data.Semigroup
import           Network.Wai
import           System.IO.Unsafe

-- | Typeclass for types that can be converted into from a strict ByteString and will be used as
-- arguments to 'LogFilter'
class LogFilterable a where
  prep :: ByteString -> Maybe a

instance LogFilterable ByteString where
  prep = return

instance LogFilterable BL.ByteString where
  prep = return . BL.fromStrict

-- | Type that represents a log filtering function. If the return type
-- is Nothing, then no log message will be created. Otherwise, a log message
-- will be created using the (potentially modified) returned value
newtype LogFilter a = LogFilter {
    operate :: (LogFilterable a) => a -> Maybe a
  }

instance Semigroup (LogFilter a) where
  (<>) = sequenceFilter

instance Monoid (LogFilter a) where
  mempty  = LogFilter Just
  mappend = (<>)

-- | Constructor function for 'LogFilter' types.
mkFilter :: (LogFilterable a) => (a -> Maybe a) -> LogFilter a
mkFilter = LogFilter

logFilter :: (LogFilterable a) => ByteString -> LogFilter a -> Maybe a
logFilter reqBody lf = case prep reqBody of
  (Just a) -> operate lf a
  Nothing  -> Nothing

unwrap :: (LogFilterable a) => LogFilter a -> (a -> Maybe a)
unwrap = operate

sequenceFilter :: LogFilter a -> LogFilter a -> LogFilter a
sequenceFilter llf rlf = LogFilter (operate llf >=> operate rlf)

-- | Given a 'LogFilter', construct a 'Middleware' value that
-- will log messages where the request body of the incoming request passes
-- the filter.
mkFilterLogger :: LogFilter a -> Middleware
mkFilterLogger = unsafePerformIO undefined
