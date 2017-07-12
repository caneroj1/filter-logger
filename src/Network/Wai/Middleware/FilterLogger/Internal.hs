{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Wai.Middleware.FilterLogger.Internal where

import           Control.Monad
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as BS hiding (ByteString)
import qualified Data.ByteString.Lazy                 as BL (ByteString,
                                                             fromStrict,
                                                             toStrict)
import           Data.Default
import           Data.Semigroup
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           System.IO.Unsafe
import           System.Log.FastLogger

-- | Typeclass for types that can be converted into a strict 'ByteString'
-- and be shown in a log.
class LogShowable a where
  logShow :: a -> ByteString

instance LogShowable ByteString where
  logShow = id

instance LogShowable BL.ByteString where
  logShow = BL.toStrict

-- | Typeclass for types that can be converted into from a strict 'ByteString' and will be used as
-- arguments to 'LogFilter'
class LogFilterable a where
  prep :: ByteString -> Maybe a

instance LogFilterable ByteString where
  prep = return

instance LogFilterable BL.ByteString where
  prep = return . BL.fromStrict

class (LogFilterable a, LogShowable a) => Loggable a where

type LogFilter a = a -> Maybe a

-- | Type that represents a log filtering function. If the return type
-- is Nothing, then no log message will be created. Otherwise, a log message
-- will be created using the (potentially modified) returned value
-- newtype LogFilter a = LogFilter {
--     operate :: (LogFilterable a, LogShowable a) => a -> Maybe a
--   }


logFilter :: (LogFilterable a, LogShowable a) => ByteString -> LogFilter a -> Maybe a
logFilter bs lf = prep bs >>= lf

-- sequenceFilter :: LogFilter a -> LogFilter a -> LogFilter a
-- sequenceFilter llf rlf = LogFilter (operate llf >=> operate rlf)

-- | Given a 'LogFilter', construct a 'Middleware' value that
-- will log messages where the request body of the incoming request passes
-- the filter.
mkFilterLogger :: (Loggable a) => Bool -> LogFilter a -> Middleware
mkFilterLogger True  lf = unsafePerformIO undefined
mkFilterLogger False lf = unsafePerformIO undefined
  where reqLogger = mkRequestLogger def { outputFormat = CustomOutputFormatWithDetails $ customDetailedOutputFormatter lf }

customDetailedOutputFormatter :: (Loggable a) => LogFilter a -> OutputFormatterWithDetails
customDetailedOutputFormatter lf date req status responseSize time reqBody builder =
  maybe mempty (toLogStr . logShow) $ logFilter (BS.concat reqBody) lf
