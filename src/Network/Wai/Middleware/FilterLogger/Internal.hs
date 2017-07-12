{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE OverloadedStrings #-}

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

instance Loggable ByteString where
instance Loggable BL.ByteString where

-- | Type that represents a log filtering function. If the return type
-- is Nothing, then no log message will be created. Otherwise, a log message
-- will be created using the (potentially modified) returned value
type LogFilter a = a -> Maybe a

logFilter :: (Loggable a) => ByteString -> LogFilter a -> Maybe a
logFilter bs lf = prep bs >>= lf

-- | Given a valid 'LogFilter', construct a 'Middleware' value that
-- will log messages where the request body of the incoming request passes
-- the filter.
mkFilterLogger :: (Loggable a) => Bool -> LogFilter a -> Middleware
mkFilterLogger True  lf = unsafePerformIO $ mkRequestLogger def { outputFormat = CustomOutputFormatWithDetails $ customDetailedOutputFormatter lf }
mkFilterLogger False lf = unsafePerformIO $ mkRequestLogger def { outputFormat = CustomOutputFormatWithDetails $ customDetailedOutputFormatter lf }
{-# NOINLINE mkFilterLogger #-}

customDetailedOutputFormatter :: (Loggable a) => LogFilter a -> OutputFormatterWithDetails
customDetailedOutputFormatter lf date req status responseSize time reqBody builder =
  maybe mempty ((<>) "\n" . toLogStr . logShow) $ logFilter (BS.concat reqBody) lf
