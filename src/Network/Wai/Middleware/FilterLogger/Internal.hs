{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.FilterLogger.Internal where

import           Control.Monad
import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as BS hiding (ByteString)
import qualified Data.ByteString.Lazy                 as BL (ByteString,
                                                             fromStrict,
                                                             toStrict)
import           Data.Char
import           Data.Default
import           Data.Semigroup
import           Data.Word
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Middleware.RequestLogger
import           System.IO.Unsafe
import           System.Log.FastLogger
import           Text.Printf                          (printf)

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
mkFilterLogger detailed lf = unsafePerformIO $
  mkRequestLogger def { outputFormat = CustomOutputFormatWithDetails $ customOutputFormatter detailed lf }
{-# NOINLINE mkFilterLogger #-}

customOutputFormatter :: (Loggable a) => Bool -> LogFilter a -> OutputFormatterWithDetails
customOutputFormatter detail lf date req status responseSize time reqBody builder =
  maybe mempty (logString detail) $ logFilter (BS.concat reqBody) lf
  where
    toBS   = BS.pack . map (fromIntegral . ord)

    dfromRational :: Rational -> Double
    dfromRational = fromRational

    inMS   = printf "%.2f" . dfromRational $ toRational time * 1000

    header = date <> "\n" <>
             toBS (show . fromIntegral $ statusCode status) <> " - " <> statusMessage status <> "\n"

    buildRespSize (Just s) = "Response Size: " <> toBS (show s) <> "\n"
    buildRespSize Nothing  = ""

    buildDuration = toBS inMS <> "ms" <> "\n"

    buildDetails True  = buildRespSize responseSize <> buildDuration
    buildDetails False = ""

    logString detailed msg = toLogStr (
      header              <>
      buildDetails detail <>
      logShow msg         <>
      "\n")






