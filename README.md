# filter-logger

Filterable request logging as a wai middleware. Change what data is logged and when.

## Usage

Here is one example from the **examples/password** directory where we use the filter logger to implement password filtering. The example uses `scotty` for our web server, and we use the `logShowJSON` and `logFilterJSON` helper functions to help us create our instances.

### Password Filtering

```haskell

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai.Middleware.FilterLogger
import           Web.Scotty

data LoginRequest = LoginRequest {
    username :: String
  , password :: String
  } deriving (Generic)

instance FromJSON LoginRequest where
instance ToJSON LoginRequest where

instance LogShowable LoginRequest where
  logShow = logShowJSON

instance LogFilterable LoginRequest where
  prep = logFilterJSON

instance Loggable LoginRequest where

main :: IO ()
main = scotty 3000 $ do
  middleware filterPasswords
  post "/" $ text "SUCCESS"

filterPasswords =
  mkFilterLogger True hidePasswords
  where hidePasswords r@LoginRequest{..} = Just r {password = "*****"}

```

Sending a POST request to localhost:3000 with a body like
```
{
  "username": "test-username",
  "password": "myPassw0rd123"
}
```

  will result in a log message like
```
11/Jul/2017:21:48:20 -0400
200 - OK
0.03ms
{
    "username": "test-username",
    "password": "*****"
}
```

### Chaining Filters

Here is a rather contrived example showing that you can chain these filters together easily and do all sorts of filtering.

```haskell

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

```

Sending a POST request to localhost:3000 with a body like
```
abcdefghi
```

will result in a log message like
```
11/Jul/2017:22:00:59 -0400
200 - OK
0.03ms
abcdefghi
```

If you send a POST request with a body like
```
abcdefghij
```
or
```
abdefghij
```

you won't see anything in the server logs.
