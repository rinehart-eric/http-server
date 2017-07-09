module Http.Headers (
    RequestType(..),
    Request(..)
    )
where

import qualified Data.Map.Strict as Map

data RequestType = GET | POST deriving (Show, Read)
data Request = Request
    {
        requestType :: RequestType,
        requestPath :: String,
        requestVersion :: String,
        requestHeaders :: Map.Map String String,
        requestBody :: Maybe String
    } deriving (Show)
