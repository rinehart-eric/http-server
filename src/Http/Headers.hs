module Http.Headers
where

import Data.Map as M

data RequestType = GET | POST deriving (Show)
data Request = Request
    {
        requestType :: RequestType,
        requestPath :: String,
        requestVersion :: String,
        requestHeaders :: M.Map String String,
        requestBody :: String
    } deriving (Show)
