module Http.Headers (
    parseType,
    RequestType(..)
    )
where

import qualified Data.Map as M
import Text.Read (readMaybe)

-- | Attempt to parse the request type from the string
parseType :: String -> Maybe RequestType
parseType = readMaybe

data RequestType = GET | POST deriving (Show, Read)
data Request = Request
    {
        requestType :: RequestType,
        requestPath :: String,
        requestVersion :: String,
        requestHeaders :: M.Map String String,
        requestBody :: Maybe String
    } deriving (Show)
