module Http.Headers (
    parseType
    )
where

import qualified Data.Map as M
import Text.Read (readMaybe)

-- parse :: String -> Maybe Request
-- parse request =

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
        requestBody :: String
    } deriving (Show)
