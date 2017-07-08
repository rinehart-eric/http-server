module Http.Headers (
    parseType
    )
where

import Data.List (span)
import qualified Data.Map as M
import Data.Maybe (isJust)
import Text.Read (readMaybe)
import Text.Regex

parse :: String -> Maybe Request
parse request = do
    let (startLine:afterStart) = lines request
        reg = mkRegex "^(.+):\\s*(.+)$"
        (headerLines, afterHeaders) = span (isJust . matchRegex reg) afterStart
        content =

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
