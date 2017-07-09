module Http.Request (
    parseRequest,
    RequestType(..),
    Request(..)
    )
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as A (takeWhile)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map

-- | Attempt to parse an HTTP request
parseRequest :: B.ByteString -> Maybe Request
parseRequest = resultToMaybe . parse requestParser

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Done _ a) = Just a
resultToMaybe _          = Nothing

requestParser :: Parser Request
requestParser = do
        reqType <- typeParser
        space
        reqPath <- urlParser
        space
        httpVersion <- versionParser
        endOfLine
        return Request
            { requestType    = reqType,
              requestPath    = reqPath,
              requestVersion = httpVersion,
              requestHeaders = Map.empty,
              requestBody    = Nothing }

typeParser :: Parser RequestType
typeParser = do
        reqType <- many1 letter_iso8859_15
        return $ read reqType

urlParser :: Parser String
urlParser = do
        urlBytes <- A.takeWhile urlCharPred
        return $ B.unpack urlBytes

urlCharPred :: Char -> Bool
urlCharPred c = or [p c | p <- [isAlpha_iso8859_15, isDigit, inClass "-._~:/?#][@!$&'()*+,;=%"]]

versionParser :: Parser String
versionParser = do
        string . B.pack $ "HTTP/"
        major <- many1 digit
        remainder <- many ((:) <$> char '.' <*> many1 digit)
        return $ major ++ concat remainder

-- | HTTP request type
data RequestType = GET | POST deriving (Show, Read)

-- | Parsed HTTP request data
data Request = Request
    { requestType    :: RequestType,
      requestPath    :: String,
      requestVersion :: String,
      requestHeaders :: Map.Map String String,
      requestBody    :: Maybe String
    } deriving (Show)
