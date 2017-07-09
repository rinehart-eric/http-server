module Http.Request (
    parseRequest,
    RequestType(..),
    Request(..),
    headersParser
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
resultToMaybe (Done _ a)  = Just a
resultToMaybe (Partial f) = resultToMaybe $ f B.empty
resultToMaybe _           = Nothing

requestParser :: Parser Request
requestParser = do
        reqType <- typeParser
        space
        reqPath <- urlParser
        space
        httpVersion <- versionParser
        headers <- headersParser
        body <- option Nothing (Just <$> bodyParser)
        return Request
            { requestType    = reqType,
              requestPath    = reqPath,
              requestVersion = httpVersion,
              requestHeaders = headers,
              requestBody    = body }

typeParser :: Parser RequestType
typeParser = do
        reqType <- many1 letter_iso8859_15
        return $ read reqType

urlParser :: Parser String
urlParser = do
        urlBytes <- A.takeWhile urlCharPred
        return $ B.unpack urlBytes

urlCharPred :: Char -> Bool
urlCharPred = inClass "a-zA-Z0-9-._~:/?#][@!$&'()*+,;=%"

versionParser :: Parser String
versionParser = do
        string $ B.pack "HTTP/"
        major <- many1 digit
        remainder <- many ((:) <$> char '.' <*> many1 digit)
        return $ major ++ concat remainder

headersParser :: Parser (Map.Map String String)
headersParser = do
        lines <- many' headerLineParser
        return $ Map.fromList lines

headerLineParser :: Parser (String, String)
headerLineParser = do
        endOfLine
        key <- A.takeWhile $ inClass "a-zA-Z-"
        char ':'
        A.takeWhile isSpace
        value <- takeTill $ inClass "\r\n"
        return (B.unpack key, B.unpack value)

bodyParser :: Parser String
bodyParser = do
        endOfLine
        endOfLine
        body <- takeByteString
        return $ B.unpack body

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
