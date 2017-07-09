module Main
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as A (takeWhile)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map
import Http.Headers

-- | Parses a tuple containing the request type, path, HTTP version, headers, and body
headerParser :: Parser Request
headerParser = do
        reqType <- typeParser
        space
        reqPath <- urlParser
        space
        httpVersion <- versionParser
        endOfLine
        return Request {
                requestType = reqType,
                requestPath = reqPath,
                requestVersion = httpVersion,
                requestHeaders = Map.empty,
                requestBody = Nothing }

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

main = do
    putStrLn "tmp"
