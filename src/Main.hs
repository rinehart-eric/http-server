module Main
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import qualified Data.Attoparsec.ByteString.Char8 as A (takeWhile)
import qualified Data.ByteString.Char8 as B
import Http.Headers

type ParseResult = (String, String, String, String, String)

parseHeader :: B.ByteString -> Maybe Request
parseHeader str = do
        (reqType, reqPath, httpVersion, headers, body) <- resultToMaybe . parse headerParser $ str
        Nothing

resultToMaybe :: Result a -> Maybe a
resultToMaybe (Done _ a) = Just a
resultToMaybe _          = Nothing

-- | Parses a tuple containing the request type, path, HTTP version, headers, and body
headerParser :: Parser ParseResult
headerParser = do
        reqType <- typeParser
        space
        reqPath <- urlParser
        space
        httpVersion <- versionParser
        endOfLine
        return (reqType, reqPath, httpVersion, "", "")

typeParser :: Parser String
typeParser = many1 letter_iso8859_15

urlParser :: Parser String
urlParser = A.takeWhile urlCharPred >>= return . B.unpack

urlCharPred :: Char -> Bool
urlCharPred c = or [p c | p <- [isAlpha_iso8859_15, isDigit, inClass "-._~:/?#][@!$&'()*+,;=%"]]

versionParser :: Parser String
versionParser = do
        string $ B.pack "HTTP/"
        major <- many1 digit
        remainder <- many ((:) <$> char '.' <*> many1 digit)
        return $ major ++ concat remainder

main = do
    putStrLn $ show $ map parseType ["GET", "POST", "wrong"]
