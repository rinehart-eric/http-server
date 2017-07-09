module Main
where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as B
import Data.Word
import Http.Headers

headerParser :: Parser [String]
headerParser = do
        reqType <- typeParser
        space
        reqPath <- urlParser
        space
        httpVersion <- versionParser
        endOfLine
        return [reqType, reqPath, httpVersion]

typeParser :: Parser String
typeParser = many1 letter_iso8859_15

urlParser :: Parser String
urlParser = do
        url <- Data.Attoparsec.ByteString.Char8.takeWhile urlCharPred
        return $ unpack url

urlCharPred :: Char -> Bool
urlCharPred c = or [p c | p <- [isAlpha_iso8859_15, isDigit, inClass "-._~:/?#][@!$&'()*+,;=%"]]

versionParser :: Parser String
versionParser = do
        string $ pack "HTTP/"
        major <- many1 digit
        remainder <- many ((:) <$> char '.' <*> many1 digit)
        return $ "HTTP/" ++ major ++ concat remainder

main = do
    putStrLn $ show $ map parseType ["GET", "POST", "wrong"]
