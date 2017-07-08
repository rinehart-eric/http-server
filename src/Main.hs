module Main
where

import Http.Headers
import Text.Parsec
import Control.Applicative (liftA)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe v = case v of
    Left _ -> Nothing
    Right m -> Just m

parseHeader :: String -> Maybe [String]
parseHeader header = eitherToMaybe $ parse headerParser "" header

headerParser :: Parsec String () [String]
headerParser = do
        reqType <- many1 letter
        spaces
        reqPath <- many1 validURI
        spaces
        httpVersion <- (++) <$> string "HTTP/" <*> versionNumber
        newline
        return [reqType, reqPath, httpVersion]

validURI :: Parsec String () Char
validURI = alphaNum <|> oneOf "-._~:/?#[]@!$&'()*+,;=%"

versionNumber :: Parsec String () [Char]
versionNumber = do
        major <- many1 digit
        remainder <- many ((:) <$> char '.' <*> many1 digit)
        return $ major ++ concat remainder

main = do
    putStrLn $ show $ map parseType ["GET", "POST", "wrong"]
