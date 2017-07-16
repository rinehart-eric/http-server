module Http.Response (
    encodeResponse,
    Response(..),
    )
where

import Data.Monoid
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as Map

encodeResponse :: Response -> L.ByteString
encodeResponse = toLazyByteString . renderResponse

renderResponse :: Response -> Builder
renderResponse (Response v c h b) = renderStartingLine v c
        <> (renderHeaders $ Map.toList h) <> renderBody b <> stringUtf8 "\r\n\r\n"

renderStartingLine :: String -> Int -> Builder
renderStartingLine v c = stringUtf8 "HTTP/" <> stringUtf8 v <> charUtf8 ' '
        <> renderCodeAndPhrase c

renderCodeAndPhrase :: Int -> Builder
renderCodeAndPhrase c = intDec c <> charUtf8 ' ' <> (stringUtf8 $ reasonPhrase c)

renderHeaders :: [(String, String)] -> Builder
renderHeaders hs = mconcat [ stringUtf8 "\r\n" <> renderHeader h | h <- hs ]

renderHeader :: (String, String) -> Builder
renderHeader (k, v) = mconcat $ map stringUtf8 [k, ": ", v]

renderBody :: Maybe String -> Builder
renderBody Nothing = mempty
renderBody (Just b) = stringUtf8 "\r\n\r\n" <> stringUtf8 b

-- | Reason phrases for status codes
reasonPhrase :: Int -> String
reasonPhrase 200 = "OK"
reasonPhrase 404 = "Not Found"
reasonPhrase _   = ""

-- | HTTP response data
data Response = Response
    { httpVersion     :: String,
      statusCode      :: Int,
      responseHeaders :: Map.Map String String,
      responseBody    :: Maybe String
    } deriving (Show)
